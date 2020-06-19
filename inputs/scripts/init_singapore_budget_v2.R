#Import data
cmip6_dt <- read.csv("inputs/data/SSP_IAM_V2_201811.csv",stringsAsFactors = FALSE, check.names = FALSE)
cmip6_pop_dt <- read.csv("inputs/data/SspDb_country_data_2013-06-12.csv",stringsAsFactors = FALSE, check.names = FALSE)
lequere_co2_dt <- read.csv("inputs/data/LeQuere_CO2_emissions_by_country.csv",stringsAsFactors = FALSE, check.names = FALSE)
ceds_co2_dt <- read.csv("inputs/data/CEDS_CO2_emissions_by_country_v_2019_12_23.csv",stringsAsFactors = FALSE, check.names = FALSE)
cmip6_mapping_dt <- read.csv("inputs/data/cmip6_iam_model_region_mapping.csv",stringsAsFactors = FALSE, check.names = FALSE)
iea_co2_emissions_dt <- read.csv("inputs/data/iea_co2_emissions.csv",stringsAsFactors = FALSE, check.names = FALSE)
#Format imported data
colnames(ceds_co2_dt) <- gsub("X","",colnames(ceds_co2_dt))
iea_co2_emissions_dt[iea_co2_emissions_dt==".."] <- NA
dt_col <- as.character(1971:2017)
iea_co2_emissions_dt[,dt_col] <- sapply(dt_col,function(x) as.numeric(iea_co2_emissions_dt[,x]))
mat_hist_iea_co2 <- as.matrix(subset(iea_co2_emissions_dt,select=-c(Unit,Country)))
rownames(mat_hist_iea_co2) <- iea_co2_emissions_dt$Country
#Create the dataset over continuous years  
dt_col <- c("Scenario","Convergence_year","Year","Unit","Value")
out_dt <-  setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
#Only consider 1.5 and 2 mitigation pathways in ASIA. World
region = "R5ASIA"
#Get the list of list of countries associated with the region
cty_list <- unique(subset(cmip6_mapping_dt,R5_region==region)$ISO)

#Create matrix of emissions per country
first_hist_year <- 1971
iea_last_hist_year <- 2017
f_year = 2100
mat_cty_co2 <- matrix(NA, nrow=length(cty_list),ncol=f_year-first_hist_year+1,dimnames=list(cty_list,first_hist_year:f_year))

#Fill emissions from IEA data (when available)
iea_country_list <- intersect(subset(cmip6_mapping_dt,ISO%in%cty_list)$IEA,iea_co2_emissions_dt$Country)
for (cty in iea_country_list){
  iso_cty <- subset(cmip6_mapping_dt,IEA==cty)$ISO
  #First from CEDS
  mat_cty_co2[iso_cty,as.character(first_hist_year:iea_last_hist_year)] <- mat_hist_iea_co2[cty,as.character(first_hist_year:iea_last_hist_year)]
}

#Update with LeQuere data
cty_list2 <- rownames(mat_cty_co2)[is.na(mat_cty_co2[,"2015"])]
lequere_country_list <- intersect(subset(cmip6_mapping_dt,ISO%in%cty_list2)$LeQuere,colnames(lequere_co2_dt))
lequere_last_hist_year <- 2017
#First fill historical data. In Mt CO2
for (cty in lequere_country_list){
  iso_cty <- subset(cmip6_mapping_dt,LeQuere==cty)$ISO
  #from Le Quere
  mat_cty_co2[iso_cty,as.character(first_hist_year:lequere_last_hist_year)] <- subset(lequere_co2_dt,Year %in% first_hist_year:lequere_last_hist_year)[,cty]*3.664
}

#Update CEDS data
cty_list3 <- rownames(mat_cty_co2)[is.na(mat_cty_co2[,"1971"])]
for (cty in cty_list3){
  if (any(grepl(cty,ceds_co2_dt$iso,ignore.case = TRUE))){
    iso_cty = grep(cty,ceds_co2_dt$iso,ignore.case = TRUE,value = TRUE)
    last_year <- as.numeric(colnames(mat_cty_co2)[max(which(as.numeric(colnames(mat_cty_co2)[is.na(mat_cty_co2[cty,])])<2018))])
    mat_cty_co2[cty,as.character(first_hist_year:ifelse(last_year>2014,2014,last_year))] <- as.matrix(subset(ceds_co2_dt,iso==iso_cty)[,as.character(first_hist_year:ifelse(last_year>2014,2014,last_year))])/10^3
    #Assumed emissions constant from 2014 to 2017
    if (last_year>2014){
      mat_cty_co2[cty,as.character(2015:2017)] <- mat_cty_co2[cty,"2014"]
    }
  }
}

#Remove rows without data
mat_cty_co2 <- mat_cty_co2[!is.na(mat_cty_co2[,"2017"]),]
#Force to 0 data that are missing inside countries
mat_cty_co2[,as.character(1971:2017)][is.na(mat_cty_co2[,as.character(1971:2017)])] <- 0
#Update country list
cty_list_co2 <- rownames(mat_cty_co2)

#Output file
out_dt <- NULL

#Make a list of model and scenario
scen_list <- c("SSP1-19","SSP1-26","SSP2-19","SSP2-26","SSP4-19","SSP4-26","SSP5-19","SSP5-26")
for (scen in scen_list){
  ssp_scen <- substring(scen, 0,as.numeric(regexpr(pattern="-",scen))-1)
  #Create population matrix (as assumed in the CMIP6)
  mat_cty_pop <- matrix(NA, nrow=length(cty_list_co2),ncol=f_year-2010+1,dimnames=list(cty_list_co2,2010:f_year))
  years_provided <- seq(2010,2100,5)
  for (cty in cty_list){
    if (cty%in%cmip6_pop_dt$REGION){
      mat_cty_pop[cty,as.character(years_provided)] <- as.matrix(subset(cmip6_pop_dt,grepl(ssp_scen,SCENARIO) & REGION==cty & VARIABLE=="Population" & UNIT=="million")[1,as.character(years_provided)])
      mat_cty_pop[cty,as.character(2010:2100)] <- approx(x=years_provided,y=mat_cty_pop[cty,as.character(years_provided)],method="linear",xout=2010:2100)$y
    }
  }
  #Update country list assuming that country without popuplation are not included
  mat_cty_pop <- mat_cty_pop[!is.na(mat_cty_pop[,"2017"]),]
  cty_list <- rownames(mat_cty_pop)
  mat_cty_co2_tmp <- mat_cty_co2[cty_list,]
  
  for (conv_year in c(2040,2050)){
    #Calculate the fractional share of emissions
    mat_cty_fra_co2 <- matrix(NA, nrow=nrow(mat_cty_pop),ncol=ncol(mat_cty_pop),dimnames=list(rownames(mat_cty_pop),colnames(mat_cty_pop)))
    #From 2010 to 2017, historical emission share
    for (y in 2010:2017){
      mat_cty_fra_co2[,as.character(y)] <- mat_cty_co2_tmp[rownames(mat_cty_fra_co2),as.character(y)]/sum(mat_cty_co2_tmp[rownames(mat_cty_fra_co2),as.character(y)])
    }
    #Calculate the fraction in convergence year based on population
    mat_cty_fra_co2[,as.character(conv_year)] <- mat_cty_pop[rownames(mat_cty_fra_co2),as.character(conv_year)]/sum(mat_cty_pop[rownames(mat_cty_fra_co2),as.character(conv_year)])
    #Assume linear transition from present levels to convergence levels
    for (cty in cty_list){
      mat_cty_fra_co2[cty,as.character(2017:conv_year)] <- approx(x=c(2017,conv_year),y=mat_cty_fra_co2[cty,as.character(c(2017,conv_year))],method="linear",xout=2017:conv_year)$y
    }
    #Assume fraction based on population after convergence year
    mat_cty_fra_co2[,as.character((conv_year+1):f_year)] <- mat_cty_fra_co2[,as.character(conv_year)]
    #Calculate the resulting emissions
    #
    fut_co2_dt <- subset(cmip6_dt,SCENARIO%in%scen & REGION=="R5.2ASIA" & VARIABLE=="Emissions|CO2")
    #
    i_year=2005
    for (model in unique(fut_co2_dt$MODEL)){
      fut_co2_dt_tmp <- subset(fut_co2_dt,MODEL==model)
      mat_reg_co2 <- matrix(NA,nrow=1,ncol=f_year-i_year+1,dimnames = list(fut_co2_dt_tmp$SCENARIO,i_year:f_year))
      years_provided <- c(2005,seq(2010,2100,10))
      #Adjust the emissions to historical emissions from year 2015
      mat_reg_co2[,as.character(years_provided)] <- as.matrix(subset(fut_co2_dt_tmp,select=-c(MODEL, SCENARIO, REGION, VARIABLE, UNIT)))/subset(fut_co2_dt_tmp)[,"2010"]*sum(mat_cty_co2_tmp[,"2010"])
      mat_reg_co2[1,as.character(i_year:f_year)] <- approx(x=years_provided,y=mat_reg_co2[1,as.character(years_provided)],method="linear",xout=i_year:f_year)$y
      #Regional budget. Unit Gt CO2
      #rowSums(mat_reg_co2)/10^3
      #
      for (y in 2018:f_year){
        mat_cty_co2_tmp[,as.character(y)] <- mat_cty_fra_co2[rownames(mat_cty_co2_tmp),as.character(y)] * mat_reg_co2[scen,as.character(y)]
      }
      #Extract the budget for Singapore
      tmp_dt <- data.frame(Value=mat_cty_co2_tmp["SGP",as.character(2005:2050)],Year=2005:2050,Convergence_year=conv_year,Scenario=scen,Model=model,Unit="Mt CO2",row.names = NULL,stringsAsFactors = FALSE)
      out_dt <- rbind(out_dt,tmp_dt)
    }
    }
}

out_dt[grepl("-19",out_dt$Scenario),"Target"] <- "1.5C"
out_dt[grepl("-26",out_dt$Scenario),"Target"] <- "2C"

#Calculate emission budget according to INDC
library(reshape2)
#Historical emission
first_hist_year=1990
mat_cty_co2 <- matrix(NA, nrow=length(1),ncol=f_year-first_hist_year+1,dimnames=list("SGP",first_hist_year:f_year))
#First fill historical data. Convert in Mt CO2
cty="SGP"
#First from CEDS
last_hist_year=2017
mat_cty_co2[cty,as.character(first_hist_year:last_hist_year)] <- mat_hist_iea_co2["Singapore",as.character(first_hist_year:last_hist_year)]
#Import data
gdp_sgp_dt <- read.csv("inputs/data/API_SGP_DS2_en_csv_v2_824832.csv",stringsAsFactors = FALSE, check.names = FALSE)
mat_cty_gdp <- matrix(NA, nrow=1,ncol=f_year-1990+1,dimnames=list("SGP",1990:f_year)) 
mat_cty_gdp[,as.character(1990:2018)] <- as.matrix(subset(gdp_sgp_dt,`Indicator Name`=="GDP, PPP (constant 2011 international $)")[,as.character(1990:2018)])
#Projections
years_provided <- seq(2020,2100,5)
cty <- "SGP"
mat_cty_gdp[cty,as.character(years_provided)] <- as.matrix(subset(cmip6_pop_dt,MODEL%in%"IIASA GDP" & SCENARIO%in%c("SSP1_v9_130219","SSP1_v9_130325") & REGION==cty & VARIABLE=="GDP|PPP")[,as.character(years_provided)])/subset(cmip6_pop_dt,MODEL%in%"IIASA GDP" & SCENARIO%in%c("SSP1_v9_130219","SSP1_v9_130325") & REGION==cty & VARIABLE=="GDP|PPP")[,"2010"]*mat_cty_gdp[,"2010"]
mat_cty_gdp[cty,as.character(2019:2100)] <- approx(x=c(2018,years_provided),y=mat_cty_gdp[cty,c("2018",as.character(years_provided))],method="linear",xout=2019:2100)$y
#Calculate historical emission intensity per capita. Mt/GDP
mat_co2_intensity <- matrix(NA, nrow=nrow(mat_cty_gdp),ncol=ncol(mat_cty_gdp),dimnames=list(rownames(mat_cty_gdp),colnames(mat_cty_gdp)))
mat_co2_intensity[,as.character(1990:2017)] <- mat_cty_co2[rownames(mat_co2_intensity),as.character(1990:2017)]*10^9/mat_cty_gdp[rownames(mat_co2_intensity),as.character(1990:2017)]

#Target: 36% lower emission intensity in 2030
cty="SGP"
mat_co2_intensity[cty,"2030"] <- mat_co2_intensity[cty,"2005"]*(1-0.36)
mat_co2_intensity[cty,as.character(2018:2030)] <- approx(x=c(2017,2030),y=mat_co2_intensity[cty,as.character(c(2017,2030))],method="linear",xout=2018:2030)$y
#Calculate resulting CO2 emissions
mat_cty_co2[cty,as.character(2018:2030)] <- mat_co2_intensity[cty,as.character(2018:2030)] * mat_cty_gdp[cty,as.character(2018:2030)] /10^9
tmp_dt <- data.frame(Value=mat_cty_co2["SGP",as.character(2005:2030)],Year=2005:2030,Convergence_year=NA,Scenario="INDC",Model="INDC",Target="INDC",Unit="Mt CO2",row.names = NULL,stringsAsFactors = FALSE)
out_dt <- rbind(out_dt,tmp_dt)

write.csv(out_dt,"inputs/model/singapore_carbon_budget.csv",row.names = FALSE)


