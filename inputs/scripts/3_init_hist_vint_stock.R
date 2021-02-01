#Scripts to create historical vintaged stock of bus, private car, taxi and motorcycle fleets
#Inputs
source("utils/data_processing_f.R")
source("load_functions.R")
load_input_data_f()
load_attribute_value()


## Total historical stock by transport mode
veh_pop <- read.csv("inputs/data/annual-motor-vehicle-population-by-vehicle-type.csv",stringsAsFactors = FALSE)
veh_pop$Transport_mode <- get_matching_names(veh_pop$type,matching_type="passenger_transport_mode",original_source = "Type", matched_source = "Mode")
colnames(veh_pop) <- rename_values(colnames(veh_pop), list(Year="year",Value="number"))
out_veh_pop_dt <- aggregate(formula=Value~Year+Transport_mode,data=subset(veh_pop,Transport_mode!=""),FUN=sum)
mat_tot_veh_pop <- acast(data=subset(out_veh_pop_dt), Transport_mode ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)



# BUS ---------------------------------------------------------------------

#Total historical stock by technology
#Input
first_yr <- 2005
last_yr <- 2019
vh_techno <- get_input_f(input_name = 'model_matching_bus_technology')
onroad_car_pop <- read.csv("inputs/data/annual-motor-vehicle-population-by-type-of-fuel-used.csv",stringsAsFactors = FALSE)
#Format input. Be careful, buses refer to all buses, not only public buses.
onroad_car_pop$Transport_mode <- get_matching_names(onroad_car_pop$type,matching_type="passenger_transport_mode",original_source = "Type", matched_source = "Mode")
onroad_car_pop$Transport_mode <- rename_values(onroad_car_pop$Transport_mode,list(Bus="Public bus"))
onroad_car_pop$Technology <- get_matching_names(onroad_car_pop$engine,matching_type="bus_technology",original_source = "LTA", matched_source = "Technology")
colnames(onroad_car_pop) <- rename_values(colnames(onroad_car_pop), list(Year="year",Value="number"))
out_onroad_car_pop <- aggregate(formula=Value~Year+Transport_mode+Technology,data=subset(onroad_car_pop,Transport_mode!=""),FUN=sum)

#Vintaged stock by technology for the three bus types

for (mode in c("Public bus","Private bus","School bus")){
  #Create output matrix of onroad population by technology
  mat_onroad_pop <- matrix(0,nrow=length(unique(vh_techno$Technology)),ncol=(last_yr-first_yr+1),dimnames = list(unique(vh_techno$Technology),first_yr:last_yr))
  #Get incomplete matrix from data. BE CAREFUL: This technologies are for all buses, so need to be adjusted only for public bus first.
  in_mat_onroad_pop <- acast(data=subset(onroad_car_pop,Transport_mode=="Bus"), Technology ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #We adjust the technologies for bus types. We assume similar mix of tehnologies for all buse types.
  in_mat_onroad_pop[,] <- round((in_mat_onroad_pop %*% diag(x=1/colSums(in_mat_onroad_pop))) %*% diag(x=mat_tot_veh_pop[mode,colnames(in_mat_onroad_pop)]))
  #Fill matrix with incomplete data
  mat_onroad_pop[rownames(in_mat_onroad_pop),colnames(in_mat_onroad_pop)] <- in_mat_onroad_pop
  #Fill 2005 data.
  #ASSUMPTION: Similar technology share in 2006 and 2005
  mat_onroad_pop[,"2005"] <- round(mat_onroad_pop[,"2006"]/mat_tot_veh_pop[mode,"2006"]*mat_tot_veh_pop[mode,"2005"])
  #Calculate vintaged stock
  
  car_pop_dt <- read.csv("inputs/data/annual-age-distribution-of-bus.csv",stringsAsFactors = FALSE,check.names = FALSE)
  colnames(car_pop_dt) <- rename_values(colnames(car_pop_dt), list(Year="year",Value="number",Age="age_years"))
  car_pop_dt$Age <- as.numeric(substring(car_pop_dt$Age,0,as.numeric(regexpr(pattern="-",car_pop_dt$Age))-1))
  mat_age_car_pop <- acast(data=car_pop_dt, Age ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #Adjust age distribution (of all buses) for bus type. Assume all bus types have same age distribution
  mat_age_car_pop[,] <- round((mat_age_car_pop %*% diag(x=1/colSums(mat_age_car_pop))) %*% diag(x=mat_tot_veh_pop[mode,colnames(mat_age_car_pop)]))
  
  age_tbc <- 0:20
  mat_vint_stock <- matrix(0,nrow=length(unique(vh_techno$Technology)),ncol=length(age_tbc),dimnames = list(unique(vh_techno$Technology),age_tbc))
  #Out of vintaged stock
  mat_vint_stock_list <- list()
  #Initialize the vintaged stock
  #ASSUMPTION: Assume that ICEB-D is proportionaly distributed in old technologies
  mat_vint_stock["ICEB-D",as.character(0:20)] <- round(mat_age_car_pop[,"2005"]/sum(mat_age_car_pop[,"2005"])*mat_onroad_pop["ICEB-D","2005"])
  mat_vint_stock_list[["2005"]] <- mat_vint_stock
  for (year in 2006:2019){
    #Create matrix of survival rates.
    year_tbc <- year
    surv_rate_matrix <- diag(x=sapply(1:max(age_tbc), function (x) do.call(survival_rate_f,list(mode=mode,age=x, year=year_tbc,cumulative_rate="n",scrappage_rate="n"))))
    dimnames(surv_rate_matrix) <- list(1:max(age_tbc),1:max(age_tbc))
    #Create matrix vintaged stock
    mat_vint_stock <- matrix(0,nrow=length(unique(vh_techno$Technology)),ncol=length(age_tbc),dimnames = list(unique(vh_techno$Technology),age_tbc))
    #Update old stock based on previous year matrix stock and survival rates
    mat_vint_stock[,as.character(1:max(age_tbc))] <- round(mat_vint_stock_list[[as.character(year-1)]][rownames(mat_vint_stock),as.character(0:(max(age_tbc)-1))] %*% surv_rate_matrix)
    #Udpate sales based on total stock by technology
    mat_vint_stock[,"0"] <- mat_onroad_pop[rownames(mat_vint_stock),as.character(year)] - rowSums(mat_vint_stock)
    #IF sales are negative, inconsistencies to solve
    if (any(mat_vint_stock[,"0"] <0)){
      #Assumption: Adjust old stock proportionally
      mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc))] <- mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc))] +
        round((diag(x=mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(0)],nrow = length(which(mat_vint_stock[,"0"]<0))) %*%
                 (diag(x=1/vapply(rowSums(mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc)),drop=FALSE]),function(x)ifelse(x==0,1,x),FUN.VALUE = 1),nrow = length(which(mat_vint_stock[,"0"]<0))) %*%
                    mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc))])))
      #Assumption: No sales of these technologies
      mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(0)] <- 0
      #IF no sales, force diesel bus to 1 for market share consistency
      if (sum(mat_vint_stock[,as.character(0)])==0){
        mat_vint_stock["ICEB-D",as.character(0)] <- 1
      }
    }
    #Update list
    mat_vint_stock_list[[as.character(year)]] <- mat_vint_stock
  }
  
  #Save
  bus_hist_vint_stock <- mat_vint_stock_list
  output_name <- switch(mode,
                        "Public bus"="pub_bus",
                        "Private bus"="priv_bus",
                        "School bus"="school_bus")
  #Save list in Rdata
  saveRDS(bus_hist_vint_stock,file=paste0("inputs/model/",output_name,"_hist_vint_stock.RDS"))
  #Save dataframe that summarize the list in dataframe
  out_hist_car_vint_stock <- NULL
  for (i in names(bus_hist_vint_stock)){
    tmp_stock_dt <- as.data.frame(bus_hist_vint_stock[[i]]) %>% 
      cbind(Technology=rownames(bus_hist_vint_stock[[i]]),stringsAsFactors = FALSE) %>% 
      gather("Age","Value",-Technology,convert=TRUE) %>%
      cbind(Year=as.numeric(i),Mode=mode)
    #Update output 
    out_hist_car_vint_stock <- rbind(out_hist_car_vint_stock,tmp_stock_dt)
  }
  write.csv(out_hist_car_vint_stock,paste0("inputs/model/hist_",output_name,"_vint_stock.csv"),row.names = FALSE)
}


# CAR ---------------------------------------------------------------------


#Update the total population with other data from Singstat
annual_veh_pop <- read.csv("inputs/data/annual_car_motorcyle_population_hist.csv",stringsAsFactors = FALSE,check.names = FALSE)
for (year in 2005:2017){
  #Update values for private cars
  mat_tot_veh_pop["Private car",as.character(year)] <- subset(annual_veh_pop,Year==year)$Private_car
  mat_tot_veh_pop["Private hire car",as.character(year)] <- subset(annual_veh_pop,Year==year)$Hire_car
}

# Total historical stock by technology
#Input
first_yr <- 2005
last_yr <- 2019
vh_techno <- get_input_f(input_name = 'model_matching_vehicle_technology')
onroad_car_pop <- read.csv("inputs/data/annual-motor-vehicle-population-by-type-of-fuel-used.csv",stringsAsFactors = FALSE)
mode <- "Private car"
#Format input
onroad_car_pop$Transport_mode <- get_matching_names(onroad_car_pop$type,matching_type="passenger_transport_mode",original_source = "Type", matched_source = "Mode")
onroad_car_pop$Technology <- get_matching_names(onroad_car_pop$engine,matching_type="vehicle_technology",original_source = "LTA", matched_source = "Technology")
colnames(onroad_car_pop) <- rename_values(colnames(onroad_car_pop), list(Year="year",Value="number"))
out_onroad_car_pop <- aggregate(formula=Value~Year+Transport_mode+Technology,data=subset(onroad_car_pop,Transport_mode!=""),FUN=sum)
#Create output matrix
mat_onroad_pop <- matrix(0,nrow=length(unique(vh_techno$Technology)),ncol=(last_yr-first_yr+1),dimnames = list(unique(vh_techno$Technology),first_yr:last_yr))
#Get incomplete matrix from data
in_mat_onroad_pop <- acast(data=subset(onroad_car_pop,Transport_mode=="Private car"), Technology ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
#Adjust: We adjust the stock to previously found data
in_mat_onroad_pop[,] <- round((in_mat_onroad_pop %*% diag(x=1/colSums(in_mat_onroad_pop))) %*% diag(x=mat_tot_veh_pop[mode,colnames(in_mat_onroad_pop)]))

#Fill matrix with incomplete data
mat_onroad_pop[rownames(in_mat_onroad_pop),colnames(in_mat_onroad_pop)] <- in_mat_onroad_pop
#Need to adjust to match private car stock

#Fill 2005 data.
#ASSUMPTION: Similar technology share in 2006 and 2005
mat_onroad_pop[,"2005"] <- round(mat_onroad_pop[,"2006"]/subset(out_veh_pop_dt,Transport_mode=="Private car" & Year==2006)$Value*subset(out_veh_pop_dt,Transport_mode=="Private car" & Year==2005)$Value)

#Calculate vintaged stock for private car
car_pop_dt <- read.csv("inputs/data/annual-age-distribution-of-car.csv",stringsAsFactors = FALSE,check.names = FALSE)
colnames(car_pop_dt) <- rename_values(colnames(car_pop_dt), list(Year="year",Value="number",Age="age_year"))
car_pop_dt$Age <- as.numeric(substring(car_pop_dt$Age,0,as.numeric(regexpr(pattern="-",car_pop_dt$Age))-1))
mat_age_car_pop <- acast(data=car_pop_dt, Age ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
age_tbc <- 0:20
mat_vint_stock <- matrix(0,nrow=length(unique(vh_techno$Technology)),ncol=length(age_tbc),dimnames = list(unique(vh_techno$Technology),age_tbc))
#Out of vintaged stock
mat_vint_stock_list <- list()
#Initialize the vintaged stock
#ASSUMPTION: Asume ICEV-G are distibuted and all other are new. Adjust to total stock of private car
mat_vint_stock["ICEV-G",as.character(0:20)] <- round(mat_age_car_pop[,"2005"]/sum(mat_age_car_pop[,"2005"])*mat_onroad_pop["ICEV-G","2005"])
mat_vint_stock[rownames(mat_onroad_pop)!="ICEV-G","0"] <- mat_onroad_pop[rownames(mat_onroad_pop)!="ICEV-G","2005"]
mat_vint_stock_list[["2005"]] <- mat_vint_stock
for (year in 2006:2019){
  #Create matrix of survival rates
  surv_rate_matrix <- diag(x=sapply(1:max(age_tbc), function (x) do.call(survival_rate_f,list(mode="Private car",age=x, year=year,cumulative_rate="n",scrappage_rate="n"))))
  dimnames(surv_rate_matrix) <- list(1:max(age_tbc),1:max(age_tbc))
  #Create matrix vintaged stock
  mat_vint_stock <- matrix(0,nrow=length(unique(vh_techno$Technology)),ncol=length(age_tbc),dimnames = list(unique(vh_techno$Technology),age_tbc))
  #Update old stock based on previous year matrix stock and survival rates
  mat_vint_stock[,as.character(1:max(age_tbc))] <- round(mat_vint_stock_list[[as.character(year-1)]][rownames(mat_vint_stock),as.character(0:(max(age_tbc)-1))] %*% surv_rate_matrix)
  #Udpate sales based on total stock by technology
  mat_vint_stock[,"0"] <- mat_onroad_pop[rownames(mat_vint_stock),as.character(year)] - rowSums(mat_vint_stock)
  #IF sales are negative, inconsistencies to solve
  if (any(mat_vint_stock[,"0"] <0)){
    #Assumption: Adjust old stock proportionally
    mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc))] <- mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc))] +
      round((diag(x=mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(0)],nrow = length(which(mat_vint_stock[,"0"]<0))) %*%
               (diag(x=1/vapply(rowSums(mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc)),drop=FALSE]),function(x)ifelse(x==0,1,x),FUN.VALUE = 1),nrow = length(which(mat_vint_stock[,"0"]<0))) %*%
                  mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc))])))
    #Assumption: No sales of these technologies
    mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(0)] <- 0
  }
  #Update list
  mat_vint_stock_list[[as.character(year)]] <- mat_vint_stock
}

#Save list in Rdata
saveRDS(mat_vint_stock_list,file="inputs/model/car_hist_vint_stock.RDS")
#Save dataframe that summarize the list in dataframe
out_hist_car_vint_stock <- NULL
for (i in names(mat_vint_stock_list)){
  tmp_stock_dt <- as.data.frame(mat_vint_stock_list[[i]]) %>% 
    cbind(Technology=rownames(mat_vint_stock_list[[i]]),stringsAsFactors = FALSE) %>% 
    gather("Age","Value",-Technology,convert=TRUE) %>%
    cbind(Year=as.numeric(i))
  #Update output 
  out_hist_car_vint_stock <- rbind(out_hist_car_vint_stock,tmp_stock_dt)
}
write.csv(out_hist_car_vint_stock,"inputs/model/hist_car_vint_stock.csv",row.names = FALSE)


# PRIVATE HIRE CAR --------------------------------------------------------

#Total historical stock by technology for car----------------------------------------------------------------------

#Input
first_yr <- 2005
last_yr <- 2019
vh_techno <- get_input_f(input_name = 'model_matching_vehicle_technology')
onroad_car_pop <- read.csv("inputs/data/annual-motor-vehicle-population-by-type-of-fuel-used.csv",stringsAsFactors = FALSE)
mode <- "Private hire car"
#Format input
onroad_car_pop$Transport_mode <- get_matching_names(onroad_car_pop$type,matching_type="passenger_transport_mode",original_source = "Type", matched_source = "Mode")
onroad_car_pop$Technology <- get_matching_names(onroad_car_pop$engine,matching_type="vehicle_technology",original_source = "LTA", matched_source = "Technology")
colnames(onroad_car_pop) <- rename_values(colnames(onroad_car_pop), list(Year="year",Value="number"))
out_onroad_car_pop <- aggregate(formula=Value~Year+Transport_mode+Technology,data=subset(onroad_car_pop,Transport_mode!=""),FUN=sum)
#Create output matrix
mat_onroad_pop <- matrix(0,nrow=length(unique(vh_techno$Technology)),ncol=(last_yr-first_yr+1),dimnames = list(unique(vh_techno$Technology),first_yr:last_yr))
#Get incomplete matrix from data
in_mat_onroad_pop <- acast(data=subset(onroad_car_pop,Transport_mode=="Private car"), Technology ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
#Adjust: We adjust the stock to previously found data. Assumption: Same distribution of fuel between private hire and private cars
in_mat_onroad_pop[,] <- round((in_mat_onroad_pop %*% diag(x=1/colSums(in_mat_onroad_pop))) %*% diag(x=mat_tot_veh_pop[mode,colnames(in_mat_onroad_pop)]))

#Fill matrix with incomplete data
mat_onroad_pop[rownames(in_mat_onroad_pop),colnames(in_mat_onroad_pop)] <- in_mat_onroad_pop
#Need to adjust to match private car stock

#Fill 2005 data.
#ASSUMPTION: Similar technology share in 2006 and 2005
mat_onroad_pop[,"2005"] <- round(mat_onroad_pop[,"2006"]/subset(out_veh_pop_dt,Transport_mode=="Private car" & Year==2006)$Value*subset(out_veh_pop_dt,Transport_mode=="Private car" & Year==2005)$Value)

# Calculate vintaged stock for private hire car -----------------------------
car_dt <- read.csv("inputs/model/hist_car_vint_stock.csv",stringsAsFactors = FALSE)

car_pop_dt <- read.csv("inputs/data/annual-age-distribution-of-car.csv",stringsAsFactors = FALSE,check.names = FALSE)
colnames(car_pop_dt) <- rename_values(colnames(car_pop_dt), list(Year="year",Value="number",Age="age_year"))
car_pop_dt$Age <- as.numeric(substring(car_pop_dt$Age,0,as.numeric(regexpr(pattern="-",car_pop_dt$Age))-1))
mat_age_car_pop <- acast(data=car_pop_dt, Age ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
age_tbc <- 0:20
mat_vint_stock <- matrix(0,nrow=length(unique(vh_techno$Technology)),ncol=length(age_tbc),dimnames = list(unique(vh_techno$Technology),age_tbc))
#Out of vintaged stock
mat_vint_stock_list <- list()
#Initialize the vintaged stock
#ASSUMPTION: Asume ICEV-G are distibuted and all other are new. Adjust to total stock of private car
mat_vint_stock["ICEV-G",as.character(0:20)] <- round(mat_age_car_pop[,"2005"]/sum(mat_age_car_pop[,"2005"])*mat_onroad_pop["ICEV-G","2005"])
mat_vint_stock[rownames(mat_onroad_pop)!="ICEV-G","0"] <- mat_onroad_pop[rownames(mat_onroad_pop)!="ICEV-G","2005"]
mat_vint_stock_list[["2005"]] <- mat_vint_stock
#Adjust sales to new registration minus private car sales
for (year in 2006:2019){
  #Create matrix of survival rates
  surv_rate_matrix <- diag(x=sapply(1:max(age_tbc), function (x) do.call(survival_rate_f,list(mode="Private car",age=x, year=year,cumulative_rate="n",scrappage_rate="n"))))
  dimnames(surv_rate_matrix) <- list(1:max(age_tbc),1:max(age_tbc))
  #Create matrix vintaged stock
  mat_vint_stock <- matrix(0,nrow=length(unique(vh_techno$Technology)),ncol=length(age_tbc),dimnames = list(unique(vh_techno$Technology),age_tbc))
  #Update old stock based on previous year matrix stock and survival rates
  mat_vint_stock[,as.character(1:max(age_tbc))] <- round(mat_vint_stock_list[[as.character(year-1)]][rownames(mat_vint_stock),as.character(0:(max(age_tbc)-1))] %*% surv_rate_matrix)
  #Udpate sales based on total stock by technology
  mat_vint_stock[,"0"] <- mat_onroad_pop[rownames(mat_vint_stock),as.character(year)] - rowSums(mat_vint_stock)
  #IF sales are negative, inconsistencies to solve
  if (any(mat_vint_stock[,"0"] <0)){
    #Assumption: Adjust old stock proportionally
    mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc))] <- mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc))] +
      round((diag(x=mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(0)],nrow = length(which(mat_vint_stock[,"0"]<0))) %*%
               (diag(x=1/vapply(rowSums(mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc)),drop=FALSE]),function(x)ifelse(x==0,1,x),FUN.VALUE = 1),nrow = length(which(mat_vint_stock[,"0"]<0))) %*%
                  mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc))])))
    #Assumption: No sales of these technologies
    mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(0)] <- 0
  }
  #Adjust total sales with private car sales
  adj_private_hire_car_sales <- mat_age_car_pop["0",as.character(year)]-sum(subset(car_dt,Age==0 & Year==year)$Value)
  if (adj_private_hire_car_sales>sum(mat_vint_stock[,"0"])){
    #Adjust sales with
    mat_vint_stock[,"0"] <- round(mat_vint_stock[,"0"]/sum(mat_vint_stock[,"0"])*adj_private_hire_car_sales)
    #Adjust old stock
    mat_vint_stock[,as.character(1:max(age_tbc))] <- round((diag(x=vapply(mat_onroad_pop[rownames(mat_vint_stock),as.character(year)]-mat_vint_stock[,as.character(0)],function(x)ifelse(x<0,0,x),FUN.VALUE = 1),nrow = nrow(mat_vint_stock)) %*%
                                                              (diag(x=1/vapply(rowSums(mat_vint_stock[,as.character(1:max(age_tbc)),drop=FALSE]),function(x)ifelse(x==0,1,x),FUN.VALUE = 1),nrow = nrow(mat_vint_stock)) %*%
                                                                 mat_vint_stock[,as.character(1:max(age_tbc))])))
  }
  #Update list
  mat_vint_stock_list[[as.character(year)]] <- mat_vint_stock
}

#Save list in Rdata
saveRDS(mat_vint_stock_list,file="inputs/model/private_hire_hist_vint_stock.RDS")
#Save dataframe that summarize the list in dataframe
out_hist_car_vint_stock <- NULL
for (i in names(mat_vint_stock_list)){
  tmp_stock_dt <- as.data.frame(mat_vint_stock_list[[i]]) %>% 
    cbind(Technology=rownames(mat_vint_stock_list[[i]]),stringsAsFactors = FALSE) %>% 
    gather("Age","Value",-Technology,convert=TRUE) %>%
    cbind(Year=as.numeric(i))
  #Update output 
  out_hist_car_vint_stock <- rbind(out_hist_car_vint_stock,tmp_stock_dt)
}
write.csv(out_hist_car_vint_stock,"inputs/model/hist_hire_car_vint_stock.csv",row.names = FALSE)

# TAXI --------------------------------------------------------------------

#Total historical stock by technology
#Input
first_yr <- 2005
last_yr <- 2019
vh_techno <- get_input_f(input_name = 'model_matching_vehicle_technology')
onroad_car_pop <- read.csv("inputs/data/annual-motor-vehicle-population-by-type-of-fuel-used.csv",stringsAsFactors = FALSE)
#Format input
onroad_car_pop$Transport_mode <- get_matching_names(onroad_car_pop$type,matching_type="passenger_transport_mode",original_source = "Type", matched_source = "Mode")
onroad_car_pop$Technology <- get_matching_names(onroad_car_pop$engine,matching_type="vehicle_technology",original_source = "LTA", matched_source = "Technology")
colnames(onroad_car_pop) <- rename_values(colnames(onroad_car_pop), list(Year="year",Value="number"))
out_onroad_car_pop <- aggregate(formula=Value~Year+Transport_mode+Technology,data=subset(onroad_car_pop,Transport_mode!=""),FUN=sum)
#Create output matrix
mat_onroad_pop <- matrix(0,nrow=length(unique(vh_techno$Technology)),ncol=(last_yr-first_yr+1),dimnames = list(unique(vh_techno$Technology),first_yr:last_yr))
#Get incomplete matrix from data
in_mat_onroad_pop <- acast(data=subset(onroad_car_pop,Transport_mode=="Taxi"), Technology ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
#Fill matrix with incomplete data
mat_onroad_pop[rownames(in_mat_onroad_pop),colnames(in_mat_onroad_pop)] <- in_mat_onroad_pop
#Fill 2005 data.
#ASSUMPTION: Similar technology share in 2006 and 2005
mat_onroad_pop[,"2005"] <- round(mat_onroad_pop[,"2006"]/mat_tot_veh_pop["Taxi","2006"]*mat_tot_veh_pop["Taxi","2005"])


#Calculate vintaged stock
#REgistration data 
reg_dt <- read.csv("inputs/data/new-registration-of-motor-vehicles-under-vehicle-quota-system-vqs.csv",stringsAsFactors = FALSE)
car_pop_dt <- read.csv("inputs/data/annual-age-distribution-of-car.csv",stringsAsFactors = FALSE,check.names = FALSE)
colnames(car_pop_dt) <- rename_values(colnames(car_pop_dt), list(Year="year",Value="number",Age="age_year"))
car_pop_dt$Age <- as.numeric(substring(car_pop_dt$Age,0,as.numeric(regexpr(pattern="-",car_pop_dt$Age))-1))
mat_age_car_pop <- acast(data=car_pop_dt, Age ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
age_tbc <- 0:20
mat_vint_stock <- matrix(0,nrow=length(unique(vh_techno$Technology)),ncol=length(age_tbc),dimnames = list(unique(vh_techno$Technology),age_tbc))
#Out of vintaged stock
mat_vint_stock_list <- list()
#Initialize the vintaged stock
#ASSUMPTION: Assume that all other technologies than ICEV-G are new
ref_techno="ICEV-D"
#ASSUMPTION: Age distribution of 
mat_vint_stock[ref_techno,as.character(1:max(age_tbc))] <- round(mat_age_car_pop[,"2005"]/sum(mat_age_car_pop[,"2005"])*mat_tot_veh_pop["Taxi","2005"])[-1]
mat_vint_stock[rownames(mat_vint_stock)!=ref_techno,"0"] <- mat_onroad_pop[rownames(mat_vint_stock)[rownames(mat_vint_stock)!=ref_techno],"2005"]
mat_vint_stock[ref_techno,"0"] <- mat_tot_veh_pop["Taxi","2005"] - sum(mat_vint_stock)
mat_vint_stock_list[["2005"]] <- mat_vint_stock
for (year in 2006:2019){
  #Create matrix of survival rates
  surv_rate_matrix <- diag(x=sapply(1:max(age_tbc), function (x) do.call(survival_rate_f,list(mode="Private car",age=x, year=year,cumulative_rate="n",scrappage_rate="n"))))
  dimnames(surv_rate_matrix) <- list(1:max(age_tbc),1:max(age_tbc))
  #Create matrix vintaged stock
  mat_vint_stock <- matrix(0,nrow=length(unique(vh_techno$Technology)),ncol=length(age_tbc),dimnames = list(unique(vh_techno$Technology),age_tbc))
  #Update old stock based on previous year matrix stock and survival rates
  mat_vint_stock[,as.character(1:max(age_tbc))] <- round(mat_vint_stock_list[[as.character(year-1)]][rownames(mat_vint_stock),as.character(0:(max(age_tbc)-1))] %*% surv_rate_matrix)
  #Udpate sales based on total stock by technology. First, estimate from difference 
  mat_vint_stock[,"0"] <- mat_onroad_pop[rownames(mat_vint_stock),as.character(year)] - rowSums(mat_vint_stock)
  #For negative values, force to 0 the sales. Inconsistency in 2013.
  if (any(mat_vint_stock[,"0"]<0) & year!=2013){
    mat_vint_stock[mat_vint_stock[,"0"]<0,"0"] <- 0
  } else if (year==2013){
    mat_vint_stock[mat_vint_stock[,"0"]<0,"0"] <- 0
    mat_vint_stock["ICEV-D","0"] <- 2200
  }
  #Adjust the total sales to match number of new registration
  mat_vint_stock[,"0"] <- round(mat_vint_stock[,"0"]/sum(mat_vint_stock[,"0"])*reg_dt[reg_dt$year==year & reg_dt$category=="Taxis","number"])
  #Create a matrix of difference in stock between wanted and estimated
  mat_diff_stock <- matrix(mat_onroad_pop[rownames(mat_vint_stock),as.character(year)]-rowSums(mat_vint_stock),dimnames = list(rownames(mat_vint_stock),"Difference"))
  #If negative values, then more estimated than projected. Reduce proportionally the old stock
  if (any(mat_diff_stock<0)){
    #Reduce lod stock
    mat_vint_stock[mat_diff_stock<0,as.character(1:max(age_tbc))] <- round(mat_vint_stock[mat_diff_stock<0,as.character(1:max(age_tbc))] +
                                                                             (diag(x=mat_diff_stock[mat_diff_stock<0,],nrow = length(which(mat_diff_stock<0))) %*% 
                                                                                (diag(x=1/vapply(rowSums(mat_vint_stock[mat_diff_stock<0,as.character(1:max(age_tbc)),drop=FALSE]),function(x)ifelse(x==0,1,x),FUN.VALUE = 1),nrow = length(which(mat_diff_stock<0))) %*% 
                                                                                   mat_vint_stock[mat_diff_stock<0,as.character(1:max(age_tbc))])))
    #If negative values, force to 0
    mat_vint_stock[mat_vint_stock<0] <- 0 
  }
  #Update list
  mat_vint_stock_list[[as.character(year)]] <- mat_vint_stock
}

taxi_hist_vint_stock <- mat_vint_stock_list
#Save list in Rdata
saveRDS(taxi_hist_vint_stock,file="inputs/model/taxi_hist_vint_stock.RDS")
#Save dataframe that summarize the list in dataframe
out_hist_car_vint_stock <- NULL
for (i in names(taxi_hist_vint_stock)){
  tmp_stock_dt <- as.data.frame(taxi_hist_vint_stock[[i]]) %>% 
    cbind(Technology=rownames(taxi_hist_vint_stock[[i]]),stringsAsFactors = FALSE) %>% 
    gather("Age","Value",-Technology,convert=TRUE) %>%
    cbind(Year=as.numeric(i))
  #Update output 
  out_hist_car_vint_stock <- rbind(out_hist_car_vint_stock,tmp_stock_dt)
}
write.csv(out_hist_car_vint_stock,"inputs/model/hist_taxi_vint_stock.csv",row.names = FALSE)



# MOTORCYCLE --------------------------------------------------------------
# Total historical stock by technology
#Input
first_yr <- 2005
last_yr <- 2019
vh_techno <- get_input_f(input_name = 'model_matching_moto_technology')
onroad_car_pop <- read.csv("inputs/data/annual-motor-vehicle-population-by-type-of-fuel-used.csv",stringsAsFactors = FALSE)
#Format input
onroad_car_pop$Transport_mode <- get_matching_names(onroad_car_pop$type,matching_type="passenger_transport_mode",original_source = "Type", matched_source = "Mode")
onroad_car_pop$Technology <- get_matching_names(onroad_car_pop$engine,matching_type="moto_technology",original_source = "LTA", matched_source = "Technology")
colnames(onroad_car_pop) <- rename_values(colnames(onroad_car_pop), list(Year="year",Value="number"))
out_onroad_car_pop <- aggregate(formula=Value~Year+Transport_mode+Technology,data=subset(onroad_car_pop,Transport_mode!=""),FUN=sum)
#Create output matrix
mat_onroad_pop <- matrix(0,nrow=length(unique(vh_techno$Technology)),ncol=(last_yr-first_yr+1),dimnames = list(unique(vh_techno$Technology),first_yr:last_yr))
#Get incomplete matrix from data
in_mat_onroad_pop <- acast(data=subset(onroad_car_pop,Transport_mode=="Motorcycle"), Technology ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
#Fill matrix with incomplete data
mat_onroad_pop[rownames(in_mat_onroad_pop),colnames(in_mat_onroad_pop)] <- in_mat_onroad_pop
#Fill 2005 data.
#ASSUMPTION: Similar technology share in 2006 and 2005
mat_onroad_pop[,"2005"] <- round(mat_onroad_pop[,"2006"]/subset(out_veh_pop_dt,Transport_mode=="Motorcycle" & Year==2006)$Value*subset(out_veh_pop_dt,Transport_mode=="Motorcycle" & Year==2005)$Value)

#Calculate vintaged stock
car_pop_dt <- read.csv("inputs/data/annual-age-distribution-of-motorcycles.csv",stringsAsFactors = FALSE,check.names = FALSE)
colnames(car_pop_dt) <- rename_values(colnames(car_pop_dt), list(Year="year",Value="number",Age="age_year"))
car_pop_dt$Age <- as.numeric(substring(car_pop_dt$Age,0,as.numeric(regexpr(pattern="-",car_pop_dt$Age))-1))
mat_age_car_pop <- acast(data=car_pop_dt, Age ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)

age_tbc <- 0:20
mat_vint_stock <- matrix(0,nrow=length(unique(vh_techno$Technology)),ncol=length(age_tbc),dimnames = list(unique(vh_techno$Technology),age_tbc))
#Out of vintaged stock
mat_vint_stock_list <- list()
#Initialize the vintaged stock
#ASSUMPTION: Assume that all other technologies than ICEV-G are new
mat_vint_stock["ICEM-G",as.character(1:20)] <- mat_age_car_pop[-1,"2005"]
mat_vint_stock[rownames(mat_vint_stock)!="ICEM-G","0"] <- mat_onroad_pop[rownames(mat_vint_stock)[rownames(mat_vint_stock)!="ICEM-G"],"2005"]
mat_vint_stock["ICEM-G","0"] <- mat_age_car_pop["0","2005"] - sum(mat_vint_stock[rownames(mat_vint_stock)!="ICEM-G","0"])
mat_vint_stock_list[["2005"]] <- mat_vint_stock
for (year in 2006:2019){
  #Create matrix of survival rates. Careful: No 2018 data, so assume 2017
  year_tbc <- as.numeric(switch(as.character(year),"2018"="2017",year))
  surv_rate_matrix <- diag(x=sapply(1:max(age_tbc), function (x) do.call(survival_rate_f,list(mode="Motorcycle",age=x, year=year_tbc,cumulative_rate="n",scrappage_rate="n"))))
  dimnames(surv_rate_matrix) <- list(1:max(age_tbc),1:max(age_tbc))
  #Create matrix vintaged stock
  mat_vint_stock <- matrix(0,nrow=length(unique(vh_techno$Technology)),ncol=length(age_tbc),dimnames = list(unique(vh_techno$Technology),age_tbc))
  #Update old stock based on previous year matrix stock and survival rates
  mat_vint_stock[,as.character(1:max(age_tbc))] <- round(mat_vint_stock_list[[as.character(year-1)]][rownames(mat_vint_stock),as.character(0:(max(age_tbc)-1))] %*% surv_rate_matrix)
  #Udpate sales based on total stock by technology
  mat_vint_stock[,"0"] <- mat_onroad_pop[rownames(mat_vint_stock),as.character(year)] - rowSums(mat_vint_stock)
  #IF sales are negative, inconsistencies to solve
  if (any(mat_vint_stock[,"0"] <0)){
    #Assumption: Adjust old stock proportionally
    mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc))] <- mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc))] +
      round((diag(x=mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(0)],nrow = length(which(mat_vint_stock[,"0"]<0))) %*%
               (diag(x=1/vapply(rowSums(mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc)),drop=FALSE]),function(x)ifelse(x==0,1,x),FUN.VALUE = 1),nrow = length(which(mat_vint_stock[,"0"]<0))) %*%
                  mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc))])))
    #Assumption: No sales of these technologies
    mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(0)] <- 0
  }
  #Update list
  mat_vint_stock_list[[as.character(year)]] <- mat_vint_stock
}

#Save
moto_hist_vint_stock <- mat_vint_stock_list
#Save list in Rdata
saveRDS(moto_hist_vint_stock,file="inputs/model/moto_hist_vint_stock.RDS")
#Save dataframe that summarize the list in dataframe
out_hist_car_vint_stock <- NULL
for (i in names(moto_hist_vint_stock)){
  tmp_stock_dt <- as.data.frame(moto_hist_vint_stock[[i]]) %>% 
    cbind(Technology=rownames(moto_hist_vint_stock[[i]]),stringsAsFactors = FALSE) %>% 
    gather("Age","Value",-Technology,convert=TRUE) %>%
    cbind(Year=as.numeric(i),Mode="Motorcycle")
  #Update output 
  out_hist_car_vint_stock <- rbind(out_hist_car_vint_stock,tmp_stock_dt)
}
write.csv(out_hist_car_vint_stock,"inputs/model/hist_moto_vint_stock.csv",row.names = FALSE)

