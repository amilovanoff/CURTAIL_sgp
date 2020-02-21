#Script to create historical vehicle technology market share
# 1) Total historical stock by transport mode --------------------------------

veh_pop <- read.csv("inputs/data/annual-motor-vehicle-population-by-vehicle-type.csv",stringsAsFactors = FALSE)
veh_pop$Transport_mode <- get_matching_names(veh_pop$type,matching_type="passenger_transport_mode",original_source = "Type", matched_source = "Mode")
colnames(veh_pop) <- rename_values(colnames(veh_pop), list(Year="year",Value="number"))
out_veh_pop_dt <- aggregate(formula=Value~Year+Transport_mode,data=subset(veh_pop,Transport_mode!=""),FUN=sum)
mat_tot_veh_pop <- acast(data=subset(out_veh_pop_dt), Transport_mode ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
#Update the total population with other data from Singstat
annual_veh_pop <- read.csv("inputs/data/annual_car_motorcyle_population_hist.csv",stringsAsFactors = FALSE,check.names = FALSE)
for (year in 2005:2017){
  #Update values for private cars
  mat_tot_veh_pop["Private car",as.character(year)] <- subset(annual_veh_pop,Year==year)$Private_car
  mat_tot_veh_pop["Private hire car",as.character(year)] <- subset(annual_veh_pop,Year==year)$Hire_car
}

#write.csv(out_veh_pop_dt,"inputs/model/historical_vehicle_population_mode.csv",row.names = FALSE)

# 2) Total historical stock by technology for car----------------------------------------------------------------------

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

# 3) Calculate vintaged stock for private hire car -----------------------------
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




