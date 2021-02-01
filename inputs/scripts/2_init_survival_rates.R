###Script that calculates survival rates of private cars, motorcycles and bus
source("utils/data_processing_f.R")

#Private car
car_pop_dt <- read.csv("inputs/data/annual-age-distribution-of-car.csv",stringsAsFactors = FALSE,check.names = FALSE)
colnames(car_pop_dt) <- rename_values(colnames(car_pop_dt), list(Year="year",Value="number",Age="age_year"))
car_pop_dt$Age <- as.numeric(substring(car_pop_dt$Age,0,as.numeric(regexpr(pattern="-",car_pop_dt$Age))-1))
mat_car_pop <- acast(data=car_pop_dt, Age ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)

mat_car_survival <- matrix(0,nrow=nrow(mat_car_pop),ncol=ncol(mat_car_pop)-1,dimnames = list(rownames(mat_car_pop),colnames(mat_car_pop)[-1]))
for (i in 1:ncol(mat_car_survival)){
  mat_car_survival[rownames(mat_car_survival)[-1],i] <- mat_car_pop[rownames(mat_car_survival)[-1],colnames(mat_car_survival)[i]]/mat_car_pop[rownames(mat_car_survival)[-nrow(mat_car_survival)],as.character(as.numeric(colnames(mat_car_survival)[i])-1)]
}
#Age zero a 1 survival
mat_car_survival <- mat_car_survival[as.character(1:19),]
#Force negative values t 0
mat_car_survival[mat_car_survival>1] <- 1
#Calculate the lowest, median and highest distribution
cum_survival <- sapply(colnames(mat_car_survival),function(x)cumprod(mat_car_survival[,x]))["10",as.character(2006:2019)]
min_dis_yr <- as.character(2006:2019)[which.min(cum_survival)]
max_dis_yr <- as.character(2006:2019)[which.max(cum_survival)]
def_dis_yr <- as.character(2006:2019)[which.min(abs(cum_survival-median(cum_survival)))]

#Save output
car_survival_rate_dt <- as.data.frame(mat_car_survival) %>% 
  cbind(Age=as.numeric(rownames(mat_car_survival)),stringsAsFactors = FALSE) %>% 
  gather("Year","Value",-Age,convert=TRUE) %>% 
  cbind(Mode="Private car",Data="Annual survival rate",Source="LTA",stringsAsFactors = FALSE)
car_survival_rate_dt[car_survival_rate_dt$Year==min_dis_yr,"Model"] <- "min"
car_survival_rate_dt[car_survival_rate_dt$Year==max_dis_yr,"Model"] <- "max"
car_survival_rate_dt[car_survival_rate_dt$Year==2018,"Model"] <- "def"

#Motorcycle
car_pop_dt <- read.csv("inputs/data/annual-age-distribution-of-motorcycles.csv",stringsAsFactors = FALSE,check.names = FALSE)
colnames(car_pop_dt) <- rename_values(colnames(car_pop_dt), list(Year="year",Value="number",Age="age_year"))
car_pop_dt$Age <- as.numeric(substring(car_pop_dt$Age,0,as.numeric(regexpr(pattern="-",car_pop_dt$Age))-1))
mat_age_car_pop <- acast(data=car_pop_dt, Age ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)

mat_car_survival <- matrix(0,nrow=nrow(mat_age_car_pop),ncol=ncol(mat_age_car_pop)-1,dimnames = list(rownames(mat_age_car_pop),colnames(mat_age_car_pop)[-1]))
for (i in 1:ncol(mat_car_survival)){
  mat_car_survival[rownames(mat_car_survival)[-1],i] <- mat_age_car_pop[rownames(mat_car_survival)[-1],colnames(mat_car_survival)[i]]/mat_age_car_pop[rownames(mat_car_survival)[-nrow(mat_car_survival)],as.character(as.numeric(colnames(mat_car_survival)[i])-1)]
}
#Age zero a 1 survival
mat_car_survival <- mat_car_survival[as.character(1:19),]
#Force negative values t 0
mat_car_survival[mat_car_survival>1] <- 1
#Calculate the lowest, median and highest distribution
cum_survival <- sapply(colnames(mat_car_survival),function(x)cumprod(mat_car_survival[,x]))["19",as.character(2006:2019)]
min_dis_yr <- as.character(2006:2019)[which.min(cum_survival)]
max_dis_yr <- as.character(2006:2019)[which.max(cum_survival)]
def_dis_yr <- as.character(2006:2019)[which.min(abs(cum_survival-median(cum_survival)))]

#Save output
moto_survival_rate_dt <- as.data.frame(mat_car_survival) %>% 
  cbind(Age=as.numeric(rownames(mat_car_survival)),stringsAsFactors = FALSE) %>% 
  gather("Year","Value",-Age,convert=TRUE) %>%
  cbind(Mode="Motorcycle", Data="Annual survival rate",Source="LTA",stringsAsFactors = FALSE)
moto_survival_rate_dt[moto_survival_rate_dt$Year==min_dis_yr,"Model"] <- "min"
moto_survival_rate_dt[moto_survival_rate_dt$Year==max_dis_yr,"Model"] <- "max"
moto_survival_rate_dt[moto_survival_rate_dt$Year==2018,"Model"] <- "def"

#Bus
car_pop_dt <- read.csv("inputs/data/annual-age-distribution-of-bus.csv",stringsAsFactors = FALSE,check.names = FALSE)
colnames(car_pop_dt) <- rename_values(colnames(car_pop_dt), list(Year="year",Value="number",Age="age_years"))
car_pop_dt$Age <- as.numeric(substring(car_pop_dt$Age,0,as.numeric(regexpr(pattern="-",car_pop_dt$Age))-1))
mat_age_car_pop <- acast(data=car_pop_dt, Age ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)

mat_car_survival <- matrix(0,nrow=nrow(mat_age_car_pop),ncol=ncol(mat_age_car_pop)-1,dimnames = list(rownames(mat_age_car_pop),colnames(mat_age_car_pop)[-1]))
for (i in 1:ncol(mat_car_survival)){
  mat_car_survival[rownames(mat_car_survival)[-1],i] <- mat_age_car_pop[rownames(mat_car_survival)[-1],colnames(mat_car_survival)[i]]/mat_age_car_pop[rownames(mat_car_survival)[-nrow(mat_car_survival)],as.character(as.numeric(colnames(mat_car_survival)[i])-1)]
}
#Age zero a 1 survival
mat_car_survival <- mat_car_survival[as.character(1:19),]
#Force negative values t 0
mat_car_survival[mat_car_survival>1] <- 1
#Calculate the lowest, median and highest distribution
cum_survival <- sapply(colnames(mat_car_survival),function(x)cumprod(mat_car_survival[,x]))["19",as.character(2006:2019)]
min_dis_yr <- as.character(2006:2019)[which.min(cum_survival)]
max_dis_yr <- as.character(2006:2019)[which.max(cum_survival)]
def_dis_yr <- as.character(2006:2019)[which.min(abs(cum_survival-median(cum_survival)))]
#Save output
bus_survival_rate_dt <- as.data.frame(mat_car_survival) %>% 
  cbind(Age=as.numeric(rownames(mat_car_survival)),stringsAsFactors = FALSE) %>% 
  gather("Year","Value",-Age,convert=TRUE) %>% 
  cbind(Mode="Bus", Data="Annual survival rate",Source="LTA",stringsAsFactors = FALSE)
bus_survival_rate_dt[bus_survival_rate_dt$Year==min_dis_yr,"Model"] <- "min"
bus_survival_rate_dt[bus_survival_rate_dt$Year==max_dis_yr,"Model"] <- "max"
bus_survival_rate_dt[bus_survival_rate_dt$Year==2016,"Model"] <- "def"

#Combine final output
survival_rate_dt <- rbind(car_survival_rate_dt,moto_survival_rate_dt,bus_survival_rate_dt)
write.csv(survival_rate_dt,"inputs/model/survival_rate_singapore.csv",row.names = FALSE)
