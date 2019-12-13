library(reshape2)
#Calculate trends in fuel economy for considered models
fe_us_data <- read.csv("inputs/data/fuel_economy_dataset_us.csv",stringsAsFactors = FALSE)
conv <- read.csv("inputs/user/conversion_units.csv",stringsAsFactors = FALSE,row.names = 1,check.names = FALSE)
fe_us_data$comb08_fc <- 1/fe_us_data$comb08*conv["L","1 gal"]*conv["mile","1 km"]*100

#1) Honda Civic
tmp_fe_dt <- subset(fe_us_data,make=="Honda" & grepl("Civic",model) & atvType=="")
mat <- acast(data=subset(tmp_fe_dt,year>=1990), make + model + displ + cylinders ~ year , value.var='comb08_fc',fun.aggregate=mean, margins=FALSE)


#2) Toyota prius
tmp_fe_dt <- subset(fe_us_data,make=="Toyota" & grepl("Prius",model) & eng_dscr%in%c("HEV","Hybrid"))
mat <- acast(data=subset(tmp_fe_dt,year>=1990), make + model + displ + cylinders ~ year , value.var='comb08_fc',fun.aggregate=mean, margins=FALSE)
#Calculate ratio of improvements in the Fuel consumption of prius hybrid
(mat[,"2019"]-mat[1,"2005"])/mat[,"2019"]
#Results: 12% improvements from 2009 to 2019

fe_us_data <- read.csv("inputs/data/fuel_economy_dataset_us.csv",stringsAsFactors = FALSE)
make_tbc<-c("BMW","Mazda","Honda","Mercedes-Benz","Mitsubishi","Nissan","Subaru","Toyota")
fe_us_data <- subset(fe_us_data,make%in%make_tbc)

matrix <- acast(data=subset(fe_us_data,year>=1990 & fuelType1=="Regular Gasoline"), make + model ~ year , value.var='comb08',fun.aggregate=sum, margins=FALSE)
