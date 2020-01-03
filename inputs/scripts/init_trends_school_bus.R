#Script to establish trends in school bus PKT
source("model_script_run.R")
modelframework::load_input_data_f()
modelframework::load_attribute_value()
transport <- do.call(transport_activity_f,list())
res <- transport$get_list_dataframe()
transport_pkt <- res[["transport_pkt"]]
pkt_dt <- subset(transport_pkt,Mode=="School bus" & Year<2019)

#Get input data of Singaporean population by age
pop_dt <- read.csv("inputs/data/historical_population_age.csv",stringsAsFactors = FALSE, check.names = FALSE)
long_pop_dt <- gather(data=pop_dt,key="Year",value="Value",-c(Age),convert=TRUE)
age_tbc <- c(" 5-9 Years "," 10-14 Years "," 15-19 Years ")
pkt_dt$Population <- sapply(1:nrow(pkt_dt),function(x)sum(subset(long_pop_dt,Age%in%age_tbc & Year==pkt_dt[x,"Year"])$Value))

lin_reg <- lm(Value~Population,data=pkt_dt)
summary(lin_reg)
