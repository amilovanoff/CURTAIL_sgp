source("model_script_run.R")
modelframework::load_input_data_f()
res <- do.call(transport_activity_f,list())
transport_pkt <- res[["transport_pkt"]]
tot_transport_pkt <- aggregate(formula=Value~Year,data=transport_pkt,FUN=sum)
#Convert thousand pkt
tot_transport_pkt$Value <- tot_transport_pkt$Value*10^3
#Delete projections
tot_transport_pkt <- subset(tot_transport_pkt,Year<=2018)
#GEt population data
pop_dt <- read.csv("inputs/data/population_singapore.csv",stringsAsFactors = FALSE, check.names = FALSE)
#Get GDP data
gdp_dt <- read.csv("inputs/data/gdp_singapore.csv",stringsAsFactors = FALSE, check.names = FALSE)

#
tot_transport_pkt$Population <- sapply(1:nrow(tot_transport_pkt),function(x)subset(pop_dt,Year==tot_transport_pkt[x,"Year"] & Scenario=="Medium")$Value)
tot_transport_pkt$GDP <- sapply(1:nrow(tot_transport_pkt),function(x)subset(gdp_dt,Year==tot_transport_pkt[x,"Year"])$Value)

lin_reg <- lm(formula=Value~Population+GDP,data=tot_transport_pkt)
summary(lin_reg)
