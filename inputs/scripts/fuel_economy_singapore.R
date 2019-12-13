library(readxl)
#@Format a csv file from .pdf format
sheet_name_list <- excel_sheets("inputs/data/fuel_economy_data_singapore_191102.xlsx")
#Output file
dt_col <- c("Make",
            "Model",
            "Body",
            "Enginee_cc",
            "MPO_kw",
            "Fuel",
            "Transmission",
            "Turbo_supercharged",
            "Hybrid",
            "fe_urban",
            "fe_xtra_urban",
            "fe_combined",
            "co2_urban",
            "co2_xtra_urban",
            "co2_combined",
            "ves_band")
out_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)

for (sheet in sheet_name_list){
  dt <- as.data.frame(read_xlsx(path="inputs/data/fuel_economy_data_singapore_191102.xlsx",sheet=sheet,.name_repair = "minimal",col_names = FALSE))
  #Get make
  make <- substring(grep("Make :",dt[,1],value = TRUE),as.numeric(regexpr(pattern=" : ",grep("Make :",dt[,1],value = TRUE)))+3,5000)
  #Obtain rows with colnames
  row_colname <- which(dt[,1]=="Model")
  #Obtain the colnames without na
  cols_colnames <- which(!is.na(dt[row_colname,]))
  #
  for (i in (row_colname+1):nrow(dt)){
    tmp_out_dt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 1),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
    tmp_out_dt$Make <- make
    for (j in 1:(ncol(tmp_out_dt)-1)){
      tmp_out_dt[1,j+1] <- dt[i,cols_colnames[j]]
    }
    out_dt <- rbind(out_dt,tmp_out_dt)
  }
  
}
#Format data
formated_out_dt <-na.omit(out_dt)
formated_out_dt$Body <- rename_values(formated_out_dt$Body,list(Hatchback=c("Hatchback"),
                                        Sedan=c("Sedan/Saloon"),
                                        'Multi-purpose Vehicle/Station-wagon'=c("Station-wago","Multi-purpos"),
                                        'Sports Utility Vehicle'=c("Sport"),
                                        'Coupe/ Convertible'=c("Coupe","Convertible")))

write.csv(formated_out_dt,"inputs/model/fuel_economy_singapore.csv",row.names = FALSE)

