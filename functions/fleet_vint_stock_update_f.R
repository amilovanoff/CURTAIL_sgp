#' fleet_vint_stock_update_f
#' Function: Update the vintaged fleet stock by vehicle type in year year from previous year vintaged stock, new sales and can adjust with stock.
#' @import modelframework
#' @export
#Require: Previous year vintaged stock, current year sales by type, current year stock by type
fleet_vint_stock_update_f <- function(fleet,year){
  attribute_f("fleet_vint_stock_update_f")
  #Inputs
  age_tbc <- as.numeric(colnames(fleet$vint_stock[[as.character(year-1)]]))
  #Create matrix of survival rates
  surv_rate_matrix <- diag(x=sapply(1:max(age_tbc), function (x) do.call(survival_rate_f,list(mode=fleet$mode,age=x, year=year,cumulative_rate="n",scrappage_rate="n"))))
  dimnames(surv_rate_matrix) <- list(1:max(age_tbc),1:max(age_tbc))
  #Create matrix vintaged stock
  mat_vint_stock <- matrix(0,nrow=nrow(fleet$vint_stock[[as.character(year-1)]]),ncol=ncol(fleet$vint_stock[[as.character(year-1)]]),dimnames = dimnames(fleet$vint_stock[[as.character(year-1)]]))
  #Update old stock based on previous year matrix stock and survival rates
  mat_vint_stock[,as.character(1:max(age_tbc))] <- round(fleet$vint_stock[[as.character(year-1)]][rownames(mat_vint_stock),as.character(0:(max(age_tbc)-1))] %*% surv_rate_matrix)
  #Udpate sales based on total stock and market share
  mat_vint_stock[,"0"] <- round(fleet$technology_market_share[rownames(mat_vint_stock),as.character(year)] * (fleet$on_road_stock_tot["Total",as.character(year)] - sum(mat_vint_stock[,as.character(1:max(age_tbc))])))
  #if the total stock is lower than estimated from survival rates, adjust the sales
  if (any(mat_vint_stock[,"0"]<0)){
    #Reduce lod stock
    mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc))] <- round(mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc))] +
                                                                             (diag(x=mat_vint_stock[mat_vint_stock[,"0"]<0,"0"],nrow = length(which(mat_vint_stock[,"0"]<0))) %*% 
                                                                                (diag(x=1/vapply(rowSums(mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc)),drop=FALSE]),function(x)ifelse(x==0,1,x),FUN.VALUE = 1),nrow = length(which(mat_vint_stock[,"0"]<0))) %*% 
                                                                                   mat_vint_stock[mat_vint_stock[,"0"]<0,as.character(1:max(age_tbc))])))
    #If negative values, force to 0
    mat_vint_stock[mat_vint_stock<0] <- 0 
  }
  
  
  #Update fleet object
  fleet$vint_stock[[as.character(year)]] <- mat_vint_stock
  #Update the current year on-road stock and sales with actual values
  fleet$on_road_stock[rownames(mat_vint_stock),as.character(year)] <- rowSums(mat_vint_stock)
  fleet$sales[rownames(mat_vint_stock),as.character(year)] <- mat_vint_stock[,"0"]
  #Update matrix scrap
  matrix_scrap <- fleet$vint_stock[[as.character(year-1)]][rownames(mat_vint_stock),] - cbind(mat_vint_stock[,as.character(1:max(age_tbc))],0)
  dimnames(matrix_scrap) <- list(rownames(mat_vint_stock),1:(max(age_tbc)+1))
  fleet$vint_scrap[[as.character(year)]] <- trunc(matrix_scrap)
  return(fleet)
}
