# Imputation
library(mice)
library(dplyr)
data = read.csv("fullData.csv")

getDataset = function(impute = FALSE, colnames, numImputations = 10, data = NA, seed = 556){
  if(impute == TRUE){
    sets = mice(data, m = numImputations, seed = seed)
    currData = complete(sets, action = "long")
    dta = currData %>% select(all_of(colnames))
  }
  else{
    dta = data %>% select(all_of(colnames))
    ind = complete.cases(dta)
    dta = dta[ind,]
  }
  return(dta)
}



# keep new transformed variables and remove old ones
names = colnames(data)
Allnames = c("ptp3_yn", ".imp","age", "race7", "ethnic3", "sex2", "sig_other","tobacco", "alcohol", 
          "drugs", "MedianIncomeForZip", "PercentAboveHighSchoolEducationForZip", 
          "PercentAboveBachelorsEducationForZip", "payertype", "tbiS02",                               
          "tbiS06", "tbiS09", "ptp1_yn", "ptp2_yn", "ptp0_yn", "ed_yn", "icu", 
          "delirium", "agitated", "lethargic","comatose", "disoriented", 
          "gcs_min", "gcs_max", "adl_min", "adl_max", "mobility_min", "mobility_max",                         
          "los_total", "dc_setting", "prehosp", "posthosp", "summaryGCS",                           
          "summaryADL", "summaryMOB", "loglos_total", "PABEFZ",                               
          "PAHSEFZ", "logtbiS02", "logtbiS06", "logtbiS09")
names1 = c("ptp3_yn", ".imp","age", "race7", "ethnic3", "sex2", "sig_other","tobacco", "alcohol", 
           "drugs", "MedianIncomeForZip", "payertype",                               
           "ptp1_yn", "ptp2_yn", "ed_yn", "icu", 
           "delirium", "agitated", "lethargic","comatose", "disoriented", 
           "dc_setting", "prehosp", "posthosp", "summaryGCS",                           
           "summaryADL", "summaryMOB", "loglos_total", "PABEFZ",                               
           "PAHSEFZ", "logtbiS02", "logtbiS06", "logtbiS09")
namesTest = c("ptp3_yn","age", "race7", "ethnic3", "sex2", "sig_other","tobacco", "alcohol", 
           "drugs", "payertype",                               
           "ptp1_yn", "ptp2_yn", "ed_yn", "icu", 
           "delirium", "agitated", "lethargic","comatose", "disoriented", 
           "dc_setting", "prehosp", "posthosp", "ptp3_yn", "summaryGCS",                           
           "summaryADL", "summaryMOB", "loglos_total", "PABEFZ",                               
           "PAHSEFZ", "logtbiS02", "logtbiS06", "logtbiS09")

#data1 = getDataset(colnames = names1, data = data)
data2 = getDataset(colnames = namesTest, data = data)
data3 = getDataset(colnames = names1, data = data, impute = TRUE, numImputations = 2)

CrossEntropyLoss = function(probabilities,truth){
  return(mean((truth*log(probabilities)) + (1-truth)*log(1-probabilities)))
}



