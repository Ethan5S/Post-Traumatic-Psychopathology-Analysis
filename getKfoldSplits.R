library(mice)
library(dplyr)

# data loaded from the end of LoadDataWithGraphs.R
data = read.csv("fullData.csv")

# all column names in data
#names = colnames(data)
Allnames = c("ptp3_yn","age", "race7", "ethnic3", "sex2", "sig_other","tobacco", "alcohol", 
             "drugs", "MedianIncomeForZip", "PercentAboveHighSchoolEducationForZip", 
             "PercentAboveBachelorsEducationForZip", "payertype", "tbiS02",                               
             "tbiS06", "tbiS09", "ptp1_yn", "ptp2_yn", "ptp0_yn", "ed_yn", "icu", 
             "delirium", "agitated", "lethargic","comatose", "disoriented", 
             "gcs_min", "gcs_max", "adl_min", "adl_max", "mobility_min", "mobility_max",                         
             "los_total", "dc_setting", "prehosp", "posthosp", "summaryGCS",                           
             "summaryADL", "summaryMOB", "loglos_total", "PABEFZ",                               
             "PAHSEFZ", "logtbiS02", "logtbiS06", "logtbiS09")

kfoldData = function(impute = FALSE, colnames, data, seed = 556, k = 5, printImputer = FALSE){
  set.seed(seed)
  
  # shuffiling rows of data frame
  ind = sample(nrow(data))
  data = data[ind,]
  
  # selecting columns in colnames
  dta = data %>% select(all_of(colnames))
  
  # initializing list to return
  listReturn = list()
  
  # If not imputing, remove rows with NA
  if(impute == FALSE){
    ind = complete.cases(dta)
    dta = dta[ind,]
  }
  
  # Splitting features and labels
  X = dta %>% select(-"ptp3_yn")
  Y = dta %>% select("ptp3_yn")
  
  # Number of rows per fold
  numPerFold = round(nrow(dta)/k)
  
  #loop through each split
  for(i in 1:k){
    
    # Get indices of train and test set for the ith iteration
    startInd = (i-1)*numPerFold+1
    endInd = ifelse(i == k, nrow(dta), i*numPerFold)
    testInd = startInd:endInd
    
    # split features and labels into train and test sets
    Xtest = X[testInd,]
    Xtrain = X[-testInd,]
    Ytest = Y[testInd,]
    Ytrain = Y[-testInd,]
    
    # imputing
    if(impute == TRUE){
      
      # imputing Xtrain
      imputeObj = mice(Xtrain, m = 1, seed = seed, printFlag = printImputer)
      #print(imputeObj$loggedEvents)
      Xtrain = complete(imputeObj, 1)
      # imputing Xtest using the already imputed Xtrain
      full = rbind(Xtrain, Xtest)
      fullImputeObj = mice(full, m = 1, maxit = 1, seed = seed, printFlag = printImputer)
      fullImputed = complete(fullImputeObj, 1)
      testStart = nrow(Xtrain)+1
      Xtest = fullImputed[testStart:nrow(fullImputed),]
    }
    # Append to the list that is returned
    listReturn[[4*i-3]] = Xtrain
    listReturn[[4*i-2]] = Ytrain
    listReturn[[4*i-1]] = Xtest
    listReturn[[4*i]] = Ytest
  }
  # naming elements in the list
  for(i in 1:k){
    start = 4*i-3
    end = start + 3
    names(listReturn)[start:end] = c(paste0("Xtrain",i), paste0("Ytrain",i),
                                     paste0("Xtest",i), paste0("Ytest",i))
  }
  return(listReturn)
}

names1 = c("ptp3_yn","age", "race7", "ethnic3", "sex2", "sig_other","tobacco", "alcohol", 
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

l1 = kfoldData(colnames = names1, data = data, impute = TRUE, k = 7)




CrossEntropyLoss = function(probabilities,truth){
  return(mean((truth*log(probabilities)) + (1-truth)*log(1-probabilities)))
}



