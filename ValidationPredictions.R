
#train <- read.csv("C:/Users/Ronnie/Downloads/tbistaa556_training.csv")
#validate <- read.csv("C:/Users/Ronnie/Downloads/tbistaa556_validate.csv")

train = read.csv("tbistaa556_training.csv")
validate = read.csv("tbistaa556_validate.csv")
v = validate

library(ggplot2)
library(dplyr)
library(tidyverse)
library(glmnet)
library(randomForest)
library(ggcorrplot)
library(mice)
library(xgboost)
library(dbarts)
library(BART)
library(knitr)

names1 = c("age", "race7", "ethnic3", "sex2", "sig_other","tobacco", "alcohol", "drugs", "MedianIncomeForZip", "payertype", "ptp1_yn", "ptp2_yn", "ptp0_yn", "ed_yn", "icu", "delirium", "agitated", "lethargic","comatose", "disoriented","dc_setting", "prehosp", "posthosp", "gcs_min", "gcs_max", "adl_min", "adl_max", "mobility_min", "mobility_max", "loglos_total", "PABEFZ", "PAHSEFZ", "logtbiS02", "logtbiS06", "logtbiS09")

names2 = c("age", "race7", "sex2", "sig_other","tobacco", "alcohol", 
           "drugs", "MedianIncomeForZip", "payertype", "ptp1_yn", "ptp2_yn", "ptp0_yn", "ed_yn", "delirium", "dc_setting", "prehosp", "posthosp", "summaryADL", 
           "mobility_min", "mobility_max", "loglos_total",                               "PAHSEFZ", "logtbiS02", "logtbiS06", "logtbiS09")

names3 = c("age", "race7", "sex2", "sig_other","tobacco", "alcohol", 
           "drugs", "MedianIncomeForZip", "payertype","ptp1_yn", "ptp2_yn", "ptp0_yn", "ed_yn", "delirium", "disoriented","dc_setting", "prehosp", "posthosp", "summaryADL", "mobility_min", "mobility_max", "loglos_total",                     "PAHSEFZ", "PAHSEFZ", "logtbiS02", "logtbiS06", "logtbiS09")

train = train[,-1]
validate = validate[,-1]
train = train[,-36]
validate = validate[,-36]

# converting NA in the race column to unknown
indR = is.na(train$race7)
train$race7[indR] = 0
indS = is.na(validate$race7)
validate$race7[indS] = 0

trainY = train$ptp3_yn
train = train[,-36]

train = rbind(train, validate)


# performance summaries
train$summaryGCS = (train$gcs_min + train$gcs_max)/2
train$summaryADL = (train$adl_min + train$adl_max)/2
train$summaryMOB = (train$mobility_min + train$mobility_max)/2

# put ceiling on Percentages
train$PABEFZ = ifelse(train$PercentAboveBachelorsEducationForZip>1,train$PercentAboveBachelorsEducationForZip/100,train$PercentAboveBachelorsEducationForZip)
train$PAHSEFZ = ifelse(train$PercentAboveHighSchoolEducationForZip>1,train$PercentAboveHighSchoolEducationForZip/100,train$PercentAboveHighSchoolEducationForZip)

# log skewed right predictors
train$loglos_total = log(train$los_total+1)
train$logtbiS02 = log(train$tbiS02+1)
train$logtbiS06 = log(train$tbiS06+1)
train$logtbiS09 = log(train$tbiS09+1)
data = train



predictTest = function(impute = FALSE, colnames, data, Y, seed = 556, printImputer = FALSE){
  set.seed(seed)
  
  # selecting columns in colnames
  dta = data %>% select(all_of(colnames))
  
  # initializing list to return
  listReturn = list()
  
  # convert to factors
  for (col in names(dta)) {
    if (length(unique(dta[[col]])) < 8) {
      dta[[col]] = as.factor(dta[[col]])
    }
  }
  dta$dc_setting = as.factor(dta$dc_setting)
  #mf = model.frame(~.-1, data = dta, na.action = na.pass)
  #dta = model.matrix(~.-1, data = mf, na.action = na.pass)
  #dta = as.data.frame(dta)
  #return(dta)
  # If not imputing, remove rows with NA
  if(impute == FALSE){
    ind = complete.cases(dta)
    dta = dta[ind,]
  }
  
  # Splitting features and labels
  X = dta
    
  # split features and labels into train and test sets
  train = X[1:9044,]
  validate = X[9045:11309,]
    
    # imputing
    if(impute == TRUE){
      
      # imputing Xtrain
      imputeObj = mice(train, m = 1, seed = seed, printFlag = printImputer)
      #print(imputeObj$loggedEvents)
      train = complete(imputeObj, 1)
      # imputing Xtest using the already imputed Xtrain
      full = rbind(train, validate)
      fullImputeObj = mice(full, m = 1, maxit = 1, seed = seed, printFlag = printImputer)
      fullImputed = complete(fullImputeObj, 1)
      testStart = nrow(train)+1
      validate = fullImputed[testStart:nrow(fullImputed),]
    }
    Xtrain = train
    Ytrain = Y
    Xvalidate = validate
    # Append to the list that is returned
    listReturn[[1]] = Xtrain
    listReturn[[2]] = Ytrain
    listReturn[[3]] = Xvalidate
    return(listReturn)
  }

CrossEntropyLoss = function(probabilities,truth){
  probabilities <- pmin(pmax(probabilities, 1e-15), 1 - 1e-15)
  return(-mean((truth*log(probabilities)) + (1-truth)*log(1-probabilities)))
}

Imp1 = predictTest(colnames = names1, data = data, Y = trainY, impute = TRUE)



Xtrain = Imp1[[1]] 
Xtrain = model.matrix(~., data = Xtrain)
Ytrain = Imp1[[2]]
Xvalidate = Imp1[[3]]
Xvalidate = model.matrix(~., data = Xvalidate)

verbose = FALSE
nrounds = 259
params = list(objective = "binary:logistic", eta = 0.028635479810182, max_depth = 4, subsample = 0.809286485658959, colsample_bytree = 0.711043330607936, min_child_weight = 8.33287097513676, gamma = 0.595529325027019, lambda = 4.07533963210881)

trainXG1 = xgb.DMatrix(data = Xtrain, label = Ytrain)

trainFit1 = xgb.train(params = params, data = trainXG1, nrounds = nrounds, verbose = verbose)

# predict using the test set
probabilities1 = predict(trainFit1, Xvalidate)

classes1 = ifelse(probabilities1 > 0.5, 1, 0)




k = 1
power = 1.6752053586533
base = 0.821862926776521
ntree = 120
trainFit = bart(x.train = Xtrain, y.train = Ytrain, k = k,
                power = power, base = base, ntree = ntree,
                keeptrees = TRUE)

posteriorTest = predict(trainFit, newdata = Xvalidate)
probsTest = colMeans(posteriorTest)
classesTest = ifelse(probsTest > 0.5, 1, 0)

predictionsXGBoost = classes1
predictionsBART = classesTest
subj_id = v$subj_id

df = data.frame(subj_id, predictionsXGBoost, predictionsBART)

write.csv(df,"validationPredictions.csv")
