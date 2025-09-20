library(mice)
library(dplyr)
library(xgboost)
library(dbarts)
library(BART)
library(ggplot2)

# data loaded from the end of LoadDataWithGraphs.R
data = read.csv("fullData.csv")

kfoldData = function(impute = FALSE, colnames, data, seed = 556, k = 5, printImputer = FALSE){
  set.seed(seed)
  
  # shuffiling rows of data frame
  ind = sample(nrow(data))
  data = data[ind,]
  
  # selecting columns in colnames
  dta = data %>% select(all_of(colnames))
  
  # initializing list to return
  listReturn = list()
  
  # convert to factors
  for (col in names(dta)) {
    if(col == "ptp3_yn"){
      next
    }
    if (length(unique(dta[[col]])) < 8) {
      dta[[col]] <- as.factor(dta[[col]])
    }
  }
  dta$dc_setting = as.factor(dta$dc_setting)
  mf <- model.frame(~ . - 1, data = dta, na.action = na.pass)
  dta <- model.matrix(~ . - 1, data = mf)
  dta = as.data.frame(dta)
  
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
Allnames = c("ptp3_yn","age", "race7", "ethnic3", "sex2", "sig_other","tobacco", "alcohol", 
             "drugs", "MedianIncomeForZip", "PercentAboveHighSchoolEducationForZip", 
             "PercentAboveBachelorsEducationForZip", "payertype", "tbiS02",                               
             "tbiS06", "tbiS09", "ptp1_yn", "ptp2_yn", "ptp0_yn", "ed_yn", "icu", 
             "delirium", "agitated", "lethargic","comatose", "disoriented", 
             "gcs_min", "gcs_max", "adl_min", "adl_max", "mobility_min", "mobility_max",                         
             "los_total", "dc_setting", "prehosp", "posthosp", "summaryGCS",                           
             "summaryADL", "summaryMOB", "loglos_total", "PABEFZ",                               
             "PAHSEFZ", "logtbiS02", "logtbiS06", "logtbiS09")

names1 = c("ptp3_yn","age", "race7", "ethnic3", "sex2", "sig_other","tobacco", "alcohol", 
           "drugs", "MedianIncomeForZip", "payertype",                               
           "ptp1_yn", "ptp2_yn", "ptp0_yn", "ed_yn", "icu", 
           "delirium", "agitated", "lethargic","comatose", "disoriented", 
           "dc_setting", "prehosp", "posthosp", "gcs_min", "gcs_max", "adl_min", 
           "adl_max", "mobility_min", "mobility_max", "loglos_total", "PABEFZ",                               
           "PAHSEFZ", "logtbiS02", "logtbiS06", "logtbiS09")

names2 = c("ptp3_yn","age", "race7", "sex2", "sig_other","tobacco", "alcohol", 
           "drugs", "MedianIncomeForZip", "payertype",                               
           "ptp1_yn", "ptp2_yn", "ptp0_yn", "ed_yn", 
           "delirium", "dc_setting", "prehosp", "posthosp", "summaryADL", 
           "mobility_min", "mobility_max", "loglos_total",                               
           "PAHSEFZ", "logtbiS02", "logtbiS06", "logtbiS09")

names3 = c("ptp3_yn","age", "race7", "sex2", "sig_other","tobacco", "alcohol", 
           "drugs", "MedianIncomeForZip", "payertype",                               
           "ptp1_yn", "ptp2_yn", "ptp0_yn", "ed_yn", 
           "delirium", "disoriented","dc_setting", "prehosp", "posthosp", "summaryADL", 
           "mobility_min", "mobility_max", "loglos_total",                               
           "PAHSEFZ", "PAHSEFZ", "logtbiS02", "logtbiS06", "logtbiS09")

noImp1 = kfoldData(colnames = names1, data = data, impute = FALSE, k = 5)
noImp2 = kfoldData(colnames = names2, data = data, impute = FALSE, k = 5)
noImp3 = kfoldData(colnames = names3, data = data, impute = FALSE, k = 5)
Imp1 = kfoldData(colnames = names1, data = data, impute = TRUE, k = 5)
Imp2 = kfoldData(colnames = names2, data = data, impute = TRUE, k = 5)
Imp3 = kfoldData(colnames = names3, data = data, impute = TRUE, k = 5)





CrossEntropyLoss = function(probabilities,truth){
  return(-mean((truth*log(probabilities)) + (1-truth)*log(1-probabilities)))
}



BOOST = function(kfold_list, eta =  .3, max_depth= 6, min_child_weight= 1, 
                 subsample = 1, colsample_bytree = 1, 
                 nrounds = 10000, verbose = 0, k = 5, early_stopping_rounds = 10){
  
  # get all permutations of parameters
  boostParams = expand.grid(eta = eta, max_depth = max_depth, 
                            min_child_weight = min_child_weight, 
                            subsample = subsample, colsample_bytree = colsample_bytree,
                            objective = "binary:logistic")
  
  # initialize data frame for results
  results <- data.frame(id = numeric(), eta = numeric(), max_depth = numeric(), 
                        min_child_weight = numeric(), subsample = numeric(), 
                        colsample_bytree = numeric(), bestIter = numeric(), CELoss = numeric(), 
                        accuracy = numeric(), TPR = numeric(), TNR = numeric())
  
  # loop through each row of parameters
  for(row in 1:nrow(boostParams)){
    params = as.list(boostParams[row,])
    
    # loop through each fold
    for (i in 1:k){
      
      # separate train, validate, and test sets
      Xt <- as.matrix(kfold_list[[paste0("Xtrain", i)]])
      Yt <- as.vector(kfold_list[[paste0("Ytrain", i)]])
      # The original train is split into train and validation with roughly 80% in train and 20% in validate
      cutoff = round(nrow(Xt)*0.8)
      c1 = cutoff+1
      Xtrain = Xt[1:cutoff,]
      Ytrain = Yt[1:cutoff]
      Xvalid = Xt[c1:nrow(Xt),]
      Yvalid = Yt[c1:nrow(Xt)]
      Xtest <- as.matrix(kfold_list[[paste0("Xtest", i)]])
      Ytest <- as.vector(kfold_list[[paste0("Ytest", i)]])
      
      #convert train and validate into xgb matrices
      trainXG <- xgb.DMatrix(data = Xtrain, label = Ytrain)
      validXG <- xgb.DMatrix(data = Xvalid, label = Yvalid)
      wl = list(train = trainXG, eval = validXG)
      
      # train with early stopping using the validation set.
      trainFit = xgb.train(params = params, data = trainXG, watchlist = wl,
                           nrounds = nrounds, verbose = verbose, 
                           early_stopping_rounds = early_stopping_rounds)
      
      # predict using the test set
      probabilities = predict(trainFit, Xtest, iteration_range = c(0, trainFit$best_iteration))
      classes = ifelse(probabilities > 0.5, 1, 0)
      
      # calculate performance metrics
      ceL = CrossEntropyLoss(probabilities, Ytest)
      acc = mean(classes == Ytest)
      TP <- sum(classes == 1 & Ytest == 1)
      TN <- sum(classes == 0 & Ytest == 0)
      FP <- sum(classes == 1 & Ytest == 0)
      FN <- sum(classes == 0 & Ytest == 1)
      TPR <- ifelse((TP + FN) > 0, TP / (TP + FN))
      TNR <- ifelse((TN + FP) > 0, TN / (TN + FP))
      
      # row bind performance metrics to the data frame
      results = rbind(results, list(id = row, eta = params$eta, max_depth = params$max_depth,
                    min_child_weight = params$min_child_weight, subsample = params$subsample,
                    colsample = params$colsample_bytree, bestIter = trainFit$best_iteration, 
                    CELoss = ceL, accuracy = acc, TPR = TPR, TNR = TNR
                    ))
    }
  }
  return(results)
}
noImp1 = kfoldData(colnames = names1, data = data, impute = FALSE, k = 5, seed = 4)
boostnoImp1 = BOOST(noImp1, eta = c(0.01, 0.1), max_depth = c(2,3,4,6))
boostnoImp2 = BOOST(noImp2, eta = c(0.01, 0.1), max_depth = c(2,3,4,6))
boostnoImp3 = BOOST(noImp3, eta = c(0.01, 0.1), max_depth = c(2,3,4,6))
boostImp1 = BOOST(Imp1, eta = c(0.01, 0.1), max_depth = c(2,3,4,6))
boostImp2 = BOOST(Imp2, eta = c(0.01, 0.1), max_depth = c(2,3,4,6))
boostImp3 = BOOST(Imp3, eta = c(0.01, 0.1), max_depth = c(2,3,4,6))

ggplot(boostnoImp1, aes(x = eta, y = accuracy, color = as.factor(id))) + geom_point() + facet_wrap(~as.factor(max_depth))


bartt = function(kfold_list, numfolds = 5, k = 2, power = 2, 
                base = 0.95, ntree = 200, seed = 556){
  bartParams = expand.grid(k = k, power = power, base = base, ntree = ntree)
  results <- data.frame(id = numeric(), k = numeric(), power = numeric(), 
                        base = numeric(), ntree = numeric())
  # loop through each row of parameters
  for(row in 1:nrow(bartParams)){
    params = as.list(bartParams[row,])
    
    # loop through each fold
    for (i in 1:numfolds){
      # separate train, validate, and test sets
      Xt <- as.matrix(kfold_list[[paste0("Xtrain", i)]])
      Yt <- as.vector(kfold_list[[paste0("Ytrain", i)]])
      # The original train is split into train and validation with roughly 80% in train and 20% in validate
      cutoff = round(nrow(Xt)*0.8)
      c1 = cutoff+1
      Xtrain = Xt[1:cutoff,]
      Ytrain = Yt[1:cutoff]
      Xvalid = Xt[c1:nrow(Xt),]
      Yvalid = Yt[c1:nrow(Xt)]
      Xtest <- as.matrix(kfold_list[[paste0("Xtest", i)]])
      Ytest <- as.vector(kfold_list[[paste0("Ytest", i)]])
      trainFit = bart(x.train = Xtrain, y.train = Ytrain, k = params$k,
                    power = params$power, base = params$base, ntree = params$ntree,
                    keeptrees = TRUE)
      # predict using the test set and validation set
      posteriorValidate = predict(trainFit, newdata = Xvalid)
      probsValidate = colMeans(posteriorValidate)
      classesValidate = ifelse(probsValidate > 0.5, 1, 0)
    
      posteriorTest = predict(trainFit, newdata = Xtest)
      probsTest = colMeans(posteriorTest)
      classesTest = ifelse(probsTest > 0.5, 1, 0)
      
      # calculate performance metrics
      ceLValidate = CrossEntropyLoss(probsValidate, Yvalid)
      ceLTest = CrossEntropyLoss(probsTest, Ytest)
      accValidate = mean(classesValidate == Yvalid)
      accTest = mean(classesTest == Ytest)
      TP <- sum(classesTest == 1 & Ytest == 1)
      TN <- sum(classesTest == 0 & Ytest == 0)
      FP <- sum(classesTest == 1 & Ytest == 0)
      FN <- sum(classesTest == 0 & Ytest == 1)
      TPR <- ifelse((TP + FN) > 0, TP / (TP + FN))
      TNR <- ifelse((TN + FP) > 0, TN / (TN + FP))
    
      # row bind performance metrics to the data frame
      results = rbind(results, list(id = row, k = params$k, power = params$power, 
                                  base = params$base, ntree = params$ntree, 
                                  CELossValidate = ceLValidate, CELossTest = ceLTest, 
                                  accValidate = accValidate, accTest = accTest, 
                                  TPR = TPR, TNR = TNR))
    }
  }
  return(results)
}

bartnoImp1 = bartt(kfold_list = noImp1, k = c(1,3,5), ntree = 500)
bartnoImp2 = bartt(kfold_list = noImp2, k = c(1,3,5), ntree = 500)
bartnoImp3 = bartt(kfold_list = noImp3, k = c(1,3,5), ntree = 500)
bartImp1 = bartt(kfold_list = Imp1, k = c(1,3,5), ntree = 500)
bartImp2 = bartt(kfold_list = Imp2, k = c(1,3,5), ntree = 500)
bartImp3 = bartt(kfold_list = Imp3, k = c(1,3,5), ntree = 500)

ggplot(bartnoImp1, aes(x = k, y = accValidate)) + geom_point()


