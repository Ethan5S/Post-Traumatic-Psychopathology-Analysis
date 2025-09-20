library(dplyr)
library(mice)
library(glmnet)
library(randomForest)
library(ggplot2)

set.seed(556)
# Reading in Data
fulldata <- read.csv("fullData.csv")


##### Creating KfoldData function

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

names1 = c("ptp3_yn","age", "race7", "ethnic3", "sex2", "sig_other","tobacco", "alcohol", 
           "drugs", "MedianIncomeForZip", "payertype",                               
           "ptp1_yn", "ptp2_yn", "ed_yn", "icu", 
           "delirium", "agitated", "lethargic","comatose", "disoriented", 
           "dc_setting", "prehosp", "posthosp", "summaryGCS",                           
           "summaryADL", "summaryMOB", "loglos_total", "PABEFZ",                               
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

CrossEntropyLoss = function(probabilities,truth){
  return(mean((truth*log(probabilities)) + (1-truth)*log(1-probabilities)))
}

### Fitting lasso

# Converting data set into a matrix suitable for lasso GLM 
X = as.matrix(fulldata)[,-37]

y <- fulldata[,37]

# creating a vector of lambda values
lambda <- 10^seq(-4, -1, length.out = 100)

# creating lasso fit
lassofit = glmnet(X, y, alpha = 1, lambda = lambda)

# doing 10 fold cv on lasso model for best lambda value
cv_lasso <- cv.glmnet(X, y, alpha = 1, lambda = lambda)

# Ploitting cv test mse and lambda values 
ggplot() +
  geom_line(aes(cv_lasso$lambda, cv_lasso$cvm)) +
  geom_point(aes(cv_lasso$lambda, cv_lasso$cvm)) +
  scale_x_log10()

# Our optimal lambda value
cv_lasso$lambda.min

# the number of beta values which don't go to zero when we use optimal lambda value
cv_lasso$nzero[which.min(cv_lasso$cvm)]


# Gettinh coefficients at best lambda
best_lambda <- cv_lasso$lambda.min
best_model_coefs <- coef(cv_lasso, s = "lambda.min")

best_model_coefs


### testing lasso with kfold splits


kfold_list <- kfoldData(impute = TRUE, colnames = names1, data = fulldata, k = 5)


error_rates <- numeric(5)
cross_entropies_losses <- numeric(5)
true_positive_rates <- numeric(5)
true_negative_rates <- numeric(5)

## testing on names1 with imputation

for (i in 1:5) {
  Xtrain <- as.matrix(kfold_list[[paste0("Xtrain", i)]])
  Ytrain <- as.vector(kfold_list[[paste0("Ytrain", i)]])
  Xtest <- as.matrix(kfold_list[[paste0("Xtest", i)]])
  Ytest <- as.vector(kfold_list[[paste0("Ytest", i)]])

  lasso_fit <- glmnet(Xtrain, Ytrain, alpha = 1, lambda = 0.00075, family = binomial)
  
  pred_probs <- predict(lasso_fit, s = 0.00075, newx = Xtest, type = "response")
  
  predictions <- ifelse(pred_probs > 0.5, 1, 0)
  
  error_rates[i] <- mean(predictions != Ytest)
  
  cross_entropies_losses[i] <- CrossEntropyLoss(pred_probs, Ytest)
  
  TP <- sum(predictions == 1 & Ytest == 1)
  TN <- sum(predictions == 0 & Ytest == 0)
  FP <- sum(predictions == 1 & Ytest == 0)
  FN <- sum(predictions == 0 & Ytest == 1)
  
  true_positive_rates[i] <- ifelse((TP + FN) > 0, TP / (TP + FN))
  true_negative_rates[i] <- ifelse((TN + FP) > 0, TN / (TN + FP))
  
}

## testing on names2 with imputation 

error_rates <- numeric(5)
cross_entropies_losses <- numeric(5)
true_positive_rates <- numeric(5)
true_negative_rates <- numeric(5)

kfold_list <- kfoldData(impute = TRUE, colnames = names2, data = fulldata, k = 5)

for (i in 1:5) {
  Xtrain <- as.matrix(kfold_list[[paste0("Xtrain", i)]])
  Ytrain <- as.vector(kfold_list[[paste0("Ytrain", i)]])
  Xtest <- as.matrix(kfold_list[[paste0("Xtest", i)]])
  Ytest <- as.vector(kfold_list[[paste0("Ytest", i)]])
  
  lasso_fit <- glmnet(Xtrain, Ytrain, alpha = 1, lambda = 0.00075, family = binomial)
  
  pred_probs <- predict(lasso_fit, s = 0.00075, newx = Xtest, type = "response")
  
  predictions <- ifelse(pred_probs > 0.5, 1, 0)
  
  error_rates[i] <- mean(predictions != Ytest)
  
  cross_entropies_losses[i] <- CrossEntropyLoss(pred_probs, Ytest)
  
  TP <- sum(predictions == 1 & Ytest == 1)
  TN <- sum(predictions == 0 & Ytest == 0)
  FP <- sum(predictions == 1 & Ytest == 0)
  FN <- sum(predictions == 0 & Ytest == 1)
  
  true_positive_rates[i] <- ifelse((TP + FN) > 0, TP / (TP + FN))
  true_negative_rates[i] <- ifelse((TN + FP) > 0, TN / (TN + FP))
  
}

## Testing with names3 with imputation

error_rates <- numeric(5)
cross_entropies_losses <- numeric(5)
true_positive_rates <- numeric(5)
true_negative_rates <- numeric(5)

kfold_list <- kfoldData(impute = TRUE, colnames = names3, data = fulldata, k = 5)

for (i in 1:5) {
  Xtrain <- as.matrix(kfold_list[[paste0("Xtrain", i)]])
  Ytrain <- as.vector(kfold_list[[paste0("Ytrain", i)]])
  Xtest <- as.matrix(kfold_list[[paste0("Xtest", i)]])
  Ytest <- as.vector(kfold_list[[paste0("Ytest", i)]])
  
  lasso_fit <- glmnet(Xtrain, Ytrain, alpha = 1, lambda = 0.00075, family = binomial)
  
  pred_probs <- predict(lasso_fit, s = 0.00075, newx = Xtest, type = "response")
  
  predictions <- ifelse(pred_probs > 0.5, 1, 0)
  
  error_rates[i] <- mean(predictions != Ytest)
  
  cross_entropies_losses[i] <- CrossEntropyLoss(pred_probs, Ytest)
  
  TP <- sum(predictions == 1 & Ytest == 1)
  TN <- sum(predictions == 0 & Ytest == 0)
  FP <- sum(predictions == 1 & Ytest == 0)
  FN <- sum(predictions == 0 & Ytest == 1)
  
  true_positive_rates[i] <- ifelse((TP + FN) > 0, TP / (TP + FN))
  true_negative_rates[i] <- ifelse((TN + FP) > 0, TN / (TN + FP))
  
}


## testing on names1 with no imputation

error_rates <- numeric(5)
cross_entropies_losses <- numeric(5)
true_positive_rates <- numeric(5)
true_negative_rates <- numeric(5)

kfold_list <- kfoldData(impute = FALSE, colnames = names1, data = fulldata, k = 5)

for (i in 1:5) {
  Xtrain <- as.matrix(kfold_list[[paste0("Xtrain", i)]])
  Ytrain <- as.vector(kfold_list[[paste0("Ytrain", i)]])
  Xtest <- as.matrix(kfold_list[[paste0("Xtest", i)]])
  Ytest <- as.vector(kfold_list[[paste0("Ytest", i)]])
  
  lasso_fit <- glmnet(Xtrain, Ytrain, alpha = 1, lambda = 0.00075, family = binomial)
  
  pred_probs <- predict(lasso_fit, s = 0.00075, newx = Xtest, type = "response")
  
  predictions <- ifelse(pred_probs > 0.5, 1, 0)
  
  error_rates[i] <- mean(predictions != Ytest)
  
  cross_entropies_losses[i] <- CrossEntropyLoss(pred_probs, Ytest)
  
  TP <- sum(predictions == 1 & Ytest == 1)
  TN <- sum(predictions == 0 & Ytest == 0)
  FP <- sum(predictions == 1 & Ytest == 0)
  FN <- sum(predictions == 0 & Ytest == 1)
  
  true_positive_rates[i] <- ifelse((TP + FN) > 0, TP / (TP + FN))
  true_negative_rates[i] <- ifelse((TN + FP) > 0, TN / (TN + FP))
  
}

## testing on names2 with no imputation 

error_rates <- numeric(5)
cross_entropies_losses <- numeric(5)
true_positive_rates <- numeric(5)
true_negative_rates <- numeric(5)

kfold_list <- kfoldData(impute = FALSE, colnames = names2, data = fulldata, k = 5)

for (i in 1:5) {
  Xtrain <- as.matrix(kfold_list[[paste0("Xtrain", i)]])
  Ytrain <- as.vector(kfold_list[[paste0("Ytrain", i)]])
  Xtest <- as.matrix(kfold_list[[paste0("Xtest", i)]])
  Ytest <- as.vector(kfold_list[[paste0("Ytest", i)]])
  
  lasso_fit <- glmnet(Xtrain, Ytrain, alpha = 1, lambda = 0.00075, family = binomial)
  
  pred_probs <- predict(lasso_fit, s = 0.00075, newx = Xtest, type = "response")
  
  predictions <- ifelse(pred_probs > 0.5, 1, 0)
  
  error_rates[i] <- mean(predictions != Ytest)
  
  cross_entropies_losses[i] <- CrossEntropyLoss(pred_probs, Ytest)
  
  TP <- sum(predictions == 1 & Ytest == 1)
  TN <- sum(predictions == 0 & Ytest == 0)
  FP <- sum(predictions == 1 & Ytest == 0)
  FN <- sum(predictions == 0 & Ytest == 1)
  
  true_positive_rates[i] <- ifelse((TP + FN) > 0, TP / (TP + FN))
  true_negative_rates[i] <- ifelse((TN + FP) > 0, TN / (TN + FP))
  
}

## Testing with names3 with no imputation

error_rates <- numeric(5)
cross_entropies_losses <- numeric(5)
true_positive_rates <- numeric(5)
true_negative_rates <- numeric(5)

kfold_list <- kfoldData(impute = FALSE, colnames = names3, data = fulldata, k = 5)

for (i in 1:5) {
  Xtrain <- as.matrix(kfold_list[[paste0("Xtrain", i)]])
  Ytrain <- as.vector(kfold_list[[paste0("Ytrain", i)]])
  Xtest <- as.matrix(kfold_list[[paste0("Xtest", i)]])
  Ytest <- as.vector(kfold_list[[paste0("Ytest", i)]])
  
  lasso_fit <- glmnet(Xtrain, Ytrain, alpha = 1, lambda = 0.00075, family = binomial)
  
  pred_probs <- predict(lasso_fit, s = 0.00075, newx = Xtest, type = "response")
  
  predictions <- ifelse(pred_probs > 0.5, 1, 0)
  
  error_rates[i] <- mean(predictions != Ytest)
  
  cross_entropies_losses[i] <- CrossEntropyLoss(pred_probs, Ytest)
  
  TP <- sum(predictions == 1 & Ytest == 1)
  TN <- sum(predictions == 0 & Ytest == 0)
  FP <- sum(predictions == 1 & Ytest == 0)
  FN <- sum(predictions == 0 & Ytest == 1)
  
  true_positive_rates[i] <- ifelse((TP + FN) > 0, TP / (TP + FN))
  true_negative_rates[i] <- ifelse((TN + FP) > 0, TN / (TN + FP))
  
}



### Fitting random forest
# changing response into factor
trainClean$ptp3_yn = as.factor(trainClean$ptp3_yn)

rf_fit <- randomForest(ptp3_yn ~ ., data = trainClean, mtry = ncol(trainClean) - 1, importance = TRUE) ## change predictors for full dataset


# plotting which predictors are the most important
data.frame(rf_fit$importance )%>%
  mutate(variable = rownames(rf_fit$importance)) %>%
  mutate(variable = factor(variable, levels = variable[order(MeanDecreaseGini)])) %>% 
  ggplot() +
  geom_point(aes(MeanDecreaseGini, variable))




### testing Random Forest with kfold splits

## testing names1 with imputed values

kfold_list <- kfoldData(impute = TRUE, colnames = names1, data = fulldata, k = 5)


rf_error_rates <- numeric(5)
rf_cross_entropies_losses <- numeric(5)
rf_true_positive_rates <- numeric(5)
rf_true_negative_rates <- numeric(5)

## testing on names1 with imputation

for (i in 1:5) {
  Xtrain <- as.matrix(kfold_list[[paste0("Xtrain", i)]])
  Ytrain <- as.vector(kfold_list[[paste0("Ytrain", i)]])
  Xtest <- as.matrix(kfold_list[[paste0("Xtest", i)]])
  Ytest <- as.vector(kfold_list[[paste0("Ytest", i)]])
  
  rf_fit <- randomForest(Ytrain ~ ., data = Xtrain, mtry = ncol(trainClean) - 1, importance = TRUE)
  
  pred_probs <- predict(rf_fit, newx = Xtest, type = "response")
  
  predictions <- ifelse(pred_probs > 0.5, 1, 0)
  
  rf_error_rates[i] <- mean(predictions != Ytest)
  
  rf_cross_entropies_losses[i] <- CrossEntropyLoss(pred_probs, Ytest)
  
  TP <- sum(predictions == 1 & Ytest == 1)
  TN <- sum(predictions == 0 & Ytest == 0)
  FP <- sum(predictions == 1 & Ytest == 0)
  FN <- sum(predictions == 0 & Ytest == 1)
  
  rf_true_positive_rates[i] <- ifelse((TP + FN) > 0, TP / (TP + FN))
  rf_true_negative_rates[i] <- ifelse((TN + FP) > 0, TN / (TN + FP))
  
}


rf_error_rates2 <- numeric(5)
rf_cross_entropies_losses2 <- numeric(5)
rf_true_positive_rates2 <- numeric(5)
rf_true_negative_rates2 <- numeric(5)

kfold_list <- kfoldData(impute = TRUE, colnames = names1, data = fulldata, k = 5)

for (i in 1:5) {
  Xtrain <- kfold_list[[paste0("Xtrain", i)]]
  Ytrain <- kfold_list[[paste0("Ytrain", i)]]
  Xtest <- kfold_list[[paste0("Xtest", i)]]
  Ytest <- kfold_list[[paste0("Ytest", i)]]
  
  train_df <- data.frame(Xtrain, ptp3_yn = as.factor(Ytrain))
  test_df <- data.frame(Xtest)
  
  rf_fit <- randomForest(ptp3_yn ~ ., data = train_df, mtry = ncol(Xtrain), importance = TRUE)
  
  pred_probs <- predict(rf_fit, newdata = test_df, type = "prob")[, "1"]
  
  predictions <- ifelse(pred_probs > 0.5, 1, 0)
  
  rf_error_rates2[i] <- mean(predictions != Ytest)
  
  rf_cross_entropies_losses2[i] <- CrossEntropyLoss(pred_probs, Ytest)
  
  TP <- sum(predictions == 1 & Ytest == 1)
  TN <- sum(predictions == 0 & Ytest == 0)
  FP <- sum(predictions == 1 & Ytest == 0)
  FN <- sum(predictions == 0 & Ytest == 1)
  
  rf_true_positive_rates2[i] <- ifelse((TP + FN) > 0, TP / (TP + FN))
  rf_true_negative_rates2[i] <- ifelse((TN + FP) > 0, TN / (TN + FP))
}

rf_error_rates2
rf_cross_entropies_losses2
rf_true_positive_rates2
rf_true_negative_rates2



