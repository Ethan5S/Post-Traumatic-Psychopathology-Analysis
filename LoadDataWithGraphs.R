train = read.csv("tbistaa556_training.csv")
validate = read.csv("tbistaa556_validate.csv")

# reomving the died and subject id columns
#DON'T RUN MORE THAN ONCE WITHOUT RELOADING THE CSV FILES SO YOU DON'T KEEP REMOVING COLUMNS
train = train[,-1]
validate = validate[,-1]
train = train[,-36]
validate = validate[,-35]

# converting NA in the race column to unknown in both datasets
indR = is.na(train$race7)
train$race7[indR] = 0

# splitting features and labels
xtrain = train[,-36]
ytrain = train[,36]
xvalidate = validate[,-35]

# removing all rows with NA for a clean dataset
ind = complete.cases(train)
trainClean = train[ind,]

#how many NAs per column
for(i in 1:ncol(xtrain)){
  cat("\nColumn ", names(xtrain)[i], ". Num_NAs:", sum(is.na(xtrain[,i])))
}

# making new data frame with strings instead of numbers for visualizations
library(dplyr)
train1 = train
train1 = train1 %>% mutate(race7 = case_when(
  race7 == 0 ~ "Unknown",
  race7 == 1 ~ "Multi-race",
  race7 == 2 ~ "Pacific Islander",
  race7 == 3 ~ "American Indian",
  race7 == 4 ~ "Asian",
  race7 == 5 ~ "Black",
  race7 == 6 ~ "White"))

train1 = train1 %>% mutate(ethnic3 = case_when(
  ethnic3 == 0 ~ "Unknown",
  ethnic3 == 1 ~ "Hispanic",
  ethnic3 == 2 ~ "Non-Hispanic"))


train1 = train1 %>% mutate(sex2 = case_when(
  sex2 == 0 ~ "Unknown",
  sex2 == 1 ~ "Female",
  sex2 == 2 ~ "Male"))

train1 = train1 %>% mutate(sig_other = case_when(
  sig_other == 0 ~ "Unknown",
  sig_other == 1 ~ "No Significant Other",
  sig_other == 2 ~ "Significant Other"))

train1 = train1 %>% mutate(tobacco = case_when(
  tobacco == 0 ~ "Missing",
  tobacco == 1 ~ "Yes",
  tobacco == 2 ~ "No"))

train1 = train1 %>% mutate(alcohol = case_when(
  alcohol == 0 ~ "Missing",
  alcohol == 1 ~ "Yes",
  alcohol == 2 ~ "No"))

train1 = train1 %>% mutate(drugs = case_when(
  drugs == 0 ~ "Missing",
  drugs == 1 ~ "Yes",
  drugs == 2 ~ "No"))

train1 = train1 %>% mutate(payertype = case_when(
  payertype == 1 ~ "Work Comp and Self Pay",
  payertype == 2 ~ "Medicaid",
  payertype == 3 ~ "Medicare",
  payertype == 4 ~ "VA and Indian Services",
  payertype == 5 ~ "Commercial"))

train1 = train1 %>% mutate(ptp1_yn = case_when(
  ptp1_yn == 1 ~ "Yes",
  ptp1_yn == 2 ~ "No",))

train1 = train1 %>% mutate(ptp2_yn = case_when(
  ptp2_yn == 1 ~ "Yes",
  ptp2_yn == 2 ~ "No",))

train1 = train1 %>% mutate(ptp0_yn = case_when(
  ptp0_yn == 1 ~ "Yes",
  ptp0_yn == 2 ~ "No",))

train1 = train1 %>% mutate(ed_yn = case_when(
  ed_yn == 0 ~ "Missing",
  ed_yn == 1 ~ "Yes",
  ed_yn == 2 ~ "No"))

train1 = train1 %>% mutate(icu = case_when(
  icu == 1 ~ "Yes",
  icu == 2 ~ "No"))

train1 = train1 %>% mutate(delirium = case_when(
  delirium == 0 ~ "Missing",
  delirium == 1 ~ "Yes",
  delirium == 2 ~ "No"))

train1 = train1 %>% mutate(agitated = case_when(
  agitated == 0 ~ "Missing",
  agitated == 1 ~ "Yes",
  agitated == 2 ~ "No"))

train1 = train1 %>% mutate(lethargic = case_when(
  lethargic == 0 ~ "Missing",
  lethargic == 1 ~ "Yes",
  lethargic == 2 ~ "No"))

train1 = train1 %>% mutate(comatose = case_when(
  comatose == 0 ~ "Missing",
  comatose == 1 ~ "Yes",
  comatose == 2 ~ "No"))

train1 = train1 %>% mutate(disoriented = case_when(
  disoriented == 0 ~ "Missing",
  disoriented == 1 ~ "Yes",
  disoriented == 2 ~ "No"))

train1 = train1 %>% mutate(dc_setting = case_when(
  dc_setting == 0 ~ "Died",
  dc_setting == 1 ~ "Unknown",
  dc_setting == 2 ~ "Hospice",
  dc_setting == 3 ~ "Other",
  dc_setting == 4 ~ "AMA",
  dc_setting == 5 ~ "IRF",
  dc_setting == 6 ~ "SNF",
  dc_setting == 7 ~ "Mental/Psych",
  dc_setting == 8 ~ "Home",))

train1 = train1 %>% mutate(prehosp = case_when(
  prehosp == 1 ~ "Yes",
  prehosp == 2 ~ "No"))

train1 = train1 %>% mutate(posthosp = case_when(
  posthosp == 1 ~ "Yes",
  posthosp == 2 ~ "No"))

train1 = train1 %>% mutate(ptp3_yn = case_when(
  ptp3_yn == 0 ~ "No",
  ptp3_yn == 1 ~ "Yes"))



#Find proportion of PTP in each category of categorical variable
propPTP = function(train){
  for(col in 1:ncol(train)){
    uvs = sort(unique(train[,col]))
    # skip continuous variables
    if(length(uvs) >= 10){
      next
    }
    cat("\n\nColumn:", colnames(train)[col], "\n")
    for(value in uvs){
      subset = train[train[,col] == value,]
      cat("  value =", value, ". Proportion of PTP Post=", mean(subset$ptp3_yn), "\n")
    }
  }
}

propPTP(train)

# more continuous predictors
#train$logPAHSEFZ = log(train$PercentAboveHighSchoolEducationForZip+1)
#train$logPABEFZ = log(train$PercentAboveBachelorsEducationForZip+1)
train$summaryGCS = (train$gcs_min + train$gcs_max)/2
train$summaryADL = (train$adl_min + train$adl_max)/2
train$summaryMOB = (train$mobility_min + train$mobility_max)/2
train$loglos_total = log(train$los_total+1)
#train$loglogPAHSEFZ = log(train$logPAHSEFZ + 1)
#train$loglogPABEFZ = log(train$logPABEFZ + 1)
#train$logloglogPAHSEFZ = log(train$loglogPAHSEFZ + 1)
#train$logloglogPABEFZ = log(train$loglogPABEFZ + 1)
train$PABEFZ = ifelse(train$PercentAboveBachelorsEducationForZip>0.4,0.4,train$PercentAboveBachelorsEducationForZip)
train$PAHSEFZ = ifelse(train$PercentAboveHighSchoolEducationForZip>1,1,train$PercentAboveHighSchoolEducationForZip)
train$logtbiS02 = log(train$tbiS02+1)
train$logtbiS06 = log(train$tbiS06+1)
train$logtbiS09 = log(train$tbiS09+1)

#train1$logPAHSEFZ = log(train$PercentAboveHighSchoolEducationForZip+1)
#train1$logPABEFZ = log(train$PercentAboveBachelorsEducationForZip+1)
train1$summaryGCS = (train$gcs_min + train$gcs_max)/2
train1$summaryADL = (train$adl_min + train$adl_max)/2
train1$summaryMOB = (train$mobility_min + train$mobility_max)/2
train1$loglos_total = log(train$los_total+1)
train1$PABEFZ = ifelse(train$PercentAboveBachelorsEducationForZip>0.4,0.4,train$PercentAboveBachelorsEducationForZip)
train1$PAHSEFZ = ifelse(train$PercentAboveHighSchoolEducationForZip>1.2,1.2,train$PercentAboveHighSchoolEducationForZip)
train1$logtbiS02 = log(train$tbiS02+1)
train1$logtbiS06 = log(train$tbiS06+1)
train1$logtbiS09 = log(train$tbiS09+1)

# graphs of continuous predictors vs trt
died = train1[train1$dc_setting == "Died",]
library(ggplot2)
graphs = function(train, numUnique = 8){
  for(col in 1:ncol(train)){
  uvs = sort(unique(train[,col]))
  # skip continuous variables
  if(length(uvs) <= numUnique){
    next
  }
  colname = colnames(train)[col]
  print(ggplot(train, aes(x = .data[[colname]], y = as.factor(ptp3_yn), color = as.factor(ptp3_yn))) + 
    geom_jitter(size = 1, alpha = 0.5) + 
      labs(x = colname, y = "PTP Post") + theme(legend.position = "none"))
  }
}


graphs(train, numUnique = 4)
#graphs(died, numUnique = 4)

#load training data as a csv file
write.csv(train, file = "fullData.csv")
