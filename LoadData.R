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

# removing all rows with NA
ind = complete.cases(train)
trainClean = train[ind,]

#how many NAs per column
for(i in 1:ncol(xtrain)){
  cat("\nColumn ", names(xtrain)[i], ". Num_NAs:", sum(is.na(xtrain[,i])))
}