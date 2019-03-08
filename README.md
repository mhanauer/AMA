---
title: "AMA Results"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Libraring packages
```{r}
library(stringr)
library(prettyR)
```
Loading data
```{r}
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/AMAData")
AMAData = read.csv("AMAData.csv", header = TRUE, na.strings = c("NULL"))
```
Try to look at the variables
Variables to keep:
Client_ID, AgeAt_ASSESS_Date, DischargeType, ClientEncounter_NUM

A292_Q1	A292_Q59	A292_Q62	A292_Q66	A298_Q5	A298_Q6	A298_Q7	A292_Q1	A298_Q2	A298_Q5	A298_Q6	A298_Q7	A292_Q59	A292_Q62	A292_Q66	A294_Q20	A294_Q41	A294_Q43	A294_Q49	A294_Q50	A294_Q51	A294_Q53	A294_Q55	A294_Q57	A294_Q60	A294_Q62	A294_Q63	A294_Q64	A294_Q66	A294_Q67	A294_Q68	A294_Q69	A294_Q70	A294_Q72	A294_Q73	A294_Q74	A294_Q78	A294_Q90	A294_Q94	A294_Q95	A294_Q102	A294_Q104	A294_Q106	A294_Q108	A294_Q110	A294_Q125	A294_Q127	A294_Q129	A294_Q137	A294_Q141	A294_Q143	A294_Q145	A294_Q149	A294_Q158	A294_Q166	A294_Q174	A294_Q183	A294_Q184	A294_Q193	A294_Q200	A294_Q204	A294_Q207	A294_Q213	A294_Q216	A294_Q225	A294_Q230	A294_Q231	A294_Q233	A295_Q5	A296_Q5	A297_Q1	A297_Q3	A297_Q5

NULL is a value need to get rid of it at some point

Clean them one by one
Alcohol, Heroin, Cocaine/Crack, Marijuana/Hashish, Methamphetamines/Amphetamines, Benzodiazepines, Percocet


So I think what I need to do is create some type of if statement that says if this variable contains the word "X", then this new variable get this value and if not then...until you code everything that you want then you can factor the variable 
Try with alcohol


```{r, include=FALSE}
dim(AMAData)
head(AMAData)
##A292_Q1
head(AMAData$A292_Q1)
AMAData$A292_Q1 = factor(AMAData$A292_Q1)


Alcohol = str_detect(AMAData$A292_Q1, "Alcohol")
Heroin =  str_detect(AMAData$A292_Q1, "Heroin")
Cocaine_Crack = str_detect(AMAData$A292_Q1, "Cocaine/Crack")
Marijuana_Hashish = str_detect(AMAData$A292_Q1, "Marijuana/Hashish")
Methamphetamines_Amphetamines = str_detect(AMAData$A292_Q1, "Methamphetamines/Amphetamines")
Benzodiazepines = str_detect(AMAData$A292_Q1, "Benzodiazepines")
Percocet = str_detect(AMAData$A292_Q1, "Percocet")

### Add back variables
AMAData$Alcohol = Alcohol
AMAData$Heroin = Heroin
AMAData$Cocaine_Crack = Cocaine_Crack
AMAData$Marijuana_Hashish = Marijuana_Hashish
AMAData$Methamphetamines_Amphetamines = Methamphetamines_Amphetamines
AMAData$Benzodiazepines = Benzodiazepines
AMAData$Percocet = Percocet

##Look at the rest of the variables and see which ones are not dicot
##DischargeType
describe(AMAData)
dim(AMAData)

###Deal with any data errors before this point. 
###Get count and percentages for the descirptives 
describe.factor(AMAData$A298_Q5)
describe.factor(AMAData$A298_Q6)
describe.factor(AMAData$A298_Q7)
describe.factor(AMAData$A294_Q41)
describe.factor(AMAData$A294_Q57)
describe.factor(AMAData$A294_Q137)
describe.factor(AMAData$A297_Q5)
dim(AMAData)

#A298_Q5 = Sex_Orien: Hetero versus all else including not stated
describe.factor(AMAData$A298_Q5)
Sex_Orien = ifelse(AMAData$A298_Q5 == "Heterosexual", 1, 0)
describe.factor(Sex_Orien)
AMAData$Sex_Orien = Sex_Orien

#A298_Q6
#White or Caucasian versus all else including undisclosed
describe.factor(AMAData$A298_Q6)
Race = ifelse(AMAData$A298_Q6 == "White or Caucasian", 1, 0)
AMAData$Race = Race

#A298_Q7
#Gender = Female 1 male and undisclosed 0
describe.factor(AMAData$A298_Q7)
Gender = ifelse(AMAData$A298_Q7 == "Female", 1, 0)
AMAData$Gender = Gender

#A294_Q41
#Parents or Partner and everything else 0
describe.factor(AMAData$A294_Q41)
Parents = ifelse(AMAData$A294_Q41 == "Parents", 1, 0)
Partner = ifelse(AMAData$A294_Q41 == "Partner", 1, 0)
AMAData$Parents= Parents
AMAData$Partner = Partner

#A294_Q57, use autocontent analysis to figure this out
#describe.factor(AMAData$A294_Q57)
#length(AMAData$A294_Q57)

#A294_Q137
#None = 1 versus everything else 
describe.factor(AMAData$A294_Q137)
No_trouble = ifelse(AMAData$A294_Q137 == "None", 1, 0)
AMAData$No_trouble = No_trouble
describe.factor(AMAData$No_trouble)

#A297_Q5
#If Polysubstance = 1 all else 0
describe.factor(AMAData$A297_Q5)
Polysubstance = str_detect(AMAData$A297_Q5, "Polysubstance")
describe.factor(Polysubstance)
AMAData$Polysubstance = Polysubstance

#DischargeType
#AMA Against Medical Advice = 1; completed program is 0
describe.factor(AMAData$DischargeType)
AMA = ifelse(AMAData$DischargeType == "AMA Against Medical Advice", "Class1", "Class2")
AMAData$AMA = AMA
describe.factor(AMA)

#Now get rid of old variables and extra variables
AMAData$A292_Q1 = NULL 
AMAData$A298_Q5 = NULL
AMAData$A298_Q6 = NULL
AMAData$A294_Q137 = NULL
AMAData$A297_Q5 = NULL
AMAData$DischargeType = NULL
### Keep this null for now
AMAData$A294_Q57 = NULL
AMAData$A294_Q41 = NULL
AMAData$A298_Q7 = NULL
describe.factor(AMAData$ClientEncounter_NUM)
head(AMAData$A294_Q57)
### Don't need ClientID can create training and test on client encounter
#ClientBiopsychosocial_NUM, 

#Need to recode the Yes No variables into 1 and zero's
#These variables come after the sequence
#A294_Q233, A297_Q1, A297_Q3

head(AMAData)
head(AMAData[c(13:55, 58:59)])
AMAData_bin_con = AMAData[c(13:55, 58:59)]
sum(is.na(AMAData_bin_con))
head(AMAData_analysis)

AMAData_bin_con = apply(AMAData_bin_con, 2, function(x){ifelse(x == "Yes", 1, 0)})
describe(AMAData_bin_con)
sum(is.na(AMAData_bin_con))

#Ok now get the corrected binary data back with the original data
head(AMAData)

### Data I want to keep from the first data set to combine with the second
AMAData_keep = AMAData[c(2, 5, 60:74)]
head(AMAData)

AMAData_analysis = data.frame(AMAData_keep, AMAData_bin_con)
head(AMAData_analysis)

#### Changeing trues to ones and false to zeros for later data analysis
AMAData_analysis_true =AMAData_analysis[,3:16]
head(AMAData_analysis_true)
AMAData_analysis_true = apply(AMAData_analysis_true,2, function(x){as.numeric(x)})
head(AMAData_analysis_true)

AMAData_analysis[,3:16] = AMAData_analysis_true
head(AMAData_analysis)
```

Now do missing data analysis
```{r}
AMAData_analysis_complete = na.omit(AMAData_analysis)
dim(AMAData_analysis_complete)

1-dim(AMAData_analysis_complete)[1]/dim(AMAData_analysis)[1]

AMAData_analysis_complete$AMA = factor(AMAData_analysis_complete$AMA)
```
Drop variables with with near zero variance
```{r}
lowVarAMA =  nearZeroVar(AMAData_analysis_complete)
lowVarAMA

describe(AMAData_analysis_complete[c(26, 31, 34, 41, 42, 46, 51, 52, 57)])

AMAData_analysis_complete[c(26, 31, 34, 41, 42, 46, 51, 52, 57)] = NULL
head(AMAData_analysis_complete)

nearZeroVar(AMAData_analysis_complete)
```
Need a correlation matrix to justify the logistic pca

Pearson's r is fine for two binary variables: https://www.dummies.com/business/customers/associations-between-binary-variables/
```{r}
library(psych)
head(AMAData_analysis_complete)
head(AMAData_analysis_complete)
cor(AMAData_analysis_complete[,-c(17)])

```
Now we are going to reduce the variables so we capture at least 95% of the variation in logistics PCA to reduce the variables
```{r}
AMAData_analysis_complete_pca =  AMAData_analysis_complete[,-c(17)] 

library(logisticPCA)
library(rARPACK)

test_logPCA = logisticSVD(AMAData_analysis_complete_pca, k = 5)

dat =  data.frame(test_logPCA$A)

AMAData_analysis_complete = data.frame(AMA = AMAData_analysis_complete$AMA, dat)
head(AMAData_analysis_complete)
```



Get training
```{r}
inTrain = createDataPartition(y = AMAData_analysis_complete$AMA, p = .50, list = FALSE)
training = AMAData_analysis_complete[inTrain,]
testing = AMAData_analysis_complete[-inTrain,] 
```
Now develop the controls for the model
```{r}
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10)
```
Now run the model
```{r}
gbmFit1 <- train(AMA ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)
```
Now get the summary statistics
```{r}
gbmFit1
summary(gbmFit1)
```
Ok try and get rid of variables that are not important and try other statistics and figure out what to do with small classification problems.



Now run the model
```{r}
gbmFitInfl <- train(AMA ~ AgeAt_ASSESS_Date +	Heroin +	Alcohol +	Percocet +	A294_Q51 +	Sex_Orien +	A294_Q64 +	Cocaine_Crack +	Gender +	A294_Q104 +	A294_Q55 +	A297_Q3 +	A294_Q53 +	A294_Q183 +	A294_Q230 +	A294_Q145 +	A294_Q20 +	A294_Q66 +	A294_Q213 +	ClientEncounter_NUM, data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)
```
Now get the summary statistics
```{r}
gbmFitInfl
summary(gbmFitInfl)
```
Now run the model
```{r}
gbmFitInfl_2 <- train(AMA ~ AgeAt_ASSESS_Date +	Heroin +	Alcohol +	Percocet +	A294_Q51 +	Sex_Orien +	A294_Q64 +	Cocaine_Crack +	Gender +	A294_Q104 +	A294_Q55  +	A294_Q183 +	A294_Q230 +	A294_Q145 +	ClientEncounter_NUM, data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)
```
Now get the summary statistics
```{r}
gbmFitInfl_2
summary(gbmFitInfl_2)
```
Not getting us anywhere kappa still sucks.  Try Rare event models
```{r}
library(Zelig)

AMAData_analysis_complete$AMA = factor(AMAData_analysis_complete$AMA)

write.csv(AMAData_analysis_complete, "AMAData_analysis_complete.csv", row.names = FALSE)
AMAData_analysis_complete = read.csv("AMAData_analysis_complete.csv", header = TRUE)


rare_model = zelig(AMA ~ AgeAt_ASSESS_Date +	Heroin +	Alcohol +	Percocet +	A294_Q51 +	Sex_Orien +	A294_Q64 +	Cocaine_Crack +	Gender +	A294_Q104 +	A294_Q55 +	A297_Q3 +	A294_Q53 +	A294_Q183 +	A294_Q230 +	A294_Q145 +	A294_Q20 +	A294_Q66 +	A294_Q213 +	ClientEncounter_NUM, model = "relogit", bias.correct = TRUE, data = AMAData_analysis_complete)
summary(rare_model)




```
Ok try sampling methods
```{r}

upSampleTrain = upSample(x = training[,2:dim(training)[2]], y = training$AMA, yname = "AMA")
describe.factor(upSampleTrain$AMA)

downSampleTrain = downSample(x = training[,2:dim(training)[2]], y = training$AMA, yname = "AMA")
describe.factor(downSampleTrain$AMA)

```
Ok now try with this method
Now run the model
```{r}
gbmFitUp <- train(AMA ~ ., data = upSampleTrain, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)

### Try down sample
gbmFitDown <- train(AMA ~ ., data = downSampleTrain, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)

### Try SMOTE
library(DMwR)
smoteTrain <- SMOTE(AMA ~ ., data = training)
dim(smoteTrain)
table(smoteTrain$AMA)

gbmFitSMOTE <- train(AMA ~ ., data = smoteTrain, 
                 method = "gbm", 
                 trControl = fitControl,
                 verbose = FALSE)

```
Now get the summary statistics
```{r}
gbmFitUp
summary(gbmFitUp)

gbmFitDown
summary(gbmFitDown)

gbmFitSMOTE
summary(gbmFitSMOTE)
```
Try with ROC curves
```{r}

fitControl_ROC <- trainControl(
  method = "repeatedcv",
  number = 10,
  classProbs = TRUE,
  repeats = 10)

rfFitUp <- train(AMA ~ ., data = upSampleTrain, 
                 method = "rf", 
                 metric ="ROC",
                 trControl = fitControl_ROC,
                 verbose = FALSE)

### Try down sample
rfFitDown <- train(AMA ~ ., data = downSampleTrain, 
                 method = "rf", 
                 trControl = fitControl,
                 metric ="ROC",
                 verbose = FALSE)

### Try SMOTE
library(DMwR)
smoteTrain <- SMOTE(AMA ~ ., data = training)
dim(smoteTrain)
table(smoteTrain$AMA)

rfFitSMOTE <- train(AMA ~ ., data = smoteTrain, 
                 method = "rf", 
                 metric ="ROC",
                 trControl = fitControl,
                 verbose = FALSE)
```
Now results for ROC Metric
```{r}
gbmFitUpROC
summary(gbmFitUpROC)

gbmFitDownROC
summary(gbmFitDownROC)

gbmFitSMOTEROC
summary(gbmFitSMOTEROC)

```
ROC graphs
```{r}
rfUpPred = predict(rfFitUp, testing, type = "prob")[1]


rfUpPredRoc = roc(response = testing$AMA, predictor = rfUpPred$Class1, levels = rev(levels(testing$AMA)))
rfUpPredRoc
plot(rfUpPredRoc)



rfThresh <- coords(rfUpPredRoc, x = "best", best.method = "closest.topleft")
rfThresh


posPredValue(data = rfUpPred, reference = testing$AMA, positive = levels(testing$AMA)[1])

sensitivity(data = rfUpPred, reference = testing$AMA, positive = levels(testing$AMA)[1])
specificity(data = rfUpPred, reference = testing$AMA, positive = levels(testing$AMA)[1])


```



Ok now try and random forrest for small sample sizes

Sensitivity, when the test says the person has the disease how often to they catually have the disease
```{r}

nmin = sum(training$AMA == "Class1")

ctrl <- trainControl(method = "cv",classProbs = TRUE, summaryFunction = twoClassSummary)

rfDownsampled <- train(AMA ~ ., data = upSampleTrain,
                        method = "rf",
                        ntree = 1500,
                        tuneLength = 5,
                        metric = "ROC",
                        trControl = ctrl,
                        ## Tell randomForest to sample by strata. Here, 
                        ## that means within each class
                        strata = training$AMA,
                        ## Now specify that the number of samples selected
                        ## within each class should be the same
                        sampsize = rep(nmin, 2))


plot(rfDownsampled)
rfDownsampled

rfDownPred = predict(rfDownsampled, testing, type = "prob")[,1]

library(caret)
library(pROC)
rfDownPredRoc = roc(response = testing$AMA, predictor = rfDownPred, levels = rev(levels(testing$AMA)))
rfDownPredRoc
plot(rfDownPredRoc)

rfThresh <- coords(rfDownPredRoc, x = "best", best.method = "closest.topleft")
rfThresh

```

Messing around with predictive book
Try logPCa and get values with minimum number of factors just include the few continious ones

Get non-zero variance out first, then PCA for binary variables
Think about bootstrap method to reduce bias, but check page 72 for certain method
```{r}
library(caret)
library(AppliedPredictiveModeling)
library(e1071)
trans <- preProcess(training,method = c("BoxCox", "center", "scale", "pca"))
trans
dim(training)

dat = data.frame(a = rnorm(20), b = rnorm(20), c = rnorm(20), d = rnorm(20), e = rnorm(20))

trans = prcomp(dat)

trans$x


```











