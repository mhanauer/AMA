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
```{r}
dim(AMAData)
head(AMAData)
##A292_Q1
head(AMAData$A292_Q1)
AMAData$A292_Q1 = factor(AMAData$A292_Q1)
```
So I think what I need to do is create some type of if statement that says if this variable contains the word "X", then this new variable get this value and if not then...until you code everything that you want then you can factor the variable 
Try with alcohol
```{r}
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


```
Look at the rest of the variables and see which ones are not dicot
DischargeType

```{r}
describe(AMAData)
dim(AMAData)
```
Deal with any data errors before this point. 


Get count and percentages for the descirptives 
```{r}
describe.factor(AMAData$A298_Q5)
describe.factor(AMAData$A298_Q6)
describe.factor(AMAData$A298_Q7)
describe.factor(AMAData$A294_Q41)
describe.factor(AMAData$A294_Q57)
describe.factor(AMAData$A294_Q137)
describe.factor(AMAData$A297_Q5)
```



A298_Q5 = Sex_Orien: Hetero versus all else including not stated
```{r}
describe.factor(AMAData$A298_Q5)
Sex_Orien = ifelse(AMAData$A298_Q5 == "Heterosexual", 1, 0)
describe.factor(Sex_Orien)
AMAData$Sex_Orien = Sex_Orien
```
A298_Q6
White or Caucasian versus all else including undisclosed
```{r}
describe.factor(AMAData$A298_Q6)
Race = ifelse(AMAData$A298_Q6 == "White or Caucasian", 1, 0)
AMAData$Race = Race
```
A298_Q7
Gender = Female 1 male and undisclosed 0
```{r}
describe.factor(AMAData$A298_Q7)
Gender = ifelse(AMAData$A298_Q7 == "Female", 1, 0)
AMAData$Gender = Gender

```
A294_Q41

Parents or Partner and everything else 0

```{r}
describe.factor(AMAData$A294_Q41)

Parents = ifelse(AMAData$A294_Q41 == "Parents", 1, 0)
Partner = ifelse(AMAData$A294_Q41 == "Partner", 1, 0)

AMAData$Parents= Parents
AMAData$Partner = Partner
```
A294_Q57, use autocontent analysis to figure this out
```{r}
describe.factor(AMAData$A294_Q57)
length(AMAData$A294_Q57)
```
A294_Q137

None = 1 versus everything else 
```{r}
describe.factor(AMAData$A294_Q137)
No_trouble = ifelse(AMAData$A294_Q137 == "None", 1, 0)
AMAData$No_trouble = No_trouble

```
A297_Q5

If Polysubstance = 1 all else 0
```{r}
describe.factor(AMAData$A297_Q5)
Polysubstance = str_detect(AMAData$A297_Q5, "Polysubstance")
describe.factor(Polysubstance)
AMAData$Polysubstance = Polysubstance
```
DischargeType
AMA Against Medical Advice = 1; completed program is 0
```{r}
describe.factor(AMAData$DischargeType)
AMA = ifelse(AMAData$DischargeType == "AMA Against Medical Advice", 1, 0)
AMAData$AMA = AMA
```


Now get rid of old variables and extra variables
```{r}
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
```
Need to recode the Yes No variables into 1 and zero's
These variables come after the sequence
A294_Q233, A297_Q1, A297_Q3

```{r}
head(AMAData)
head(AMAData[,13:55])
AMAData_bin_con = 
head(AMAData_analysis)
```














