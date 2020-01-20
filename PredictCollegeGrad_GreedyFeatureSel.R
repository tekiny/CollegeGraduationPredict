library(RSQLite)
library(ggplot2)
library(caret)
library (stats)
library(leaps)
library(elasticnet)
library(LiblineaR)
library(pls)
library(neuralnet)

#PROBLEM DESCRIPTION
#Predict the graduation rate based on multiple variables. The target variable is C150_4
#C150_4: Completion rate for first-time, full-time students at four-year institutions (150% of expected time to completion)	
#This is a regression problem.

#We use student data from 2011 to train the model and student data from 2013 to test the model.  

#We will analyze how the following variables can affect the graduation rate:

#1. ADM_RATE_ALL: rate of admission rate for all campuses   
#2. COSTT4_A: Average cost of attendance per year  
#3. PPTUG_EF: fraction of part-time students  
#4. INC_PCT_LO: Percentage of students from low-income families, defined as annual family income < $30,000   
#5. UG25ABV: Percentage of students above 25 years of age   
#6. PAR_ED_PCT_1STGEN: Percentage of first-generation college students in the family  
#7. PCTFLOAN: Percent of students receiving a federal loan
#8 SAT_AVG_ALL:Average SAT equivalent score of students admitted for all campuses


# set this to the directory where you downloaded the dataset
# setwd("~/Adriana/personal/HV/Clases/IntroDataScience/FinalProject/Dataset/college-scorecard")

# You can read in the SQLite datbase like this
#setwd("C:/Users/MetaData/Documents/College")
db <- dbConnect(dbDriver("SQLite"), "database.sqlite")

MyData <- dbGetQuery(db, "SELECT year,INSTNM College,ADM_RATE_ALL AdmissionRate,SAT_AVG_ALL,COSTT4_A Average_Cost,UGDS_BLACK,UGDS_WHITE,PPTUG_EF,INC_PCT_LO,UG25abv,PAR_ED_PCT_1STGEN,PCTFLOAN,C150_4 GradRate FROM Scorecard WHERE Year=2011 AND ADM_RATE_ALL != 'PrivacySuppressed' AND ADM_RATE_ALL IS NOT NULL AND COSTT4_A != 'PrivacySuppressed' AND COSTT4_A IS NOT NULL AND UGDS_WHITE IS NOT NULL AND UGDS_BLACK != 'PrivacySuppressed' AND UGDS_BLACK IS NOT NULL AND PPTUG_EF != 'PrivacySuppressed' AND PPTUG_EF IS NOT NULL AND INC_PCT_LO != 'PrivacySuppressed' AND INC_PCT_LO IS NOT NULL AND UG25abv != 'PrivacySuppressed' AND UG25abv IS NOT NULL AND PAR_ED_PCT_1STGEN != 'PrivacySuppressed' AND PAR_ED_PCT_1STGEN IS NOT NULL AND PCTFLOAN != 'PrivacySuppressed' AND PCTFLOAN IS NOT NULL AND SAT_AVG_ALL IS NOT NULL AND C150_4 != 'PrivacySuppressed' AND C150_4 IS NOT NULL AND PREDDEG = 'Predominantly bachelor''s-degree granting' ORDER BY INSTNM ASC")


# any correlation between graduation rate and admission rate?
ggplot(MyData,aes(x=AdmissionRate,y=GradRate)) +
  geom_point()+geom_smooth(se=FALSE,method="lm")

cor(MyData$GradRate,MyData$AdmissionRate) 
# Low negative correlation= -0.22

# any correlation between graduation rate and cost?
ggplot(MyData,aes(x=Average_Cost,y=GradRate))+
  geom_point()+geom_smooth(se=FALSE,method="lm")

cor(MyData$GradRate,MyData$Average_Cost) 
# High postive correlation= 0.60

# any correlation between graduation rate and part time students?
ggplot(MyData,aes(x=PPTUG_EF,y=GradRate))+
  geom_point()+geom_smooth(se=FALSE,method="lm")

cor(MyData$GradRate,MyData$PPTUG_EF) 
# Negative correlation= -0.40


# any correlation between graduation rate and low income families?
ggplot(MyData,aes(x=INC_PCT_LO,y=GradRate))+
  geom_point()+geom_smooth(se=FALSE,method="lm")

cor(MyData$GradRate,MyData$INC_PCT_LO) 
# High negative correlation= - 0.68

# any correlation between graduation rate and students above 25 years of age?
ggplot(MyData,aes(x=UG25abv,y=GradRate))+
  geom_point()+geom_smooth(se=FALSE,method="lm")

cor(MyData$GradRate,MyData$UG25abv) 
# correlation= 0.68

# any correlation between graduation rate and first-generation college students?
ggplot(MyData,aes(x=PAR_ED_PCT_1STGEN,y=GradRate))+
  geom_point()+geom_smooth(se=FALSE,method="lm")

cor(MyData$GradRate,MyData$PAR_ED_PCT_1STGEN) 
# High negative correlation= - 0.70

# any correlation between graduation rate and students receiving a federal loan?
ggplot(MyData,aes(x=PCTFLOAN,y=GradRate))+
  geom_point()+geom_smooth(se=FALSE,method="lm")

cor(MyData$GradRate,MyData$PCTFLOAN) 
# Low negative correlation= -0.20


# any correlation between graduation rate and SAT?
ggplot(MyData,aes(x=SAT_AVG_ALL,y=GradRate))+
  geom_point()+geom_smooth(se=FALSE,method="lm")

cor(MyData$GradRate,MyData$SAT_AVG_ALL) 
# High postive correlation= 0.77

# Use a plot to show the distribution of the SAT
ggplot(MyData,aes(SAT_AVG_ALL))+geom_histogram(binwidth=150,color="red",fill="blue")+ylim(0,500)


#  Variable  | Correlation with Graduation Rate(C150_4)
#- --------  | -----------------------------------
#ADM_RATE_ALL| -0.22
#COSTT4_A    |  0.60
#PPTUG_EF    | -0.40
#INC_PCT_LO  | -0.68
#UG25abv     | -0.48
#PAR_ED_PCT_1STGEN | -0.70
#PCTFLOAN    | -0.20  
#SAT_AVG_ALL |  0.77



#Whole feature subset is below. 31 features exist.
#Features are select via an SQL. 
#Data is cleansed via condtions in the SQL where clause
#Features with two many NULL values eleminated.() C150_4_NHPI,C150_4_AIAN,TUITFTE)

# C150_4 GradRate,
# year,
# INSTNM College,
# COSTT4_A Average_Cost,
# INC_PCT_LO LowIncome,
# PAR_ED_PCT_1STGEN FirstGen,
# SAT_AVG_ALL AvgSat, 
# UGDS, 
# UGDS_WHITE,
# UGDS_BLACK,
# UGDS_HISP,
# UGDS_ASIAN,
# UGDS_AIAN,
# UGDS_NHPI, 
# SAT_AVG,
# TUITIONFEE_IN,
# TUITIONFEE_OUT, 
# C150_4_WHITE, 
# C150_4_BLACK, 
# C150_4_HISP, 
# C150_4_ASIAN,  
# CONTROL,
# ADM_RATE_ALL, 
# COSTT4_A, 
# PPTUG_EF, 
# UG25abv, 
# PCTFLOAN, 
# AVGFACSAL, 
# INC_PCT_LO, 
# PCTPELL, 
# DEBT_MDN

trainData <- dbGetQuery(db, "SELECT C150_4 GradRate,year,INSTNM College,COSTT4_A Average_Cost,INC_PCT_LO LowIncome,PAR_ED_PCT_1STGEN FirstGen,SAT_AVG_ALL AvgSat, UGDS, UGDS_WHITE,UGDS_BLACK,UGDS_HISP,UGDS_ASIAN,UGDS_AIAN,UGDS_NHPI, SAT_AVG,TUITIONFEE_IN,TUITIONFEE_OUT, C150_4_WHITE, C150_4_BLACK, C150_4_HISP, C150_4_ASIAN,  CONTROL,ADM_RATE_ALL, COSTT4_A, PPTUG_EF, UG25abv, PCTFLOAN, AVGFACSAL, INC_PCT_LO, PCTPELL, DEBT_MDN FROM Scorecard WHERE Year = 2011 AND COSTT4_A != 'PrivacySuppressed' AND COSTT4_A IS NOT NULL AND INC_PCT_LO != 'PrivacySuppressed' AND INC_PCT_LO IS NOT NULL AND PAR_ED_PCT_1STGEN != 'PrivacySuppressed' AND PAR_ED_PCT_1STGEN IS NOT NULL AND SAT_AVG_ALL IS NOT NULL AND C150_4 != 'PrivacySuppressed' AND UGDS IS NOT NULL AND UGDS_WHITE IS NOT NULL AND UGDS_BLACK IS NOT NULL AND UGDS_HISP IS NOT NULL AND UGDS_ASIAN IS NOT NULL AND UGDS_AIAN IS NOT NULL AND UGDS_NHPI IS NOT NULL AND  SAT_AVG IS NOT NULL AND TUITIONFEE_IN IS NOT NULL AND TUITIONFEE_OUT IS NOT NULL AND C150_4 IS NOT NULL AND C150_4_WHITE IS NOT NULL AND C150_4_BLACK IS NOT NULL AND C150_4_HISP IS NOT NULL AND C150_4_ASIAN IS NOT NULL AND CONTROL IS NOT NULL AND ADM_RATE_ALL IS NOT NULL AND COSTT4_A IS NOT NULL AND PPTUG_EF IS NOT NULL AND UG25abv IS NOT NULL AND PCTFLOAN IS NOT NULL AND AVGFACSAL IS NOT NULL AND INC_PCT_LO IS NOT NULL AND PCTPELL IS NOT NULL AND DEBT_MDN AND PREDDEG = 'Predominantly bachelor''s-degree granting' ORDER BY INSTNM ASC")

#trainData=trainData[complete.cases(trainData), ]

testData <- dbGetQuery(db, "SELECT C150_4 GradRate,year,INSTNM College,COSTT4_A Average_Cost,INC_PCT_LO LowIncome,PAR_ED_PCT_1STGEN FirstGen,SAT_AVG_ALL AvgSat, UGDS, UGDS_WHITE,UGDS_BLACK,UGDS_HISP,UGDS_ASIAN,UGDS_AIAN,UGDS_NHPI, SAT_AVG,TUITIONFEE_IN,TUITIONFEE_OUT, C150_4_WHITE, C150_4_BLACK, C150_4_HISP, C150_4_ASIAN,  CONTROL,ADM_RATE_ALL, COSTT4_A, PPTUG_EF, UG25abv, PCTFLOAN, AVGFACSAL, INC_PCT_LO, PCTPELL, DEBT_MDN FROM Scorecard WHERE Year = 2013 AND INSTNM IN (SELECT INSTNM FROM Scorecard WHERE Year = 2011 AND COSTT4_A != 'PrivacySuppressed' AND COSTT4_A IS NOT NULL AND INC_PCT_LO != 'PrivacySuppressed' AND INC_PCT_LO IS NOT NULL AND PAR_ED_PCT_1STGEN != 'PrivacySuppressed' AND PAR_ED_PCT_1STGEN IS NOT NULL AND SAT_AVG_ALL IS NOT NULL AND C150_4 != 'PrivacySuppressed' AND UGDS IS NOT NULL AND UGDS_WHITE IS NOT NULL AND UGDS_BLACK IS NOT NULL AND UGDS_HISP IS NOT NULL AND UGDS_ASIAN IS NOT NULL AND UGDS_AIAN IS NOT NULL AND UGDS_NHPI IS NOT NULL AND  SAT_AVG IS NOT NULL AND TUITIONFEE_IN IS NOT NULL AND TUITIONFEE_OUT IS NOT NULL AND C150_4 IS NOT NULL AND C150_4_WHITE IS NOT NULL AND C150_4_BLACK IS NOT NULL AND C150_4_HISP IS NOT NULL AND C150_4_ASIAN IS NOT NULL AND CONTROL IS NOT NULL AND ADM_RATE_ALL IS NOT NULL AND COSTT4_A IS NOT NULL AND PPTUG_EF IS NOT NULL AND UG25abv IS NOT NULL AND PCTFLOAN IS NOT NULL AND AVGFACSAL IS NOT NULL AND INC_PCT_LO IS NOT NULL AND PCTPELL IS NOT NULL AND DEBT_MDN AND PREDDEG = 'Predominantly bachelor''s-degree granting') AND COSTT4_A != 'PrivacySuppressed' AND COSTT4_A IS NOT NULL AND INC_PCT_LO != 'PrivacySuppressed' AND INC_PCT_LO IS NOT NULL AND PAR_ED_PCT_1STGEN != 'PrivacySuppressed' AND PAR_ED_PCT_1STGEN IS NOT NULL AND SAT_AVG_ALL IS NOT NULL AND C150_4 != 'PrivacySuppressed' AND UGDS IS NOT NULL AND UGDS_WHITE IS NOT NULL AND UGDS_BLACK IS NOT NULL AND UGDS_HISP IS NOT NULL AND UGDS_ASIAN IS NOT NULL AND UGDS_AIAN IS NOT NULL AND UGDS_NHPI IS NOT NULL AND  SAT_AVG IS NOT NULL AND TUITIONFEE_IN IS NOT NULL AND TUITIONFEE_OUT IS NOT NULL AND C150_4 IS NOT NULL AND C150_4_WHITE IS NOT NULL AND C150_4_BLACK IS NOT NULL AND C150_4_HISP IS NOT NULL AND C150_4_ASIAN IS NOT NULL AND CONTROL IS NOT NULL AND ADM_RATE_ALL IS NOT NULL AND COSTT4_A IS NOT NULL AND PPTUG_EF IS NOT NULL AND UG25abv IS NOT NULL AND PCTFLOAN IS NOT NULL AND AVGFACSAL IS NOT NULL AND INC_PCT_LO IS NOT NULL AND PCTPELL IS NOT NULL AND DEBT_MDN AND PREDDEG = 'Predominantly bachelor''s-degree granting' ORDER BY INSTNM ASC")

#testData=testData[complete.cases(testData), ]

#Greedy Algorithm starts 
#We start with the feature with the 
#best correlation ratio we found from the plots SAT_AVG_ALL AS AvgSat


selectedFeatures=c("AvgSat","GradRate")
possibleFeatures=names(trainData[3:31])
training=trainData[,selectedFeatures]
testing=testData[,selectedFeatures]

modelloop=train(GradRate ~.,training,method="lm")
predictloop=predict(modelloop,testing)
baseRMSE=postResample(predictloop,testing$GradRate)[1]


for (i in 1:length(possibleFeatures)){
  print(i)
  print("beforeif:")
  print(baseRMSE)
  
  training=trainData[,c(selectedFeatures,possibleFeatures[i])]
  testing=testData[,c(selectedFeatures,possibleFeatures[i])]
  modelloop=train(GradRate ~.,training,method="lm")
  predictloop=predict(modelloop,testing)
  inloopRMSE=postResample(predictloop,testing$GradRate)[1]
  
  #  print(selectedFeatures)
  if(inloopRMSE<baseRMSE){
    #    selectedFeatures=append(selectedFeatures,possibleFeatures[i])
    selectedFeatures=c(selectedFeatures,possibleFeatures[i])
    baseRMSE=inloopRMSE
    print(baseRMSE)
  }
}
# print the selectedFeatures 
selectedFeatures
baseRMSE

#After greedy algorithm Selected featires turned out to be:
# "AvgSat"       "GradRate"     "College"      "UGDS_WHITE"   "UGDS_BLACK"  
# "C150_4_WHITE"
# RMSE = 0.04674695
# Final Check
training=trainData[,selectedFeatures]
testing=testData[,selectedFeatures]

modelfinalLM=train(GradRate ~.,training,method="lm")
predictfinalLM=predict(modelfinalLM,testing)
LMfinalRMSE=postResample(predictfinalLM,testing$GradRate)[1]
LMfinalRMSE


#Featureselect with regsubset did not finished approx in 1 hour time
#SO I canceled
#m1 <- regsubsets( GradRate ~    College + Average_Cost+ LowIncome + FirstGen+ AvgSat+ UGDS+ UGDS_WHITE+UGDS_BLACK+UGDS_HISP+UGDS_ASIAN+UGDS_AIAN+UGDS_NHPI+ SAT_AVG+TUITIONFEE_IN+TUITIONFEE_OUT+ C150_4_WHITE+ C150_4_BLACK+ C150_4_HISP+ C150_4_ASIAN+  CONTROL+ADM_RATE_ALL+ COSTT4_A+ PPTUG_EF+ UG25abv+ PCTFLOAN+ AVGFACSAL+ INC_PCT_LO+ PCTPELL+ DEBT_MDN,
#                 data = trainData, nvmax =10)
#summary(m1)
#m1 <- regsubsets( GradRate ~    College + Average_Cost+ LowIncome + FirstGen+ AvgSat+ UGDS+ UGDS_WHITE+UGDS_BLACK+SAT_AVG++TUITIONFEE_OUT+  CONTROL+ADM_RATE_ALL+ COSTT4_A+ PPTUG_EF+ AVGFACSAL+ INC_PCT_LO+ PCTPELL+ DEBT_MDN,
#                  data = trainData, nvmax =10, really.big=T)

#Random Forest with the features we found with greedy selection
selectedFeatures
training=trainData[,selectedFeatures]
testing=testData[,selectedFeatures]

modelfinalRF=train(GradRate ~.,training,method="rf")
predictfinalRF=predict(modelfinalRF,testing)
RFfinalRMSE=postResample(predictfinalRF,testing$GradRate)[1]
RFfinalRMSE

#After greedy algorithm Selected features turned out to be:
# "AvgSat"       "GradRate"     "College"      "UGDS_WHITE"   "UGDS_BLACK"  
# "C150_4_WHITE"
# RMSE with RF = 0.0367262

#Elastic net regression with the features we found with greedy selection
#Elastic net did not work
# Something is wrong; all the RMSE metric values are missing:
#   RMSE        Rsquared        MAE     
# Min.   : NA   Min.   : NA   Min.   : NA  
# 1st Qu.: NA   1st Qu.: NA   1st Qu.: NA  

#Bridge regression with the features we found with greedy selection
#Birdge net did not worked
# INTERRUPT: blasso model leaked, is now destroyed

modelfinalBR=train(GradRate ~.,training,method="bridge")
predictfinalBR=predict(modelfinalBR,testing)
BRfinalRMSE=postResample(predictfinalBR,testing$GradRate)[1]
BRfinalRMSE


#L2 Regularized Support Vector Machine (dual) with Linear Kernel method = svmLinear3 with the features we found with greedy selection
#The result is     
# RMSE 0.1244765 
modelfinalSVM3=train(GradRate ~.,training,method="svmLinear3")
predictfinalSVM3=predict(modelfinalSVM3,testing)
SVM3finalRMSE=postResample(predictfinalSVM3,testing$GradRate)[1]
SVM3finalRMSE

#Support Vector Machines with Linear Kernel regression method = svmLinear2 with the features we found with greedy selection
#The result is     
# RMSE 0.03431968 
modelfinalSVM2=train(GradRate ~.,training,method="svmLinear2")
predictfinalSVM2=predict(modelfinalSVM2,testing)
SVM2finalRMSE=postResample(predictfinalSVM2,testing$GradRate)[1]
SVM2finalRMSE

#Bayesian Regular Network regression method = brnn with the features we found with greedy selection
#Did not work      
# Error is Something is wrong; all the RMSE metric values are missing
#Error: Stopping
modelfinalBRNN=train(GradRate ~.,training,method="brnn")
predictfinalBRNN=predict(modelfinalBRNN,testing)
BRNNfinalRMSE=postResample(predictfinalBRNN,testing$GradRate)[1]
BRNNfinalRMSE


#Principal Component Analysis regression method = pcr with the features we found with greedy selection
# RMSE 0.04242075
modelfinalPCR=train(GradRate ~.,training,method="pcr")
predictfinalPCR=predict(modelfinalPCR,testing)
PCRfinalRMSE=postResample(predictfinalPCR,testing$GradRate)[1]
PCRfinalRMSE

#Support Vector Machines with Polynomial Kernel regression method = svmPoly with the features we found with greedy selection
# RMSE 0.04873651 
modelfinalSVMP=train(GradRate ~.,training,method="svmPoly")
predictfinalSVMP=predict(modelfinalSVMP,testing)
SVMPfinalRMSE=postResample(predictfinalSVMP,testing$GradRate)[1]
SVMPfinalRMSE


#Neural Network regression method = neuralnet with the features we found with greedy selection
# Did not work
# Message:Something is wrong; all the RMSE metric values are missing
modelfinalNN1=train(GradRate ~.,training,method="neuralnet")
predictfinalNN1=predict(modelfinalNN1,testing)
NN1finalRMSE=postResample(predictfinalNN1,testing$GradRate)[1]
NN1finalRMSE



#Repeated K-fold cross validation with the whole trainData --31 Variables--

train_control <- trainControl(method="repeatedcv", number=5, repeats=3)
#m1 <- train(Ozone~., data=air, trControl=train_control, method="rf")
mdlLM <-train(GradRate~., data=trainData,trControl=train_control, method="lm")
mdlRF <-train(GradRate~., data=trainData,trControl=train_control, method="rf")
mdlSVM3 <-train(GradRate~., data=trainData,trControl=train_control, method="svmLinear3")
mdlSVM2 <-train(GradRate~., data=trainData,trControl=train_control, method="svmLinear2")
mdlPCR <-train(GradRate~., data=trainData,trControl=train_control, method="pcr")
#mdlSVMP <-train(GradRate~., data=trainData,trControl=train_control, method="svmPoly")
#didnt't finish
allModels=resamples(list(LinearRegression=mdlLM,RandomForest=mdlRF,SupporVector3=mdlSVM3,SupporVector2=mdlSVM2,PrincipalComponent=mdlPCR))
bwplot(allModels,scales=list(relation="free"))

#Variable explanations:
#ADM_RATE: Admission rate
#UGDS: Enrollment of undergraduate certificate/degree-seeking students	student	size	
#UGDS_WHITE: Total share of  students who are white		
#UGDS_BLACK: Total share of students who are black
#UGDS_HISP: Total share of who are Hispanic	
#UGDS_ASIAN: Total share of students who are Asian	
#UGDS_AIAN: Total share of students who are American Indian/Alaska Native			
#UGDS_NHPI: Total share of students who are Native Hawaiian/Pacific Islander	
#SAT_AVG: Average SAT equivalent score of students admitted	admissions	sat_scores.average.overall
#SAT_AVG_ALL: Average SAT equivalent score of students admitted for all campuses rolled up to the 6-digit OPE ID	
#SATVR75: 75% percentile score on SAT reading 
#SATWR75: 75% percentile score on SAT writing   
#SATMT75: 75% percentile score on SAT math   
#TUITIONFEE_IN: In-state tuition and fees		
#TUITIONFEE_OUT: Out-of-state tuition and fees	
#C150_4_WHITE: Completion rate for first-time, full-time students at four-year institutions (150% of expected time to completion) for white students		
#C150_4_BLACK: Completion rate for first-time, full-time students at four-year institutions (150% of expected time to completion) for black students		
#C150_4_HISP: Completion rate for first-time, full-time students at four-year institutions (150% of expected time to completion) for Hispanic students	
#C150_4_ASIAN:Completion rate for first-time, full-time students at four-year institutions (150% of expected time to completion) for Asian students	
#C150_4_AIAN: Completion rate for first-time, full-time students at four-year institutions (150% of expected time to completion) for American Indian/Alaska Native students		
#C150_4_NHPI: Completion rate for first-time, full-time students at four-year institutions (150% of expected time to completion) for Native Hawaiian/Pacific Islander students			
#CONTROL 

