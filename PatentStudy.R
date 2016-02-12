require("XML")
require("plyr")
require("gridExtra")
library(timeDate)
require(ggplot2)
library(lattice)
library(reshape2)
library(data.table)
library(sfsmisc)
library(grid)
library(stringr)
require(gridExtra)
library(e1071)
library(class)
library(robustbase)
library(cvTools)
library(car)
library(randomForest)
require(scales)

rm(list = ls())

# data <- xmlParse("USRE041169E.xml")
# 
# #datatable=ldply(xmlToList("US8005455B2.xml"), data.frame)
# # Read XML Files into R and run analysis
# 
# 
# 
# #Some basic XML exploration functions
# xmltop=xmlRoot(data) #Gives content of root
# class(xmltop)#"XMLInternalElementNode" "XMLInternalNode" "XMLAbstractNode"
# a=xmlName(xmltop) #give name of node, PubmedArticleSet
# b=xmlSize(xmltop) #how many children in node, 19
# c=xmlName(xmltop[[1]]) #name of root's child
# 
# # Exploring subnodes
# 
# #Root Node's children
# q=xmlSize(xmltop[[1]]) #number of nodes in each child
# r=xmlSApply(xmltop[[1]], xmlName) #name(s)
# s=xmlSApply(xmltop[[1]], xmlAttrs) #attribute(s)
# t=xmlSApply(xmltop[[1]], xmlSize) #size
# 
# #Populate a data frame
# 
# #xml_data <- xmlToList(data)
# #MasterFrame=xmlToDataFrame(data)


# Key Statistics

# Load Scraped Forward Citations

setwd("/Users/SumanthSwaminathan/Documents/DataScienceCourse/DataIncubator2015")

LitForwardCit <- as.numeric(scan("LitCitData.txt", what="", sep="\n"))
UnLitForwardCit <- as.numeric(scan("UnLitCitData.txt", what="", sep="\n"))

#Collect relevant data for both Litigated XMLS


setwd("/Users/SumanthSwaminathan/Documents/DataScienceCourse/DataIncubator2015/XMLs/Litigated_Training")

count=1
files <- list.files()

#Initialize Vectors
Tot = length(files)
Nclaims = vector(,Tot)
Ncits = vector(,Tot)
Norg = vector(,Tot)
Ninv = vector(,Tot)
Nleg = vector(,Tot)
Assignmentvec = vector(,Tot)
Correctionvec = vector(,Tot)
Disclaimervec = vector(,Tot)
Reexamvec = vector(,Tot)

NAssignment=0
NCorrection=0
NDisclaimer=0
NFeePayment=0
NReexam=0



Ngeneral=vector()
countleg=0
for (file in files) 
{
  Acount=0
  Ccount=0
  Dcount=0
  Rcount=0
    data <- xmlTreeParse(file)
    xmltop=xmlRoot(data)
    
    if (length(xmltop[[1]][["number-of-claims"]])==0)
    { Nclaims[count]=21
    }
    else
    {
      Nclaims[count] = as.numeric(xmlValue(xmltop[[1]][["number-of-claims"]]))
    }
    
    
    if (length(xmltop[[1]][["references-cited"]])==0)
    { 
      Ncits[count]=291
    }
    else
    {
      Ncits[count] = length(xmlSApply(xmltop[[1]][["references-cited"]], xmlName)) #forward and prior
    }
    
    if (length(xmltop)>2){
      
      if (length(xmltop[[3]])==0)
      { 
        Nleg[count]=0
      }
    else
      {
      Nleg[count] = length(xmlSApply(xmltop[[3]], xmlName))
      for (n in 1:Nleg[count] ){
        type = toString(xmltop[[3]][[n]][['legal-description']][['text']])
        Ngeneral[countleg] = toString(xmltop[[3]][[n]][['legal-description']][['text']])
        countleg=countleg+1
        if (type=='ASSIGNMENT'){
          NAssignment=NAssignment+1
          Acount = Acount+1
        }
        else if (type=="CERTIFICATE OF CORRECTION"){
          NCorrection = NCorrection+1
          Ccount = Ccount+1
        }
        else if (type=="DISCLAIMER FILED"){
          NDisclaimer = NDisclaimer + 1
          Dcount = Dcount+1
        }
        else if (type=="FEE PAYMENT"){
          NFeePayment = NFeePayment+1
        }
        else if (type=="REQUEST FOR REEXAMINATION FILED"){
          NReexam = NReexam + 1
          Rcount = Rcount+1
        }
     }
     Assignmentvec[count] = Acount
     Correctionvec[count] = Ccount
     Disclaimervec[count] = Dcount
     Reexamvec[count] = Rcount
      }
    }

    if (length(xmltop[[1]][["parties"]][["applicants"]][["applicant"]][["addressbook"]][["orgname"]])==0)
    { 
      Norg[count]="NA"
    }
    else
    {
      Norg[count] = xmlValue(xmltop[[1]][["parties"]][["applicants"]][["applicant"]][["addressbook"]][["orgname"]]) #applicant organization
    }
    
    if (length(xmltop[[1]][["parties"]][["inventors"]][["inventor"]][["addressbook"]][["last-name"]])==0)
    { Ninv[count]="NA"
    }
    else
    {
      Ninv[count] = xmlValue(xmltop[[1]][["parties"]][["inventors"]][["inventor"]][["addressbook"]][["last-name"]])
    }
    
    count=count+1
    
}
LitData = data.frame(Claims=Nclaims, Citations=Ncits, ForwardCitations = LitForwardCit, LegalEvents=Nleg, Applicant=Norg, Inventor=Ninv, Litigation=rep("Litigated", Tot))
LitDataLegalCat = data.frame(Assignments=Assignmentvec, Corrections = Correctionvec, Disclaimers = Disclaimervec, Reexaminations=Reexamvec)
LitDataLegal = c(NAssignment, NCorrection, NReexam, NDisclaimer)


#Collect relevant data for both Litigated XMLS

setwd("/Users/SumanthSwaminathan/Documents/DataScienceCourse/DataIncubator2015/XMLs/Unlitigated_Training")

count=1
countleg=0
files <- list.files()

#Initialize Vectors
Tot = length(files)
Nclaims = vector(,Tot)
Ncits = vector(,Tot)
Norg = vector(,Tot)
Ninv = vector(,Tot)
Nleg = vector(,Tot)
LegType = vector(,Tot)
Assignmentvec = vector(,Tot)
Correctionvec = vector(,Tot)
Disclaimervec = vector(,Tot)
Reexamvec = vector(,Tot)
NAssignment=0
NCorrection=0
NDisclaimer=2
NFeePayment=15
NReexam=4
Ngeneral=vector()
countleg=0

for (file in files) 
{
  Acount=0
  Ccount=0
  Dcount=0
  Rcount=0
  data <- xmlTreeParse(file)
  xmltop=xmlRoot(data)
  
  if (length(xmltop[[1]][["number-of-claims"]])==0)
  { Nclaims[count]=0
  }
  else
  {
    Nclaims[count] = as.numeric(xmlValue(xmltop[[1]][["number-of-claims"]]))
  }
  
  
  if (length(xmltop[[1]][["references-cited"]])==0)
  { Ncits[count]=0
  }
  else
  {
    Ncits[count] = length(xmlSApply(xmltop[[1]][["references-cited"]], xmlName)) #forward and prior
  }
  if (length(xmltop)>2){
    
    if (length(xmltop[[3]])==0)
    { Nleg[count]=0
    }
    else
    {
      Nleg[count] = length(xmlSApply(xmltop[[3]], xmlName))
      for (n in 1:Nleg[count] ){
        type = toString(xmltop[[3]][[n]][['legal-description']][['text']])
        Ngeneral[countleg] = toString(xmltop[[3]][[n]][['legal-description']][['text']])
        countleg=countleg+1
        if (type=='ASSIGNMENT'){
          NAssignment=NAssignment+1
        }
        else if (type=="CERTIFICATE OF CORRECTION"){
          NCorrection = NCorrection+1
        }
        else if (type=="DISCLAIMER FILED"){
          NDisclaimer = NDisclaimer + 1
        }
        else if (type=="FEE PAYMENT"){
          NFeePayment = NFeePayment+1
        }
        else if (type=="REQUEST FOR REEXAMINATION FILED"){
          NReexam = NReexam + 1
        }
      }
      Assignmentvec[count] = Acount
      Correctionvec[count] = Ccount
      Disclaimervec[count] = Dcount
      Reexamvec[count] = Rcount
    }
  }
  if (length(xmltop[[1]][["parties"]][["applicants"]][["applicant"]][["addressbook"]][["orgname"]])==0)
  { Norg[count]="NA"
  }
  else
  {
    Norg[count] = xmlValue(xmltop[[1]][["parties"]][["applicants"]][["applicant"]][["addressbook"]][["orgname"]]) #applicant organization
  }
  
  if (length(xmltop[[1]][["parties"]][["inventors"]][["inventor"]][["addressbook"]][["last-name"]])==0)
  { Ninv[count]="NA"
  }
  else
  {
    Ninv[count] = xmlValue(xmltop[[1]][["parties"]][["inventors"]][["inventor"]][["addressbook"]][["last-name"]])
  }
  
  count=count+1
  
}

UnLitData = data.frame(Claims=Nclaims, Citations=Ncits, ForwardCitations = UnLitForwardCit, LegalEvents=Nleg, Applicant=Norg, Inventor=Ninv, Litigation=rep("UnLitigated", Tot))
UnLitDataLegalCat = data.frame(Assignments=Assignmentvec, Corrections = Correctionvec, Disclaimers = Disclaimervec, Reexaminations=Reexamvec)
UnLitDataLegal = c(NAssignment, NCorrection, NReexam, NDisclaimer)
setwd("/Users/SumanthSwaminathan/Documents/DataScienceCourse/DataIncubator2015")

totassign = LitDataLegal[1]+UnLitDataLegal[1]
totcorrect = LitDataLegal[2]+UnLitDataLegal[2]
totexam = LitDataLegal[3]+UnLitDataLegal[3]
totdisclaim = LitDataLegal[4]+UnLitDataLegal[4]

## Create Plots
#par(mar=c(5,1,2,1))

#Bar Plot of Sum Total Statistics Summarized
Masterdata=rbind(LitData,UnLitData)
plottable=ddply(Masterdata,~Litigation,summarise,Claims=sum(Claims),Citations=sum(Citations),LegalEvents=sum(LegalEvents))

plottable$Claims=plottable$Claims/sum(plottable$Claims)
plottable$Citations=plottable$Citations/sum(plottable$Citations)
plottable$LegalEvents=plottable$LegalEvents/sum(plottable$LegalEvents)

barplot(as.matrix(plottable[,2:4]), main="Litigated Vs Unlitigated Feature Summary", ylab="Total Fraction", col=c("darkblue","red"), cex.axis=1.1, cex.names=1.1, cex.lab=1.1, beside=TRUE)
legend("topleft", legend = c("Litigated","Unlitigated"), cex=0.8, fill=c("darkblue","red"))
dev.copy(png, file="BarPlot.png", width=900, height=600) #Copy my plot to a PNG file
dev.off() #close dev environment


#Box Plot of Statistics

MasterDataP=Masterdata
MasterDataP$Claims=MasterDataP$Claims/max(MasterDataP$Claims)
MasterDataP$Citations=MasterDataP$Citations/max(MasterDataP$Citations)
MasterDataP$LegalEvents=MasterDataP$LegalEvents/max(MasterDataP$LegalEvents)
MasterDataP$ForwardCitations=MasterDataP$ForwardCitations/max(MasterDataP$ForwardCitations)

MasterDataPn=MasterDataP[,c(1:4,7)]
dfm <- melt(MasterDataPn, id.vars="Litigation")
p2=ggplot(dfm, aes(x=Litigation, y=value, fill=variable)) + geom_boxplot() + theme(text = element_text(size=20))
p2=p2+ylab("Normalized Frequency")

grid.arrange(p2)
dev.copy(png, file="BoxPlot.png", width=900, height=600) #Copy my plot to a PNG file
dev.off() #close dev environment

Type      <- c(rep(c("Assignment", "Correction", "Reexamination", "Disclaimer"), each = 2))
Category  <- c(rep(c('Litigated', 'UnLitigated'), times = 2))
Frequency <- c(round(LitDataLegal[1]/totassign, digits=3)*100, round(UnLitDataLegal[1]/totassign, digits=3)*100, round(LitDataLegal[2]/totcorrect, digits=3)*100, round(UnLitDataLegal[2]/totcorrect, digits=3)*100, round(LitDataLegal[3]/totexam,digits=3)*100 , round(UnLitDataLegal[3]/totexam, digits=3)*100, round(LitDataLegal[4]/totdisclaim, digits=3)*100, round(UnLitDataLegal[4]/totdisclaim, digits=3)*100)
Data      <- data.frame(Type, Category, Frequency)
p <- qplot(Type, Frequency, data = Data, geom = "bar", stat='identity', fill = Category, theme_set(theme_bw()))
p = p + geom_text(aes(label = Frequency), size = 7, hjust = 0.5, vjust = 3, position = "stack") + theme_bw() + theme(text = element_text(size=20))
p = p + ylab("Normalized Frequency (%)") + xlab("Legal Event")
print(p)
dev.copy(png, file="LegalBarPlot.png", width=900, height=600) #Copy my plot to a PNG file
dev.off() #close dev environment
#boxplot(MasterDataP, main = "",fill=c("gold","darkgreen"), notch = TRUE, col = 1:3)



##Begin Machine Learning Predictions

# Prediction on Complete Data Set


#Transform Data
MasterDataL = rbind(LitDataLegalCat,UnLitDataLegalCat)
MasterDataNew = cbind(MasterDataP[,c(1:4,7)],MasterDataL) 
rows=sample(nrow(MasterDataNew))

ModelData = MasterDataNew[rows,]

#Divide Into Testing and Training Sets
Tot = as.numeric(dim(MasterDataNew)[1])
PercentTraining = 0.65
Ntraining = round(Tot*PercentTraining)

train <- ModelData[1:Ntraining,]
test <- ModelData[(Ntraining+1):Tot,]


#Classification with different models


#Linear Logistic Regression

LogReg <- glm(Litigation ~.,family=binomial(link='logit'), data=train)
LogReg.pred <- predict(LogReg,newdata = test,type='response')
LogReg.pred <- ifelse(LogReg.pred > 0.5,1,0)
LogReg.pred = recode(LogReg.pred, '1="UnLitigated"; 0="Litigated"')
#LogReg.pred <- ifelse(LogReg.pred > 0.5,1,0)
#misClasificError <- mean(LogReg.pred != model.matrix( ~ Litigation - 1, data=test )[,2])
misClasificError <- 1-mean(LogReg.pred  == test$Litigation)

LogLitError = 1-mean(LogReg.pred[test$Litigation=='Litigated'] == test$Litigation[test$Litigation=='Litigated'])
LogUnLitError = 1-mean(LogReg.pred[test$Litigation=='UnLitigated'] == test$Litigation[test$Litigation=='UnLitigated'])

LOGvec = c(misClasificError, LogLitError, LogUnLitError)


#Naive Bayes

NBayes <- naiveBayes(Litigation ~ ., data = train)
NBayes.pred <- predict(NBayes, test)
misClasificError <- 1-mean(NBayes.pred == test$Litigation)
NBLitError = 1-mean(NBayes.pred[test$Litigation=='Litigated'] == test$Litigation[test$Litigation=='Litigated'])
NBUnLitError = 1-mean(NBayes.pred[test$Litigation=='UnLitigated'] == test$Litigation[test$Litigation=='UnLitigated'])
NBvec = c(misClasificError, NBLitError, NBUnLitError)

#Support Vector Machine

SVM <- svm(Litigation ~ ., data = train, cost = 100, gamma = 1)
svm.pred  <- predict(SVM, test)
misClasificError <- 1-mean(svm.pred == test$Litigation)
SVMLitError = 1-mean(svm.pred[test$Litigation=='Litigated'] == test$Litigation[test$Litigation=='Litigated'])
SVMUnLitError = 1-mean(svm.pred[test$Litigation=='UnLitigated'] == test$Litigation[test$Litigation=='UnLitigated'])
SVMvec = c(misClasificError, SVMLitError, SVMUnLitError)

# K Nearest Neighbors

KNN.pred <- knn(train[-5], test[-5],cl = train$Litigation, k=10)
misClasificError <- 1-mean(KNN.pred == test$Litigation)
KNNLitError = 1-mean(KNN.pred[test$Litigation=='Litigated'] == test$Litigation[test$Litigation=='Litigated'])
KNNUnLitError = 1-mean(KNN.pred[test$Litigation=='UnLitigated'] == test$Litigation[test$Litigation=='UnLitigated'])
KNNvec = c(misClasificError, KNNLitError, KNNUnLitError)

#Random Forest

rf1 <- randomForest(Litigation ~ ., data = train)
rf1.pred <- predict(rf1, test)
misClasificError <- 1-mean(rf1.pred==test$Litigation)
rfLitError = 1-mean(rf1.pred[test$Litigation=='Litigated'] == test$Litigation[test$Litigation=='Litigated'])
rfUnLitError = 1-mean(rf1.pred[test$Litigation=='UnLitigated'] == test$Litigation[test$Litigation=='UnLitigated'])
RFvec = c(misClasificError, rfLitError, rfUnLitError)

#Make Visualization of predictions
Error = c(NBvec, SVMvec, KNNvec, LOGvec, RFvec)
ErrorType = rep(c('Total MisClassification Error','% Litigated Error', '% UnLitigated Error'), 5)
Method = c(rep('NaiveBayes',3), rep('SupportVectorMachine',3), rep('Knn',3), rep('LogisticRegression',3), rep('RandomForest',3))
PredictionData = data.frame(Error=Error, ErrorType = ErrorType, Method = Method)
PredictionData$Method <- factor(PredictionData$Method, levels=unique(as.character(PredictionData$Method)) ) #Order by Type Entered in Data Frame
PredictionData$ErrorType <- factor(PredictionData$ErrorType, levels=unique(as.character(PredictionData$ErrorType)) ) #Order by Type Entered in Data Frame
p3 = ggplot(PredictionData, aes(x = Method, y = Error, fill=ErrorType)) + geom_bar(position="dodge", stat='identity') + coord_flip() 
p3 = p3 + ylab("Error") + xlab("Classifier") + scale_y_continuous(limits = c(0,0.8), breaks = round(seq(0, 0.8, by = 0.1),1)) + labs(title ="Classifier Performance (Full Feature Set)") + theme_bw() + theme(text = element_text(size=20))
print(p3)
dev.copy(png, file="ClassifierPlotComplete.png", width=900, height=600) #Copy my plot to a PNG file
dev.off() #close dev environment

# Prediction on Partial Data Set
#Transform Data
#MasterDataL = rbind(LitDataLegalCat,UnLitDataLegalCat)
MasterDataNew = MasterDataP[,c(1:4,7)]
ModelData = MasterDataNew[rows,]

#Divide Into Testing and Training Sets

train <- ModelData[1:Ntraining,]
test <- ModelData[(Ntraining+1):Tot,]


#Classification with different models


#Linear Logistic Regression

LogReg <- glm(Litigation ~.,family=binomial(link='logit'), data=train)
LogReg.pred <- predict(LogReg,newdata = test,type='response')
LogReg.pred <- ifelse(LogReg.pred > 0.5,1,0)
LogReg.pred = recode(LogReg.pred, '1="UnLitigated"; 0="Litigated"')
#LogReg.pred <- ifelse(LogReg.pred > 0.5,1,0)
#misClasificError <- mean(LogReg.pred != model.matrix( ~ Litigation - 1, data=test )[,2])
misClasificError <- 1-mean(LogReg.pred  == test$Litigation)

LogLitError = 1-mean(LogReg.pred[test$Litigation=='Litigated'] == test$Litigation[test$Litigation=='Litigated'])
LogUnLitError = 1-mean(LogReg.pred[test$Litigation=='UnLitigated'] == test$Litigation[test$Litigation=='UnLitigated'])

LOGvec = c(misClasificError, LogLitError, LogUnLitError)


#Naive Bayes

NBayes <- naiveBayes(Litigation ~ ., data = train)
NBayes.pred <- predict(NBayes, test)
misClasificError <- 1-mean(NBayes.pred == test$Litigation)
NBLitError = 1-mean(NBayes.pred[test$Litigation=='Litigated'] == test$Litigation[test$Litigation=='Litigated'])
NBUnLitError = 1-mean(NBayes.pred[test$Litigation=='UnLitigated'] == test$Litigation[test$Litigation=='UnLitigated'])
NBvec = c(misClasificError, NBLitError, NBUnLitError)

#Support Vector Machine

SVM <- svm(Litigation ~ ., data = train, cost = 100, gamma = 1)
svm.pred  <- predict(SVM, test)
misClasificError <- 1-mean(svm.pred == test$Litigation)
SVMLitError = 1-mean(svm.pred[test$Litigation=='Litigated'] == test$Litigation[test$Litigation=='Litigated'])
SVMUnLitError = 1-mean(svm.pred[test$Litigation=='UnLitigated'] == test$Litigation[test$Litigation=='UnLitigated'])
SVMvec = c(misClasificError, SVMLitError, SVMUnLitError)

# K Nearest Neighbors

KNN.pred <- knn(train[-5], test[-5],cl = train$Litigation, k=10)
misClasificError <- 1-mean(KNN.pred == test$Litigation)
KNNLitError = 1-mean(KNN.pred[test$Litigation=='Litigated'] == test$Litigation[test$Litigation=='Litigated'])
KNNUnLitError = 1-mean(KNN.pred[test$Litigation=='UnLitigated'] == test$Litigation[test$Litigation=='UnLitigated'])
KNNvec = c(misClasificError, KNNLitError, KNNUnLitError)

#Random Forest

rf1 <- randomForest(Litigation ~ ., data = train)
rf1.pred <- predict(rf1, test)
misClasificError <- 1-mean(rf1.pred==test$Litigation)
rfLitError = 1-mean(rf1.pred[test$Litigation=='Litigated'] == test$Litigation[test$Litigation=='Litigated'])
rfUnLitError = 1-mean(rf1.pred[test$Litigation=='UnLitigated'] == test$Litigation[test$Litigation=='UnLitigated'])
RFvec = c(misClasificError, rfLitError, rfUnLitError)

#Make Visualization of predictions
Error = c(NBvec, SVMvec, KNNvec, LOGvec, RFvec)
ErrorType = rep(c('Total MisClassification Error','% Litigated Error', '% UnLitigated Error'), 5)
Method = c(rep('NaiveBayes',3), rep('SupportVectorMachine',3), rep('Knn',3), rep('LogisticRegression',3), rep('RandomForest',3))
PredictionData = data.frame(Error=Error, ErrorType = ErrorType, Method = Method)
PredictionData$Method <- factor(PredictionData$Method, levels=unique(as.character(PredictionData$Method)) ) #Order by Type Entered in Data Frame
PredictionData$ErrorType <- factor(PredictionData$ErrorType, levels=unique(as.character(PredictionData$ErrorType)) ) #Order by Type Entered in Data Frame
p4=ggplot(PredictionData, aes(x = Method, y = Error, fill=ErrorType)) + geom_bar(position="dodge", stat='identity') + coord_flip() 
p4=p4+ylab("Error") + xlab("Classifier") + scale_y_continuous(limits = c(0,0.8), breaks = round(seq(0, 0.8, by = 0.1),1)) + labs(title ="Classifier Performance (No Legal Events)") + theme_bw() + theme(text = element_text(size=20))
print(p4)
dev.copy(png, file="ClassifierPlotPartial.png", width=900, height=600) #Copy my plot to a PNG file
dev.off() #close dev environment

# Prediction on Partial Data Set
#Transform Data

MasterDataNew = cbind(MasterDataP[,c(3:4,7)],MasterDataL) 
ModelData = MasterDataNew[rows,]

#Divide Into Testing and Training Sets

train <- ModelData[1:Ntraining,]
test <- ModelData[(Ntraining+1):Tot,]


#Classification with different models


#Linear Logistic Regression

LogReg <- glm(Litigation ~.,family=binomial(link='logit'), data=train)
LogReg.pred <- predict(LogReg,newdata = test,type='response')
LogReg.pred <- ifelse(LogReg.pred > 0.5,1,0)
LogReg.pred = recode(LogReg.pred, '1="UnLitigated"; 0="Litigated"')
#LogReg.pred <- ifelse(LogReg.pred > 0.5,1,0)
#misClasificError <- mean(LogReg.pred != model.matrix( ~ Litigation - 1, data=test )[,2])
misClasificError <- 1-mean(LogReg.pred  == test$Litigation)

LogLitError = 1-mean(LogReg.pred[test$Litigation=='Litigated'] == test$Litigation[test$Litigation=='Litigated'])
LogUnLitError = 1-mean(LogReg.pred[test$Litigation=='UnLitigated'] == test$Litigation[test$Litigation=='UnLitigated'])

LOGvec = c(misClasificError, LogLitError, LogUnLitError)


#Naive Bayes

NBayes <- naiveBayes(Litigation ~ ., data = train)
NBayes.pred <- predict(NBayes, test)
misClasificError <- 1-mean(NBayes.pred == test$Litigation)
NBLitError = 1-mean(NBayes.pred[test$Litigation=='Litigated'] == test$Litigation[test$Litigation=='Litigated'])
NBUnLitError = 1-mean(NBayes.pred[test$Litigation=='UnLitigated'] == test$Litigation[test$Litigation=='UnLitigated'])
NBvec = c(misClasificError, NBLitError, NBUnLitError)

#Support Vector Machine

SVM <- svm(Litigation ~ ., data = train, cost = 100, gamma = 1)
svm.pred  <- predict(SVM, test)
misClasificError <- 1-mean(svm.pred == test$Litigation)
SVMLitError = 1-mean(svm.pred[test$Litigation=='Litigated'] == test$Litigation[test$Litigation=='Litigated'])
SVMUnLitError = 1-mean(svm.pred[test$Litigation=='UnLitigated'] == test$Litigation[test$Litigation=='UnLitigated'])
SVMvec = c(misClasificError, SVMLitError, SVMUnLitError)

# K Nearest Neighbors

KNN.pred <- knn(train[-3], test[-3],cl = train$Litigation, k=10)
misClasificError <- 1-mean(KNN.pred == test$Litigation)
KNNLitError = 1-mean(KNN.pred[test$Litigation=='Litigated'] == test$Litigation[test$Litigation=='Litigated'])
KNNUnLitError = 1-mean(KNN.pred[test$Litigation=='UnLitigated'] == test$Litigation[test$Litigation=='UnLitigated'])
KNNvec = c(misClasificError, KNNLitError, KNNUnLitError)

#Random Forest

rf1 <- randomForest(Litigation ~ ., data = train)
rf1.pred <- predict(rf1, test)
misClasificError <- 1-mean(rf1.pred==test$Litigation)
rfLitError = 1-mean(rf1.pred[test$Litigation=='Litigated'] == test$Litigation[test$Litigation=='Litigated'])
rfUnLitError = 1-mean(rf1.pred[test$Litigation=='UnLitigated'] == test$Litigation[test$Litigation=='UnLitigated'])
RFvec = c(misClasificError, rfLitError, rfUnLitError)

#Make Visualization of predictions
Error = c(NBvec, SVMvec, KNNvec, LOGvec, RFvec)
ErrorType = rep(c('Total MisClassification Error','% Litigated Error', '% UnLitigated Error'), 5)
Method = c(rep('NaiveBayes',3), rep('SupportVectorMachine',3), rep('Knn',3), rep('LogisticRegression',3), rep('RandomForest',3))
PredictionData = data.frame(Error=Error, ErrorType = ErrorType, Method = Method)
PredictionData$Method <- factor(PredictionData$Method, levels=unique(as.character(PredictionData$Method)) ) #Order by Type Entered in Data Frame
PredictionData$ErrorType <- factor(PredictionData$ErrorType, levels=unique(as.character(PredictionData$ErrorType))) #Order by Type Entered in Data Frame
p5=ggplot(PredictionData, aes(x = Method, y = Error, fill=ErrorType)) + geom_bar(position="dodge", stat='identity') + coord_flip() 
p5=p5 + ylab("Error") + xlab("Classifier") + scale_y_continuous(limits = c(0,0.8), breaks = round(seq(0, 0.8, by = 0.1),1)) + labs(title ="Classifier Performance (Forward Citations and Legal Events)") + theme_bw() + theme(text = element_text(size=20))
print(p5)
dev.copy(png, file="ClassifierPlotSignals.png", width=900, height=600) #Copy my plot to a PNG file
dev.off() #close dev environment

grid.arrange(p4,p5,p3, ncol=3)

dev.copy(png, file="ClassifierComparison.png", width=2000, height=1000) #Copy my plot to a PNG file
dev.off() #close dev environment

