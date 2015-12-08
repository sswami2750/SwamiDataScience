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
require(gridExtra)

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

for (file in files) 
{
    data <- xmlParse(file)
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
    
    if (length(xmltop[[3]])==0)
    { 
      Nleg[count]=0
    }
    else
    {
      Nleg[count] = length(xmlSApply(xmltop[[3]], xmlName))
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

LitData=data.frame(Claims=Nclaims, Citations=Ncits, LegalEvents=Nleg, Applicant=Norg, Inventor=Ninv, Litigation=rep("Litigated", Tot))

#Collect relevant data for both Litigated XMLS

setwd("/Users/SumanthSwaminathan/Documents/DataScienceCourse/DataIncubator2015/XMLs/Unlitigated_Training")

count=1
files <- list.files()

#Initialize Vectors
Tot = length(files)
Nclaims = vector(,Tot)
Ncits = vector(,Tot)
Norg = vector(,Tot)
Ninv = vector(,Tot)
Nleg = vector(,Tot)

for (file in files) 
{
  data <- xmlParse(file)
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
  
  if (length(xmltop[[3]])==0)
  { Nleg[count]=0
  }
  else
  {
    Nleg[count] = length(xmlSApply(xmltop[[3]], xmlName))
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

UnLitData=data.frame(Claims=Nclaims, Citations=Ncits, LegalEvents=Nleg, Applicant=Norg, Inventor=Ninv, Litigation=rep("UnLitigated", Tot))

setwd("/Users/SumanthSwaminathan/Documents/DataScienceCourse/DataIncubator2015")

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

MasterDataPn=MasterDataP[,c(1:3,6)]
dfm <- melt(MasterDataPn, id.vars="Litigation")
p2=ggplot(dfm, aes(x=Litigation, y=value, fill=variable)) + geom_boxplot() + theme(text = element_text(size=20))
p2=p2+ylab("Normalized Value")

grid.arrange(p2)
dev.copy(png, file="BoxPlot.png", width=900, height=600) #Copy my plot to a PNG file
dev.off() #close dev environment

#boxplot(MasterDataP, main = "",fill=c("gold","darkgreen"), notch = TRUE, col = 1:3)

# ggplot(data = Masterdata, aes(x = Litigation, y = x)) + 
#   geom_boxplot(aes(fill = Litigation), width = 0.8) + theme_bw()
#dev.copy(png, file="plot4.png", width=580, height=480) #Copy my plot to a PNG file
#dev.off() #close dev environment

#Put in grouped bar plots
#Health Data

# Nt=5
# plottable1=Dtdth[1:Nt,1:3]
# plottable1n=t(plottable1[,2:ncol(plottable1)])
# colnames(plottable1n)=plottable1[,1]
# 
# #Economic Data
# Nt=5
# plottable2=Dtdtd[1:Nt,c(1,4,5)]
# plottable2n=t(plottable2[,2:ncol(plottable2)])
# colnames(plottable2n)=plottable2[,1]
# 
# 
# 
# 
# #Economic Data
# par(mfrow=c(1,3))
# 
# # # Create Bar Plot
# barplot(plottable1n, main="Percent Injuries & Fatalities by Storm Type",
#         xlab="Storm type", col=c("darkblue","red"),
#         legend = c("% Fatalities","% Injuries"), beside=TRUE)
