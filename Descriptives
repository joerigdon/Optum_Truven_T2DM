##Descriptive statistics for Optum One Percent##
#Load data
j = read.csv("\\\\phs-isilon.private/phs/users/jrigdon/T2DM/diab_optum1.csv", header=TRUE)

#Load table making functions
install.packages("Gmisc")
install.packages("Hmisc")
install.packages("rJava")
install.packages("ReporteRs")
install.packages("rJava", .libPaths()[1], "http://www.rforge.net/")
install.packages("fmsb")
#library(devtools)

#Have to source "raw" Github files
Sys.setenv(JAVA_HOME="C:/Program_Files/Java/jre1.8.0_121/bin/server")
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)
library(ReporteRs)
library(rJava) #problem is here

source("https://raw.githubusercontent.com/joerigdon/Useful_Functions/master/Tables.R") #error w/ Java here?
source("https://raw.githubusercontent.com/joerigdon/Useful_Functions/master/Tables2.R") #version w/o Java dependency
source("https://raw.githubusercontent.com/joerigdon/Useful_Functions/master/Figures.R")
source("https://raw.githubusercontent.com/joerigdon/Useful_Functions/master/Functions.R")

#Define variables
#Age
j2 = j[1:10, ]
head(as.Date(j2$FST_DT))
j2$age = as.numeric(as.Date(j2$FST_DT)-as.Date(paste(j2$YRDOB, "-01-01", sep="")))/
         365.25
j$age = as.numeric(as.Date(j$FST_DT)-as.Date(paste(j$YRDOB, "-01-01", sep="")))/
         365.25
summary(j$age)

#One row per person data set
d = j[!duplicated(j$PATID), ]
dim(d)

#Tables of demographic variables
#Age
summary(d$age)

#Sex
table(d$GDR_CD, exclude=NULL)

#Race
table(d$D_Race_Code, exclude=NULL)

#Household Income
table(d$D_Household_Income_Range_Code, exclude=NULL)

#Education
table(d$D_Education_Level_Code, exclude=NULL)

#PROVCAT (Provider category?)
table(d$PROVCAT, exclude=NULL)

#Create variables to merge with d (e.g., total costs, ever renal, etc.)
#Total STD_COST
j2 = j[1:1000, ]
tcost = by(j$STD_COST, j$PATID, function(x) sum(x, na.rm=TRUE))

#Health outcomes
renal = by(j$renal, j$PATID, function(x) sum(x, na.rm=TRUE)>0)
opth = by(j$opth, j$PATID, function(x) sum(x, na.rm=TRUE)>0)
neuro = by(j$neuro, j$PATID, function(x) sum(x, na.rm=TRUE)>0)
circ = by(j$circ, j$PATID, function(x) sum(x, na.rm=TRUE)>0)
ulcer = by(j$ulcer, j$PATID, function(x) sum(x, na.rm=TRUE)>0)
ihd = by(j$ihd, j$PATID, function(x) sum(x, na.rm=TRUE)>0)
stroke = by(j$stroke, j$PATID, function(x) sum(x, na.rm=TRUE)>0)
hf = by(j$hf, j$PATID, function(x) sum(x, na.rm=TRUE)>0)

#Make data frame and merge to d
df = data.frame(PATID=names(renal), renal=as.numeric(renal), opth=as.numeric(opth), 
      neuro=as.numeric(neuro), circ=as.numeric(circ),
      ulcer=as.numeric(ulcer), ihd=as.numeric(ihd), 
      stroke=as.numeric(stroke), hf=as.numeric(hf), tcost=as.numeric(tcost))


#Make some simple descriptive tables
keep = c("D_Race_Code", "D_Household_Income_Range_Code", "D_Education_Level_Code",
         "age", "GDR_CD", "PATID")

d2 = d[, names(d) %in% keep]

df2 = merge(df, d2, by="PATID")

summary(df2$tcost) #4M spent on ONE patient!!

#Fix Gender variable
table(df2$GDR_CD, exclude=NULL)
df2$sex = NA
df2$sex[df2$GDR_CD=="F"] = "a.Female"
df2$sex[df2$GDR_CD=="M"] = "b.Male"
table(df2$sex, df2$GDR_CD, exclude=NULL)

#Fix race variable
table(df2$D_Race_Code, exclude=NULL)
df2$race = NA
df2$race[df2$D_Race_Code=="W"] = "a.White"
df2$race[df2$D_Race_Code=="B"] = "b.Black"
df2$race[df2$D_Race_Code=="H"] = "c.Hispanic"
df2$race[df2$D_Race_Code=="A"] = "d.Asian"
table(df2$race, df2$D_Race_Code, exclude=NULL)

#Fix education level
table(df2$D_Education_Level_Code, exclude=NULL)
df2$educ = NA
df2$educ[df2$D_Education_Level_Code=="A"] = "A"
df2$educ[df2$D_Education_Level_Code=="B"] = "B"
df2$educ[df2$D_Education_Level_Code=="C"] = "C"
df2$educ[df2$D_Education_Level_Code=="D"] = "D"
table(df2$educ, df2$D_Education_Level_Code, exclude=NULL)

#Fix income
table(df2$D_Household_Income_Range_Code, exclude=NULL)
df2$inc = df2$D_Household_Income_Range_Code

#Renal table
t.renal = mktab(data=df2, var.names=c("age", "sex", "race", "educ", "inc", "tcost"), 
             ind.cat=c(0, 1, 1, 1, 1, 0), group.name="renal",
             cfn=describeMedian, miss="always", pval=TRUE, tot="last", digit=1)

#Opth table
t.opth = mktab(data=df2, var.names=c("age", "sex", "race", "educ", "inc", "tcost"), 
             ind.cat=c(0, 1, 1, 1, 1, 0), group.name="opth",
             cfn=describeMedian, miss="always", pval=TRUE, tot="last", digit=1)

#Neuro table
t.neuro = mktab(data=df2, var.names=c("age", "sex", "race", "educ", "inc", "tcost"), 
             ind.cat=c(0, 1, 1, 1, 1, 0), group.name="neuro",
             cfn=describeMedian, miss="always", pval=TRUE, tot="last", digit=1)

#Circ table
t.circ = mktab(data=df2, var.names=c("age", "sex", "race", "educ", "inc", "tcost"), 
             ind.cat=c(0, 1, 1, 1, 1, 0), group.name="circ",
             cfn=describeMedian, miss="always", pval=TRUE, tot="last", digit=1)

#Ulcer table
t.ulcer = mktab(data=df2, var.names=c("age", "sex", "race", "educ", "inc", "tcost"), 
             ind.cat=c(0, 1, 1, 1, 1, 0), group.name="ulcer",
             cfn=describeMedian, miss="always", pval=TRUE, tot="last", digit=1)

#IHD
t.ihd = mktab(data=df2, var.names=c("age", "sex", "race", "educ", "inc", "tcost"), 
             ind.cat=c(0, 1, 1, 1, 1, 0), group.name="ihd",
             cfn=describeMedian, miss="always", pval=TRUE, tot="last", digit=1)

#Stroke
t.stroke = mktab(data=df2, var.names=c("age", "sex", "race", "educ", "inc", "tcost"), 
             ind.cat=c(0, 1, 1, 1, 1, 0), group.name="stroke",
             cfn=describeMedian, miss="always", pval=TRUE, tot="last", digit=1)

#HF
t.hf = mktab(data=df2, var.names=c("age", "sex", "race", "educ", "inc", "tcost"), 
             ind.cat=c(0, 1, 1, 1, 1, 0), group.name="hf",
             cfn=describeMedian, miss="always", pval=TRUE, tot="last", digit=1)


#Save and send to Sanjay
word.doc(obj.list=list(t.renal, t.opth, t.neuro, t.circ, t.ulcer, t.ihd,
         t.stroke, t.hf), 
         obj.title=c("Table 1: Demographics by renal status",
                     "Table 2: Demographics by opth status",
                     "Table 3: Demographics by neuro status",
                     "Table 4: Demographics by circ status",
                     "Table 5: Demographics by ulcer status",
                     "Table 6: Demographics by ischemic heart disease status",
                     "Table 7: Demographics by stroke status",
                     "Table 8: Demographics by heart failure status"),  
         dest="\\\\phs-isilon.private/phs/users/jrigdon/T2DM/demo_tables.docx",
         ftype="Arial", col.odd="white")

word.doc(obj.list=list(t.opth), 
         obj.title=c("Table 1: Demographics by opth status"), 
         dest="\\\\phs-isilon.private/phs/users/jrigdon/T2DM/opth.docx",
         ftype="Arial", col.odd="white")

word.doc(obj.list=list(t.neuro), 
         obj.title=c("Table 1: Demographics by neuro status"), 
         dest="\\\\phs-isilon.private/phs/users/jrigdon/T2DM/neuro.docx",
         ftype="Arial", col.odd="white")

word.doc(obj.list=list(t.circ), 
         obj.title=c("Table 1: Demographics by circ status"), 
         dest="\\\\phs-isilon.private/phs/users/jrigdon/T2DM/circ.docx",
         ftype="Arial", col.odd="white")

word.doc(obj.list=list(t.ulcer), 
         obj.title=c("Table 1: Demographics by ulcer status"), 
         dest="\\\\phs-isilon.private/phs/users/jrigdon/T2DM/ulcer.docx",
         ftype="Arial", col.odd="white")

word.doc(obj.list=list(t.ihd), 
         obj.title=c("Table 1: Demographics by ihd status"), 
         dest="\\\\phs-isilon.private/phs/users/jrigdon/T2DM/ihd.docx",
         ftype="Arial", col.odd="white")

word.doc(obj.list=list(t.stroke), 
         obj.title=c("Table 1: Demographics by stroke status"), 
         dest="\\\\phs-isilon.private/phs/users/jrigdon/T2DM/stroke.docx",
         ftype="Arial", col.odd="white")

word.doc(obj.list=list(t.hf), 
         obj.title=c("Table 1: Demographics by hf status"), 
         dest="\\\\phs-isilon.private/phs/users/jrigdon/T2DM/hf.docx",
         ftype="Arial", col.odd="white")




