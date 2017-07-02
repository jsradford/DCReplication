setwd('C://DonorsChoose//Data')

library(MASS)
#library(nlme)
#library(hglm)

#GENERAL ANALYSIS FOR ALL DATA
dta1 <- read.table('DataRerun2002Original2.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"') 
dta2 <- read.table('DataRerun2003Original2.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"')
dta3 <- read.table('DataRerun2004Original2.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"')
dta4 <- read.table('DataRerun2005Original2.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"')
dta5 <- read.table('DataRerun2006Original2.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"') 
dta6 <- read.table('DataRerun2007Original2.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"')
dta7 <- read.table('DataRerun2008Original2.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"')
dta8 <- read.table('DataRerun2009Original2.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"')
dta9 <- read.table('DataRerun2010Original2.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"')
allDta<-rbind(dta3,dta4,dta5,dta6,dta7,dta8,dta9)
#clean out common outliers: TotTime is calculated at 116 months, asked for 10.2 million dollars, reach 10's of thousands of children
#those asking for zero dollars (n=~16)and those with a missing gender ("Dr." or blank)


################ SUBSETTING DATA FOR ANALYSIS #################################
allDta<-subset(allDta,!(allDta$TotTime > 110 | allDta$reach>4000 |allDta$price > 40000|allDta$price==0|is.na(allDta$male)))

#NOTE PRICE AND REACH == 0 IS NOT POSSIBLE.
#allDta$tot<-allDta$applied + allDta$sports + allDta$socsci + allDta$language + allDta$STEM + allDta$arts + allDta$special

#Keep Kitchen sinks' worth of variables
col<-c(1:3,7:9,16:17,22:28,31:37,39:42,45,46,52,63:length(allDta))         #select columns for use as matchit cannot work if any data (even unused) contains NAs
cutDta<-allDta[col]
cutDta<-subset(cutDta,!(is.na(cutDta$poverty)|is.na(cutDta$male)))

#keep structural gender variables - no poverty, future, reach, or lix
#col<-c(1:3,7:9,16,17,22:28,31:36,39:42,52)
#cutDta<-allDta[col]

################    BEGIN MATCHING CODES   #####################################
library(MatchIt)

##### EXACT MATCHING SEX PROGRAM ########

exactModel<-matchit(as.integer(male) ~ gradeK + gradeElem + gradeMid +special + arts + language + socsci + sports + applied
    +altSchool + rural + suburban, method = "exact",data=cutDta)
cutDta$grade<-cutDta$gradeK+cutDta$gradeElem*2+cutDta$gradeMid*3+cutDta$gradeHigh*4
cutDta$grade[cutDta$grade==0]<-NA

exDta<-match.data(exactModel)
col<-c(1,8,length(exDta))   #no distance measure
cDta<-exDta[col]
write.csv(cDta, file="ExactTeacherStrata.csv", col.names = TRUE, sep = ",", qmethod = "double")

##### EXACT MATCHING ALL STRUCTURE ########
exDta<-setNames(data.frame(cutDta$projectid),c('projectid'))
cutDta$grade<-cutDta$gradeK+cutDta$gradeElem*2+cutDta$gradeMid*3+cutDta$gradeHigh*4
cutDta<-subset(cutDta,cutDta$grade>0)

cutDta$metro<-cutDta$rural+cutDta$suburban*2+cutDta$urban*3
cutDta<-subset(cutDta,cutDta$metro>0)

exactModel<-matchit(as.integer(male) ~ gradeK + gradeElem + gradeMid +special + arts + language + socsci + sports + applied
    +altSchool + rural + suburban, method = "exact",data=cutDta)
mDta<-match.data(exactModel)
cols<-c(1,length(mDta))
mDta<-setNames(mDta[cols],c('projectid','sexMatch'))
pDta<-merge(exDta,mDta,all.x=TRUE)
summary(pDta)

exactModel<-matchit(as.integer(gradeElem) ~ male+special + arts + language + socsci + sports + applied
    +altSchool + rural + suburban, method = "exact",data=cutDta)
mDta<-match.data(exactModel)
cols<-c(1,length(mDta))
mDta<-setNames(mDta[cols],c('projectid','gradeElemMatch'))
pDta<-merge(pDta,mDta,all.x=TRUE)
summary(pDta)

exactModel<-matchit(as.integer(gradeK) ~ male+special + arts + language + socsci + sports + applied
    +altSchool + rural + suburban, method = "exact",data=cutDta)
mDta<-match.data(exactModel)
cols<-c(1,length(mDta))
mDta<-setNames(mDta[cols],c('projectid','gradeKMatch'))
pDta<-merge(pDta,mDta,all.x=TRUE)
summary(pDta)

exactModel<-matchit(as.integer(gradeMid) ~ male+special + arts + language + socsci + sports + applied
    +altSchool + rural + suburban, method = "exact",data=cutDta)
mDta<-match.data(exactModel)
cols<-c(1,length(mDta))
mDta<-setNames(mDta[cols],c('projectid','gradeMidMatch'))
pDta<-merge(pDta,mDta,all.x=TRUE)

exactModel<-matchit(as.integer(gradeHigh) ~ male+special + arts + language + socsci + sports + applied
    +altSchool + rural + suburban, method = "exact",data=cutDta)
mDta<-match.data(exactModel)
cols<-c(1,length(mDta))
mDta<-setNames(mDta[cols],c('projectid','gradeHighMatch'))
pDta<-merge(pDta,mDta,all.x=TRUE)

exactModel<-matchit(as.integer(altSchool) ~ male+gradeK + gradeElem + gradeMid +special + arts + language + socsci + sports + applied
     + rural + suburban, method = "exact",data=cutDta)
mDta<-match.data(exactModel)
cols<-c(1,length(mDta))
mDta<-setNames(mDta[cols],c('projectid','altSchoolMatch'))
pDta<-merge(pDta,mDta,all.x=TRUE)

exactModel<-matchit(as.integer(urban) ~ male+gradeK + gradeElem + gradeMid +special + arts + language + socsci + sports + applied
    +altSchool, method = "exact",data=cutDta)
mDta<-match.data(exactModel)
cols<-c(1,length(mDta))
mDta<-setNames(mDta[cols],c('projectid','urbanMatch'))
pDta<-merge(pDta,mDta,all.x=TRUE)
summary(pDta)

exactModel<-matchit(as.integer(suburban) ~ male+gradeK + gradeElem + gradeMid +special + arts + language + socsci + sports + applied
    +altSchool, method = "exact",data=cutDta)
mDta<-match.data(exactModel)
cols<-c(1,length(mDta))
mDta<-setNames(mDta[cols],c('projectid','suburbanMatch'))
pDta<-merge(pDta,mDta,all.x=TRUE)

exactModel<-matchit(as.integer(rural) ~ male+gradeK + gradeElem + gradeMid +special + arts + language + socsci + sports + applied
    +altSchool, method = "exact",data=cutDta)
mDta<-match.data(exactModel)
cols<-c(1,length(mDta))
mDta<-setNames(mDta[cols],c('projectid','ruralMatch'))
pDta<-merge(pDta,mDta,all.x=TRUE)

exactModel<-matchit(as.integer(special) ~ male+gradeK + gradeElem + gradeMid +STEM + arts + language + socsci + sports + applied
    +altSchool+rural + suburban, method = "exact",data=cutDta)
mDta<-match.data(exactModel)
cols<-c(1,length(mDta))
mDta<-setNames(mDta[cols],c('projectid','specialMatch'))
pDta<-merge(pDta,mDta,all.x=TRUE)
summary(pDta)  #these summary() commands are to check and make sure data is being carried over

exactModel<-matchit(as.integer(STEM) ~ male+gradeK + gradeElem + gradeMid +special+ arts + language + socsci + sports + applied
    +altSchool+rural + suburban, method = "exact",data=cutDta)
mDta<-match.data(exactModel)
cols<-c(1,length(mDta))
mDta<-setNames(mDta[cols],c('projectid','stemMatch'))
pDta<-merge(pDta,mDta,all.x=TRUE)

exactModel<-matchit(as.integer(arts) ~ male+gradeK + gradeElem + gradeMid +STEM +special  + language + socsci + sports + applied
    +altSchool+rural + suburban, method = "exact",data=cutDta)
mDta<-match.data(exactModel)
cols<-c(1,length(mDta))
mDta<-setNames(mDta[cols],c('projectid','artsMatch'))
pDta<-merge(pDta,mDta,all.x=TRUE)

exactModel<-matchit(as.integer(language) ~ male+gradeK + gradeElem + gradeMid +STEM + arts +special  + socsci + sports + applied
    +altSchool+rural + suburban, method = "exact",data=cutDta)
mDta<-match.data(exactModel)
cols<-c(1,length(mDta))
mDta<-setNames(mDta[cols],c('projectid','languageMatch'))
pDta<-merge(pDta,mDta,all.x=TRUE)

exactModel<-matchit(as.integer(socsci) ~ male+gradeK + gradeElem + gradeMid +STEM + arts + language + special + sports + applied
    +altSchool+rural + suburban, method = "exact",data=cutDta)
mDta<-match.data(exactModel)
cols<-c(1,length(mDta))
mDta<-setNames(mDta[cols],c('projectid','socsciMatch'))
pDta<-merge(pDta,mDta,all.x=TRUE)
summary(pDta)

exactModel<-matchit(as.integer(sports) ~ male+gradeK + gradeElem + gradeMid +STEM + arts + language + socsci +special  + applied
    +altSchool+rural + suburban, method = "exact",data=cutDta)
mDta<-match.data(exactModel)
cols<-c(1,length(mDta))
mDta<-setNames(mDta[cols],c('projectid','sportsMatch'))
pDta<-merge(pDta,mDta,all.x=TRUE)

exactModel<-matchit(as.integer(applied) ~ male+gradeK + gradeElem + gradeMid +STEM + arts + language + socsci + sports + special
    +altSchool+rural + suburban, method = "exact",data=cutDta)
mDta<-match.data(exactModel)
cols<-c(1,length(mDta))
mDta<-setNames(mDta[cols],c('projectid','appliedMatch'))
pDta<-merge(pDta,mDta,all.x=TRUE)

col<-c(1,7:9,16,17,22:28,39:42)
rawDta<-allDta[col]
rawDta<-merge(rawDta,pDta,,all.x=TRUE)
#cut out the missing projectIDs during final merge

write.csv(rawDta, file="ExactAllCatTeacherStrata.csv", col.names = TRUE, sep = ",", qmethod = "double")

##### PROPENSITY SUBCLASSIFICATION #########

propModel<-matchit(as.integer(male) ~ gradeK + gradeElem + gradeMid +special + arts + language + socsci + sports + applied+
    +altSchool + rural + suburban, method = "subclass",data=cutDta, subclass=seq(0.1,1,.1))

propModel<-matchit(as.integer(male) ~ gradeHigh +special + arts + language + socsci + sports + applied+
    +altSchool*urban, method = "subclass",data=cutDta, subclass=seq(0.1,1,.1))
# The default subclassification metric is the logit propensity score
propDta<-match.data(propModel)
#unbalanced on metrotype, language, socsci, gradeK,altSchool
GenSexModel<-glm(as.integer(altSchool) ~ male+as.factor(subclass)+as.factor(male*subclass),family=binomial,data=propDta)
sp<-subset(propDta,propDta$subclass==3)
GenSexModel<-glm(as.integer(altSchool) ~ male,family=binomial,data=sp)
GenSexModel<-glm(as.integer(male) ~ resbooks + restech + resother+ log(reach) + log(price2)+poverty+
    +as.factor(subclass),family=binomial,data=propDta)

col<-c(1,8,29,38)
#cDta<-propDta[col]

#write.csv(cDta, file="PropTeacherStrata.csv", col.names = TRUE, sep = ",", qmethod = "double")

#####  K-NEAREST NEIGHBORS #########


###knn estimation takes a long time
knnModel<-matchit(as.integer(male) ~ gradeK + gradeElem + gradeMid +special + arts + language + socsci + sports + applied+
    +AltSchool + rural + suburban, method = "nearest",replace=TRUE, data=cutDta,m.order="largest")
knnDta<-match.data(knnModel)
length(knnModel$match.matrix)
summary(as.numeric(table(knnModel$match.matrix)))


knnModelNoRep<-matchit(as.integer(male) ~ gradeK + gradeElem + gradeMid +special + arts + language + socsci + sports + applied+
    +AltSchool + rural + suburban, method = "nearest",replace=FALSE, data=cutDta,m.order="largest")
knnDtaNoRep<-match.data(knnModelNoRep)
length(knnModelNoRep$match.matrix)
summary(as.numeric(table(knnModelNoRep$match.matrix)))


