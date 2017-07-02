
#R-CODE presumes a directory structure of:
#DonorsChoose
#   - Data: containing all .csv files
#   - Results: empty
#Note that figures are not saved to files. Edit code to save them.

#Script houses rolling window functions used to produce a number of figures
source('DonorsChoose/Stratification/WindowFunctions.R')

ImportDCFundData<-function(){
    #function used for importing the donation data to be used for nearly all of the functions below
    setwd('DonorsChoose/Data')
    
    
    #GENERAL DATA BROKEN OUT BY YEAR
    dta3 <- read.table('DataRerun2004Original2.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"')
    dta4 <- read.table('DataRerun2005Original2.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"')
    dta5 <- read.table('DataRerun2006Original2.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"') 
    dta6 <- read.table('DataRerun2007Original2.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"')
    dta7 <- read.table('DataRerun2008Original2.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"')
    dta8 <- read.table('DataRerun2009Original2.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"')
    dta9 <- read.table('DataRerun2010Original2.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"')
    allDta<-rbind(dta3,dta4,dta5,dta6,dta7,dta8,dta9)
    print(names(allDta))
    
    
    #clean out 2,283 extreme cases. E.g. someone asked for 10.2 million dollars, 17 people had empty essays.
    print("Number of Cases before exclusions: ")
    print(length(allDta$male))
    allDta<-subset(allDta,!(allDta$TotTime > 110 | allDta$Totlength==0 | allDta$reach>4000 |allDta$reach==0 |allDta$price > 40000|allDta$price==0| is.na(allDta$poverty)))    
    print("Number of Cases after exclusions: ")
    print(length(allDta$male))
    
    #Flag teachers reporting no sex
    allDta$noSex<-0
    allDta$noSex[is.na(allDta$male)]<-1
    
    #Compute the "Occupational Score" 
    PropModel<-glm(as.integer(male) ~  gradeK + gradeElem + gradeMid +special + arts + language + socsci + sports + applied+altSchool + rural + suburban,family = binomial,data=allDta,na.action=na.exclude)
    allDta$preds<-predict(PropModel, type='response')
    print(PropModel)
    allDta$sMale<-scale(allDta$male)
    allDta$sLogPreds<-scale(log(allDta$preds))
    
    #IMPORT the text classification sores ("Language Scores")
    #merge with core data and recode classification scores into a log-likelihood measure of Gender
    #NOTE: NFold...csv is the 10-fold cross-validation results mentioned, but not reported in the paper.
    #gDta <- read.table('NFoldValidationResults.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"')
    gDta <- read.table('InterMatchSexClassResults.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"')
    nbdta<-subset(gDta,gDta$classifier=='nb')
    cols<-c(1,3,6)
    nbdta<-setNames(nbdta[cols],c('projectid','nbTimesTested','nbZLkRatio'))
    allDta<-merge(allDta,nbdta, by=c('projectid'),all.x=TRUE)
    allDta$nbZLkRatio<-allDta$nbZLkRatio*-1 #adjusting the directionality for consistency
    svmdta<-subset(gDta,gDta$classifier=='svm')
    cols<-c(1,3,6)
    svmdta<-setNames(svmdta[cols],c('projectid','svmTimesTested','svmZLkRatio'))
    allDta<-merge(allDta,svmdta, by=c('projectid'),all.x=TRUE)
    allDta$svmZLkRatio<-allDta$svmZLkRatio*-1 #adjusting the directionality for consistency
    
    #IMPORT data on who was on the website pre/post 2007
    gDta <- read.table('prePostData.csv', sep=' ' , header=TRUE , as.is=TRUE , quote='"')
    print(names(gDta))
    allDta<-merge(allDta,gDta,by=c('projectid'),all.x=TRUE)
    
    
    #IMPORT data for repeat donors
    gDta <- read.table('RepeatDonors.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"')
    print(names(gDta))
    allDta<-merge(allDta,gDta,by=c('projectid'),all.x=TRUE)
    
    
    ####IMPORT AND RECODE variables for in-state and within-zip code donations; note in-state results not reported, but fit zip-code results
    gDta <- read.table('LocalDonors.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"')
    print(names(gDta))
    allDta<-merge(allDta,gDta,by=c('projectid'),all.x=TRUE)
    allDta$inState<-NA
    allDta$inState[allDta$numInState>0]<-1
    allDta$inState[allDta$numInState==0&allDta$numMissingState!=allDta$numDonations&allDta$numDonations!=0]<-0
    #table(allDta$inState)  NOTE: Excludes those with no donations
    allDta$inZip<-NA
    allDta$inZip[allDta$numInZip>0]<-1
    allDta$inZip[(allDta$numInZip==0 & allDta$numMissingZip<allDta$numDonations) & allDta$numDonations!=0]<-0
    #table(allDta$inZip)    NOTE: Excludes those with no donations. Adding this to the model eliminates a lot of effects b/c of the exclusion.
    
    ####IMPORT EXACT GROUP VARIABLE - from exact matching analysis
    gDta<-read.table('ExactTeacherStrata.csv',sep=',',header=TRUE,as.is=TRUE,quote='"')
    cols<-c(1,3)
    nbdta<-setNames(gDta[cols],c('projectid','group'))
    allDta<-merge(allDta,nbdta, by=c('projectid'),all.x=TRUE)
    
    #Perform variable recoding for tracking evolution of donors giving large amounts (>$100) and donations through giving pages and gift cards.
    allDta$BigDonors<-0
    allDta$BigDonors[allDta$PercentHigh*allDta$NumDonors>0]<-1
    allDta$Gifts<-0
    allDta$Gifts[allDta$PercentGiftCards*allDta$NumDonors>0]<-1
    allDta$GivePage<-0
    allDta$GivePage[allDta$perGivePage*allDta$NumDonors>0]<-1

    return(allDta)
}

allDta<-ImportDCFundData()


####### CRONBACH'S ALPHA TESTING
library(psych
cols<-c('sMale','nbZLkRatio','sLogPreds')
alpha(allDta[,cols]))


####### FUNCTIONS FOR ALL OF THE FIGURES

#FIGURE 1: CHANGE IN MALE PARTICIPATION 2005-2010
maleProb<-function(dta){
    return(mean(dta$male,na.rm=T))
}
maleCount<-function(dta){
    return(sum(dta$male,na.rm=T))
}
prob<-moveFunction(allDta,maleProb,2005,2010,3)
cnt<-moveFunction(allDta,maleCount,2005,2010,3)
prob$prob<-prob$results
prob$cnt<-cnt$results
rows<-seq(1,nrow(prob),2)
dates<-prob$date[rows]
par(mar=c(5,5,5,5))
plot(seq(1,nrow(prob),1),prob$prob,main="Change in Male Participation 2005-2010",xlab='Date',ylab="Percent of New Projects by Males",
    axes=F,type='l',lty=1)
axis(1,rows,dates)
#axis(3)
axis(2, seq(.08,.2,.02),seq(8,20,2), lty=1)
par(new=T)
plot(seq(1,nrow(prob),1),prob$cnt,xlab='Date',ylab='',axes=F,type='l',lty=2)
axis(4, pretty(c(0, 1.1*max(prob$cnt))), lty=2)
mtext('Number of New Male Projects',4,padj=3.5)
axis(3,labels=F,tick=F)


#FUNCTION FOR FIGURES IN THE METHODS SECTION (OCCUPATIONAL GENDER) AND UNPROVIDED FIGURE FOR RESULTS SECTION (LINEARITY OF SEX-LANGUAGE)
ExpObsPreds<-function(dta,resp,expl,grad,plotParams){
    x<-numeric()
    means<-numeric()
    cis<-numeric()
    mn<-min(dta[expl],na.rm=T)
    mx<-max(dta[expl],na.rm=T)
    #rng<-min(c(abs(init-mn),abs(init-mx)))
    vals<-seq(mn,mx, (mx-mn)*grad)       #rng takes the shorter distance between mean and upper and lower bound as end points for gradient
    for (i in seq(1,length(vals),1)){
        print(c(i,vals[i]))
        if (i>4 & i < length(vals)-4){         #due to range specification, the 4 edge points represent low-N cases
            d<-subset(dta,(dta[expl]>vals[i-1]&dta[expl]<vals[i+1]))  
            print(mean(d[[expl]],na.rm=T))
            x<-c(x,mean(d[[expl]],na.rm=T))
            mn<-mean(d[[resp]],na.rm=T)
            means<-c(means,mn)
            num<-length(d[[resp]])
            sds<-sd(d[[resp]],na.rm=T)
            ci<-1.95*(sds/sqrt(num))
            cis<-c(cis,ci)
            #male<-c(male,mean(d[[resp]],na.rm=T))
            print(length(d[[expl]]))
        }
    }
    pars<-c(list(x=x,y=means))
    do.call(plot,modifyList(pars,plotParams))
    for (i in seq(1,length(means),1)){
        segments(x[i],means[i]+cis[i],x1=x[i],y1=means[i]-cis[i],pch='|')
        #points(c(1,1),c(bike$conf.int[1],bike$conf.int[2]),pch='-',cex=2)
    }
    return(c(x,means,cis))
}

#FIGURE 2: PREDICTED AND OBSERVED OCCUPATIONAL SEGREGATION
resp<-'male'
expl<-'preds'
grad<-1/40
plotParams<-c(list(main="Predicted and Observed Occupational Segregation"),list(ylab="Observed Probability of Male Projects"),
    list(xlab="Predicted Probability of Male Projects \n (Occupational Score)"),list(xlim=c(0,.6)),list(ylim=c(0,.6)))
b<-ExpObsPreds(allDta,resp,expl,grad,plotParams)
abline(0,1)

#FIGURE 3A: CLASSIFIER ACCURACY BY SAMPLE SIZE
ClassAcc<-function(){
    #data was pasted from the output of [CORE CLASSIFIER PROPER NAME].
    train<-c(100, 100, 500, 100, 500, 100, 100, 100, 500, 1500, 500, 100, 500, 100, 100, 1000, 100, 1000, 1500, 1000, 500, 500, 1500, 1000, 1000, 2000, 500, 500, 500, 1500, 1500, 1000, 1000, 2000, 2000, 1000, 1500, 1000, 2000, 1500, 2000, 1500, 1000, 1500, 3000, 1500, 2000, 2000, 2000, 2000, 2000, 3000, 3000, 3000, 5000, 3000, 5000, 3000, 3000, 3000, 3000, 3000, 5000, 5000, 5000, 5000, 5000, 5000, 5000, 5000)
    nbacc<-c(0.5, 0.5, 0.5, 0.5, 0.51, 0.51, 0.51, 0.52, 0.54, 0.54, 0.54, 0.54, 0.54, 0.54, 0.54, 0.55, 0.55, 0.56, 0.56, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.58, 0.58, 0.58, 0.59, 0.59, 0.59, 0.59, 0.59, 0.59, 0.59, 0.59, 0.59, 0.59, 0.59, 0.59, 0.6, 0.6, 0.6, 0.6, 0.6, 0.61, 0.61, 0.61, 0.61, 0.61, 0.61, 0.61, 0.61, 0.61, 0.61, 0.61, 0.62, 0.62, 0.62, 0.62, 0.62, 0.62, 0.62, 0.63) 
    svmacc<-c(0.51, 0.52, 0.52, 0.53, 0.53, 0.53, 0.53, 0.54, 0.54, 0.54, 0.55, 0.55, 0.56, 0.56, 0.56, 0.56, 0.56, 0.56, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.58, 0.58, 0.58, 0.58, 0.58, 0.58, 0.58, 0.58, 0.58, 0.58, 0.58, 0.59, 0.59, 0.59, 0.59, 0.59, 0.59, 0.59, 0.59, 0.59, 0.59, 0.59, 0.59, 0.59, 0.59, 0.59, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.61, 0.61)
    plot(jitter(train),nbacc,type='n',main="Accuracy by Training Sample Size",ylab="Classification Accuracy",xlab="Number of Essays in Training Sample")
    points(jitter(train),svmacc,type='n')
    text(jitter(train),nbacc, labels="o")
    text(jitter(train),svmacc, labels="x")
    text(c(4000,4000),c(.56,.55),c('x - SVM','o - NB'))
    }
    
ClassAcc()

#FIGURE 3B: CLASSIFIER ACCURACY BY THRESHOLD 
#The first two functions are the same except one prints SVM and the other Naive Bayes
#Afterwards, each function is run separately with the appropriately specified inputs
ExtremesAccuracyNBGradient<-function(dta,resp,expl,init,grad){
    #init = center point from which extremes are defined
    #grad = gradient of point between init and nearest extreme to measure changing effect
    #dta[expl]<-scale(dta[expl])
    mn<-min(dta[expl],na.rm=T)
    mx<-max(dta[expl],na.rm=T)
    rng<-min(c(abs(init-mn),abs(init-mx)))
    vals<-seq(init,init+rng,rng*grad)       #rng takes the shorter distance between mean and upper and lower bound as end points for gradient
    acc<-numeric()
    for (v in vals){
        print(v)
        dta$Guess<-NA
        dta$Guess[dta[expl]< init-v]<-0
        dta$Guess[dta[expl]>= init+v]<-1
        correct<-sum(dta[resp]==dta['Guess'],na.rm=T)
        tot<-length(dta$Guess[is.na(dta$Guess)!=T])
        ac<-correct/tot
        print(tot)
        acc<-c(acc,ac)
    }
    plot(vals,acc,type='n', main="Accuracy by Classification Threshold",ylab="Classification Accuracy",xlab="Threshold (in Std. Dev.)")
    text(vals,acc, labels="nb")
    return(c(vals,acc))
    
}

ExtremesAccuracySVMGradient<-function(dta,resp,expl,init,grad){
    #init = center point from which extremes are defined
    #grad = gradient of point between init and nearest extreme to measure changing effect
    #dta[expl]<-scale(dta[expl])
    mn<-min(dta[expl],na.rm=T)
    mx<-max(dta[expl],na.rm=T)
    rng<-min(c(abs(init-mn),abs(init-mx)))
    vals<-seq(init,init+rng,rng*grad)       #rng takes the shorter distance between mean and upper and lower bound as end points for gradient
    acc<-numeric()
    for (v in vals){
        print(v)
        dta$Guess<-NA
        dta$Guess[dta[expl]< init-v]<-0
        dta$Guess[dta[expl]>= init+v]<-1
        correct<-sum(dta[resp]==dta['Guess'],na.rm=T)
        tot<-length(dta$Guess[is.na(dta$Guess)!=T])
        ac<-correct/tot
        print(tot)
        acc<-c(acc,ac)
    }
    points(vals,acc,type='n')
    text(vals,acc, labels="svm")
    return(c(vals,acc))
}


resp<-'male'
expl<-'nbZLkRatio'
grad<-.05
init<-mean(allDta[[expl]],na.rm=T)
b<-ExtremesAccuracyNBGradient(allDta,resp,expl,init,grad)

expl<-'svmZLkRatio'
init<-mean(allDta[[expl]],na.rm=T)
b<-ExtremesAccuracySVMGradient(allDta,resp,expl,init,grad) 


#FIGURE 4
CoVariantGradient<-function(dta,resp,expl,grad,plotParams){
    ### Uncomment the code below to generate a plot with confident intervals.
    x<-numeric()
    male<-numeric()
    female<-numeric()
    dist<-numeric()
    dx<-numeric()
    #malecis<-numeric()
    #femalecis<-numeric()
    mn<-min(dta[expl],na.rm=T)
    mx<-max(dta[expl],na.rm=T)
    vals<-seq(mn,mx, (mx-mn)*grad)       
    for (i in seq(1,length(vals),1)){
        print(c(i,vals[i]))
        if (i!=1 & i != length(vals)){
            d<-subset(dta,(dta[expl]>vals[i-1]&dta[expl]<vals[i+1]))
            print(mean(d[[expl]],na.rm=T))
            x<-c(x,mean(d[[expl]],na.rm=T))
            male<-c(male,mean(d[[resp]][d$male==1],na.rm=T))
            #num<-length(d[[resp]][d$male==1])
            #sds<-sd(d[[resp]][d$male==1],na.rm=T)
            #ci<-1.95*(sds/sqrt(num))
            #malecis<-c(malecis,ci)
            female<-c(female,mean(d[[resp]][d$male==0],na.rm=T))
            #num<-length(d[[resp]][d$male==0])
            #sds<-sd(d[[resp]][d$male==0],na.rm=T)
            #ci<-1.95*(sds/sqrt(num))
            #femalecis<-c(femalecis,ci)
        }
    }
    pars<-c(list(x=x,y=male,pch='m'))
    do.call(plot,modifyList(pars,plotParams))
    #for (i in seq(1,length(x),1)){
    #    segments(x[i],male[i]+malecis[i],x1=x[i],y1=male[i]-malecis[i],pch='|')
    #    }
    pars<-c(list(x=x,y=female,pch='f'))
    do.call(points,modifyList(pars,plotParams))
    #for (i in seq(1,length(x),1)){
    #    segments(x[i],female[i]+femalecis[i],x1=x[i],y1=female[i]-femalecis[i],pch='|')
    #    }
    return(c(x,male,female))
}

#FIGURE 4a "Sex-Text.png"
#note the ExpObsPreds() function is defined much earlier in this code
resp<-'male'
expl<-'nbZLkRatio'
grad<-1/40
plotParams<-c(list(main="Sex and Language Score"),list(ylab="Probability of Male Projects"),
    list(xlab="Language Score"),list(xlim=c(-2.6,3.3)),list(ylim=c(0,1)))
png(filename="D:/Users/Administrator/Documents/Chicago/Research/Gender on DonorsChoose/Publication/Images/New Sex-Text.png")
b<-ExpObsPreds(allDta,resp,expl,grad,plotParams)
dev.off()

#model for curvilinearity of sex and text
allDta$sqLkRatio<-allDta$nbZLkRatio**2
allDta$cbLkRatio<-allDta$nbZLkRatio**3
allDta$quLkRatio<-allDta$nbZLkRatio**4
m<-glm(male~nbZLkRatio+sqLkRatio+cbLkRatio,allDta,family=binomial)
summary(m)

#Figure 4b "Text-Occupation.png"
resp<-'nbZLkRatio'
expl<-'preds'
grad<-1/40
plotParams<-c(list(main="Language and Occupation Score"),list(ylab="Language Score"),
    list(xlab="Occupation Score"),list(xlim=c(0.0,0.6)),list(ylim=c(-.4,.5)))
png(filename="D:/Users/Administrator/Documents/Chicago/Research/Gender on DonorsChoose/Publication/Images/New Text-Occupation.png")
b<-ExpObsPreds(allDta,resp,expl,grad,plotParams)
dev.off()

#Estimating change in slope before and after .25 
m<-lm(scale(nbZLkRatio)~preds*(preds<.25),allDta)
summary(m)

# FIGURE 4c,  'Three-way.png'
resp<-'nbZLkRatio'
expl<-'preds'
grad<-1/40
plotParams<-c(list(main="Occupation, Language, and Sex"),list(ylab="Language Score"),
    list(xlab="Occupation Score"),list(xlim=c(0,.6)),list(ylim=c(-1,1)))
png(filename="D:/Users/Administrator/Documents/Chicago/Research/Gender on DonorsChoose/Publication/Images/New Three-way2.png")
b<-CoVariantGradient(allDta,resp,expl,grad,plotParams)
dev.off()

#model for changing signifcance of male and occupation on language usage. Referenced, but unreported.
m<-lm(scale(nbZLkRatio)~scale(male)*scale(log(preds)),subset(allDta,allDta$preds<.25))  
m<-lm(scale(nbZLkRatio)~scale(male)*scale(log(preds)),subset(allDta,allDta$preds>.25))
summary(m)



# FIGURE 5: The Changing Rate of Success and Large Donor Funding 2005-2010
FundProb<-function(dta){
    return(mean(dta$Funded,na.rm=T))
}
HighProb<-function(dta){
    return(mean(dta$PercentHigh*dta$TotalDonations>0,na.rm=T))
}

#the following three functions are those "not shown" in the paper but reported with the discussion of Figure 7.
NumDonations<-function(dta){         
    return(sum(dta$TotalDonations,na.rm=T)/length(dta$male))
}

AverageRequested<-function(dta){
    return(mean(dta$price2,na.rm=T))
}

AverageNoDonations<-function(dta){
    return(mean(dta$TotalDonations==0))
}

fprob<-moveFunction(allDta,FundProb,2005,2010,3)
hprob<-moveFunction(allDta,HighProb,2005,2010,3)
fprob$fprob<-fprob$results
fprob$hprob<-hprob$results
rows<-seq(1,nrow(fprob),2)
dates<-fprob$date[rows]

png(filename="D:/Users/Administrator/Documents/Chicago/Research/Gender on DonorsChoose/Publication/Images/New Change in Success.png")
par(mar=c(5,5,5,5))
plot(seq(1,nrow(fprob),1),fprob$fprob,main="Change in Success 2005-2010",xlab='Date',ylab="Percent of Projects",
    type='l',lty=1,xaxt='n',ylim=c(0,1))
points(seq(1,nrow(fprob),1),fprob$fprob,pch=1)
axis(1,rows,dates)
lines(seq(1,nrow(fprob),1),fprob$hprob,type='l',lty=2)
points(seq(1,nrow(fprob),1),fprob$hprob,pch='+')
text(c(55,55),c(.40,.33),c('+ Pr(>$100 Donation)','o Pr(Funded)'),cex=.8)
dev.off()



#######CODE TO REPRODUCE ALL OF THE TABLES IN THE ANALYSIS
##DESCRIPTIVES TABLES: TABLE ? AND TABLE ?

#TABLE 1
#Note, this excludes cases that are included during the initial run.  Thus the exact numbers here should vary slightly, but not much
#(see the PredModel in ImportDCFundData function to reproduce the reported results)

PropModel<-glm(as.integer(male) ~  gradeK + gradeElem + gradeMid +special + arts + language + socsci + sports + applied+altSchool + rural + suburban,family = binomial,data=allDta,na.action=na.exclude)
summary(PropModel)

#TABLE 2
WordsWork<-function(file,savefile){
    words<-read.table(file, sep=',' , header=TRUE , as.is=TRUE , quote='"')
    ag<-setNames(aggregate(words$male,by=list(words$word),sum),c('word','maleCount'))
    a<-aggregate(words$male,by=list(words$word),length)
    ag$num<-a$x
    
    
    #Following code adjust count for shannon count
    ag<-subset(ag,ag$num>5) #smooth shannon count
    ag$adjNum<-ag$num+2
    ag$adjMCount<-ag$maleCount+1
    ag$p<-ag$adjMCount/ag$adjNum
    ag$info<- -ag$p*log2(ag$p)-((1-ag$p)*log2(1-ag$p))
    ag<-ag[order(ag$info),]
    write.table(file=savefile,x=ag)
    return(ag)
}
f='DonorsChoose/Results/FinalTopWords.csv'
savefile='DonorsChoose/Results/cleanFinalTopWords.csv'
ag<-WordsWork(f,savefile)



occupationVars<-c("male","preds","gradeK","gradeElem","gradeMid","gradeHigh","applied","arts","language","socsci","special","sports","STEM","altSchool","rural","suburban","urban")
occDat<-allDta[,occupationVars]

#First exclude cases without sex b/c they cannot have an occupational value
occDat<-subset(occDat,is.na(occDat$male)==F)

makeOccupationTable <- do.call(data.frame, 
           list(mean = apply(occDat, 2, mean),
                sd = apply(occDat, 2, sd),
                min = apply(occDat, 2, min),
                max = apply(occDat, 2, max),
                n = apply(occDat, 2, length)))


#TABLE 3
#Note, this excludes cases that are included during the initial run.  Thus the exact numbers here should vary slightly, but not much
#(see the PredModel in ImportDCFundData function to reproduce the reported results)
PropModel<-glm(as.integer(male) ~  gradeK + gradeElem + gradeMid +special + arts + language + socsci + sports + applied+altSchool + rural + suburban,family = binomial,data=allDta,na.action=na.exclude)
summary(PropModel)

#TABLE 4
#The original FinalTopWords.csv is produced from the Classificaiton python script [FIND PROPER NAME]
WordsWork<-function(file,savefile){
    words<-read.table(file, sep=',' , header=TRUE , as.is=TRUE , quote='"')
    ag<-setNames(aggregate(words$male,by=list(words$word),sum),c('word','maleCount'))
    a<-aggregate(words$male,by=list(words$word),length)
    ag$num<-a$x
    #Following code adjust count for shannon entropy (i.e. most words are 100% male or female)
    ag<-subset(ag,ag$num>5) #smooth shannon count
    ag$adjNum<-ag$num+2
    ag$adjMCount<-ag$maleCount+1
    ag$p<-ag$adjMCount/ag$adjNum
    ag$info<- -ag$p*log2(ag$p)-((1-ag$p)*log2(1-ag$p))
    ag<-ag[order(ag$info),]
    write.table(file=savefile,x=ag)
    return(ag)
}


f='DonorsChoose/Results/FinalTopWords.csv'
savefile='DonorsChoose/Results/cleanFinalTopWords.csv'
ag<-WordsWork(f,savefile)

#Table 5 Descriptives for Standard Model
#
controlVars<-c("Funded","male","preds","nbZLkRatio","resbooks","resother","ressupplies","restech","restrips","resvis","reach","price2","poverty","future","Totlength","lix")
conDat<-allDta[,controlVars]

#First exclude cases with missing data. Really, it's just those who never got an language estimate.
sconDat<-subset(conDat,(is.na(conDat$male)==F & is.na(conDat$preds)==F & is.na(conDat$nbZLkRatio)==F))
#sconDat$male<-scale(sconDat$male)
#sconDat$preds<-scale(log(sconDat$preds))
makeControlTable <- do.call(data.frame, 
           list(mean = apply(sconDat, 2, mean),
                sd = apply(sconDat, 2, sd),
                min = apply(sconDat, 2, min),
                max = apply(sconDat, 2, max),
                n = apply(sconDat, 2, length)))


#Table 6 Comparing the Explanatory power of the three Dimensional Model
singleNull<-glm(Funded~scale(male),family = binomial,data=subset(allDta,is.na(allDta$nbZLkRatio)==F),na.action=na.exclude)
singleFull<-glm(Funded~scale(male)+ resvis+restrips+resbooks + restech + resother+ log(reach) + log(price2)+poverty+as.factor(MonthPosted),family = binomial,data=subset(allDta,is.na(allDta$nbZLkRatio)==F),na.action=na.exclude)
threeNull <- glm(Funded~scale(male)*scale(log(preds))*scale(nbZLkRatio),family = binomial,data=allDta,na.action=na.exclude)
threeFull<-glm(Funded~scale(male)*scale(log(preds))*scale(nbZLkRatio)+resvis+restrips+ resbooks + restech + resother+ log(reach) + log(price2)+poverty+as.factor(MonthPosted),family = binomial,data=allDta,na.action=na.exclude)

anova(singleFull,threeFull,test="Chisq")
#  Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
#1     56817      61885                          
#2     56811      60959  6   926.19 < 2.2e-16 ***

#test if interaction terms add power:
threeInd<-glm(Funded~scale(male)+scale(log(preds))+scale(nbZLkRatio)+resvis+restrips+ resbooks + restech + resother+ log(reach) + log(price2)+poverty+as.factor(MonthPosted),family = binomial,data=allDta,na.action=na.exclude)
anova(threeInd,threeFull,test="Chisq")
#  Resid. Df Resid. Dev Df Deviance Pr(>Chi)    
#1     56815      61006                         
#2     56811      60959  4   46.725 1.74e-09 ***

#Hypothesis 2C Barplot of coefficients - don't know if it'll make it into the final version of the paper
barplot(c(.069,.365,.088,.042,.002,.079,.002),ylim=c(0,.4), main='Figure ?? Size of Model Coefficients',ylab='Coefficient',las=2, names.arg=c("Male","Occ","Lang","Male*Occ","Male*Lang","Occ*Lang","All"))


#Table 6 2007, 2008, 2009
nullPreModel<-glm(as.integer(Funded) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio),family=binomial,data=subset(allDta,(allDta$Year==2007)))
nullDoseModel<-glm(as.integer(Funded) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio),family=binomial,data=subset(allDta,(allDta$Year==2008)))
nullLateModel<-glm(as.integer(Funded) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio),family=binomial,data=subset(allDta,(allDta$Year==2009)))

PreModel<-glm(as.integer(Funded) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+ restrips+resvis+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,allDta$Year==2007))
DoseModel<-glm(as.integer(Funded) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+ restrips+resvis+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,allDta$Year==2008))
LateModel<-glm(as.integer(Funded) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+restrips+resvis+ resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,allDta$Year==2009))


#Comparing Masculine and feminine portraits
#2007
(.049*2.65) + (.139*1) + (.151*1) + (-.007*2.65) + (-.003*2.65) + (-.009*1) + (.018*2.65) #= 0.432, exp(.432)=1.537.
#2008
(.080*2.65) + (.496*1) + (.037*1) + (-.066*2.65) + (-.009*2.65) + (.138*1) + (.010*2.65) #= .711, exp(.711)=2.03
#2009
(.072*2.65) + (.579*1) + (.139*1) + (-.047*2.65) + (.007*2.65) + (.123*1) + (-.024*2.65) #= .862, exp(.711)=2.37

#Do the pre-2008 coefficients become significant when we include earlier/more data?
VeryPreModel<-glm(as.integer(Funded) ~ scale(male)*scale(log(preds))*nbZLkRatio+ resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,allDta$Year<2008&allDta$Year>2004))


#ADDING IN THE STOCK MARKET DATA
stock <- read.table('C://DonorsChoose//Data//stockData2.csv', sep=',' , header=TRUE , as.is=TRUE , quote='"')
colnames(stock)<-c('DOW','Month','Year')
allDta<-merge(allDta, stock, by.x=c("MonthPosted", "Year"), by.y=c("Month", "Year"))


#Table 7: Models 5-6, Models for Same and New Teachers.

PreModelNew<-glm(as.integer(Funded) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,(allDta$Year==2007&(sameTeach==0|sameState==0))))
PreModelSame<-glm(as.integer(Funded) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,(allDta$Year==2007&sameTeach==1)))
#NOT PUBLISHED: PreModelBoth<-glm(as.integer(Funded) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,(allDta$Year==2007&sameTeach==1&sameState==1)))

PostModelNew<-glm(as.integer(Funded) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,((allDta$Year>2007&allDta$Year<2010)&(sameTeach==0|sameState==0))))
PostModelSame<-glm(as.integer(Funded) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,((allDta$Year>2007&allDta$Year<2010)&sameTeach==1)))
#NOT PUBLISHED: ModelBoth<-glm(as.integer(Funded) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,((allDta$Year>2007&allDta$Year<2010)&(sameTeach==1&allDta$sameState==1))))

#Table 8: Models 7-8, Network Controls for Same and New Teachers.
netNewPreModel<-glm(as.integer(Funded) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+log(numInZip+1)+log(numRepeatDonations+1)+ resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,(allDta$Year==2007&sameTeach==0)))
netSamePreModel<-glm(as.integer(Funded) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+log(numInZip+1)+log(numRepeatDonations+1)+ resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,(allDta$Year==2007&sameTeach==1)))

netNewPostModel<-glm(as.integer(Funded) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+log(numInZip+1)+log(numRepeatDonations+1)+ resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,(allDta$Year>2007&allDta$Year<2010&sameTeach==0)))
netSamePostModel<-glm(as.integer(Funded) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+log(numInZip+1)+log(numRepeatDonations+1)+ resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,(allDta$Year>2007&allDta$Year<2010&sameTeach==1)))

#Table 9: Models 9-11, Predicting Repeat Donations by gender
repPreModel<-glm(numRepeatDonations ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=poisson,data=subset(allDta,allDta$Year==2007))
repDoseModel<-glm(numRepeatDonations ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+ resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=poisson,data=subset(allDta,allDta$Year==2008))
repLateModel<-glm(numRepeatDonations ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)++ resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=poisson,data=subset(allDta,allDta$Year==2009))
repSameLateModel<-glm(numRepeatDonations ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)++ resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=poisson,data=subset(allDta,allDta$Year==2009&allDta$sameTeach==0))

#Table Unshown: Predicting in-zip donations by gender
#The process of offline donor recruitment is gendered, but the patterns are different.  
#PreModel<-glm(numInZip ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=poisson,data=subset(allDta,allDta$Year==2007))
#DoseModel<-glm(numInZip ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+ resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=poisson,data=subset(allDta,allDta$Year==2008))
#PostModel<-glm(numInZip ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)++ resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=poisson,data=subset(allDta,allDta$Year==2009))


#GIVE PAGE AND MATCHING GRANTS
PreModel<-glm(as.integer(givePage) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+ restrips+resvis+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,allDta$Year==2007))
DoseModel<-glm(as.integer(givePage) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+ restrips+resvis+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,allDta$Year==2008))
LateModel<-glm(as.integer(givePage) ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+restrips+resvis+ resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,allDta$Year==2009))






#################################
#Analyses that didn't make it in the paper
#################################


#####EXAMINING HETEROSKEDASTICITY BY COMPARING MODELS OF SUBSAMPLES OF THE DATA
#Relates to discussion on page ??? (wait for final publication)
ElemSchoolModel<-glm(as.integer(male) ~  special + arts + language + socsci + sports + applied+altSchool + rural + suburban,family = binomial,data=subset(allDta,(allDta$gradeK==1|allDta$gradeElem==1)),na.action=na.exclude)
HighSchoolModel<-glm(as.integer(male) ~  special + arts + language + socsci + sports + applied+altSchool + rural + suburban,family = binomial,data=subset(allDta,(allDta$gradeHigh==1)),na.action=na.exclude)
summary(HighSchoolModel)

AltSchoolModel<-glm(as.integer(male) ~  gradeK + gradeElem + gradeMid +special + arts + language + socsci + sports + applied+ rural + suburban,family = binomial,data=subset(allDta,(allDta$altSchool==1)),na.action=na.exclude)
NormalSchoolModel<-glm(as.integer(male) ~  gradeK + gradeElem + gradeMid +special + arts + language + socsci + sports + applied+ rural + suburban,family = binomial,data=subset(allDta,(allDta$altSchool==1)),na.action=na.exclude)
summary(AltSchoolModel)

RuralSchoolModel<-glm(as.integer(male) ~  gradeK + gradeElem + gradeMid +special + arts + language + socsci + sports + applied+altSchool ,family = binomial,data=subset(allDta,(allDta$rural==1)),na.action=na.exclude)
UrbanSchoolModel<-glm(as.integer(male) ~  gradeK + gradeElem + gradeMid +special + arts + language + socsci + sports + applied+altSchool ,family = binomial,data=subset(allDta,(allDta$urban==1)),na.action=na.exclude)
summary(UrbanSchoolModel)

ArtsModel<-glm(as.integer(male) ~  gradeK + gradeElem + gradeMid +altSchool + rural + suburban,family = binomial,data=subset(allDta,(allDta$arts==1)),na.action=na.exclude)
STEMModel<-glm(as.integer(male) ~  gradeK + gradeElem + gradeMid +altSchool + rural + suburban,family = binomial,data=subset(allDta,(allDta$stem==1)),na.action=na.exclude)
summary(ArtsModel)



######GLASS ESCALATOR: WHERE IS SEX*OCCUPATION NEGATIVE?
#Is the Negative Coefficient for Sex*Occupation about the glass escalator or does it
#apply to females in male-typed jobs as well?  This anlaysis plots the coefficient across different subgroups of
#the occupational score. 
#Results: Results show no real pattern. It's noisy, but generally negative across subgroups.
form<-as.formula('Funded~scale(male)*scale(log(preds))*scale(nbZLkRatio)+ resbooks + restech + resother+log(reach) + log(price2)+poverty+as.factor(MonthPosted)')
values<-rep(0,25)
values[length(values)-3]<-1 #c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
expl<-'preds'
grad<-1/15
plotParams=list(main='Male*Occupation Coefficient over Occupation',ylab='Standardized Coefficient',xlab='Occupation Score',ylim=c(-0.1,0.13),xlim=c(0,.6),type='l')
dat<-PlotFunctionGradient(allDta,form,values,expl,grad,plotParams)#
abline(0,0)

#TESTING PERFORMANCE THEORY: ARE MEN MORE ACCOUNTABLE TO MALE LANGUAGE
#Result: both males and females are rewarded for masculine language, though males may benefit slightly more. 
model<-glm(Funded ~ scale(log(preds))*scale(nbZLkRatio)+resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,allDta$male==0))
model<-glm(Funded ~ scale(log(preds))*scale(nbZLkRatio)+resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=subset(allDta,allDta$male==1))


#####UNUSED ANALYSIS: EXAMINING CHANGES IN DISCRIMINATION WITHIN 5 MONTH WINDOWS
#If we plot the coefficients for discrimination over time, we see the month-to-month changes in discrimination.
allDta$resOther<-0
allDta$resOther[allDta$resother==1|allDta$resvis==1|allDta$restrips==1]<-1
form<-as.formula('Funded~scale(male)*scale(log(preds))*scale(nbZLkRatio)+ resbooks + restech + resOther+ log(reach) + log(price2)+poverty+as.factor(MonthPosted)')
mascValues<-rep(0,18)   #have fewer values because only using 5 months of data at a time and so only 4 monthposted values.
mascValues[2:4]<-1
mascValues[(length(mascValues)-3):length(mascValues)]<-1 #c(0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
mdat<-moveRegressionCoefficients(allDta,2006,2010,5,form,mascValues)#
dat<-subset(mdat,select=c('V1','date'))
colnames(dat)<-c('masc','date')

femValues<-rep(0,18)
femValues[2:4]<- -1
femValues[(length(femValues)-3):length(femValues)]<- -1
fdat<-moveRegressionCoefficients(allDta,2006,2010,5,form,femValues)#
fdat<-subset(fdat,select=c('V1','date'))
colnames(fdat)<-c('fem','date')
dat$fem<-fdat$fem   #this only works because both are sorted by date and have the same dates.

plotParams=list(main='Direct Effects',ylab='Likelihood of Funding \n(compared to neutral teacher)',xlab='Date Posted\n(5 Month Window)',ylim=c(-1,1),type='l')
plotMover(dat,c('masc','fem'),c('Masculine Estimate','Feminine Estimate'), c(list(lty=1),list(lty=2)),plotParams,'plot')
dates<-c("1 2008","2 2008") #date for Sex Publication
plotDose(dat,dates,5,col='#ECE9E9AA')
text(23.5,-.2,'sex \n published')
dates<-c("12 2008","1 2009") #date for Sex in search is Dec 2008
plotDose(dat,dates,5,col='#ECE9E9AA')
text(34.5,-.2,'sex in \n search')

abline(0,0)




#####DO MATCHING GRANTS AFFECT DISCRIMINATION?
#Relates to discussion on page ??? (wait for publication)
#Summary: The goal is to estimate the degree of gender discrimination controlling for whether or not a project was elgible for a
#matching grant.  The problem is that adding a dummy variable for this in the general model of gender discrimination
#is incapable of controlling for selection effects.  Specifically, my analysis indicates that projects that were eligible for
#matching grants were more likely to be in more masculine locations (among other variables).  So, when Match is entered as
#a covariate in the base model, it takes on some of the effect attributable to gender.
#Results: When you enter Match in the model, the basic results hold, but coefficients and the significance level of some features change.
#This indicates to me that the effect is probably not qualitatively different.  But, too, the effect is not zero.
#I tried to create substrata balancing gender coefficients on the probability of receiving a matching grant. Unfortunately, it was
#impossible to balance the data.  Structural equation modelling

#PREDICTORS OF MATCHING GRANT ELIGIBILITY
#MatchModel is illustrative. Play with the model as you wish.
MatchModel<-glm(Match ~ scale(male)*scale(log(preds))*scale(nbZLkRatio)+resvis+restrips+resbooks + restech + resother+ log(reach)+log(price2)+poverty+as.factor(MonthPosted),family=binomial,data=allDta)
summary(MatchModel)

#PROPENSITY STRATA
#More illustrative code.  Play with the number of strata created (below at "probs=0:10/10") and examine balance on different variables of
#interst in the balance model. 
MatchModel<-glm(Match ~ scale(male)*scale(log(preds))*scale(nbZLkRatio),family=binomial,data=allDta,na.action=na.exclude)
allDta$MatchSelect<-predict(MatchModel,type='response')
allDta$MatchDeciles<- as.integer(cut(allDta$MatchSelect, quantile(allDta$MatchSelect, probs=0:10/10,na.rm=T), include.lowest=TRUE))
balance<-glm(scale(male)~Match*MatchDeciles,data=allDta,family='gaussian')
summary(balance)

#PLOT OF MATCHING GRANTS BY OCCUPATION Score
resp<-'Match'
expl<-'preds'
grad<-1/40
plotParams<-c(list(main="Probability of Being Match Eligible by Occupatoin Score"),list(ylab="Percent of Projects Eligible for Matching Grants "),
     list(xlab="Occupation Score"),list(xlim=c(0,.6)),list(ylim=c(0.18,0.6)))
b<-ExpObsPreds(allDta,resp,expl,grad,plotParams)

#PATH ANALYSIS FOR MATCH
#This is the model I eventually stopped on.  It properly models the path effects and the results are consistent with the overall
#results reported in the paper.
library(lavaan)
allDta$slogpreds<-scale(log(allDta$preds))
allDta$sMale<-scale(allDta$male)
allDta$sGender<-scale(allDta$nbZLkRatio)
allDta$mpreds<-scale(allDta$male)*scale(log(allDta$preds))
allDta$nbpreds<-scale(allDta$nbZLkRatio)*scale(log(allDta$preds))
allDta$nbmale<-scale(allDta$male)*scale(allDta$nbZLkRatio)
allDta$allway<-scale(allDta$nbZLkRatio)*scale(log(allDta$preds))*scale(allDta$nbZLkRatio)
allDta$lprice<-log(allDta$price2)
allDta$lreach<-log(allDta$reach)
allDta$months<-as.character(allDta$MonthPosted)
a<-as.data.frame(model.matrix( ~ months - 1, data=allDta))
colnames(a)<-c('jan','oct','nov','dec','feb','mar','apr','may','jun','jul','aug','sep')
sdta<-merge(allDta,a,by='row.names')

#A number of specifications are tried.  This is the direct plus selection path model
mod<-   'Funded~ sMale+slogpreds+sGender+mpreds+nbpreds+nbmale+allway+resvis+restrips+resbooks + restech + resother+ lreach+lprice+poverty+jan + oct + nov + dec + feb + mar + apr + may + jun + jul + aug
    Match~sMale+slogpreds+sGender+mpreds+nbpreds+nbmale+allway+resvis+restrips+resbooks + restech + resother+ lreach+lprice+poverty
    Funded~Match'
    
sdta<-subset(allDta,select=c('jan','oct','nov','dec','feb','mar','apr','may','jun','jul','aug','sep','Match','sMale','slogpreds','sGender','mpreds','nbpreds','nbmale','allway','Funded','resvis','restrips','resbooks','restech','resother','lreach','lprice','poverty','MonthPosted'))

fit<-sem(mod,data=subset(sdta,sdta$Year==2007))
summary(fit)
fit<-sem(mod,data=subset(sdta,sdta$Year==2008))
summary(fit)
fit<-sem(mod,data=subset(sdta,sdta$Year==2009))
summary(fit)


