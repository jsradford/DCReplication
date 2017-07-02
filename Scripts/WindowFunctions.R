functions<-function(){
    print("HERE IS A LIST OF AVAILABLE FUNCITONS IN WindowFunctions.R written for donors chooose analysis.")
    print('moveFunction: moveFunction runs a function (fun) on the data within a certain window, See formals(moveFunction) for parameters. For a list of premade functions see moveFunctions()')
    print('moveRegression runs a regression (defined by "form") and returns expected likelihood of regression given list of values. See formals(moveRegression) for parameters')
    print('plotDose: code plots a grey polygon covering dates specified in "dates" from moveFunction or moveRegresion where dates<-c("6 2009","10 2010").  See formals(plotDose) for parameters')
    print('plotMover: plots variables produced by the results of a moving window function. varPars and plotParams are custom plot parameters. See body(plotMover) for details')
}

moveFunctions<-function(){
    print('No premade functions yet')
}

moveFunction<-function(dta,fun,year1,yearLast,wSize){
    #moveFunction runs a function (fun) on the data within a certain window
    #fun is given subDta, so fun must be written to handle the subsetted data.frame
    #Returs the results of fun as a data.frame of outputs and dates (if fun returns data.frame, result is data.frame)
    years<-seq(year1,yearLast,1)
    months<-seq(range(dta$MonthPosted)[1],range(dta$MonthPosted)[2],1)
    wind<-c(1,wSize)
    it<-1
    y<-1
    year1<-years[y]
    year2<-years[y]
    tot<-length(months)*length(years)-(wSize-1)
    results<-numeric()
    while (it<=tot){
        print(it)
        if (year1==2010&wind[2]>9){
            print('hit 1 break')
            it<-tot+1
            break
        }
        print(c(wind[1],year1,wind[2], year2))
        if (wind[2]>12){
            #print('hit this part')
            if (year2!=yearLast){
                wind[2]<-1
                y<-y+1
                year2<-years[y]
                #print('got 1')
                subDta<-subset(dta,((dta$Year==year2 & dta$MonthPosted<=wind[2]) | (dta$Year==year1 & dta$MonthPosted>=wind[1])))
                #print(length(subDta$male))
                }
            else {
                it<-tot+1
                print('hit 2 break')
                break
            }
        } else if (wind[1]>12){
            wind[1]<-1
            year1=years[y]
            #print('got 2')
            subDta<-subset(dta,(dta$Year==year2 & dta$MonthPosted<=wind[2] & dta$MonthPosted>=wind[1]))
        } else {
            if (wind[1]>wind[2]){
                #print('got 3')
                subDta<-subset(dta,((dta$Year==year2 & dta$MonthPosted<=wind[2]) | (dta$Year==year1 & dta$MonthPosted>=wind[1])))
            }else if (wind[1]<wind[2]){
                #print('got 4')
                subDta<-subset(dta,(dta$Year==year1 & dta$MonthPosted<=wind[2] & dta$MonthPosted>=wind[1]))
            }else {
                subDta<-subset(dta,(dta$Year==year1 & dta$MonthPosted==wind[2]))
            }
        }
        
        #RUN THE FUNCTION
        r<-fun(subDta)
        
        
        print(r)
        results<-c(results,r)
        it<-it+1
        wind[1]<-wind[1]+1
        wind[2]<-wind[2]+1
        }
    xlab<-vector()
    for (year in years){
        xlab<-c(xlab,paste(months,year))   
    }
    xlab<-xlab[-1:-((wSize-1)/2)]
    if (2010 %in% years){
        if ((wSize-1)/2<=(12-wind[2]+1)){
            xlab<-xlab[-(length(xlab)-(12-wind[2]+1)):-length(xlab)]    #subtract 1 because the wind[2]<-wind[2]+1 even when hits the end
        }else{
            xlab<-xlab[-(length(xlab)-((wSize-1)/2)+1):-length(xlab)]
        }
    }else{
        xlab<-xlab[-(length(xlab)-((wSize-1)/2)+1):-length(xlab)]
        }
    dat<-as.data.frame(results)
    dat$date<-xlab
    return(dat)
    }
    
moveRegressionLikelihood<-function(dta,year1,yearLast,wSize,form,values){
    #moveRegression runs a regression (defined by 'form') and returns expected likelihood of regression given list of values
    #length(values)==length(model$coefficients).
    #Returns a data.frame of expected likelihood, model coefficients, and median date (i.e. if Jan-March '08, date is February '08)
    years<-seq(year1,yearLast,1)
    months<-seq(range(dta$MonthPosted)[1],range(dta$MonthPosted)[2],1)
    wind<-c(1,wSize)
    it<-1
    y<-1
    year1<-years[y]
    year2<-years[y]
    tot<-length(months)*length(years)-(wSize-1)
    null<-numeric()
    prob<-numeric()
    
    #intercept<-numeric()
    #pred<-numeric()
    #cof<-numeric()
    #out<-numeric()
    while (it<=tot){
        print(it)
        if (year1==2010&wind[2]>9){
            print('hit 1 break')
            it<-tot+1
            break
        }
        print(c(wind[1],year1,wind[2], year2))
        if (wind[2]>12){
            #print('hit this part')
            if (year2!=yearLast){
                wind[2]<-1
                y<-y+1
                year2<-years[y]
                #print('got 1')
                subDta<-subset(dta,((dta$Year==year2 & dta$MonthPosted<=wind[2]) | (dta$Year==year1 & dta$MonthPosted>=wind[1])))
                #print(length(subDta$male))
                }
            else {
                it<-tot+1
                print('hit 2 break')
                break
            }
        } else if (wind[1]>12){
            wind[1]<-1
            year1=years[y]
            #print('got 2')
            subDta<-subset(dta,(dta$Year==year2 & dta$MonthPosted<=wind[2] & dta$MonthPosted>=wind[1]))
        } else {
            if (wind[1]>wind[2]){
                #print('got 3')
                subDta<-subset(dta,((dta$Year==year2 & dta$MonthPosted<=wind[2]) | (dta$Year==year1 & dta$MonthPosted>=wind[1])))
            }else if (wind[1]<wind[2]){
                #print('got 4')
                subDta<-subset(dta,(dta$Year==year1 & dta$MonthPosted<=wind[2] & dta$MonthPosted>=wind[1]))
            }else {
                subDta<-subset(dta,(dta$Year==year1 & dta$MonthPosted==wind[2]))
            }
        }
        #if (length(subDta$male)==0){
        #    print('empty data')
        #    print('hit 3 break')
        #    break
        #}
        #if (year2==doseDay[1] & wind[2]==doseDay[2]){
        #    init<-it
        #}
        #if (year1==doseDay[3] & wind[1]==doseDay[4]){
        #    last<-it
        #}
        #RUN THE MODEL
        subDta<-subset(subDta,(is.na(subDta$nbZLkRatio)== FALSE&subDta$restrips==0 & subDta$resvis==0)) #cut out small-n variables that cannot be estimated.  Eliminated rather than thrown into 'other' because they are not the same as 'other'
        model<-glm(form,family=binomial,data=subDta) #+ log(reach) + log(price2)+resbooks + restech + resvis +restrips + resother|||||||+poverty+ future + log(reach) + log(price2) +gradeK + gradeElem + gradeMid +poverty + resbooks + restech + resvis +restrips + resother +special + arts + language + socsci + sports + applied  + AltSchool + rural + suburban, family = binomial,data=subDta)
        attach(subDta)
        print(paste(model$coefficients, values[2,]))
        
        #get predicted log odds for particular types of people, edit the 1,sLogPreds, and sGender for different configurations of sex, institution, and performance
        for (i in c(1,2)){
            fit<-sum(model$coefficients*values[i,])
            if (i==1)
                null<-c(null,exp(fit)/(1+exp(fit)))
            if (i==2)
                prob<-c(prob,exp(fit)/(1+exp(fit)))
            }
        
        #store the coefficients from the model for post-hoc analysis of changes in coefficients
        if (it==1){
            bigs<-length(subDta$GivePage[subDta$GivePage==1])/length(subDta$sMale)
            d<-rbind(c(fit,model$coefficients))
        }else{
            bigs<-c(bigs,length(subDta$GivePage[subDta$GivePage==1])/length(subDta$sMale))
            d<-rbind(d,c(fit,model$coefficients))
        }
        #transform prediction into a percent chance of funding
        #prob<-c(prob,exp(fit)/(1+exp(fit)))#GenSexModel$coefficients['male:preds'][[1]])#-GenSexModel2$coefficients['male:normpreds'][[1]])
        print('Number of Cases:')
        print(length(subDta$male))
        detach(subDta)
        it<-it+1
        wind[1]<-wind[1]+1
        wind[2]<-wind[2]+1
        }
    xlab<-vector()
    for (year in years){
        xlab<-c(xlab,paste(months,year))   
    }
    xlab<-xlab[-1:-((wSize-1)/2)]
    if (2010 %in% years){
        if ((wSize-1)/2<=(12-wind[2]+1)){
            xlab<-xlab[-(length(xlab)-(12-wind[2]+1)):-length(xlab)]    #subtract 1 because the wind[2]<-wind[2]+1 even when hits the end
        }else{
            xlab<-xlab[-(length(xlab)-((wSize-1)/2)+1):-length(xlab)]
        }
    }else{
        xlab<-xlab[-(length(xlab)-((wSize-1)/2)+1):-length(xlab)]
        }
    print(xlab)
    x<-seq(1,length(xlab),1)
    if (length(years)>1){
        x2<-seq(1,length(xlab),4)
    }else{
        x2<-seq(1,length(xlab),2)
    }
    xlab2<-xlab[x2]
    #mx<-max(c(prob))
    #mn<-min(c(prob))
    #print(x)
    #print(prob)
    #plot(x,bigs,type='l',xaxt='n',ylab='Percent receiving gift cards')
    #plot(x,prob,type='l',
    #xlab="Time (per Five Months)", ylab="Probability of Success",
    #ylim=c(0,1),
    #xaxt='n',
    #col="red")
    #par(new=T)
    #lines(x,null, type='l',
    ##xlab="", ylab="",axes=F,
    #col="black")
    #axis(1,at=x2,labels=xlab2)
    ##construcing the vertical lines for the dose period
    #print(init)
    #print(last)
    #doses<-c(init,init,last,last)
    #yvals<-c(-.5,2,2,-.5)
    #polygon(doses,yvals,col='#ECE9E9AA',border=NA)
    dat<-as.data.frame(d)
    dat$date<-xlab
    return(dat)
    }
    
plotMover<-function(dat,vars,names,varPars,plotParams,ptype,lgnd=F){
    #plots variables in varNames produced by the results of a moving regression function. varPars and plotParams are custom plot parameters.
    #varNames = c('VarName1','VarName2')
    #varPars = concatenated lists of parameters for each variable (especially color or pch) for legend()c('color_For_Var1','color_For_Var2')
    #plotParams is a list(ylab='Y-Label',main='Title')
    #Must supply xlab and ylab in plotParams for readable graphs, type='l' or type='p' or pch='?' for line or points is also recommended
    xs<-seq(1,nrow(dat))
    for (i in seq(1,length(vars))){
        pars<-c(list(x=xs,y=dat[[vars[i]]],xaxt='n'),varPars[i])
        if (i==1){
            if (ptype=='plot'){
                do.call(plot,modifyList(pars,plotParams))
            }else if(ptype=='points'){
                do.call(points,modifyList(pars,plotParams))  
            }else{
                do.call(lines,modifyList(pars,plotParams))  
            }
        }
        else{
            if (ptype=='points'){
                do.call(points,modifyList(pars,plotParams))
            }else{
                do.call(lines,modifyList(pars,plotParams))
            }
        }
        #lines(xs, dat[vars[i]],col=colors[i]
        #xaxt='n')
    }
    if (length(xs)>10){
        rows<-seq(1,length(xs),2)
        x<-xs[rows]
        xax<-dat$date[rows]
    }else{
        xax<-dat$date
        x<-xs
    }
    print(c(x,xax))
    axis(1,at=x,labels=xax)
    if (lgnd==T){
        addLegend(names,varPars)
    }
}

addLegend<-function(names,params){
    xlims=par('usr')[1:2]
    ylims=par('usr')[3:4]
    legend(xlims[1],ylims[2],names,as.character(params),bg='white')
}


plotDose<-function(dat,dates,windowSize,col='#ECE9E9AA'){
    #code plots a grey polygon covering dates specified in 'dates' where dates<-c("6 2009","10 2010")
    #code assumes the plot is already open. 
    init<-which(dat$date==dates[1])-trunc(windowSize/2)
    last<-which(dat$date==dates[2])+trunc(windowSize/2)
    doses<-c(init,init,last,last)
    xlims<-par('usr')[1:2]
    ylims<-par('usr')[3:4]
    yvals<-c(ylims[1],ylims[2],ylims[2],ylims[1])
    polygon(doses,yvals,col=col,border=NA)
}


moveRegressionCoefficients<-function(dta,year1,yearLast,wSize,form,values){
    #moveRegression runs a regression (defined by 'form') and returns expected likelihood of regression given list of values
    #length(values)==length(model$coefficients) and values={0,1} for wether or not to include a coefficient
    #Returns a data.frame of cumulative coefficients, each individual model coefficient, and median date (i.e. if Jan-March '08, date is February '08)
    years<-seq(year1,yearLast,1)
    months<-seq(range(dta$MonthPosted)[1],range(dta$MonthPosted)[2],1)
    wind<-c(1,wSize)
    it<-1
    y<-1
    year1<-years[y]
    year2<-years[y]
    tot<-length(months)*length(years)-(wSize-1)
    null<-numeric()
    prob<-numeric()
    
    #intercept<-numeric()
    #pred<-numeric()
    #cof<-numeric()
    #out<-numeric()
    while (it<=tot){
        print(it)
        if (year1==2010&wind[2]>9){
            print('hit 1 break')
            it<-tot+1
            break
        }
        print(c(wind[1],year1,wind[2], year2))
        if (wind[2]>12){
            #print('hit this part')
            if (year2!=yearLast){
                wind[2]<-1
                y<-y+1
                year2<-years[y]
                #print('got 1')
                subDta<-subset(dta,((dta$Year==year2 & dta$MonthPosted<=wind[2]) | (dta$Year==year1 & dta$MonthPosted>=wind[1])))
                #print(length(subDta$male))
                }
            else {
                it<-tot+1
                print('hit 2 break')
                break
            }
        } else if (wind[1]>12){
            wind[1]<-1
            year1=years[y]
            #print('got 2')
            subDta<-subset(dta,(dta$Year==year2 & dta$MonthPosted<=wind[2] & dta$MonthPosted>=wind[1]))
        } else {
            if (wind[1]>wind[2]){
                #print('got 3')
                subDta<-subset(dta,((dta$Year==year2 & dta$MonthPosted<=wind[2]) | (dta$Year==year1 & dta$MonthPosted>=wind[1])))
            }else if (wind[1]<wind[2]){
                #print('got 4')
                subDta<-subset(dta,(dta$Year==year1 & dta$MonthPosted<=wind[2] & dta$MonthPosted>=wind[1]))
            }else {
                subDta<-subset(dta,(dta$Year==year1 & dta$MonthPosted==wind[2]))
            }
        }
        
        #RUN THE MODEL
        subDta<-subset(subDta,(is.na(subDta$nbZLkRatio)== FALSE)) #cut out small-n variables that cannot be estimated.  Eliminated rather than thrown into 'other' because they are not the same as 'other'
        model<-glm(form,family=binomial,data=subDta) #+ log(reach) + log(price2)+resbooks + restech + resvis +restrips + resother|||||||+poverty+ future + log(reach) + log(price2) +gradeK + gradeElem + gradeMid +poverty + resbooks + restech + resvis +restrips + resother +special + arts + language + socsci + sports + applied  + AltSchool + rural + suburban, family = binomial,data=subDta)
        fit<-sum(model$coefficients*values)
        #attach(subDta)
        print(paste(model$coefficients, values))
        
        #store the coefficients from the model for post-hoc analysis of changes in coefficients
        if (it==1){
            #bigs<-length(subDta$GivePage[subDta$GivePage==1])/length(subDta$sMale)
            d<-rbind(c(fit,model$coefficients[as.logical(values)]))
        }else{
            #bigs<-c(bigs,length(subDta$GivePage[subDta$GivePage==1])/length(subDta$sMale))
            d<-rbind(d,c(fit,model$coefficients[as.logical(values)]))
        }
        #transform prediction into a percent chance of funding
        #prob<-c(prob,exp(fit)/(1+exp(fit)))#GenSexModel$coefficients['male:preds'][[1]])#-GenSexModel2$coefficients['male:normpreds'][[1]])
        print('Number of Cases:')
        print(length(subDta$male))
        #detach(subDta)
        it<-it+1
        wind[1]<-wind[1]+1
        wind[2]<-wind[2]+1
        }
    xlab<-vector()
    for (year in years){
        xlab<-c(xlab,paste(months,year))   
    }
    xlab<-xlab[-1:-((wSize-1)/2)] #cuts off the first wszie-1/2 observations.
    if (2010 %in% years){
        if ((wSize-1)/2<=(12-wind[2]+1)){
            xlab<-xlab[-(length(xlab)-(12-wind[2]+((wSize-1)/2))):-length(xlab)]     #subtract 1 because the wind[2]<-wind[2]+1 even when hits the end
        }else{
            xlab<-xlab[-(length(xlab)-(wind[2]+((wSize-1)/2))+1):-length(xlab)]
        }
    }else{
        xlab<-xlab[-(length(xlab)-((wSize-1)/2)+1):-length(xlab)]
        }
    print(xlab)
    x<-seq(1,length(xlab),1)
    if (length(years)>1){
        x2<-seq(1,length(xlab),4)
    }else{
        x2<-seq(1,length(xlab),2)
    }
    print(x2)
    xlab2<-xlab[x2]
    dat<-as.data.frame(d)
    dat$date<-xlab
    return(dat)
    }
    

moveRegressionIntCoefficients<-function(dta,year1,yearLast,wSize,form,values){
    #moveRegressionIntCoefficients adjusts moveRegression for the interaction effects which always print at the end of the output
    #and hence may not always be the same position (if the number of variables changes)
    #moveRegression runs a regression (defined by 'form') and returns expected likelihood of regression given list of values
    #length(values)==length(model$coefficients) and values={0,1} for wether or not to include a coefficient
    #Returns a data.frame of cumulative coefficients, each individual model coefficient, and median date (i.e. if Jan-March '08, date is February '08)
    #values is the {0,1} for the last N number of coefficients where N is the total number of interaction terms.
    years<-seq(year1,yearLast,1)
    months<-seq(range(dta$MonthPosted)[1],range(dta$MonthPosted)[2],1)
    wind<-c(1,wSize)
    it<-1
    y<-1
    year1<-years[y]
    year2<-years[y]
    tot<-length(months)*length(years)-(wSize-1)
    null<-numeric()
    prob<-numeric()
    
    #intercept<-numeric()
    #pred<-numeric()
    #cof<-numeric()
    #out<-numeric()
    while (it<=tot){
        print(it)
        if (year1==2010&wind[2]>9){
            print('hit 1 break')
            it<-tot+1
            break
        }
        print(c(wind[1],year1,wind[2], year2))
        if (wind[2]>12){
            #print('hit this part')
            if (year2!=yearLast){
                wind[2]<-1
                y<-y+1
                year2<-years[y]
                #print('got 1')
                subDta<-subset(dta,((dta$Year==year2 & dta$MonthPosted<=wind[2]) | (dta$Year==year1 & dta$MonthPosted>=wind[1])))
                #print(length(subDta$male))
                }
            else {
                it<-tot+1
                print('hit 2 break')
                break
            }
        } else if (wind[1]>12){
            wind[1]<-1
            year1=years[y]
            #print('got 2')
            subDta<-subset(dta,(dta$Year==year2 & dta$MonthPosted<=wind[2] & dta$MonthPosted>=wind[1]))
        } else {
            if (wind[1]>wind[2]){
                #print('got 3')
                subDta<-subset(dta,((dta$Year==year2 & dta$MonthPosted<=wind[2]) | (dta$Year==year1 & dta$MonthPosted>=wind[1])))
            }else if (wind[1]<wind[2]){
                #print('got 4')
                subDta<-subset(dta,(dta$Year==year1 & dta$MonthPosted<=wind[2] & dta$MonthPosted>=wind[1]))
            }else {
                subDta<-subset(dta,(dta$Year==year1 & dta$MonthPosted==wind[2]))
            }
        }
        
        #RUN THE MODEL
        subDta<-subset(subDta,(is.na(subDta$nbZLkRatio)== FALSE&subDta$restrips==0 & subDta$resvis==0)) #cut out small-n variables that cannot be estimated.  Eliminated rather than thrown into 'other' because they are not the same as 'other'
        model<-glm(form,family=binomial,data=subDta) #+ log(reach) + log(price2)+resbooks + restech + resvis +restrips + resother|||||||+poverty+ future + log(reach) + log(price2) +gradeK + gradeElem + gradeMid +poverty + resbooks + restech + resvis +restrips + resother +special + arts + language + socsci + sports + applied  + AltSchool + rural + suburban, family = binomial,data=subDta)
        svalues<-c(rep(0,length(model$coefficients)-length(values)),values)
        fit<-sum(model$coefficients*svalues)
        #attach(subDta)
        print(paste(model$coefficients, svalues))
        
        #store the coefficients from the model for post-hoc analysis of changes in coefficients
        if (it==1){
            #bigs<-length(subDta$GivePage[subDta$GivePage==1])/length(subDta$sMale)
            d<-rbind(c(fit,model$coefficients[as.logical(svalues)]))
        }else{
            #bigs<-c(bigs,length(subDta$GivePage[subDta$GivePage==1])/length(subDta$sMale))
            d<-rbind(d,c(fit,model$coefficients[as.logical(svalues)]))
        }
        #transform prediction into a percent chance of funding
        #prob<-c(prob,exp(fit)/(1+exp(fit)))#GenSexModel$coefficients['male:preds'][[1]])#-GenSexModel2$coefficients['male:normpreds'][[1]])
        print('Number of Cases:')
        print(length(subDta$male))
        #detach(subDta)
        it<-it+1
        wind[1]<-wind[1]+1
        wind[2]<-wind[2]+1
        }
    xlab<-vector()
    for (year in years){
        xlab<-c(xlab,paste(months,year))   
    }
    xlab<-xlab[-1:-((wSize-1)/2)]
    if (2010 %in% years){
        if ((wSize-1)/2<=(12-wind[2]+1)){
            xlab<-xlab[-(length(xlab)-(12-wind[2]+((wSize-1)/2))):-length(xlab)]    #subtract 1 because the wind[2]<-wind[2]+1 even when hits the end
        }else{
            xlab<-xlab[-(length(xlab)-((wSize-1)/2)+1):-length(xlab)]
        }
    }else{
        xlab<-xlab[-(length(xlab)-((wSize-1)/2)+1):-length(xlab)]
        }
    print(xlab)
    x<-seq(1,length(xlab),1)
    if (length(years)>1){
        x2<-seq(1,length(xlab),4)
    }else{
        x2<-seq(1,length(xlab),2)
    }
    xlab2<-xlab[x2]
    dat<-as.data.frame(d)
    dat$date<-xlab
    return(dat)
    }
    

moveRegressionErrors<-function(dta,year1,yearLast,wSize,form,values){
    #moveRegression runs a regression (defined by 'form') and returns expected likelihood of regression given list of values
    #length(values)==length(model$coefficients) and values={0,1} for wether or not to include a coefficient
    #Returns a data.frame of cumulative coefficients, each individual model coefficient, and median date (i.e. if Jan-March '08, date is February '08)
    years<-seq(year1,yearLast,1)
    months<-seq(range(dta$MonthPosted)[1],range(dta$MonthPosted)[2],1)
    wind<-c(1,wSize)
    it<-1
    y<-1
    year1<-years[y]
    year2<-years[y]
    tot<-length(months)*length(years)-(wSize-1)
    null<-numeric()
    prob<-numeric()
    
    #intercept<-numeric()
    #pred<-numeric()
    #cof<-numeric()
    #out<-numeric()
    while (it<=tot){
        print(it)
        if (year1==2010&wind[2]>9){
            print('hit 1 break')
            it<-tot+1
            break
        }
        print(c(wind[1],year1,wind[2], year2))
        if (wind[2]>12){
            #print('hit this part')
            if (year2!=yearLast){
                wind[2]<-1
                y<-y+1
                year2<-years[y]
                #print('got 1')
                subDta<-subset(dta,((dta$Year==year2 & dta$MonthPosted<=wind[2]) | (dta$Year==year1 & dta$MonthPosted>=wind[1])))
                #print(length(subDta$male))
                }
            else {
                it<-tot+1
                print('hit 2 break')
                break
            }
        } else if (wind[1]>12){
            wind[1]<-1
            year1=years[y]
            #print('got 2')
            subDta<-subset(dta,(dta$Year==year2 & dta$MonthPosted<=wind[2] & dta$MonthPosted>=wind[1]))
        } else {
            if (wind[1]>wind[2]){
                #print('got 3')
                subDta<-subset(dta,((dta$Year==year2 & dta$MonthPosted<=wind[2]) | (dta$Year==year1 & dta$MonthPosted>=wind[1])))
            }else if (wind[1]<wind[2]){
                #print('got 4')
                subDta<-subset(dta,(dta$Year==year1 & dta$MonthPosted<=wind[2] & dta$MonthPosted>=wind[1]))
            }else {
                subDta<-subset(dta,(dta$Year==year1 & dta$MonthPosted==wind[2]))
            }
        }
        
        #RUN THE MODEL
        subDta<-subset(subDta,(is.na(subDta$nbZLkRatio)== FALSE&subDta$restrips==0 & subDta$resvis==0)) #cut out small-n variables that cannot be estimated.  Eliminated rather than thrown into 'other' because they are not the same as 'other'
        model<-glm(form,family=binomial,data=subDta) #+ log(reach) + log(price2)+resbooks + restech + resvis +restrips + resother|||||||+poverty+ future + log(reach) + log(price2) +gradeK + gradeElem + gradeMid +poverty + resbooks + restech + resvis +restrips + resother +special + arts + language + socsci + sports + applied  + AltSchool + rural + suburban, family = binomial,data=subDta)
        fit<-sum(sqrt(diag(vcov(model)))*values)
        #attach(subDta)
        print(paste(sqrt(diag(vcov(model))), values))
        
        #calculate and store the errors from the model for post-hoc analysis of changes in errors
        if (it==1){
            #bigs<-length(subDta$GivePage[subDta$GivePage==1])/length(subDta$sMale)
            d<-rbind(c(fit,sqrt(diag(vcov(model)))[as.logical(values)]))
        }else{
            #bigs<-c(bigs,length(subDta$GivePage[subDta$GivePage==1])/length(subDta$sMale))
            d<-rbind(d,c(fit,sqrt(diag(vcov(model)))[as.logical(values)]))
        }
        print('Number of Cases:')
        print(length(subDta$projectid))
        #detach(subDta)
        it<-it+1
        wind[1]<-wind[1]+1
        wind[2]<-wind[2]+1
        }
    xlab<-vector()
    for (year in years){
        xlab<-c(xlab,paste(months,year))   
    }
    xlab<-xlab[-1:-((wSize-1)/2)]
    if (2010 %in% years){
        if ((wSize-1)/2<=(12-wind[2]+1)){
            xlab<-xlab[-(length(xlab)-(12-wind[2]+((wSize-1)/2))):-length(xlab)]    #subtract 1 because the wind[2]<-wind[2]+1 even when hits the end
        }else{
            xlab<-xlab[-(length(xlab)-((wSize-1)/2)+1):-length(xlab)]
        }
    }else{
        xlab<-xlab[-(length(xlab)-((wSize-1)/2)+1):-length(xlab)]
        }
    print(xlab)
    x<-seq(1,length(xlab),1)
    if (length(years)>1){
        x2<-seq(1,length(xlab),4)
    }else{
        x2<-seq(1,length(xlab),2)
    }
    xlab2<-xlab[x2]
    dat<-as.data.frame(d)
    dat$date<-xlab
    return(dat)
    }



ExpObsPreds<-function(dta,resp,expl,grad,plotParams){
    #This code takes an explanatory and response variable and plots the mean of the response across subgroups of expl
    #The subgroups are set by grad which is a percentage.  A grad of .1 means to cut the explanatory variable into
    #chunks of 10 percent (not quartiles, but the range).  So, 1,5,10 would be split, 1,2,3,4,5,5,6,7,8,9,10; even though
    #there are not 10 data points.
    #
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
    cisgon<-c(mapply('+',cis,means),mapply('-',rev(means),rev(cis)))
    xgon<-c(x,rev(x))
    pars<-c(list(x=x,y=means))
    do.call(plot,modifyList(pars,plotParams))
    for (i in seq(1,length(means),1)){
        segments(x[i],means[i]+cis[i],x1=x[i],y1=means[i]-cis[i],pch='|')
        #points(c(1,1),c(bike$conf.int[1],bike$conf.int[2]),pch='-',cex=2)
    }
    #polygon(xgon,cisgon)
    return(c(x,means,cis))
}


PlotFunctionGradient<-function(dta,form,values,expl,grad,plotParams){
    #This code takes a formula, runs it on subsets of the data specified by expl and plots coeficients specified by values.
    #The subgroups are set by grad which is a percentage.  A grad of .1 means to cut the explanatory variable into
    #chunks of 10 percent (not quartiles, but the range).  So, 1,5,10 would be split, 1,2,3,4,5,5,6,7,8,9,10; even though
    #there are not 10 data points.
    #
    x<-numeric()
    fits<-numeric()
    errors<-numeric()
    
    #creating gradient range
    mn<-min(dta[expl],na.rm=T)
    mx<-max(dta[expl],na.rm=T)
    #rng<-min(c(abs(init-mn),abs(init-mx)))
    vals<-seq(mn,mx, (mx-mn)*grad)       #rng takes the shorter distance between mean and upper and lower bound as end points for gradient
    
    for (i in seq(1,length(vals),1)){
        print(c(i,vals[i]))
        if (i>1 & i < length(vals)-0){         #due to range specification, the 4 edge points represent low-N cases
            d<-subset(dta,(dta[expl]>vals[i-1]&dta[expl]<vals[i+1]))  #subset of data within gradient window
            print(mean(d[[expl]],na.rm=T))
            
            #run the model, extract the needed values, and print for verification
            model<-glm(form,family=binomial,data=d)
            fit<-sum(model$coefficients*values)
            error<-sum(sqrt(diag(vcov(model)))*values)
            print(paste(model$coefficients, values))
            
            x<-c(x,mean(d[[expl]],na.rm=T))
            fits<-c(fits,fit)
            errors<-c(errors,error)
            
            print(length(model$weights))
        }
    }
    
    pars<-c(list(x=x,y=fits))
    do.call(plot,modifyList(pars,plotParams))
    for (i in seq(1,length(fits),1)){
        segments(x[i],fits[i]+errors[i],x1=x[i],y1=fits[i]-errors[i],pch='|')
        #points(c(1,1),c(bike$conf.int[1],bike$conf.int[2]),pch='-',cex=2)
    }
    return(c(x,fits,errors))
}

