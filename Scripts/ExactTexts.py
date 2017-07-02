'''
Created 3/23/2014

Summary: this code is written to take an exactly matched list of project IDs from the Stratifying.R code and generate
estimates of gender across the exactly matched sample.  The main options are whether to do one-to-one exact matching,
withing exact match grouping, or maximum exact match grouping.  These are parameters for me, not publication, as they
each should produce the same results.  This should be used in conjunction with the within-matched group text analysis
which should determine the extent to which classifiers work better within groups and whether or not different groups
have different gender (measure of structural endorsement)
'''


import re
import nltk
import csv
import random as rn
from sklearn.feature_extraction.text import TfidfVectorizer as tfidf
from sklearn import svm
from sklearn.naive_bayes import MultinomialNB as mnb
from sklearn import neighbors
import numpy as np



def tokenize(text,stopwords=nltk.corpus.stopwords.words('english')):
    '''this function tokenizes `text` using simple rules:
    tokens are defined as maximal strings of letters, digits
    and apostrophies.
    The optional argument `stopwords` is a list of words to
    exclude from the tokenzation'''
    # make lowercase
    text = text.lower()
    # grab just the words we're interested in
    text = re.findall(r"[\d\w']+",text)
    # remove stopwords
    res = []
    for w in text:
        if w not in stopwords:
            res.append(w)
    
    return(res)


def getMatchSet(fname='',idx=''):
    '''this code generates the list of match classes to be included in the training set and those left out of the training set
    matchName is the name of the varaible in the file for the match group variable.  pct is the percent of matched groups to
    select from all match groups.  Returns a list of the selected matched groups and a list of those held out '''
    matches={}
    classKey={}
    f=open(fname,'rb')
    data=csv.reader(f)
    for i,line in enumerate(data):
        if i==0:
            for j,l in enumerate(line):
                if l.lower()=='projectid':
                    pid=j
                if l.lower()==idx[0].lower():
                    midx=j
                    print 'found ', idx[0], ' index = ', j,l
                if l.lower()==idx[1].lower():
                    cidx=j
                    print 'found ', idx[1], ' index = ', j,l
        if i>0:
            if line[cidx]!='NA' and line[midx]!='NA':
                cl=int(line[cidx])
                mt=int(line[midx])
                p=line[pid]
                if mt in matches.keys():
                    matches[mt][cl].append(p)
                else:
                    matches[mt]={}
                    matches[mt][0]=[]
                    matches[mt][1]=[]
                    matches[mt][cl].append(p)
                classKey[p]=cl
    f.close()
    
    return matches,classKey


def InGroupExactSample(match):
    '''This code uses the exact match groups to create a within matched sample set of IDs (without replacement).
    If a group has 1000 females and 2000 males, it will produce 1000 male-female matches.  Matches is the dictionary
    {groupID: MaleIDs: [], FemaleIDs:[]}  Cut is the minimum number of texts to return from a matched group to include
    that group.  For example if cut = 10, there must be at least 10 men and 10 women in the matched group to run the analysis'''
    sample=[]
    mn=min(map(len,match.values()))  #get min number of cases
    for k in match.keys():
        sample+=rn.sample(match[k],mn)
    
    rn.shuffle(sample)
    if len(sample)==len(set(sample)):
        return sample
    else:
        print 'found duplicate cases in matched data'   #this should never happen
        return sample


def MaxExactSample(matches,cut=0):
    '''This code uses the exact match groups to create a min-max matched sample (currently without replacement).
    If a group has 1000 females and 2000 males, it will produce 1000 male-female matches.  Matches is the dictionary
    {groupID: MaleIDs: [], FemaleIDs:[]}  Cut is the minimum number of texts to return from a matched group to include
    that group.  For example if cut = 10, there must be at least 10 men and 10 women in the matched group to run the analysis'''
    sample=[]
    for m,match in matches.iteritems():
        mn=min(map(len,match.values()))#get min number of cases
        for ids in match.values():
            sample+=rn.sample(ids,mn)
    if len(sample)==len(set(sample)):
        return sample
    else:
        print 'found duplicate cases in matched data'   #this should never happen
        return sample

def ExtractTexts(samples,essayDir=''):
    '''This code grabs the texts for each eligible essay selected by the subsamples within selected matched groups
    returns a dictionary of dict[project id]=text.  Type can be either "one2one" or "group."  Samples should be a
    the list of projectIDs from ExactSample functions'''
    print 'getting %d essays' % len(samples)
    texts={}
    er=0
    if essayDir=='':
        essayDir='Essays'
    for proj in samples: 
        fname=essayDir+'\\'+proj+'.txt'
        try:
            f=open(fname,'rb')
            texts[proj]=f.read()
            f.close()
        except:
            er+=1
            continue
    print '%d MISSING CASES' % er
    return texts

def Vectorize(texts,genKey,vocabulary):
    if vocabulary ==[]:
        vectorizer= tfidf(texts.values(),stop_words=None,min_df=5)   #the only real question I have with this is whether it ejects twitter-specific text (ie. @ or #)
    else:
        vectorizer= tfidf(texts.values(),stop_words=None,vocabulary=vocabulary,min_df=5)
    vec=vectorizer.fit_transform(texts.values()) 
    labels=[]
    for k in texts.keys():
        labels.append(genKey[k])
    labels=np.asarray(labels)   
    return vec,labels,vectorizer
    
    
def Sample(vec,labels,texts,samp=.2):
    '''This code creates the randomized test/train samples and the trains and tests the classifier
    and returns the vectors of test and train texts and labels as well as keys for linking results to IDs   '''
    if type(samp)==float:
        trainIds=rn.sample(xrange(np.shape(labels)[0]),int(round(np.shape(labels)[0]*samp)))
    if type(samp)==int:
        trainIds=rn.sample(xrange(np.shape(labels)[0]),samp)
    if type(samp)==np.ndarray:
        #print np.mean(samp)
        trainIds=list(samp)
        #print trainIds
        
    testIds=[]
    trainKey={}
    testKey={}
    ts=0
    tr=0
    for t in xrange(np.shape(labels)[0]):    
        if t not in trainIds:
            testIds.append(t)
            testKey[ts]=texts.keys()[t]
            ts+=1
        else:
            trainKey[tr]=texts.keys()[t]
            tr+=1
    trainTexts=vec[trainIds]
    trainLabels=labels[trainIds]
    testTexts=vec[testIds]
    testLabels=labels[testIds]
    
    return trainTexts, trainLabels, testTexts,testLabels,trainKey,testKey


def Classify(trainT,trainL,testT,testL,clf='knn'):
    '''Code to train and test classifiers.  type can be 'knn' 'nb' or 'svm'
    returns the fit matrix #a dictionary of {twitterID: likelihood ratio}'''
    if clf=='knn':
        cl = neighbors.KNeighborsClassifier()
        cl.fit(trainT,trainL)
        fit=cl.predict_proba(testT)
        #print(cl.score(testT,testL))
    if clf=='svm':
        cl=svm.SVC(C=100,gamma=.1,probability=True)
        cl.fit(trainT,trainL)
        fit=cl.predict_proba(testT)
        #print(cl.score(testT,testL))
    if clf=='nb':
        cl=mnb()
        cl.fit(trainT,trainL)
        fit=cl.predict_proba(testT)
        #print(cl.score(testT,testL))
    return fit, cl

def TopWords(vec,clf,clfType='',n=20):
    if clfType=='nb':
        feature_names = vec.get_feature_names()
        coefs_with_fns = sorted(zip(clf.coef_[0], feature_names))
        top = zip(coefs_with_fns[:n], coefs_with_fns[:-(n + 1):-1])   
        
        #Access top words with this print loop:
        #for (coef_1, fn_1), (coef_2, fn_2) in top:
        #    print "\t%.4f\t%-15s\t\t%.4f\t%-15s" % (coef_1, fn_1, coef_2, fn_2)
    else:
        top=''
    return top 

def CleanResults(fit,testKeys):
    '''This code takes the results of classifier.predict_proba() and cleans out extreme scores and produces z-scored likelihood ratios.
    It replaces any probabilites of 1 or 0 (which produce inf likelihoods) with the nearest max and min probabilites given.
    It then computes the likelihood ratio and z-scores them, returning res as a dictionary of {ID: z-scored likelihood ratio}'''
    #identify any possible infinite values and recode using the next maximum probability
    if 0 in fit:
        lis=sorted(fit[:,0],reverse=True)
        lis+=sorted(fit[:,1],reverse=True)
        for l in sorted(lis,reverse=True):
            if l!=1.0:
                fit[fit==1.0]=l
                break
        for l in sorted(lis):
            if l!=0.0:
                fit[fit==0.0]=l
                break

    res=dict(zip(testKeys.values(),[0 for i in xrange(len(testKeys.keys()))]))
    for i,line in enumerate(fit):
        res[testKeys[i]]=[line[0],line[1],np.log(line[0]/line[1])]
    vals=[i[2] for i in res.values()]
    m=np.mean(vals)
    sd=np.std(vals)
    for k,v in res.iteritems():
        res[k]=[v[0],v[1],(v[2]-m)/sd]
    return res



def SaveResults(data,metaHeader,classHeader,outfile=''):
    #check code for writing correctness then validate format and add headers for initial data creation
    '''This function joins the classifier results with the classifier metadata and the person metadata to
    the existing data of the same structure:
    PersonData, classifier data, classificaiton results
    res is the z-scored likelihood ratio data {twitterid: scored ratio}
    metadata is the dictionary {twitterID: list of person data}
    classMeta is a list of classifier features including sample pct, type of classifier, and iteration
    fname is the name of the data file being dumped to.'''
    
    print 'saving data'
    header=metaHeader+classHeader+['FemProb','MascProb','zLkRatio']
    f=open(outfile,'wb')
    writeit=csv.writer(f)
    writeit.writerow(header)
    for line in data:
        writeit.writerow(line)
    f.close()
    return


def InterMatchTest(clfs = ['nb','knn','svm'],vocabulary=[],classNames=[],samples=[.5],iters=200,dfile='',essaydir='',outfile=''):
    '''This function selects the maxi-min number of matching texts (maximum number of minimum matches - or double the lowest number
    of matches per match class) gets the essays, and then trains, tests, and prints the results of the classifiers '''
    Res=[]
    for cname in classNames:
        data={}
        words={}
        #accs=dict(zip(clfs,[[] for i in xrange(len(clfs))]))
        matches,genKey=getMatchSet(fname=dfile,idx=cname)
        exSample=MaxExactSample(matches,cut=0)
        texts=ExtractTexts(exSample,essayDir=essaydir)
        vec,labels,vectorizer=Vectorize(texts,genKey,vocabulary)
        for samp in samples:
            for clf in clfs:
                data[clf]={}
                words[clf]={}
                accs=[]
                for it in xrange(iters):
                    print "Doing: ", clf, ' ', samp, ' ', it
                    #classMeta=[clf,samp,it]
                    #classHeader=['Classifier','SamplePct','Iteration']
                    trainTexts, trainLabels, testTexts,testLabels,trainKey,testKey =Sample(vec,labels,texts,samp=samp)
                    fit,classifier=Classify(trainTexts, trainLabels, testTexts,testLabels,clf=clf)
                    words[clf][it]=TopWords(vectorizer,classifier,clfType=clf,n=400)
                    #accs[clf].append([nUsed,classifier.score(testTexts,testLabels)])
                    print 'Accuracy for ', clf, ' from ', samp, ' essays was ', classifier.score(testTexts,testLabels)
                    accs.append(classifier.score(testTexts,testLabels))
                    res=CleanResults(fit, testKey)
                    for k,r in res.iteritems():
                        if k in data[clf].keys():
                            for i,num in enumerate(r):
                                data[clf][k][i].append(num)
                        else:
                                data[clf][k]=[[num] for num in r]
                        #data.append(metaData[k]+classMeta+r)
                Res.append([clf,samp,np.mean(accs)])
                print "Accuracy of ", clf, " classifer on ",samp," samples is ",np.mean(accs)
    
        print 'formatting data for saving'
        outdata=[]
        for clf,res in data.iteritems():
            for k,vals in res.iteritems():
                outdata.append([k,clf,len(vals[0])]+map(np.mean,vals))
        metaHeader=['projectid']
        classHeader=['classifier','numTested']
        print 'writing data'
        SaveResults(outdata,metaHeader,classHeader,outfile=outfile)
        
    return Res,words

def IntraMatchTest(clfs = ['nb','knn','svm'],vocabulary=[],classNames=[],minSize=0,nfolds=2,dfile='',essaydir='',outfile=''):
    '''This function selects the maxi-min number of matching texts (maximum number of minimum matches - or double the lowest number
    of matches per match class) gets the essays, and then trains, tests, and prints the results of the classifiers.
    Note, IntraMatchTest uses folding instead of iterative random subsampling to estimate probabilities within groups.
    You set the number of folds with 'nfolds' (default = 2, or train with first half, then train with second half)
    minSize = minimum number of texts within a matched group needed to include that group.
    '''
    data=[]
    Res=dict(zip(clfs,[[] for i in xrange(len(clfs))]))
    for cname in classNames:
        words={}
        matches,classKey=getMatchSet(fname=dfile,idx=cname)
        accs=dict(zip(clfs,[[] for i in xrange(len(clfs))]))
        for m,gens in matches.iteritems():
            #print gens.keys(),gens.values()[1][0:10]
            if min(map(len,gens.values()))>minSize:
                words[m]={}
                gSample=InGroupExactSample(gens)
                nUsed=len(gSample)
                genIneq= float(nUsed)/(sum([len(v) for v in gens.values()])/2.0)      #compares used portion to whole match. basically measures how many females were excluded within groups.
                texts=ExtractTexts(gSample,essayDir=essaydir)
                vec,labels,vectorizer=Vectorize(texts,classKey,vocabulary)
                folds=np.array_split(xrange(np.shape(labels)[0]),nfolds)
                for clf in clfs:
                    words[m][clf]=[]
                    for fold in folds:
                        print "Running: ", clf, ' on ', cname, ' group ', m,
                        #classMeta=[clf,samp,it]
                        #classHeader=['Classifier','SamplePct','Iteration']
                        trainTexts, trainLabels, testTexts,testLabels,trainKey,testKey =Sample(vec,labels,texts,samp=fold)
                        fit,classifier=Classify(trainTexts, trainLabels, testTexts,testLabels,clf=clf)
                        tops=TopWords(vectorizer,classifier,clfType=clf,n=40)
                        words[m][clf].append(tops)
                        accs[clf].append([nUsed,classifier.score(testTexts,testLabels)])
                        print 'Accuracy of ', clf, ' on ', cname, ' was ', classifier.score(testTexts,testLabels)
                        res=CleanResults(fit, testKey)
                        for k,r in res.iteritems():
                            data.append([k]+[clf,nUsed,genIneq]+r)
                    Res[clf].append(np.mean(accs[clf]))
        #print "Accuracy of ", clf, " classifer on ",cname[0]," samples is ",np.mean(accs[clf]),' from ', len(accs[clf])  , ' matched samples'
        #metaHeader=['projectid']
        #classHeader=['classifier','numTxtUsed','matchSexBalance']
        #print 'writing data'
        #SaveResults(data,metaHeader,classHeader,outfile=outfile)
            
    return Res,accs,words,classifier,vectorizer

def RunIntraGroup():
    dfile='Data\ExactTeacherStrata.csv'
    essayDir='Essays'
    outfile='Data\IntraMatchAllWordsClassResults.csv'
    #res=InterMatchTest(clfs = ['svm'],samples=[5000,10000,20000],iters=1,dfile=dfile,essaydir=essayDir,outfile='')
    
    res,accs,words,clf,vec=IntraMatchTest(clfs = ['nb'],vocabulary=[],classNames=[('subclass','male')],minSize=200,nfolds=2,dfile=dfile,essaydir=essayDir,outfile=outfile)
    return res,accs,words,clf,vec

def RunInterGroup():
    dfile='Data\ExactTeacherStrata.csv'
    essayDir='Essays'
    outfile='Data\InterMatchAllWordsClassResults.csv'
    #res=IntraMatchTest(clfs = ['nb','svm'],classNames=[('subclass','male')],minSize=200,nfolds=2,dfile=dfile,essaydir=essayDir,outfile=outfile)
    res,words=InterMatchTest(clfs = ['nb','svm'],vocabulary=[],classNames=[('subclass','male')],samples=[2000],iters=30,dfile=dfile,essaydir=essayDir,outfile=outfile)
    
    #(clfs = ['nb','knn','svm'],samples=[.5],iters=200,dfile='',essaydir='',outfile='')
    return res,words

intraRes,intraAccs,intraWords,clf,vec=RunIntraGroup()
#interRes,interWords=RunInterGroup()

def PlotAccuracyByNum(accs):
    x=[a[0] for a in accs['nb']]
    y=[a[1] for a in accs['nb']]
    matplotlib.pyplot.scatter(x,y)
    matplotlib.pyplot.xlabel('Number of Essays used from a Matched Group')
    matplotlib.pyplot.ylabel('Accuracy of the Naive Bayes Classifier')
    matplotlib.pyplot.title('NB Accuracy by Number of Essays in Matched Groups')
    matplotlib.pyplot.savefig('Results\\IntraGroupAccByNumEssaysForSex2.png', bbox_inches='tight')
    matplotlib.pyplot.close('all')
    
    x=[a[0] for a in accs['svm']]
    y=[a[1] for a in accs['svm']]
    matplotlib.pyplot.scatter(x,y)
    matplotlib.pyplot.xlabel('Number of Essays used from a Matched Group')
    matplotlib.pyplot.ylabel('Accuracy of the SVM Classifier')
    matplotlib.pyplot.title('SVM Accuracy by Number of Essays in Matched Groups')
    matplotlib.pyplot.savefig('Results\\SVMIntraGroupAccByNumEssaysForSex.png', bbox_inches='tight')
    matplotlib.pyplot.close('all')
    return
