'''
Created 3/23/2014

Summary: this code is written to take an exactly matched list of project IDs from the Stratifying.R code and generate
estimates of gender across the exactly matched sample.  The main options are whether to do one-to-one exact matching,
withing exact match grouping, or maximum exact match grouping.  These are parameters for me, not publication, as they
each should produce the same results.  This should be used in conjunction with the within-matched group text analysis
which should determine the extent to which classifiers work better within groups and whether or not different groups
have different gender (measure of structural endorsement)

Update 9/23/2015
Summary: Following reviewer 3's question, this code was significantly restructured to perform k-fold cross-validation
on the random sample of 56,000 essays. The core functions are mostly the same though data handling was re-organized.

Preliminary testing indicated that reducing the number of features had no effect on the speed of the algorithm nor
its accuracy.  Increasing the number of texts in the training sample significantly decreases the speed of the algorithm
with little improvement to accuracy (~60% accuracy when training on 2,200 essay and 64% when training on 52,000).

Conclusion: while I expect no difference in the results, k-fold cross-validation is a better way to approach to estimation than
random subsampling.  Since I never tried it, I'm doing it now.

Results: The 10-fold cross-validation correlates with the existing measure at .89 and yields the same results, though
the significance of the independent effect may be slightly reduced.  I didn't do a full comparison.

'''

import matplotlib
import re
import nltk
import csv
import os
import cPickle
from sklearn.feature_extraction.text import TfidfVectorizer as tfidf
from sklearn import svm
from sklearn.naive_bayes import MultinomialNB as mnb
from sklearn import cross_validation
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


def ExtractTexts(essayDir=''):
    '''This code grabs the texts for each eligible essay selected by the subsamples within selected matched groups
    returns a dictionary of dict[project id]=text.  Type can be either "one2one" or "group."  Samples should be a
    the list of projectIDs from ExactSample functions'''
    texts={}
    er=0
    if essayDir=='':
        essayDir='Data\MatchedEssays'
    for fn in os.listdir(essayDir):
        if fn.endswith('.txt')==True:
            with open(essayDir+'\\'+fn,'rb') as txt:
                try:
                    texts[fn.split('.')[0]]=txt.read()
                except:
                    er+=1
                    continue
        if fn.endswith('.pkl')==True:
            with open(essayDir+'\\'+fn,'rb') as pkl:
                sexKey=cPickle.load(pkl)
    print '%d MISSING CASES' % er
    return texts, sexKey

def Vectorize(texts,genKey,vocabulary):
    if vocabulary ==[]:
        vectorizer= tfidf(texts.values(),stop_words='english',min_df=5)   #the only real question I have with this is whether it ejects twitter-specific text (ie. @ or #)
    else:
        vectorizer= tfidf(texts.values(),stop_words=None,vocabulary=vocabulary,min_df=5)
    vec=vectorizer.fit_transform(texts.values()) 
    labels=[]
    for k in texts.keys():
        labels.append(genKey[k])
    labels=np.asarray(labels)   
    return vec,labels,vectorizer
    
def K_Sample(vec,labels,texts,train,test):
    '''
    take the list of indexes from KFold() and select out train and text vectors and labels.
    '''
    testKey={}
    ts=0
    for t in test:    
        testKey[ts]=texts.keys()[t]
        ts+=1
    trainTexts=vec[train]
    trainLabels=labels[train]
    testTexts=vec[test]
    testLabels=labels[test]
    return trainTexts, trainLabels, testTexts,testLabels,testKey

def Classify(trainT,trainL,testT,testL,clf='knn'):
    '''Code to train and test classifiers.  type can be 'knn' 'nb' or 'svm'
    returns the fit matrix #a dictionary of {twitterID: likelihood ratio}'''
    if clf=='knn':
        cl = neighbors.KNeighborsClassifier()
        cl.fit(trainT,trainL)
        fit=cl.predict_proba(testT)
        #print(cl.score(testT,testL))
    if clf=='svm':
        cl=svm.SVC(C=100,gamma=.1,probability=True) #C and gamma identified by grid_search elsewhere.
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
    '''
    vec= original vectorized essays
    clf = the trained classifier
    clfType = the classifier type "svm",'nb', etc
    n = pull out the top n words
    
    returns a list of the top n words for both males and females
    '''
    if clfType=='nb':
        feature_names = vec.get_feature_names()
        coefs_with_fns = sorted(zip(clf.coef_[0], feature_names))
        top = zip(coefs_with_fns[:n], coefs_with_fns[:-(n + 1):-1])   
    else:
        top=''
    return top

def CleanResults(fit,testKeys):
    '''This code takes the results of classifier.predict_proba() and cleans out extreme scores and produces z-scored likelihood ratios.
    It replaces any probabilites of 1 or 0 (which produce inf likelihoods) with the nearest max and min probabilites given.
    It then computes the likelihood ratio and z-scores them, returning res as a dictionary of {ID: z-scored likelihood ratio}'''
    
    #identify any possible infinite values and recode using the next maximum probability.  It's rare but breaks the code.
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

    #standardize the estimates
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
    header=metaHeader+classHeader+['FemProb','MascProb','zLkRatio']+['FemProbStd','MascProbStd','zLkRatioStd']
    f=open(outfile,'wb')
    writeit=csv.writer(f)
    writeit.writerow(header)
    for line in data:
        writeit.writerow(line)
    f.close()
    return


def KFoldValidationTest(clfs = ['nb','knn','svm'],vocabulary=[],classNames=[],folds=10,dfile='',essaydir='',outfile=''):
    '''This function selects the maxi-min number of matching texts (maximum number of minimum matches - or double the lowest number
    of matches per match class) gets the essays, and then trains, tests, and prints the results of the classifiers '''
    accs=dict(zip(clfs,[{} for i in xrange(len(clfs))]))
    words={}
    data={}
    for cname in classNames:
        print 'beginning kfold classification'
        #accs=dict(zip(clfs,[[] for i in xrange(len(clfs))]))
        #matches,genKey=getMatchSet(fname=dfile,idx=cname)
        #exSample=MaxExactSample(matches,cut=0)
        texts,sexKey=ExtractTexts(essayDir=essaydir)
        vec,labels,vectorizer=Vectorize(texts,sexKey,vocabulary)
        for clf in clfs:
            data[clf]={}
            words[clf]={}
        print 'getting k-folds'
        k_folds=cross_validation.KFold(np.shape(vec)[0],folds)
        for k, (train, test) in enumerate(k_folds):
            for clf in clfs:
                print "Doing: ", clf, ' on ', len(train),' training texts on fold ', k,
                trainTexts, trainLabels, testTexts,testLabels,testKey=K_Sample(vec,labels,texts,train, test)
                fit,classifier=Classify(trainTexts, trainLabels, testTexts,testLabels,clf=clf)
                words[clf][k]=TopWords(vectorizer,classifier,clfType=clf,n=100)
                res=CleanResults(fit, testKey)
                for projectid,r in res.iteritems():
                    if projectid in data[clf].keys():
                        for i,num in enumerate(r):
                            data[clf][projectid][i].append(num)
                    else:
                            data[clf][projectid]=[[num] for num in r]
                print 'Accuracy for ', clf, ' from fold ', k, ' was: ', classifier.score(testTexts,testLabels)
                accs[clf][k]=classifier.score(testTexts,testLabels)
        
        print 'formatting data for saving'
        outdata=[]
        for clf,res in data.iteritems():
            for projectid,vals in res.iteritems():
                outdata.append([projectid,clf,len(vals[0])]+map(np.mean,vals)+map(np.std,vals))
        metaHeader=['projectid']
        classHeader=['classifier','numTested']
        print 'writing data'
        SaveResults(outdata,metaHeader,classHeader,outfile=outfile)
        
    return data,words,accs

def RunNFoldValidation():
    dfile='Data\ExactTeacherStrata.csv'
    
    essayDir='Data\MatchedEssays2'
    outfile='Data\NFoldValidationResults.csv'
    #res=IntraMatchTest(clfs = ['nb','svm'],classNames=[('subclass','male')],minSize=200,nfolds=2,dfile=dfile,essaydir=essayDir,outfile=outfile)
    Res,words,accs=KFoldValidationTest(clfs = ['nb'],vocabulary=[],classNames=[('subclass','male')],folds=10,dfile=dfile,essaydir=essayDir,outfile=outfile)
    
    #(clfs = ['nb','knn','svm'],samples=[.5],iters=200,dfile='',essaydir='',outfile='')
    return Res,words,accs

Res,words,accs=RunNFoldValidation()
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
