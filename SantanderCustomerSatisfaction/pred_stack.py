# -*- coding: utf-8 -*-
"""
Created on Sun Apr 17 16:54:22 2016

@author: ppj
"""
"

from __future__ import division
import numpy as np
#import load_data
from sklearn.cross_validation import StratifiedKFold
from sklearn.ensemble import RandomForestClassifier, ExtraTreesClassifier, GradientBoostingClassifier
from sklearn.linear_model import LogisticRegression
import pandas as pd

def logloss(attempt, actual, epsilon=1.0e-15):
    """Logloss, i.e. the score of the bioresponse competition.
    """
    attempt = np.clip(attempt, epsilon, 1.0-epsilon)
    return - np.mean(actual * np.log(attempt) + (1.0 - actual) * np.log(1.0 - attempt))


if __name__ == '__main__':

    np.random.seed(0) # seed to shuffle the train set

    n_folds = 10
    verbose = True
    shuffle = False
    X = pd.read_csv('/Users/ppj/Documents/Practice/SantanderCustomerSatisfaction/cache/df_train.csv')
    y = X[["TARGET"]]
    X_submission = pd.read_csv('/Users/ppj/Documents/Practice/SantanderCustomerSatisfaction/cache/df_test.csv')

    # X, y, X_submission = load_data.load()

    if shuffle:
        idx = np.random.permutation(y.size)
        X = X[idx]
        y = y[idx]

    skf = list(StratifiedKFold(X.TARGET, n_folds))

    clfs = [RandomForestClassifier(n_estimators=500, n_jobs=-1, criterion='gini'),
            RandomForestClassifier(n_estimators=500, n_jobs=-1, criterion='entropy'),
            ExtraTreesClassifier(n_estimators=500, n_jobs=-1, criterion='gini'),
            ExtraTreesClassifier(n_estimators=500, n_jobs=-1, criterion='entropy'),
            GradientBoostingClassifier(learning_rate=0.05, subsample=0.5, max_depth=6, n_estimators=800)]

    print "Creating train and test sets for blending."
    
    dataset_blend_train = np.zeros((X.shape[0], len(clfs)))
    dataset_blend_test = np.zeros((X_submission.shape[0], len(clfs)))
    
    for j, clf in enumerate(clfs):
        print j, clf
        dataset_blend_test_j = np.zeros((X_submission.shape[0], len(skf)))
        for i, (train, test) in enumerate(skf):
            print "Fold", i
            X_train = np.array(X.iloc[train])
            y_train = np.array(y.iloc[train])
            X_test = np.array(X.iloc[test])
            y_test = np.array(y.iloc[test])
            clf.fit(X_train, y_train.ravel())
            y_submission = clf.predict_proba(X_test)[:,1]
            dataset_blend_train[test, j] = y_submission
            dataset_blend_test_j[:, i] = clf.predict_proba(X_submission)[:,1]
        dataset_blend_test[:,j] = dataset_blend_test_j.mean(1)

    print "Blending."
    clf = LogisticRegression()
    clf.fit(dataset_blend_train, y)
    y_submission = clf.predict_proba(dataset_blend_test)[:,1]

    print "Linear stretch of predictions to [0,1]"
    y_submission = (y_submission - y_submission.min()) / (y_submission.max() - y_submission.min())

    print "Saving Results."
    np.savetxt(fname='test.csv', X=y_submission, fmt='%0.9f')