
import numpy as  np
from sklearn.metrics import log_loss
from sklearn.base import BaseEstimator
from scipy.optimize import minimize

def objf_ens_optA(w, Xs, y, n_class=12):
    """
    Function to be minimized in the EN_optA ensembler.
    
    Parameters:
    ----------
    w: array-like, shape=(n_preds)
       Candidate solution to the optimization problem (vector of weights).
    Xs: list of predictions to combine
       Each prediction is the solution of an individual classifier and has a
       shape=(n_samples, n_classes).
    y: array-like sahpe=(n_samples,)
       Class labels
    n_class: int
       Number of classes in the problem (12 in Airbnb competition)
    
    Return:
    ------
    score: Score of the candidate solution.
    """
    w = np.abs(w)
    sol = np.zeros(Xs[0].shape)
    for i in range(len(w)):
        sol += Xs[i] * w[i]
    #Using log-loss as objective function (different objective functions can be used here). 
    score = log_loss(y, sol)   
    return score
        

class EN_optA(BaseEstimator):
    """
    Given a set of predictions $X_1, X_2, ..., X_n$,  it computes the optimal set of weights
    $w_1, w_2, ..., w_n$; such that minimizes $log\_loss(y_T, y_E)$, 
    where $y_E = X_1*w_1 + X_2*w_2 +...+ X_n*w_n$ and $y_T$ is the true solution.
    """
    def __init__(self, n_class=12):
        super(EN_optA, self).__init__()
        self.n_class = n_class
        
    def fit(self, X, y):
        """
        Learn the optimal weights by solving an optimization problem.
        
        Parameters:
        ----------
        Xs: list of predictions to be ensembled
           Each prediction is the solution of an individual classifier and has 
           shape=(n_samples, n_classes).
        y: array-like
           Class labels
        """
        Xs = np.hsplit(X, X.shape[1]/self.n_class)
        #Initial solution has equal weight for all individual predictions.
        x0 = np.ones(len(Xs)) / float(len(Xs)) 
        #Weights must be bounded in [0, 1]
        bounds = [(0,1)]*len(x0)   
        #All weights must sum to 1
        cons = ({'type':'eq','fun':lambda w: 1-sum(w)})
        #Calling the solver
        res = minimize(objf_ens_optA, x0, args=(Xs, y, self.n_class), 
                       method='SLSQP', 
                       bounds=bounds,
                       constraints=cons
                       )
        self.w = res.x
        return self
    
    def predict_proba(self, X):
        """
        Use the weights learned in training to predict class probabilities.
        
        Parameters:
        ----------
        Xs: list of predictions to be blended.
            Each prediction is the solution of an individual classifier and has 
            shape=(n_samples, n_classes).
            
        Return:
        ------
        y_pred: array_like, shape=(n_samples, n_class)
                The blended prediction.
        """
        Xs = np.hsplit(X, X.shape[1]/self.n_class)
        y_pred = np.zeros(Xs[0].shape)
        for i in range(len(self.w)):
            y_pred += Xs[i] * self.w[i] 
        return y_pred  
        
        
        
        def objf_ens_optB(w, Xs, y, n_class=12):
    """
    Function to be minimized in the EN_optB ensembler.
    
    Parameters:
    ----------
    w: array-like, shape=(n_preds)
       Candidate solution to the optimization problem (vector of weights).
    Xs: list of predictions to combine
       Each prediction is the solution of an individual classifier and has a
       shape=(n_samples, n_classes).
    y: array-like sahpe=(n_samples,)
       Class labels
    n_class: int
       Number of classes in the problem, i.e. = 12
    
    Return:
    ------
    score: Score of the candidate solution.
    """
    #Constraining the weights for each class to sum up to 1.
    #This constraint can be defined in the scipy.minimize function, but doing 
    #it here gives more flexibility to the scipy.minimize function 
    #(e.g. more solvers are allowed).
    w_range = np.arange(len(w))%n_class 
    for i in range(n_class): 
        w[w_range==i] = w[w_range==i] / np.sum(w[w_range==i])
        
    sol = np.zeros(Xs[0].shape)
    for i in range(len(w)):
        sol[:, i % n_class] += Xs[int(i / n_class)][:, i % n_class] * w[i] 
        
    #Using log-loss as objective function (different objective functions can be used here). 
    score = log_loss(y, sol)   
    return score
    

class EN_optB(BaseEstimator):
    """
    Given a set of predictions $X_1, X_2, ..., X_n$, where each $X_i$ has
    $m=12$ clases, i.e. $X_i = X_{i1}, X_{i2},...,X_{im}$. The algorithm finds the optimal 
    set of weights $w_{11}, w_{12}, ..., w_{nm}$; such that minimizes 
    $log\_loss(y_T, y_E)$, where $y_E = X_{11}*w_{11} +... + X_{21}*w_{21} + ... 
    + X_{nm}*w_{nm}$ and and $y_T$ is the true solution.
    """
    def __init__(self, n_class=12):
        super(EN_optB, self).__init__()
        self.n_class = n_class
        
    def fit(self, X, y):
        """
        Learn the optimal weights by solving an optimization problem.
        
        Parameters:
        ----------
        Xs: list of predictions to be ensembled
           Each prediction is the solution of an individual classifier and has 
           shape=(n_samples, n_classes).
        y: array-like
           Class labels
        """
        Xs = np.hsplit(X, X.shape[1]/self.n_class)
        #Initial solution has equal weight for all individual predictions.
        x0 = np.ones(self.n_class * len(Xs)) / float(len(Xs)) 
        #Weights must be bounded in [0, 1]
        bounds = [(0,1)]*len(x0)   
        #Calling the solver (constraints are directly defined in the objective
        #function)
        res = minimize(objf_ens_optB, x0, args=(Xs, y, self.n_class), 
                       method='L-BFGS-B', 
                       bounds=bounds, 
                       )
        self.w = res.x
        return self
    
    def predict_proba(self, X):
        """
        Use the weights learned in training to predict class probabilities.
        
        Parameters:
        ----------
        Xs: list of predictions to be ensembled
            Each prediction is the solution of an individual classifier and has 
            shape=(n_samples, n_classes).
            
        Return:
        ------
        y_pred: array_like, shape=(n_samples, n_class)
                The ensembled prediction.
        """
        Xs = np.hsplit(X, X.shape[1]/self.n_class)
        y_pred = np.zeros(Xs[0].shape)
        for i in range(len(self.w)):
            y_pred[:, i % self.n_class] += \
                   Xs[int(i / self.n_class)][:, i % self.n_class] * self.w[i]  
        return y_pred  
        
        
        
        
        
        

from sklearn.datasets import make_classification
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier, ExtraTreesClassifier
from sklearn.svm import SVC
from sklearn.neighbors import KNeighborsClassifier
from sklearn.calibration import CalibratedClassifierCV
from sklearn.cross_validation import train_test_split
from sklearn.linear_model import LogisticRegressionCV
from xgboost.sklearn import XGBClassifier





n_classes = 12  # Same number of classes as in Airbnb competition.
data, labels = make_classification(n_samples=2000, n_features=100, 
                                   n_informative=50, n_classes=n_classes, 
                                   random_state=random_state)

#Spliting data into train and test sets.
X, X_test, y, y_test = train_test_split(data, labels, test_size=0.2, 
                                        random_state=random_state)
    
#Spliting train data into training and validation sets.
X_train, X_valid, y_train, y_valid = train_test_split(X, y, test_size=0.25, 
                                                      random_state=random_state)

print('Data shape:')
print('X_train: %s, X_valid: %s, X_test: %s \n' %(X_train.shape, X_valid.shape, 
                                                  X_test.shape))
    



#Defining the classifiers
clfs = {'LR'  : LogisticRegression(random_state=random_state), 
        'SVM' : SVC(probability=True, random_state=random_state), 
        'RF'  : RandomForestClassifier(n_estimators=100, n_jobs=-1, 
                                       random_state=random_state), 
        'GBM' : GradientBoostingClassifier(n_estimators=50, 
                                           random_state=random_state), 
        'ETC' : ExtraTreesClassifier(n_estimators=100, n_jobs=-1, 
                                     random_state=random_state),
        'KNN' : KNeighborsClassifier(n_neighbors=30)}
    
#predictions on the validation and test sets
p_valid = []
p_test = []
   
print('Performance of individual classifiers (1st layer) on X_test')   
print('------------------------------------------------------------')
   
for nm, clf in clfs.items():
    #First run. Training on (X_train, y_train) and predicting on X_valid.
    clf.fit(X_train, y_train)
    yv = clf.predict_proba(X_valid)
    p_valid.append(yv)
        
    #Second run. Training on (X, y) and predicting on X_test.
    clf.fit(X, y)
    yt = clf.predict_proba(X_test)
    p_test.append(yt)
       
    #Printing out the performance of the classifier
    print('{:10s} {:2s} {:1.7f}'.format('%s: ' %(nm), 'logloss  =>', log_loss(y_test, yt)))
print('')


print('Performance of optimization based ensemblers (2nd layer) on X_test')   
print('------------------------------------------------------------')
    
#Creating the data for the 2nd layer.
XV = np.hstack(p_valid)
XT = np.hstack(p_test)  
        
#EN_optA
enA = EN_optA(n_classes)
enA.fit(XV, y_valid)
w_enA = enA.w
y_enA = enA.predict_proba(XT)
print('{:20s} {:2s} {:1.7f}'.format('EN_optA:', 'logloss  =>', log_loss(y_test, y_enA)))
    
#Calibrated version of EN_optA 
cc_optA = CalibratedClassifierCV(enA, method='isotonic')
cc_optA.fit(XV, y_valid)
y_ccA = cc_optA.predict_proba(XT)
print('{:20s} {:2s} {:1.7f}'.format('Calibrated_EN_optA:', 'logloss  =>', log_loss(y_test, y_ccA)))
        
#EN_optB
enB = EN_optB(n_classes) 
enB.fit(XV, y_valid)
w_enB = enB.w
y_enB = enB.predict_proba(XT)
print('{:20s} {:2s} {:1.7f}'.format('EN_optB:', 'logloss  =>', log_loss(y_test, y_enB)))

#Calibrated version of EN_optB
cc_optB = CalibratedClassifierCV(enB, method='isotonic')
cc_optB.fit(XV, y_valid)
y_ccB = cc_optB.predict_proba(XT)  
print('{:20s} {:2s} {:1.7f}'.format('Calibrated_EN_optB:', 'logloss  =>', log_loss(y_test, y_ccB)))
print('')





y_3l = (y_enA * 4./9.) + (y_ccA * 2./9.) + (y_enB * 2./9.) + (y_ccB * 1./9.)
print('{:20s} {:2s} {:1.7f}'.format('3rd_layer:', 'logloss  =>', log_loss(y_test, y_3l)))




from tabulate import tabulate
print('               Weights of EN_optA:')
print('|---------------------------------------------|')
wA = np.round(w_enA, decimals=2).reshape(1,-1)
print(tabulate(wA, headers=clfs.keys(), tablefmt="orgtbl"))
print('')
print('                                    Weights of EN_optB:')
print('|-------------------------------------------------------------------------------------------|')
wB = np.round(w_enB.reshape((-1,n_classes)), decimals=2)
wB = np.hstack((np.array(list(clfs.keys()), dtype=str).reshape(-1,1), wB))
print(tabulate(wB, headers=['y%s'%(i) for i in range(n_classes)], tablefmt="orgtbl"))


#By default the best C parameter is obtained with a cross-validation approach, doing grid search with
#10 values defined in a logarithmic scale between 1e-4 and 1e4.
#Change parameters to see how they affect the final results.
lr = LogisticRegressionCV(Cs=10, dual=False, fit_intercept=True, 
                          intercept_scaling=1.0, max_iter=100,
                          multi_class='ovr', n_jobs=1, penalty='l2', 
                          random_state=random_state,
                          solver='lbfgs', tol=0.0001)

lr.fit(XV, y_valid)
y_lr = lr.predict_proba(XT)
print('{:20s} {:2s} {:1.7f}'.format('Log_Reg:', 'logloss  =>', log_loss(y_test, y_lr)))



#For each value in classes, a dataset with that number of classes will be created. 
classes = range(3, 15)

ll_sc = []  #to store logloss of individual classifiers
ll_eA = []  #to store logloss of EN_optA ensembler
ll_eB = []  #to store logloss of EN_optB ensembler
ll_e3 = []  #to store logloss of the third-layer ensembler (method used for submission in the competition).
ll_lr = []  #to store logloss of LogisticRegression as 2nd layer ensembler.
ll_gb = []  #to store logloss of GradientBoosting as 2nd layer ensembler.

#Same code as above for generating the dataset, applying the 3-layer learning architecture and copmparing with  
#LogisticRegression and GradientBoosting based ensembles. 
#The code is applied to each independent problem/dataset (each dataset with a different number of classes).
for i in classes:
    print('Working on dataset with n_classes: %s' %(i))
    n_classes=i
    
    #Generating the data
    data, labels = make_classification(n_samples=2000, n_features=100, 
                                       n_informative=50, n_classes=n_classes,
                                       random_state=random_state)
    X, X_test, y, y_test = train_test_split(data, labels, test_size=0.2, 
                                            random_state=random_state)
    X_train, X_valid, y_train, y_valid = train_test_split(X, y, 
                                              test_size=0.25, 
                                              random_state=random_state)
    
    #First layer
    clfs = [LogisticRegression(random_state=random_state), 
            SVC(probability=True, random_state=random_state), 
            RandomForestClassifier(n_estimators=100, n_jobs=-1, 
                                   random_state=random_state), 
            GradientBoostingClassifier(n_estimators=50, 
                                       random_state=random_state), 
            ExtraTreesClassifier(n_estimators=100, n_jobs=-1, 
                                 random_state=random_state), 
            KNeighborsClassifier(n_neighbors=30, n_jobs=-1)]
    p_valid = []
    p_test = []
    for clf in clfs:
        #First run
        clf.fit(X_train, y_train)
        yv = clf.predict_proba(X_valid)
        p_valid.append(yv)
        #Second run
        clf.fit(X, y)
        yt = clf.predict_proba(X_test)
        p_test.append(yt)
        #Saving the logloss score
        ll_sc.append(log_loss(y_test, yt))

    #Second layer
    XV = np.hstack(p_valid)
    XT = np.hstack(p_test)  
    
    enA = EN_optA(n_classes)   #EN_optA
    enA.fit(XV, y_valid)
    y_enA = enA.predict_proba(XT)    
    ll_eA.append(log_loss(y_test, y_enA))  #Saving the logloss score
    
    cc_optA = CalibratedClassifierCV(enA, method='isotonic') #Calibrated version of EN_optA 
    cc_optA.fit(XV, y_valid)
    y_ccA = cc_optA.predict_proba(XT)
    
    enB = EN_optB(n_classes)   #EN_optB
    enB.fit(XV, y_valid)
    y_enB = enB.predict_proba(XT)   #Saving the logloss score
    ll_eB.append(log_loss(y_test, y_enB))
    
    cc_optB = CalibratedClassifierCV(enB, method='isotonic') #Calibrated version of EN_optB 
    cc_optB.fit(XV, y_valid)
    y_ccB = cc_optB.predict_proba(XT) 
    
    #Third layer
    y_3l = (y_enA * 4./9.) + (y_ccA * 2./9.) + (y_enB * 2./9.) + (y_ccB * 1./9.)
    ll_e3.append(log_loss(y_test, y_3l))   #Saving the logloss score
    
    #Logistic regresson
    lr = LogisticRegressionCV(Cs=10, dual=False, fit_intercept=True,
                              intercept_scaling=1.0, max_iter=100,
                              multi_class='ovr', n_jobs=1, penalty='l2',
                              random_state=random_state,
                              solver='lbfgs', tol=0.0001)
    lr.fit(XV, y_valid)
    y_lr = lr.predict_proba(XT)
    ll_lr.append(log_loss(y_test, y_lr))   #Saving the logloss score
    
    #Gradient boosting
    xgb = XGBClassifier(max_depth=5, learning_rate=0.1, 
                        n_estimators=10000, objective='multi:softprob', 
                        seed=random_state)
    #Computing best number of iterations on an internal validation set
    XV_train, XV_valid, yv_train, yv_valid = train_test_split(XV, y_valid,
                                             test_size=0.15, random_state=random_state)
    xgb.fit(XV_train, yv_train, eval_set=[(XV_valid, yv_valid)], 
            eval_metric='mlogloss', 
            early_stopping_rounds=15, verbose=False)
    xgb.n_estimators = xgb.best_iteration
    
    xgb.fit(XV, y_valid)
    y_gb = xgb.predict_proba(XT)
    ll_gb.append(log_loss(y_test, y_gb)) #Saving the logloss score 

ll_sc = np.array(ll_sc).reshape(-1, len(clfs)).T 
ll_eA = np.array(ll_eA) 
ll_eB = np.array(ll_eB) 
ll_e3 = np.array(ll_e3)
ll_lr = np.array(ll_lr) 
ll_gb = np.array(ll_gb)


import matplotlib.pylab as plt
plt.figure(figsize=(10,7))

plt.plot(classes, ll_sc[0], color='black', label='Single_Classifiers')
for i in range(1, 6):
    plt.plot(classes, ll_sc[i], color='black')
    
plt.plot(classes, ll_lr, 'bo-', label='EN_LogisticRegression')
plt.plot(classes, ll_gb, 'mo-', label='EN_XGBoost')
plt.plot(classes, ll_eA, 'yo-', label='EN_optA')
plt.plot(classes, ll_eB, 'go-', label='EN_optB')
plt.plot(classes, ll_e3, 'ro-', label='EN_3rd_layer')

plt.title('Log-loss of the different models for different number of classes.')
plt.xlabel('Number of classes')
plt.ylabel('Log-loss')
plt.grid(True)
plt.legend(loc=4)
plt.show()

