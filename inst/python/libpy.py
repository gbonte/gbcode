#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np
import warnings 
warnings.filterwarnings("ignore")
  
#import pandas as pd
#import matplotlib.pyplot as plt
#from sklearn import preprocessing
#import sklearn.metrics as sm
#from sklearn import datasets
#from sklearn.metrics import mean_squared_error, explained_variance_score
#from sklearn.utils import shuffle
#from sklearn import datasets
#from sklearn import model_selection

#
#from sklearn.svm import LinearSVC
#from sklearn.ensemble import RandomForestClassifier
#from sklearn.feature_selection import VarianceThreshold
#from sklearn.decomposition import PCA


#yhat=np.zeros(int(r.Nts))+np.mean(r.pyY)

if r.pym==1:
  r.pyY=np.ravel(r.pyY)
## CLASSIFICATION 

if r.plearn=="sgd_class": 
  from sklearn.linear_model import SGDClassifier
  sgd_clf = SGDClassifier(random_state=42)
  sgd_clf.fit(r.pyX, r.pyY)
  yhat = sgd_clf.predict(r.pyXts)

 
if r.plearn=="rf_class":
  from sklearn.ensemble import RandomForestClassifier
  clf = RandomForestClassifier()
  clf.fit(r.pyX, r.pyY)
  yhat = clf.predict(r.pyXts) 
  phat = clf.predict_proba(r.pyXts) 
  
if r.plearn=="gb_class":
  from sklearn.ensemble import GradientBoostingClassifier
  gb_classifier = GradientBoostingClassifier()
  gb_classifier.fit(r.pyX, r.pyY)
  yhat = gb_classifier.predict(r.pyXts)
  phat = gb_classifier.predict_proba(r.pyXts) 
  
if r.plearn=="piperf_class":  
  from sklearn.pipeline import Pipeline
  from sklearn.feature_selection import SelectFromModel
  from sklearn.ensemble import RandomForestClassifier
  clf = Pipeline([
    ('feature_selection', SelectFromModel(RandomForestClassifier())),
    ('regression', RandomForestClassifier())
  ])
  clf.fit(r.pyX, r.pyY)
  yhat = clf.predict(r.pyXts)
  phat = clf.predict_proba(r.pyXts) 
  
   
## REGRESSION  
if r.plearn=="lasso_regr":  
  from sklearn.linear_model import LassoCV, MultiTaskLassoCV
  if r.pym==1:
    reg = LassoCV(cv=10, random_state=0).fit(r.pyX, r.pyY)
  else:
    reg = MultiTaskLassoCV(cv=5, random_state=0,max_iter=100,
    verbose=0).fit(r.pyX, r.pyY)
  yhat = reg.predict(r.pyXts)
  
if r.plearn=="lin_regr":
# Create linear regression object
  from sklearn import linear_model
  linear_regressor = linear_model.LinearRegression()
  linear_regressor.fit(r.pyX, r.pyY)
  yhat = linear_regressor.predict(r.pyXts)

if r.plearn=="rf_regr":
  from sklearn.ensemble import RandomForestRegressor
  #from sklearn.model_selection import GridSearchCV
  from sklearn.model_selection import RandomizedSearchCV

  max_depth = [int(x) for x in np.linspace(10, 110, num = 11)]
  max_depth.append(None)
  # Minimum number of samples required to split a node
  min_samples_split = [2, 5, 10]
  # Minimum number of samples required at each leaf node
  min_samples_leaf = [1, 2, 4]

  # Create the random grid
  random_grid = {'max_depth': max_depth,
               'min_samples_split': min_samples_split,
               'min_samples_leaf': min_samples_leaf}
  rf_r = RandomForestRegressor()
  rf_regressor = RandomizedSearchCV(estimator = rf_r, param_distributions = random_grid,
  n_iter = 50, cv = 3, verbose=0, random_state=42)
  rf_regressor.fit(r.pyX, r.pyY)
  yhat = rf_regressor.predict(r.pyXts)
 

if r.plearn=="gb_regr":
  from sklearn.ensemble import GradientBoostingRegressor
  gb_regressor = GradientBoostingRegressor()
  r.pyY.shape=(int(r.pyN),1)
  gb_regressor.fit(r.pyX, r.pyY)
  r.pyXts.shape=(int(r.pyNts),int(r.pyn))
  yhat = gb_regressor.predict(r.pyXts)
  
  
if r.plearn=="ab_regr":
  from sklearn.ensemble import AdaBoostRegressor
  ab_regressor = AdaBoostRegressor(DecisionTreeRegressor(max_depth=4), n_estimators=400, random_state=7)
  ab_regressor.fit(r.pyX, r.pyY)
  yhat = ab_regressor.predict(r.pyXts)
  


  
if r.plearn=="piperf_regr":  
  from sklearn.pipeline import Pipeline
  from sklearn.feature_selection import SelectFromModel
  from sklearn.ensemble import RandomForestRegressor
  clf = Pipeline([
    ('feature_selection', SelectFromModel(RandomForestRegressor())),
    ('regression', RandomForestRegressor())
  ])
  clf.fit(r.pyX, r.pyY)
  yhat = clf.predict(r.pyXts)
  

  
if r.plearn=="pipelin_regr":  
  clf = Pipeline([
    ('reduce_dim', PCA()),
    ('feature_selection', SelectFromModel(RandomForestRegressor())),
    ('regression', linear_model.LinearRegression())
  ])
  clf.fit(r.pyX, r.pyY)
  yhat = clf.predict(r.pyXts)
  
  
if r.plearn=="pipeab_regr":  
  clf = Pipeline([
    ('feature_selection', SelectFromModel(RandomForestRegressor())),
    ('regression', AdaBoostRegressor(n_estimators=500))
  ])
  clf.fit(r.pyX, r.pyY)
  yhat = clf.predict(r.pyXts)
  
