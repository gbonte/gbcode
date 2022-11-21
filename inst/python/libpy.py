#!/usr/bin/env python
# coding: utf-8

# In[1]:


#import numpy as np
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


#yhat=np.zeros(int(r.Nts))+np.mean(r.Y)

print(r.plearn)

if r.plearn=="sgd_class":
  from sklearn.linear_model import SGDClassifier
  sgd_clf = SGDClassifier(random_state=42)
  sgd_clf.fit(r.X, r.Y)
  yhat = sgd_clf.predict(r.Xts)

if r.plearn=="lin_regr":
# Create linear regression object
  from sklearn import linear_model
  linear_regressor = linear_model.LinearRegression()
  linear_regressor.fit(r.X, r.Y)
  yhat = linear_regressor.predict(r.Xts)

if r.plearn=="rf_regr":
  from sklearn.ensemble import RandomForestRegressor
  rf_regressor = RandomForestRegressor()
  rf_regressor.fit(r.X, r.Y)
  yhat = rf_regressor.predict(r.Xts)
 
if r.plearn=="rf_class":
  from sklearn.ensemble import RandomForestClassifier
  clf = RandomForestClassifier()
  clf.fit(r.X, r.Y)
  yhat = clf.predict(r.Xts) 
  
if r.plearn=="ab":
  from sklearn.ensemble import AdaBoostRegressor
  ab_regressor = AdaBoostRegressor(DecisionTreeRegressor(max_depth=4), n_estimators=400, random_state=7)
  ab_regressor.fit(r.X, r.Y)
  yhat = ab_regressor.predict(r.Xts)
  
if r.plearn=="piperf_regr":  
  from sklearn.pipeline import Pipeline
  from sklearn.feature_selection import SelectFromModel
  from sklearn.ensemble import RandomForestRegressor
  clf = Pipeline([
    ('feature_selection', SelectFromModel(RandomForestRegressor())),
    ('regression', RandomForestRegressor())
  ])
  clf.fit(r.X, r.Y)
  yhat = clf.predict(r.Xts)
  
if r.plearn=="pipelin":  
  clf = Pipeline([
    ('reduce_dim', PCA()),
    ('feature_selection', SelectFromModel(RandomForestRegressor())),
    ('regression', linear_model.LinearRegression())
  ])
  clf.fit(r.X, r.Y)
  yhat = clf.predict(r.Xts)
  
  
if r.plearn=="pipeab":  
  clf = Pipeline([
    ('feature_selection', SelectFromModel(RandomForestRegressor())),
    ('regression', AdaBoostRegressor(n_estimators=500))
  ])
  clf.fit(r.X, r.Y)
  yhat = clf.predict(r.Xts)
  
  
