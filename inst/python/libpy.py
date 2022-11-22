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

if r.plearn=="keras_regr0":
  from tensorflow import keras 
  from tensorflow.keras import layers
  def build_model():
    model = keras.Sequential([              
        layers.Dense(4, activation="relu"),
        layers.Dropout(0.95),
        layers.Dense(2, activation="relu"),
        layers.Dense(r.pym)
    ])
    model.compile(optimizer="rmsprop", loss="mse", metrics=["mae"])
    return model
  model = build_model()                                
  model.fit(r.pyX, r.pyY,                 
          epochs=500, batch_size=10, verbose=0, validation_split=0.2)
  
  yhat=model.predict(r.pyXts)
  
if r.plearn=="keras_regr":
  from tensorflow import keras 
  from tensorflow.keras import layers
  import keras_tuner as kt
  import tensorflow as tf
  def model_builder(hp):
    model = keras.Sequential()
    # Tune the number of units in the first Dense layer
    # Choose an optimal value between 32-512
    hp_units = hp.Int('units', min_value=2, max_value=20, step=1)
    model.add(keras.layers.Dense(units=hp_units, activation='relu'))
    hp_units2 = hp.Int('units', min_value=2, max_value=10, step=2)
    model.add(keras.layers.Dense(units=hp_units2, activation='relu'))
    model.add(keras.layers.Dense(r.pym))

    model.compile(optimizer="rmsprop",
                loss="mse",
                metrics=['accuracy'])

    return model

  tuner = kt.Hyperband(model_builder,
                     objective='val_accuracy',
                     max_epochs=10,
                     factor=3)
                     
  stop_early = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=5)
  tuner.search(r.pyX, r.pyY,
    epochs=50, validation_split=0.2, callbacks=[stop_early],verbose=0)

  # Get the optimal hyperparameters
  best_hps=tuner.get_best_hyperparameters(num_trials=1)[0]
  hypermodel = tuner.hypermodel.build(best_hps)
  hypermodel.fit(r.pyX, r.pyY,epochs=500, batch_size=10, validation_split=0.2, 
    verbose=0)
                                
  yhat=hypermodel.predict(r.pyXts)
  
  
  
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
  random_grid = {'max_depth': max_depth}
  #             'min_samples_split': min_samples_split,
  #               'min_samples_leaf': min_samples_leaf}
  rf_r = RandomForestRegressor()
  #rf_regressor = RandomizedSearchCV(estimator = rf_r, param_distributions = random_grid,
  #n_iter = 5, cv = 3, verbose=0, random_state=42)
  rf_regressor =rf_r
  rf_regressor.fit(r.pyX, r.pyY)
  yhat = rf_regressor.predict(r.pyXts)
 
if r.plearn=="knn_regr":
  from sklearn.neighbors import KNeighborsRegressor
  from sklearn.feature_selection import SequentialFeatureSelector
  from sklearn.model_selection import RandomizedSearchCV

  # Create the random grid
  random_grid = {'n_neighbors': [int(x) for x in np.linspace(1, 20, num = 10)],
                'weights':['uniform', 'distance']}
  knn_r = KNeighborsRegressor(n_neighbors=3)
  sfs = SequentialFeatureSelector(knn_r, n_features_to_select='auto')
  knn_regressor = RandomizedSearchCV(estimator = knn_r, param_distributions = random_grid,
  n_iter = 50, cv = 3, verbose=0, random_state=42)
  sfs.fit(r.pyX,r.pyY)
  X_train_sfs = sfs.transform(r.pyX)
  X_test_sfs = sfs.transform(r.pyXts)
  knn_regressor.fit(X_train_sfs, r.pyY)
  yhat = knn_regressor.predict(X_test_sfs)
  
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
  
if r.plearn=="pipeknn_regr":  
  from sklearn.pipeline import Pipeline
  from sklearn.feature_selection import SelectFromModel
  from sklearn.neighbors import KNeighborsRegressor
  clf = Pipeline([
    ('feature_selection', SelectFromModel(RandomForestRegressor())),
    ('regression', KNeighborsRegressor())
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
  
