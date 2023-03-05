## python implementation of learning models used by mlearn




import numpy as np
import warnings 
import sys
warnings.filterwarnings("ignore")

try:
  import sklearn
except ImportError as e:
  print(e)
  print("Missing python package sklearn. Install it first using py_install") 
  sys.exit(1)  

try:
  import tensorflow
except ImportError as e:
  print(e)
  print("Missing package tensorflow. Install it first using py_install ") 
  sys.exit(1)
  
try:
  import tensorflow.keras
except ImportError as e:
  print(e)
  print("Missing package tensorflow.keras. Install it first using py_install ") 
  sys.exit(1)  
  
try:
  import keras_tuner
except ImportError as e:
  print(e)
  print("Missing package keras_tuner. Install it first using py_install ") 
  sys.exit(1)
  
yhat=[]
if r.pym==1:
  r.pyY=np.ravel(r.pyY)
  
#############################
## CLASSIFICATION 
#############################


if r.plearn=="sgd_class": 
  from sklearn.linear_model import SGDClassifier
  sgd_clf = SGDClassifier(loss='log',random_state=42)
  sgd_clf.fit(r.pyX, r.pyY)
  yhat = sgd_clf.predict(r.pyXts)
  phat = sgd_clf.predict_proba(r.pyXts) 
 

  
if r.plearn=="nb_class":
  from sklearn.naive_bayes import GaussianNB
  gnb = GaussianNB()
  gnb.fit(r.pyX, r.pyY)
  yhat = gnb.predict(r.pyXts) 
  phat = gnb.predict_proba(r.pyXts)   
  
if r.plearn=="knn_class":
  from sklearn.neighbors import KNeighborsClassifier
  from sklearn.model_selection import RandomizedSearchCV

  # Create the random grid
  random_grid = {'n_neighbors': [int(x) for x in np.linspace(1, 20, num = 10)],
                'weights':['uniform', 'distance']}
  neigh_r = KNeighborsClassifier(n_neighbors=3)
  neigh = RandomizedSearchCV(estimator = neigh_r, param_distributions = random_grid,
  n_iter = 50, cv = 3, verbose=0, random_state=42)
  neigh.fit(r.pyX, r.pyY)
  yhat = neigh.predict(r.pyXts) 
  phat = neigh.predict_proba(r.pyXts) 
  
if r.plearn=="rf_class":
  from sklearn.ensemble import RandomForestClassifier
  clf = RandomForestClassifier(max_depth=2, random_state=0)
  clf.fit(r.pyX, r.pyY)
  yhat = clf.predict(r.pyXts) 
  phat = clf.predict_proba(r.pyXts) 
  
if r.plearn=="ab_class":
  from sklearn.ensemble import AdaBoostClassifier
  clf = AdaBoostClassifier(n_estimators=100, random_state=0)
  clf.fit(r.pyX, r.pyY)
  yhat = clf.predict(r.pyXts) 
  phat = clf.predict_proba(r.pyXts) 
  
if r.plearn=="svm_class":
  from sklearn.svm import SVC
  clf = SVC(gamma='auto',probability=True)
  clf.fit(r.pyX, r.pyY)
  yhat = clf.predict(r.pyXts) 
  phat = clf.predict_proba(r.pyXts) 
  
if r.plearn=="lsvm_class":
  from sklearn.svm import SVC
  clf = SVC(kernel="linear", C=0.025,probability=True)
  clf.fit(r.pyX, r.pyY)
  yhat = clf.predict(r.pyXts) 
  phat = clf.predict_proba(r.pyXts)
  
if r.plearn=="gp_class":
  from sklearn.gaussian_process import GaussianProcessClassifier
  from sklearn.gaussian_process.kernels import RBF
  kernel = 1.0 * RBF(1.0)
  gpc = GaussianProcessClassifier(kernel=kernel,
       random_state=0)
  gpc.fit(r.pyX, r.pyY)
  yhat = gpc.predict(r.pyXts) 
  phat = gpc.predict_proba(r.pyXts)
  
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
  # Based on Keras Tuner
  ## https://keras.io/keras_tuner/
  ##  https://www.tensorflow.org/tutorials/keras/keras_tuner
  from tensorflow import keras 
  from tensorflow.keras import layers
  import keras_tuner as kt
  import tensorflow as tf
  def model_builder(hp):
    model = keras.Sequential()
    # Tune the number of units in the first Dense layer
    # Choose an optimal value between 32-512
    hp_units = hp.Int('units', min_value=1, max_value=20, step=1)
    model.add(keras.layers.Dense(units=hp_units, activation='relu'))
    hp_units2 = hp.Int('units2', min_value=2, max_value=10, step=2)
    model.add(keras.layers.Dense(units=hp_units2, activation='relu'))
    hp_droprate = hp.Choice('droprate', values=[0.1, 0.5, 0.7, 0.9])
    model.add(keras.layers.Dropout(hp_droprate))
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
  model = tuner.hypermodel.build(best_hps)
  history =model.fit(r.pyX, r.pyY,epochs=500, batch_size=10, 
    validation_split=0.25, verbose=0)
 
  #val_acc_per_epoch = history.history['val_accuracy']
  #best_epoch = val_acc_per_epoch.index(max(val_acc_per_epoch)) + 1                              
  #hypermodel = tuner.hypermodel.build(best_hps)
  # Retrain the model
  #hypermodel.fit(r.pyX, r.pyY, epochs=best_epoch, validation_split=0.2,verbose=0)
  yhat=model.predict(r.pyXts)
  
  
if r.plearn=="pls_regr":
  from sklearn.cross_decomposition import PLSRegression
  from sklearn.model_selection import RandomizedSearchCV
  random_grid = {'n_components': [int(x) for x in np.linspace(1, 20, num = 15)]}
  reg = PLSRegression()
    
  pls_regressor = RandomizedSearchCV(estimator = reg, param_distributions = random_grid,
  n_iter = 70, cv = 3, verbose=0, random_state=42)
  
  pls_regressor.fit(r.pyX, r.pyY)
  yhat = pls_regressor.predict(r.pyXts)
  
if r.plearn=="ridge_regr":
  from sklearn.linear_model import RidgeCV
  
  reg = RidgeCV(alphas=np.linspace(0.1, 100, num = 50))
  reg.fit(r.pyX, r.pyY)
  yhat = reg.predict(r.pyXts)
  
  
if r.plearn=="lasso_regr":  
  from sklearn.linear_model import LassoCV, MultiTaskLassoCV
  if r.pym==1:
    reg = LassoCV(cv=10, random_state=0).fit(r.pyX, r.pyY)
  else:
    reg = MultiTaskLassoCV(cv=2, random_state=0,max_iter=10,
    verbose=0).fit(r.pyX, r.pyY)
  yhat = reg.predict(r.pyXts)
  
if r.plearn=="enet_regr":  
  from sklearn.linear_model import ElasticNet, MultiTaskElasticNet
  from sklearn.model_selection import RandomizedSearchCV
  
  if r.pym==1:
    reg = ElasticNet(random_state=0).fit(r.pyX, r.pyY)
  else:
    random_grid = {'alpha': [int(x) for x in np.linspace(0.1, 2, num = 10)]}
    reg =MultiTaskElasticNet()
    reg = RandomizedSearchCV(estimator = reg, param_distributions = random_grid,
    n_iter = 50, cv = 3, verbose=0, random_state=42)
    reg.fit(r.pyX, r.pyY)
    
  yhat = reg.predict(r.pyXts)
  
  
  
if r.plearn=="lin_regr":
  # Create linear regression object
  from sklearn import linear_model
  linear_regressor = linear_model.LinearRegression()
  linear_regressor.fit(r.pyX, r.pyY)
  yhat = linear_regressor.predict(r.pyXts)

if r.plearn=="rf_regr0":
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
  rf_regressor = RandomizedSearchCV(estimator = rf_r, param_distributions = random_grid,
    n_iter = 5, cv = 3, verbose=0, random_state=42)
  rf_regressor =rf_r
  rf_regressor.fit(r.pyX, r.pyY)
  yhat = rf_regressor.predict(r.pyXts)

if r.plearn=="rf_regr":
  from sklearn.ensemble import RandomForestRegressor
  #from sklearn.model_selection import GridSearchCV
  from sklearn.model_selection import RandomizedSearchCV
  rf_r = RandomForestRegressor()
  #if r.pym>1:
  #  from sklearn.multioutput import RegressorChain
  #  rf_r = RegressorChain(base_estimator=rf_r, order='random')
  rf_regressor =rf_r
  rf_regressor.fit(r.pyX, r.pyY)
  yhat = rf_regressor.predict(r.pyXts)
 
if r.plearn=="knn_regr":
  from sklearn.neighbors import KNeighborsRegressor
  from sklearn.model_selection import RandomizedSearchCV

  # Create the random grid
  random_grid = {'n_neighbors': [int(x) for x in np.linspace(3, 20, num = 10)],
                'weights':['uniform', 'distance']}
  knn_r = KNeighborsRegressor()
    
  knn_regressor = RandomizedSearchCV(estimator = knn_r, param_distributions = random_grid,
  n_iter = 50, cv = 3, verbose=0, random_state=42)
  knn_regressor.fit(r.pyX, r.pyY)
  yhat = knn_regressor.predict(r.pyXts)
  
if r.plearn=="gb_regr":
  from sklearn.ensemble import GradientBoostingRegressor
  from sklearn.model_selection import RandomizedSearchCV
  if r.pym>1:
    #from sklearn.multioutput import MultiOutputRegressor
    #gb_regressor = MultiOutputRegressor(GradientBoostingRegressor(n_estimators=5))
    from sklearn.multioutput import RegressorChain
    gb_regressor = RegressorChain(base_estimator=GradientBoostingRegressor(), order='random')
    random_grid = {'base_estimator__n_estimators': [int(x) for x in np.linspace(1, 20, num = 5)]}
  else:
    gb_regressor = GradientBoostingRegressor()
    random_grid = {'n_estimators': [int(x) for x in np.linspace(1, 20, num = 5)]}
  
  gb_regressor = RandomizedSearchCV(estimator = gb_regressor, param_distributions = random_grid,
  n_iter = 20, cv = 2, verbose=0, random_state=42)
  
  gb_regressor.fit(r.pyX, r.pyY)
  
  yhat = gb_regressor.predict(r.pyXts)
 
  
if r.plearn=="ab_regr":
  from sklearn.ensemble import AdaBoostRegressor
  from sklearn.tree import DecisionTreeRegressor
  if r.pym>1:
    from sklearn.multioutput import MultiOutputRegressor
    ab_regressor = MultiOutputRegressor(AdaBoostRegressor(DecisionTreeRegressor(max_depth=4), 
      n_estimators=400, random_state=7))
    random_grid = {'estimator__base_estimator__max_depth': [int(x) for x in np.linspace(1, 10, num = 5)]}
  else:
    ab_regressor = AdaBoostRegressor(DecisionTreeRegressor(max_depth=4), n_estimators=400, random_state=7)
    random_grid = {'base_estimator__max_depth': [int(x) for x in np.linspace(1, 10, num = 5)]}
  
  ab_regressor = RandomizedSearchCV(estimator = ab_regressor, param_distributions = random_grid,
  n_iter = 20, cv = 2, verbose=0, random_state=42)
 
  ab_regressor.fit(r.pyX, r.pyY)
  yhat = ab_regressor.predict(r.pyXts)
  


  
if r.plearn=="piperf_regr":  
  from sklearn.pipeline import Pipeline
  from sklearn.feature_selection import SelectFromModel
  from sklearn.ensemble import RandomForestRegressor
  from sklearn.multioutput import RegressorChain
  #if r.pym>1:
  #  clf = Pipeline([
  #  ('feature_selection', SelectFromModel(RandomForestRegressor())),
  #  ('regression', RegressorChain(base_estimator=RandomForestRegressor(), order='random'))
  #  ])
  #else:
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
  from sklearn.ensemble import RandomForestRegressor
  from sklearn.model_selection import RandomizedSearchCV
  knn_r = Pipeline([
    ('feature_selection', SelectFromModel(RandomForestRegressor())),
    ('regression', KNeighborsRegressor())
  ])
  random_grid = {'feature_selection__max_features': [int(x) for x in np.linspace(1, 10, num = 5)],
  'regression__n_neighbors': [int(x) for x in np.linspace(1, 20, num = 5)]}
  knn_regressor = RandomizedSearchCV(estimator = knn_r, param_distributions = random_grid,
  n_iter = 20, cv = 2, verbose=0, random_state=42)
  knn_regressor.fit(r.pyX, r.pyY)
  yhat = knn_regressor.predict(r.pyXts)
  
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

  yhat.shape=(int(r.pyNts),int(r.pym))
  
  
  
############################  
## TIMESERIES

 
if r.plearn=="lstm_gpt":
  import numpy as np
  import pandas as pd
  from keras.layers import Dense, LSTM, Dropout
  from keras.models import Sequential
  from sklearn.preprocessing import StandardScaler

  # Create a function that converts an array of data into a dataset for training or testing
  def create_dataset(data, look_back=1, look_ahead=1):
    data_X, data_Y = [], []
    for i in range(len(data) - look_back - look_ahead + 1):
        a = data[i:(i + look_back), :]
        data_X.append(a)
        data_Y.append(data[i + look_back:i + look_back + look_ahead, :])
    return np.array(data_X), np.array(data_Y)


  def create_query(data, look_back=1):    
    query=[]   
    query.append(data[(len(data)-look_back):len(data), :])
    return  query
  
  # Scale the time series data
  scaler = StandardScaler()
  scaled_data = scaler.fit_transform(r.pyTS)

  m=int(r.pym)
  look_back = int(r.pyn)
  look_ahead = int(r.pyH)
  
  # Create training and testing datasets
  
  train_X, train_Y = create_dataset(scaled_data, look_back=look_back, look_ahead=look_ahead)

  # Reshape the input data for use with a LSTM model
  train_X = np.reshape(train_X, (train_X.shape[0], train_X.shape[1], train_X.shape[2]))
  train_Y = np.reshape(train_Y, (train_Y.shape[0], train_Y.shape[1]* train_Y.shape[2]))
  # Create a LSTM model
  model = Sequential()
  model.add(LSTM(units=int(r.pynunits), input_shape=(look_back, m)))
  model.add(Dense(look_ahead*m))
  model.compile(loss='mean_squared_error', optimizer='adam')

  # Train the model on the training data.  

  model.fit(train_X, train_Y, epochs=int(r.pynepochs), batch_size=50, verbose=0)

  ## forecasting
  
  q=create_query(scaled_data, look_back=look_back)
  q = np.reshape(q, (1, train_X.shape[1], train_X.shape[2]))
  fore = model.predict(q,verbose=0)
  
  fore=np.reshape(fore, (1, int(r.pyH), int(r.pym)))
  fore=scaler.inverse_transform(fore[0,:,:])
  yhat=fore



if r.plearn=="lstm_gpt_hyper":
  import numpy as np
  import pandas as pd
  import keras_tuner as kt
  from keras.layers import Dense, LSTM, Dropout
  from keras.models import Sequential
  from sklearn.preprocessing import StandardScaler
  import tensorflow as tf
  
  # Create a function that converts an array of data into a dataset for training or testing
  def create_dataset(data, look_back=1, look_ahead=1):
    data_X, data_Y = [], []
    for i in range(len(data) - look_back - look_ahead + 1):
        a = data[i:(i + look_back), :]
        data_X.append(a)
        data_Y.append(data[i + look_back:i + look_back + look_ahead, :])
    return np.array(data_X), np.array(data_Y)


  def create_query(data, look_back=1):    
    query=[]   
    query.append(data[(len(data)-look_back):len(data), :])
    return  query
  
  def model_builder(hp):
    model = Sequential()
    nunits = hp.Int('units', min_value=20, max_value=200, step=10)
    model.add(LSTM(units=nunits, input_shape=(look_back, m)))
    hp_droprate = hp.Choice('droprate', values=[0.1, 0.5, 0.7, 0.9])
    model.add(Dropout(hp_droprate))
    model.add(Dense(look_ahead*m))
    model.compile(loss='mean_squared_error', optimizer='adam')
    return model
  
  
  # Scale the time series data
  scaler = StandardScaler()
  scaled_data = scaler.fit_transform(r.pyTS)

  m=int(r.pym)
  look_back = int(r.pyn)
  look_ahead = int(r.pyH)
  
  # Create training and testing datasets
  
  train_X, train_Y = create_dataset(scaled_data, look_back=look_back, look_ahead=look_ahead)

  # Reshape the input data for use with a LSTM model
  train_X = np.reshape(train_X, (train_X.shape[0], train_X.shape[1], train_X.shape[2]))
  train_Y = np.reshape(train_Y, (train_Y.shape[0], train_Y.shape[1]* train_Y.shape[2]))
  # Create a LSTM model
  
  # Train the model on the training data.  
  tuner = kt.Hyperband(model_builder,
                     objective='val_loss',
                     max_epochs=10,
                     factor=3)
  stop_early = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=5)
  tuner.search(train_X, train_Y,
    epochs=int(r.pynepochs), validation_split=0.2, callbacks=[stop_early],verbose=0)

  # Get the optimal hyperparameters
  best_hps=tuner.get_best_hyperparameters(num_trials=1)[0]
  model = tuner.hypermodel.build(best_hps)

  model.fit(train_X, train_Y, epochs=int(r.pynepochs), batch_size=50, verbose=0)

  ## forecasting
  
  q=create_query(scaled_data, look_back=look_back)
  q = np.reshape(q, (1, train_X.shape[1], train_X.shape[2]))
  fore = model.predict(q,verbose=0)
  
  fore=np.reshape(fore, (1, int(r.pyH), int(r.pym)))
  fore=scaler.inverse_transform(fore[0,:,:])
  yhat=fore


if r.plearn=="rnn_gpt":
  import numpy as np
  import pandas as pd
  from keras.layers import Dense,  SimpleRNN
  from keras.models import Sequential
  from sklearn.preprocessing import StandardScaler

  # Create a function that converts an array of data into a dataset for training or testing
  def create_dataset(data, look_back=1, look_ahead=1):
    data_X, data_Y = [], []
    for i in range(len(data) - look_back - look_ahead + 1):
        a = data[i:(i + look_back), :]
        data_X.append(a)
        data_Y.append(data[i + look_back:i + look_back + look_ahead, :])
    return np.array(data_X), np.array(data_Y)  


  def create_query(data, look_back=1):    
    query=[]   
    query.append(data[(len(data)-look_back):len(data), :])
    return  query
  
  # Scale the time series data
  scaler = StandardScaler()
  scaled_data = scaler.fit_transform(r.pyTS)

  m=int(r.pym)
  look_back = int(r.pyn)
  look_ahead = int(r.pyH)
  
  # Create training and testing datasets
  
  train_X, train_Y = create_dataset(scaled_data, look_back=look_back, look_ahead=look_ahead)

  # Reshape the input data for use with a LSTM model
  train_X = np.reshape(train_X, (train_X.shape[0], train_X.shape[1], train_X.shape[2]))
  train_Y = np.reshape(train_Y, (train_Y.shape[0], train_Y.shape[1]* train_Y.shape[2]))
  # Create a LSTM model
  model = Sequential()
  model.add(SimpleRNN(units=int(r.pynunits), input_shape=(look_back, m)))
  model.add(Dense(look_ahead*m))
  model.compile(loss='mean_squared_error', optimizer='adam')

  # Train the model on the training data
  model.fit(train_X, train_Y, epochs=int(r.pynepochs), batch_size=50, verbose=0)

  ## forecasting
  
  q=create_query(scaled_data, look_back=look_back)
  q = np.reshape(q, (1, train_X.shape[1], train_X.shape[2]))
  fore = model.predict(q, verbose=0)
  
  fore=np.reshape(fore, (1, int(r.pyH), int(r.pym)))
  fore=scaler.inverse_transform(fore[0,:,:])
  yhat=fore


if r.plearn=="rnn_gpt_hyper":
  import numpy as np
  import pandas as pd
  import keras_tuner as kt
  from keras.layers import Dense, SimpleRNN, Dropout
  from keras.models import Sequential
  from sklearn.preprocessing import StandardScaler
  import tensorflow as tf
  
  # Create a function that converts an array of data into a dataset for training or testing
  def create_dataset(data, look_back=1, look_ahead=1):
    data_X, data_Y = [], []
    for i in range(len(data) - look_back - look_ahead + 1):
        a = data[i:(i + look_back), :]
        data_X.append(a)
        data_Y.append(data[i + look_back:i + look_back + look_ahead, :])
    return np.array(data_X), np.array(data_Y)


  def create_query(data, look_back=1):    
    query=[]   
    query.append(data[(len(data)-look_back):len(data), :])
    return  query
  
  def model_builder(hp):
    model = Sequential()
    nunits = hp.Int('units', min_value=20, max_value=200, step=10)
    model.add(SimpleRNN(units=nunits, input_shape=(look_back, m)))
    hp_droprate = hp.Choice('droprate', values=[0.1, 0.5, 0.7, 0.9])
    model.add(Dropout(hp_droprate))
    model.add(Dense(look_ahead*m))
    model.compile(loss='mean_squared_error', optimizer='adam')
    return model


  
  
  # Scale the time series data
  scaler = StandardScaler()
  scaled_data = scaler.fit_transform(r.pyTS)

  m=int(r.pym)
  look_back = int(r.pyn)
  look_ahead = int(r.pyH)
  
  # Create training and testing datasets
  
  train_X, train_Y = create_dataset(scaled_data, look_back=look_back, look_ahead=look_ahead)

  # Reshape the input data for use with a LSTM model
  train_X = np.reshape(train_X, (train_X.shape[0], train_X.shape[1], train_X.shape[2]))
  train_Y = np.reshape(train_Y, (train_Y.shape[0], train_Y.shape[1]* train_Y.shape[2]))
  # Create a LSTM model
  
  # Train the model on the training data.  
  tuner = kt.Hyperband(model_builder,
                     objective='val_loss',
                     max_epochs=10,
                     factor=3)
  stop_early = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=5)
  tuner.search(train_X, train_Y,
    epochs=int(r.pynepochs), validation_split=0.2, callbacks=[stop_early],verbose=0)

  # Get the optimal hyperparameters
  best_hps=tuner.get_best_hyperparameters(num_trials=1)[0]
  model = tuner.hypermodel.build(best_hps)

  model.fit(train_X, train_Y, epochs=int(r.pynepochs), batch_size=50, verbose=0)

  ## forecasting
  
  q=create_query(scaled_data, look_back=look_back)
  q = np.reshape(q, (1, train_X.shape[1], train_X.shape[2]))
  fore = model.predict(q,verbose=0)
  
  fore=np.reshape(fore, (1, int(r.pyH), int(r.pym)))
  fore=scaler.inverse_transform(fore[0,:,:])
  yhat=fore



if r.plearn=="transformer_gpt":
  from tensorflow import keras
  from tensorflow.keras import layers
  import numpy as np
  import pandas as pd
  
  from keras.layers import Dense,  SimpleRNN
  from keras.models import Sequential
  from sklearn.preprocessing import StandardScaler

  # Create a function that converts an array of data into a dataset for training or testing
  def create_dataset(data, look_back=1, look_ahead=1):
    data_X, data_Y = [], []
    for i in range(len(data) - look_back - look_ahead + 1):
      a = data[i:(i + look_back), :]
      data_X.append(a)
      data_Y.append(data[i + look_back:i + look_back + look_ahead, :])
    return np.array(data_X), np.array(data_Y)  


  def create_query(data, look_back=1):    
    query=[]   
    query.append(data[(len(data)-look_back):len(data), :])
    return  query
  # Scale the time series data
  scaler = StandardScaler()
  scaled_data = scaler.fit_transform(r.pyTS)

  m=int(r.pym)
  look_back = int(r.pyn)
  look_ahead = int(r.pyH)
  
  # Create training and testing datasets
  
  train_X, train_Y = create_dataset(scaled_data, look_back=look_back, look_ahead=look_ahead)

  # Reshape the input data for use with a LSTM model
  train_X = np.reshape(train_X, (train_X.shape[0], train_X.shape[1], train_X.shape[2]))
  train_Y = np.reshape(train_Y, (train_Y.shape[0], train_Y.shape[1]* train_Y.shape[2]))
 
 
  def transformer_encoder(inputs, head_size, num_heads, ff_dim, dropout=0):
    # Normalization and Attention
    x = layers.LayerNormalization(epsilon=1e-6)(inputs)
    x = layers.MultiHeadAttention(
        key_dim=head_size, num_heads=num_heads, dropout=dropout
    )(x, x)
    x = layers.Dropout(dropout)(x)
    res = x + inputs

    # Feed Forward Part
    x = layers.LayerNormalization(epsilon=1e-6)(res)
    x = layers.Conv1D(filters=ff_dim, kernel_size=1, activation="relu")(x)
    x = layers.Dropout(dropout)(x)
    x = layers.Conv1D(filters=inputs.shape[-1], kernel_size=1)(x)
    return x + res
  
  

  
  
  def build_model( input_shape, head_size,
    num_heads,ff_dim, num_transformer_blocks, mlp_units, dropout=0,
    mlp_dropout=0):
    inputs = keras.Input(shape=input_shape)
    x = inputs
    for _ in range(num_transformer_blocks):
        x = transformer_encoder(x, head_size, num_heads, ff_dim, dropout)

    x = layers.GlobalAveragePooling1D(data_format="channels_first")(x)
    for dim in mlp_units:
        x = layers.Dense(dim, activation="relu")(x)
        x = layers.Dropout(mlp_dropout)(x)
    outputs = layers.Dense(look_ahead*m)(x)
    return keras.Model(inputs, outputs)


  input_shape = train_X.shape[1:]

  model = build_model(
    input_shape,
    head_size=256,
    num_heads=2,
    ff_dim=4,
    num_transformer_blocks=int(r.pynunits),
    mlp_units=[128],
    mlp_dropout=0.4,
    dropout=0.5,
  )

  model.compile(
    loss='mean_squared_error', optimizer='adam'
  )
  #model.summary()

  callbacks = [keras.callbacks.EarlyStopping(patience=10, restore_best_weights=True)]

  model.fit(
    train_X,
    train_Y,
    validation_split=0.1,
    epochs=int(r.pynepochs),
    batch_size=64,
    callbacks=callbacks,verbose=0
  )

  #model.evaluate(train_X, train_Y, verbose=1)
  q=create_query(scaled_data, look_back=look_back)
  q = np.reshape(q, (1, train_X.shape[1], train_X.shape[2]))
  fore = model.predict(q, verbose=0)
  
  fore=np.reshape(fore, (1, int(r.pyH), int(r.pym)))
  fore=scaler.inverse_transform(fore[0,:,:])
  yhat=fore


if r.plearn=="transformer_gpt_hyper":
  import tensorflow
  from tensorflow import keras
  from tensorflow.keras import layers
  import numpy as np
  import pandas as pd
  import keras_tuner as kt
  
  from keras.layers import Dense
  from keras.models import Sequential
  from sklearn.preprocessing import StandardScaler

  # Create a function that converts an array of data into a dataset for training or testing
  def create_dataset(data, look_back=1, look_ahead=1):
    data_X, data_Y = [], []
    for i in range(len(data) - look_back - look_ahead + 1):
      a = data[i:(i + look_back), :]
      data_X.append(a)
      data_Y.append(data[i + look_back:i + look_back + look_ahead, :])
    return np.array(data_X), np.array(data_Y)  


  def create_query(data, look_back=1):    
    query=[]   
    query.append(data[(len(data)-look_back):len(data), :])
    return  query
  # Scale the time series data
  scaler = StandardScaler()
  scaled_data = scaler.fit_transform(r.pyTS)

  m=int(r.pym)
  look_back = int(r.pyn)
  look_ahead = int(r.pyH)
  
  # Create training and testing datasets
  
  train_X, train_Y = create_dataset(scaled_data, look_back=look_back, look_ahead=look_ahead)

  # Reshape the input data for use with a LSTM model
  train_X = np.reshape(train_X, (train_X.shape[0], train_X.shape[1], train_X.shape[2]))
  train_Y = np.reshape(train_Y, (train_Y.shape[0], train_Y.shape[1]* train_Y.shape[2]))
  input_shape = train_X.shape[1:]
 
  def transformer_encoder(inputs, head_size, num_heads, ff_dim, dropout=0):
    # Normalization and Attention
    x = layers.LayerNormalization(epsilon=1e-6)(inputs)
    x = layers.MultiHeadAttention(
        key_dim=head_size, num_heads=num_heads, dropout=dropout
    )(x, x)
    x = layers.Dropout(dropout)(x)
    res = x + inputs

    # Feed Forward Part
    x = layers.LayerNormalization(epsilon=1e-6)(res)
    x = layers.Conv1D(filters=ff_dim, kernel_size=1, activation="relu")(x)
    x = layers.Dropout(dropout)(x)
    x = layers.Conv1D(filters=inputs.shape[-1], kernel_size=1)(x)
    return x + res
  
  
  def model_builder(hp,input_shape=input_shape, head_size=256,
    ff_dim=4,  mlp_units=[128], dropout=0.5):
      num_transformer_blocks = hp.Int('num_transformer_blocks', 
        min_value=2, max_value=40, step=2)
      num_heads = hp.Int('num_heads', 
        min_value=2, max_value=5, step=1)
      mlp_dropout = hp.Choice('mlp_dropout', values=[0.1, 0.5, 0.7, 0.9])
      inputs = keras.Input(shape=input_shape)
      x = inputs
      for _ in range(num_transformer_blocks):
        x = transformer_encoder(x, head_size, num_heads, ff_dim, dropout)

      x = layers.GlobalAveragePooling1D(data_format="channels_first")(x)
      for dim in mlp_units:
        x = layers.Dense(dim, activation="relu")(x)
        x = layers.Dropout(mlp_dropout)(x)
      outputs = layers.Dense(look_ahead*m)(x)
      return keras.Model(inputs, outputs)
  
  

  
  
  
  # Train the model on the training data.  
  tuner = kt.Hyperband(model_builder,
                     objective='val_loss',
                     max_epochs=10,
                     factor=3)
  stop_early = tensorflow.keras.callbacks.EarlyStopping(monitor='val_loss', patience=5)
  tuner.search(train_X, train_Y,
    epochs=20, validation_split=0.2, callbacks=[stop_early],verbose=0)

  # Get the optimal hyperparameters
  best_hps=tuner.get_best_hyperparameters(num_trials=1)[0]
  model = tuner.hypermodel.build(best_hps,input_shape)

  model.compile(
    loss='mean_squared_error', optimizer='adam'
  )
  #model.summary()

  callbacks = [keras.callbacks.EarlyStopping(patience=10, restore_best_weights=True)]

  model.fit(
    train_X,
    train_Y,
    validation_split=0.1,
    epochs=int(r.pynepochs),
    batch_size=64,
    callbacks=callbacks,verbose=0
  )

  #model.evaluate(train_X, train_Y, verbose=1)
  q=create_query(scaled_data, look_back=look_back)
  q = np.reshape(q, (1, train_X.shape[1], train_X.shape[2]))
  fore = model.predict(q, verbose=0)
  
  fore=np.reshape(fore, (1, int(r.pyH), int(r.pym)))
  fore=scaler.inverse_transform(fore[0,:,:])
  yhat=fore


if r.plearn=="nbeats_pytorch":
  import pytorch_lightning as pl
  from pytorch_lightning.callbacks import EarlyStopping, LearningRateMonitor

  from pytorch_forecasting import TimeSeriesDataSet, TemporalFusionTransformer
  import os
  import warnings
  import numpy as np
  warnings.filterwarnings("ignore")


  import pandas as pd
  import pytorch_lightning as pl
  from pytorch_lightning.callbacks import EarlyStopping
  import torch

  from pytorch_forecasting import Baseline, NBeats, TimeSeriesDataSet
  from pytorch_forecasting.data import NaNLabelEncoder
  from pytorch_forecasting.data.examples import generate_ar_data
  from pytorch_forecasting.metrics import SMAPE,MultivariateNormalDistributionLoss
  import logging
  logging.getLogger("lightning").setLevel(logging.ERROR)
  m=int(r.pym)
  look_back = int(r.pyn)
  r.pyTS=np.reshape(r.pyTS,-1)
  N=len(r.pyTS)
  H=int(r.pyH)
  
  max_prediction_length = H
  data = generate_ar_data(seasonality=0.0, timesteps=N+H, n_series=1, seed=42)
  
  TS=np.concatenate((np.array(r.pyTS),np.arange(H)*0.0))
 
  data["value"]=TS
  training_cutoff = N-H #data["time_idx"].max() - (2*max_prediction_length)
  validation_cutoff = N


  # create dataset and dataloaders
  max_encoder_length = int(r.pyn)

  context_length = max_encoder_length
  prediction_length = max_prediction_length

  training = TimeSeriesDataSet(
    data[lambda x: x.time_idx < training_cutoff],
    time_idx="time_idx",
    target="value",
    categorical_encoders={"series": NaNLabelEncoder().fit(data.series)},
    group_ids=["series"],
    # only unknown variable is "value" - and N-Beats can also not take any additional variables
    time_varying_unknown_reals=["value"],
    max_encoder_length=context_length,
    #min_encoder_length=context_length//2,
    max_prediction_length=prediction_length,
  )

  validation = TimeSeriesDataSet.from_dataset(training, 
    data[lambda x: x.time_idx<=validation_cutoff],
    min_prediction_idx=training_cutoff + 1)
  test = TimeSeriesDataSet.from_dataset(training, data, 
    min_prediction_idx=validation_cutoff )

  batch_size = 128
  train_dataloader = training.to_dataloader(train=True, batch_size=batch_size, num_workers=0)
  val_dataloader = validation.to_dataloader(train=False, batch_size=batch_size, num_workers=0)
  test_dataloader = test.to_dataloader(train=False, batch_size=batch_size, num_workers=0)
  pl.seed_everything(42)
  trainer = pl.Trainer(gpus=0, gradient_clip_val=0.01,enable_progress_bar=False)
  

  net = NBeats.from_dataset(training, learning_rate=3e-2, 
    weight_decay=1e-2, widths=[32, 512], backcast_loss_ratio=0.1,
    optimizer='adam')

  res = trainer.tuner.lr_find(net, 
    train_dataloaders=train_dataloader, 
    val_dataloaders=val_dataloader, min_lr=1e-5)
  net.hparams.learning_rate = res.suggestion()


  early_stop_callback = EarlyStopping(monitor="val_loss", 
    min_delta=1e-4, patience=10, verbose=False, mode="min")
  trainer = pl.Trainer(
      max_epochs=20,
      gpus=0,
      enable_model_summary=False,
      gradient_clip_val=0.01,
      callbacks=[early_stop_callback],
      limit_train_batches=30,
      enable_progress_bar=False
    
  )


  net = NBeats.from_dataset(
    training,
    learning_rate=4e-3,
    log_interval=10,
    log_val_interval=1,
    weight_decay=1e-2,
    widths=[32, 512],
    backcast_loss_ratio=1.0,
  )

  
  trainer.fit(
    net,
    train_dataloaders=train_dataloader,
    val_dataloaders=val_dataloader
  )

  best_model_path = trainer.checkpoint_callback.best_model_path
  best_model = NBeats.load_from_checkpoint(best_model_path)
     
  predictions = best_model.predict(test_dataloader)    
  yhat=np.array(predictions)

if r.plearn=="deepar_pytorch":
  import pytorch_lightning as pl
  from pytorch_lightning.callbacks import EarlyStopping, LearningRateMonitor

  from pytorch_forecasting import TimeSeriesDataSet, TemporalFusionTransformer
  import os
  import warnings
  import numpy as np
  warnings.filterwarnings("ignore")


  import pandas as pd
  import pytorch_lightning as pl
  from pytorch_lightning.callbacks import EarlyStopping
  import torch

  from pytorch_forecasting import Baseline, TimeSeriesDataSet,DeepAR
  from pytorch_forecasting.data import NaNLabelEncoder
  from pytorch_forecasting.data.examples import generate_ar_data
  from pytorch_forecasting.metrics import SMAPE,MultivariateNormalDistributionLoss
  import logging
  logging.getLogger("lightning").setLevel(logging.ERROR)
  m=int(r.pym)
  look_back = int(r.pyn)
  r.pyTS=np.reshape(r.pyTS,-1)
  N=len(r.pyTS)
  H=int(r.pyH)
  
  max_prediction_length = H
  data = generate_ar_data(seasonality=0.0, timesteps=N+H, n_series=1, seed=42)
  
  TS=np.concatenate((np.array(r.pyTS),np.arange(H)*0.0))
 
  data["value"]=TS
  training_cutoff = N-H #data["time_idx"].max() - (2*max_prediction_length)
  validation_cutoff = N

  # create dataset and dataloaders
  max_encoder_length = int(r.pynunits)

  context_length = max_encoder_length
  prediction_length = max_prediction_length

  training = TimeSeriesDataSet(
    data[lambda x: x.time_idx < training_cutoff],
    time_idx="time_idx",
    target="value",
    #categorical_encoders={"series": NaNLabelEncoder().fit(data.series)},
    group_ids=["series"],
    # only unknown variable is "value" - and N-Beats can also not take any additional variables
    time_varying_unknown_reals=["value"],
    min_encoder_length=context_length//2,
    max_encoder_length=context_length,
    max_prediction_length=prediction_length,
  )

  validation = TimeSeriesDataSet.from_dataset(training, 
    data[lambda x: x.time_idx<=validation_cutoff],
    min_prediction_idx=training_cutoff + 1)
  test = TimeSeriesDataSet.from_dataset(training, data, 
    min_prediction_idx=validation_cutoff )

  batch_size = 128
  train_dataloader = training.to_dataloader(train=True, batch_size=batch_size, num_workers=0)
  val_dataloader = validation.to_dataloader(train=False, batch_size=batch_size, num_workers=0)
  test_dataloader = test.to_dataloader(train=False, batch_size=batch_size, num_workers=0)
  pl.seed_everything(42)
  trainer = pl.Trainer(gpus=0, gradient_clip_val=0.01,enable_progress_bar=False)
  net = DeepAR.from_dataset(
    training, learning_rate=3e-2, hidden_size=30, 
    rnn_layers=2, optimizer='adam',
    loss=MultivariateNormalDistributionLoss(rank=30)
  )

 
  res = trainer.tuner.lr_find(net, 
    train_dataloaders=train_dataloader, 
    val_dataloaders=val_dataloader, min_lr=1e-5)
 
  net.hparams.learning_rate = res.suggestion()


  early_stop_callback = EarlyStopping(monitor="val_loss", 
    min_delta=1e-4, patience=10, verbose=False, mode="min")
  trainer = pl.Trainer(
      max_epochs=20,
      gpus=0,
      enable_model_summary=False,
      gradient_clip_val=0.01,
      callbacks=[early_stop_callback],
      limit_train_batches=30,
      enable_progress_bar=False
    
  )


  net = DeepAR.from_dataset(
    training,
    learning_rate=0.1,
    log_interval=10,
    log_val_interval=1,
    hidden_size=30,
    rnn_layers=2,
    loss=MultivariateNormalDistributionLoss(rank=30),
    optimizer='adam'
  )
  trainer.fit(
    net,
    train_dataloaders=train_dataloader,
    val_dataloaders=val_dataloader
  )

  best_model_path = trainer.checkpoint_callback.best_model_path
  best_model = DeepAR.load_from_checkpoint(best_model_path)
     
  predictions = best_model.predict(test_dataloader)    
  yhat=np.array(predictions)


if r.plearn=="tft_pytorch":
  import pytorch_lightning as pl
  from pytorch_lightning.callbacks import EarlyStopping, LearningRateMonitor

  from pytorch_forecasting import TimeSeriesDataSet, TemporalFusionTransformer
  import os
  import warnings
  import numpy as np
  warnings.filterwarnings("ignore")


  import pandas as pd
  import pytorch_lightning as pl
  from pytorch_lightning.callbacks import EarlyStopping
  import torch

  from pytorch_forecasting import Baseline, TimeSeriesDataSet,DeepAR
  from pytorch_forecasting.data import NaNLabelEncoder
  from pytorch_forecasting.data.examples import generate_ar_data
  from pytorch_forecasting.metrics import SMAPE,QuantileLoss
  import logging
  m=int(r.pym)
  look_back = int(r.pyn)
  r.pyTS=np.reshape(r.pyTS,-1)
  N=len(r.pyTS)
  H=int(r.pyH)
  
  max_prediction_length = H
  data = generate_ar_data(seasonality=0.0, timesteps=N+H, n_series=1, seed=42)
  
  TS=np.concatenate((np.array(r.pyTS),np.arange(H)*0.0))
 
  data["value"]=TS
  training_cutoff = N-H #data["time_idx"].max() - (2*max_prediction_length)
  validation_cutoff = N

  # create dataset and dataloaders
  max_encoder_length = 24

  context_length = max_encoder_length
  prediction_length = max_prediction_length

  training = TimeSeriesDataSet(
    data[lambda x: x.time_idx < training_cutoff],
    time_idx="time_idx",
    target="value",
    categorical_encoders={"series": NaNLabelEncoder().fit(data.series)},
    group_ids=["series"],
    # only unknown variable is "value" - and N-Beats can also not take any additional variables
    time_varying_unknown_reals=["value"],
    #min_encoder_length=context_length//2,
    max_encoder_length=context_length,
    max_prediction_length=prediction_length,
    add_relative_time_idx=True,
    add_target_scales=True,
    add_encoder_length=True
  )

  validation = TimeSeriesDataSet.from_dataset(training, 
    data[lambda x: x.time_idx<=validation_cutoff],
    min_prediction_idx=training_cutoff + 1)
  test = TimeSeriesDataSet.from_dataset(training, data, 
    min_prediction_idx=validation_cutoff )

  batch_size = 128
  train_dataloader = training.to_dataloader(train=True, batch_size=batch_size, num_workers=0)
  val_dataloader = validation.to_dataloader(train=False, batch_size=batch_size, num_workers=0)
  test_dataloader = test.to_dataloader(train=False, batch_size=batch_size, num_workers=0)
  pl.seed_everything(42)
  trainer = pl.Trainer(gpus=0, gradient_clip_val=0.01,
    enable_progress_bar=False,max_epochs=20,
    enable_model_summary=False)
  tft = TemporalFusionTransformer.from_dataset(
    training,
    # not meaningful for finding the learning rate but otherwise very important
    learning_rate=0.08268905348250247,
    hidden_size=int(r.pynunits),  # most important hyperparameter apart from learning rate
    # number of attention heads. Set to up to 4 for large datasets
    attention_head_size=3,
    #dropout=0.1,  # between 0.1 and 0.3 are good values
    hidden_continuous_size=16,  # set to <= hidden_size
    output_size=7,  # 7 quantiles by default
    loss=QuantileLoss(),
    # reduce learning rate if no improvement in validation loss after x epochs
    reduce_on_plateau_patience=4,optimizer='adam'
  )
  res = trainer.tuner.lr_find(tft, 
    train_dataloaders=train_dataloader, 
    val_dataloaders=val_dataloader, min_lr=1e-5)
 
  tft.hparams.learning_rate = res.suggestion()
  early_stop_callback = EarlyStopping(monitor="val_loss", 
    min_delta=1e-4, patience=10, verbose=False, mode="min")
  trainer = pl.Trainer(
      max_epochs=int(r.pynepochs),
      gpus=0,
      enable_model_summary=False,
      gradient_clip_val=0.01,
      callbacks=[early_stop_callback],
      limit_train_batches=30,
      enable_progress_bar=True
    
  )

  trainer.fit(
    tft,
    train_dataloaders=train_dataloader,
    val_dataloaders=val_dataloader
  )

  best_model_path = trainer.checkpoint_callback.best_model_path
  best_model = TemporalFusionTransformer.load_from_checkpoint(best_model_path)
      
  predictions = best_model.predict(test_dataloader)    
  baseline_predictions = Baseline().predict(test_dataloader)
  yhat=np.array(predictions)

if r.plearn=="darts_nbeats":
  import pandas as pd
  import numpy as np
  from darts import TimeSeries
  from darts.models import NBEATSModel
  series=TimeSeries.from_values(np.array(r.pyTS))
  
  N=len(r.pyTS)
  H=int(r.pyH)
  n=int(r.pyn)
  model = NBEATSModel(input_chunk_length=max(H+1,5*n),
    output_chunk_length=H, random_state=42)
  
  model.fit(series,epochs=int(r.pynepochs),verbose=False)
  forecast = model.predict(H,verbose=False)
  yhat=np.array(forecast.pd_dataframe().values)

if r.plearn=="darts_tft":
  import pandas as pd
  import numpy as np
  from darts import TimeSeries
  from darts.models import TransformerModel
  series=TimeSeries.from_values(np.array(r.pyTS))
  
  N=len(r.pyTS)
  H=int(r.pyH)
  n=int(r.pyn)
  model = TransformerModel(
    batch_size=32,
    input_chunk_length=max(H+1,5*n),
    output_chunk_length=H,
    n_epochs=int(r.pynepochs),
    model_name="transformer",
    nr_epochs_val_period=5,
    d_model=16,
    nhead=4,
    num_encoder_layers=2,
    num_decoder_layers=2,
    dim_feedforward=int(r.pynunits),
    dropout=0.1,
    random_state=42,
    optimizer_kwargs={"lr": 1e-3},
    save_checkpoints=False,
    force_reset=True,
  )
  model.fit(series,epochs=int(r.pynepochs),verbose=False)
  forecast = model.predict(H,verbose=False)
  yhat=np.array(forecast.pd_dataframe().values)

if r.plearn=="darts_nhits":
  import pandas as pd
  import numpy as np
  from darts import TimeSeries
  from darts.models import NHiTSModel
  series=TimeSeries.from_values(np.array(r.pyTS))
  
  N=len(r.pyTS)
  H=int(r.pyH)
  n=int(r.pyn)
  model = NHiTSModel(input_chunk_length=max(H+1,5*n),
    output_chunk_length=H,n_epochs=int(r.pynepochs),)
  model.fit(series,epochs=int(r.pynepochs),verbose=False)
  forecast = model.predict(H,verbose=False)
  yhat=np.array(forecast.pd_dataframe().values)
  
if r.plearn=="darts_transformer":
  import pandas as pd
  import numpy as np
  from darts import TimeSeries
  from darts.models import TransformerModel
  series=TimeSeries.from_values(np.array(r.pyTS))
  
  N=len(r.pyTS)
  H=int(r.pyH)
  n=int(r.pyn)
  model = TransformerModel(input_chunk_length=max(H+1,5*n),
    output_chunk_length=H,n_epochs=int(r.pynepochs),)
  model.fit(series,epochs=int(r.pynepochs),verbose=False)
  forecast = model.predict(H,verbose=False)
  yhat=np.array(forecast.pd_dataframe().values)  

if r.plearn=="darts_TCN":
  import pandas as pd
  import numpy as np
  from darts import TimeSeries
  from darts.models import TCNModel
  series=TimeSeries.from_values(np.array(r.pyTS))
  
  N=len(r.pyTS)
  H=int(r.pyH)
  n=int(r.pyn)
  model = TCNModel(input_chunk_length=max(H+1,5*n),
    output_chunk_length=H,n_epochs=int(r.pynepochs),)
  model.fit(series,epochs=int(r.pynepochs),verbose=False)
  forecast = model.predict(H,verbose=False)
  yhat=np.array(forecast.pd_dataframe().values)  

if r.plearn=="darts_RNN":
  import pandas as pd
  import numpy as np
  from darts import TimeSeries
  from darts.models import RNNModel
  series=TimeSeries.from_values(np.array(r.pyTS))
  
  N=len(r.pyTS)
  H=int(r.pyH)
  n=int(r.pyn)
  model = RNNModel(input_chunk_length=max(H+1,5*n),
    output_chunk_length=H,n_epochs=int(r.pynepochs),)
  model.fit(series,epochs=int(r.pynepochs),verbose=False)
  forecast = model.predict(H,verbose=False)
  yhat=np.array(forecast.pd_dataframe().values)  

if r.plearn=="darts_blockRNN":
  import pandas as pd
  import numpy as np
  from darts import TimeSeries
  from darts.models import BlockRNNModel
  series=TimeSeries.from_values(np.array(r.pyTS))
  
  N=len(r.pyTS)
  H=int(r.pyH)
  n=int(r.pyn)
  model = BlockRNNModel(input_chunk_length=max(H+1,5*n),
    output_chunk_length=H,n_epochs=int(r.pynepochs),)
  model.fit(series,epochs=int(r.pynepochs),verbose=False)
  forecast = model.predict(H,verbose=False)
  yhat=np.array(forecast.pd_dataframe().values)
  
if r.plearn=="darts_lightGBM":
  import pandas as pd
  import numpy as np
  from darts import TimeSeries
  from darts.models import LightGBMModel
  series=TimeSeries.from_values(np.array(r.pyTS))
  
  N=len(r.pyTS)
  H=int(r.pyH)
  n=int(r.pyn)
  model = LightGBMModel(lags=n)
  model.fit(series,verbose=False)
  forecast = model.predict(H)
  yhat=np.array(forecast.pd_dataframe().values)  

if r.plearn=="darts_xGBM":
  import pandas as pd
  import numpy as np
  from darts import TimeSeries
  from darts.models import XGBModel
  series=TimeSeries.from_values(np.array(r.pyTS))
  
  N=len(r.pyTS)
  H=int(r.pyH)
  n=int(r.pyn)
 
  model = XGBModel(lags=n)
  model.fit(series,verbose=False)
  forecast = model.predict(H)
  yhat=np.array(forecast.pd_dataframe().values)  
  
if r.plearn=="darts_RandomForest":
  import pandas as pd
  import numpy as np
  from darts import TimeSeries
  from darts.models import XGBModel
  series=TimeSeries.from_values(np.array(r.pyTS))
  
  N=len(r.pyTS)
  H=int(r.pyH)
  n=int(r.pyn)
  model = XGBModel(lags=n)
  model.fit(series,verbose=False)
  forecast = model.predict(H)
  yhat=np.array(forecast.pd_dataframe().values)  
  import pandas as pd
  import numpy as np
  from darts import TimeSeries
  from darts.models import RandomForest
  series=TimeSeries.from_values(np.array(r.pyTS))
  
  N=len(r.pyTS)
  H=int(r.pyH)
  model = RandomForest(lags=int(r.pyn))
  model.fit(series)
  forecast = model.predict(H)
  yhat=np.array(forecast.pd_dataframe().values)  
  
if yhat==[]:
  import sys
  sys.exit("empty output in the call "+ r.plearn)
