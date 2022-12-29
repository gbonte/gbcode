## python implementation of learning models used by mlearn




import numpy as np
import warnings 
warnings.filterwarnings("ignore")
  


yhat=[]
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
  reg = PLSRegression(n_components=2)
  reg.fit(r.pyX, r.pyY)
  yhat = reg.predict(r.pyXts)
  
if r.plearn=="ridge_regr":
  from sklearn.linear_model import RidgeCV
  reg = RidgeCV(alphas=[1e-3, 1e-2, 1e-1, 1])
  reg.fit(r.pyX, r.pyY)
  yhat = reg.predict(r.pyXts)
  
  
if r.plearn=="lasso_regr":  
  from sklearn.linear_model import LassoCV, MultiTaskLassoCV
  if r.pym==1:
    reg = LassoCV(cv=10, random_state=0).fit(r.pyX, r.pyY)
  else:
    reg = MultiTaskLassoCV(cv=3, random_state=0,max_iter=50,
    verbose=0).fit(r.pyX, r.pyY)
  yhat = reg.predict(r.pyXts)
  
if r.plearn=="enet_regr":  
  from sklearn.linear_model import ElasticNet, MultiTaskElasticNet
  if r.pym==1:
    reg = ElasticNet(random_state=0).fit(r.pyX, r.pyY)
  else:
    reg =MultiTaskElasticNet(alpha=0.1).fit(r.pyX, r.pyY)
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
  if r.pym>1:
    from sklearn.multioutput import RegressorChain
    rf_r = RegressorChain(base_estimator=rf_r, order='random',cv=3)
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
  if r.pym>1:
    #from sklearn.multioutput import MultiOutputRegressor
    #gb_regressor = MultiOutputRegressor(GradientBoostingRegressor(n_estimators=5))
    from sklearn.multioutput import RegressorChain
    gb_regressor = RegressorChain(base_estimator=GradientBoostingRegressor(n_estimators=5), order='random',cv=3)
  else:
    gb_regressor = GradientBoostingRegressor(n_estimators=5)
  
  
  
  
  gb_regressor.fit(r.pyX, r.pyY)
  
  yhat = gb_regressor.predict(r.pyXts)
 
  
if r.plearn=="ab_regr":
  from sklearn.ensemble import AdaBoostRegressor
  from sklearn.tree import DecisionTreeRegressor
  if r.pym>1:
    from sklearn.multioutput import MultiOutputRegressor
    ab_regressor = MultiOutputRegressor(AdaBoostRegressor(DecisionTreeRegressor(max_depth=4), 
      n_estimators=400, random_state=7))
  else:
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

  yhat.shape=(int(r.pyNts),int(r.pym))
## TIMESERIES

if r.plearn=="lstm_ts0":
  from tensorflow import keras 
  from tensorflow.keras import layers
  from keras.layers import LSTM
  r.pyY=r.pyY.reshape(int(r.pyN),int(r.pyH)*int(r.pym))
  model = keras.Sequential()
  nunits=10
  model.add(keras.layers.LSTM(units            = nunits, 
               input_shape      = (int(r.pyn),int(r.pym)), 
               batch_size         =1,
               return_sequences = True, 
               stateful         = True))
  model.add(keras.layers.LSTM(units            = nunits, 
               batch_size         =1,
               return_sequences = False, 
               stateful         = True))
  model.add(keras.layers.Dense(int(r.pym)*int(r.pyH)))
 
  model.compile(optimizer="rmsprop",
                loss="mse",
                metrics=['accuracy'])

  for i in np.arange(r.pynepochs):
      model.fit(r.pyX, r.pyY,batch_size =1,epochs=1,shuffle=False,verbose=1)
      model.reset_states()

  #model.fit(r.pyX, r.pyY,epochs=100)
  print(r.pyXts.shape)
  yhat=model.predict(r.pyXts, batch_size = 1)
  print(yhat)

    
if r.plearn=="lstm_ts":
  from tensorflow import keras 
  from tensorflow.keras import layers
  from keras.layers import LSTM
  import keras_tuner as kt
  import tensorflow as tf
  def model_builder(hp):
    model = keras.Sequential()
    nunits = hp.Int('units', min_value=32, max_value=512, step=32)
  
    model.add(keras.layers.LSTM(units            = nunits, 
               input_shape      = (int(r.pyn),1), 
               batch_size         =1,
               return_sequences = True, 
               stateful         = False))
    model.add(keras.layers.LSTM(units            = nunits, 
               batch_size         =1,
               return_sequences = True, 
               stateful         = False))
    model.add(keras.layers.Dense(int(r.pym)))
 
    model.compile(optimizer="rmsprop",
                loss="mse",
                metrics=['accuracy'])
    # Tune the number of units in the first Dense layer
    # Choose an optimal value between 32-512
    #hp_droprate = hp.Choice('droprate', values=[0.1, 0.5, 0.7, 0.9])
    #model.add(keras.layers.Dropout(hp_droprate))
    #model.add(keras.layers.Dense(r.pym))

   
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
  
  #for i in np.arange(10):
  #    model.fit(r.pyX, r.pyY,batch_size =1,epochs=1,shuffle=False,verbose=0)
  #    model.reset_states()
  
  model.fit(r.pyX, r.pyY,epochs=r.pynepochs,verbose=0)
  #
  yhat=model.predict(r.pyXts, batch_size = 1)



if r.plearn=="lstm_ts2":
  from tensorflow import keras 
  from tensorflow.keras import layers
  from keras.layers import LSTM
  from tensorflow.keras.models import Sequential
  from tensorflow.keras.layers import Dropout
  from tensorflow.keras.layers import Dense, Reshape
  from keras_tuner.tuners import RandomSearch
  import keras_tuner as kt
  import tensorflow as tf
  bsize=1
  def model_builder(hp):
    model = Sequential()
    model.add(LSTM(hp.Int('input_unit',min_value=2,max_value=512,step=32),
    return_sequences=True, batch_size=bsize,stateful         = True,
    input_shape=(int(r.pyn),int(r.pym))))
    for i in range(hp.Int('n_layers', 1, 2)):
        model.add(LSTM(hp.Int(f'lstm_{i}_units',min_value=2,max_value=512,step=32),return_sequences=True))
    model.add(LSTM(hp.Int('layer_2_neurons',min_value=2,max_value=512,step=32),return_sequences=True,
    stateful = True))
    model.add(Dropout(hp.Float('Dropout_rate',min_value=0,max_value=0.5,step=0.1)))
    model.add(Dense((int(r.pyH)*int(r.pym))))# activation=hp.Choice('dense_activation',values=['relu', 'sigmoid'],default='relu')))
    #model.add(Reshape((int(r.pyH),int(r.pym))))
    model.compile(loss='mean_squared_error', optimizer='adam',metrics = ['mse'])
    return model
  
  print(r.pyY)
  
  r.pyY=r.pyY.reshape(int(r.pyN),int(r.pyH)*int(r.pym))
  tuner = kt.Hyperband(model_builder,
                     objective='val_accuracy',
                     max_epochs=100,
                     factor=3)
  stop_early = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=5)
  tuner.search(r.pyX, r.pyY,
    epochs=500, validation_split=0.2, callbacks=[stop_early],verbose=1)
 
  # Get the optimal hyperparameters
  best_hps=tuner.get_best_hyperparameters(num_trials=1)[0]
  print(best_hps)
  model = tuner.hypermodel.build(best_hps)
  
  #for i in np.arange(100):
  #    model.fit(r.pyX, r.pyY,batch_size =1,epochs=1,shuffle=False,verbose=1)
  #    model.reset_states()

  model.fit(r.pyX, r.pyY,epochs=r.pynepochs,validation_split=0.2, verbose=1,batch_size =bsize,shuffle=False,)
  #print(r.pyXts.shape)
  yhat=model.predict(r.pyXts, batch_size = 1)



if r.plearn=="rnn_ts":
  from tensorflow import keras 
  from tensorflow.keras import layers
  from keras.layers import SimpleRNN
  import keras_tuner as kt
  import tensorflow as tf
  def model_builder(hp):
    model = keras.Sequential()
    nunits = hp.Int('units', min_value=1, max_value=200, step=1)
  
    model.add(keras.layers.SimpleRNN(units            = nunits, 
               input_shape      = (int(r.pyn),1), 
               batch_size         =1,
               return_sequences = True, 
               stateful         = True))
    nunits2 = hp.Int('units', min_value=1, max_value=200, step=5)
    model.add(keras.layers.SimpleRNN(units            = nunits, 
               batch_size         =1,
               return_sequences = True, 
               stateful         = True))
    hp_droprate = hp.Choice('droprate', values=[0.1, 0.5, 0.7, 0.9])
    model.add(keras.layers.Dropout(hp_droprate))
    model.add(keras.layers.Dense(int(r.pym)))
 
    model.compile(optimizer="rmsprop",
                loss="mse",
                metrics=['accuracy'])
    # Tune the number of units in the first Dense layer
    # Choose an optimal value between 32-512
    #
    #model.add(keras.layers.Dense(r.pym))

   
    return model
  
  tuner = kt.Hyperband(model_builder,
                     objective='val_accuracy',
                     max_epochs=10,
                     factor=3)
  stop_early = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=5)
  tuner.search(r.pyX, r.pyY,
    epochs=r.pynepochs, validation_split=0.2, callbacks=[stop_early],verbose=1)

  # Get the optimal hyperparameters
  best_hps=tuner.get_best_hyperparameters(num_trials=1)[0]
  model = tuner.hypermodel.build(best_hps)
  

  yhat=model.predict(r.pyXts, batch_size = 1)

if r.plearn=="lstm_gpt":
  import numpy as np
  import pandas as pd
  from keras.layers import Dense, LSTM
  from keras.models import Sequential
  from sklearn.preprocessing import MinMaxScaler

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
  scaler = MinMaxScaler()
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
  model.add(LSTM(units=50, input_shape=(look_back, m)))
  model.add(Dense(look_ahead*m))
  model.compile(loss='mean_squared_error', optimizer='adam')

  # Train the model on the training data.  

  model.fit(train_X, train_Y, epochs=int(r.pynepochs), batch_size=50, verbose=0)

  ## forecasting
  
  q=create_query(scaled_data, look_back=look_back)
  q = np.reshape(q, (1, train_X.shape[1], train_X.shape[2]))
  fore = model.predict(q)
  
  fore=np.reshape(fore, (1, int(r.pyH), int(r.pym)))
  fore=scaler.inverse_transform(fore[0,:,:])
  yhat=fore


if r.plearn=="rnn_gpt":
  import numpy as np
  import pandas as pd
  from keras.layers import Dense,  SimpleRNN
  from keras.models import Sequential
  from sklearn.preprocessing import MinMaxScaler

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
  scaler = MinMaxScaler()
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
  model.add(SimpleRNN(units=50, input_shape=(look_back, m)))
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


if yhat==[]:
  import sys
  sys.exit("empty output in the call "+ r.plearn)
