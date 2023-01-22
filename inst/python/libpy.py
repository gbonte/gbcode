## python implementation of learning models used by mlearn




import numpy as np
import warnings 
warnings.filterwarnings("ignore")
  


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

  for i in np.arange(int(r.pynepochs)):
      model.fit(r.pyX, r.pyY,batch_size =1,epochs=1,shuffle=False,verbose=0)
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
  
  model.fit(r.pyX, r.pyY,epochs=int(r.pynepochs),verbose=0)
  #
  yhat=model.predict(r.pyXts, batch_size = 1,verbose=0)



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

  model.fit(r.pyX, r.pyY,epochs=int(r.pynepochs),validation_split=0.2, verbose=0,
    batch_size =bsize,shuffle=False,)
  #print(r.pyXts.shape)
  yhat=model.predict(r.pyXts, batch_size = 1,verbose=0)



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
    epochs=int(r.pynepochs), validation_split=0.2, callbacks=[stop_early],verbose=1)

  # Get the optimal hyperparameters
  best_hps=tuner.get_best_hyperparameters(num_trials=1)[0]
  model = tuner.hypermodel.build(best_hps)
  

  yhat=model.predict(r.pyXts, batch_size = 1,verbose=0)

if r.plearn=="lstm_gpt2":
  import numpy as np
  import pandas as pd
  from keras.layers import Dense, LSTM, Dropout
  from keras.models import Sequential
  from sklearn.preprocessing import StandardScaler

  def create_dataset(data, look_back=1, look_ahead=1):
    data_X, data_Y = [], []
    for i in range(len(data) - look_back - look_ahead + 1):
        a = data[i:(i + look_back),:].T
        data_X.append(a)
        data_Y.append(data[i + look_back:i + look_back + look_ahead, :].T)
    return np.array(data_X), np.array(data_Y)

  def create_query(data, look_back=1):    
    query=[]   
    query.append(data[(len(data)-look_back):len(data), :].T)
    return  query

  # Scale the time series data
  scaler = StandardScaler()
  scaled_data = scaler.fit_transform(r.pyTS)
  scaled_data =r.pyTS
  m=int(r.pym)
  look_back = int(r.pyn)
  look_ahead = int(r.pyH)
  
  # Create training and testing datasets
  
  train_X, train_Y = create_dataset(scaled_data, look_back=look_back, look_ahead=look_ahead)

  train_X = np.reshape(train_X, (train_X.shape[0], train_X.shape[1], train_X.shape[2]))
  test_X = np.reshape(test_X, (test_X.shape[0], test_X.shape[1], test_X.shape[2]))
  train_Y = np.reshape(train_Y, (train_Y.shape[0], train_Y.shape[1], train_Y.shape[2]))
  test_Y = np.reshape(test_Y, (test_Y.shape[0], test_Y.shape[1], test_Y.shape[2]))
  model = Sequential()
  model.add(LSTM(units=int(r.pynunits), input_shape=(m,look_back), return_sequences=True))
  #model.add(Dense(look_ahead*m))
  model.add(TimeDistributed(Dense(look_ahead)))
  model.compile(loss='mean_squared_error', optimizer='adam')

  # Train the model on the training data.  

  model.fit(train_X, train_Y, epochs=int(r.pynepochs), batch_size=1, verbose=2)

  ## forecasting
  
  q=create_query(scaled_data, look_back=look_back)
  q = np.reshape(q, (1, train_X.shape[1], train_X.shape[2]))
  print(q)
  fore = model.predict(q,verbose=0)[0,:,:]
  
  ##fore=np.reshape(fore, (1, int(r.pyH), int(r.pym)))
  ##yhat=scaler.inverse_transform(fore.T)
  yhat=fore.T

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
                     #objective='val_accuracy',
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
                     objective='val_accuracy',
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
    num_transformer_blocks=4,
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



if yhat==[]:
  import sys
  sys.exit("empty output in the call "+ r.plearn)
