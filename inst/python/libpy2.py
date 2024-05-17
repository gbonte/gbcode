## python implementation of learning models used by mlearn

import numpy as np
import warnings 
warnings.filterwarnings("ignore")
  



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



if yhat==[]:
  import sys
  sys.exit("empty output in the call "+ r.plearn)
