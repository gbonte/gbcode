#py_install("lightgbm")
# repl_python()
from tensorflow import keras 
from tensorflow.keras import layers
from keras.layers import LSTM
import keras_tuner as kt
import tensorflow as tf
from tensorflow.keras.datasets import boston_housing
(train_data, train_targets), (test_data, test_targets) = (
    boston_housing.load_data())
train_data.shape

mean = train_data.mean(axis=0)
train_data -= mean
std = train_data.std(axis=0)
train_data /= std
test_data -= mean
test_data /= std
X=train_data
N=X.shape[0]
n=X.shape[1]
Nts=Xts.shape[0]
X = X.reshape(N,n ,1)
Y=X[1:,:]
Y = Y.reshape(N,1)
Xts=X[1:,:]
#Xts = Xts.reshape(Nts,n ,1)
  
def model_builder(hp):
  model = keras.Sequential()
  #model.add(keras.layers.Flatten(input_shape=(28, 28)))
  # Tune the number of units in the first Dense layer
  # Choose an optimal value between 32-512
  hp_units = hp.Int('units', min_value=1, max_value=10, step=1)
  model.add(keras.layers.Dense(units=1, activation='relu'))
  model.add(keras.layers.LSTM(1,input_shape=(n, 1)))
  model.add(keras.layers.Dense(1))
  # Tune the learning rate for the optimizer
  # Choose an optimal value from 0.01, 0.001, or 0.0001
  #hp_learning_rate = hp.Choice('learning_rate', values=[1e-2, 1e-3, 1e-4])

  model.compile(optimizer="rmsprop",
                loss="mse",
                metrics=['accuracy'])

  return model

tuner = kt.Hyperband(model_builder,
                     objective='val_accuracy',
                     max_epochs=10,
                     factor=3,
                     directory='my_dir',
                     project_name='intro_to_kt')
                     
stop_early = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=5)
tuner.search(X, train_targets,
  epochs=50, validation_split=0.2, callbacks=[stop_early])

# Get the optimal hyperparameters
best_hps=tuner.get_best_hyperparameters(num_trials=1)[0]
hypermodel = tuner.hypermodel.build(best_hps)
hypermodel.fit(X, Y,epochs=130, batch_size=16, validation_split=0.2, 
  verbose=0)
                                
#model.fit(train_data, train_targets,                 
#          epochs=130, batch_size=16, verbose=0)
#test_mse_score, test_mae_score = model.evaluate(test_data, test_targets)
yhat=hypermodel.predict(Xts)
print(yhat)
