import numpy as np
import pandas as pd
from keras.layers import Dense, LSTM, TimeDistributed
from keras.models import Sequential
from sklearn.preprocessing import MinMaxScaler
df = pd.read_csv('test.csv')
print(df.shape)
# Scale the time series data
scaler = MinMaxScaler()
scaled_data = scaler.fit_transform(df[["x1", "x2", "x3"]])
unscaled_data =np.array(df)
# Split the data into training and testing sets
m=scaled_data.shape[1]
n_train = int(scaled_data.shape[0] * 0.9)
train_data = scaled_data[:n_train]
test_data = scaled_data[n_train:]

# Create a function that converts an array of data into a dataset for training or testing
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

# Create training and testing datasets
look_back = 2
look_ahead = 5
train_X, train_Y = create_dataset(train_data, look_back=look_back, look_ahead=look_ahead)
test_X, test_Y = create_dataset(test_data, look_back=look_back, look_ahead=look_ahead)

# Reshape the input data for use with a LSTM model
train_X = np.reshape(train_X, (train_X.shape[0], train_X.shape[1], train_X.shape[2]))
test_X = np.reshape(test_X, (test_X.shape[0], test_X.shape[1], test_X.shape[2]))
train_Y = np.reshape(train_Y, (train_Y.shape[0], train_Y.shape[1], train_Y.shape[2]))
test_Y = np.reshape(test_Y, (test_Y.shape[0], test_Y.shape[1], test_Y.shape[2]))
model = Sequential()
model.add(LSTM(units=50, input_shape=(m,look_back), return_sequences=True))
#model.add(Dense(look_ahead*m))
model.add(TimeDistributed(Dense(look_ahead)))
model.compile(loss='mean_squared_error', optimizer='adam')
print(model.summary())
# Train the model on the training data
model.fit(train_X, train_Y, epochs=100, batch_size=1, verbose=2)

# Make predictions on the test data
predictions = model.predict(test_X)

print(predictions)

predictions = np.reshape(predictions, (predictions.shape[0], test_Y.shape[1], test_Y.shape[2]))
print(predictions.shape)
print(test_Y.shape)

nmse=[]
for i in range(predictions.shape[0]):
  yhat=scaler.inverse_transform(predictions[i,:,:].T)  
  y=scaler.inverse_transform(test_Y[i,:,:].T) 
  
  for j in range(y.shape[1]):
    nmse.append( np.mean((yhat[:,j] - y[:,j]) ** 2)/np.var(y[:,j]))
print(np.mean(nmse))

## forecasting
q=create_query(scaled_data, look_back=look_back)
q = np.reshape(q, (1, train_X.shape[1], train_X.shape[2]))
fore = model.predict(q)
fore=np.reshape(fore, (1, test_Y.shape[1], test_Y.shape[2]))
fore=scaler.inverse_transform(fore[0,:,:].T)
print(fore)
