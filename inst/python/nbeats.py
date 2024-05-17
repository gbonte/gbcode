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
from pytorch_forecasting.metrics import SMAPE


N=1000
H=40
max_prediction_length = H
data = generate_ar_data(seasonality=0.0, timesteps=N+H, n_series=1, seed=42)
TS=np.concatenate((np.sin(2*np.pi*(np.arange(N)*1.0)/20),np.arange(H)*0.0))
data["value"]=TS
training_cutoff = data["time_idx"].max() - 2*max_prediction_length

data["value"][training_cutoff +  max_prediction_length+ 1:]=0
data.head()

# create dataset and dataloaders
max_encoder_length = 60




context_length = max_encoder_length
prediction_length = max_prediction_length

training = TimeSeriesDataSet(
    data[lambda x: x.time_idx <= training_cutoff],
    time_idx="time_idx",
    target="value",
    categorical_encoders={"series": NaNLabelEncoder().fit(data.series)},
    group_ids=["series"],
    # only unknown variable is "value" - and N-Beats can also not take any additional variables
    time_varying_unknown_reals=["value"],
    max_encoder_length=context_length,
    max_prediction_length=prediction_length,
)

validation = TimeSeriesDataSet.from_dataset(training, data, 
  min_prediction_idx=training_cutoff + 1)
test = TimeSeriesDataSet.from_dataset(training, data, 
  min_prediction_idx=training_cutoff +  max_prediction_length+ 1)

batch_size = 128
train_dataloader = training.to_dataloader(train=True, batch_size=batch_size, num_workers=0)
val_dataloader = validation.to_dataloader(train=False, batch_size=batch_size, num_workers=0)
test_dataloader = test.to_dataloader(train=False, batch_size=batch_size, num_workers=0)
pl.seed_everything(42)
trainer = pl.Trainer(gpus=0, gradient_clip_val=0.01)
net = NBeats.from_dataset(training, learning_rate=3e-2, 
  weight_decay=1e-2, widths=[32, 512], backcast_loss_ratio=0.1,
  optimizer='adam')

res = trainer.tuner.lr_find(net, 
  train_dataloaders=train_dataloader, 
  val_dataloaders=val_dataloader, min_lr=1e-5)
print(f"suggested learning rate: {res.suggestion()}")
fig = res.plot(show=True, suggest=True)
fig.show()
net.hparams.learning_rate = res.suggestion()


early_stop_callback = EarlyStopping(monitor="val_loss", 
  min_delta=1e-4, patience=10, verbose=False, mode="min")
trainer = pl.Trainer(
    max_epochs=20,
    gpus=0,
    enable_model_summary=True,
    gradient_clip_val=0.01,
    callbacks=[early_stop_callback],
    limit_train_batches=30
    
)


net = DeepAR.from_dataset(
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
    val_dataloaders=val_dataloader,
    ckpt_path="last"
)

best_model_path = trainer.checkpoint_callback.best_model_path
best_model = DeepAR.load_from_checkpoint(best_model_path)
raw_predictions, x = best_model.predict(test_dataloader, mode="raw", return_x=True)

for idx in range(1):  # plot 10 examples
    best_model.plot_prediction(x, raw_predictions, idx=idx, add_loss_to_title=True);
    
predictions = best_model.predict(test_dataloader)    


