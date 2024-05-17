# repl_python()
import os
import warnings

warnings.filterwarnings("ignore")  # avoid printing out absolute paths

import copy
from pathlib import Path
import warnings

import numpy as np
import pandas as pd
import pytorch_lightning as pl
from pytorch_lightning.callbacks import EarlyStopping, LearningRateMonitor
from pytorch_lightning.loggers import TensorBoardLogger
import torch

from pytorch_forecasting import Baseline, TemporalFusionTransformer, TimeSeriesDataSet
from pytorch_forecasting.data import GroupNormalizer
from pytorch_forecasting.metrics import SMAPE, PoissonLoss, QuantileLoss
from pytorch_forecasting.models.temporal_fusion_transformer.tuning import optimize_hyperparameters
from pytorch_forecasting import Baseline, DeepAR, TimeSeriesDataSet
from pytorch_forecasting.metrics import MultivariateNormalDistributionLoss
from pytorch_forecasting.data.examples import generate_ar_data

data = pd.read_csv('energy_set_api.csv')
data['time_idx'] = np.arange(len(data))
data['Symbol'] = '0'
data = data[np.isnan(data['time_idx'])==False]
data.time_idx=data.time_idx.astype(int)
data.logdif += 1
data = data.dropna()
data["time_idx"] -= data["time_idx"].min()

TS=np.random.rand(30) - 0.5
test_data = pd.DataFrame(
    dict(
        value=TS,
        group=np.repeat(np.arange(1), len(TS)),
        time_idx=np.tile(np.arange(len(TS)), 1),
    )
  )
data = generate_ar_data(seasonality=10.0, timesteps=400, n_series=100, seed=42)
data["static"] = 2
data["date"] = pd.Timestamp("2020-01-01") + pd.to_timedelta(data.time_idx, "D")
data.head()
data = data.astype(dict(series=str))

max_prediction_length = 72
max_encoder_length = 24
training_cutoff = data["time_idx"].max() - max_prediction_length
print('\nmaking training set...\n')
dataset = TimeSeriesDataSet(
    test_data,
    group_ids=["group"],
    target="value",
    time_idx="time_idx",
    min_encoder_length=5,
    max_encoder_length=5,
    min_prediction_length=2,
    max_prediction_length=2,
    time_varying_unknown_reals=["value"],
)

# create validation set (predict=True) which means to predict the last max_prediction_length points in time
# for each series
print('creating validation...')
validation = TimeSeriesDataSet.from_dataset(training, data, predict=True, stop_randomization=True)
print('creating dataloaders...')
# create dataloaders for model
batch_size = 128  # set this between 32 to 128
train_dataloader = training.to_dataloader(train=True, batch_size=batch_size, num_workers=0)
val_dataloader = validation.to_dataloader(train=False, batch_size=batch_size * 10, num_workers=0)

print("configuring network...")
# configure network and trainer
early_stop_callback = EarlyStopping(monitor="val_loss", min_delta=1e-4, patience=10, verbose=False, mode="min")
lr_logger = LearningRateMonitor()  # log the learning rate
logger = TensorBoardLogger("lightning_logs")  # logging results to a tensorboard
pl.seed_everything(42)
print("configuring trainer...")
trainer = pl.Trainer(
    max_epochs=100,
    gpus=0,
    enable_model_summary=True,
    limit_train_batches=30,
    # clipping gradients is a hyperparameter and important to prevent divergance
    # of the gradient for recurrent neural networks
    gradient_clip_val=0.05125275276671815,
    callbacks=[lr_logger, early_stop_callback],
    logger=logger,
)

#{'gradient_clip_val': 0.05125275276671815, 'hidden_size': 128, 'dropout': 0.1809752133171034, 'hidden_continuous_size': 16, 'attention_head_size': 3, 'learning_rate': 0.08268905348250247}
print("configuring DeepAR...")
net = DeepAR.from_dataset(
    training, 
    learning_rate=3e-2, 
    hidden_size=30, 
    rnn_layers=2, 
    loss=QuantileLoss() #MultivariateNormalDistributionLoss(rank=30)
)

print("configuring TFT...")
tft = TemporalFusionTransformer.from_dataset(
    training,
    # not meaningful for finding the learning rate but otherwise very important
    learning_rate=0.08268905348250247,
    hidden_size=128,  # most important hyperparameter apart from learning rate
    # number of attention heads. Set to up to 4 for large datasets
    attention_head_size=3,
    dropout=0.1809752133171034,  # between 0.1 and 0.3 are good values
    hidden_continuous_size=16,  # set to <= hidden_size
    output_size=7,  # 7 quantiles by default
    loss=QuantileLoss(),
    # reduce learning rate if no improvement in validation loss after x epochs
    reduce_on_plateau_patience=4,
)
print(f"Number of parameters in network: {tft.size()/1e3:.1f}k")

trainer.fit(
    tft,
    train_dataloaders=train_dataloader,
    val_dataloaders=val_dataloader,
)

# calcualte mean absolute error on validation set
actuals = torch.cat([y[0] for x, y in iter(val_dataloader)])
predictions = tft.predict(val_dataloader)
(actuals - predictions).abs().mean()


# raw predictions are a dictionary from which all kind of information including quantiles can be extracted
raw_predictions, x = tft.predict(val_dataloader, mode="raw", return_x=True)



predictions, x = tft.predict(val_dataloader, return_x=True)

for idx in range(1):  # plot 10 examples
    tft.plot_prediction(x, raw_predictions, idx=idx, add_loss_to_title=True)

predictions_vs_actuals = tft.calculate_prediction_actual_by_variable(x, predictions)
tft.plot_prediction_actual_by_variable(predictions_vs_actuals);



test_data = pd.DataFrame(
    dict(
        value=np.random.rand(30) - 0.5,
        group=np.repeat(np.arange(1), 30),
        time_idx=np.tile(np.arange(30), 1),
    )
)
test_data

