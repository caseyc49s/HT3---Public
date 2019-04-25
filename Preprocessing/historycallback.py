# **********************************************************************************************************************
# Importing Libraries
from sklearn.metrics import roc_curve
from sklearn.metrics import auc
from keras.callbacks import Callback
import numpy as np

class IntervalEvaluation(Callback):
    def __init__(self, testing_data=(), batch_size = (), interval= ()):
        super(IntervalEvaluation, self).__init__()
        self.batch_size = batch_size
        self.interval = interval

        self.X_val, self.y_val = testing_data
        self.score = []
        self.final_score = []
        self.fpr = []
        self.tpr = []

        self.fpr0 = []
        self.tpr0 = []

    def on_train_begin(self, logs={}):

        y_pred = self.model.predict(self.X_val, batch_size=self.batch_size, verbose=0)
        y_pred = y_pred[:, -1]
        y_pred = np.array(y_pred)
        y_pred.transpose()

        fpr_keras, tpr_keras, thresholds_keras = roc_curve(self.y_val, y_pred, pos_label=1)
        auc_keras = auc(fpr_keras, tpr_keras)

        self.score.append(auc_keras)
        self.fpr0.append(fpr_keras)
        self.tpr0.append(tpr_keras)

    def on_epoch_end(self, epoch, logs={}):

        y_pred = self.model.predict(self.X_val, batch_size=self.batch_size, verbose=0)
        y_pred = y_pred[:, -1]
        y_pred = np.array(y_pred)

        fpr_keras, tpr_keras, thresholds_keras = roc_curve(self.y_val, y_pred, pos_label=1)
        auc_keras = auc(fpr_keras, tpr_keras)
        self.score.append(auc_keras)

    def on_train_end(self, logs=None):
        y_pred = self.model.predict(self.X_val, batch_size=self.batch_size, verbose=0)
        y_pred = y_pred[:, -1]
        y_pred = np.array(y_pred)

        fpr_keras, tpr_keras, thresholds_keras = roc_curve(self.y_val, y_pred, pos_label=1)
        auc_keras = auc(fpr_keras, tpr_keras)
        self.final_score.append(auc_keras)
        self.fpr.append(fpr_keras)
        self.tpr.append(tpr_keras)

