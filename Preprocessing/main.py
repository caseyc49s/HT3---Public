# **********************************************************************************************************************
# Importing Libraries
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from keras.models import Sequential
from keras.layers import Dense
from keras.layers.recurrent import LSTM
from keras.optimizers import Adam
from keras.utils import np_utils
from sklearn.metrics import roc_curve, auc
from keras.utils.vis_utils import plot_model
import historycallback

# **********************************************************************************************************************
# Reading CSV file for Training Set
training_set = pd.read_csv('/home/user/Model Data/FINAL DATA/Train/trainshift300.csv')
training_set.head()

# Selecting Training Set Data (All columns)---------------------------------------------
training_set = training_set.iloc[:, :]
training_set.head()
print ('\nTraining Set Data Ready...')

# Converting into 2D Array
training_set = training_set.values
training_set

# Scaling of Data [Normalization]
from sklearn.preprocessing import MinMaxScaler
sc = MinMaxScaler()
print ('\nNormalizing Training Set Data...')
training_set = sc.fit_transform(training_set)
training_set = sc.fit_transform(training_set)
training_set

# Length of the Data Set
train = len(training_set)
print ('\nSize of Training Set:'), train

# Defining Training Data
X_train = training_set[:,1:25] #All except last column
Y_train = training_set[:,-1].reshape(len(training_set), 1)

X_train_check = pd.DataFrame(X_train).to_csv('/home/user/Model Data/726/xtraincheck300.csv')
Y_train_check = pd.DataFrame(Y_train).to_csv('/home/user/Model Data/726/ytrain200check300.csv')

# Reshaping for Keras (Reshape into 3 dimensions, [batch_size, timesteps, input_dim])
X_train = np.reshape(X_train, (X_train.shape[0], 1, X_train.shape[1]))

print("\nTraining data ready.")

# **********************************************************************************************************************
# Reading CSV file for Validation Set
validation_set = pd.read_csv('/home/user/Model Data/FINAL DATA/Validate/validationshift300.csv')
validation_set.head()

# Selecting Training Set Data (All columns)---------------------------------------------
validation_set = validation_set.iloc[:, :]
validation_set.head()
print ('\nValidation Set Data Ready...')

# Converting into 2D Array
validation_set = validation_set.values
validation_set

# Scaling of Data [Normalization]
from sklearn.preprocessing import MinMaxScaler
sc = MinMaxScaler()
print ('\nNormalizing Validation Set Data...')
validation_set = sc.fit_transform(validation_set)
validation_set = sc.fit_transform(validation_set)
validation_set

# Length of the Data Set
validation = len(validation_set)
print ('\nSize of Validation Set:'), validation

# Defining Training Data
X_val = validation_set[:,1:25] #All except last column
Y_val = validation_set[:,-1].reshape(len(validation_set), 1)
X_val_check = pd.DataFrame(X_val).to_csv('/home/user/Model Data/726/xvalcheck300.csv')
Y_val_check = pd.DataFrame(Y_val).to_csv('/home/user/Model Data/726/yvalcheck300.csv')
Y_val = np_utils.to_categorical(Y_val, 2)

# Reshaping for Keras (Reshape into 3 dimensions, [batch_size, timesteps, input_dim])
X_val = np.reshape(X_val, (X_val.shape[0], 1, X_val.shape[1]))

print("\nValidation data ready.")

# **********************************************************************************************************************
# Reading CSV file for Test Set
test_set = pd.read_csv('/home/user/Model Data/FINAL DATA/Test/testshift300.csv')
test_set.head()

# Selecting Test Set Data
test_set = test_set.iloc[:,:]

# Coverting into 2D Array
test_set = test_set.values

# Getting the Predicted Value
data = test_set[:,1:25]
datasize = len(data)
reshapedinputs = data.reshape(datasize,-1)
X_test = [reshapedinputs]
X_test = sc.fit_transform(reshapedinputs)

X_test_check = pd.DataFrame(X_test).to_csv('/home/user/Model Data/726/xtestcheck300.csv')

Y_test = test_set[:,-1].reshape(len(test_set), 1)
print ('\nGround Truth is:\n'), Y_test

# Length of the Test Data Set
test = len(test_set)
print ('\nSize of Test Set:'), test
print ('\nTesting data ready.\n')

# Reshaping for Keras (reshape into 3 dimensions, [batch_size, timesteps, input_dim])
X_test = np.reshape(X_test, (X_test.shape[0], 1, X_test.shape[1]))


# **********************************************************************************************************************

def define_model(X_train, Y_train):
    # Model Hyperparameters
    nb_classes = 2
    nb_epoch = 15
    batch_size = 40 #40 80 toka should be ok
    learning_rate = 0.0001

    # Model Parameters: Units per Layer
    seq_len = 300
    hid_layer_1 = 64
    hid_layer_2 = 32
    out_layer = nb_classes

    batchinputshape = (batch_size, X_train.shape[1], X_train.shape[2])
    print ('\nConverting labels to categorical binary indicating classes...')
    Y_train = np_utils.to_categorical(Y_train, nb_classes)

    # Building the Model
    print('\nBuilding Model...')

    model = Sequential()

    model.add(LSTM(seq_len, batch_input_shape=batchinputshape, stateful=True, return_sequences=True, implementation=2,
                   recurrent_dropout=0.2))
    model.add(LSTM(hid_layer_1, stateful=True, return_sequences=False, implementation=2, recurrent_dropout=0.2))
    model.add(Dense(hid_layer_2, activation='sigmoid'))
    model.add(Dense(out_layer, activation='softmax'))

    adam = Adam(lr=learning_rate, beta_1=0.9, beta_2=0.999, epsilon=1e-08, decay=0.0)

    model.compile(loss='binary_crossentropy', optimizer=adam, metrics=['accuracy'])

    print('Model Compiled\n\nParameters as shown:\n')

    model.summary()

    print('Training Model Now...')
    return model, Y_train, batch_size, nb_epoch

conditions = ["All Data", "No Surrounding Vehicles"]

def train_validate(X_train, Y_train, batch_size, nb_epoch, model) :

    X_train = X_train
    Y_train = Y_train

    X_validation = X_val
    Y_validation = Y_val

    y_validation_val = Y_validation[:, -1]
    y_validation_val = np.array(y_validation_val)
    y_validation_val.transpose()

    y_train_val = Y_train[:, -1]
    y_train_val = np.array(y_train_val)
    y_train_val.transpose()

    ival = historycallback.IntervalEvaluation(testing_data=(X_train, y_train_val), batch_size=(batch_size), interval=(5))
    ival2 = historycallback.IntervalEvaluation(testing_data=(X_validation, y_validation_val), batch_size=(batch_size), interval=(5))

    model.fit(X_train, Y_train, validation_data=[X_val, Y_val], epochs=nb_epoch, batch_size=batch_size, callbacks=[ival, ival2])
    plot_model(model, to_file='/home/user/Model Data/model.png', show_shapes=True, show_layer_names=True)

    return ival, ival2

for i, condition in enumerate(conditions):
    evaluation_score = [],[]

    if condition == "All Data":
        X_train = X_train
        model, Y_train_all, batch_size, nb_epoch = define_model(X_train, Y_train)
        evaluation_score_all_train, evaluation_score_all_validation = train_validate(X_train, Y_train_all, batch_size, nb_epoch, model)

    # elif condition == "No Surrounding Vehicles":
    #     X_train_ego = X_train[:,:,:8]
    #     model, Y_train_ego, batch_size, nb_epoch = define_model(X_train_ego, Y_train)
    #     evaluation_score_ego_train, evaluation_score_ego_validation = train_validate(X_train_ego, Y_train_ego, batch_size, nb_epoch, model)

# **********************************************************************************************************************
# Testing Model and ROC
Y_pred = model.predict(X_test, verbose=1, batch_size=batch_size)
predicted_probability = Y_pred[:, -1]
fpr_keras, tpr_keras, thresholds_keras = roc_curve(Y_test, predicted_probability)
auc_keras = auc(fpr_keras, tpr_keras)
predicted_probability = pd.DataFrame(predicted_probability).to_csv('/home/user/Model Data/726/predicted_probabilities_300t3.csv')

# **********************************************************************************************************************
# Graphs
fig1 = plt.figure('Figure 1: 300t3')
fig1.patch.set_facecolor('w')
plt.plot(evaluation_score_all_train.score, label='Train (Before Train, AUC = {:.3f} )'.format(evaluation_score_all_train.score[0]))
plt.plot(evaluation_score_all_validation.score, label='Validation (Before Train, AUC = {:.3f} )'.format(evaluation_score_all_validation.score[0]))
plt.title('AUC per Epoch for Train and Validation Set (All Vehicles)')
plt.ylabel('AUC')
plt.xlabel('Epoch')
plt.legend(loc='best')
plt.show()

fig2 = plt.figure('Figure 2: 300t3')
fig2.patch.set_facecolor('w')
plt.plot([0, 1], [0, 1], 'k--')
#plt.plot(evaluation_score_all_train.fpr0[0][:], evaluation_score_all_train.tpr0[0][:], color='b', linestyle = '-.', label='Train (Before Train, AUC = {:.3f} )'.format(evaluation_score_all_train.score[0]), linewidth=4)
#plt.plot(evaluation_score_all_validation.fpr0[0][:], evaluation_score_all_validation.tpr0[0][:], color='g', linestyle = '-.',label='Validation (Before Train, AUC = {:.3f} )'.format(evaluation_score_all_validation.score[0]), linewidth=4)
plt.plot(evaluation_score_all_train.fpr[0][:], evaluation_score_all_train.tpr[0][:], color='b',label='Train (Trained, AUC = {:.3f} )'.format(evaluation_score_all_train.score[-1]), linewidth=4)
plt.plot(evaluation_score_all_validation.fpr[0][:], evaluation_score_all_validation.tpr[0][:],  color='g', label='Validation (Trained, AUC = {:.3f} )'.format(evaluation_score_all_validation.score[-1]), linewidth=4)
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curves: All Vehicles')
plt.legend(loc='upper center', bbox_to_anchor=(0.5, -0.05), fancybox=True, shadow=True, ncol=5)
plt.show()

print evaluation_score_all_validation.score[-1]
# fig3 = plt.figure('Figure 3')
# fig3.patch.set_facecolor('w')
# plt.bar([1,2], [evaluation_score_all_validation.score[-1], evaluation_score_ego_validation.score[-1]])
# plt.xlabel('Experiment Type')
# plt.ylabel('AUC')
# plt.title('AUC of using All Vehicles vs Ego Vehicle Only')
# plt.show()
#
fig4 = plt.figure('Figure 4: 300t3')
fig4.patch.set_facecolor('w')
plt.plot([0, 1], [0, 1], 'k--')
plt.plot(fpr_keras, tpr_keras, label='AUC = {:.3f}'.format(auc_keras), linewidth=4, color='r')
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('ROC Curve: Test Data (All Vehicles)')
plt.legend(loc='best')
plt.show()
