#!/usr/bin/env python
# coding: utf-8

# In[2]:


import pandas as pd
import numpy as np
import tensorflow as tf
import xgboost as xgb
import warnings
import shap
import matplotlib.pyplot as plt
import joblib
import sklearn
import pickle
import os
import lime
import lime.lime_tabular
import re

from tensorflow.keras.models import Sequential, save_model
from tensorflow.keras.layers import Dense, Dropout, BatchNormalization
from tensorflow.keras.callbacks import EarlyStopping, ReduceLROnPlateau
from tensorflow.keras.wrappers.scikit_learn import KerasClassifier
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler,MinMaxScaler
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import accuracy_score
from sklearn.datasets import make_classification
from sklearn.model_selection import GridSearchCV, KFold, RandomizedSearchCV, cross_val_score,TimeSeriesSplit
from sklearn.pipeline import Pipeline
from tensorflow.keras.regularizers import l2
from sklearn.utils.class_weight import compute_class_weight
from scipy.stats import uniform, randint
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import RandomForestClassifier
from xgboost import XGBClassifier
from sklearn.ensemble import VotingClassifier
from sklearn.linear_model import LogisticRegression
from IPython.display import display, clear_output

warnings.filterwarnings('ignore')


# In[5]:


def set_wd():
    # Get the GitHub Actions workspace directory
    workspace = os.getenv('GITHUB_WORKSPACE', '.')
    
    # Set the working directory to the folder where the data resides
    cleaned_data = os.path.join(workspace, 'cleaned data')
    website_code = os.path.join(workspace, 'Website code')
    return cleaned_data,website_code


# In[3]:


def load_dataset():
    # Load the dataset
    os.chdir(cleaned_data)
    match_results = pd.read_csv('afl_match_results_cleaned.csv')
    os.chdir(website_code)
    
    # Define the features and the target variable
    weather_dummies = pd.get_dummies(match_results['weather.weatherType'])
    X = match_results.drop(columns=['match.homeTeam.name', 'match.awayTeam.name','venue.name','Margin','Result','weather.weatherType']).astype('float64')  # Drop irrelevant columns
    X = pd.concat([X, weather_dummies], axis=1)
    y = match_results['Result']  # BW, LW, D, LL, BL
    
    # Assuming 'weather_columns' is a list of your dummy weather variables
    weather_columns = weather_dummies.columns  # Replace with actual weather columns
    discrete_columns = ['Home.Team.Venue.Win.Streak', 'Away.Team.Venue.Win.Streak','Home.Win.Streak'] 
    continuous_columns = [col for col in X.columns if col not in weather_columns and col not in discrete_columns]
    
    # ColumnTransformer to apply StandardScaler only to continuous features
    preprocessor = ColumnTransformer(
        transformers=[
            ('num', StandardScaler(), continuous_columns),
            ('disc', MinMaxScaler(), discrete_columns),
            ('weather', 'passthrough', weather_columns)  # Weather columns are passed through unchanged
        ]
    )
    
    # Initialize LabelEncoder
    encoder = LabelEncoder()
    
    # Fit and transform the target variable
    y_encoded = encoder.fit_transform(y)
    
    cutoff_index = int(0.8 * len(match_results))
    
    y_train = encoder.fit_transform(y)    
    
    # Standardize the features
    X_train = preprocessor.fit_transform(X)

    return encoder,preprocessor,X_train,y_train,X,y


# ### Run at beginning of season

# In[ ]:


# # TimeSeriesSplit for time-aware cross-validation
# tscv = TimeSeriesSplit(n_splits=3)

# # Function to create the Keras model
# def create_model(optimizer='adam', dropout_rate=0.3, neurons=64, learn_rate=0.01):
#     model = Sequential([
#         Dense(neurons, kernel_initializer='he_uniform', activation='relu', kernel_regularizer=l2(0.001), input_dim=X_train.shape[1]),
#         BatchNormalization(),
#         Dropout(dropout_rate),
#         Dense(32, kernel_initializer='he_uniform', activation='relu', kernel_regularizer=l2(0.001)),
#         BatchNormalization(),
#         Dropout(dropout_rate),
#         Dense(16, kernel_initializer='he_uniform', activation='relu', kernel_regularizer=l2(0.01)),
#         BatchNormalization(),
#         Dropout(dropout_rate),
#         Dense(5, kernel_initializer='he_uniform', activation='softmax')
#     ])
#     model.compile(optimizer=tf.keras.optimizers.SGD(learning_rate=learn_rate, name=optimizer), loss='sparse_categorical_crossentropy', metrics=['accuracy'])
#     return model


# # Example: Cross-validate Neural Network (Keras) base model
# model = KerasClassifier(build_fn=create_model, verbose=0)

# # Define the grid of hyperparameters to search
# nn_param_grid = {
#     'batch_size': [32, 64, 128],
#     'epochs': [50],
#     'optimizer': ['adam', 'sgd'],
#     'dropout_rate': [0.2, 0.3, 0.5],
#     'neurons': [32, 64, 128],
#     'learn_rate': [0.001, 0.01, 0.1]
# }

# # Initialize GridSearchCV
# grid_search_nn = GridSearchCV(
#     estimator=model,
#     param_grid=nn_param_grid,
#     cv=tscv,
#     scoring='accuracy',
#     n_jobs=-1
# )

# # Fit the model using GridSearchCV
# grid_search_nn.fit(X_train, y_train)

# # Best Neural Network model
# best_nn_model = grid_search_nn.best_estimator_
# print(f"Best NN params: {grid_search_nn.best_params_}")
# print(f"Best NN accuracy: {grid_search_nn.best_score_}")


# In[ ]:


# # Example: Cross-validate Random Forest base model
# rf_model = RandomForestClassifier()

# rf_param_grid = {
#     'n_estimators': [100, 200],
#     'max_depth': [10, 20, None],
#     'min_samples_split': [2, 5, 10]
# }

# grid_search_rf = GridSearchCV(
#     estimator=rf_model,
#     param_grid=rf_param_grid,
#     cv=tscv,
#     scoring='accuracy',
#     verbose=1,
#     n_jobs=-1
# )

# grid_search_rf.fit(X_train, y_train)

# # Best Random Forest model
# best_rf_model = grid_search_rf.best_estimator_
# print(f"Best RF params: {grid_search_rf.best_params_}")
# print(f"Best RF accuracy: {grid_search_rf.best_score_}")


# In[ ]:


# # Define the XGBoost model
# xgb_model = xgb.XGBClassifier(use_label_encoder=False, eval_metric='mlogloss')

# # Define the hyperparameters to tune
# xgb_param_grid = {
#     'n_estimators': [100, 200],
#     'max_depth': [3, 6, 9],
#     'learning_rate': [0.01, 0.1, 0.2],
#     'subsample': [0.8, 1.0],
#     'colsample_bytree': [0.8, 1.0]
# }

# # Grid search for hyperparameter tuning
# grid_search_xgb = GridSearchCV(
#     estimator=xgb_model,
#     param_grid=xgb_param_grid,
#     cv=tscv,
#     scoring='accuracy',
#     verbose=1,
#     n_jobs=-1
# )

# # Fit the grid search
# grid_search_xgb.fit(X_train, y_train)

# # Best XGBoost model
# best_xgb_model = grid_search_xgb.best_estimator_
# print(f"Best XGBoost params: {grid_search_xgb.best_params_}")
# print(f"Best XGBoost accuracy: {grid_search_xgb.best_score_}")


# In[ ]:


# # Function to create the Keras model
# def create_model(optimizer='adam', dropout_rate=0.3, neurons=64, learn_rate=0.01):
#     model = Sequential([
#         Dense(128, kernel_initializer='he_uniform', activation='relu', kernel_regularizer=l2(0.001), input_dim=X_train.shape[1]),
#         BatchNormalization(),
#         Dropout(0.5),
#         Dense(32, kernel_initializer='he_uniform', activation='relu', kernel_regularizer=l2(0.001)),
#         BatchNormalization(),
#         Dropout(0.5),
#         Dense(16, kernel_initializer='he_uniform', activation='relu', kernel_regularizer=l2(0.01)),
#         BatchNormalization(),
#         Dropout(0.5),
#         Dense(5, kernel_initializer='he_uniform', activation='softmax')
#     ])
#     model.compile(optimizer=tf.keras.optimizers.SGD(learning_rate=0.1, name='adam'), loss='sparse_categorical_crossentropy', metrics=['accuracy'])
#     return model

# # Example: Cross-validate Neural Network (Keras) base model
# model = KerasClassifier(build_fn=create_model, verbose=0)

# # Initialize base models
# nn_model = KerasClassifier(build_fn=lambda: create_model(X_train.shape[1]), epochs=100, batch_size=32, verbose=0)
# rf_model = RandomForestClassifier(n_estimators=100, max_depth=20, min_samples_split=5)
# xgb_model = XGBClassifier(use_label_encoder=False, eval_metric='mlogloss', colsample_bytree=0.8,
#                           learning_rate=0.2, max_depth=3, n_estimators=100, subsample=0.8)

# # Fit the base models
# nn_model.fit(X_train, y_train)
# rf_model.fit(X_train, y_train)
# xgb_model.fit(X_train, y_train)


# rf_predictions_train = rf_model.predict_proba(X_train)
# xgb_predictions_train = xgb_model.predict_proba(X_train)
# nn_predictions_train = nn_model.predict_proba(X_train)

# meta_train_X = np.hstack([rf_predictions_train, xgb_predictions_train, nn_predictions_train])
# meta_train_y = y_train  # Your training labels

# meta_model = LogisticRegression()

# # Set up the TimeSeriesSplit for time-aware cross-validation
# tscv = TimeSeriesSplit(n_splits=5)

# # Define the hyperparameters to tune for Logistic Regression
# param_grid = {
#     'C': [0.01, 0.1, 1, 10, 100],  # Regularization strength
#     'solver': ['liblinear', 'lbfgs'],  # Different solvers for logistic regression
#     'max_iter': [100, 200]  # Maximum number of iterations
# }

# # Set up GridSearchCV to find the best hyperparameters
# grid_search = GridSearchCV(
#     estimator=meta_model,
#     param_grid=param_grid,
#     cv=tscv,
#     scoring='accuracy',  # You can change this to 'f1', 'roc_auc', etc., depending on your metric of choice
#     verbose=1,
#     n_jobs=-1  # Use all available cores for parallel processing
# )

# # Perform the hyperparameter search using GridSearchCV
# grid_search.fit(meta_train_X, meta_train_y)

# # Get the best meta-model from the grid search
# best_meta_model = grid_search.best_estimator_

# # Output the best hyperparameters
# print(f"Best hyperparameters: {grid_search.best_params_}")
# print(f"Best cross-validation accuracy: {grid_search.best_score_}")


# ### Continue programming

# In[4]:


# Define the neural network model function
def create_simple_model(input_dim):
    model = Sequential([
        Dense(128, kernel_initializer='he_uniform', activation='relu', kernel_regularizer=l2(0.001), input_dim=input_dim),
        BatchNormalization(),
        Dropout(0.5),
        Dense(32, kernel_initializer='he_uniform', activation='relu', kernel_regularizer=l2(0.001)),
        BatchNormalization(),
        Dropout(0.5),
        Dense(16, kernel_initializer='he_uniform', activation='relu', kernel_regularizer=l2(0.01)),
        BatchNormalization(),
        Dropout(0.5),
        Dense(5, kernel_initializer='he_uniform', activation='softmax')
    ])
    model.compile(optimizer=tf.keras.optimizers.SGD(learning_rate=0.1, name='adam'), loss='sparse_categorical_crossentropy', metrics=['accuracy'])
    return model

def train_model(X_train, y_train):
    # Initialize base models
    nn_model = KerasClassifier(build_fn=lambda: create_simple_model(X_train.shape[1]), epochs=100, batch_size=32, verbose=0)
    rf_model = RandomForestClassifier(n_estimators=100, max_depth=20, min_samples_split=5)
    xgb_model = XGBClassifier(use_label_encoder=False, eval_metric='mlogloss', colsample_bytree=0.8,
                              learning_rate=0.2, max_depth=3, n_estimators=100, subsample=0.8)
    
    # Fit the base models
    rf_model.fit(X_train, y_train)
    xgb_model.fit(X_train, y_train)
    
    # Cross-validation
    tscv = TimeSeriesSplit(n_splits=5)
    fold_accuracies = []
    
    for train_index, val_index in tscv.split(X_train):
        X_t, X_val = X_train[train_index], X_train[val_index]
        y_t, y_val = y_train[train_index], y_train[val_index]
        
        # Re-instantiate KerasClassifier for each fold with proper input_dim
        nn_model = KerasClassifier(build_fn=lambda: create_simple_model(X_train.shape[1]), epochs=100, batch_size=32, verbose=0)
        
        # Fit base models
        rf_model.fit(X_t, y_t)
        xgb_model.fit(X_t, y_t)
        
        # Get predictions from the neural network for the validation fold
        nn_model.fit(X_t, y_t)  # Fit the neural network on the training fold
        nn_predictions_val = nn_model.predict_proba(X_val)  # Get predictions for the validation fold
        
        # Get predictions from the other base models for the validation fold
        rf_predictions_val = rf_model.predict_proba(X_val)
        xgb_predictions_val = xgb_model.predict_proba(X_val)
        
        # Combine predictions from all models for stacking
        meta_features_val = np.hstack([rf_predictions_val, xgb_predictions_val, nn_predictions_val])
        
        # Train a meta-model (LogisticRegression in this case) on the combined predictions
        meta_model = LogisticRegression(C=100, max_iter=100, solver='liblinear')
        meta_model.fit(meta_features_val, y_val)
        
        # Evaluate the meta-model on the validation set
        meta_predictions_val = meta_model.predict(meta_features_val)
        accuracy = accuracy_score(y_val, meta_predictions_val)
        fold_accuracies.append(accuracy)

    # After cross-validation, calculate the average accuracy
    average_accuracy = np.mean(fold_accuracies)

    return average_accuracy,rf_model,xgb_model


# In[5]:


def final_models(X,y):
    # Combine scaled features for the entire dataset
    X_full_scaled = preprocessor.fit_transform(X)
    y_encoded = encoder.fit_transform(y)
    
    # Train base models on the full dataset
    rf_model.fit(X_full_scaled, y_encoded)
    xgb_model.fit(X_full_scaled, y_encoded)
    nn_model_full = KerasClassifier(build_fn=lambda: create_simple_model(X_full_scaled.shape[1]), 
                                    epochs=100, batch_size=32, verbose=0)
    nn_model_full.fit(X_full_scaled, y_encoded)
    
    # Generate predictions (probabilities) for stacking
    rf_predictions_full = rf_model.predict_proba(X_full_scaled)
    xgb_predictions_full = xgb_model.predict_proba(X_full_scaled)
    nn_predictions_full = nn_model_full.predict_proba(X_full_scaled)
    
    # Stack predictions to form meta-features
    meta_features_full = np.hstack([rf_predictions_full, xgb_predictions_full, nn_predictions_full])
    
    # Train meta-model (Logistic Regression) on the full dataset's meta-features
    meta_model_full = LogisticRegression()
    meta_model_full.fit(meta_features_full, y_encoded)

    return nn_model_full,meta_model_full
    
    print("Final ensemble model trained on the full dataset.")


# In[6]:


def save_models(encoder,preprocessor,average_accuracy,rf_model,xgb_model,nn_model_full,meta_model_full):
    with open('encoder.pkl', 'wb') as f:
        pickle.dump(encoder, f)
    with open('preprocessor.pkl', 'wb') as f:
        pickle.dump(preprocessor, f)
    with open('accuracy.pkl', 'wb') as f:
        pickle.dump(average_accuracy, f)
    
    # Save RandomForest model
    joblib.dump(rf_model, 'rf_model.pkl')
    
    # Save XGBoost model
    joblib.dump(xgb_model, 'xgb_model.pkl')
    
    # Save the neural network model
    save_model(nn_model_full.model, 'nn_model.h5')
    
    # Save the meta-model (Logistic Regression)
    joblib.dump(meta_model_full, 'meta_model.pkl')


# In[6]:


if __name__ == '__main__':
    cleaned_data,website_code=set_wd()
    encoder,preprocessor,X_train,y_train,X,y = load_dataset()
    average_accuracy, rf_model, xgb_model = train_model(X_train, y_train)    
    nn_model_full, meta_model_full = final_models(X,y)
    save_models(encoder,preprocessor,average_accuracy,rf_model,xgb_model,nn_model_full,meta_model_full)

