#!/usr/bin/env python
# coding: utf-8

# In[1]:


import os
import pandas as pd
import numpy as np
import joblib
import pickle
import warnings
from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import TimeSeriesSplit, GridSearchCV, train_test_split
from sklearn.preprocessing import OneHotEncoder, StandardScaler, LabelEncoder
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline
from sklearn.metrics import accuracy_score

warnings.filterwarnings('ignore')


# In[2]:


def set_wd():
    # Get the GitHub Actions workspace directory
    workspace = os.getenv('GITHUB_WORKSPACE', '.')
    
    # Set the working directory to the folder where the data resides
    cleaned_data = os.path.join(workspace, 'cleaned data')
    website_code = os.path.join(workspace, 'Website code')
    return cleaned_data,website_code


# In[18]:


def load_dataset():
    # Load the dataset
    os.chdir(cleaned_data)
    match_results = pd.read_csv('afl_match_results_cleaned.csv')
    os.chdir(website_code)
    
    # Define the features and the target variable
    X = match_results.drop(columns=['match.homeTeam.name', 'match.awayTeam.name','venue.name','Margin','Result'])  # Drop irrelevant columns
    # Initialize LabelEncoder
    encoder = LabelEncoder()
    # Fit and transform the target variable
    y = encoder.fit_transform(match_results['Result'])

    # Split the data into train (80%) and test (20%) sets
    train_size = int(len(X) * 0.8)
    X_train, X_test = X.iloc[:train_size], X.iloc[train_size:]
    y_train, y_test = y[:train_size], y[train_size:]

    # Identify categorical features (you can list their indices or column names)
    categorical_features = ['weather.weatherType']  # Replace with your actual categorical feature names
    numerical_features = X.select_dtypes(include=['int64', 'float64']).columns.tolist()

    # OneHotEncode categorical features and scale numerical features
    preprocessor = ColumnTransformer(
        transformers=[
            ('num', StandardScaler(), numerical_features),  # Standardize numerical features
            ('cat', OneHotEncoder(handle_unknown='ignore'), categorical_features)  # One-hot encode categorical features
        ]
    )
    
    return encoder,preprocessor,X_train,y_train,X,y


# ### Continue programming

# In[19]:


# Define the neural network model function
def model():
    params = {
    'classifier__n_neighbors': 9,  # Number of neighbors
    'classifier__weights': 'uniform',  # Weight function
    'classifier__p': 1  # 1: Manhattan distance
    }
    
    model = Pipeline(steps=[
        ('preprocessor', preprocessor),
        ('classifier', KNeighborsClassifier(params['classifier__n_neighbors'],
                                        weights=params['classifier__weights'],
                                        p=params['classifier__p']))
        ]
    )
    
    return model

def train_model(X_train, y_train):
    K_NN_model=model()
    # Cross-validation
    tscv = TimeSeriesSplit(n_splits=5)
    fold_accuracies = []
    
    for train_index, val_index in tscv.split(X_train):
        X_t, X_val = X_train.iloc[train_index], X_train.iloc[val_index]
        y_t, y_val = y_train[train_index], y_train[val_index]

        K_NN_model.fit(X_t, y_t)
        
        K_NN_predictions_val = np.argmax(K_NN_model.predict_proba(X_val),axis=1)

        accuracy = accuracy_score(y_val, K_NN_predictions_val)
        fold_accuracies.append(accuracy)

    # After cross-validation, calculate the average accuracy
    average_accuracy = np.mean(fold_accuracies)
    
    final_model = K_NN_model.fit(X, y)
    
    return average_accuracy, final_model


# In[23]:


def save_models(encoder,preprocessor,average_accuracy,final_model):
    os.chdir(website_code)
    with open('encoder.pkl', 'wb') as f:
        pickle.dump(encoder, f)
    with open('preprocessor.pkl', 'wb') as f:
        pickle.dump(preprocessor, f)
    with open('accuracy.pkl', 'wb') as f:
        pickle.dump(average_accuracy, f)
    
    # Save the kNN-model
    joblib.dump(final_model, 'kNN_model.pkl')


# In[52]:


if __name__ == '__main__':
    cleaned_data,website_code=set_wd()
    encoder,preprocessor,X_train,y_train,X,y = load_dataset()
    average_accuracy, final_model = train_model(X_train, y_train)    
    save_models(encoder,preprocessor,average_accuracy,final_model)

