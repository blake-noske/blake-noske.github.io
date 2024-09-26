#!/usr/bin/env python
# coding: utf-8

# In[3]:


import warnings
warnings.filterwarnings('ignore')

import pandas as pd
import numpy as np
import ipywidgets as widgets
from IPython.display import display
import tensorflow as tf
from tensorflow.keras.models import load_model
import joblib
import math
import pickle
import os

# Load the datasets
os.chdir('C:\\Users\\blake\\Desktop\\AFL Odds\\cleaned data')
match_results = pd.read_csv('afl_match_results_cleaned.csv')
team_stats = pd.read_csv('afl_team_stats_cleaned.csv')
win_streaks = pd.read_csv('afl_team_streaks_cleaned.csv',index_col=0)
venue_streaks = pd.read_csv('afl_venue_streaks_cleaned.csv',index_col=0)
team_form = pd.read_csv('afl_team_form_cleaned.csv',index_col=0)
fixture = pd.read_csv('afl_fixture_cleaned.csv',index_col=0)
os.chdir('C:\\Users\\blake\\Desktop\\AFL Odds\\python scripts')

with open('accuracy.pkl', 'rb') as f:
    a = pickle.load(f)

# Load RandomForest model
rf_model = joblib.load('rf_model.pkl')

# Load XGBoost model
xgb_model = joblib.load('xgb_model.pkl')

# Load the neural network model
nn_model_full = load_model('nn_model.h5')

# Load the meta-model (Logistic Regression)
meta_model_full = joblib.load('meta_model.pkl')

with open('encoder.pkl', 'rb') as f:
    encoder = pickle.load(f)
with open('preprocessor.pkl', 'rb') as f:
    preprocessor = pickle.load(f)


# In[4]:


weather_categories = ['CLEAR_NIGHT','MOSTLY_SUNNY','OVERCAST','RAIN','SUNNY','THUNDERSTORMS','WINDY']  # Add all weather types you used

# Create a dictionary where all categories are 0
weather_dict = {category: 0 for category in weather_categories}


# In[7]:


def round_decimals_up(number:float, decimals:int=2):
    """
    Returns a value rounded up to a specific number of decimal places.
    """
    if not isinstance(decimals, int):
        raise TypeError("decimal places must be an integer")
    elif decimals < 0:
        raise ValueError("decimal places has to be 0 or more")
    elif decimals == 0:
        return math.ceil(number)

    factor = 10 ** decimals
    return math.ceil(number * factor) / factor

def extract_features(home_team, away_team, venue,weather):
    # Get the weighted average stats for home and away teams
    home_stats = team_stats[team_stats['Team'] == home_team].iloc[:, 1:].values.flatten()
    away_stats = team_stats[team_stats['Team'] == away_team].iloc[:, 1:].values.flatten()
    
    # Get the win streaks
    team_win_streak = win_streaks.loc[away_team, home_team].flatten()
    home_venue_streak = venue_streaks.loc[home_team, venue].flatten()
    away_venue_streak = venue_streaks.loc[away_team, venue].flatten()
    home_team_form = team_form.loc[team_form['Team'] == home_team, 'Current.Form'].values[0].flatten()
    away_team_form = team_form.loc[team_form['Team'] == home_team, 'Current.Form'].values[0].flatten()
    
    weather_dict[weather] = 1
    # Convert the dictionary to a DataFrame (single row)
    weather_df = pd.DataFrame([weather_dict]).values.flatten()
    
    # Combine all features into a single array
    features = np.concatenate([
    home_stats, home_team_form,
    away_stats, away_team_form,
    home_venue_streak,away_venue_streak,team_win_streak,
    weather_df
    ])

    column_names = match_results.drop(columns=['match.homeTeam.name', 'match.awayTeam.name','venue.name','Margin','Result','weather.weatherType',
                                          'Home.Team.Venue.Win.Streak', 'Away.Team.Venue.Win.Streak','Home.Win.Streak']).columns  # Replace with actual feature names
    column_names = column_names.append(pd.Index(['Home.Team.Venue.Win.Streak', 'Away.Team.Venue.Win.Streak','Home.Win.Streak'])).append(pd.Index(weather_categories))


    new_data_df = pd.DataFrame(features.reshape(1, -1), columns=column_names)

    features = preprocessor.transform(new_data_df)  # Normalize features
    
    return features

def make_prediction(home_team, away_team, venue, weather):
    features = extract_features(home_team, away_team, venue,weather)

    rf_predictions_new = rf_model.predict_proba(features)
    xgb_predictions_new = xgb_model.predict_proba(features)
    nn_predictions_new = nn_model_full.predict(features)

    # Stack predictions to form meta-features
    meta_features_new = np.hstack([rf_predictions_new, xgb_predictions_new, nn_predictions_new])

    # Get final ensemble prediction
    final_prediction = meta_model_full.predict_proba(meta_features_new)
    
    #prediction_probs = model.predict(features)[0]
    predicted_class_index = np.argmax(final_prediction)
    predicted_class = encoder.inverse_transform([predicted_class_index])[0]  # Decode the prediction
    max_prob = final_prediction[0][predicted_class_index]  # Get the maximum probability and its corresponding class
    acc = max_prob * a
    max_prob_percent = f"{acc * 100:.2f}%"
    market = f"{round_decimals_up(1 / acc,2):.2f}"

    return predicted_class,max_prob_percent,market


# In[14]:


preds = []
def predictions(fixture):
    for h,a,v,w in zip(list(fixture['home.team.name']),
                                             list(fixture['away.team.name']),
                                             list(fixture['venue.name']),
                                             list(fixture['Next_round_weather'])):
        (r,p,m)=make_prediction(h,a,v,w)

        if r == "BW":
            r = f'{h} 40+'
        if r == "LW":
            r = f'{h} 1-39'
        if r == "LL":
            r = f'{a} 1-39'
        if r == "BL":
            r = f'{a} 40+'
        if r == "D":
            r = f'draw'
    
        new = [
            {"Match": f"{h} vs {a}", "Venue": f"{v}" , "Prediction": f"{r}",
             "Market": f"${m}", "Probability": f"{p}"}
        ]
        preds.append(new)
    return preds

