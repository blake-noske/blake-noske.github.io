#!/usr/bin/env python
# coding: utf-8

# In[12]:


from flask import Flask, render_template
import prediction_function  # Replace with the name of your model script

app = Flask(__name__)

@app.route('/')
def home():
    # Call your prediction function and get the output
    predictions = prediction_function.gen_predictions()
    return render_template('index.html', predictions=predictions)

if __name__ == '__main__':
    app.run(debug=True)

