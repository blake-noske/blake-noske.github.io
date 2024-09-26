#!/usr/bin/env python
# coding: utf-8

# In[12]:


from flask import Flask, render_template
from Prediction_function import predictions

app = Flask(__name__)

@app.route('/')
def home():
    # Call your prediction function and get the output
    prediction = predictions()
    #predictions = your_model_script.predict_next_round()
    return render_template('index.html', predictions=predictions)

if __name__ == '__main__':
    app.run(debug=True)


# In[13]:


get_ipython().run_line_magic('tb', '')

