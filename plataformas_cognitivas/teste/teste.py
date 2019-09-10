from pandas import DataFrame
import pandas as pd
import statsmodels.api as sm
from sklearn import linear_model

df = pd.read_csv("dataset.csv")

x = df[['Interest_Rate','Unemployment_Rate']]
y = df['Stock_Index_Price']

lr_model = linear_model.LinearRegression()
lr_model.fit(x, y)

print('Intercept: \n', lr_model.intercept_ )
print('Coeficients: \n', lr_model.coef_ )

new_Interest_rate = 2.75
new_Unemployment_Rate = 5.3

print('Predict Stock \n', lr_model.predict([[new_Interest_rate,new_Unemployment_Rate]]) )
#print(model.summary())
