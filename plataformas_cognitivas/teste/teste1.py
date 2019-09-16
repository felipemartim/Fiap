from pandas import DataFrame
import statsmodels.api as sm
from sklearn import linear_model

Stock_Market = {'Year': [2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016],
'Month': [12, 11,10,9,8,7,6,5,4,3,2,1,12,11,10,9,8,7,6,5,4,3,2,1],
'Interest_Rate': [2.75,2.5,2.5,2.5,2.5,2.5,2.5,2.25,2.25,2.25,2,2,2,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75,1.75],
'Unemployment_Rate': [5.3,5.3,5.3,5.3,5.4,5.6,5.5,5.5,5.5,5.6,5.7,5.9,6,5.9,5.8,6.1,6.2,6.1,6.1,6.1,5.9,6.2,6.2,6.1],
'Stock_Index_Price': [1464,1394,1357,1293,1256,1254,1234,1195,1159,1167,1130,1075,1047,965,943,958,971,949,884,866,876,822,704,719]
}

df = DataFrame( Stock_Market, columns= ['Year','Month','Interest_Rate','Unemployment_Rate','Stock_Index_Price'])

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