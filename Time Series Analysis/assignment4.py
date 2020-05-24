
#Python Code for Transpose and Aggregation Function

"""
For 15 mins Data:
"""
import pandas as pd 
import numpy as np
df = pd.read_csv('MT123electricity_1HOUR.csv')
df_group_by = df.groupby(['date_actual','final_lag_hour']).sum()
df_group_by.reset_index(inplace=True)
df_group_by.to_csv('Final_hourly_Electricity.csv')
df = df_group_by
table = pd.pivot_table(df,values = 'val' ,index='date_actual',columns='final_lag_hour')
table.reset_index(inplace=True)
del table['date_actual']
table.columns.name = ''
table.to_csv('Final_hourly_Electricity_transposed.csv')

"""
For Hourly Data:
"""
df = pd.read_csv('MT123electricity_15mins_1000rows.csv')
df.reset_index(inplace=True)
df['index'] = df['index'].apply(lambda x: x+1)
def day(raw):
    if raw/7 != 0:
        day_of_week = raw%7
        if day_of_week ==0:
            return 7
        else:
            return day_of_week 
def week(raw):
    day_of_week = raw%7
    if day_of_week ==0:
        return int(raw/7) - 1
    else:
        return int(raw/7) 
    
df['column'] = df['index'].apply(week)
df['rows'] = df['index'].apply(day)
transposed_df = pd.pivot_table(df,values = 'Value' ,index='column',columns='rows')
transposed_df.dropna(inplace=True, axis=0)
transposed_df.to_csv("transposed_data_15mins.csv")

"""
For Daily Data:
"""
def day(raw):
    if raw/7 != 0:
        day_of_week = raw%7
        if day_of_week ==0:
            return 7
        else:
            return day_of_week 
def week(raw):
    if raw%7 ==0:
        return int(raw/7) - 1
    else:
        return int(raw/7)
        
df = pd.read_csv('MT123electricity_1DAY.csv')
df = df.round({'val':2})
df['final_lag_day'][0] = '2011-01-01'
df.drop('date_actual', inplace=True, axis=1)
df['final_lag_day'] = df['final_lag_day'].astype('datetime64[ns]')
df['day_of_year'] = df['final_lag_day'].dt.dayofyear
df = df.groupby('final_lag_day').agg({'val':'sum'})
df.reset_index(inplace=True)
# df.to_csv('day_wise_data.csv')

df['year'] = df['final_lag_day'].dt.year
df['day_of_week'] = df['final_lag_day'].dt.dayofyear.apply(day)
df['week_num'] = df['final_lag_day'].dt.dayofyear.apply(week)
transposed_df = pd.pivot_table(df,values = 'val' ,index=['year','week_num'],columns='day_of_week')
transposed_df.dropna(inplace=True)
transposed_df.to_csv('day_wise_data_transformed.csv')
