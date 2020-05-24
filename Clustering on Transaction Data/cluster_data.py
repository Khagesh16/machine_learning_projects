import pandas as pd 
import numpy  as np
df = pd.read_csv("customerresults_Final.csv")
features = ['CustomerID', 'distinct.products.bought',
       'number.of.products.bought', 'Revenue', 'Vists', 'Avg_spent',
       'pkm$cluster']

df = df[features]
cluster1 = df[df['pkm$cluster']==1]
cluster2 = df[df['pkm$cluster']==2]
cluster3 = df[df['pkm$cluster']==3]
cluster4 = df[df['pkm$cluster']==4]
cluster5 = df[df['pkm$cluster']==5]
cluster6 = df[df['pkm$cluster']==6]
cluster7 = df[df['pkm$cluster']==7]

print('Purchase Freq. for Cluster1', sum(cluster1['Vists'])/(cluster1['number.of.products.bought'].count()))
print('Purchase Freq. for Cluster2', sum(cluster2['Vists'])/(cluster2['number.of.products.bought'].count()))
print('Purchase Freq. for Cluster3', sum(cluster3['Vists'])/(cluster3['number.of.products.bought'].count()))
print('Purchase Freq. for Cluster4', sum(cluster4['Vists'])/(cluster4['number.of.products.bought'].count()))
print('Purchase Freq. for Cluster5', sum(cluster5['Vists'])/(cluster5['number.of.products.bought'].count()))
print('Purchase Freq. for Cluster6', sum(cluster6['Vists'])/(cluster6['number.of.products.bought'].count()))
print('Purchase Freq. for Cluster7', sum(cluster7['Vists'])/(cluster7['number.of.products.bought'].count()))

print('Time Between Purchases for Cluster1', 365/(sum(cluster1['Vists'])/(cluster1['number.of.products.bought'].count())))
print('Time Between Purchases for Cluster2', 365/(sum(cluster2['Vists'])/(cluster2['number.of.products.bought'].count())))
print('Time Between Purchases for Cluster3', 365/(sum(cluster3['Vists'])/(cluster3['number.of.products.bought'].count())))
print('Time Between Purchases for Cluster4', 365/(sum(cluster4['Vists'])/(cluster4['number.of.products.bought'].count())))
print('Time Between Purchases for Cluster5', 365/(sum(cluster5['Vists'])/(cluster5['number.of.products.bought'].count())))
print('Time Between Purchases for Cluster6', 365/(sum(cluster6['Vists'])/(cluster6['number.of.products.bought'].count())))
print('Time Between Purchases for Cluster7', 365/(sum(cluster7['Vists'])/(cluster7['number.of.products.bought'].count())))
