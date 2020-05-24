library(ggplot2)
library(GGally)
library(DMwR)
library(cluster) 
library(factoextra)

#install.packages("factoextra")
#install.packages("Cluster")
#install.packages("GGally") 
#install.packages("DMwR")
set.seed(5580)
prod <- read.csv("C:/MSCDA/Second Sem/Data Mining/all-quarter-csv.csv",header=TRUE)
nrow(prod) # length of the rows
reqQuarter <- c('quater1.csv')
prod = prod[prod$File %in% reqQuarter,]
prod = prod[,c(-1,-3)]

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

nrow(prod)
head(prod)

#View(prod) # To View the loaded dataset
prod$AVERAGE_PRICE =  as.numeric.factor(prod$AVERAGE_PRICE)

head(prod)
sapply(prod, class)

ggpairs(prod[, which(names(prod) != "StockCode")], upper = list(continuous = ggally_points),lower = list(continuous = "points"), title = "Products before outlier removal") # To visualize data

#boxplot(prod$BASKETS) # For Box and Whisker plot. here prod is dataset and BASKETS is column

#prod.clean <- prod[prod$ITEM_SK != 11740941, ] 

prod.scale = scale(prod[-1])



withinSSrange <- function(data,low,high,maxIter)
{
  withinss = array(0, dim=c(high-low+1));
  for(i in low:high)
  {
    withinss[i-low+1] <- kmeans(data, i, maxIter)$tot.withinss
  }
  withinss
}        


plot(withinSSrange(prod.scale,1,50,150)) # Elbow plot to determine the optimal number of clusters between 1 and 50.

silhouette_score <- function(data, low, high, maxIter){
  vect <- c();
  for(i in low:high){
    kmClust <- kmeans(data, i, maxIter);
    ss <- silhouette(kmClust$cluster, dist(data))
    
    vect[i-1] <- mean(ss[, 3]);
    ss = c();
    
  }
  vect
  
}

avg_sil = silhouette_score(prod.scale, 2,10,150)
 
plot(2:10, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
#fviz_nbclust(prod.scale, kmeans, method='silhouette')

betterKmean <- function(clust_size, data, maxIter, noOfTimes)
{
  finalClust = kmeans(data, clust_size, maxIter);
  for(i in 1:noOfTimes)
  {
    tempClust <- kmeans(data, clust_size, maxIter);
    if(finalClust$tot.withinss < tempClust$tot.withinss){
      finalClust = tempClust;
    }
  }
  finalClust
}



pkm = betterKmean(4,prod.scale, 100, 10) # K-means using k=5 for products based on results of  elbow plot.

prod.realCenters = unscale(pkm$centers, prod.scale) # Denormalize data by reversing scale function

clusteredProd = cbind(prod, pkm$cluster) # Bind clusers to cleansed Data

plot(clusteredProd[,2:5], col=pkm$cluster) # Visualizing clusering results. Here we want all rows so we are not mentioning anything but we want columns only from 2 to 5 (we don't want to visualize first column - ITEM_SK).


write.csv(clusteredProd, file = "C:\\MSCDA\\Second Sem\\Data Mining\\quarter1Prodresults.csv") 
write.csv(prod.realCenters, file = "C:\\MSCDA\\Second Sem\\Data Mining\\quarter1ClusterCenters.csv")

