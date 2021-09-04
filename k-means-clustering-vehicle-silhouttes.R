# Install this Once
pkgs <- c('factoextra','NbClust','caret')
install.packages(pkgs)

# Required Libraries
library(caret)
library(factoextra)
library(NbClust)
library(cluster)
library(gridExtra)

# CSV file was exported from xlsx to csv using Microsoft Office Excel 

OverAll_dataset = read.csv('vehicles.csv')

# dropping unwanted columns 
# Samples = Index 
# Class = Class name 
# Both these Columns are not needed in clustering

dataset = subset(OverAll_dataset,select = -c(Samples,Class))

# Box Plot before Scaling
boxplot(dataset)

## Pre-processing tasks ########################################################
# Data Scaling
ScaledData = scale(dataset)
# Outlier Treatment 
for (i in 1:18){
  outliers = boxplot.stats(ScaledData[,i])$out
  outliersIndexes = which(ScaledData[,i]  %in% c(outliers))
  
  if (length(outliersIndexes) > 0){
    TempData <- ScaledData[-c(outliersIndexes), ]
    ScaledData = data.frame(TempData)
    
    OverAll_dataset = OverAll_dataset[-c(outliersIndexes),]
  }  
}
## Defining the number of cluster centers ######################################

## Automatic Techniques for calculation of Optimal Number of Cluster 
# Using the silhouette technique 
fviz_nbclust(ScaledData, kmeans, method = "silhouette")

# Using the Elbow technique
fviz_nbclust(ScaledData, kmeans, method = "wss",)

##  NbClust package uses 30 indices for determining the relevant number of clusters
res = NbClust(data = ScaledData, min.nc=2, max.nc=10, method = "kmeans", index = "all")

## K-Means clustering with Multiple Attempts 
k2 = kmeans(ScaledData, centers = 2, nstart = 25)
k3 = kmeans(ScaledData, centers = 3, nstart = 25)
k4 = kmeans(ScaledData, centers = 4, nstart = 25)
k5 = kmeans(ScaledData, centers = 5, nstart = 25)

# plots to compare
p1 = fviz_cluster(k2, geom = "point", data = ScaledData) + ggtitle("k = 2")
p2 = fviz_cluster(k3, geom = "point",  data = ScaledData) + ggtitle("k = 3")
p3 = fviz_cluster(k4, geom = "point",  data = ScaledData) + ggtitle("k = 4")
p4 = fviz_cluster(k5, geom = "point",  data = ScaledData) + ggtitle("k = 5")

# fviz_cluster() function Takes PCA before visulization

grid.arrange(p1, p2, p3, p4, nrow = 2)

#Confusion Matrix for k =2
table(OverAll_dataset[,20],k2$cluster)

#Confusion Matrix for k =3
table(OverAll_dataset[,20],k3$cluster)

#Confusion Matrix for k =4
table(OverAll_dataset[,20],k4$cluster)

#Confusion Matrix for k =5
table(OverAll_dataset[,20],k5$cluster)

## Winner is k = 2 case ########################################################
# because we can see clearly from graph that in other cases 
# we are over fitting our data points
# Also from NbClust() function which calculates optimal number from 30 different 
# tecniques provides k = 2 as optimal number of clusters Also illustrated in Dindex Graph


#co-ordinates of K = 2 case 

print(k2$centers)