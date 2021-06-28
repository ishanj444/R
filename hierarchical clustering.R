library(tidyverse)
library(cluster)
library(factoextra)
input<-read.csv('D:/Iris.csv')
anyNA(input)
cleandata<-input[complete.cases(input),]
df<-cleandata
df <- na.omit(df)
print(df)
View(df)

df <- scale(df)

d <- dist(df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1)

# Compute with agnes
hc2 <- agnes(df, method = "complete")

A1 = c(2,3,5,7,8,10,20,21,23)
A2 = A1
A3 = A1

install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(A1,A2,A3, angle = 25, type = "h")

demo = hclust(dist(cbind(A1,A2,A3)))
plot(demo)