
rm(list=ls())#Removing all variables
#install.packages("readxl")

library("readxl")#library to import data

my_data <- read.csv("C:\\Users\\IkeOl\\OneDrive\\University\\Final\\Project\\Data\\Mall_Customers.csv")


summary(my_data)

hist(my_data$Age, main = "Histogram of Age",xlab = "Age")
hist(my_data$Annual.Income..k.., main = "Histogram of Annual Imcome",xlab = "Annual Income (K$)")
hist(my_data$Spending.Score..1.100., main = "Histogram of Spending Score",xlab = "Spending Score")

counts <- table(my_data$Gender)
barplot(counts, main="Gender Distribution",
        xlab="Gender")

typeof(my_data$Gender)
class(my_data$Gender)
typeof(my_data$Age)
class(my_data$Age)
typeof(my_data$Annual.Income..k..)
class(my_data$Annual.Income..k..)
typeof(my_data$Spending.Score..1.100.)
class(my_data$Spending.Score..1.100.)



sapply(my_data, function(x) which(is.na(x)))
my_data$Spending.Score..1.100.[69] <- mean(my_data$Spending.Score..1.100.)
my_data$Age[168] <- mean(my_data$Age)
my_data$Age[200] <- mean(my_data$Age)
sapply(my_data, function(x) which(is.na(x)))

#my_data$Gender = factor(my_data$Gender, levels = c('1','0'), labels = c(Male,Female))
drop <-c("CustomerID")
my_data = my_data[,!(names(my_data)%in% drop)]



#install.packages("factoextra")
library(factoextra)

Scale_Data <- scale(my_data[2:4])
Scale_Data
fviz_nbclust(Scale_Data, kmeans, method = "wss") + labs(subtitle = "Elbow Method")

#k=4
km4 <- kmeans(Scale_Data, 4, nstart = 1000)
km4
km4.cluster <- km4$cluster
fviz_cluster(list(data = Scale_Data, cluster = km4.cluster)) 
#k=5
km5 <- kmeans(Scale_Data, 5, nstart = 1000)
km5
km5.cluster <- km5$cluster
fviz_cluster(list(data = Scale_Data, cluster = km5.cluster)) 
#k=6
km6 <- kmeans(Scale_Data, 6, nstart = 1000)
km6
km6.cluster <- km6$cluster

fviz_cluster(list(data = Scale_Data, cluster = km6.cluster)) 
Clusters <- data.frame(my_data,km6$cluster)

#install.packages("openxlsx", dependencies=TRUE)
library(openxlsx)
#install.packages("writexl")
#writexl::write_xlsx(Clusters,"C:\\Users\\IkeOl\\OneDrive\\University\\Final\\Project\\Data\\ClusterOutput2.xlsx")


#hierarchical
dist_my_data <- dist(Scale_Data)

hc.out <- hclust(dist_my_data, method = "complete")
hc.out

plot(hc.out)
rect.hclust(hc.out, k = 6, border = 2.5)
hc.out$order
hc_clusters <-cutree(hc.out, k=6)
hc_clusters

hc_frame <-data.frame(my_data,hc_clusters)
writexl::write_xlsx(hc_frame,"C:\\Users\\IkeOl\\OneDrive\\University\\Final\\Project\\Data\\Clusterhc.xlsx")


#density
#install.packages("dbscan")
library(dbscan)
data_matrix <- as.matrix(my_data[,-1])
kNNdistplot(data_matrix, k=6)
abline(h=11.5, col="yellow")

set.seed(1234)
db.my_data <- dbscan(data_matrix, 11.5)
db.my_data

hullplot(data_matrix, db.my_data$cluster)

db.clusters <- data.frame(my_data, db.my_data$cluster)
writexl::write_xlsx(db.clusters,"C:\\Users\\IkeOl\\OneDrive\\University\\Final\\Project\\Data\\Clusterdb.xlsx")
