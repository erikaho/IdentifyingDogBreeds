library(data.table)
library(Rtsne)
library(ggplot2)
library(caret)
library(Metrics)
library(ClusterR)

set.seed(1000)
data <- fread("./project/volume/data/raw/data.csv")

#we aren't supposed to know id, so hide this
id<-data$id
data$id<-NULL

# do a pca
pca<-prcomp(data)

# look at the percent variance explained by each pca
screeplot(pca)

# look at the rotation of the variables on the PCs
pca

# see the values of the scree plot in a table 
summary(pca)

# see a biplot of the first 2 PCs
biplot(pca)

# use the unclass() function to get the data in PCA space
pca_dt<-data.table(unclass(pca)$x)

#add back id to prove that it works
pca_dt$id<-id

#perplexity between 5-50
tsne_dt1<-Rtsne(pca_dt,pca = F,perplexity=40,max_iter = 1000, check_duplicates = F)
tsne_dt2<-Rtsne(pca_dt,pca = F,perplexity=20, max_iter = 1000, check_duplicates = F)



# grab out the coordinates
tsne_dt1_coordinates<-data.table(tsne_dt1$Y)
tsne_dt2_coordinates<-data.table(tsne_dt2$Y)

tsne_dt<-cbind(tsne_dt1_coordinates, tsne_dt2_coordinates)

tsne_dt$id<-id

# use a gaussian mixture model to find optimal k and then get probability of membership for each row to each group
tsne_dt<-data.table(tsne_dt)

# this fits a gmm to the data for all k=1 to k= max_clusters, we then look for a major change in likelihood between k values
k_bic<-Optimal_Clusters_GMM(tsne_dt[,c("V1", "V2")],max_clusters = 10,criterion = "BIC")

# now we will look at the change in model fit between successive k values
delta_k<-c(NA,k_bic[-1] - k_bic[-length(k_bic)])

#plot
delta_k_dt <- data.table(delta_k = delta_k, k = 1: length(delta_k))

opt_k<-4

# now we run the model with our chosen k value
gmm_data<-GMM(tsne_dt[,c("V1", "V2")],opt_k)

#ggplot(tsne_dt,aes(x=V1,y=V2,col=Cluster_1_prob))+geom_point()

clus<- predict_GMM(tsne_dt[,c("V1", "V2")], gmm_data$centroids, gmm_data$covariance_matrices, gmm_data$weights)

clus_dt<- data.table(clus$cluster_proba)

#submission
clus_dt$id<-id
setnames(clus_dt, c("V1", "V2", "V3", "V4"), c("breed_1","breed_2", "breed_3", "breed_4"))
setcolorder(clus_dt, c("id", "breed_1","breed_2", "breed_3", "breed_4"))
fwrite(clus_dt,'./project/volume/data/processed/submission9.csv')


