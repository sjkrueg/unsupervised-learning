## This script takes an input table with observations in rows and values in columns
## Variables should be quantitative (ratios or ordinal)
## It conducts hierarchichal and k-means clustering and asesses cluster stability
## Author: Kevin Stofan, Andie Dodd, Steve Krueger
## Book References
## James, G., Witten, D., Hastie, T., & Tibshirani, R. (2013). An introduction to statistical learning (Vol. 112). New York: springer.
## Zumel, N., Mount, J., & Porzak, J. (2014). Practical data science with R. Manning.


## Load libraries
library(ggplot2)
library(fpc)
library(reshape2)
library(dplyr)
library(RColorBrewer)
library(corrplot)


## Set working directory and load in data
setwd("~/R Playground/unsupervised_learning")
nfl_stats <- read.csv("nfl_stats_new2.csv", sep=",", header=TRUE)
summary(nfl_stats)

##Make a correlation plot 
data <- nfl_stats[-1:-1]
ccor <- cor(data)
corrplot(ccor, order="hclust", tl.col="black", tl.srt=45, bg = "white",
         col=brewer.pal(n=8, name="RdYlGn"))

## Scale the attributes to all have a mean of 0 and std. dev of 1
vars.to.use   <- colnames(nfl_stats)[-1]
pmatrix       <- scale(nfl_stats[,vars.to.use])
pcenter       <- attr(pmatrix, "scaled:center")
pscale        <- attr(pmatrix, "scaled:scale")


## Create the distance matrix and conduct hierarchical clustering
d             <- dist(pmatrix, method="euclidean")
pfit          <- hclust(d, method="ward.D")


## Plot the dendrogram and create groupings (clusters)
## The k value below predetermines the number of clusters to create
plot(pfit, labels=FALSE, xlab="", sub="")

## Set number of clusters
clusters <- 3

rect.hclust(pfit, k=clusters, border="#003399")

## Print cluster groups
groups <- cutree(pfit, k=clusters)
groups
print_clusters <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(nfl_stats[labels==i,c("Team")])
  }
}
print_clusters(groups,clusters)

## Some intermediate pricipal components analysis to add values the clusters
pmat_no_na <- na.omit(pmatrix)
pmat_no_na
princ <- prcomp(pmat_no_na, scale=TRUE)
names(princ)
princ$center
princ$scale
princ$rotation
dim(princ$x)
summary(princ)
nComp <- 2

## Project the results and write the clusters to a new file within the workspace
project <- predict(princ, newdata=pmatrix)[,1:nComp]
project.plus <- cbind(as.data.frame(project), cluster=as.factor(groups), nfl_stats)

## Add cluster results to original data
write.table(project.plus, file = "nfl_with_clusters_n3.csv", sep = ",", col.names = NA,
            qmethod = "double")

## Plot the clusters using PC1 and PC2 as axes
ggplot(project.plus, aes(x=PC1, y=PC2)) +
  geom_point(aes(shape=cluster, color=cluster))

## Bootstrap clusters to determine stability
## k values below should match the number of clusters set above
kbest.p<-clusters
cboot.hclust <- clusterboot(pmatrix, clustermethod=hclustCBI, method="ward.D", k=kbest.p)
summary(cboot.hclust$result)
groups<-cboot.hclust$result$partition
print_clusters(groups, kbest.p)

## As a rule of thumb, clusters with a stability value less than 0.6 should be considered unstable.
## bootbrd is the number of times that 
cboot.hclust$bootmean
cboot.hclust$bootbrd

## Calculate some basic descriptive statistics
nfl_stats_clus <- read.csv("nfl_with_clusters.csv", sep=",", header=TRUE)
nfl_stats_clus$Team <- NULL 
arr <- arrange(nfl_stats_clus, cluster)

mean <- nfl_stats_clus %>%
  group_by(cluster) %>%
  summarise_each(funs(mean(.)))
mean$type <- "mean"

min <- nfl_stats_clus %>%
  group_by(cluster) %>%
  summarise_each(funs(min(.))) 
min$type <- "min"

median <- nfl_stats_clus %>%
  group_by(cluster) %>%
  summarise_each(funs(median(.)))
median$type <- "median"

max <- nfl_stats_clus %>%
  group_by(cluster) %>%
  summarise_each(funs(max(.)))
max$type <- "max"

## Write values to new file
summary_nfl_stats <- rbind(mean, min, median, max)
write.table(summary_nfl_stats, file = "summary.csv", sep = ",", col.names = NA)
