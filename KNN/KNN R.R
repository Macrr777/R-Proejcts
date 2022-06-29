
library(knitr)
library(class)
library(tidyverse)
library(GGally)
library(ggplot2)

head(iris)

# Some minor changes in the data set
iris2 <- iris %>%
  rename(`Sepal length`=Sepal.Length,
         `Sepal width`=Sepal.Width,
         `Petal length`=Petal.Length,
         `Petal width`=Petal.Width) %>%
  mutate(Species=fct_recode(Species, "Setosa"="setosa",
                            "Versicolor"="versicolor",
                            "Virginica"="virginica"))



# Density plot for each species
iris2 %>%
  gather(Attributes, value, 1:4) %>%
  ggplot(aes(x=value, fill=Species)) +
  geom_density(colour="black", alpha=0.5) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Density",
       title="Iris data set",
       subtitle="Density plot for each attribute") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank())

# Violin plot for each attribute
iris2 %>%
  gather(Attributes, value, 1:4) %>%
  ggplot(aes(x=reorder(Attributes, value, FUN=median), y=value, fill=Attributes)) +
  geom_violin(show.legend=FALSE) +
  geom_boxplot(width=0.05, fill="white") +
  labs(title="Iris data set",
       subtitle="Violin plot for each attribute") +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank())

# Boxplot for each attribute
iris2 %>%
  gather(Attributes, value, 1:4) %>%
  ggplot(aes(x=reorder(Attributes, value, FUN=median), y=value, fill=Attributes)) +
  geom_boxplot(show.legend=FALSE) +
  labs(title="Iris data set",
       subtitle="Boxplot for each attribute") +
  theme_bw() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank())

# Scatter plot and correlations
ggpairs(cbind(iris2, Cluster=as.factor(iris2$Species)),
        columns=1:4, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        axisLabels="none", switch="both") +
  theme_bw() 

# Normalization of all columns except Species
dataNorm <- iris
dataNorm[, -5] <- scale(iris[, -5])

# Reproducible results
set.seed(1234)

# 70% train and 30% test
ind <- sample(2, nrow(dataNorm), replace=TRUE, prob=c(0.7, 0.3))
trainData <- dataNorm[ind==1,]
testData <- dataNorm[ind==2,]

# Execution of k-NN with k=1
KnnTestPrediction_k1 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=1, prob=TRUE)

# Execution of k-NN with k=2
KnnTestPrediction_k2 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=2, prob=TRUE)

# Execution of k-NN with k=3
KnnTestPrediction_k3 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=3, prob=TRUE)

# Execution of k-NN with k=4
KnnTestPrediction_k4 <- knn(trainData[,-5], testData[,-5],
                            trainData$Species, k=4, prob=TRUE)

# Confusion matrix of KnnTestPrediction_k1
table(testData$Species, KnnTestPrediction_k1)

# Classification accuracy of KnnTestPrediction_k1
sum(KnnTestPrediction_k1==testData$Species)/length(testData$Species)*100

# Confusion matrix of KnnTestPrediction_k2
table(testData$Species, KnnTestPrediction_k2)

# Classification accuracy of KnnTestPrediction_k2
sum(KnnTestPrediction_k2==testData$Species)/length(testData$Species)*100

# Confusion matrix of KnnTestPrediction_k3
table(testData$Species, KnnTestPrediction_k3)

# Classification accuracy of KnnTestPrediction_k3
sum(KnnTestPrediction_k3==testData$Species)/length(testData$Species)*100

# Confusion matrix of KnnTestPrediction_k4
table(testData$Species, KnnTestPrediction_k4)

# Classification accuracy of KnnTestPrediction_k4
sum(KnnTestPrediction_k4==testData$Species)/length(testData$Species)*100


# Empty variables
KnnTestPrediction <- list()
accuracy <- numeric()

# From k=1 to k=100...
for(k in 1:100){
  
  # KnnTestPrediction for each k
  KnnTestPrediction[[k]] <- knn(trainData[,-5], testData[,-5], trainData$Species, k, prob=TRUE)
  
  # Accuracy for each k   
  accuracy[k] <- sum(KnnTestPrediction[[k]]==testData$Species)/length(testData$Species)*100
  
}

# Accuracy vs Choice of k
plot(accuracy, type="b", col="dodgerblue", cex=1, pch=20,
     xlab="k, number of neighbors", ylab="Classification accuracy", 
     main="Accuracy vs Neighbors")

# Add lines indicating k with best accuracy
abline(v=which(accuracy==max(accuracy)), col="darkorange", lwd=1.5)

# Add line for max accuracy seen
abline(h=max(accuracy), col="grey", lty=2)

# Add line for min accuracy seen 
abline(h=min(accuracy), col="grey", lty=2)
