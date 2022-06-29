data = read.csv('C:/Users/choud/Desktop/Amir/K mean Clustering/Clustering.csv', header = T)
data = as.data.frame(data)
head(data)
class(data)
data = data[, -1]
head(data)
data = scale(data)
class(data)
data = as.data.frame(data)
library(ggplot2)
ggplot(data, aes(x, y)) + 
  geom_point() +  
  theme(
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.y = element_text(size = 20))
install.packages("factoextra")
library(factoextra)

fviz_nbclust(data, kmeans, method = "wss")
# method can be = 'gap_stat', 'wss', 'silhouette'

fviz_nbclust(data, kmeans, method = "gap_stat")


fviz_nbclust(data, kmeans, method = "silhouette")
set.seed(123) 
km <- kmeans(data, 2)
km
dd = data
dd$label = km$cluster

head(dd)
tail(dd)
fviz_cluster(km, data = data,
             palette = c("#2E9FDF", "#00AFBB"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())
