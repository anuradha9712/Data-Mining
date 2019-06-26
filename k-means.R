setwd("C:/Users/Mayank/Desktop/anudesktop/Anu DUCS/sem2/data mining/DM/data")
# Install
install.packages("FactoMineR")
# Load
library("FactoMineR")
install.packages("factoextra")
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(FactoMineR)


df1<- read.csv("DATA_perlakh_2012.csv",header=TRUE)
df1
df <- subset(df1, select = -c(STATE.UT))
df
#df<- df2
df <- scale(df)
head(df)
distance <- get_dist(df)
#fviz_dist(distance, gradient = list(low = "black", mid = "yellow", high = "purple"))
k2 <- kmeans(df, centers = 2, nstart = 35)
str(k2)
k2
fviz_cluster(k2, data = df)
df1 %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(df1)) %>%
  ggplot(aes(KIDNAPPING...ABDUCTION, RAPE, color = factor(cluster), label = state)) +
  geom_text()

k3 <- kmeans(df, centers = 3, nstart = 35)
k4 <- kmeans(df, centers = 4, nstart = 35)
k5 <- kmeans(df, centers = 5, nstart = 35)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

#ELBOW method to find optimal number of cluster
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")

#silhouette method to find optimal number of cluster
fviz_nbclust(df, kmeans, method = "silhouette")

gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)


set.seed(123)
final <- kmeans(df, 2, nstart = 35)
print(final)
fviz_cluster(final, data = df)
print(final)

