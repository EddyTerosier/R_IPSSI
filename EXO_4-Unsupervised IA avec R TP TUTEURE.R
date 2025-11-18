# TP tuteuré

library(jsonlite)
library(ggplot2)
library(dplyr)
library(factoextra)
library(cluster)
library(tidyr)

# 1. Import des données JSON

cheese <- fromJSON("cheese.json")

print("Aperçu du dataset :")
print(head(cheese))
summary(cheese)

# 2. Sélection des variables numériques uniquement

cheese_num <- cheese %>% select_if(is.numeric)

print("Variables numériques utilisées :")
print(names(cheese_num))

# 3. Histogrammes des variables numériques

cheese_num %>%
  gather(variable, valeur) %>%
  ggplot(aes(x = valeur)) +
  geom_histogram(bins = 20, fill = "#87CEFA", color = "black") +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distribution des variables nutritives des fromages")

# 4. Centrer et réduire

cheese_scaled <- scale(cheese_num)
print("Aperçu des données centrées-réduites :")
head(cheese_scaled)

# 5. Clustering KMeans (essai avec k = 3)

set.seed(123)
km_test <- kmeans(cheese_scaled, centers = 3, nstart = 25)
print(km_test)

# 6. Détermination du nombre optimal de clusters

fviz_nbclust(cheese_scaled, kmeans, method = "wss") +
  labs(title = "Méthode du coude (Elbow)")

fviz_nbclust(cheese_scaled, kmeans, method = "silhouette") +
  labs(title = "Méthode de la Silhouette")

# 7. KMeans final 

set.seed(123)
km <- kmeans(cheese_scaled, centers = 3, nstart = 25)

cheese$Cluster <- factor(km$cluster)

print("Effectifs par cluster :")
print(table(cheese$Cluster))

# 8. Visualisation des clusters KMeans

fviz_cluster(km,
             data = cheese_scaled,
             palette = c("#4CAF50", "#E53935", "#1E88E5"),
             ggtheme = theme_minimal(),
             main = "Visualisation des clusters KMeans")

# 9. Statistiques descriptives par cluster

stats_clusters <- cheese %>%
  group_by(Cluster) %>%
  summarize(across(where(is.numeric), mean))

print("Moyenne des variables par cluster :")
print(stats_clusters)

# 10. ACP
res_pca <- prcomp(cheese_scaled)

# INDIVIDUS 
fviz_pca_ind(res_pca,
             geom.ind = "point",
             habillage = cheese$Cluster,
             addEllipses = TRUE,
             pointsize = 3,
             palette = c("#4CAF50", "#E53935", "#1E88E5"),
             title = "ACP - individus (clusters)")

# VARIABLES 
var <- res_pca$rotation
var_df <- as.data.frame(var)
var_df$Variable <- rownames(var_df)

ggplot(var_df, aes(x = PC1, y = PC2, label = Variable)) +
  geom_segment(aes(xend = 0, yend = 0),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "#1E88E5") +
  geom_text(color = "#E53935", size = 5) +
  theme_minimal() +
  labs(title = "ACP - Variables",
       x = "Composante principale 1",
       y = "Composante principale 2")

# 11. CAH

d <- dist(cheese_scaled)
hc <- hclust(d, method = "ward.D2")

plot(hc, cex = 0.7, main = "Dendrogramme CAH (Ward)")

groups <- cutree(hc, k = 3)

print("Comparaison KMeans vs CAH :")
print(table(KMeans = cheese$Cluster, CAH = groups))

# 12. Mise en prod

predict_cluster <- function(new_data, km_model, scaling_center, scaling_scale) {
  
  new_scaled <- scale(new_data,
                      center = scaling_center,
                      scale = scaling_scale)
  
  distances <- apply(km_model$centers, 1, function(center) {
    sum((new_scaled - center)^2)
  })
  
  return(which.min(distances))
}

# Ex de nouveau fromage à classer
nouveau <- data.frame(
  calories = 350,
  sodium = 200,
  calcium = 250,
  lipides = 28,
  retinol = 80,
  folates = 10,
  proteines = 22,
  cholesterol = 90,
  magnesium = 25
)

cluster_pred <- predict_cluster(
  nouveau,
  km_model = km,
  scaling_center = attr(cheese_scaled, "scaled:center"),
  scaling_scale = attr(cheese_scaled, "scaled:scale")
)

cat("\n Le nouveau fromage appartient au cluster :", cluster_pred, "\n")