rm(list = ls())
# LIBRARIES and SOURCES ---------------------------------------------------

library(ClustGeo)
pacman::p_load(rgdal)
library(ggplot2)
library(tidyverse)


# CONSTANTS ---------------------------------------------------------------
imgs_dir <- "img"
BMP_WIDTH <- 800
BMP_HEIGHT <- 800

# READ GEO DATA AND PLOT MAP ----------------------------------------------

# 1st, some playing around
ogrDrivers()

dsn_dir <- "./data/areas"
ogrListLayers(dsn_dir)
ogrInfo(dsn_dir)
ogrInfo(dsn = dsn_dir, layer = "Area_geografica")

# Read geo data
areas_LL <- readOGR(dsn  = dsn_dir, layer = "Area_geografica")

# Plot 1st map
plot(areas_LL)

# Explore the returned object
class(areas_LL)
names(areas_LL)


# LOAD DATA ---------------------------------------------------------------

# final_dataset
load("data/final_dataset.Rds")

# external_vars, internal_vars, y la BD normalizada por poblacion_total (train_preds, train_tgt)
load("data/train_set.Rds")


# 1 - SOCIO-ECONOMIC CLUSTERING -------------------------------------------

# Number of clusters
K <- 6

# the socio-economic distances
se_data <- final_dataset %>% 
  select(tipo_zona:ratio_pdv_lae) %>% 
  # select(tipo_zona, densidad_pob_km2, edad_media, ind_consumo, 
  #        prop_poblacion_activa, prop_tasa_paro, nivel_socioeconomico, 
  #        ind_transito)
  # select(tipo_zona, edad_media, 
  #        prop_poblacion_activa, nivel_socioeconomico)
  select(tipo_zona, densidad_pob_km2, edad_media, nivel_socioeconomico) %>% 
  mutate(tipo_zona = scales::rescale(tipo_zona),
         densidad_pob_km2 = scales::rescale(densidad_pob_km2),
         edad_media = scales::rescale(edad_media),
         nivel_socioeconomico = scales::rescale(nivel_socioeconomico))

se_data %>% glimpse

D0_se <- dist(se_data) 

tree <- hclustgeo(D0_se)
plot(tree, hang = -1, label = FALSE, xlab = "", sub = "", main = "")
rect.hclust(tree, k = K, border = c(4, 5, 3, 2, 1))
legend("topright", legend = paste("cluster", 1:K), fill = 1:K, 
       bty = "n", border = "white")

P_K_se <- cutree(tree, K) 
# plot an object of class sp
sp::plot(areas_LL, border = "grey", col = P_K_se,
         main = "Socio - Economical clustering") 
legend("left", legend = paste("cluster", 1:K), fill = 1:K, bty = "n", 
       border = "white", cex = .7)

dev.copy(png, file = file.path(imgs_dir, "P_K_se.png"),
         width = BMP_WIDTH, height = BMP_HEIGHT)
dev.off() 

# list of the areas in cluster 5
area_label <- as.vector(areas_LL$DSNOMBRE)
area_label[which(P_K_se == 5)]

area_desc <- as.vector(areas_LL$DSAREAGEO)
area_desc[which(P_K_se == 5)]



# 1.1 - ADDING GEO CONSTRAINS ---------------------------------------------


# 1.1.1 - DISTANCE CONSTRAINS ---------------------------------------------

# Calc distances between areas centroids
areas_LL_centers <- SpatialPointsDataFrame(rgeos::gCentroid(areas_LL, 
                                                            byid = TRUE), 
                                           areas_LL@data, match.ID = FALSE)
areas_dists <- spDists(areas_LL_centers)

# the geographic distances between the areas
D1_dist <- as.dist(areas_dists) 

## Choicing the mixing parameter

cr <- choicealpha(D0_se, D1_dist, 
                  range.alpha = seq(0, 1, 0.1), K = K, graph = TRUE)
# proportion of explained pseudo-inertia
cr$Q 
# normalized proportion of explained pseudo-inertias
cr$Qnorm 

tree <- hclustgeo(D0_se, D1_dist, alpha=0.5)
P_K_se_dist <- cutree(tree, K)
sp::plot(areas_LL, border = "grey", col = P_K_se_dist,
         main = "Socio - Economical + Distance Clustering")
legend("left", legend=paste("cluster", 1:K), fill = 1:K, 
       bty = "n", border = "white", cex = .7)

dev.copy(png, file = file.path(imgs_dir, "P_K_se_dist.png"),
         width = BMP_WIDTH, height = BMP_HEIGHT)
dev.off() 

# 1.1.2 - NEIGHBORHOOD CONSTRAINS -----------------------------------------

# list of neighbors
list.nb <- spdep::poly2nb(areas_LL,
                          row.names = areas_LL$DSNOMBRE) 
# list of the neighbors of DISTRITO CENTRO DE MADRID (CASCO HISTÃ“RICO)
tgt <- sapply(areas_LL$DSAREAGEO, 
              function(x) {grepl("DISTRITO CENTRO DE MADRID", x)})
areas_LL$DSAREAGEO[[which(tgt == TRUE)]]
areas_LL$DSNOMBRE[[which(tgt == TRUE)]]
area_label[list.nb[[which(tgt == TRUE)]]] 
tgt <- which(areas_LL$DSNOMBRE %in% area_label[list.nb[[which(tgt == TRUE)]]])
areas_LL$DSAREAGEO[tgt]

# The dissimilarity matrix D1 is constructed based on the adjacency matrix A 
# with D1 =  1_nn − A.
# build the adjacency matrix
A <- spdep::nb2mat(list.nb, style = "B", zero.policy = TRUE) 
diag(A) <- 1
colnames(A) <- rownames(A) <- area_label
D1_neigh <- 1-A
D1_neigh[1:2, 1:5]
D1_neigh <- as.dist(D1_neigh)

## Choicing the mixing parameter

cr <- choicealpha(D0_se, D1_neigh, 
                  range.alpha = seq(0, 1, 0.1), K = K, graph = TRUE)
# proportion of explained pseudo-inertia
cr$Q 
# normalized proportion of explained pseudo-inertias
cr$Qnorm 

tree <- hclustgeo(D0_se, D1_neigh, alpha = 0.3)
P_K_se_neigh <- cutree(tree, K)
sp::plot(areas_LL, border = "grey", col = P_K_se_neigh,
         main = "Socio - Economical + Neighborhood Clustering")
legend("left", legend=paste("cluster", 1:K), fill = 1:K, 
       bty = "n", border = "white", cex = .7)

dev.copy(png, file = file.path(imgs_dir, "P_K_se_neigh.png"),
         width = BMP_WIDTH, height = BMP_HEIGHT)
dev.off() 

# 1.2 - BOXPLOT -----------------------------------------------------------

se_data %>% 
  mutate(P_K_se = P_K_se, 
         P_K_se_dist= P_K_se_dist,
         P_K_se_neigh = P_K_se_neigh) %>% 
  gather(Partition, Cluster, P_K_se:P_K_se_neigh) %>% 
  gather(Var, Value, tipo_zona:nivel_socioeconomico) %>% 
  # mutate(Var = factor(Var, levels = c("employ_rate_city", "graduate_rate",
  #                                     "housing_appart", "agri_land"))) %>%
  ggplot(aes(x = Var, y = Value)) +
  geom_boxplot(alpha = 0.8, color = "blue") +
  coord_flip() + 
  facet_grid( Cluster ~ Partition, 
              scales = "free")

dev.copy(png, file = file.path(imgs_dir, "boxplot_se.png"),
         width = BMP_WIDTH, height = BMP_HEIGHT)
dev.off() 


# 2 - MARKET CLUSTERING ---------------------------------------------------

# the potencial market distances
pot_data <- final_dataset %>% 
  select(Potencial, Ind_Eficacia_Comercial) %>% 
  mutate(Potencial = scale(Potencial),
         Ind_Eficacia_Comercial = scale(Ind_Eficacia_Comercial))

pot_data %>% glimpse

D0_pot <- dist(pot_data) 

tree <- hclustgeo(D0_pot)
P_K_pot <- cutree(tree, K) 
# plot an object of class sp
sp::plot(areas_LL, border = "grey", col = P_K_pot,
         main = "Potential Market clustering") 
legend("left", legend=paste("cluster", 1:K), fill = 1:K, bty = "n", 
       border = "white", cex = .7)

dev.copy(png, file = file.path(imgs_dir, "P_K_pot.png"),
         width = BMP_WIDTH, height = BMP_HEIGHT)
dev.off() 


# 2.1 - ADDING GEO CONSTRAINS ---------------------------------------------



# 2.1.1 - DISTANCES -------------------------------------------------------

## Choicing the mixing parameter

cr <- choicealpha(D0_pot, D1_dist, 
                  range.alpha = seq(0, 1, 0.1), K = K, graph = TRUE)
# proportion of explained pseudo-inertia
cr$Q 
# normalized proportion of explained pseudo-inertias
cr$Qnorm 

tree <- hclustgeo(D0_pot, D1_neigh, alpha = 0.5)
P_K_pot_dist <- cutree(tree, K)
sp::plot(areas_LL, border="grey", col = P_K_pot_dist,
         main = "Potential Market + Distances Clustering")
legend("left", legend = paste("cluster", 1:K), fill = 1:K, 
       bty = "n", border = "white", cex = .7)

dev.copy(png, file = file.path(imgs_dir, "P_K_pot_dist.png"),
         width = BMP_WIDTH, height = BMP_HEIGHT)
dev.off() 


# 2.1.2 - NEIGHBORHOOD ----------------------------------------------------

## Choicing the mixing parameter

cr <- choicealpha(D0_pot, D1_neigh, 
                  range.alpha = seq(0, 1, 0.1), K = K, graph = TRUE)
# proportion of explained pseudo-inertia
cr$Q 
# normalized proportion of explained pseudo-inertias
cr$Qnorm 

tree <- hclustgeo(D0_pot, D1_neigh, alpha = 0.2)
P_K_pot_neigh <- cutree(tree, K)
sp::plot(areas_LL, border = "grey", col = P_K_pot_neigh,
         main = "Potential Market + Neighborhood Clustering")
legend("left", legend = paste("cluster", 1:K), fill = 1:K, 
       bty = "n", border = "white", cex = .7)

dev.copy(png, file = file.path(imgs_dir, "P_K_pot_neigh.png"),
         width = BMP_WIDTH, height = BMP_HEIGHT)
dev.off() 


# 2.2 - BOXPLOT -----------------------------------------------------------


pot_data %>% 
  mutate(P_K_pot= as.integer(P_K_pot),
         P_K_pot_dist= as.integer(P_K_pot_dist),
         P_K_pot_neigh= as.integer(P_K_pot_neigh)) %>% 
  gather(Partition, Cluster, P_K_pot:P_K_pot_neigh) %>% 
  gather(Var, Value, Potencial:Ind_Eficacia_Comercial) %>% 
  ggplot(aes(x = Var, y = Value)) +
  geom_boxplot(alpha = 0.8, color = "blue") +
  coord_flip() + 
  facet_grid( Cluster ~ Partition, 
              scales = "free")

dev.copy(png, file = file.path(imgs_dir, "boxplot_pot.png"),
         width = BMP_WIDTH, height = BMP_HEIGHT)
dev.off() 


# 3 - SEARCHING K ---------------------------------------------------------

# 3.1 - fviz_nbclust() ----------------------------------------------------

library(factoextra)

## ELBOW Method

# Socioeconomic
fviz_nbclust(se_data, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
# Potential
fviz_nbclust(pot_data, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
# Distances
fviz_nbclust(areas_dists, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
# Neighborhood
fviz_nbclust(1-A, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# 3.2 - NbClust() ---------------------------------------------------------

library(NbClust)

# Socioeconomic
nb <- NbClust(se_data, distance = "euclidean", min.nc = 2,
              max.nc = 20, method = "ward.D")
fviz_nbclust(nb)
# Potential
nb <- NbClust(pot_data, distance = "euclidean", min.nc = 2,
              max.nc = 20, method = "ward.D")
fviz_nbclust(nb)
# Distances
nb <- NbClust(areas_dists, distance = "euclidean", min.nc = 2,
              max.nc = 20, method = "ward.D")
fviz_nbclust(nb)
# Neighborhood
nb <- NbClust(1-A, distance = "euclidean", min.nc = 2,
              max.nc = 20, method = "ward.D")
fviz_nbclust(nb)

