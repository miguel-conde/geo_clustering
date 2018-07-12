
# REFERENCES --------------------------------------------------------------

# https://arxiv.org/abs/1707.03897

# LIBRARIES and SOURCES ---------------------------------------------------

library(ClustGeo)
library(janitor)
library(ggplot2)
library(tidyverse)


# CONSTANTS ---------------------------------------------------------------


# 1 - DATA ----------------------------------------------------------------

data(estuary) # list of 3 objects (dat, D.geo, map)
              # where: 
              #    - dat= socio-economic data (n*p data frame),
              #    - D.geo = n*n data frame of geographical distances,
              #    - map = object of class "SpatialPolygonsDataFrame"
              #            used to draw the map

estuary %>% glimpse

head(estuary$dat)

estuary$dat %>% summary

estuary$dat <- bind_cols(muni_id = estuary$dat %>% rownames,
                         estuary$dat) %>% clean_names

# 2 - EXAMPLE 1 -----------------------------------------------------------

# the socio-economic distances
D0 <- dist(estuary$dat %>% select(-muni_id)) 

# 2.1 - Choice of the number K of clusters -------------------------------

# To choose the suitable number K of clusters, we focus on the Ward dendrogram 
# based on the p = 4 socio-economic variables, that is using D0 only

tree <- hclustgeo(D0)
plot(tree, hang=-1, label = FALSE, xlab = "", sub = "", main= "")
rect.hclust(tree, k = 5, border = c(4, 5, 3, 2, 1))
legend("topright", legend = paste("cluster", 1:5), fill = 1:5, 
       bty = "n", border = "white")

# The visual inspection of the dendrogram in Figure 1 suggests to retain K = 5 
# clusters. We can use the map provided in the estuary data to visualize the 
# corresponding partition in five clusters, called P5 hereafter.

# cut the dendrogram to get the partition in 5 clusters
P5 <- cutree(tree, 5) 
# plot an object of class sp
sp::plot(estuary$map, border="grey", col = P5) 
legend("topleft", legend=paste("cluster", 1:5), fill = 1:5, bty = "n", 
       border="white")

# municipalities of cluster 5 are geographically compact, corresponding
# to Bordeaux and the 15 municipalities of its suburban area and Arcachon. On 
# the contrary, municipalities in cluster 3 are scattered over a wider 
# geographical area from North to South of the study area. The composition of 
# each cluster is easily obtained, as shown for cluster 5:
  
# list of the municipalities in cluster 5
city_label <- as.vector(estuary$map$"NOM_COMM")
city_label[which(P5 == 5)]


# 3 - EXAMPLE 2: a partition with geo constrains --------------------------

# To obtain more geographically compact clusters, we can now introduce the 
# matrix D1 of geographical distances into hclustgeo. This requires a mixing 
# parameter to be selected α to improve the geographical cohesion of the 5 
# clusters without adversely affecting socioeconomic cohesion.
# Choose the value of α means a trade-off between the
# lost of socio-economic homogeneity and the gain of geographic cohesion.

# the geographic distances between the municipalities
D1 <- as.dist(estuary$D.geo) 

## Choicing the mixing parameter

cr <- choicealpha(D0, D1, range.alpha=seq(0, 1, 0.1), K=5, graph=TRUE)
# proportion of explained pseudo-inertia
cr$Q 
# normalized proportion of explained pseudo-inertias
cr$Qnorm 

# Here, the plot would appear to suggest choosing α = 0.2 which corresponds to
# a loss of only 7% of socio-economic homogeneity, and a 17% increase in 
# geographical homogeneity.

tree <- hclustgeo(D0, D1, alpha = 0.2)
P5bis <- cutree(tree, 5)
sp::plot(estuary$map, border="grey", col=P5bis)
legend("topleft", legend=paste("cluster", 1:5), fill=1:5, bty="n", 
       border="white")


# 4 - EXAMPLE 3: a partition with neighborhood constrains -----------------

# Let us construct a different type of matrix D1 to take neighbouring 
# municipalities into account when clustering the 303 municipalities.
# Two regions with contiguous boundaries, that is sharing one or more boundary 
# point, are considered as neighbors. Let us first build the adjacency matrix A.

# list of neighbors
list.nb <- spdep::poly2nb(estuary$map,
                          row.names=rownames(estuary$dat)) 
# list of the neighbors of BORDEAUX
city_label[list.nb[[117]]] 

# The dissimilarity matrix D1 is constructed based on the adjacency matrix A 
# with D1 =  1_nn − A.
# build the adjacency matrix
A <- spdep::nb2mat(list.nb, style="B") 
diag(A) <- 1
colnames(A) <- rownames(A) <- city_label
D1 <- 1-A
D1[1:2, 1:5]
D1 <- as.dist(D1)

## Choicing the mixing parameter

cr <- choicealpha(D0, D1, range.alpha=seq(0, 1, 0.1), K=5, graph=TRUE)
# proportion of explained pseudo-inertia
cr$Q 
# normalized proportion of explained pseudo-inertias
cr$Qnorm 

tree <- hclustgeo(D0, D1, alpha=0.2)
P5ter <- cutree(tree, 5)
sp::plot(estuary$map, border="grey", col=P5ter)
legend("topleft", legend=paste("cluster", 1:5), fill=1:5, 
       bty="n", border="white")


# 5 - CHARACTERISING CLUSTERS ---------------------------------------------

estuary$dat %>% 
  mutate(P5 = P5, 
         P5bis = as.integer(P5bis), 
         P5ter= as.integer(P5ter)) %>% 
  gather(Partition, Cluster, P5:P5ter) %>% 
  gather(Var, Value, employ_rate_city:agri_land) %>% 
  mutate(Var = factor(Var, labels = c("employ_rate_city", "graduate_rate",
                                      "housing_appart", "agri_land"))) %>%
  ggplot(aes(x = Var, y = Value)) +
  geom_boxplot(alpha = 0.8, color = "grey") +
  coord_flip() + 
  facet_grid( Cluster ~ Partition, 
              scales = "free")

              