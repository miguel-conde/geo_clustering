pacman::p_load(rgdal)

ogrDrivers()

dsn_dir <- "./data/areas"
ogrListLayers(dsn_dir)
ogrInfo(dsn_dir)
ogrInfo(dsn=dsn_dir, layer="Area_geografica")

areas_LL <- readOGR(dsn  = dsn_dir, layer = "Area_geografica")

plot(areas_LL)

class(areas_LL)
names(areas_LL)

# final_dataset
load("data/final_dataset.Rds")

# external_vars, internal_vars, y la BD normalizada por poblacion_total (train_preds, train_tgt)
load("data/train_set.Rds")


# SOCIO-ECONOMIC CLUSTERING -----------------------------------------------

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
  mutate(tipo_zona = scale(tipo_zona),
         densidad_pob_km2 = scale(densidad_pob_km2),
         edad_media = scale(edad_media))

se_data %>% glimpse

D0 <- dist(se_data) 

tree <- hclustgeo(D0)
plot(tree, hang=-1, label = FALSE, xlab = "", sub = "", main= "")
rect.hclust(tree, k = K, border = c(4, 5, 3, 2, 1))
legend("topright", legend = paste("cluster", 1:K), fill = 1:K, 
       bty = "n", border = "white")

P10 <- cutree(tree, K) 
# plot an object of class sp
sp::plot(areas_LL, border="grey", col = P10) 
legend("topleft", legend=paste("cluster", 1:K), fill = 1:K, bty = "n", 
       border="white")

# list of the areas in cluster 5
area_label <- as.vector(areas_LL$DSNOMBRE)
area_label[which(P10 == 5)]

area_desc <- as.vector(areas_LL$DSAREAGEO)
area_desc[which(P10 == 5)]


# ADDING GEO CONSTRAINS ---------------------------------------------------

# list of neighbors
list.nb <- spdep::poly2nb(areas_LL,
                          row.names=areas_LL$DSNOMBRE) 
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
A <- spdep::nb2mat(list.nb, style="B", zero.policy = TRUE) 
diag(A) <- 1
colnames(A) <- rownames(A) <- area_label
D1 <- 1-A
D1[1:2, 1:5]
D1 <- as.dist(D1)

## Choicing the mixing parameter

cr <- choicealpha(D0, D1, range.alpha=seq(0, 1, 0.1), K = K, graph=TRUE)
# proportion of explained pseudo-inertia
cr$Q 
# normalized proportion of explained pseudo-inertias
cr$Qnorm 

tree <- hclustgeo(D0, D1, alpha=0.2)
P10ter <- cutree(tree, K)
sp::plot(areas_LL, border="grey", col = P10ter)
legend("left", legend=paste("cluster", 1:K), fill=1:K, 
       bty="n", border="white", cex = .7)

# BOXPLOT -----------------------------------------------------------------


se_data %>% 
  mutate(P10 = P10, 
         P10ter= as.integer(P10ter)) %>% 
  gather(Partition, Cluster, P10:P10ter) %>% 
  gather(Var, Value, tipo_zona:nivel_socioeconomico) %>% 
  # mutate(Var = factor(Var, levels = c("employ_rate_city", "graduate_rate",
  #                                     "housing_appart", "agri_land"))) %>%
  ggplot(aes(x = Var, y = Value)) +
  geom_boxplot(alpha = 0.8, color = "blue") +
  coord_flip() + 
  facet_grid( Cluster ~ Partition, 
              scales = "free")
