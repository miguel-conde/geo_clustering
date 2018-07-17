# http://mazamascience.com/WorkingWithData/?p=1494
# http://www.kevjohnson.org/making-maps-in-r/

library(maptools)
library(ggmap)
library(tidyverse)

# add to data a new column termed "id" composed of the rownames of data
ss_cc_CAM@data$id <- rownames(ss_cc_CAM@data)

# reproject the data onto a "longlat" projection
ss_cc_CAM <- spTransform(ss_cc_CAM, CRS("+proj=longlat"))

# determine the bounding box of the spatial object
b <- bbox(ss_cc_CAM)

# get and plot a map
CAM <- ggmap(get_map(location = b, maptype = "satellite", zoom = 6))

# create a data.frame from our spatial object
CAMFortified <- fortify(ss_cc_CAM, region = "id")

CAMFortified <- left_join(CAMFortified, ss_cc_CAM@data, by = "id")

CAM + geom_polygon(data = CAMFortified,
                               aes(x = long, y = lat, group = group,
                                   fill = as.numeric(as.character(CUDIS))), 
                   alpha = 0.5) +
  geom_path(color = "white") +
  scale_fill_gradient() +
  scale_x_continuous(limits = c(b[1,1],b[1,2])) +
  scale_y_continuous(limits = c(b[2,1],b[2,2])) +
  theme(legend.position = "none", title = element_blank())



ggplot(data = CAMFortified,
       aes(x = long, y = lat, group = group,
           fill = as.numeric(as.character(CUDIS))), alpha = 0.5)  +
  geom_polygon()  +
  geom_path(color = "white") +
  scale_fill_gradient() +
  coord_equal() +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(title = "Secciones Censales de la Comunidad de Madrid", fill = "Area")

######

# add to data a new column termed "id" composed of the rownames of data
areas_LL@data$id <- rownames(areas_LL@data)
areas_LL@data$clust <- P_K_se_dist

# reproject the data onto a "longlat" projection
areas_LL_trf <- spTransform(areas_LL, CRS("+proj=longlat"))

# create a data.frame from our spatial object
SPFortified <- fortify(areas_LL_trf, region = "id")

SPFortified <- left_join(SPFortified, areas_LL_trf@data, by = "id")

ggplot(data = SPFortified,
       aes(x = long, y = lat, group = group,
           fill = clust), alpha = 0.5)  +
  geom_polygon()  +
  geom_path(color = "white") +
  scale_fill_gradient(low = "red", high = "blue") +
  coord_equal() +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(title = "Secciones Censales de la Comunidad de Madrid", fill = "Area")


####
# determine the bounding box of the spatial object
b <- bbox(areas_LL_trf)

# get and plot a map
SP <- ggmap(get_map(location = b, maptype = "satellite", zoom = 6))

SP + geom_polygon(data = SPFortified,
                   aes(x = long, y = lat, group = group,
                       fill = clust), 
                   alpha = 0.5) +
  geom_path(color = "white") +
  scale_fill_gradient() +
  scale_x_continuous(limits = c(b[1,1],b[1,2])) +
  scale_y_continuous(limits = c(b[2,1],b[2,2])) +
  theme(legend.position = "none", title = element_blank())


#####
cod_area <- "CE221" # ES123 NE522 NO411 SE312 SO312
cod_area <- "ES223" # ES123 NE522 NO411 SE312 SO312 ES411
cod_area <- "NE522" # ES123 NE522 NO411 SE312 SO312
cod_area <- "NO411" # ES123 NE522 NO411 SE312 SO312
cod_area <- "SE312" # ES123 NE522 NO411 SE312 SO312
cod_area <- "SO312" # ES123 NE522 NO411 SE312 SO312


sscc_sel_area <- SSCC_ONCE %>% 
  filter(area_geografica == cod_area) %>% 
  pull(seccode_2014)

sscc_sel_area <- sscc_LL[sscc_LL$CUSEC %in% sscc_sel_area,]

# BASE SYSTEM
plot(sscc_sel_area, col = sscc_sel_area$CUSEC,
     main = paste0("Secciones Censales del Área ",
                   cod_area, "\n",
                   final_dataset %>% 
                        filter(area_geografica == cod_area) %>% 
                        pull(descripcion_resumida)))

# GGPLOT

# add to data a new column termed "id" composed of the rownames of data
sscc_sel_area@data$id <- rownames(sscc_sel_area@data)

# reproject the data onto a "longlat" projection
sscc_sel_area_trf <- spTransform(sscc_sel_area, CRS("+proj=longlat"))

# create a data.frame from our spatial object
sscc_sel_areaFortified <- fortify(sscc_sel_area_trf, region = "id")

sscc_sel_areaFortified <- left_join(sscc_sel_areaFortified, 
                                    sscc_sel_area_trf@data, by = "id")

ggplot(data = sscc_sel_areaFortified,
       aes(x = long, y = lat, 
           fill = as.numeric(as.character(CUSEC)),
           # fill = as.character(CUSEC), # PROBARLO !!!!
           group = group
           ), alpha = 0.5)  +
  geom_polygon()  +
  geom_path(color = "white") +
  # scale_fill_gradient(low = "red", high = "blue") +
  coord_equal() +
  theme(axis.title = element_blank(), axis.text = element_blank()) +
  labs(title = paste0("Secciones Censales del Área ",
                      cod_area), 
       subtitle = paste(final_dataset %>% 
                          filter(area_geografica == cod_area) %>% 
                          pull(descripcion_resumida)),
       fill = "Area") 
