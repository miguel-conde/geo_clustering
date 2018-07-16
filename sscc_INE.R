
# REFERENCES --------------------------------------------------------------

# DATA FROM:
# http://www.ine.es/censos2011_datos/cen11_datos_resultados_seccen.htm

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

dsn_dir <- "./data/SSCC"
ogrListLayers(dsn_dir)
ogrInfo(dsn_dir)
ogrInfo(dsn = dsn_dir, layer = "SECC_CPV_E_20111101_01_R_INE")

# Read geo data
sscc_LL <- readOGR(dsn  = dsn_dir, layer = "SECC_CPV_E_20111101_01_R_INE")

# Explore the returned object
class(sscc_LL)
names(sscc_LL)
as.data.frame(sscc_LL) %>% as.tibble() %>% glimpse
sscc_LL$NCA %>% unique

## Only CAM
ss_cc_CAM <- sscc_LL[sscc_LL$NCA == "Comunidad de Madrid",]

# Plot sscc in CAM
plot(ss_cc_CAM)

library(readxl)
SSCC_ONCE <- read_excel("data/SSCC_ONCE.xlsx", 
                        sheet = "Relacion-SSCC-Provincia-AreaGeo")

# CE221 - CHAMARTIN (HISPANOAMERICA, EL VISO, CIUDAD JARDÃN Y PROSPERIDAD) Y 
#         SALAMANCA (GUINDALERA, CASTELLANA, RECOLETOS, LISTA Y GOYA CORTE POR 
#         ALCALÃ IMPARES)

sscc_CE221 <- SSCC_ONCE %>% 
  filter(area_geografica == "CE221") %>% 
  pull(seccode_2014)

sscc_CE221 <- sscc_LL[sscc_LL$CUSEC %in% sscc_CE221,]
# Plot sscc in area CE221
plot(sscc_CE221, col = sscc_CE221$CDIS)
