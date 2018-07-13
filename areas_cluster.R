pacman::p_load(rgdal)

ogrDrivers()

dsn_dir <- "./data/areas"
ogrListLayers(dsn_dir)
ogrInfo(dsn_dir)
ogrInfo(dsn=dsn, layer="Area_geografica")

areas_LL <- readOGR(dsn  = dsn_dir, layer = "Area_geografica")

class(areas_LL)
names(areas_LL)

# final_dataset
load("data/final_dataset.Rds")

# external_vars, internal_vars, y la BD normalizada por poblacion_total (train_preds, train_tgt)
load("data/train_set.Rds")
