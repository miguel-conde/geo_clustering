require(RgoogleMaps)
# https://cran.r-project.org/web/packages/RgoogleMaps/RgoogleMaps.pdf


# This function creates a bubble plot of spatial
# data, with options for bicolour residual plots.
data(lat.lon.meuse, package="loa", envir = environment())

map <- GetMap(center=c(lat=50.97494,lon=5.743606), zoom=13,
              size=c(480,480),destfile = file.path(tempdir(),"meuse.png"),
              maptype="mobile", SCALE = 1);
par(cex=1.5)
bubbleMap(lat.lon.meuse, coords = c("longitude","latitude"), map=map,
          zcol='zinc', key.entries = 100+ 100 * 2^(0:4));

# Plot Levels of a Variable in a Colour-Coded Map
if (interactive()){
  data("NYleukemia", envir = environment())
  population <- NYleukemia$data$population
  cases <- NYleukemia$data$cases
  mapNY <- GetMap(center=c(lat=42.67456,lon=-76.00365), destfile = "NYstate.png",
                  maptype = "mobile", zoom=9)
  ColorMap(100*cases/population, mapNY, NYleukemia$spatial.polygon, add = FALSE,
           alpha = 0.35, log = TRUE, location = "topleft")
}
#ColorMap(100*cases/population, map=NULL, NYleukemia$spatial.polygon)

# Geocode your data using, R, JSON and Google Maps’ Geocoding APIs
getGeoCode("1600 Amphitheatre Parkway, Mountain View, CA")
getGeoCode("Brooklyn")
#You can run this on the entire column of a data frame or a data table:
DF = cbind.data.frame(address=c("Berlin,Germany", "Princeton,NJ",
                                "cadillac+mountain+acadia+national+park"), 
                      lat = NA, lon = NA)
DF <- with(DF, data.frame(address, t(sapply(DF$address, getGeoCode))))

# Query the Google server for a static map tile, defined primarily by its
# center and zoom. Many additional arguments allow the user to customize
# the map tile.
lat = c(40.702147,40.718217,40.711614);
lon = c(-74.012318,-74.015794,-73.998284);
center = c(mean(lat), mean(lon));
zoom <- min(MaxZoom(range(lat), range(lon)));
#this overhead is taken care of implicitly by GetMap.bbox();
markers = paste0("&markers=color:blue|label:S|40.702147,-74.015794&markers=color:",
                 "green|label:G|40.711614,-74.012318&markers=color:red|color:red|",
                 "label:C|40.718217,-73.998284")
myMap <- GetMap(center=center, zoom=zoom,markers=markers);
#Note that in the presence of markers one often needs to add some extra padding to the
#latitude range to accomodate the extent of the top most marker
if (1){#takes too long to run for CRAN check
  #add a path, i.e. polyline:
  myMap <- GetMap(center=center, zoom=zoom,
                  path = paste0("&path=color:0x0000ff|weight:5|40.737102,-73.990318|",
                                "40.749825,-73.987963|40.752946,-73.987384|40.755823,-73.986397"));
  #use implicit geo coding
  BrooklynMap <- GetMap(center="Brooklyn", zoom=13)
  PlotOnStaticMap(BrooklynMap)
  #use implicit geo coding and display labels in Korean:
  BrooklynMap <- GetMap(center="Brooklyn", zoom=13, hl="ko")
  PlotOnStaticMap(BrooklynMap)
  #no highways
  ManHatMap <- GetMap(center="Lower Manhattan", zoom=14,
                      extraURL="&style=feature:road.highway|visibility:off",
                      destfile = "LowerManhattan.png")
  PlotOnStaticMap(ManHatMap)
  #reload the map without a new download:
  ManHatMap <- GetMap(destfile = "LowerManhattan.png",NEWMAP=FALSE)
  PlotOnStaticMap(ManHatMap)
  #The example below defines a polygonal area within Manhattan, passed a series of
  #intersections as locations:
  #myMap <- GetMap(path = paste0("&path=color:0x00000000|weight:5|fillcolor:0xFFFF0033|",
  # "8th+Avenue+%26+34th+St,New+York,NY|8th+Avenue+%26+42nd+St,New+York,NY|",
  # "Park+Ave+%26+42nd+St,New+York,NY,NY|Park+Ave+%26+34th+St,New+York,NY,NY"),
  # destfile = "MyTile3a.png");
  #note that since the path string is just appended to the URL you can "abuse" the path
  #argument to pass anything to the query, e.g. the style parameter:
  #The following example displays a map of Brooklyn where local roads have been changed
  #to bright green and the residential areas have been changed to black:
  # myMap <- GetMap(center="Brooklyn", zoom=12, maptype = "roadmap",
  #path = paste0("&style=feature:road.local|element:geometry|hue:0x00ff00|",
  # "saturation:100&style=feature:landscape|element:geometry|lightness:-100"),
  # sensor='false', destfile = "MyTile4.png", RETURNIMAGE = FALSE);
  #In the last example we set RETURNIMAGE to FALSE which is a useful feature in general
  #if png is not installed. In that cases, the images can still be fetched
  #and saved but not read into R.
  #In the following example we let the Static Maps API determine the correct center and
  #zoom level implicitly, based on evaluation of the position of the markers.
  #However, to be of use within R we do need to know the values for zoom and
  #center explicitly, so it is better practice to compute them ourselves and
  #pass them as arguments, in which case meta information on the map tile can be saved as well.
  #myMap <- GetMap(markers = paste0("&markers=color:blue|label:S|40.702147,-74.015794&",
  # "markers=color:green|label:G|40.711614,-74.012318&markers=color:red|",
  # "color:red|label:C|40.718217,-73.998284"),
  # destfile = "MyTile1.png", RETURNIMAGE = FALSE);
}


# LEAFLET -----------------------------------------------------------------

# https://rstudio.github.io/leaflet/
# https://rstudio.github.io/leaflet/shapes.html
# https://blog.rstudio.com/2015/06/24/leaflet-interactive-web-maps-with-r/
# https://leafletjs.com/reference-1.3.0.html

require(leaflet)

m <- leaflet() %>%
  addTiles %>% # Add default OpenStreetMap map tiles
  setView(lng = 5.0, lat = 51.0, zoom = 6)
m

# set bounds
m %>% fitBounds(0, 40, 10, 50)
# move the center to Snedecor Hall
m <- m %>% setView(-93.65, 42.0285, zoom = 17)
m
# popup
m %>% addPopups(-93.65, 42.0285, "Here is the <b>Department of Statistics</b>, ISU")
rand_lng <- function(n = 10) rnorm(n, -93.65, .01)
rand_lat <- function(n = 10) rnorm(n, 42.0285, .01)
# use automatic bounds derived from lng/lat data
m <- m %>% clearBounds()
# popup
m %>% addPopups(rand_lng(), rand_lat(), "Random popups")
# marker
m %>% addMarkers(rand_lng(), rand_lat())
m %>% addMarkers(
  rand_lng(), rand_lat(), popup = paste("A random letter", sample(LETTERS, 10))
)
Rlogo <- file.path(R.home("doc"), "html", "logo.jpg")
m %>% addMarkers(
  174.7690922, -36.8523071, icon = list(
    iconUrl = Rlogo, iconSize = c(100, 76)
  ), popup = "R was born here!"
)
m %>% addMarkers(rnorm(30, 175), rnorm(30, -37), icon = list(
  iconUrl = Rlogo, iconSize = c(25, 19)
))
m %>% addMarkers(
  c(-71.0382679, -122.1217866), c(42.3489054, 47.6763144), icon = list(
    iconUrl = "http://www.rstudio.com/wp-content/uploads/2014/03/blue-125.png"
  ), popup = c("RStudio @ Boston", "RStudio @ Seattle")
)
# circle (units in metres)
m %>% addCircles(rand_lng(50), rand_lat(50), radius = runif(50, 50, 150))
# circle marker (units in pixels)
m %>% addCircleMarkers(rand_lng(50), rand_lat(50), color = "#ff0000")
m %>% addCircleMarkers(rand_lng(100), rand_lat(100), radius = runif(100, 5, 15))
# rectangle
m %>% addRectangles(
  rand_lng(), rand_lat(), rand_lng(), rand_lat(),
  color = "red", fill = FALSE, dashArray = "5,5", weight = 3
)
# polyline
m %>% addPolylines(rand_lng(50), rand_lat(50))

# polygon
m %>% addPolygons(rand_lng(), rand_lat(), layerId = "foo")
# geoJSON
seattle_geojson <- list(
  type = "Feature",
  geometry = list(
    type = "MultiPolygon",
    coordinates = list(list(list(
      c(-122.36075812146, 47.6759920119894),
      c(-122.360781646764, 47.6668890126755),
      c(-122.360782108665, 47.6614990696722),
      c(-122.366199035722, 47.6614990696722),
      c(-122.366199035722, 47.6592874248973),
      c(-122.364582509469, 47.6576254522105),
      c(-122.363887331445, 47.6569107302038),
      c(-122.360865528129, 47.6538418253251),
      c(-122.360866157644, 47.6535254473167),
      c(-122.360866581103, 47.6533126275176),
      c(-122.362526540691, 47.6541872926348),
      c(-122.364442114483, 47.6551892850798),
      c(-122.366077719797, 47.6560733960606),
      c(-122.368818463838, 47.6579742346694),
      c(-122.370115159943, 47.6588730808334),
      c(-122.372295967029, 47.6604350102328),
      c(-122.37381369088, 47.660582362063),
      c(-122.375522972109, 47.6606413027949),
      c(-122.376079703095, 47.6608793094619),
      c(-122.376206315662, 47.6609242364243),
      c(-122.377610811371, 47.6606160735197),
      c(-122.379857378879, 47.6610306942278),
      c(-122.382454873022, 47.6627496239169),
      c(-122.385357955057, 47.6638573778241),
      c(-122.386007328104, 47.6640865692306),
      c(-122.387186331506, 47.6654326177161),
      c(-122.387802656231, 47.6661492860294),
      c(-122.388108244121, 47.6664548739202),
      c(-122.389177800763, 47.6663784774359),
      c(-122.390582858689, 47.6665072251861),
      c(-122.390793942299, 47.6659699214511),
      c(-122.391507906234, 47.6659200946229),
      c(-122.392883050767, 47.6664166747017),
      c(-122.392847210144, 47.6678696739431),
      c(-122.392904778401, 47.6709016021624),
      c(-122.39296705153, 47.6732047491624),
      c(-122.393000803496, 47.6759322346303),
      c(-122.37666945305, 47.6759896300663),
      c(-122.376486363943, 47.6759891899754),
      c(-122.366078869215, 47.6759641734893),
      c(-122.36075812146, 47.6759920119894)
    )))
  ),
  properties = list(
    name = "Ballard",
    population = 48000,
    # You can inline styles if you want
    style = list(
      fillColor = "yellow",
      weight = 2,
      color = "#000000"
    )
  ),
  id = "ballard"
)
m %>% setView(-122.36075812146, 47.6759920119894, zoom = 13) %>% addGeoJSON(seattle_geojson)
# use the Dark Matter layer from CartoDB
leaflet() %>% addTiles("http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
                       attribution = paste(
                         "&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap</a> contributors",
                         "&copy; <a href=\"http://cartodb.com/attributions\">CartoDB</a>"
                       )
) %>% setView(-122.36, 47.67, zoom = 10)
# provide a data frame to leaflet()
categories <- LETTERS[1:10]
df <- data.frame(
  lat = rand_lat(100), lng = rand_lng(100), size = runif(100, 5, 20),
  category = factor(sample(categories, 100, replace = TRUE), levels = categories),
  value = rnorm(100)
)
m <- leaflet(df) %>% addTiles()
m %>% addCircleMarkers(~lng, ~lat, radius = ~size)
m %>% addCircleMarkers(~lng, ~lat, radius = runif(100, 4, 10), color = c("red"))
# Discrete colors using the "RdYlBu" colorbrewer palette, mapped to categories
RdYlBu <- colorFactor("RdYlBu", domain = categories)
m %>% addCircleMarkers(~lng, ~lat, radius = ~size,
                       color = ~RdYlBu(category), fillOpacity = 0.5)
# Continuous colors using the "Greens" colorbrewer palette, mapped to value
greens <- colorNumeric("Greens", domain = NULL)
m %>% addCircleMarkers(~lng, ~lat, radius = ~size,
                       color = ~greens(value), fillOpacity = 0.5)

leaflet(data = areas_LL_trf) %>% addPolygons()

# ‘googleway’ -------------------------------------------------------------

# https://cran.r-project.org/web/packages/googleway/vignettes/googleway-vignette.html

library(googleway)

google_map(data = tram_stops, key = map_key) %>%
  add_heatmap(lat = "stop_lat", lon = "stop_lon", option_radius = 0.0025)