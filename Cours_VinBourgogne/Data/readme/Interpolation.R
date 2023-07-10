library(sf)
library(tidyverse)

### Jointure des prix et autres infos vignobles:

## Données issues de : 
# https://journals.openedition.org/cybergeo/36443
# https://cesaer-datas.inra.fr/geoind/
  
polyras <- st_read("PolyRas.shp") %>% st_transform(crs = "EPSG:2154")

## Données issues de : https://www.hachette-vins.com/vins/list/?filtre[region]=Bourgogne&filtre[millesime]=1998|2002|2003&filtre[appellation]=Vougeot

prix <- read_excel("prix.xlsx")
prix$PRIX_1 <- as.numeric(prix$PRIX_1)
prix_vin <- prix %>%
  mutate(Source = as.character(ifelse(is.na(Prix_moyen), "interpolation",
                                      ifelse(PRIX_1>100,"wine_search","guide_hachette")))) %>%
  select(Concat,Prix_moyen,Source)

## Jointure

polyrasPrix <- left_join(polyras,prix_vin,by="Concat")

## Interpolation selon methode issue de : https://rspatial.org/analysis/4-interpolation.html

polyrasPrix$Source <- ifelse(is.na(polyrasPrix$Source), 
                             "interpolation",
                             as.character(polyrasPrix$Source))

interpolation_des_prix <- function(layer1, layer2, 
                                   resolution){
  if (!require("rspat")) remotes::install_github('rspatial/rspat')
  library(rspat)
  require(sp)
  require(rgeos)
  require(gstat)
  
  layer1_centroids <- st_centroid(polyrasPrix) %>%
    filter(!is.na(Prix_moyen))
  
  
  RMSE <- function(observed, predicted) {
    sqrt(mean((predicted - observed)^2, na.rm=TRUE))
  }
  null <- RMSE(mean(layer1_centroids$Prix_moyen), layer1_centroids$Prix_moyen)
  null
  
  vsp <- as(layer1_centroids, "Spatial")
  vectvsp <- vect(vsp)
  v <- voronoi(vectvsp)
  
  vsp_layer2 <- as(layer2, "Spatial")
  vsp_layer2 <- gBuffer(vsp_layer2, byid=TRUE, width=0)
  cata <- vect(vsp_layer2)
  vca <- crop(v, cata)
  
  r <- rast(vca, res=resolution)
  vr <- rasterize(vca, r, "Prix_moyen")
  d <- data.frame(geom(vectvsp)[,c("x", "y")], as.data.frame(vectvsp))
  gs <- gstat(formula=Prix_moyen~1, locations=~x+y, data=d)
  idw <- interpolate(r, gs, debug.level=0)
  idwr <- mask(idw, vr)
  
  layer1_estimate <- extract(x = idwr, y = layer1, df = TRUE) %>%
    group_by(ID) %>%
    summarise(meanEstPrice = mean(var1.pred, na.rm=T)) 
  
  vignobles_sf <- cbind(layer1,layer1_estimate)
  return(vignobles_sf)
}


vignobles_sf <- interpolation_des_prix(layer1=polyrasPrix, 
                                       layer2=Cotedor,
                                       resolution=50)

vignobles_sf <- vignobles_sf %>%
  mutate(PrixMoy = ifelse(Source == "interpolation", round(meanEstPrice),Prix_moyen)) %>%
  select(Concat, LIBCOM,NOM,NIVEAU,SURFACE,
         SCORE_b,Source,PrixMoy)

st_write(obj = vignobles_sf, 
         dsn = "/Users/ccottineau/Documents/GitHub/QuantiLilleGeo/Cours_VinBourgogne/Data/Geo/Vignobles_prix.shp",
         driver = "ESRI Shapefile")


### Selection des données communales

## données communes issues de : 
# https://geo.data.gouv.fr/en/datasets/cac9f2c0de2d3a0209af2080854b6f6a7ee3d9f4

COM <- st_read("vin/COM/n_commune_geofla_s_fr.shp") %>% 
  st_transform(crs = "EPSG:2154")

Cotedor <- COM %>% 
  dplyr::filter(nom_comm %in% unique(polyras$LIBCOM) & code_dept == 21)

Cotedor$Cote <- ifelse(Cotedor$nom_comm %in% 
                         c("FLAGEY-ECHEZEAUX","BROCHON",
                           "VOSNE-ROMANEE", "FIXIN","NUITS-SAINT-GEORGES" ,
                           "GEVREY-CHAMBERTIN", "PREMEAUX-PRISSEY" , "CORGOLOIN"  ,
                           "COMBLANCHIEN", "MOREY-SAINT-DENIS" , 
                           "CHAMBOLLE-MUSIGNY"  ,"VOUGEOT" ),
                      "Côte de Nuit", 
                      ifelse(Cotedor$nom_comm %in% 
                               c("CHENOVE","MARSANNAY-LA-COTE",
                                 "COUCHEY"), "Côte D'or", 
                      ifelse(!is.na(Cotedor$nom_comm), 
                             "Côte de Beane", NA)))

st_write(obj = Cotedor,
         dsn = "/Users/ccottineau/Documents/GitHub/QuantiLilleGeo/Cours_VinBourgogne/Data/Geo/Communes.shp",
         driver = "ESRI Shapefile")
