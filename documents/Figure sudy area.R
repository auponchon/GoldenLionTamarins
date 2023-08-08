library(here)
library(raster)
library(tidyverse)
library(sf)
library(rcartocolor)

source(here::here("R","merge_ummp.R"))

extreg<-extent(-42.7,-41.9,-22.9,-22.3)
extregproj<-extent(740500, 809200,7481000, 7524000)
proj<-"+proj=utm +zone=23 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"


#raster land cover for each year obtained from BioMAPS
rast<-raster(here::here("data","rawData","Landscape","Rasters land cover",
                        "brasil_coverage_2021.tif")) %>% 
  raster::crop(.,extreg) %>% 
  projectRaster(., crs=proj ,method="ngb",res=30) %>%
  raster::crop(.,extregproj) 

#conservation units (UMMPs)
ummp<-return_complete_ummp() #add vendaval and boa esperanza which are missing in shapefile

#remove accents in names
ummp$UMMPs<-plyr::revalue(ummp$UMMPs, c("Imbaú I" = "Imbau I",
                                        "Imbaú II" = "Imbau II",
                                        "Imbaú III" = "Imbau III",
                                        "Poço das Antas" = "Poco das Antas",
                                        "União I" = "Uniao I",
                                        "União II" = "Uniao II"))


#forest patches from shapefile
land<-sf::read_sf(here::here("data","RawData","Landscape","Shapefiles Landscape AMLD", 
                             "SIG-EDUC Redescobrindo 2021 - Fragmentos de Vegetação.shp"))

loc<-read.table(here::here("data","rawData","Landscape","RegionsName.csv"),
                header=T,sep=";") %>% 
  dplyr::rename(Long=CENTROIDE.X.UTM.SAD69.23S,
                Lat=CENTROIDE.Y.UTM.SAD69.23S,
                Group=Abreviation) %>% 
  dplyr::select(-Platform,-City) %>% 
  dplyr::filter_all(any_vars(!is.na(.))) %>% 
  sf::st_as_sf(., coords = c("Long","Lat")) %>% 
  sf::st_set_crs(31983)


plot(land$geometry,col="grey90",add=T)
plot(loc,add=T,col="red",pch=16)
