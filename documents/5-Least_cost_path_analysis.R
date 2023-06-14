library(leastcostpath)
library(here)
library(tidyverse)
library(raster)
library(sf)

source(here::here("R","create_raster.R"))
source(here::here("R","merge_ummp.R"))

#extent of study area
extreg<-extent(-42.7,-41.9,-22.9,-22.3) 

#get land cover for each year from 1990 to 2021
#raster land cover for each year obtained from BioMAPS
rast<-list.files(here::here("data","RawData","Landscape","Rasters land cover"),
                 full.names=T)

#coverStack<-purrr::map(rast,create_raster_stack,.progress=T)
#save(coverStack,file=here::here("data","NewlyCreatedData","LandUseStack.RData"))
load(here::here("data","NewlyCreatedData","LandUseStack.RData"))

#Extract 1 raster for extent
landuse<-coverStack[[1]]

#Import file with locations associated with groups
loc<-read.table(here::here("data","rawData","Landscape","RegionsName.csv"),
                header=T,sep=";") %>% 
  dplyr::rename(Long=CENTROIDE.X.UTM.SAD69.23S,
                Lat=CENTROIDE.Y.UTM.SAD69.23S,
                Group=Abreviation) %>% 
  dplyr::select(-Platform,-City) %>% 
  dplyr::filter_all(any_vars(!is.na(.)))


#import road layers so that they can be rasterized in QGIS
# road1<-read_sf(here::here("data","RawData","Landscape","Roads","BRA_roads.shp")) %>% 
#        st_transform(.,proj) %>% 
#        st_crop(.,landuse)
# road2<-read_sf(here::here("data","RawData","Landscape","Roads","rodovia-estadual.shp")) %>% 
#   st_transform(.,proj) %>% 
#   st_crop(.,landuse)
# road3<-read_sf(here::here("data","RawData","Landscape","Roads","ferrovia.shp")) %>%        
#   st_transform(.,proj) %>% 
#   st_crop(.,landuse)
# roads<-st_union(road1,road2,road3) %>%
#     dplyr::select(1:3) %>% 
#   write_sf(.,dsn=here::here("data","RawData","Landscape","Roads","RoadsTrainUnionBrazil.shp"),
#            delete_dns=T,
#            delete_layer=T)

roads<-read_sf(here::here("data","RawData","Landscape","Roads","BRA_roads.shp")) 


#extract topography for the study site from elevatr package
library(elevatr)
# stage_bbox = st_bbox(landuse)
# ex.df <- data.frame(x= c(stage_bbox[['xmin']], stage_bbox[['xmax']]), 
#                     y= c(stage_bbox[['ymin']], stage_bbox[['ymax']]))
# elev_img <- get_elev_raster(ex.df, prj = proj, z = 12, clip = "bbox") %>% 
#             resample(., landuse) %>% 
# plot(elev_img)
#save(elev_img,file=here::here("data","NewlyCreatedData","elevationRaster.RData"))
load(here::here("data","NewlyCreatedData","elevationRaster.RData"))
elev_img[elev_img<0]<-NA

#get cost based on topography
slope_cs <-create_slope_cs(x=elev_img, cost_function = "tobler", neighbours=16)

plot.conductanceMatrix(slope_cs$conductanceMatrix)





