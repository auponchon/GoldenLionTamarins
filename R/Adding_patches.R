
library(here)
library(raster)
library(tidyverse)
library(sf)

#get landscape polygons
land<-sf::read_sf(here::here("data","RawData","Landscape","Shapefiles Landscape AMLD", 
                             "SIG-EDUC Redescobrindo 2021 - Fragmentos de Vegetação.shp")) %>% 
   dplyr::select(ID,UMMPs)

#get conservation area names
ummp<-sf::read_sf(here::here("data","RawData","Landscape","Shapefiles Landscape AMLD",
                             "SIG-LGCI_UMMP-13.shp")) %>% 
  dplyr::select(Id,UMMPs,geometry) %>% 
  dplyr::mutate(UMMPs = as.factor(UMMPs)) 


#isolate vendaval region
vendaval<-land %>% 
  dplyr::filter(ID==100) %>% 
  dplyr::rename(Id=ID) %>% 
  st_buffer(., 100)  


#isolate boa esperanza region
boaesperanz<-land %>% 
  dplyr::filter(ID>100) %>% 
  dplyr::rename(Id=ID) %>% 
  dplyr::mutate(UMMPs="Boa Esperanca") %>%
  st_buffer(., 100) 
  # st_sf(st_sfc(.))


sfg_list <- st_sfc(vendaval)

st_combine(sfg_list)








