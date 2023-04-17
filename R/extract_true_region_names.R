extract_true_region_names<-function(){

library(sf)
  
#import polygons with right regions names
land<-sf::read_sf(here::here("data","RawData","Landscape","Shapefiles Landscape AMLD", "SIG-LGCI_UMMP-13.shp")) %>% 
  dplyr::select(Name,Id,UMMPs,geometry) %>% 
  revalue_true_regions()

#import spatial locations of groups
loc<-read.table(here::here("data","rawData","Landscape","RegionsName.csv"),
                header=T,
                sep=";",
                as.is=T) %>% 
  dplyr::rename(Long=CENTROIDE.X.UTM.SAD69.23S,
                Lat=CENTROIDE.Y.UTM.SAD69.23S,
                Group=Abreviation) %>% 
  dplyr::select(-Platform,-City) %>% 
  sf::st_as_sf(., coords = c("Long","Lat")) %>% 
  sf::st_set_crs(31983) 


group_regions<-st_join(loc,land) %>% 
  dplyr::select(Group,Farm,Id,UMMPs) %>% 
  st_drop_geometry()


return(group_regions)
}
