extract_true_region_names<-function(){

library(sf)
  source(here::here("R","merge_ummp.R"))
  
#import polygons with right regions names
  ummp<-return_complete_ummp()
  
  ummp$UMMPs<-plyr::revalue(ummp$UMMPs, c("Imbaú I" = "Imbau I",
                                          "Imbaú II" = "Imbau II",
                                          "Imbaú III" = "Imbau III",
                                          "Poço das Antas" = "Poco das Antas",
                                          "União I" = "Uniao I",
                                          "União II" = "Uniao II"))
  
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


group_regions<-st_join(loc,ummp) %>% 
  dplyr::select(Group,Farm,Id,UMMPs) %>% 
  st_drop_geometry()


return(group_regions)
}
