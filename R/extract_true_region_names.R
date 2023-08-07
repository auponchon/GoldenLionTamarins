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
  
  #load groups without coordinates but with region information
  gp.reg<-read.csv2(here::here("data","rawData","Landscape","groups_with_corrected_ummp.csv"),
                    header=T,
                    as.is=T) %>% 
    dplyr::filter(!is.na(Group))
  
#import spatial locations of groups
loc<-read.table(here::here("data","rawData","Landscape","RegionsName.csv"),
                header=T,
                sep=";",
                as.is=T) %>% 
  dplyr::rename(Long=CENTROIDE.X.UTM.SAD69.23S,
                Lat=CENTROIDE.Y.UTM.SAD69.23S,
                Group=Abreviation,
                Region=Fragment.Region) %>% 
  sf::st_as_sf(., coords = c("Long","Lat")) %>% 
  sf::st_set_crs(31983) 


group_regions<-st_join(loc,ummp) %>% 
  dplyr::select(Group,Region,UMMPs) %>% 
  st_drop_geometry()

nolocgroup<-gp.reg[!gp.reg$Group %in% group_regions$Group,]


group_regions_all<-group_regions %>% 
      rbind(.,nolocgroup) %>% 
      dplyr::arrange(Group) %>% 
  dplyr::select(-Region)

return(group_regions_all)
}
