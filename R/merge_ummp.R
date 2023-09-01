return_complete_ummp<-function(){

#get landscape polygons
land<-sf::read_sf(here::here("data","RawData","Landscape","Shapefiles Landscape AMLD", 
                             "SIG-EDUC Redescobrindo 2021 - Fragmentos de Vegetação.shp")) %>% 
   dplyr::select(ID)

#get conservation area names
ummp<-sf::read_sf(here::here("data","RawData","Landscape","Shapefiles Landscape AMLD",
                             "SIG-LGCI_UMMP-13.shp")) %>% 
  dplyr::select(Id,UMMPs,geometry) %>% 
  dplyr::mutate(UMMPs = as.factor(UMMPs)) 


# #isolate vendaval region
# vendaval<-land %>% 
#   dplyr::filter(ID==100) %>% 
#   dplyr::mutate(UMMPs="Boa Esperanca") %>% 
#   dplyr::rename(Id=ID) %>% 
#   dplyr::select(Id,UMMPs,geometry) %>% 
#   st_buffer(., 100)  
# 
# 
# #isolate boa esperanza region and bind vendaval
# boaesperanz<-land %>% 
#   dplyr::filter(ID>100) %>% 
# #  dplyr::rename(Id=ID) %>% 
#   dplyr::mutate(UMMPs="Boa Esperanca") %>% 
#   dplyr::rename(Id=ID) %>% 
#   dplyr::select(Id,UMMPs,geometry) %>% 
#    st_buffer(., 100) 
#   
# ummp_add<- boaesperanz %>% 
#   rbind(vendaval)  %>% 
#   rbind(ummp)


ummp_add<-ummp

ummp_add$UMMPs<-plyr::revalue(ummp_add$UMMPs, c("Imbaú I" = "Imbau I",
                                        "Imbaú II" = "Imbau II",
                                        "Imbaú III" = "Imbau III",
                                        "Poço das Antas" = "Poco das Antas",
                                        "União I" = "Uniao I",
                                        "União II" = "Uniao II"))


return(ummp_add)
}






