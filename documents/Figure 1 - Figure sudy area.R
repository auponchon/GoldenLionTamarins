library(here)
library(raster)
library(tidyverse)
library(sf)
library(viridis)
library(ggspatial)

source(here::here("R","merge_ummp.R"))
source(here::here("R","create_raster.R"))

extreg<-extent(-42.7,-41.9,-22.9,-22.3)
extregproj<-extent(740500, 809200,7481000, 7524000)

#extentinset
proj<-"+proj=utm +zone=23 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"


#get groups and regions retained in the final dataset from 2009 to 2022
ind<-read.csv(here::here("data","NewlyCreatedData","CMR",
                         "CMR_3states_AD_Sex_UniaoI_PDA_ImbauI_II_LB_EB_EX_AX2_2009-2022.csv"),
              header=T,
              sep=";") 
load(here::here("data","NewlyCreatedData","data_clean_long_final.RData"))
gp.reg<-data.clean.final %>% 
  dplyr::filter(GLT%in% ind$GLT & Year > 2008) %>% 
  distinct(Group, UMMPs) %>% 
  dplyr::filter(!is.na(Group) | !is.na(UMMPs))



#load all coordinates of groups
loc<-read.table(here::here("data","rawData","Landscape","RegionsName.csv"),
                header=T,sep=";") %>% 
  dplyr::rename(Long=CENTROIDE.X.UTM.SAD69.23S,
                Lat=CENTROIDE.Y.UTM.SAD69.23S,
                Group=Abreviation) %>% 
  dplyr::select(-Platform,-City) %>% 
  dplyr::filter(Group %in% gp.reg$Group) %>% 
  dplyr::filter_all(any_vars(!is.na(.))) %>% 
  sf::st_as_sf(., coords = c("Long","Lat")) %>% 
  sf::st_set_crs(proj) 


# hab_df<-coverStack[[max(length(coverStack))]]   %>% 
# as.data.frame(. , xy = TRUE) %>%
#   rename(value=brasil_coverage_2021)


#            as(., "SpatialPixelsDataFrame") %>%
#              as.data.frame(.)
# colnames(hab_df) <- c("value", "x", "y")

#conservation units (UMMPs)
ummp<-return_complete_ummp() %>% 
  sf::st_transform(.,proj)    #add vendaval and boa esperanza which are missing in shapefile

  
#remove accents in names
ummp$UMMPs<-plyr::revalue(ummp$UMMPs, c("Imbaú I" = "Imbau I",
                                        "Imbaú II" = "Imbau II",
                                        "Imbaú III" = "Imbau III",
                                        "Poço das Antas" = "Poco das Antas",
                                        "União I" = "Uniao I",
                                        "União II" = "Uniao II"))

ummp.sub<-subset(ummp, as.character(ummp$UMMPs) %in% as.character(gp.reg$UMMPs))
  
#raster land cover for each year obtained from BioMAPS
layer<-list.files(here::here("data","RawData","Landscape","Rasters land cover"),
                 full.names=T)[34]
  
ext<-extent(-43,-41,-23,-22)
landuse<-create_raster_stack(layer,ext)
  

ext<-extent(extent(landuse)[1],
       extent(landuse)[2],
       7478887,
       extent(landuse)[4])

landusecrop<-crop(landuse,ext) %>% 
  as.data.frame(. , xy = TRUE) %>%
  rename(value=brasil_coverage_2021) 

loc<-st_intersection(loc,ummp)

#extent of the study region
longmn<-extent(landusecrop)[1]
longmx<-extent(landusecrop)[2]
latmn<-extent(landusecrop)[3]
latmx<-extent(landusecrop)[4]



  plasma<-inferno(n=9,begin=0.75,end=0.9)
col<-c("#006d2c","#c7e9c0","#c7e9c0","skyblue","white",plasma) #"#a1d99b"
# brazil<-sf::read_sf(here("data","RawData","Landscape","Shapefiles Landscape AMLD",
#                          "World_WGS84_Fine_Reso.shp")) %>% 
#   sf::st_transform(31983) 

gg<-ggplot()+
    # geom_sf(data=land,
    #         fill="grey80",
    #         col="grey80") +
 geom_raster(data=landusecrop, aes (x=x,y=y,fill=as.factor(value)),
             alpha=0.9)+
  geom_sf(data=ummp,
          mapping=aes(fill=UMMPs),alpha=0.6,
          col="grey10",
          fill="grey") +
scale_fill_manual(values = col)+
   geom_sf(data=ummp.sub,
           mapping=aes(fill=UMMPs),alpha=0.6,col="grey10")+
    geom_sf(data=loc,
            aes(fill=UMMPs),
            shape=23,
            size=3,
            show.legend = F)+
  scale_color_manual(values=plasma) +
  annotation_scale(location = "br", 
                   width_hint = 0.15,
                   pad_x = unit(0.8, "cm"),
                   bar_cols = "black") +
  annotation_north_arrow(location = "br", 
                         which_north = "true",
                         style = ggspatial::north_arrow_nautical(), 
                         pad_y = unit(0.8, "cm")) +
  coord_sf(xlim = c(longmn,longmx),
           ylim=c(latmn, latmx),
           expand = FALSE) +
  labs(x="",
       y="") +
  theme_bw() +
  theme(legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 0, 0), "lines"),
        axis.text = element_text(size=10),
        plot.title=element_text(hjust=0.5,
                                face = "bold",
                                size=12))
#  inset(braz,)
print(gg)    


