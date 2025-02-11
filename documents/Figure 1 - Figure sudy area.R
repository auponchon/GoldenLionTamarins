library(here)
library(raster)
library(tidyverse)
library(sf)
library(viridis)
library(terra)
library(ggspatial)
library(gridExtra)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)


set.seed(456)

source(here::here("R","merge_ummp.R"))
source(here::here("R","create_raster.R"))

#projection in UTM
proj<-"+proj=utm +zone=23 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs"


#define extent for the study region
extreg<-extent(-42.7,-41.9,-22.9,-22.3)
extregproj<-terra::rast(xmin=739000, xmax=809600,ymin=7482000, ymax=7525000,
                 crs=proj,
                 vals=1)


#get groups and regions selected in the final CMR dataset from 2010 to 2022
ind<-read.csv(here::here("data","NewlyCreatedData","CMR",
                         "CMR_3states_AD_Sex_UniaoI_PDA_ImbauI_II_LB_EB_EX_AX2_2010-2022_corrected.csv"),
              header=T,
              sep=";") 
load(here::here("data","NewlyCreatedData","data_clean_long_final.RData"))
gp.reg<-data.clean.final %>% 
  dplyr::filter(GLT%in% ind$GLT & Year > 2008) %>% 
  distinct(Group, UMMPs) %>% 
  dplyr::filter(!is.na(Group) | !is.na(UMMPs)) 


#load all coordinates of groups selected
loc<-read.table(here::here("data","rawData","Landscape","RegionsName.csv"),
                header=T,sep=";") %>% 
  dplyr::rename(Long=CENTROIDE.X.UTM.SAD69.23S,
                Lat=CENTROIDE.Y.UTM.SAD69.23S,
                Group=Abreviation) %>% 
  dplyr::select(-Platform,-City) %>% 
  dplyr::filter(Group %in% gp.reg$Group) %>% 
  dplyr::filter_all(any_vars(!is.na(.))) %>% 
  sf::st_as_sf(., coords = c("Long","Lat")) %>% 
   sf::st_set_crs(proj) # %>% 

# load conservation units (UMMPs)
ummp<-return_complete_ummp() %>% 
  sf::st_transform(.,proj)    #add vendaval and boa esperanza which are missing in shapefile

#remove accents in names
ummp$UMMPs<-plyr::revalue(ummp$UMMPs, c("Imbaú I" = "Imbau I",
                                        "Imbaú II" = "Imbau II",
                                        "Imbaú III" = "Imbau III",
                                        "Poço das Antas" = "Poco das Antas",
                                        "União I" = "Uniao I",
                                        "União II" = "Uniao II"))

#select ummp included in the study
 ummp.sub<-subset(ummp, as.character(ummp$UMMPs) %in% as.character(gp.reg$UMMPs))
# ummp_centroids <- st_centroid(ummp.sub)


#load raster land cover for 2021 obtained from BioMAPS
layer<-list.files(here::here("data","RawData","Landscape","Rasters land cover"),
                 full.names=T)[32]
  

landuse<-create_raster_stack(layer,extreg) 
  
  
landusecrop<-landuse %>% 
  terra::crop(.,extregproj) %>% 
  # terra::writeRaster(., here::here("data","NewlyCreatedData","landscape raster","Habitat.tif"),
  #                    overwrite=T) %>% 
  as.data.frame(. , xy = TRUE) %>%
  rename(value=brasil_coverage_2021)

loc<-st_intersection(loc,ummp)

#load road layer
road<-read_sf(here::here("data","RawData","Landscape","Roads","Roads_BRAZIL.shp"))  %>% 
   st_transform(., crs=proj) %>%
  st_crop(., extregproj)


#extent of the study region
longmn<-terra::ext(extregproj)[1]
longmx<-terra::ext(extregproj)[2]
latmn<-terra::ext(extregproj)[3]
latmx<-terra::ext(extregproj)[4]



  plasma<-sample(inferno(n=10,begin=0.75,end=0.95),size=1)
col<-c("#006d2c","#34cf61","#c7e9c0","#81dfff","#b53613") #"#a1d99b"

map<-ggplot()+
   
  #plot land use raster with habitat
 geom_raster(data=landusecrop, aes (x=x,y=y,fill=as.factor(value)),
             alpha=0.9) +
scale_fill_manual(values = c(col,plasma)) +   #fill with manual colors
  geom_sf(data=ummp,  #add ummp layer
          alpha=0.6,
          col="grey10",
          fill="grey",
          show.legend = F) +
  geom_sf(key_glyph = "rect", color = NULL) + #remove frame in legend
   geom_sf(data=ummp.sub, #add ummp included in the study
           mapping=aes(fill=plasma),
           alpha=0.6,col="grey10",
           show.legend=F)+
  geom_sf(data=road,col="black") + #add road layer
    geom_sf(data=loc,  #add groups included in the study
            aes(fill=plasma),
            shape=23,
            size=3,
            show.legend = F) +
 
 # scale_color_manual(values=plasma) +   #add colors to points
  annotation_scale(location = "br",  #add map scale
                   width_hint = 0.15,
                   pad_x = unit(0.9, "cm"),
                   bar_cols = "black",
                   text_cex = 1.5) +
  annotation_north_arrow(location = "br", #add north arrow
                         which_north = "true",
                         style = ggspatial::north_arrow_nautical(), 
                         pad_y = unit(1, "cm"),
                         height = unit(3, "cm"),
                         width = unit(3, "cm"),) +
  coord_sf(xlim = c(longmn,longmx), #set extent of the study area
           ylim=c(latmn, latmx),
           expand = F) +
  labs(x="",
       y="") +
  theme_bw() +
  theme(legend.position = "none",
        plot.margin = unit(c(0.05, 0, 0, 0), "lines"),
        axis.text = element_text(size=12),
        plot.title=element_text(hjust=0.5,
                                face = "bold",
                                size=12))

#extract legend
legplot<-ggplot()+
  geom_raster(data=landusecrop, aes (x=x,y=y,fill=as.factor(value)),
              alpha=0.9) +
  scale_fill_manual(values = col,
                    labels=c("Forest","Non-forest","Agriculture","Water","Urban areas"),
                    name="Habitats")+
  labs(fill="",
       x="",
       y="")+
theme(legend.position = "bottom",
      legend.text = element_text(size=14),
      legend.title = element_text(size=16))

leg<-g_legend(legplot)

#plot brazil as an inset map
braz <- rnaturalearth::ne_states("Brazil", returnclass = "sf") %>% 
  st_transform(., crs=proj) 

x1<-extent(braz)[1] -70000
x2<-1723876
y1<-extent(braz)[3] -70000
y2<-extent(braz)[4] +70000


inset <- braz %>% 
  ggplot() + 
  geom_sf(col="black",
          fill="white") +
  coord_sf(xlim = c(x1,x2),
           ylim=c(y1, y2),
           expand = F) +
  geom_rect(aes(xmin =longmn,
                xmax = longmx, 
                ymin = latmn, 
                ymax = latmx), 
            color = "red", 
            fill = NA,
            lwd=1.2) +
  labs(x = NULL, y = NULL) +
  theme_test() + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "pt"),
        axis.title=element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        panel.background = element_rect(fill = "grey90"))+
  annotation_scale(location = "br", 
                   width_hint = 0.2,
                   pad_x = unit(2.8, "cm"),
                   bar_cols = "black",
                   line_width = 0.4,
                   text_cex = 0.8)


#print(inset)

#plot inset and study area map
combo<-ggdraw() +
  draw_plot(map) +
  draw_plot(inset,
            height = 0.27,
            width=0.27,
             x=0.04,
             y=0.7) #bottom right
#print(combo)

tiff(here::here("outputs","Figure1_study_area.tiff"),
     height=3000,
     width=5000,
     res=400,
     type="cairo")
grid.arrange(combo,leg,
              nrow=2, 
             heights=c(1,0.05)  )
dev.off()

