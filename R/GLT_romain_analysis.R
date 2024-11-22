####### GLT observation data analysis
### Author: Romain Monassier
### Date: 2023

##### Libraries
load.lib = c("dplyr","tibble", "data.table",
             "tidyr", "xlsx", "zoo", "readxl",
             "readr", "ggplot2", "lubridate",
             "plyr", "tidyverse", "changepoint",
             "sp", "raster", "here", "sf", 
             "fitbitViz", "terra", "psych",
             "ggpubr", "cowplot", "lme4", "MASS",
             "glmmTMB", "broom.mixed", "emmeans","multcomp",
             "ggeffects", "lmerTest", "car", "outliers",
             "ggalt")
sapply(load.lib,require,character=TRUE)


#### Load data

### GLT dataset
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/data_clean_v2.RData")
data_clean_v2 = data_clean_v2 %>% 
  dplyr::group_by(rowid) %>% 
  dplyr::mutate(Ref_GLT_Year = paste("01","07",Year, sep="/")) %>% 
  dplyr::ungroup() # Create "GLT Year"
data_clean_v2$Ref_GLT_Year = as.Date(data_clean_v2$Ref_GLT_Year, format="%d/%m/%Y")
data_clean_v2 = data_clean_v2 %>% 
  dplyr::mutate(GLT_Year1 = ifelse(DateObs >= Ref_GLT_Year, Year, Year-1)) %>% 
  dplyr::mutate(GLT_Year2 = ifelse(DateObs >= Ref_GLT_Year, paste(Year, Year+1, sep="-"), paste(Year-1, Year, sep="-")))


### Monitored groups
Nb_obs_gp = read_delim("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Nb_obs_gp.csv", delim=";", show_col_types = FALSE)
# Join
data_clean_v2 = data_clean_v2 %>%
  dplyr::left_join(dplyr::select(Nb_obs_gp,c(Group,UMMPs,Farm,Lat,Long,Fragment.Region)))


# ### Worldclim
# # UMMPs
# source("R/merge_ummp.R")
# ummp = return_complete_ummp()
# ummp_bb = extend_AOI_buffer(
#   ummp,
#   buffer_in_meters = 7000
# ) # Extract bounding box with buffer
# ummp_bb = ummp_bb[[2]]
# 
# # Crop rasters with study area bounding box
# files = list.files("C:/Users/monas/OneDrive/Bureau/GLT/data/wc2.1_cruts4.06_5m_prec_2000_2021",
#                            pattern = "*.tif$", full.names = TRUE) # read the rasters
# files = lapply(files, terra::rast) # convert as terra::rasters
# crop.list = list()
# for(i in 1:length(files)){
#   crop.list[[i]] = terra::crop(files[[i]], ummp_bb, mask=F)
# } # crop with a mask
# lapply(crop.list, function (x) writeRaster(x, filename=paste0("C:/Users/monas/OneDrive/Bureau/GLT/output","/precipitation/",names(x)))) # write rasters
# 
# # Calculate mean by raster
# files = as.list(list.files("C:/Users/monas/OneDrive/Bureau/GLT/output/precipitation",
#                            pattern = "*.tif$", full.names = TRUE)) #create list of raster file paths
# outlist <- list() #create empty list to store outputs from loop
# for (i in 1:length(files)) { # for each raster in rasterlist
#   r <- raster(files[[i]]) # read element i of rasterlist into R
#   val <- getValues(r) # get raster values
#   m <- mean(val,na.rm=T) # remove NAs and compute mean
#   outlist[[i]] <- c(files[[i]],m) # store raster path with mean
# }
# rainfall <- data.frame(do.call(rbind,outlist)) #convert list to data frame
# colnames(rainfall) <- c("raster_path","mean_rainfall")
# rainfall[,1] <- gsub("C:/Users/monas/OneDrive/Bureau/GLT/output/precipitation/wc2.1_5m_prec_", "", rainfall[,1])
# rainfall[,1] <- gsub(".tif", "", rainfall[,1])
# rainfall$Date = as.Date(paste(rainfall$raster_path, "-01", sep=""))
# rainfall$Year = as.numeric(format(rainfall$Date, format="%Y"))
# rainfall$mean_rainfall = as.numeric(rainfall$mean_rainfall)
# 
# # Calculate cumulative rainfall
# rainfall = rainfall %>%
#   dplyr::mutate(Ref_GLT_Year = paste("01","07",Year, sep="/")) %>%
#   dplyr::ungroup()
# rainfall$Ref_GLT_Year = as.Date(rainfall$Ref_GLT_Year, format="%d/%m/%Y")
# rainfall = rainfall %>%
#   dplyr::mutate(GLT_Year1 = ifelse(Date >= Ref_GLT_Year, Year, Year-1)) %>%
#   dplyr::mutate(GLT_Year2 = ifelse(Date >= Ref_GLT_Year, paste(Year, Year+1, sep="-"), paste(Year-1, Year, sep="-")))
# cumul_rainfall = rainfall %>%
#   dplyr::group_by(GLT_Year1) %>%
#   dplyr::mutate(cum_mean_cell_rainfall = cumsum(mean_rainfall)) %>%
#   dplyr::filter(cum_mean_cell_rainfall == max(cum_mean_cell_rainfall)) %>%
#   dplyr::select(GLT_Year1, GLT_Year2, cum_mean_cell_rainfall) %>%
#   dplyr::filter(GLT_Year1 != 1999 & GLT_Year1 != 2021) %>% 
#   dplyr::ungroup()
# save(cumul_rainfall, file="D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/cumul_rainfall.RData")
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/cumul_rainfall.RData")
# ggplot(cumul_rainfall, aes(x=cum_mean_cell_rainfall))+
#   geom_histogram(aes(y = after_stat(density)), fill='lightgray', col='darkgrey') +
#   geom_density(lwd = 1, colour = 4,
#                fill = 4, alpha = 0.25) +
#   xlab("Mean cumulative rainfall") + 
#   ylab("Density") +
#   theme(legend.title = element_blank())
# plot <- ts(cumul_rainfall$cum_mean_cell_rainfall, start = 2000)
# plot.ts(plot, type= "b", main = "Mean rainfall over time", xlab = "Years", ylab = "Mean rainfall (in mm)")


### Fragment parameters
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/TotalPerimetre.RData")
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/TotalShape.RData")
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/TotalSizeArea.RData")
# plot = totsizearea %>% 
#   dplyr::group_by(Year) %>%
#   dplyr::summarise(mean = mean(Size)) %>% 
#   ungroup() %>% 
#   as.data.frame()
# plot <- ts(plot$mean, start = 1990)
# plot.ts(plot, type= "b", main = "Mean fragment size over time", xlab = "Years", ylab = "Mean fragment size")
# ggplot(totsizearea,aes(x = UMMPs, y = Size)) +
#   geom_boxplot() +
#   xlab("UMMPs") + 
#   ylab("Fragment size")


## Join fragment parameters
data_clean_v2 = data_clean_v2 %>% 
  dplyr::left_join(dplyr::select(totperim,c(Group,Year,Perimetre))) %>% 
  dplyr::left_join(dplyr::select(totshape,c(Group,Year,Shape))) %>% 
  dplyr::left_join(dplyr::select(totsizearea,c(Group,Year,Size))) %>% 
  dplyr::distinct(rowid, .keep_all =TRUE)
data_clean_v2 = data_clean_v2 %>% 
  dplyr::left_join(dplyr::select(cumul_rainfall,c(GLT_Year1,cum_mean_cell_rainfall)))
data_clean_v2 = data_clean_v2%>%                                        
  dplyr::group_by(GLT_Year1,Shape,Size,Perimetre) %>%
  dplyr::mutate(id_frag = as.character(cur_group_id())) %>% 
  ungroup() %>% 
  as.data.frame() # Create ID by fragment/year
data_clean_v2 = data_clean_v2 %>% 
  dplyr::group_by(id_frag, GLT_Year1) %>% 
  dplyr::mutate(weight = n_distinct(Group)) %>% 
  dplyr::ungroup() # Compute a weight to take the monitoring effort into account
data_clean_v2 = data_clean_v2 %>% 
  dplyr::mutate(Size_km2 = Size/100) %>% 
  dplyr::mutate(Size_cat = ifelse(Size<=60, "Petit",
                                  ifelse(Size>60, "Grand", NA)))
data_clean_v2$Size_cat = factor(data_clean_v2$Size_cat, levels=c("Petit","Grand")) # Fragment size (small/large)



### SUBSET DATA FOR GROUP DYNAMICS ANALYSIS
sub = data_clean_v2 %>% 
  dplyr::filter(File == "Obs") %>% # Filter by type of file (Obs)
  dplyr::group_by(Group) %>% 
  dplyr::filter(GLT_Year1 > 2000 & GLT_Year1 <= 2020) %>% # Filter by period
  dplyr::filter(!is.na(UMMPs)) %>%
  dplyr::mutate(Monit_Years_grp = diff(range(GLT_Year1))+1) %>% 
  dplyr::mutate(Monit_1stYear_grp = min(GLT_Year1), 
                Monit_LastYear_grp = max(GLT_Year1)) %>%
  dplyr::mutate(Monit_Period_grp = paste0(unique(c(Monit_1stYear_grp, Monit_LastYear_grp)), collapse = '-')) %>%
  ungroup() %>% 
  as.data.frame() # Create monitoring periods by group
sub = sub %>% 
  dplyr::group_by(UMMPs) %>% 
  dplyr::mutate(Monit_Years_frag = diff(range(GLT_Year1))+1) %>% 
  dplyr::mutate(Monit_1stYear_frag = min(GLT_Year1), 
                Monit_LastYear_frag = max(GLT_Year1)) %>%
  dplyr::mutate(Monit_Period_frag = paste0(unique(c(Monit_1stYear_frag, Monit_LastYear_frag)), collapse = '-')) %>%
  ungroup() %>% 
  as.data.frame() # Create monitoring periods by group
sub = sub %>% 
  dplyr::group_by(Group, Monit_Years_grp) %>% 
  dplyr::filter(Monit_Years_grp >= 6) %>% # Subset groups monitored >= 6 years
  dplyr::filter(!is.na(Group)) %>%
  ungroup() %>% 
  as.data.frame()


### STATISTICAL SUMMARY

# ## Group sample
# sub %>% 
#   dplyr::select(GLT_Year1, Monit_Years_grp) %>% 
#   summary()
# sub %>% 
#   dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="T0" & GLT!="FT") %>% 
#   dplyr::summarise(n=n(),
#                    n_mu = n_distinct(UMMPs),
#                    n_grp = n_distinct(Group), 
#                    tot_GLT = n_distinct(GLT))
# visites_grp = sub %>% 
#   dplyr::group_by(Group,Monit_Years_grp) %>% 
#   dplyr::summarise(n_visites=n_distinct(DateObs),
#                    mean_obs_year = n_visites/Monit_Years_grp) %>% 
#   distinct(Group,n_visites,mean_obs_year) %>% 
#   ungroup()
# visites_grp %>% 
#   dplyr::select(n_visites, mean_obs_year) %>%
#   psych::describe(quant=c(.25,.75)) %>%
#   as_tibble(rownames="rowname")  %>%
#   print()
# 
# # Distribution plot
# ggplot(visites_grp, aes(x=Monit_Years_grp))+
#   geom_histogram(aes(y = ..density..), fill='lightgray', col='darkgrey') +
#   geom_density(lwd = 1, colour = 4,
#                fill = 4, alpha = 0.25) +
#   xlab("Monitoring period (in years)") + 
#   ylab("Density") +
#   theme(legend.title = element_blank())
# ggplot(visites_grp, aes(x=n_visites))+
#   geom_histogram(aes(y = ..density..), fill='lightgray', col='darkgrey') +
#   geom_density(lwd = 1, colour = 4,
#                fill = 4, alpha = 0.25) +
#   xlab("Frequency of visits") + 
#   ylab("Density") +
#   theme(legend.title = element_blank())
# ggplot(visites_grp, aes(x=mean_obs_year))+
#   geom_histogram(aes(y = ..density..), fill='lightgray', col='darkgrey') +
#   geom_density(lwd = 1, colour = 4,
#                fill = 4, alpha = 0.25) +
#   xlab("Mean number of annual visits") + 
#   ylab("Density") +
#   theme(legend.title = element_blank())
# # Plot monitoring periods by fragment
# g <- sub[order(sub$Monit_1stYear_frag,sub$UMMPs),]
# g$UMMPs <- factor(g$UMMPs, levels=unique(g$UMMPs))
# ggplot(g, aes(x = Monit_1stYear_frag, y = UMMPs)) +
#   geom_segment(aes(xend = Monit_LastYear_frag, yend = UMMPs), colour = "orange") +
#   geom_point(size = 2, colour="darkorange") +
#   geom_point(aes(x = Monit_LastYear_frag), size = 2, colour="darkorange") +
#   theme_bw() +
#   theme(legend.position = "none",
#         text = element_text(size=10)) +
#   xlab("Monitoring period") + 
#   ylab("Management unit")
# # Plot monitoring periods by fragment
# g = sub %>% 
#   dplyr::group_by(Group) %>% 
#   dplyr::filter(Monit_Years_grp >= 6)
# g <- g[order(g$Monit_1stYear_grp,g$Group),]
# g$Group <- factor(g$Group, levels=unique(g$Group))
# ggplot(g, aes(x = Monit_1stYear_grp, y = Group)) +
#   geom_segment(aes(xend = Monit_LastYear_grp, yend = Group), colour = "orange") +
#   geom_point(size = 2, colour="darkorange") +
#   geom_point(aes(x = Monit_LastYear_grp), size = 2, colour="darkorange") +
#   theme_bw() +
#   theme(legend.position = "none") +
#   xlab("Monitoring period") + 
#   ylab("Group (monitored >= 6 years)")
# # By group and by fragment
# sub_frag = sub %>% 
#   dplyr::group_by(UMMPs) %>% 
#   dplyr::arrange(UMMPs, Monit_1stYear_grp) %>% 
#   group_split() # Split the dataset by fragments
# g = ldply(sub_frag[c(1:2)], data.frame) 
# g <- g[order(g$Monit_1stYear_grp,g$Group),]
# g$Group <- factor(g$Group, levels=unique(g$Group))
# ggplot(g, aes(x = Monit_1stYear_grp, y = Group)) +
#   geom_segment(aes(xend = Monit_LastYear_grp, yend = Group), colour = "orange") +
#   geom_point(size = 2, colour="darkorange") +
#   geom_point(aes(x = Monit_LastYear_grp), size = 2, colour="darkorange") +
#   theme_bw() +
#   theme(legend.position = "none") +
#   facet_grid(UMMPs ~ ., scales = "free_y", space = "free_y", switch = "y") +
#   theme(strip.placement = "outside")
# g = sub %>% 
#   dplyr::filter(UMMPs == "Aldeia I" | UMMPs =="Aldeia II")
# g <- g[order(g$Monit_1stYear_grp,g$Group),]
# g$Group <- factor(g$Group, levels=unique(g$Group))
# ggplot(g, aes(x = Monit_1stYear_grp, y = Group)) +
#   geom_segment(aes(xend = Monit_LastYear_grp, yend = Group), colour = "orange") +
#   geom_point(size = 2, colour="darkorange") +
#   geom_point(aes(x = Monit_LastYear_grp), size = 2, colour="darkorange") +
#   theme_bw() +
#   theme(legend.position = "none") +
#   facet_grid(UMMPs ~ ., scales = "free_y", space = "free_y", switch = "y") +
#   theme(strip.placement = "outside")


### GROUP SIZE
# Compute annual group size and growth rate  
GS_year = sub %>% 
  dplyr::group_by(UMMPs,Group,GLT_Year1) %>% 
  dplyr::summarise(Year_grp_size = n_distinct(GLT)) %>% 
  dplyr::mutate(Year_growth_rate = Year_grp_size/lag(Year_grp_size)) %>% 
  ungroup() %>% 
  as.data.frame()
GS_year = GS_year %>% 
  dplyr::left_join(dplyr::select(data_clean_v2,c("UMMPs","Group","GLT_Year1",
                                                 "id_frag","Shape","Perimetre","Size","Size_km2","Size_cat",
                                                 "cum_mean_cell_rainfall",
                                                 "weight"))) %>% 
  dplyr::distinct(UMMPs,Group,GLT_Year1, .keep_all = TRUE) # Join habitat parameters and keep the distinct groups/year
# ggplot(GS_year, aes(x=Year_grp_size))+
#   geom_histogram(aes(y = ..density..), fill='lightgray', col='darkgrey') +
#   geom_density(lwd = 1, colour = 4,
#                fill = 4, alpha = 0.25) +
#   xlab("Annual group size") + 
#   ylab("Density") +
#   theme(legend.title = element_blank())
# ggplot(GS_year, aes(x=Year_growth_rate))+
#   geom_histogram(aes(y = ..density..), fill='lightgray', col='darkgrey') +
#   geom_density(lwd = 1, colour = 4,
#                fill = 4, alpha = 0.25) +
#   xlab("Annual growth rate") + 
#   ylab("Density") +
#   theme(legend.title = element_blank())
# Seasonal group size and growth rate 
GS_season = sub %>%
  dplyr::group_by(UMMPs,Group,GLT_Year1,season) %>% 
  dplyr::arrange(Group,GLT_Year1,season) %>% 
  dplyr::summarise(Season_grp_size = n_distinct(GLT)) %>% 
  dplyr::mutate(Season_growth_rate = Season_grp_size/lag(Season_grp_size)) %>% 
  dplyr::ungroup()
# GS_season %>% 
#   dplyr::select(Season_grp_size, Season_growth_rate) %>% 
#   psych::describe(quant=c(.25,.75)) %>%
#   as_tibble(rownames="rowname")  %>%
#   print()
# ggplot(GS_season,aes(x = season, y = Season_grp_size)) +
#   geom_boxplot() +
#   xlab("Season") + 
#   ylab("Group size")
# # Plot group size/growth rate evolution
# plot = GS_year %>% 
#   dplyr::group_by(GLT_Year1) %>%
#   dplyr::summarise(mean = mean(Year_grp_size)) %>% 
#   ungroup() %>% 
#   as.data.frame()
# plot <- ts(plot$mean, start = 2001)
# plot.ts(plot, type= "b", main = "Mean group size over time", xlab = "Years", ylab = "Mean number of individuals")
# plot = GS_year %>% 
#   dplyr::filter(!is.na(Year_growth_rate)) %>% 
#   dplyr::group_by(GLT_Year1) %>%
#   dplyr::summarise(mean = mean(Year_growth_rate)) %>% 
#   ungroup() %>% 
#   as.data.frame()
# plot <- ts(plot$mean, start = 2002)
# plot.ts(plot, type= "b", main = "Mean group growth rate over time", xlab = "Years", ylab = "Mean growth rate")

# # Plot group size/grath rate evolution by fragment
# ggplot(GS_year,aes(x = UMMPs, y = Year_grp_size)) +
#   geom_boxplot(outlier.shape = NA) +
#   geom_jitter(colour = "black", size=0.25) +
#   xlab("UMMPs") + 
#   ylab("Group size") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# ggplot(GS_year,aes(x = UMMPs, y = Year_growth_rate)) +
#   geom_boxplot(outlier.shape = NA) +
#   geom_jitter(colour = "black", size=0.25) +
#   xlab("UMMPs") + 
#   ylab("Annual growth rate") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# ggplot(GS_year,aes(x = GLT_Year1, y = Year_grp_size)) + 
#   geom_point(aes(color=Group)) +
#   geom_line(data=GS_year[!is.na(GS_year$Year_grp_size),],aes(color=Group)) +
#   theme(legend.position = "none") +
#   facet_wrap(~UMMPs)
# plot = GS_year %>% 
#   dplyr::group_by(GLT_Year1,UMMPs) %>%
#   dplyr::summarise(mean = mean(Year_grp_size)) %>% 
#   ungroup() %>% 
#   as.data.frame()
# ggplot(plot,aes(x = GLT_Year1, y = mean)) + 
#   geom_line(data=plot[!is.na(plot$mean),]) +
#   geom_point() +
#   theme(legend.position = "none") +
#   facet_wrap(~UMMPs, ncol=3) +
#   xlab("Year") + 
#   ylab("Mean group size")
# plot = GS_year %>% 
#   dplyr::group_by(GLT_Year1,UMMPs) %>%
#   dplyr::summarise(mean = mean(Year_growth_rate)) %>% 
#   ungroup() %>% 
#   as.data.frame()
# ggplot(plot,aes(x = GLT_Year1, y = mean)) + 
#   geom_line(data=plot[!is.na(plot$mean),]) +
#   geom_point() +
#   theme(legend.position = "none") +
#   facet_wrap(~UMMPs, ncol=3) +
#   xlab("Year") + 
#   ylab("Mean growth rate")


# Habitat parameters
# Summarise by fragment
GS_frag_year = GS_year %>% 
  dplyr::group_by(GLT_Year1,id_frag) %>% 
  dplyr::mutate(n_glt = sum(Year_grp_size),
                mean_annual_grp_size = mean(Year_grp_size)) %>% 
  dplyr::mutate(n_grp = n_distinct(Group),
                density = n_grp/Size) %>% 
  ungroup()
GS_frag_year = GS_frag_year %>% 
  dplyr::distinct(id_frag, .keep_all = TRUE) %>% 
  dplyr::select(id_frag,UMMPs,GLT_Year1,Shape,Size,Size_km2,Size_cat,Perimetre,
                n_glt, mean_annual_grp_size,cum_mean_cell_rainfall,
                n_grp,density,weight)
# GS_frag_year %>% 
#   dplyr::select(Shape, Size, Size_km2, Perimetre) %>% 
#   psych::describe(quant=c(.25,.75)) %>%
#   as_tibble(rownames="rowname")  %>%
#   as.data.frame() %>% 
#   format(scientific=FALSE)
# GS_frag_year %>% 
#   dplyr::group_by(Size_cat) %>% 
#   dplyr::summarise(n=n_distinct(id_frag),prop=n*100/193)

# Save data subset
# save(GS_year, file="D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/GS_year.RData")
# save(GS_frag_year, file="D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/GS_frag_year.RData")



### STATISTICAL ANALYSIS 
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/GS_year.RData")
summary(GS_year)

# Outliers removal
outlier(GS_year$Size)
grubbs.test(GS_year$Size)
GS_year = GS_year %>% 
  dplyr::filter(Size<27099)

# Mutate new variables
GS_year = GS_year %>% 
  dplyr::group_by(UMMPs,Group) %>% 
  dplyr::mutate(Size_evol = Size_km2/lag(Size_km2)) %>% 
  dplyr::ungroup() # Fragment size growth rate
# Rescaling
GS_year = GS_year %>% 
  dplyr::mutate(Size_sc = scale(Size_km2) %>% as.vector) %>% 
  dplyr::mutate(Rainfall_sc = scale(cum_mean_cell_rainfall) %>% as.vector)

# Statistical summaries
# GS_year %>% 
#   dplyr::summarise(across(c("UMMPs","Group","id_frag"), ~ n_distinct(.x, na.rm = TRUE)))
# GS_year %>% 
#   dplyr::select(Year_grp_size, Year_growth_rate) %>% 
#   psych::describe(quant=c(.25,.75)) %>%
#   as_tibble(rownames="rowname")  %>%
#   print()
# GS_year %>% 
#   dplyr::group_by(GLT_Year1) %>% 
#   dplyr::summarise_at("Year_grp_size",list(min=min, max=max, mean=mean, sd=sd)) %>% 
#   dplyr::arrange(mean) %>% 
#   print(n=30)
# GS_year %>% 
#   dplyr::group_by(UMMPs) %>% 
#   dplyr::summarise_at("Year_grp_size",list(min=min, max=max, mean=mean, sd=sd)) %>% 
#   dplyr::arrange(mean) %>% 
#   print(n=30)
# GS_year %>% 
#   dplyr::filter(!is.na(Year_growth_rate)) %>% 
#   dplyr::group_by(GLT_Year1) %>% 
#   dplyr::summarise_at("Year_growth_rate",list(min=min, max=max, mean=mean, median=median, sd=sd)) %>% 
#   dplyr::arrange(mean) %>% 
#   print(n=30)
# GS_year %>% 
#   dplyr::filter(!is.na(Year_growth_rate)) %>% 
#   dplyr::group_by(UMMPs) %>% 
#   dplyr::summarise_at("Year_growth_rate",list(min=min, max=max, mean=mean, sd=sd)) %>% 
#   dplyr::arrange(mean) %>% 
#   print(n=30)
# Growth rate table
growth_rate = GS_year %>% 
  dplyr::select(Group,GLT_Year1,Year_growth_rate) %>% 
  dplyr::arrange(GLT_Year1) %>% 
  pivot_wider(names_from = GLT_Year1, values_from = Year_growth_rate)

# Correlation matrix
GS_year %>% 
  dplyr::select(Size_km2, Shape, Perimetre, cum_mean_cell_rainfall, GLT_Year1) %>% 
  cor()
par(mfrow=c(2,3))
plot(Year_grp_size~Size_km2+Shape+Perimetre+cum_mean_cell_rainfall+GLT_Year1, data=GS_year)

# GROUP SIZE (group level)
# # Diagnosis of best fitting distribution
# # See : https://ase.tufts.edu/bugs/guide/assets/mixed_model_guide.html
# # For alternative plotting: https://r.qcbs.ca/workshop07/book-fr/choisir-la-distribution-des-erreurs.html
# # Pick the distribution for which the largest number of observations falls between the dashed lines
# par(mfrow=c(3,2))
# car::qqp(GS_year$Year_grp_size, "norm") # QQplot normal distribution
# car::qqp(GS_year$Year_grp_size, "lnorm") # QQplot lognormal distribution
# nbinom <- fitdistr(GS_year$Year_grp_size, "Negative Binomial")
# car::qqp(GS_year$Year_grp_size, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]]) # QQplot negative binomial
# poisson <- fitdistr(GS_year$Year_grp_size, "Poisson")
# car::qqp(GS_year$Year_grp_size, "pois", lambda=poisson$estimate) # QQplot Poisson
# gamma <- fitdistr(GS_year$Year_grp_size, "gamma")
# car::qqp(GS_year$Year_grp_size, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]]) # Gamma distribution
# Model
glmm1 = lme4::glmer(Year_grp_size ~ Size_sc + Rainfall_sc + (1|GLT_Year1) + (1|UMMPs/Group), 
              family=poisson(link="log"), data=GS_year, weights=weight) # Groups are nested within UMMPs
summary(glmm1)
# Over-dispersion (deviance/df.resid)
23297.7/607
# Function for over-dispersion detection
overdisp_fun <- function(model) {
  ## number of variance parameters in an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m) * (nrow(m) + 1)/2
  }
  # The next two lines calculate the residual degrees of freedom
  model.df <- sum(sapply(VarCorr(model), vpars)) + length(fixef(model))
  rdf <- nrow(model.frame(model)) - model.df
  # extracts the Pearson residuals
  rp <- residuals(model, type = "pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df = rdf, lower.tail = FALSE)
  c(chisq = Pearson.chisq, ratio = prat, rdf = rdf, p = pval)
} # if p-value less than 0.05, the data are overdispersed.
overdisp_fun(glmm1)

# Model correction
# CF https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#glmmtmb
# Alternative distribution = negative binomial
glmm2.1 = glmmTMB::glmmTMB(Year_grp_size ~ Size_sc + Rainfall_sc + (1|GLT_Year1) + (1|UMMPs/Group),
                           family="nbinom2", data=GS_year, weights=weight,
                           control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))
summary(glmm2.1)
# Variable selection
glmm_up = update(glmm2.1,~.-Rainfall_sc)
summary(glmm_up)



# GROUP GROWTH RATE (group level)
GS_year2 = GS_year %>% 
  dplyr::filter(!is.na(Year_growth_rate)) %>% 
  dplyr::mutate(Evolution = ifelse(Year_growth_rate < 1, "Négative",
                                   ifelse(Year_growth_rate == 1, "Stable",
                                          ifelse(Year_growth_rate > 1, "Positive", NA)))) %>% 
  dplyr::mutate(logYGR = log(Year_growth_rate))
GS_year2 %>% 
  summary()
GS_year2 %>% 
  dplyr::summarise(across(c("UMMPs","Group","id_frag"), ~ n_distinct(.x, na.rm = TRUE)))
# # Distribution
# car::qqp(GS_year2$Year_growth_rate, "norm") # QQplot normal distribution
# car::qqp(GS_year2$Year_growth_rate, "lnorm") # QQplot lognormal distribution
# nbinom <- fitdistr(GS_year2$Year_growth_rate, "Negative Binomial")
# car::qqp(GS_year2$Year_growth_rate, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]]) # QQplot negative binomial
# poisson <- fitdistr(GS_year2$Year_growth_rate, "Poisson")
# car::qqp(GS_year2$Year_growth_rate, "pois", lambda=poisson$estimate) # QQplot Poisson
# gamma <- fitdistr(GS_year2$Year_growth_rate, "gamma")
# car::qqp(GS_year2$Year_growth_rate, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]]) # Gamma distribution

# LMM
lmm1 = lmerTest::lmer(logYGR ~ Size_sc + Rainfall_sc + (1|GLT_Year1) + (1|UMMPs/Group),
                      data = GS_year2, weights=weight, REML=FALSE)
summary(lmm1)
# Variable selection
lmm_up = update(lmm1,~.-Rainfall_sc)
summary(lmm_up)
# Comparison between large and small fragments
my_sum <- GS_year2 %>%
  dplyr::group_by(Size_cat) %>%
  dplyr::summarise( 
    n=n(),
    mean=mean(Year_growth_rate),
    sd=sd(Year_growth_rate),
    min=min(Year_growth_rate),
    max=max(Year_growth_rate)
  ) %>%
  dplyr::mutate( se=sd/sqrt(n))  %>%
  dplyr::mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
# ggplot(my_sum) +
#   geom_bar( aes(x=Size_cat, y=mean), stat="identity", fill="gold2", alpha=0.7) +
#   geom_errorbar( aes(x=Size_cat, ymin=mean-sd, ymax=mean+sd), width=0.4, colour="black", alpha=0.9, size=1) +
#   ylab("Taux de croissance annuel moyen") +
#   xlab("Catégorie de fragment") +
#   theme_light()
# Test
GS_year2 %>% 
  dplyr::select(Size_cat, Year_growth_rate) %>% 
  dplyr::group_by(Size_cat) %>%
  summarise_all(.funs = funs(statistic = shapiro.test(.)$statistic, 
                             p.value = shapiro.test(.)$p.value))
# Since p-value < 0.05, distribution differs from normal
wilcox.test(GS_year2$Year_growth_rate~GS_year2$Size_cat)


# # Scatter plot
# # CF : https://strengejacke.github.io/ggeffects/articles/introduction_randomeffects.html
# prediction = as.data.frame(ggpredict(lmm_up, terms="Size_sc [all]", type="fixed", back.transform = TRUE))
# prediction$Size= (prediction$x * sd(GS_year2$Size_km2)) + mean(GS_year2$Size_km2) # Unscale values
# prediction = prediction %>% 
#   dplyr::mutate_at(c("predicted","conf.low","conf.high"), funs(exp=exp(.)))
# # Plot (log)
# ggplot(data=prediction, aes(x = Size, y = predicted)) +
#   geom_line() +
#   geom_line(data = prediction, aes(x = Size, y = conf.low), linetype = "dashed", color = "black") +
#   geom_line(data = prediction, aes(x = Size, y = conf.high), linetype = "dashed", color = "black") +
#   geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2) +
#   geom_jitter(aes(x = Size_km2, y = logYGR), data=GS_year2) +
#   theme_minimal() +
#   xlab("Taille du fragment") +
#   ylab("Log(Taux de croissance annuel)")
# # Plot
# ggplot(data=prediction, aes(x = Size, y = predicted_exp)) +
#   geom_line() +
#   geom_line(data = prediction, aes(x = Size, y = conf.low_exp), linetype = "dashed", color = "black") +
#   geom_line(data = prediction, aes(x = Size, y = conf.high_exp), linetype = "dashed", color = "black") +
#   geom_ribbon(aes(ymin=conf.low_exp, ymax=conf.high_exp), alpha=0.2) +
#   geom_jitter(aes(x = Size_km2, y = Year_growth_rate, color = Evolution), data=GS_year2) +
#   scale_color_manual(values = c("brown1", "blue", "black")) + 
#   theme_minimal() +
#   xlab("Taille du fragment (en km²)") +
#   ylab("Taux de croissance annuel (λ)")
# # Other plots
# my_sum <- GS_year2 %>%
#   dplyr::group_by(GLT_Year1,Size_cat) %>%
#   dplyr::summarise( 
#     n=n(),
#     mean=mean(Year_growth_rate),
#     sd=sd(Year_growth_rate),
#     min=min(Year_growth_rate),
#     max=max(Year_growth_rate)
#   ) %>%
#   dplyr::mutate(Evolution = ifelse(mean < 1, "Négative",
#                                    ifelse(mean == 1, "Stable",
#                                           ifelse(mean > 1, "Positive", NA)))) %>% 
#   ungroup() %>% 
#   as.data.frame()
# ggplot(my_sum,aes(x = GLT_Year1, y = mean)) + 
#   geom_ribbon(aes(ymin=pmin(mean,1), ymax=1), fill="red", col="indianred1", alpha=0.5) +
#   geom_ribbon(aes(ymin=1, ymax=pmax(mean,1)), fill="green", col="palegreen", alpha=0.5) +
#   geom_line(aes(y=1), linetype="dashed") +
#   geom_point(size=2) +
#   theme(legend.position = "none") +
#   facet_wrap(~Size_cat, ncol=1) +
#   xlab("Année") + 
#   ylab("Taux de croissance annuel moyen")
# ggplot(my_sum, aes(x = GLT_Year1, y = mean)) +
#   annotate(geom = "rect",
#            xmin = 2002, xmax = 2020, ymin = -Inf, ymax = 1, fill="indianred1", alpha = 0.4) +
#   annotate(geom = "rect",
#            xmin = 2002, xmax = 2020, ymin = 1, ymax = +Inf, fill="palegreen", alpha = 0.4) +
#   geom_line(aes(x= GLT_Year1, y=1),linetype="dashed") +
#   geom_ribbon(aes(ymin = mean - sd,
#                   ymax = mean + sd),
#               color = "grey", fill="grey", alpha=0.3) + 
#   geom_line() +
#   geom_point(aes(color=Evolution), size=3) +
#   scale_color_manual(values=c("indianred3", "palegreen3", "black")) +
#   scale_x_continuous(n.breaks=10) +
#   theme(legend.position = "none") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90)) +
#   facet_wrap(~Size_cat, ncol=2) +
#   xlab("Année") + 
#   ylab("Taux de croissance annuel moyen")
# ggplot(my_sum) +
#   geom_bar( aes(x=GLT_Year1, y=mean), stat="identity", fill="gold2", alpha=0.7) +
#   geom_errorbar( aes(x=GLT_Year1, ymin=mean-sd, ymax=mean+sd), width=0.4, colour="black", alpha=0.9, size=1) +
#   geom_line(aes(x=GLT_Year1, y=1), linetype="dashed") +
#   facet_wrap(~Size_cat, ncol=1) +
#   ylab("Taux de croissance annuel moyen") +
#   xlab("Année") +
#   theme_light()
# plot = GS_year2 %>% 
#   dplyr::group_by(UMMPs,GLT_Year1) %>%
#   dplyr::summarise(mean = mean(Year_growth_rate)) %>% 
#   dplyr::mutate(Evolution = ifelse(mean < 1, "Négative",
#                                    ifelse(mean == 1, "Stable",
#                                           ifelse(mean > 1, "Positive", NA)))) %>% 
#   ungroup() %>% 
#   as.data.frame()
# ggplot(plot,aes(x = GLT_Year1, y = mean)) + 
#   geom_ribbon(aes(ymin=pmin(mean,1), ymax=1), fill="red", col="indianred1", alpha=0.5) +
#   geom_ribbon(aes(ymin=1, ymax=pmax(mean,1)), fill="green", col="palegreen", alpha=0.5) +
#   geom_point(size=1) +
#   theme(legend.position = "none") +
#   theme_light() +
#   facet_wrap(~UMMPs, ncol=3) +
#   xlab("Année") + 
#   ylab("Taux de croissance annuel moyen")
# ggplot(plot) + 
#   annotate(geom = "rect",
#            xmin = 2002, xmax = 2020, ymin = -Inf, ymax = 1, fill="indianred1", alpha = 0.4) +
#   annotate(geom = "rect",
#            xmin = 2002, xmax = 2020, ymin = 1, ymax = +Inf, fill="palegreen", alpha = 0.4) +
#   geom_line(aes(x = GLT_Year1, y = mean)) +
#   geom_point(aes(x = GLT_Year1, y = mean), size=1) +
#   theme(legend.position = "none") +
#   theme_light() +
#   facet_wrap(~UMMPs, ncol=3) +
#   xlab("Année") + 
#   ylab("Taux de croissance annuel moyen")




## GROUP DENSITY
# Data subset
GD_frag_year = GS_frag_year %>% 
  dplyr::group_by(id_frag) %>% 
  dplyr::mutate(Grp_density_ha = weight/Size,
                Grp_density_km2 = weight/Size_km2) %>% # Compute group density
  dplyr::ungroup()
GD_frag_year = GD_frag_year %>% 
  dplyr::mutate(Size_sc = scale(Size_km2) %>% as.vector) %>% 
  dplyr::mutate(Rainfall_sc = scale(cum_mean_cell_rainfall) %>% as.vector) %>% 
  dplyr::mutate(logGD = log(Grp_density_km2))

# Outliers removal
outlier(GD_frag_year$Size)
grubbs.test(GD_frag_year$Size)
GD_frag_year = GD_frag_year %>% 
  dplyr::filter(Size<27099)

# # Summary
# GD_frag_year %>% 
#   summary()
# GD_frag_year %>% 
#   dplyr::select(weight, Grp_density_km2) %>% 
#   psych::describe(quant=c(.25,.75)) %>%
#   as_tibble(rownames="rowname")  %>%
#   print()
# # Number of groups
# sub = GD_frag_year %>% 
#   dplyr::distinct(id_frag) %>% 
#   dplyr::pull()
# data_clean_v2 %>% 
#   subset(id_frag %in% sub) %>% 
#   dplyr::summarise(across(c("UMMPs","Group","id_frag"), ~ n_distinct(.x, na.rm = TRUE)))

# # Distribution
# car::qqp(GD_frag_year$Grp_density_km2, "norm") # QQplot normal distribution
# car::qqp(GD_frag_year$Grp_density_km2, "lnorm") # QQplot lognormal distribution
# nbinom <- fitdistr(GD_frag_year$Grp_density_km2, "Negative Binomial")
# car::qqp(GD_frag_year$Grp_density_km2, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]]) # QQplot negative binomial
# poisson <- fitdistr(GD_frag_year$Grp_density_km2, "Poisson")
# car::qqp(GD_frag_year$Grp_density_km2, "pois", lambda=poisson$estimate) # QQplot Poisson
# gamma <- fitdistr(GD_frag_year$Grp_density_km2, "gamma")
# car::qqp(GD_frag_year$Grp_density_km2, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]]) # Gamma distribution
# LMM
lmm1 = lmerTest::lmer(logGD ~ Size_sc + Rainfall_sc + (1|GLT_Year1) + (1|UMMPs),
                      data = GD_frag_year, weights=weight, REML=FALSE)
summary(lmm1)
# Update model
lmm_up = update(lmm1,~.-Rainfall_sc)
summary(lmm_up)
# Comparison between large and small fragments
GD_frag_year %>% 
  dplyr::select(Size_cat, weight) %>% 
  dplyr::group_by(Size_cat) %>%
  summarise_all(.funs = funs(statistic = shapiro.test(.)$statistic, 
                             p.value = shapiro.test(.)$p.value))
# Alternative test
wilcox.test(GD_frag_year$weight~GD_frag_year$Size_cat)



# # Scatter plot
# # CF : https://strengejacke.github.io/ggeffects/articles/introduction_randomeffects.html
# prediction = as.data.frame(ggpredict(lmm_up, terms="Size_sc [all]", type="fixed", back.transform = TRUE))
# prediction$Size= (prediction$x * sd(GD_frag_year$Size_km2)) + mean(GD_frag_year$Size_km2) # Unscale values
# prediction = prediction %>% 
#   dplyr::mutate_at(c("predicted","conf.low","conf.high"), funs(exp=exp(.)))
# # Plot (log)
# ggplot(data=prediction, aes(x = Size, y = predicted)) +
#   geom_line() +
#   geom_line(data = prediction, aes(x = Size, y = conf.low), linetype = "dashed", color = "black") +
#   geom_line(data = prediction, aes(x = Size, y = conf.high), linetype = "dashed", color = "black") +
#   geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2) +
#   geom_jitter(aes(x = Size_km2, y = logGD), data=GD_frag_year) +
#   theme_minimal() +
#   xlab("Taille du fragment (en km²)") +
#   ylab("Log(Densité de groupes par km²)")
# # Plot
# ggplot(data=prediction, aes(x = Size, y = predicted_exp)) +
#   geom_line() +
#   geom_line(data = prediction, aes(x = Size, y = conf.low_exp), linetype = "dashed", color = "black") +
#   geom_line(data = prediction, aes(x = Size, y = conf.high_exp), linetype = "dashed", color = "black") +
#   geom_ribbon(aes(ymin=conf.low_exp, ymax=conf.high_exp), alpha=0.2) +
#   geom_jitter(aes(x = Size_km2, y = Grp_density_km2), data=GD_frag_year) +
#   theme_minimal() +
#   xlab("Taille du fragment (en km²)") +
#   ylab("Densité (en groupes/km²)")
# # Other plots
# ggplot(GD_frag_year, aes(x=GLT_Year1, y=Grp_density_km2)) +
#   geom_jitter(size=1.6) +
#   facet_wrap(~Size_cat, ncol=1) +
#   ylab("Densité (en groupes/km²)") +
#   xlab("Année") +
#   theme_light()
# my_sum <- GD_frag_year %>%
#   dplyr::group_by(GLT_Year1,Size_cat) %>%
#   dplyr::summarise( 
#     n=n(),
#     mean=mean(Grp_density_km2),
#     sd=sd(Grp_density_km2),
#     min=min(Grp_density_km2),
#     max=max(Grp_density_km2)
#   ) %>% 
#   dplyr::mutate(ic_min=mean-sd, 
#                 ic_max=mean+sd) %>% 
#   dplyr::mutate(ic_min = ifelse(ic_min < 0, 0, ic_min))
# ggplot(my_sum, aes(x = GLT_Year1, y = mean)) + 
#   geom_ribbon(aes(ymin = ic_min,
#                   ymax = ic_max),
#               color = "grey", fill="grey", alpha=0.3) + 
#   geom_line() +
#   geom_point(size=3) +
#   scale_x_continuous(n.breaks=10) +
#   theme(legend.position = "none") +
#   theme_bw() +
#   theme(axis.text.x = element_text(angle = 90)) +
#   facet_wrap(~Size_cat, ncol=2) +
#   xlab("Année") + 
#   ylab("Densité moyenne (en groupes/km²)")
# ggplot(my_sum) +
#   geom_bar( aes(x=GLT_Year1, y=mean), stat="identity", fill="gold2", alpha=0.7) +
#   geom_errorbar( aes(x=GLT_Year1, ymin=mean-sd, ymax=mean+sd), width=0.4, colour="black", alpha=0.9, size=1) +
#   facet_wrap(~Size_cat, ncol=1) +
#   ylab("Densité moyenne (en groupes/km²)") +
#   xlab("Année") +
#   theme_bw()
# # Plot group density/grp number
# ggplot(aes(x=weight, y=logGD, fill=Size_cat), data=GD_frag_year) +
#   geom_encircle(s_shape=1.1, alpha=0.2, size=2) +
#   geom_jitter(size=3, shape=21, color="white") +
#   scale_fill_manual(name = "Taille du fragment",
#                      values=c('orange','darkolivegreen3'))+
#   theme_bw() +
#   xlab("Nombre de groupes") +
#   ylab("Log de densité (en groupes/km²)")+
#   xlim(-1, 22)



### Change point analysis
# CF https://kevin-kotze.gitlab.io/tsm/ts-2-tut/
# Overall dataset
GS_mean <- GS_year %>%
  dplyr::group_by(GLT_Year1) %>%
  dplyr::summarise(mean_size = mean(Year_grp_size)) %>% 
  ungroup()
m_pelt = GS_mean %>% 
  pull(mean_size) %>%
  cpt.mean(., penalty = "SIC", method = "PELT") # PELT algorithm for a change point test
plot(m_pelt, type = "l", cpt.col = "blue", xlab = "Index", cpt.width = 4)
GS_mean %>%
  slice(cpts(m_pelt)) # find the date for the change point
# Change point by fragment
cp_frag <-NULL
GS_frag = GS_year %>% 
  dplyr::group_by(UMMPs,GLT_Year1) %>%
  dplyr::summarise(mean_size = mean(Year_grp_size)) %>% 
  ungroup()
GS_frag = GS_frag %>% 
  group_split(UMMPs)
m_pelt = GS_frag %>% 
  map(~ dplyr::select(., mean_size)) %>% 
  map(~ pull(., mean_size)) %>% 
  map(~ cpt.mean(., penalty="SIC", method="PELT"))
values = map(m_pelt,"cpts")
for (i in 1:length(values)) {
  temp<-values[[i]]
  cp_frag<-rbind(cp_frag,GS_frag[[i]][temp,])
}
plot(m_pelt[[9]], type = "l", cpt.col = "blue", xlab = "Index", cpt.width = 4)


