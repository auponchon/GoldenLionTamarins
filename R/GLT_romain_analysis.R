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
             "ggeffects", "lmerTest", "car")
sapply(load.lib,require,character=TRUE)


#### DATA

### GLT dataset
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/data_clean_v2.RData")

## Create variables
# Create "GLT Year"
data_clean_v2 = data_clean_v2 %>% 
  dplyr::group_by(rowid) %>% 
  dplyr::mutate(Ref_GLT_Year = paste("01","07",Year, sep="/")) %>% 
  dplyr::ungroup()
data_clean_v2$Ref_GLT_Year = as.Date(data_clean_v2$Ref_GLT_Year, format="%d/%m/%Y")
data_clean_v2 = data_clean_v2 %>% 
  dplyr::mutate(GLT_Year1 = ifelse(DateObs >= Ref_GLT_Year, Year, Year-1)) %>% 
  dplyr::mutate(GLT_Year2 = ifelse(DateObs >= Ref_GLT_Year, paste(Year, Year+1, sep="-"), paste(Year-1, Year, sep="-")))


### Monitored groups
Nb_obs_gp = read_delim("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Nb_obs_gp.csv", delim=";", show_col_types = FALSE)
Nb_obs_gp %>%
  dplyr::summarise(across(c("UMMPs","Farm","Fragment.Region"), ~ n_distinct(.x, na.rm = TRUE)))
Nb_obs_gp %>%
  dplyr::group_by(UMMPs,Fragment.Region) %>%
  dplyr::summarise(n_grp = n_distinct(Group)) %>%
  print(n=50)
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
cumul_rainfall %>% 
  dplyr::arrange(cum_mean_cell_rainfall) %>% 
  print(n=30)
cumul_rainfall %>% 
  dplyr::summarise_at("cum_mean_cell_rainfall",list(min=min, max=max, mean=mean, sd=sd))
ggplot(cumul_rainfall, aes(x=cum_mean_cell_rainfall))+
  geom_histogram(aes(y = after_stat(density)), fill='lightgray', col='darkgrey') +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25) +
  xlab("Mean cumulative rainfall") + 
  ylab("Density") +
  theme(legend.title = element_blank())
plot <- ts(cumul_rainfall$cum_mean_cell_rainfall, start = 2000)
plot.ts(plot, type= "b", main = "Mean rainfall over time", xlab = "Years", ylab = "Mean rainfall (in mm)")



### Fragment parameters
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/TotalPerimetre.RData")
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/TotalShape.RData")
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/TotalSizeArea.RData")
plot = totsizearea %>% 
  dplyr::group_by(Year) %>%
  dplyr::summarise(mean = mean(Size)) %>% 
  ungroup() %>% 
  as.data.frame()
plot <- ts(plot$mean, start = 1990)
plot.ts(plot, type= "b", main = "Mean fragment size over time", xlab = "Years", ylab = "Mean fragment size")
ggplot(totsizearea,aes(x = UMMPs, y = Size)) +
  geom_boxplot() +
  xlab("UMMPs") + 
  ylab("Fragment size")


### SUBSET DATA FOR GROUP DYNAMICS ANALYSIS
# Create monitoring periods
sub = data_clean_v2 %>% 
  dplyr::filter(File == "Obs") %>%
  dplyr::group_by(Group) %>% 
  dplyr::filter(GLT_Year1 >= 2000 & GLT_Year1 <= 2020) %>%
  dplyr::filter(!is.na(UMMPs)) %>%
  dplyr::mutate(Monit_Years_grp = diff(range(GLT_Year1))+1) %>% 
  dplyr::mutate(Monit_1stYear_grp = min(GLT_Year1), 
                Monit_LastYear_grp = max(GLT_Year1)) %>%
  dplyr::mutate(Monit_Period_grp = paste0(unique(c(Monit_1stYear_grp, Monit_LastYear_grp)), collapse = '-')) %>%
  ungroup() %>% 
  as.data.frame() # By group
sub = sub %>% 
  dplyr::group_by(UMMPs) %>% 
  dplyr::mutate(Monit_Years_frag = diff(range(GLT_Year1))+1) %>% 
  dplyr::mutate(Monit_1stYear_frag = min(GLT_Year1), 
                Monit_LastYear_frag = max(GLT_Year1)) %>%
  dplyr::mutate(Monit_Period_frag = paste0(unique(c(Monit_1stYear_frag, Monit_LastYear_frag)), collapse = '-')) %>%
  ungroup() %>% 
  as.data.frame() # By UMMPs
# Subset data with years of monitoring
sub = sub %>% 
  dplyr::group_by(Group, Monit_Years_grp) %>% 
  dplyr::filter(Monit_Years_grp >= 6) %>% 
  dplyr::filter(!is.na(Group)) %>%
  ungroup() %>% 
  as.data.frame()


### STATISTICAL SUMMARY

## Group sample
sub %>% 
  dplyr::select(GLT_Year1, Monit_Years_grp) %>% 
  summary()
sub %>% 
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="T0" & GLT!="FT") %>% 
  dplyr::summarise(n=n(),
                   n_mu = n_distinct(UMMPs),
                   n_grp = n_distinct(Group), 
                   tot_GLT = n_distinct(GLT))
visites_grp = sub %>% 
  dplyr::group_by(Group,Monit_Years_grp) %>% 
  dplyr::summarise(n_visites=n_distinct(DateObs),
                   mean_obs_year = n_visites/Monit_Years_grp) %>% 
  distinct(Group,n_visites,mean_obs_year) %>% 
  ungroup()
visites_grp %>% 
  dplyr::select(n_visites, mean_obs_year) %>%
  psych::describe(quant=c(.25,.75)) %>%
  as_tibble(rownames="rowname")  %>%
  print()

# Distribution plot
ggplot(visites_grp, aes(x=Monit_Years_grp))+
  geom_histogram(aes(y = ..density..), fill='lightgray', col='darkgrey') +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25) +
  xlab("Monitoring period (in years)") + 
  ylab("Density") +
  theme(legend.title = element_blank())
ggplot(visites_grp, aes(x=n_visites))+
  geom_histogram(aes(y = ..density..), fill='lightgray', col='darkgrey') +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25) +
  xlab("Frequency of visits") + 
  ylab("Density") +
  theme(legend.title = element_blank())
ggplot(visites_grp, aes(x=mean_obs_year))+
  geom_histogram(aes(y = ..density..), fill='lightgray', col='darkgrey') +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25) +
  xlab("Mean number of annual visits") + 
  ylab("Density") +
  theme(legend.title = element_blank())

# By period
sub %>% 
  dplyr::group_by(Monit_Years_grp) %>% 
  dplyr::summarise(n_frag=n_distinct(UMMPs),
                   n_grp=n_distinct(Group),
                   n_GLT=n_distinct(GLT)) %>% 
  print(n=100)
# Groups within fragments monitored the same amount of time
sub %>% 
  dplyr::filter(!is.na(UMMPs)) %>% 
  dplyr::group_by(Group) %>% 
  dplyr::summarise(n_grp = n_distinct(Group), 
                   n_grp_1stDate = n_distinct(Group[Monit_1stYear_grp < 2002 ]),
                   n_grp_LastDate = n_distinct(Group[Monit_LastYear_grp > 2018 ]))

# Plot monitoring periods by fragment
g <- sub[order(sub$Monit_1stYear_frag,sub$UMMPs),]
g$UMMPs <- factor(g$UMMPs, levels=unique(g$UMMPs))
ggplot(g, aes(x = Monit_1stYear_frag, y = UMMPs)) +
  geom_segment(aes(xend = Monit_LastYear_frag, yend = UMMPs), colour = "orange") +
  geom_point(size = 2, colour="darkorange") +
  geom_point(aes(x = Monit_LastYear_frag), size = 2, colour="darkorange") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=10))
# Plot monitoring periods by fragment
g = sub %>% 
  dplyr::group_by(Group) %>% 
  dplyr::filter(Monit_Years_grp >= 6)
g <- g[order(g$Monit_1stYear_grp,g$Group),]
g$Group <- factor(g$Group, levels=unique(g$Group))
ggplot(g, aes(x = Monit_1stYear_grp, y = Group)) +
  geom_segment(aes(xend = Monit_LastYear_grp, yend = Group), colour = "orange") +
  geom_point(size = 2, colour="darkorange") +
  geom_point(aes(x = Monit_LastYear_grp), size = 2, colour="darkorange") +
  theme_bw() +
  theme(legend.position = "none")
# By group and by fragment
sub_frag = sub %>% 
  dplyr::group_by(UMMPs) %>% 
  dplyr::arrange(UMMPs, Monit_1stYear_grp) %>% 
  group_split() # Split the dataset by fragments
g = ldply(sub_frag[c(1:2)], data.frame) 
g <- g[order(g$Monit_1stYear_grp,g$Group),]
g$Group <- factor(g$Group, levels=unique(g$Group))
ggplot(g, aes(x = Monit_1stYear_grp, y = Group)) +
  geom_segment(aes(xend = Monit_LastYear_grp, yend = Group), colour = "orange") +
  geom_point(size = 2, colour="darkorange") +
  geom_point(aes(x = Monit_LastYear_grp), size = 2, colour="darkorange") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_grid(UMMPs ~ ., scales = "free_y", space = "free_y", switch = "y") +
  theme(strip.placement = "outside")
g = sub %>% 
  dplyr::filter(UMMPs == "Aldeia I" | UMMPs =="Aldeia II")
g <- g[order(g$Monit_1stYear_grp,g$Group),]
g$Group <- factor(g$Group, levels=unique(g$Group))
ggplot(g, aes(x = Monit_1stYear_grp, y = Group)) +
  geom_segment(aes(xend = Monit_LastYear_grp, yend = Group), colour = "orange") +
  geom_point(size = 2, colour="darkorange") +
  geom_point(aes(x = Monit_LastYear_grp), size = 2, colour="darkorange") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_grid(UMMPs ~ ., scales = "free_y", space = "free_y", switch = "y") +
  theme(strip.placement = "outside")


### GROUP SIZE
# Compute annual group size and growth rate  
GS_year = sub %>% 
  dplyr::group_by(UMMPs,Group,GLT_Year1) %>% 
  dplyr::summarise(Year_grp_size = n_distinct(GLT)) %>% 
  dplyr::mutate(Year_growth_rate = Year_grp_size/lag(Year_grp_size)) %>% 
  ungroup() %>% 
  as.data.frame()
GS_year %>% 
  dplyr::select(Year_grp_size, Year_growth_rate) %>% 
  psych::describe(quant=c(.25,.75)) %>%
  as_tibble(rownames="rowname")  %>%
  print()
ggplot(GS_year, aes(x=Year_grp_size))+
  geom_histogram(aes(y = ..density..), fill='lightgray', col='darkgrey') +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25) +
  xlab("Annual group size") + 
  ylab("Density") +
  theme(legend.title = element_blank())
ggplot(GS_year, aes(x=Year_growth_rate))+
  geom_histogram(aes(y = ..density..), fill='lightgray', col='darkgrey') +
  geom_density(lwd = 1, colour = 4,
               fill = 4, alpha = 0.25) +
  xlab("Annual growth rate") + 
  ylab("Density") +
  theme(legend.title = element_blank())

## Join habitat parameters
GS_year = GS_year %>% 
  dplyr::rename(Year=GLT_Year1) %>% 
  dplyr::left_join(dplyr::select(totperim,c(Group,Year,Perimetre))) %>% 
  dplyr::left_join(dplyr::select(totshape,c(Group,Year,Shape))) %>% 
  dplyr::left_join(dplyr::select(totsizearea,c(Group,Year,Size))) %>%
  dplyr::rename(GLT_Year1=Year) %>% 
  dplyr::distinct(Group,GLT_Year1,Year_grp_size,Perimetre,Shape,Size, .keep_all =TRUE)
GS_year = GS_year %>% 
  dplyr::left_join(dplyr::select(cumul_rainfall,c(GLT_Year1,cum_mean_cell_rainfall)))
GS_year = GS_year %>% 
  dplyr::distinct(GLT_Year1, Group, .keep_all = TRUE)
GS_year = GS_year%>%                                        
  dplyr::group_by(GLT_Year1,Shape,Size) %>%
  dplyr::mutate(id_frag = cur_group_id()) %>% 
  ungroup() %>% 
  as.data.frame() # Create ID by group
# Seasonal group size and growth rate 
GS_season = sub %>%
  dplyr::group_by(UMMPs,Group,GLT_Year1,season) %>% 
  dplyr::arrange(Group,GLT_Year1,season) %>% 
  dplyr::summarise(Season_grp_size = n_distinct(GLT)) %>% 
  dplyr::mutate(Season_growth_rate = Season_grp_size/lag(Season_grp_size)) %>% 
  dplyr::ungroup()
GS_season %>% 
  dplyr::select(Season_grp_size, Season_growth_rate) %>% 
  psych::describe(quant=c(.25,.75)) %>%
  as_tibble(rownames="rowname")  %>%
  print()
ggplot(GS_season,aes(x = season, y = Season_grp_size)) +
  geom_boxplot() +
  xlab("Season") + 
  ylab("Group size")

# Statistical summaries
GS_year %>% 
  dplyr::group_by(GLT_Year1) %>% 
  dplyr::summarise_at("Year_grp_size",list(min=min, max=max, mean=mean, sd=sd)) %>% 
  dplyr::arrange(mean) %>% 
  print(n=30)
GS_year %>% 
  dplyr::group_by(UMMPs) %>% 
  dplyr::summarise_at("Year_grp_size",list(min=min, max=max, mean=mean, sd=sd)) %>% 
  dplyr::arrange(mean) %>% 
  print(n=30)
GS_season %>% 
  dplyr::group_by(season) %>% 
  dplyr::summarise_at("Season_grp_size",list(min=min, max=max, mean=mean, sd=sd)) %>% 
  dplyr::arrange(mean) %>% 
  print(n=30)
GS_year %>% 
  dplyr::filter(!is.na(Year_growth_rate)) %>% 
  dplyr::group_by(GLT_Year1) %>% 
  dplyr::summarise_at("Year_growth_rate",list(min=min, max=max, mean=mean, sd=sd)) %>% 
  dplyr::arrange(mean) %>% 
  print(n=30)
GS_year %>% 
  dplyr::filter(!is.na(Year_growth_rate)) %>% 
  dplyr::group_by(UMMPs) %>% 
  dplyr::summarise_at("Year_growth_rate",list(min=min, max=max, mean=mean, sd=sd)) %>% 
  dplyr::arrange(mean) %>% 
  print(n=30)

# Growth rate table
growth_rate = GS_year %>% 
  dplyr::select(Group,GLT_Year1,Year_growth_rate) %>% 
  dplyr::arrange(GLT_Year1) %>% 
  pivot_wider(names_from = GLT_Year1, values_from = Year_growth_rate)

# Plot group size/growth rate evolution
plot = GS_year %>% 
  dplyr::group_by(GLT_Year1) %>%
  dplyr::summarise(mean = mean(Year_grp_size)) %>% 
  ungroup() %>% 
  as.data.frame()
plot <- ts(plot$mean, start = 2001)
plot.ts(plot, type= "b", main = "Mean group size over time", xlab = "Years", ylab = "Mean number of individuals")
plot = GS_year %>% 
  dplyr::filter(!is.na(Year_growth_rate)) %>% 
  dplyr::group_by(GLT_Year1) %>%
  dplyr::summarise(mean = mean(Year_growth_rate)) %>% 
  ungroup() %>% 
  as.data.frame()
plot <- ts(plot$mean, start = 2002)
plot.ts(plot, type= "b", main = "Mean group growth rate over time", xlab = "Years", ylab = "Mean growth rate")

# Plot group size/grath rate evolution by fragment
ggplot(GS_year,aes(x = UMMPs, y = Year_grp_size)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(colour = "black", size=0.25) +
  xlab("UMMPs") + 
  ylab("Group size") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(GS_year,aes(x = UMMPs, y = Year_growth_rate)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(colour = "black", size=0.25) +
  xlab("UMMPs") + 
  ylab("Annual growth rate") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggplot(GS_year,aes(x = GLT_Year1, y = Year_grp_size)) + 
  geom_point(aes(color=Group)) +
  geom_line(data=GS_year[!is.na(GS_year$Year_grp_size),],aes(color=Group)) +
  theme(legend.position = "none") +
  facet_wrap(~UMMPs)
plot = GS_year %>% 
  dplyr::group_by(GLT_Year1,UMMPs) %>%
  dplyr::summarise(mean = mean(Year_grp_size)) %>% 
  ungroup() %>% 
  as.data.frame()
ggplot(plot,aes(x = GLT_Year1, y = mean)) + 
  geom_line(data=plot[!is.na(plot$mean),]) +
  geom_point() +
  theme(legend.position = "none") +
  facet_wrap(~UMMPs, ncol=3) +
  xlab("Year") + 
  ylab("Mean group size")
plot = GS_year %>% 
  dplyr::group_by(GLT_Year1,UMMPs) %>%
  dplyr::summarise(mean = mean(Year_growth_rate)) %>% 
  ungroup() %>% 
  as.data.frame()
ggplot(plot,aes(x = GLT_Year1, y = mean)) + 
  geom_line(data=plot[!is.na(plot$mean),]) +
  geom_point() +
  theme(legend.position = "none") +
  facet_wrap(~UMMPs, ncol=3) +
  xlab("Year") + 
  ylab("Mean growth rate")

# Habitat parameters
# Summarise by fragment
Stat_frag_year = GS_year %>% 
  dplyr::group_by(GLT_Year1,id_frag) %>% 
  dplyr::mutate(mean_annual_grp_size = mean(Year_grp_size)) %>% 
  dplyr::mutate(n_grp = n_distinct(Group),
                density = n_grp/Size) %>% 
  ungroup() %>% 
  dplyr::distinct(id_frag, .keep_all = TRUE) %>% 
  dplyr::select(id_frag,UMMPs,GLT_Year1,Shape,Size,Perimetre,
                mean_annual_grp_size,cum_mean_cell_rainfall,
                n_grp,density)
Stat_frag_year %>% 
  dplyr::select(Shape, Size, Perimetre) %>% 
  psych::describe(quant=c(.25,.75)) %>%
  as_tibble(rownames="rowname")  %>%
  as.data.frame() %>% 
  format(scientific=FALSE)

# Save data subset
# save(GS_year, file="D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/GS_year.RData")
# save(Stat_frag_year, file="D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Stat_frag_year.RData")

### STATISTICAL ANALYSIS 
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/GS_year.RData")
summary(GS_year)
GS_year %>% 
  dplyr::summarise(across(c("UMMPs","Group","id_frag"), ~ n_distinct(.x, na.rm = TRUE)))
GS_year = GS_year %>% 
  dplyr::group_by(UMMPs,Group) %>% 
  dplyr::mutate(Size_evol = Size/lag(Size)) %>% 
  dplyr::ungroup() # Fragment size growth rate
# Compute a weight to take the monitoring effort into account
GS_year = GS_year %>% 
  dplyr::group_by(id_frag, GLT_Year1) %>% 
  dplyr::mutate(weight = n_distinct(Group)) %>% 
  dplyr::ungroup()


# Correlation matrix
GS_year %>% 
  dplyr::select(Size, Shape, Perimetre, cum_mean_cell_rainfall, GLT_Year1) %>% 
  cor()
par(mfrow=c(2,3))
plot(Year_grp_size~Size+Shape+Perimetre+cum_mean_cell_rainfall+GLT_Year1, data=GS_year)
plot(mean_annual_grp_size~Size, data=Stat_frag_year)

# Rescaling
GS_year = GS_year %>% 
  dplyr::mutate(Size_sc = scale(Size) %>% as.vector) %>% 
  dplyr::mutate(Rainfall_sc = scale(cum_mean_cell_rainfall) %>% as.vector)

# GROUP SIZE (group level)
# Diagnosis of best fitting distribution
# See : https://ase.tufts.edu/bugs/guide/assets/mixed_model_guide.html
# For alternative plotting: https://r.qcbs.ca/workshop07/book-fr/choisir-la-distribution-des-erreurs.html
# Pick the distribution for which the largest number of observations falls between the dashed lines
par(mfrow=c(3,2))
car::qqp(GS_year$Year_grp_size, "norm") # QQplot normal distribution
car::qqp(GS_year$Year_grp_size, "lnorm") # QQplot lognormal distribution
nbinom <- fitdistr(GS_year$Year_grp_size, "Negative Binomial")
car::qqp(GS_year$Year_grp_size, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]]) # QQplot negative binomial
poisson <- fitdistr(GS_year$Year_grp_size, "Poisson")
car::qqp(GS_year$Year_grp_size, "pois", lambda=poisson$estimate) # QQplot Poisson
gamma <- fitdistr(GS_year$Year_grp_size, "gamma")
car::qqp(GS_year$Year_grp_size, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]]) # Gamma distribution
# Model
glmm1 = lme4::glmer(Year_grp_size ~ Size_sc + Rainfall_sc + (1|GLT_Year1) + (1|UMMPs/Group), 
              family=poisson(link="log"), data=GS_year, weights=weight) # Groups are nested within UMMPs
summary(glmm1)
# Over-dispersion (deviance/df.resid)
21639.3/611
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
# Alternative distribution = quasipoisson
# Using MASS::glmmPQL
glmm2.1 = MASS::glmmPQL(Year_grp_size~Size+cum_mean_cell_rainfall+GLT_Year1, random=list(Group = ~1, UMMPs = ~1),
                 family=quasipoisson(link="log"), data=GS_year, weights=weight)
summary(glmm2)
# Alternative distribution = negative binomial
glmm2.1 = glmmTMB::glmmTMB(Year_grp_size ~ Size_sc + Rainfall_sc + (1|GLT_Year1) + (1|UMMPs) + (1|Group),
                           family="nbinom2", data=GS_year, weights=weight)
summary(glmm2.1)
glmm2.2 = glmmTMB::glmmTMB(Year_grp_size ~ Size_sc + Rainfall_sc + (1|GLT_Year1) + (1|UMMPs/Group),
                           family="nbinom2", data=GS_year, weights=weight,
                           control=glmmTMBControl(optimizer=optim,optArgs=list(method="BFGS")))
summary(glmm2.2)

# Variable selection
glmm_up = update(glmm2.2,~.-Rainfall_sc)
summary(glmm_up)


# Scatter plot
# CF : https://strengejacke.github.io/ggeffects/articles/introduction_randomeffects.html
# With back transformation
predict = ggpredict(glmm_up, terms="Size_sc [all]", type="fixed", back.transform = TRUE)
predict$Size = (predict$x * sd(GS_year$Size)) + mean(GS_year$Size) # Unscale values
ggplot(data=predict, aes(x = Size, y = predicted)) +
  geom_line() +
  geom_line(data = predict, aes(x = Size, y = conf.low), linetype = "dashed", color = "black") +
  geom_line(data = predict, aes(x = Size, y = conf.high), linetype = "dashed", color = "black") +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2) +
  geom_jitter(aes(x = Size, y = Year_growth_rate), data=GS_year2) +
  theme_minimal()
# Without transformation
# predict = ggpredict(glmm_up, terms="Size [all]", type="fixed")
# plot(log(Year_grp_size)~Size, data=GS_year2, ylab="Taille annuelle du groupe", xlab="Taille du fragment")
# lines(predict$x, log(predict$predicted))
# Barplot
my_sum <- GS_year %>%
  dplyr::group_by(GLT_Year1) %>%
  dplyr::summarise( 
    n=n(),
    mean=mean(Year_grp_size),
    sd=sd(Year_grp_size),
    min=min(Year_grp_size),
    max=max(Year_grp_size)
  ) %>%
  dplyr::mutate( se=sd/sqrt(n))  %>%
  dplyr::mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
ggplot(my_sum) +
  geom_bar( aes(x=GLT_Year1, y=mean), stat="identity", fill="gold2", alpha=0.7) +
  geom_errorbar( aes(x=GLT_Year1, ymin=mean-sd, ymax=mean+sd), width=0.4, colour="black", alpha=0.9, size=1) +
  ylab("Nombre moyen d'individus par groupe") +
  xlab("Année") +
  theme_light()



# GROUP SIZE (fragment level)
Stat_frag_year = Stat_frag_year %>% 
  dplyr::mutate(Size_sc = scale(Size) %>% as.vector) %>% 
  dplyr::mutate(Rainfall_sc = scale(cum_mean_cell_rainfall) %>% as.vector)
# Distribution
car::qqp(Stat_frag_year$mean_annual_grp_size, "norm") # QQplot normal distribution
car::qqp(Stat_frag_year$mean_annual_grp_size, "lnorm") # QQplot lognormal distribution
nbinom <- fitdistr(Stat_frag_year$mean_annual_grp_size, "Negative Binomial")
car::qqp(Stat_frag_year$mean_annual_grp_size, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]]) # QQplot negative binomial
poisson <- fitdistr(Stat_frag_year$mean_annual_grp_size, "Poisson")
car::qqp(Stat_frag_year$mean_annual_grp_size, "pois", lambda=poisson$estimate) # QQplot Poisson
gamma <- fitdistr(Stat_frag_year$mean_annual_grp_size, "gamma")
car::qqp(Stat_frag_year$mean_annual_grp_size, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]]) # Gamma distribution
# Model
glmm1 = lme4::glmer(mean_annual_grp_size ~ Size_sc + Rainfall_sc + (1|GLT_Year1) + (1|UMMPs), 
                    family=Gamma, data=Stat_frag_year, weights=n_grp) # Groups are nested within UMMPs
summary(glmm1)
# Variable selection
glmm_up = update(glmm1,~.-Rainfall_sc)
summary(glmm_up)

# GROUP GROWTH RATE (group level)
GS_year2 = GS_year %>% 
  dplyr::filter(!is.na(Year_growth_rate)) %>% 
  dplyr::mutate(Evolution = ifelse(Year_growth_rate < 1, "Decroit",
                                   ifelse(Year_growth_rate == 1, "Stable",
                                          ifelse(Year_growth_rate > 1, "Croit", NA)))) %>% 
  dplyr::mutate(logYGR = log(Year_growth_rate))
GS_year2 %>% 
  summary()
GS_year2 %>% 
  dplyr::summarise(across(c("UMMPs","Group"), ~ n_distinct(.x, na.rm = TRUE)))
# Distribution
car::qqp(GS_year2$Year_growth_rate, "norm") # QQplot normal distribution
car::qqp(GS_year2$Year_growth_rate, "lnorm") # QQplot lognormal distribution
nbinom <- fitdistr(GS_year2$Year_growth_rate, "Negative Binomial")
car::qqp(GS_year2$Year_growth_rate, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]]) # QQplot negative binomial
poisson <- fitdistr(GS_year2$Year_growth_rate, "Poisson")
car::qqp(GS_year2$Year_growth_rate, "pois", lambda=poisson$estimate) # QQplot Poisson
gamma <- fitdistr(GS_year2$Year_growth_rate, "gamma")
car::qqp(GS_year2$Year_growth_rate, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]]) # Gamma distribution

# LMM
lmm1 = lmerTest::lmer(logYGR ~ Size_sc + Rainfall_sc + (1|GLT_Year1) + (1|Group) + (1|UMMPs),
                    data = GS_year2, weights=weight, REML=FALSE)
summary(lmm1)
lmm2 = lmerTest::lmer(logYGR ~ Size_sc + Rainfall_sc + (1|GLT_Year1) + (1|UMMPs/Group),
                      data = GS_year2, weights=weight, REML=FALSE)
summary(lmm2)
# Variable selection
lmm_up = update(lmm2,~.-Rainfall_sc)
summary(lmm_up)

# Scatter plot
# CF : https://strengejacke.github.io/ggeffects/articles/introduction_randomeffects.html
prediction = as.data.frame(ggpredict(lmm_up, terms="Size_sc [all]", type="fixed", back.transform = TRUE))
prediction$Size= (prediction$x * sd(GS_year2$Size)) + mean(GS_year2$Size) # Unscale values
prediction = prediction %>% 
  dplyr::mutate_at(c("predicted","conf.low","conf.high"), funs(exp=exp(.)))
# Plot (log)
ggplot(data=prediction, aes(x = Size, y = predicted)) +
  geom_line() +
  geom_line(data = prediction, aes(x = Size, y = conf.low), linetype = "dashed", color = "black") +
  geom_line(data = prediction, aes(x = Size, y = conf.high), linetype = "dashed", color = "black") +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2) +
  geom_jitter(aes(x = Size, y = logYGR), data=GS_year2) +
  theme_minimal() +
  xlab("Taille du fragment") +
  ylab("Log(Taux de croissance annuel)")
# Plot
ggplot(data=prediction, aes(x = Size, y = predicted_exp)) +
  geom_line() +
  geom_line(data = prediction, aes(x = Size, y = conf.low_exp), linetype = "dashed", color = "black") +
  geom_line(data = prediction, aes(x = Size, y = conf.high_exp), linetype = "dashed", color = "black") +
  geom_ribbon(aes(ymin=conf.low_exp, ymax=conf.high_exp), alpha=0.2) +
  geom_jitter(aes(x = Size, y = Year_growth_rate, color = Evolution), data=GS_year2) +
  scale_color_manual(values = c("brown1", "cornflowerblue", "lightpink2")) + 
  theme_minimal() +
  xlab("Taille du fragment") +
  ylab("Taux de croissance annuel")

# Barplot
my_sum <- GS_year2 %>%
  dplyr::group_by(GLT_Year1) %>%
  dplyr::summarise( 
    n=n(),
    mean=mean(Year_growth_rate),
    sd=sd(Year_growth_rate),
    min=min(Year_growth_rate),
    max=max(Year_growth_rate)
  ) %>%
  dplyr::mutate( se=sd/sqrt(n))  %>%
  dplyr::mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
ggplot(my_sum) +
  geom_bar( aes(x=GLT_Year1, y=mean), stat="identity", fill="gold2", alpha=0.7) +
  geom_errorbar( aes(x=GLT_Year1, ymin=mean-sd, ymax=mean+sd), width=0.4, colour="black", alpha=0.9, size=1) +
  ylab("Taux de croissance annuel moyen") +
  xlab("Année") +
  theme_light()



## GROUP DENSITY
# Data subset
GD_year = data_clean_v2 %>% 
  dplyr::filter(File == "Obs") %>%
  dplyr::filter(GLT_Year1 >= 2000 & GLT_Year1 <= 2020) %>% 
  dplyr::group_by(Group) %>% 
  dplyr::filter(!is.na(UMMPs)) %>%
  dplyr::filter(!is.na(Group)) %>%
  dplyr::mutate(Monit_Years_grp = diff(range(GLT_Year1))+1) %>% 
  dplyr::mutate(Monit_1stYear_grp = min(GLT_Year1), 
                Monit_LastYear_grp = max(GLT_Year1)) %>%
  dplyr::mutate(Monit_Period_grp = paste0(unique(c(Monit_1stYear_grp, Monit_LastYear_grp)), collapse = '-')) %>%
  ungroup() %>% 
  as.data.frame() # By group
GD_year = sub %>% 
  dplyr::select(c(UMMPs,Group,GLT_Year1)) %>% 
  dplyr::distinct()
GD_year = GD_year %>% 
  dplyr::rename(Year=GLT_Year1) %>% 
  dplyr::left_join(dplyr::select(totperim,c(Group,Year,Perimetre))) %>% 
  dplyr::left_join(dplyr::select(totshape,c(Group,Year,Shape))) %>% 
  dplyr::left_join(dplyr::select(totsizearea,c(Group,Year,Size))) %>%
  dplyr::rename(GLT_Year1=Year) %>% 
  dplyr::distinct(Group,GLT_Year1,Perimetre,Shape,Size, .keep_all =TRUE)
GD_year = GD_year %>% 
  dplyr::left_join(dplyr::select(cumul_rainfall,c(GLT_Year1,cum_mean_cell_rainfall)))
GD_year = GD_year%>% 
  dplyr::filter(GLT_Year1 >= 2000 & GLT_Year1 <= 2020) %>% 
  dplyr::group_by(GLT_Year1,Shape,Size) %>%
  dplyr::mutate(id_frag = cur_group_id()) %>% 
  ungroup() %>% 
  as.data.frame()
GD_year = GD_year %>% 
  dplyr::group_by(id_frag) %>% 
  dplyr::mutate(n_grp = n_distinct(Group)) %>% 
  dplyr::mutate(Grp_density = n_grp/Size) %>% 
  dplyr::ungroup()
GD_year = GD_year %>% 
  dplyr::distinct(id_frag, .keep_all = TRUE) %>% 
  dplyr::select(c(GLT_Year1,UMMPs,id_frag,Size,n_grp,Grp_density,cum_mean_cell_rainfall))
GD_year = GD_year %>% 
  dplyr::mutate(Size_sc = scale(Size) %>% as.vector) %>% 
  dplyr::mutate(Rainfall_sc = scale(cum_mean_cell_rainfall) %>% as.vector) %>% 
  dplyr::mutate(logGD = log(Grp_density))
GD_year %>% 
  dplyr::summarise(across(c("UMMPs","id_frag"), ~ n_distinct(.x, na.rm = TRUE)))
GD_year %>% 
  summary()
# Distribution
car::qqp(GD_year$Grp_density, "norm") # QQplot normal distribution
car::qqp(GD_year$Grp_density, "lnorm") # QQplot lognormal distribution
nbinom <- fitdistr(GD_year$Grp_density, "Negative Binomial")
car::qqp(GD_year$Grp_density, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]]) # QQplot negative binomial
poisson <- fitdistr(GD_year$Grp_density, "Poisson")
car::qqp(GD_year$Grp_density, "pois", lambda=poisson$estimate) # QQplot Poisson
gamma <- fitdistr(GD_year$Grp_density, "gamma")
car::qqp(GD_year$Grp_density, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]]) # Gamma distribution
# GLMM
lmm1 = lmerTest::lmer(logGD ~ Size_sc + Rainfall_sc + (1|GLT_Year1) + (1|UMMPs),
                      data = GD_year, weights=n_grp, REML=FALSE)
summary(lmm1)
# Update model
lmm_up = update(lmm1,~.-Rainfall_sc)
summary(lmm_up)

# Scatter plot
# CF : https://strengejacke.github.io/ggeffects/articles/introduction_randomeffects.html
prediction = as.data.frame(ggpredict(lmm_up, terms="Size_sc [all]", type="fixed", back.transform = TRUE))
prediction$Size= (prediction$x * sd(GD_year$Size)) + mean(GD_year$Size) # Unscale values
prediction = prediction %>% 
  dplyr::mutate_at(c("predicted","conf.low","conf.high"), funs(exp=exp(.)))
# Plot (log)
ggplot(data=prediction, aes(x = Size, y = predicted)) +
  geom_line() +
  geom_line(data = prediction, aes(x = Size, y = conf.low), linetype = "dashed", color = "black") +
  geom_line(data = prediction, aes(x = Size, y = conf.high), linetype = "dashed", color = "black") +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=0.2) +
  geom_jitter(aes(x = Size, y = logGD), data=GD_year) +
  theme_minimal() +
  xlab("Taille du fragment") +
  ylab("Log(Densité de groupes)")
# Plot
ggplot(data=prediction, aes(x = Size, y = predicted_exp)) +
  geom_line() +
  geom_line(data = prediction, aes(x = Size, y = conf.low_exp), linetype = "dashed", color = "black") +
  geom_line(data = prediction, aes(x = Size, y = conf.high_exp), linetype = "dashed", color = "black") +
  geom_ribbon(aes(ymin=conf.low_exp, ymax=conf.high_exp), alpha=0.2) +
  geom_jitter(aes(x = Size, y = Grp_density), data=GD_year) +
  theme_minimal() +
  xlab("Taille du fragment") +
  ylab("Densité de groupes")




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


