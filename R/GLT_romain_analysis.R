####### GLT observation data analysis
### Author: Romain Monassier
### Date: 2023

##### Libraries
load.lib = c("dplyr","tibble", "data.table",
             "tidyr", "xlsx", "zoo", "readxl",
             "readr", "ggplot2", "lubridate",
             "plyr", "tidyverse", "changepoint",
             "strucchange", "sp", "raster")
sapply(load.lib,require,character=TRUE)


### DATA
## GLT dataset
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/data_clean_v2.RData")
## Monitored groups
Nb_obs_gp = read_delim("data/NewlyCreatedData/Nb_obs_gp.csv", delim=";", show_col_types = FALSE)
Nb_obs_gp %>%
  dplyr::summarise(across(c("UMMPs","Farm","Fragment.Region"), ~ n_distinct(.x, na.rm = TRUE)))
Nb_obs_gp %>%
  dplyr::group_by(UMMPs,Fragment.Region) %>%
  dplyr::summarise(n_grp = n_distinct(Group)) %>%
  print(n=50)
# Join
data_clean_v2 = data_clean_v2 %>%
  left_join(select(Nb_obs_gp,c(Group,UMMPs,Farm,Lat,Long,Fragment.Region)))
## Worldclim
worldclim = getData("worldclim",var="bio",res=10)


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
# Create monitoring periods
data_clean_v2 = data_clean_v2 %>% 
  dplyr::group_by(Group) %>% 
  dplyr::mutate(Monit_Years_grp = diff(range(GLT_Year1))+1) %>% 
  dplyr::mutate(Monit_1stYear_grp = min(GLT_Year1), 
                Monit_LastYear_grp = max(GLT_Year1)) %>%
  dplyr::mutate(Monit_Period_grp = paste0(unique(c(Monit_1stYear_grp, Monit_LastYear_grp)), collapse = '-')) %>%
  ungroup() %>% 
  as.data.table()
data_clean_v2 = data_clean_v2 %>% 
  dplyr::group_by(UMMPs) %>% 
  dplyr::mutate(Monit_Years_frag = diff(range(GLT_Year1))+1) %>% 
  dplyr::mutate(Monit_1stYear_frag = min(GLT_Year1), 
                Monit_LastYear_frag = max(GLT_Year1)) %>%
  dplyr::mutate(Monit_Period_frag = paste0(unique(c(Monit_1stYear_frag, Monit_LastYear_frag)), collapse = '-')) %>%
  ungroup() %>% 
  as.data.table()

### SUBSET DATA
sub = data_clean_v2 %>%
  dplyr::filter(File == "Obs") %>%
  dplyr::group_by(Group, Monit_Years_grp) %>% 
  dplyr::filter(Monit_Years_grp >= 6) %>% 
  dplyr::filter(!is.na(Group)) %>%
  ungroup() %>% 
  as.data.table()


### STATISTICAL SUMMARY
sub %>% 
  dplyr::select(GLT_Year1, Monit_Years_grp) %>% 
  summary()
sub %>% 
  dplyr::summarise_at(c("UMMPs","Fragment.Region","Group","GLT"), n_distinct, na.rm = TRUE)
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
  distinct(Group,n_visites,mean_obs_year)
visites_grp %>% 
  summary()
# By period
sub %>%
  dplyr::filter(any(Monit_1stYear_grp < 2002 & Monit_LastYear_grp > 2018)) %>% 
  #dplyr::filter(Monit_1stYear_grp < 2002 & Monit_LastYear_grp > 2018) %>% 
  dplyr::filter(!is.na(UMMPs)) %>% 
  dplyr::summarise_at(c("UMMPs","Group","GLT"), n_distinct, na.rm = TRUE)
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

# Plot monitored periods
# By fragment
g <- sub[order(sub$Monit_1stYear_frag,sub$UMMPs),]
g$UMMPs <- factor(g$UMMPs, levels=unique(g$UMMPs))
g = g %>% dplyr::filter(!is.na(UMMPs))
ggplot(g, aes(x = Monit_1stYear_frag, y = UMMPs)) +
  geom_segment(aes(xend = Monit_LastYear_frag, yend = UMMPs), colour = "orange") +
  geom_point(size = 2, colour="darkorange") +
  geom_point(aes(x = Monit_LastYear_frag), size = 2, colour="darkorange") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=10))
# By group
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
# Group size and growth rate statistical summaries
# Annual group size and growth rate  
GS_year = sub %>% 
  dplyr::group_by(UMMPs,Group,GLT_Year1) %>% 
  dplyr::summarise(Year_grp_size = n_distinct(GLT)) %>% 
  dplyr::mutate(Year_growth_rate = Year_grp_size/lag(Year_grp_size)) %>% 
  ungroup()
GS_year %>% 
  summary()
# Seasonal group size and growth rate 
GS_season = sub %>%
  dplyr::group_by(UMMPs,Group,GLT_Year1,season) %>% 
  dplyr::arrange(Group,GLT_Year1,season) %>% 
  dplyr::summarise(Season_grp_size = n_distinct(GLT)) %>% 
  ungroup()
GS_season %>% 
  summary()
# Statistical summaries
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
plot_GS = GS_year %>% 
  dplyr::group_by(GLT_Year1) %>%
  dplyr::summarise(mean = mean(Year_grp_size)) %>% 
  ungroup() %>% 
  as.data.frame()
plot_GS <- ts(plot_GS$mean, start = 2001)
plot.ts(plot_GS, type= "b", main = "Mean group size over time", xlab = "Years", ylab = "Mean number of individuals")
plot_GS = GS_year %>% 
  dplyr::filter(!is.na(Year_growth_rate)) %>% 
  dplyr::group_by(GLT_Year1) %>%
  dplyr::summarise(mean = mean(Year_growth_rate)) %>% 
  ungroup() %>% 
  as.data.frame()
plot_GS <- ts(plot_GS$mean, start = 2002)
plot.ts(plot_GS, type= "b", main = "Mean group growth rate over time", xlab = "Years", ylab = "Mean growth rate")

# Plot group size/grath rate evolution (facet)
ggplot(GS_year,aes(x = GLT_Year1, y = Year_grp_size)) + 
  geom_point(aes(color=Group)) +
  geom_line(data=GS_year[!is.na(GS_year$Year_grp_size),]) +
  theme(legend.position = "none") +
  facet_wrap(~Group)
# Plot group size/grath rate evolution by fragment
ggplot(GS_year,aes(x = GLT_Year1, y = Year_grp_size)) + 
  geom_point(aes(color=Group)) +
  geom_line(data=GS_year[!is.na(GS_year$Year_grp_size),],aes(color=Group)) +
  theme(legend.position = "none") +
  facet_wrap(~UMMPs)
plot_GS = GS_year %>% 
  dplyr::group_by(UMMPs,GLT_Year1) %>%
  dplyr::summarise(mean = mean(Year_grp_size)) %>% 
  ungroup() %>% 
  as.data.frame()
ggplot(plot_GS,aes(x = GLT_Year1, y = mean)) + 
  geom_line(data=plot_GS[!is.na(plot_GS$mean),]) +
  theme(legend.position = "none") +
  facet_wrap(~UMMPs)


## Statistical test
# Change point analysis
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
  map(~ select(., mean_size)) %>% 
  map(~ pull(., mean_size)) %>% 
  map(~ cpt.mean(., penalty="SIC", method="PELT"))
values = map(m_pelt,"cpts")
for (i in 1:length(values)) {
  temp<-values[[i]]
  cp_frag<-rbind(cp_frag,GS_frag[[i]][temp,])
}
par(mfrow = c(3,3))
plot(m_pelt[[9]], type = "l", cpt.col = "blue", xlab = "Index", cpt.width = 4)





# Composition
analysis.data %>% 
  dplyr::group_by(Group,GLT) %>% 
  dplyr::summarise(n_rows = n(),
                   count_na = sum(is.na(IdadeOK) | is.na(SexOK)),
                   prop_na=count_na*100/n_rows)
