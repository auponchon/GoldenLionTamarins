####### GLT data curing
### Author: Romain Monassier
### Date: 2023

##### Libraries
load.lib = c("dplyr","tibble", "data.table",
             "tidyr", "xlsx", "zoo", "readxl",
             "readr", "ggplot2", "lubridate")
sapply(load.lib,require,character=TRUE)

##### Data
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/clean_raw_data_long.RData")
data.clean = as.data.table(data.clean)
summary(data.clean)
# Date formatting
data.clean = data.clean %>% 
  mutate(DateObsMY = format(DateObs, "%m-%Y"))
data.clean$DateObs = as.Date(data.clean$DateObs)
data.clean = data.clean %>% 
  mutate(
    season = case_when(
      lubridate::month(DateObs) %in% 6:8 ~ 'Dry',
      lubridate::month(DateObs) %in% c(10:12, 1:4) ~ 'Wet',
      lubridate::month(DateObs) %in% c(5, 9) ~ 'Transition'
    )
  ) # Season classification according to Dietz et al (1994)


# Join capture data information
capt.data = read_excel("D:/monas/Git/repo/glt/GoldenLionTamarins/data/RawData/ProcessamentoMLD.xlsx")
capt.data = capt.data %>% 
  rename(DateObs = Date) %>% 
  rename(Group = Grupo)
capt.data = capt.data %>% 
  mutate(DateObsMY = format(DateObs, "%m-%Y"))
data.clean = data.clean %>% 
  left_join(select(capt.data,c(GLT, Group, DateObs, BWeight))) %>% 
  rename(Weight = BWeight)

# Load GLT movement
GLT_mov = read_excel("D:/monas/Git/repo/glt/GoldenLionTamarins/data/RawData/GLT_movement.xls",
                     na="NA")
GLT_mov = GLT_mov %>% 
  rename(GLT = Individual) %>% 
  rename(Tattoo = Tatoo)
GLT_mov$Estimated.date.of.emigration = as.Date(GLT_mov$Estimated.date.of.emigration, format="%d/%m/%y")

# Rowid
data.clean = rowid_to_column(data.clean) # Add a rowid column (observation unique id)
data.clean$rowid = paste0("obs",data.clean$rowid)

# Observation order
data.clean = data.clean %>% 
  group_by(GLT) %>% 
  arrange(GLT, as.Date(DateObs)) %>% 
  mutate(ObsOrder = row_number())




##### Sex verification
## Update typing errors
data.clean = data.clean %>% 
  mutate(SexOK = ifelse(Sex == "m", "M",
                        ifelse(Sex == "M0" | Sex == "nonID" | Sex == "S", NA,
                               Sex)))

## Correct sex errors
# Select duplicated sexes
dup.sex = data.clean %>% 
  select(GLT, Tattoo, Region, SexOK) %>% 
  group_by(GLT) %>%
  filter(n_distinct(SexOK) > 1) %>%
  ungroup()
dup.sex %>% summarise(n=n_distinct(GLT),
                      p=n_distinct(GLT)*100/2063)
n = dup.sex %>% 
  group_by(GLT, SexOK) %>% 
  summarise(n=n())

# IF combinaison of one sex and NAs
# Fill in NAs for individuals with another value
# Select GLT with NAs in sex column
sub = dup.sex %>% 
  filter(is.na(SexOK)) %>% 
  distinct(GLT) %>% 
  pull()
# Subset data.clean with the latter GLT, and update the NA value according to the dominant one (na.locf)
errors = data.clean %>% 
  select(rowid, GLT, Tattoo, SexOK) %>% 
  subset(GLT %in% sub) %>% 
  group_by(GLT) %>% 
  arrange(GLT, SexOK) %>% 
  mutate(SexOK = zoo::na.locf(SexOK)) %>% 
  rename(Sex.b = SexOK) %>% 
  ungroup() %>% 
  as.data.table()
# Update the SexOK in the dataset where observations match those 
setDT(data.clean)[errors, "SexOK" := .(Sex.b), on = "GLT"]


# IF Female + Male
# Remove the NAs errors from the table
dup.sex = dup.sex %>% 
  group_by(GLT) %>% 
  arrange(GLT, SexOK) %>% 
  mutate(SexOK = zoo::na.locf(SexOK)) %>% 
  filter(n_distinct(SexOK) > 1)
n = dup.sex %>% 
  group_by(GLT, SexOK) %>% 
  summarise(n=n())
n = n %>%
  pivot_wider(names_from = SexOK, values_from = n)
errors = n %>% 
  select(F, M) %>% 
  select(GLT) %>% 
  unlist()
# Red flags
rf.sex = data.clean %>% 
  filter(GLT %in% errors) %>% 
  arrange(GLT, ObsOrder) %>% 
  select(c(rowid, GLT, Tattoo, Group, DateObs, ObsOrder, Sex, File, Weight)) %>% 
  mutate(Sex.b = NA)
setDT(data.clean)[rf.sex, "SexOK" := .(Sex.b), on = "GLT"]
data.clean$Sex[data.clean$GLT == "LC4"] = "F"
data.clean$Sex[data.clean$GLT == "RT1"] = "M"


# # Assign the dominant sex for the individuals for which the difference is above 7 and deemed non significant
# n_ok = n %>% 
#   filter(SexOK == "OK") %>% 
#   select(F, M)
# n_ok$Sex.b <- apply(n_ok, 1, function(x) paste0(names(n_ok)[x == max(x)])) # For each individual, we implement the most-mentioned sex
# n_ok = n_ok %>% 
#   rownames_to_column(var="GLT") %>% 
#   select(c("GLT","Sex.b"))
# setDT(data.clean)[n_ok, "SexOK" := .(Sex.b), on = "GLT"] # Correct the dataset


## Export errors 
rf.sex = rf.sex %>% 
  filter(GLT!="LC4" & GLT!="RT1")
# write.xlsx(rf.sex, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/sex_checkV3.xlsx", row.names=FALSE)



### Age and life stage verification

## Birth Dates from 1989 to 2001
BirthDates = read_excel("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/BirthDates.xlsx",
                        na="NA")
# Remove duplicated rows (GLT)
BirthDates = BirthDates[!duplicated(BirthDates$GLT), ]
# Check typing errors
data.clean = data.clean %>% 
  left_join(select(BirthDates, c(GLT, Birth_VR)))
data.clean %>% 
  group_by(Birth_VR) %>%
  summarise(no_rows = length(Birth_VR)) %>% 
  print(n=200)
data.clean = data.clean %>% 
  mutate(Birth_VR = ifelse(Birth_VR == "?", NA,
                           Birth_VR))
data.clean$Birth_VR = as.Date(data.clean$Birth_VR, format="%d/%m/%y")

## Birth dates post-2001
# Source : GLT_mov
data.clean = data.clean %>% 
  rename(Birth_mov = Birth)
data.clean %>%
  group_by(Birth_mov) %>%
  summarise(no_rows = length(Birth_mov)) %>%
  print(n=350)
data.clean$Birth_mov = as.Date(data.clean$Birth_mov, format="%d/%m/%y")

# Copy dataset
n = data.clean %>%
  select(c(rowid, DateObs, ObsOrder, GLT, Tattoo, Group, Idade, Weight, Birth_VR, Birth_mov))

# Manual corrections from below inspection
n$Birth_VR[n$GLT == "RT13"] = "2006-03-01"
n$Birth_VR[n$GLT == "RT14"] = "2006-03-01"


## Check Birth dates errors
# 1th ERROR  : remove duplicates in the Birth_mov dates
# SOLUTION : manual inspection and correction
dup.birth = n %>% 
  select(GLT, Birth_mov) %>% 
  group_by(GLT) %>%
  filter(!is.na(Birth_mov)) %>% 
  filter(n_distinct(Birth_mov) > 1) %>%
  ungroup()
dup.birth = dup.birth %>% 
  group_by(GLT, Birth_mov) %>% 
  summarise(n=n())
dup.birth = dup.birth %>% 
  group_by(GLT) %>% 
  mutate(Births = paste0(Birth_mov, collapse = " or ")) %>% 
  distinct(GLT, Births) %>% 
  ungroup()
dup.birth = dup.birth %>% 
  filter(GLT != "?") %>% 
  separate(Births, c("opt1", "opt2", "opt3", "opt4", "opt5"), " or ")

# Create subsets according to the number of different birth dates
# Two different dates
dup.birth2 = dup.birth %>% 
  filter(is.na(opt3)) %>% 
  select(1:3) %>% 
  as.data.table()
dup.birth2$opt1 = as.Date(x = dup.birth2$opt1, format="%Y-%m-%d")
dup.birth2$opt2 = as.Date(x = dup.birth2$opt2, format="%Y-%m-%d")
dup.birth2 = dup.birth2 %>% 
  mutate(diffBirth = abs(difftime(opt1, opt2, units="days")))
dup.birth2 = dup.birth2 %>% 
  mutate(Birth.b = ifelse(diffBirth <= 180, opt1, NA))
dup.birth2$Birth.b = as.Date(dup.birth2$Birth.b)
setDT(n)[dup.birth2, "Birth_mov" := .(Birth.b), on = "GLT"] # Correct the dataset
errors.index = dup.birth2 %>% 
  filter(is.na(Birth.b)) %>% 
  select(GLT)

# Three different dates
dup.birth3 = dup.birth %>% 
  filter(is.na(opt4) & !is.na(opt3)) %>% 
  select(1:4) %>% 
  as.data.table()
dup.birth3$opt1 = as.Date(x = dup.birth3$opt1, format="%Y-%m-%d")
dup.birth3$opt2 = as.Date(x = dup.birth3$opt2, format="%Y-%m-%d")
dup.birth3$opt3 = as.Date(x = dup.birth3$opt3, format="%Y-%m-%d")
n$Birth_mov[n$GLT == "1375"] <- NA
n$Birth_mov[n$GLT == "AF20"] <- "2011-12-01"
n$Birth_mov[n$GLT == "E25"] <- "1999-10-01"
n$Birth_mov[n$GLT == "KE10"] <- "2002-09-01"
n$Birth_mov[n$GLT == "KE4"] <- "2000-10-01"
n$Birth_mov[n$GLT == "RV6"] <- "1999-10-10"
n$Birth_mov[n$GLT == "SP23"] <- "2011-11-01"
errors.index = dup.birth3 %>% 
  filter(GLT=="1375") %>% 
  select(GLT) %>% 
  bind_rows(errors.index) %>% 
  mutate(error_type="multiple_BD")

# Five different dates
dup.birth5 = dup.birth %>% 
  filter(!is.na(opt5)) %>% 
  as.data.table()
dup.birth5$opt1 = as.Date(x = dup.birth5$opt1, format="%Y-%m-%d")
dup.birth5$opt2 = as.Date(x = dup.birth5$opt2, format="%Y-%m-%d")
dup.birth5$opt3 = as.Date(x = dup.birth5$opt3, format="%Y-%m-%d")
dup.birth5$opt4 = as.Date(x = dup.birth5$opt4, format="%Y-%m-%d")
dup.birth5$opt5 = as.Date(x = dup.birth5$opt5, format="%Y-%m-%d")
n$Birth_mov[n$GLT == "FT"] <- NA
n$Birth_mov[n$GLT == "IN"] <- NA
n$Birth_mov[n$GLT == "T0"] <- NA


# 2nd ERROR : Incoherences between source birth dates
errors = n %>% 
  mutate(diff.b1.b2 = abs(difftime(Birth_mov, Birth_VR, units="days")))
errors %>%
  filter(diff.b1.b2 != "0 days" & !is.na(diff.b1.b2)) %>% 
  summarise(n_GLT=n_distinct(GLT))
# SOLUTION : keep the more reliable birth date (given by VR)
n = n %>% 
  mutate(BirthOK = ifelse(!is.na(Birth_VR) & !is.na(Birth_mov), Birth_VR,
                              ifelse(is.na(Birth_VR) & !is.na(Birth_mov), Birth_mov,
                                     Birth_VR)))
n$BirthOK = as.Date(n$BirthOK)


# 3rd ERROR : comparison between the estimated date of emigration and the Birth Date
errors = n %>% 
  left_join(select(GLT_mov,c(GLT, Estimated.date.of.emigration)))
errors = errors %>% 
  mutate(diff.emig.birth = difftime(Estimated.date.of.emigration, BirthOK, units="days"))
rf.birthdates = errors[grepl("^-", errors$diff.emig.birth), ]
# SOLUTION : NA
rf.birthdates = rf.birthdates %>% 
  mutate(Birth.b = NA) %>% 
  distinct(GLT, .keep_all = TRUE) %>% 
  mutate(error_type="diff_BD_Emig")
errors.index = errors.index %>% 
  union(select(rf.birthdates,c(GLT, error_type)))
setDT(n)[rf.birthdates, c("Birth_VR","Birth_mov","BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# 4th ERROR : differences between birth dates and dates of first observation
errors = n %>% 
  filter(ObsOrder == 1) %>%  
  group_by(GLT) %>% 
  mutate(diff.obs.birth = difftime(DateObs, BirthOK, units="days")) %>% 
  ungroup() %>% 
  as.data.table()
# Negative values
rf.birthdates = errors %>% 
  filter(grepl("^-", errors$diff.obs.birth)) # individuals observed before their birth
# SOLUTION : NA
rf.birthdates = rf.birthdates %>% 
  mutate(Birth.b = NA) %>% 
  mutate(error_type = "diff_BD_1stObs")
errors.index = errors.index %>% 
  union(select(rf.birthdates,c(GLT, error_type)))
setDT(n)[rf.birthdates, c("Birth_VR","Birth_mov","BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# 5th ERROR : individuals first observed as adults with known birth dates
errors = n %>% 
  filter(Idade == "AD" & ObsOrder == 1 & !is.na(BirthOK)) %>% 
  mutate(Birth.b = NA)
rf.birthdates = errors %>% 
  distinct(GLT, .keep_all = TRUE) %>% 
  mutate(error_type = "Adult_with_BD")
errors.index = errors.index %>% 
  union(select(rf.birthdates,c(GLT, error_type)))
setDT(n)[errors, c("Birth_VR","Birth_mov","BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# 6th ERROR : individuals whose birth date (from Birth_mov) = date of first observation
errors = n %>% 
  select(c(rowid, DateObs, ObsOrder, GLT, Tattoo, Group, Idade, Birth_VR, Birth_mov, Weight)) %>% 
  filter(!is.na(Birth_mov) & is.na(Birth_VR)) %>% 
  filter(ObsOrder == 1) %>% 
  filter(DateObs == Birth_mov) %>% 
  distinct(GLT, .keep_all = TRUE) %>% 
  mutate(Birth.b = NA)
rf.birthdates = errors %>% 
  distinct(GLT, .keep_all = TRUE) %>% 
  mutate(error_type = "1stObs_is_BD")
errors.index = errors.index %>% 
  union(select(rf.birthdates,c(GLT, error_type)))
setDT(n)[errors, c("Birth_mov","BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# 7th ERROR : individuals within groups with same birth dates
# N_obs_gp = read_delim("data/NewlyCreatedData/Nb_obs_gp.csv", delim=";")
errors = n %>%
  filter(!is.na(BirthOK) & !is.na(Group)) %>%
  distinct(GLT, .keep_all = TRUE) %>% 
  group_by(Group, BirthOK) %>%
  filter(n()>=3) %>%
  mutate(n=n()) %>% 
  arrange(Group, BirthOK) %>%
  as.data.table() %>% 
  mutate(Birth.b = NA)
rf.birthdates = errors %>% 
  mutate(error_type = ifelse(n == 3 | n == 4, "Grp_sameBD_?P",
                             "Grp_sameBD"))
errors.index = errors.index %>% 
  union(select(rf.birthdates,c(GLT, error_type)))
setDT(n)[errors, c("Birth_VR","Birth_mov","BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset



# Export birth dates errors
errors.index = errors.index[, list(error_type = paste(error_type, collapse="+")), by = GLT] # Paste multiple errors for same individuals
rf.birthdates = data.clean %>% 
  filter(GLT %in% errors.index$GLT) %>% 
  select(c(rowid, GLT, Tattoo, Group, Region, DateObs, ObsOrder, Birth_mov, Birth_VR, Idade, Weight, File)) %>% 
  left_join(errors.index) %>% 
  arrange(GLT, ObsOrder)
# write.xlsx(rf.birthdates, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/red_flags_birthdates1.xlsx", row.names=FALSE)



### Implementing the Idade column
## ARRET ICI


## From the correct birth date (Birth_VR)
n = n %>% 
  mutate(IdadeOK = ifelse(difftime(n$DateObs, n$Birth_VR, units="days") <= 60, "IN",
                         ifelse(difftime(n$DateObs, n$Birth_VR, units="days") > 60 & difftime(n$DateObs, n$Birth_VR, units="days") <= 150, "JU",
                                ifelse(difftime(n$DateObs, n$Birth_VR, units="days") > 150 & difftime(n$DateObs, n$Birth_VR, units="days") <= 300, "AdvJU",
                                       ifelse(difftime(n$DateObs, n$Birth_VR, units="days") > 300 & difftime(n$DateObs, n$Birth_VR, units="days") <= 540, "SA",
                                              ifelse(difftime(n$DateObs, n$Birth_VR, units="days") > 540, "AD", 
                                                     NA))))))

## Using the Idade existing field
n$Idade = as.character(n$Idade)
# Append the Idade variable and fill
## REPRENDRE LES SEUILS !!!

n = n %>% 
  mutate(IdadeOK = ifelse(is.na(IdadeOK), Idade, IdadeOK))
# CASE 1 : individual known as adults at least once
sub_AD = n %>% 
  group_by(GLT) %>%
  filter(all(c("AD", NA) %in% IdadeOK)) %>% 
  filter(IdadeOK == "AD" | is.na(IdadeOK)) %>% 
  mutate(RefDate = first(DateObs[!is.na(IdadeOK)])) %>% 
  ungroup() # Extract the earlier date where IdadeOK is known
sub_AD = sub_AD %>% 
  group_by(GLT) %>% 
  arrange(GLT, DateObs) %>%
  mutate(diff = DateObs - RefDate,
         diff_days = as.numeric(diff, units = 'days')) %>% 
  mutate(diff_days = ifelse(is.na(diff_days), 0, diff_days)) # Compute the time difference between the reference date and the observation date
sub_AD = sub_AD %>% 
  filter(!grepl("^-", diff_days)) %>% 
  mutate(Idade.b = "AD") %>% 
  filter(is.na(IdadeOK) & !is.na(Idade.b)) %>% 
  as.data.table() # Assign "Adult" to every observation
setDT(n)[sub_AD, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset
# CASE 2 : individuals known as juveniles at least once
sub_JU = n %>% 
  group_by(GLT) %>%
  filter(all(c("JU", NA) %in% IdadeOK)) %>% 
  filter(IdadeOK == "JU" | is.na(IdadeOK)) %>% 
  mutate(RefDate = first(DateObs[!is.na(IdadeOK)])) %>% 
  ungroup() # Extract the earlier date where IdadeOK is known
sub_JU = sub_JU %>% 
  group_by(GLT) %>% 
  arrange(GLT, DateObs) %>%
  mutate(diff = DateObs - RefDate,
         diff_days = as.numeric(diff, units = 'days')) %>% 
  mutate(diff_days = ifelse(is.na(diff_days), 0, diff_days)) # Compute the time difference between the reference date and the observation date
sub_JU = sub_JU %>% 
  filter(grepl("^-", diff_days)) %>% 
  mutate(Idade.b = "JU") %>% 
  as.data.table() # Assign "Juv" to every observation
setDT(n)[sub_JU, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset
# CASE 3 : individuals observed 16 months after being noted as juveniles
sub_JU = n %>% 
  group_by(GLT) %>%
  filter(all(c("JU", NA) %in% IdadeOK)) %>% 
  filter(IdadeOK == "JU" | is.na(IdadeOK)) %>% 
  mutate(RefDate = first(DateObs[!is.na(IdadeOK)])) %>% 
  ungroup() # Extract the earlier date where IdadeOK is known
sub_JU = sub_JU %>% 
  group_by(GLT) %>% 
  arrange(GLT, DateObs) %>%
  mutate(diff = DateObs - RefDate,
         diff_days = as.numeric(diff, units = 'days')) %>% 
  mutate(diff_days = ifelse(is.na(diff_days), 0, diff_days)) # Compute the time difference between the reference date and the observation date
sub_JU = sub_JU %>% 
  mutate(Idade.b = ifelse(diff_days > 480, "AD", IdadeOK)) %>% 
  filter(Idade.b == "AD") %>% 
  as.data.table() # Assign "Adult" to every observation
setDT(n)[sub_JU, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset
# CASE 4 : individuals observed 8 months after being noted as sub-adults
sub_SA = n %>% 
  group_by(GLT) %>%
  filter(all(c("SA", NA) %in% IdadeOK)) %>% 
  filter(IdadeOK == "SA" | is.na(IdadeOK)) %>% 
  mutate(RefDate = first(DateObs[!is.na(IdadeOK)])) %>% 
  ungroup() # Extract the earlier date where IdadeOK is known
sub_SA = sub_SA %>% 
  group_by(GLT) %>% 
  arrange(GLT, DateObs) %>%
  mutate(diff = DateObs - RefDate,
         diff_days = as.numeric(diff, units = 'days')) %>% 
  mutate(diff_days = ifelse(is.na(diff_days), 0, diff_days)) # Compute the time difference between the reference date and the observation date
sub_SA = sub_SA %>% 
  mutate(Idade.b = ifelse(diff_days > 240, "AD", IdadeOK)) %>% 
  filter(Idade.b == "AD") %>% 
  as.data.table() # Assign "Adult" to every observation
setDT(n)[sub_SA, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset


# ## Using an inferred birth date
# sub = n %>%
#   filter(is.na(BirthOK)) %>% 
#   filter(ObsOrder == 1 & Idade == "JU") %>% 
#   mutate(Birth_inferred = ymd(DateObs)-months(3))
# n = n %>% left_join(select(sub, c(GLT, Birth_inferred)))
# n = n %>% 
#   mutate(IdadeOK = ifelse(is.na(IdadeOK) & difftime(n$DateObs, n$Birth_inferred, units="days") <= 365, "JU",
#                           ifelse(is.na(IdadeOK) & difftime(n$DateObs, n$Birth_inferred, units="days") > 365 & difftime(n$DateObs, n$Birth_inferred, units="days") <= 1095, "SA",
#                                  ifelse(is.na(IdadeOK) & difftime(n$DateObs, n$Birth_inferred, units="days") > 1095, "AD",
#                                         IdadeOK))))

## Using the GLT weight
n = n %>% 
  mutate(IdadeOK = ifelse(is.na(IdadeOK) & Weight <= 400, "JU",
                          ifelse(is.na(IdadeOK) & Weight > 400, "AD",
                                 IdadeOK)))


# Append the other life stages using observed weights
# CASE 1 : individual known as adults at least once
sub_AD = n %>% 
  group_by(GLT) %>%
  filter(all(c("AD", NA) %in% IdadeOK)) %>% 
  filter(IdadeOK == "AD" | is.na(IdadeOK)) %>% 
  mutate(RefDate = first(DateObs[!is.na(IdadeOK)])) %>% 
  ungroup() # Extract the earlier date where IdadeOK is known
sub_AD = sub_AD %>% 
  group_by(GLT) %>% 
  arrange(GLT, DateObs) %>%
  mutate(diff = DateObs - RefDate,
         diff_days = as.numeric(diff, units = 'days')) %>% 
  mutate(diff_days = ifelse(is.na(diff_days), 0, diff_days)) # Compute the time difference between the reference date and the observation date
sub_AD = sub_AD %>% 
  filter(!grepl("^-", diff_days)) %>% 
  mutate(Idade.b = "AD") %>% 
  filter(is.na(IdadeOK) & !is.na(Idade.b)) %>% 
  as.data.table() # Assign "Adult" to every observation
setDT(n)[sub_AD, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset
# CASE 2 : individuals known as juveniles at least once
sub_JU = n %>% 
  group_by(GLT) %>%
  filter(all(c("JU", NA) %in% IdadeOK)) %>% 
  filter(IdadeOK == "JU" | is.na(IdadeOK)) %>% 
  mutate(RefDate = first(DateObs[!is.na(IdadeOK)])) %>% 
  ungroup() # Extract the earlier date where IdadeOK is known
sub_JU = sub_JU %>% 
  group_by(GLT) %>% 
  arrange(GLT, DateObs) %>%
  mutate(diff = DateObs - RefDate,
         diff_days = as.numeric(diff, units = 'days')) %>% 
  mutate(diff_days = ifelse(is.na(diff_days), 0, diff_days)) # Compute the time difference between the reference date and the observation date
sub_JU = sub_JU %>% 
  filter(grepl("^-", diff_days)) %>% 
  mutate(Idade.b = "JU") %>% 
  filter(is.na(IdadeOK) & !is.na(Idade.b)) %>% 
  as.data.table() # Assign "Juv" to every observation
setDT(n)[sub_JU, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset
# CASE 3 : individuals observed 3 years after being noted as juveniles
sub_JU = n %>% 
  group_by(GLT) %>%
  filter(all(c("JU", NA) %in% IdadeOK)) %>% 
  filter(IdadeOK == "JU" | is.na(IdadeOK)) %>% 
  mutate(RefDate = first(DateObs[!is.na(IdadeOK)])) %>% 
  ungroup() # Extract the earlier date where IdadeOK is known
sub_JU = sub_JU %>% 
  group_by(GLT) %>% 
  arrange(GLT, DateObs) %>%
  mutate(diff = DateObs - RefDate,
         diff_days = as.numeric(diff, units = 'days')) %>% 
  mutate(diff_days = ifelse(is.na(diff_days), 0, diff_days)) # Compute the time difference between the reference date and the observation date
sub_JU = sub_JU %>% 
  mutate(Idade.b = ifelse(diff_days > 1095, "AD", IdadeOK)) %>% 
  filter(Idade.b == "AD") %>% 
  filter(is.na(IdadeOK) & !is.na(Idade.b)) %>% 
  as.data.table() # Assign "Adult" to every observation
setDT(n)[sub_JU, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset


## Using the Birth_mov field
n = n %>% 
  mutate(IdadeOK = ifelse(is.na(IdadeOK) & difftime(DateObs, Birth_mov, units="days") <= 365, "JU",
                          ifelse(is.na(IdadeOK) & difftime(DateObs, Birth_mov, units="days") > 365 & difftime(DateObs, Birth_mov, units="days") <= 1095, "SA",
                                 ifelse(is.na(IdadeOK) & difftime(DateObs, Birth_mov, units="days") > 1095, "AD",
                                        IdadeOK))))
# CASE 1 : individual known as adults at least once
sub_AD = n %>% 
  group_by(GLT) %>%
  filter(all(c("AD", NA) %in% IdadeOK)) %>% 
  filter(IdadeOK == "AD" | is.na(IdadeOK)) %>% 
  mutate(RefDate = first(DateObs[!is.na(IdadeOK)])) %>% 
  ungroup() # Extract the earlier date where IdadeOK is known
sub_AD = sub_AD %>% 
  group_by(GLT) %>% 
  arrange(GLT, DateObs) %>%
  mutate(diff = DateObs - RefDate,
         diff_days = as.numeric(diff, units = 'days')) %>% 
  mutate(diff_days = ifelse(is.na(diff_days), 0, diff_days)) # Compute the time difference between the reference date and the observation date
sub_AD = sub_AD %>% 
  filter(!grepl("^-", diff_days)) %>% 
  mutate(Idade.b = "AD") %>% 
  filter(is.na(IdadeOK) & !is.na(Idade.b)) %>% 
  as.data.table() # Assign "Adult" to every observation
setDT(n)[sub_AD, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset
# CASE 2 : individuals known as juveniles at least once
sub_JU = n %>% 
  group_by(GLT) %>%
  filter(all(c("JU", NA) %in% IdadeOK)) %>% 
  filter(IdadeOK == "JU" | is.na(IdadeOK)) %>% 
  mutate(RefDate = first(DateObs[!is.na(IdadeOK)])) %>% 
  ungroup() # Extract the earlier date where IdadeOK is known
sub_JU = sub_JU %>% 
  group_by(GLT) %>% 
  arrange(GLT, DateObs) %>%
  mutate(diff = DateObs - RefDate,
         diff_days = as.numeric(diff, units = 'days')) %>% 
  mutate(diff_days = ifelse(is.na(diff_days), 0, diff_days)) # Compute the time difference between the reference date and the observation date
sub_JU = sub_JU %>% 
  filter(grepl("^-", diff_days)) %>% 
  mutate(Idade.b = "JU") %>% 
  filter(is.na(IdadeOK) & !is.na(Idade.b)) %>% 
  as.data.table() # Assign "Juv" to every observation
setDT(n)[sub_JU, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset
# CASE 3 : individuals observed 3 years after being noted as juveniles
sub_JU = n %>% 
  group_by(GLT) %>%
  filter(all(c("JU", NA) %in% IdadeOK)) %>% 
  filter(IdadeOK == "JU" | is.na(IdadeOK)) %>% 
  mutate(RefDate = first(DateObs[!is.na(IdadeOK)])) %>% 
  ungroup() # Extract the earlier date where IdadeOK is known
sub_JU = sub_JU %>% 
  group_by(GLT) %>% 
  arrange(GLT, DateObs) %>%
  mutate(diff = DateObs - RefDate,
         diff_days = as.numeric(diff, units = 'days')) %>% 
  mutate(diff_days = ifelse(is.na(diff_days), 0, diff_days)) # Compute the time difference between the reference date and the observation date
sub_JU = sub_JU %>% 
  mutate(Idade.b = ifelse(diff_days > 1095, "AD", IdadeOK)) %>% 
  filter(Idade.b == "AD") %>% 
  filter(is.na(IdadeOK) & !is.na(Idade.b)) %>% 
  as.data.table() # Assign "Adult" to every observation
setDT(n)[sub_JU, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset


## Remaining errors
# Check Idade incoherences according to the weight
rf.Idade = n %>% 
  filter(IdadeOK == "JU" & Weight > 400) %>% 
  mutate(Idade.b = "AD")
setDT(n)[rf.Idade, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset
rf.Idade = n %>% 
  filter((IdadeOK == "AD" & Weight < 400)) %>% 
  mutate(Idade.b = "JU")
setDT(n)[rf.Idade, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset


# Check Idade incoherences through time
rf.Idade = n %>% 
  mutate(IdadeNum = ifelse(IdadeOK == "JU", 1,
                          ifelse(IdadeOK =="SA", 2,
                                 ifelse(IdadeOK =="AD", 3,
                                        NA))))
rf.Idade = rf.Idade %>%
  group_by(GLT) %>% 
  arrange(ObsOrder) %>% 
  mutate(Diff = IdadeNum - lag(IdadeNum)) %>% 
  filter(! is.na(Diff)) %>% 
  summarise(Increasing = all(Diff >= 0))
rf.Idade = rf.Idade %>% 
  filter(Increasing == FALSE) %>% 
  distinct(GLT) %>% 
  unlist()
idade.checks = n %>% 
  filter(GLT %in% rf.Idade) %>% 
  select(rowid, GLT, Tattoo, Group, DateObs, ObsOrder, Weight, Birth_VR, Birth_mov, BirthOK, Birth_inferred, Idade, IdadeOK) %>% 
  arrange(GLT, ObsOrder) %>% 
  filter(GLT!="?") %>% 
  filter(GLT!="IN") %>% 
  filter(GLT!="T0")
# write.xlsx(idade.checks, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/idade_checks_v1.xlsx", row.names=FALSE)

## Corrected Idade via manual inspection
# First inspection
idade.checked1 = read_excel("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/idade_checked_v1.xlsx",
                     na="NA")
idade.checked1 = as.data.table(idade.checked1)
corrections = idade.checked1 %>% 
  filter(IdadeOK != Idade_correct) %>% 
  filter(!is.na(Idade_correct)) %>% 
  select(rowid, GLT, Idade_correct)
setDT(n)[corrections, "IdadeOK" := .(Idade_correct), on = "rowid"] # Correct the dataset
# Second inspection
idade.checked2 = read_excel("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/idade_checked_v2.xlsx",
                            na="NA")
idade.checked2 = as.data.table(idade.checked2)
corrections = idade.checked2 %>% 
  filter(IdadeOK != Idade_correct) %>% 
  filter(!is.na(Idade_correct)) %>% 
  select(rowid, GLT, Idade_correct)
setDT(n)[corrections, "IdadeOK" := .(Idade_correct), on = "rowid"] # Correct the dataset
# Third inspection
idade.checked3 = read_excel("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/idade_checked_v3.xlsx",
                            na="NA")
idade.checked3 = as.data.table(idade.checked3)
corrections = idade.checked3 %>% 
  filter(IdadeOK != Idade_correct) %>% 
  filter(!is.na(Idade_correct)) %>% 
  select(rowid, GLT, Idade_correct)
setDT(n)[corrections, "IdadeOK" := .(Idade_correct), on = "rowid"] # Correct the dataset

## Subset remaining uncertain rows
# From the local file manually corected
errors.index = idade.checked1 %>% 
  filter(is.na(Idade_correct)) %>% 
  select(-c(Idade_correct,BirthOK))
errors.index = idade.checked2 %>%
  filter(is.na(Idade_correct)) %>% 
  select(-c(Idade_correct,BirthOK)) %>% 
  union(errors.index) %>% 
  arrange(GLT, ObsOrder)
errors.index = idade.checked3 %>%
  filter(is.na(Idade_correct)) %>% 
  select(-c(Idade_correct,BirthOK)) %>% 
  union(errors.index) %>% 
  arrange(GLT, ObsOrder)
errors.index %>% summarise(n=n_distinct(GLT))
# write.xlsx(errors.index, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/red_flags_idade1.xlsx", row.names=FALSE)
# From a comparison between new pointed out errors and the last local correction
# The remaining rows are new errors that need to be checked
idade.checks4 = idade.checks %>%
  subset(!(GLT %in% errors.index$GLT)) %>% 
  subset(!(GLT %in% idade.checked1$GLT)) %>% 
  subset(!(GLT %in% idade.checked2$GLT)) %>% 
  subset(!(GLT %in% idade.checked3$GLT))
# write.xlsx(idade.checks3, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/idade_checks_v3.xlsx", row.names=FALSE)



## Jointure
data.clean = data.clean %>% 
  left_join(select(n,c(rowid,BirthOK,IdadeOK)))
# Check remaining NAs
sub = data.clean %>% 
  group_by(GLT) %>% 
  filter(any(is.na(IdadeOK) & any(!is.na(IdadeOK)))) %>% 
  select(c(rowid,GLT,DateObs,ObsOrder,Birth_VR,Birth_mov,Weight,Idade,IdadeOK)) %>% 
  as.data.table()
# write.xlsx(sub, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/idade_checks_v4_NA.xlsx", row.names=FALSE)


## Statistical summaries
# Weight
hist(capt.data$BWeight)
x = n %>% 
  group_by(GLT, IdadeOK) %>% 
  summarise(MWeight = mean(Weight))
ggplot(filter(x, !is.na(MWeight)), aes(x = MWeight, fill = IdadeOK, colour = IdadeOK)) + 
  geom_histogram(alpha=0.7, position="identity")
