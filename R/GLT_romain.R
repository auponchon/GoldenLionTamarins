####### GLT data curing
### Author: Romain Monassier
### Date: 2023

##### Libraries
load.lib = c("dplyr","tibble", "data.table",
             "tidyr", "xlsx", "zoo", "readxl",
             "readr", "ggplot2", "lubridate",
             "plyr")
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
  filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
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
  filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
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

# Mutate a new variable precising sex errors in the dataset
data.clean = data.clean %>% 
  mutate(SexError = ifelse(GLT %in% rf.sex$GLT, "Duplicated", "None")) %>% 
  mutate(SexError = ifelse(is.na(SexOK), NA, SexError))



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
  filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>%  
  separate(Births, c("opt1", "opt2", "opt3"), " or ")

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
  filter(!is.na(opt3)) %>% 
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


# 2nd ERROR : Incoherences between source birth dates
errors = n %>% 
  mutate(diff.b1.b2 = abs(difftime(Birth_mov, Birth_VR, units="days")))
errors %>%
  filter(diff.b1.b2 != "0 days" & !is.na(diff.b1.b2)) %>% 
  summarise(n_GLT=n_distinct(GLT))
# SOLUTION : keep the more reliable birth date (given by VR)
n = n %>% 
  group_by(GLT) %>% 
  mutate(BirthOK = ifelse(!is.na(Birth_VR) & !is.na(Birth_mov), Birth_VR,
                              ifelse(is.na(Birth_VR) & !is.na(Birth_mov), Birth_mov,
                                     Birth_VR))) %>% 
  ungroup()
n$BirthOK = as.Date(n$BirthOK)
# Check duplicates
n %>% 
  select(GLT, BirthOK) %>% 
  filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  group_by(GLT) %>%
  filter(!is.na(BirthOK)) %>% 
  filter(n_distinct(BirthOK) > 1) %>%
  ungroup()
# Fill BirthOK with known birth dates
sub = n %>% 
  filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>%
  mutate(Birth.b = ifelse(!is.na(BirthOK), "Date", NA)) %>% 
  group_by(GLT) %>% 
  filter(all(c("Date", NA) %in% Birth.b)) %>% 
  ungroup() %>% 
  distinct(GLT) %>% 
  pull()
errors = n %>% 
  filter(GLT %in% sub) %>% 
  group_by(GLT) %>% 
  mutate(Birth.b = ifelse(is.na(BirthOK), zoo::na.locf(BirthOK), BirthOK)) %>% 
  ungroup() %>% 
  as.data.table()
errors$Birth.b = as.Date(errors$Birth.b)
setDT(n)[errors, c("BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# 3rd ERROR : comparison between the estimated date of emigration and the Birth Date
errors = n %>% 
  left_join(select(GLT_mov,c(GLT, Estimated.date.of.emigration))) %>% 
  filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0")
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
setDT(n)[rf.birthdates, c("BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# 4th ERROR : differences between birth dates and dates of first observation
errors = n %>% 
  filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
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
setDT(n)[rf.birthdates, c("BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# 5th ERROR : individuals whose birth date (from Birth_mov) = date of first observation
# SOLUTION : NA
errors = n %>% 
  filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  select(c(rowid, DateObs, ObsOrder, GLT, Tattoo, Group, Idade, Birth_VR, Birth_mov, BirthOK, Weight)) %>% 
  filter(!is.na(Birth_mov) & is.na(Birth_VR)) %>% 
  filter(ObsOrder == 1) %>% 
  filter(DateObs == BirthOK) %>% 
  distinct(GLT, .keep_all = TRUE) %>% 
  mutate(Birth.b = NA)
rf.birthdates = errors %>% 
  distinct(GLT, .keep_all = TRUE) %>% 
  mutate(error_type = "1stObs_is_BD")
errors.index = errors.index %>% 
  union(select(rf.birthdates,c(GLT, error_type)))
setDT(n)[errors, c("BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# 6th ERROR : individuals within groups with same birth dates
# N_obs_gp = read_delim("data/NewlyCreatedData/Nb_obs_gp.csv", delim=";")
errors = n %>%
  filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>%
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
setDT(n)[errors, c("BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# 7th ERROR : individuals first observed as adults with known birth dates
# SOLUTION : NA
errors = n %>%
  filter(Idade == "AD" & ObsOrder == 1 & !is.na(BirthOK)) %>%
  filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>%
  mutate(Birth.b = NA)
rf.birthdates = errors %>%
  distinct(GLT, .keep_all = TRUE) %>%
  mutate(error_type = "Adult_with_BD")
errors.index = errors.index %>%
  union(select(rf.birthdates,c(GLT, error_type)))
setDT(n)[errors, c("BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# Export birth dates errors
errors.index = errors.index[, list(error_type = paste(error_type, collapse="+")), by = GLT] # Paste multiple errors for same individuals
data.clean = data.clean %>% 
  left_join(errors.index) %>% 
  rename(BirthError = error_type) %>% 
  mutate(BirthError = ifelse(is.na(BirthError), "None", BirthError)) %>% 
  mutate(BirthError = ifelse(GLT %in% "?" | GLT %in% "IN" | GLT %in% "FT" | GLT %in% "T0", "UnknownGLT", BirthError))
rf.birthdates = data.clean %>% 
  filter(GLT %in% errors.index$GLT) %>% 
  select(c(rowid, GLT, Tattoo, Group, Region, DateObs, ObsOrder, Birth_mov, Birth_VR, Idade, Weight, File, BirthError)) %>% 
  arrange(GLT, ObsOrder)
rf.birthdates %>% summarise(n_GLT=n_distinct(GLT))
# write.xlsx(rf.birthdates, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/red_flags_birthdates1.xlsx", row.names=FALSE)



### Implementing the Idade column

## From the correct birth date
n = n %>% 
  mutate(IdadeOK = ifelse(difftime(n$DateObs, n$BirthOK, units="days") <= 60, "IN",
                         ifelse(difftime(n$DateObs, n$BirthOK, units="days") > 60 & difftime(n$DateObs, n$BirthOK, units="days") <= 300, "JU",
                                ifelse(difftime(n$DateObs, n$BirthOK, units="days") > 300 & difftime(n$DateObs, n$BirthOK, units="days") <= 540, "SA",
                                       ifelse(difftime(n$DateObs, n$BirthOK, units="days") > 540, "AD", 
                                              NA)))))

## Using the Idade existing field
n$Idade = as.character(n$Idade)
n = n %>% 
  mutate(IdadeOK = ifelse(is.na(IdadeOK), Idade, IdadeOK))

## Using the GLT weight
n = n %>% 
  mutate(IdadeOK = ifelse(is.na(IdadeOK) & Weight > 550, "AD",
                          IdadeOK))

# ## Using an inferred birth date
# sub = n %>%
#   filter(is.na(BirthOK)) %>%
#   filter(ObsOrder == 1 & Idade == "JU") %>%
#   mutate(Birth_inferred = ymd(DateObs)-months(5))
# n = n %>% left_join(select(sub, c(GLT, Birth_inferred)))
# n = n %>%
#   mutate(IdadeOK = ifelse(is.na(IdadeOK) & difftime(n$DateObs, n$Birth_inferred, units="days") <= 365, "JU",
#                           ifelse(is.na(IdadeOK) & difftime(n$DateObs, n$Birth_inferred, units="days") > 365 & difftime(n$DateObs, n$Birth_inferred, units="days") <= 1095, "SA",
#                                  ifelse(is.na(IdadeOK) & difftime(n$DateObs, n$Birth_inferred, units="days") > 1095, "AD",
#                                         IdadeOK))))


# Append the Idade variable and fill
# CASE 1 : individual known as adults at least once
sub = n %>% 
  group_by(GLT) %>%
  filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  filter(IdadeOK=="AD") %>% 
  mutate(RefDate = first(DateObs[IdadeOK=="AD"])) %>% 
  ungroup() %>% 
  select(GLT,RefDate) %>% 
  distinct() # Extract the earlier date where IdadeOK is known
sub_AD = n %>% 
  left_join(sub) %>% 
  arrange(GLT, DateObs) %>%
  filter(!is.na(RefDate)) %>% 
  mutate(diff = DateObs - RefDate,
         diff_days = as.numeric(diff, units = 'days')) # Compute the time difference between the reference date and the observation date
sub_AD = sub_AD %>% 
  filter(!grepl("^-", diff_days) & (IdadeOK=="SA" | is.na(IdadeOK))) %>% 
  mutate(Idade.b = "AD") %>%
  as.data.table() # Assign "Adult" to every observation
setDT(n)[sub_AD, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset
# CASE 2 : individuals known as juveniles at least once
sub = n %>% 
  group_by(GLT) %>%
  filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  filter(IdadeOK=="JU") %>% 
  mutate(RefDate = first(DateObs[IdadeOK=="JU"])) %>% 
  ungroup() %>% 
  select(GLT,RefDate) %>% 
  distinct() # Extract the earlier date where IdadeOK is known
sub_JU = n %>% 
  left_join(sub) %>% 
  arrange(GLT, DateObs) %>%
  filter(!is.na(RefDate)) %>% 
  mutate(diff = DateObs - RefDate,
         diff_days = as.numeric(diff, units = 'days')) # Compute the time difference between the reference date and the observation date
sub_JU = sub_JU %>% 
  filter(grepl("^-", diff_days) & (IdadeOK=="SA" | is.na(IdadeOK))) %>% 
  mutate(Idade.b = "JU") %>% 
  as.data.table() # Assign "Juv" to every observation
setDT(n)[sub_JU, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset
# CASE 3 : juveniles observed 16 months (480 days) after being noted as juveniles (= adults)
sub_JU = n %>% 
  group_by(GLT) %>%
  filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
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
# CASE 4 : sub-adults observed 8 months (240 days) after being noted as sub-adults (= adults)
sub_SA = n %>% 
  group_by(GLT) %>%
  filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
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


## Remaining errors
# Check Idade incoherences according to the weight
rf.Idade = n %>% 
  filter(is.na(Birth_VR)) %>%  
  filter(IdadeOK != "AD" & Weight > 550) %>% 
  mutate(Idade.b = "AD")
errors.index = rf.Idade %>% 
  select(GLT) %>% 
  mutate(error_type = "Weight")

# Check inconstencies between Idade variables
rf.Idade = n %>% 
  filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  filter(Idade!=IdadeOK) %>% 
  distinct(GLT) %>% 
  mutate(error_type = "Diff_Idade")
errors.index = errors.index %>% 
  union(select(rf.Idade,c(GLT, error_type)))

# Check Idade incoherences through time
rf.Idade = n %>% 
  filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  mutate(IdadeNum = ifelse(IdadeOK == "IN", 1,
                          ifelse(IdadeOK =="JU", 2,
                                 ifelse(IdadeOK =="SA", 3,
                                        ifelse(IdadeOK =="AD", 4,
                                               NA)))))
rf.Idade = rf.Idade %>%
  group_by(GLT) %>% 
  arrange(ObsOrder) %>% 
  mutate(Diff = IdadeNum - lag(IdadeNum)) %>% 
  filter(! is.na(Diff)) %>% 
  summarise(Increasing = all(Diff >= 0))
rf.Idade = rf.Idade %>% 
  filter(Increasing == FALSE) %>% 
  distinct(GLT) %>% 
  mutate(error_type = "Chronology")
errors.index = errors.index %>% 
  union(select(rf.Idade,c(GLT, error_type))) %>% 
  as.data.table()

# ERRORS
errors.index = errors.index[, list(error_type = paste(error_type, collapse="+")), by = GLT] # Paste multiple errors for same individuals
# Red flags
sub = c("AF4","AX22","AX23","AX24","BI1","DI2","DN1","EL2","FG1","IR1",
                 "IR2","JO1","LS1","MB8","MB9","MD1","MD2","NC1","O21","OL30",
                 "PA5","PR1","RL11","RT15","RV34","SD1","SD4","SF9","W21","CM15",
                 "JP23","TM6","TM7")
rf.Idade = errors.index %>% 
  filter(GLT %in% sub)
cols = c("IdadeOK","BirthOK")
n[n$GLT %in% rf.Idade, cols] <- NA
# Red flags
rf.Idade = data.clean %>% 
  filter(GLT %in% sub) %>% 
  select(c(rowid, GLT, Tattoo, Group, Region, DateObs, ObsOrder, Birth_mov, Birth_VR, Idade, Weight, File)) %>% 
  left_join(rf.Idade) %>% 
  arrange(GLT, ObsOrder)
rf.Idade %>% summarise(n_GLT=n_distinct(GLT))
# write.xlsx(rf.Idade, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/red_flags_idade1.xlsx", row.names=FALSE)

# Remaining checks
idade.checks = errors.index %>% 
  filter(!(GLT %in% sub))
idade.checks = n %>% 
  filter(GLT %in% idade.checks$GLT) %>% 
  select(rowid, GLT, Tattoo, Group, DateObs, ObsOrder, Weight, Birth_VR, Birth_mov, BirthOK, Idade, IdadeOK) %>% 
  left_join(idade.checks) %>%
  arrange(GLT, ObsOrder) %>% 
  filter(GLT!="?" & GLT!="IN" & GLT!="T0") %>% 
  filter(error_type!="Diff_Idade")
# write.xlsx(idade.checks, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/idade_checks_v2.xlsx", row.names=FALSE)


## Corrected Idade via manual inspection
# Corrections
idade.checked1 = read_excel("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/idade_checked_v1.xlsx",
                            na="NA")
idade.checked1 = as.data.table(idade.checked1)
corrections = idade.checked1 %>% 
  filter(IdadeOK != IdadeCorrect) %>% 
  filter(!is.na(IdadeCorrect)) %>% 
  select(rowid, GLT, IdadeCorrect)
setDT(n)[corrections, "IdadeOK" := .(IdadeCorrect), on = "rowid"] # Correct the dataset
# The remaining rows are new errors that need to be checked
idade.checks = idade.checks %>%
  subset(!(GLT %in% corrections$GLT))




## Jointure
data_clean_v2 = data.clean %>% 
  left_join(select(n,c(rowid,BirthOK,IdadeOK))) %>% 
  left_join(select(rf.Idade,c(rowid,error_type))) %>% 
  rename(IdadeError = error_type) %>% 
  mutate(IdadeError = ifelse(is.na(IdadeError), "None", IdadeError)) %>% 
  mutate(IdadeError = ifelse(GLT %in% "?" | GLT %in% "IN" | GLT %in% "FT" | GLT %in% "T0", "UnknownGLT", IdadeError))
# save(data_clean_v2, file="D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/data_clean_v2.RData")
# Check remaining NAs
sub = data.clean %>% 
  filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  group_by(GLT) %>% 
  filter(any(is.na(IdadeOK) & any(!is.na(IdadeOK)))) %>% 
  select(c(rowid,GLT,DateObs,ObsOrder,Birth_VR,Birth_mov,BirthOK,Weight,Idade,IdadeOK)) %>% 
  as.data.table()
# write.xlsx(sub, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/idade_checks_v4_NA.xlsx", row.names=FALSE)





### Statistical summaries
# Load dataset
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/data_clean_v2.RData")
sub = data_clean_v2 %>% 
  group_by(Group) %>% 
  filter(!is.na(Group)) %>%
  mutate(Monit_Years = diff(range(Year))) %>% 
  mutate(Monit_1stYear = min(Year), 
         Monit_LastYear = max(Year)) %>%
  mutate(Monit_Period = paste0(unique(c(Monit_1stYear, Monit_LastYear)), collapse = '-')) %>%
  ungroup()

## Summary statistics
# Number of monitored Region/Groups/GLT per period
sub %>% 
  group_by(Monit_Period) %>% 
  summarise(n_reg=n_distinct(Region),
            n_grp=n_distinct(Group),
            n_GLT=n_distinct(GLT)) %>% 
  print(n=100)

# Number of NA and not-NA rows
sub %>% 
  group_by(Monit_Period, Group, GLT) %>% 
  summarise(na=sum(is.na(IdadeOK)), not_na=sum(!is.na(IdadeOK)))

# Number of NA and not-NA GLT by group/region
sub %>% 
  group_by(Monit_Period, Group) %>% 
  summarise(tot_GLT = n_distinct(GLT),
            notNA_GLT=n_distinct(GLT[all(!is.na(IdadeOK) & !is.na(SexOK))]),
            NA_GLT=n_distinct(GLT[any(is.na(IdadeOK) | is.na(SexOK))]),
            prop=notNA_GLT*100/tot_GLT)
sub %>% 
  group_by(Region, Group, Monit_Period) %>% 
  summarise(tot_GLT = n_distinct(GLT),
            notNA_GLT=n_distinct(GLT[all(!is.na(IdadeOK) & !is.na(SexOK))]),
            NA_GLT=n_distinct(GLT[any(is.na(IdadeOK) | is.na(SexOK))])) %>% 
  arrange(Region, Monit_Period) %>% 
  print(n=400)

# Plot monitored periods
# By region
g <- sub[order(sub$Monit_1stYear,sub$Region),]
g$Region <- factor(g$Region, levels=unique(g$Region))
ggplot(g, aes(x = Monit_1stYear, y = Region)) +
  geom_segment(aes(xend = Monit_LastYear, yend = Region), colour = "orange") +
  geom_point(size = 2, colour="darkorange") +
  geom_point(aes(x = Monit_LastYear), size = 2, colour="darkorange") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=7))
# By group
# Work on a subset (a number of regions)
sub_region = sub %>% 
  group_by(Region) %>% 
  arrange(Region, Monit_1stYear) %>% 
  group_split() # Split the dataset by Region
g = ldply(sub_region[c(1:13)], data.frame) 
g <- g[order(g$Monit_1stYear,g$Group),]
g$Group <- factor(g$Group, levels=unique(g$Group))
ggplot(g, aes(x = Monit_1stYear, y = Group)) +
  geom_segment(aes(xend = Monit_LastYear, yend = Group), colour = "orange") +
  geom_point(size = 2, colour="darkorange") +
  geom_point(aes(x = Monit_LastYear), size = 2, colour="darkorange") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_grid(Region ~ ., scales = "free_y", space = "free_y", switch = "y") +
  theme(strip.placement = "outside")
# Work on a subset (named regions)
g = sub %>% 
  filter(Region == "Iguapé" | Region =="Igarapé")
g <- g[order(g$Monit_1stYear,g$Group),]
g$Group <- factor(g$Group, levels=unique(g$Group))
ggplot(g, aes(x = Monit_1stYear, y = Group)) +
  geom_segment(aes(xend = Monit_LastYear, yend = Group), colour = "orange") +
  geom_point(size = 2, colour="darkorange") +
  geom_point(aes(x = Monit_LastYear), size = 2, colour="darkorange") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_grid(Region ~ ., scales = "free_y", space = "free_y", switch = "y") +
  theme(strip.placement = "outside")

# Regions monitored over a selected period of time
sub %>% 
  filter(Monit_1stYear <= 2002 & Monit_LastYear >= 2019) %>% 
  group_by(Region) %>% 
  summarise(n_grp = n_distinct(Group), 
            tot_GLT = n_distinct(GLT),
            notNA_GLT=n_distinct(GLT[all(!is.na(IdadeOK) & !is.na(SexOK))]),
            NA_GLT=n_distinct(GLT[any(is.na(IdadeOK) | is.na(SexOK))]),
            prop=notNA_GLT*100/tot_GLT)

# Groups within  region monitored the same amount of time
sub %>% 
  group_by(Region) %>% 
  summarise(n_grp = n_distinct(Group), 
            n_grp_1stDate = n_distinct(Group[Monit_1stYear <=2002 ]),
            n_grp_LastDate = n_distinct(Group[Monit_LastYear >=2017 ])) %>% 
  mutate(AllGrpMon = ifelse(n_grp_1stDate==n_grp & n_grp_LastDate==n_grp, "OK", "Not_OK")) %>% 
  print(n=80)

# Select regions with groups monitored the same amount of time
sub2 = sub %>% 
  filter(Monit_1stYear <= 2003) %>%
  group_by(Region, Group, Monit_Period) %>% 
  summarise(tot_GLT = n_distinct(GLT),
            notNA_GLT=n_distinct(GLT[all(!is.na(IdadeOK) & !is.na(SexOK))]),
            NA_GLT=n_distinct(GLT[any(is.na(IdadeOK) | is.na(SexOK))]),
            prop=notNA_GLT*100/tot_GLT) %>% 
  ungroup()
  

