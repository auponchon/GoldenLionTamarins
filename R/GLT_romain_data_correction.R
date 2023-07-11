#Correcting error in Sex, birth dates and life stage through test and error protocol

correcting_sex_stage<-function(){

  conflicts_prefer(dplyr::arrange)
  conflicts_prefer(dplyr::first)
  conflicts_prefer(dplyr::mutate)

####### GLT data correction
### Author: Romain Monassier
### Date: 2023

##### Libraries
load.lib = c("dplyr","tibble", "data.table",
             "tidyr", "xlsx", "zoo", "readxl",
             "readr", "ggplot2", "lubridate",
             "plyr", "tidyverse")
sapply(load.lib,require,character=TRUE)


##### Load data

# Clean data
load(here::here("Data","NewlyCreatedData","clean_raw_data_long.RData"))
data.clean = as.data.table(data.clean)
#•summary(data.clean)
# Date formatting
data.clean = data.clean %>% 
  dplyr::mutate(DateObsMY = format(DateObs, "%m-%Y")) # Mutate to date format
data.clean$DateObs = as.Date(data.clean$DateObs)
data.clean = data.clean %>% 
  dplyr::mutate(
    season = case_when(
      lubridate::month(DateObs) %in% 6:8 ~ 'Dry',
      lubridate::month(DateObs) %in% c(10:12, 1:4) ~ 'Wet',
      lubridate::month(DateObs) %in% c(5, 9) ~ 'Transition'
    )
  ) # Mutate season period (according to Dietz et al. 1994)


# Capture data information
capt.data = read_excel(here::here("data","RawData","ProcessamentoMLD.xlsx"))
capt.data = capt.data %>% 
  dplyr::rename(DateObs = Date) %>% 
  dplyr::rename(Group = Grupo)
capt.data = capt.data %>% 
  dplyr::mutate(DateObsMY = format(DateObs, "%m-%Y"))
data.clean = data.clean %>% 
  dplyr::left_join(select(capt.data,c(GLT, Group, DateObs, BWeight))) %>% 
  dplyr::rename(Weight = BWeight) # Join weight

# GLT movement
GLT_mov = read_excel(here::here("data","RawData","GLT_movement.xls"),
                     na="NA")
GLT_mov = GLT_mov %>% 
  dplyr::rename(GLT = Individual) %>% 
  dplyr::rename(Tattoo = Tatoo)
GLT_mov$Estimated.date.of.emigration = as.Date(GLT_mov$Estimated.date.of.emigration, format="%d/%m/%y")

# Group location
loc_grp <- read_delim(here::here("data","NewlyCreatedData","Nb_obs_gp.csv"), delim=";", show_col_types = FALSE)

# Create a unique row id (ie observation id)
data.clean = rowid_to_column(data.clean)
data.clean$rowid = paste0("obs",data.clean$rowid)
# Mutate observation order
data.clean = data.clean %>% 
  dplyr::group_by(GLT) %>% 
  dplyr::arrange(GLT, as.Date(DateObs)) %>% 
  dplyr::mutate(ObsOrder = row_number())



##### Sex verification
## 1st ERROR : Update typing errors
data.clean = data.clean %>% 
  dplyr::mutate(SexOK = ifelse(Sex == "m", "M",
                        ifelse(Sex == "M0" | Sex == "nonID" | Sex == "S" | Sex == "NA", NA,
                               Sex)))

## 2nd ERROR : Duplicated sexes
## Duplicated sexes selection
dup.sex = data.clean %>% 
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  dplyr::select(GLT, Tattoo, Region, SexOK) %>% 
  dplyr::group_by(GLT) %>%
  dplyr::filter(n_distinct(SexOK) > 1) %>%
  dplyr::ungroup()
dup.sex %>% dplyr::summarise(n=n_distinct(GLT),
                      p=n_distinct(GLT)*100/2063)
n = dup.sex %>% 
  dplyr::group_by(GLT, SexOK) %>% 
  dplyr::summarise(n=n())

# FIRST CASE : Combinaison of one sex and NAs
sub = dup.sex %>% 
  dplyr::filter(is.na(SexOK)) %>% 
  dplyr::distinct(GLT) %>% 
  dplyr::pull() # Select GLT with NAs in sex column
errors = data.clean %>% 
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  dplyr::select(rowid, GLT, Tattoo, SexOK) %>% 
  subset(GLT %in% sub) %>% 
  dplyr::group_by(GLT) %>% 
  dplyr::arrange(GLT, SexOK) %>% 
  dplyr::mutate(SexOK = zoo::na.locf(SexOK)) %>% 
  dplyr::rename(Sex.b = SexOK) %>% 
  dplyr::ungroup() %>% 
  as.data.table() # Subset data.clean with the latter GLT, and update the NA value according to the dominant one (na.locf)
setDT(data.clean)[errors, "SexOK" := .(Sex.b), on = "GLT"] # Update the SexOK in the dataset where observations match those 


# SECOND CASE : Female + Male
dup.sex = dup.sex %>% 
  dplyr::group_by(GLT) %>% 
  dplyr::arrange(GLT, SexOK) %>% 
  dplyr::mutate(SexOK = zoo::na.locf(SexOK)) %>% 
  dplyr::filter(n_distinct(SexOK) > 1) # Remove the NAs errors from the table
n = dup.sex %>% 
  dplyr::group_by(GLT, SexOK) %>% 
  dplyr::summarise(n=n()) # Sex occurrences by GLT
n = n %>%
  pivot_wider(names_from = SexOK, values_from = n)
errors = n %>% 
  dplyr::select(F, M) %>% 
  dplyr::select(GLT) %>% 
  unlist() # Subset errors
rf.sex = data.clean %>% 
  dplyr::filter(GLT %in% errors) %>% 
  dplyr::arrange(GLT, ObsOrder) %>% 
  dplyr::select(c(rowid, GLT, Tattoo, Group, DateObs, ObsOrder, Sex, File, Weight)) %>% 
  dplyr::mutate(Sex.b = NA) # Red flags dataframe
setDT(data.clean)[rf.sex, "SexOK" := .(Sex.b), on = "GLT"] # Update the dataframe
data.clean$Sex[data.clean$GLT == "LC4"] = "F" # Manual correction after verification
data.clean$Sex[data.clean$GLT == "RT1"] = "M" # Manual correction after verification

## Export red flags
rf.sex = rf.sex %>% 
  dplyr::filter(GLT!="LC4" & GLT!="RT1")
 write.xlsx(rf.sex, here::here("data","NewlyCreatedData","Checks","sex_checkV3.xlsx"), row.names=FALSE)

# Mutate a new variable precising sex errors in the dataset
data.clean = data.clean %>% 
  dplyr::mutate(SexError = ifelse(GLT %in% rf.sex$GLT, "Duplicated", "None")) %>% 
  dplyr::mutate(SexError = ifelse(is.na(SexOK), NA, SexError))



### Age and life stage verification

## Load birth Dates from 1989 to 2001
BirthDates = read_excel(here::here("data","NewlyCreatedData","Checks","BirthDates.xlsx"),
                        na="NA")
BirthDates = BirthDates[!duplicated(BirthDates$GLT), ] # Remove duplicated GLT
data.clean = data.clean %>% 
  dplyr::left_join(select(BirthDates, c(GLT, Birth_VR))) # Check typing errors
data.clean = data.clean %>% 
  dplyr::mutate(Birth_VR = ifelse(Birth_VR == "?", NA,
                           Birth_VR))
data.clean$Birth_VR = as.Date(data.clean$Birth_VR, format="%d/%m/%y")

## Load birth dates post-2001 (source = GLT_mov)
data.clean = data.clean %>% 
  dplyr::rename(Birth_mov = Birth)
data.clean$Birth_mov = as.Date(data.clean$Birth_mov, format="%d/%m/%y")

n = data.clean %>%
  dplyr::select(c(rowid, DateObs, ObsOrder, GLT, Tattoo, 
                  Group, Idade, Weight, Birth_VR, Birth_mov)) # Create a copy of the dataset

# Manual corrections
n$Birth_VR[n$GLT == "RT13"] = "2006-03-01"
n$Birth_VR[n$GLT == "RT14"] = "2006-03-01"


## Check Birth dates errors

# 1st ERROR  : remove duplicates in the Birth_mov dates
# SOLUTION : manual inspection and correction
dup.birth = n %>% 
  dplyr::select(GLT, Birth_mov) %>% 
  dplyr::group_by(GLT) %>%
  dplyr::filter(!is.na(Birth_mov)) %>% 
  dplyr::filter(n_distinct(Birth_mov) > 1) %>%
  ungroup() # Select duplicated birth dates (Birth_mov variable)
dup.birth = dup.birth %>% 
  dplyr::group_by(GLT, Birth_mov) %>% 
  dplyr::summarise(n=n()) %>% 
  ungroup()
dup.birth = dup.birth %>% 
  dplyr::group_by(GLT) %>% 
  dplyr::mutate(Births = paste0(Birth_mov, collapse = " or ")) %>% 
  dplyr::distinct(GLT,Births) %>% 
  ungroup()
dup.birth = dup.birth %>% 
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>%  
  separate(Births, c("opt1", "opt2", "opt3"), " or ") # Unfold birth dates options for each GLT

# Create subsets according to the number of different birth dates
# A) Two different dates
dup.birth2 = dup.birth %>% 
  dplyr::filter(is.na(opt3)) %>% 
  dplyr::select(1:3) %>% 
  as.data.table()
dup.birth2$opt1 = as.Date(x = dup.birth2$opt1, format="%Y-%m-%d")
dup.birth2$opt2 = as.Date(x = dup.birth2$opt2, format="%Y-%m-%d")
dup.birth2 = dup.birth2 %>% 
  dplyr::mutate(diffBirth = abs(difftime(opt1, opt2, units="days"))) # Difference between two dates
dup.birth2 = dup.birth2 %>% 
  dplyr::mutate(Birth.b = ifelse(diffBirth <= 180, opt1, NA)) # NA Birth date when the difference is > 180 days
dup.birth2$Birth.b = as.Date(dup.birth2$Birth.b)
setDT(n)[dup.birth2, "Birth_mov" := .(Birth.b), on = "GLT"] # Correct the dataset
errors.index = dup.birth2 %>% 
  dplyr::filter(is.na(Birth.b)) %>% 
  dplyr::select(GLT) # List of errors
# B) Three different dates
dup.birth3 = dup.birth %>% 
  dplyr::filter(!is.na(opt3)) %>% 
  dplyr::select(1:4) %>% 
  as.data.table()
dup.birth3$opt1 = as.Date(x = dup.birth3$opt1, format="%Y-%m-%d")
dup.birth3$opt2 = as.Date(x = dup.birth3$opt2, format="%Y-%m-%d")
dup.birth3$opt3 = as.Date(x = dup.birth3$opt3, format="%Y-%m-%d")
# Manual correction
n$Birth_mov[n$GLT == "1375"] <- NA
n$Birth_mov[n$GLT == "AF20"] <- "2011-12-01"
n$Birth_mov[n$GLT == "E25"] <- "1999-10-01"
n$Birth_mov[n$GLT == "KE10"] <- "2002-09-01"
n$Birth_mov[n$GLT == "KE4"] <- "2000-10-01"
n$Birth_mov[n$GLT == "RV6"] <- "1999-10-10"
n$Birth_mov[n$GLT == "SP23"] <- "2011-11-01"
errors.index = dup.birth3 %>% 
  dplyr::filter(GLT=="1375") %>% 
  dplyr::select(GLT) %>% 
  bind_rows(errors.index) %>% 
  dplyr::mutate(error_type="multiple_BD") # Add to the list of errors


# 2nd ERROR : Incoherences between source birth dates
# SOLUTION : keep the more reliable birth date (given by VR)
errors = n %>% 
  dplyr::mutate(diff.b1.b2 = abs(difftime(Birth_mov, Birth_VR, units="days")))
n = n %>% 
  dplyr::group_by(GLT) %>% 
  dplyr::mutate(BirthOK = ifelse(!is.na(Birth_VR) & !is.na(Birth_mov), Birth_VR,
                              ifelse(is.na(Birth_VR) & !is.na(Birth_mov), Birth_mov,
                                     Birth_VR))) %>% 
  ungroup() # Create a BirthOK variable where the birth is the one given by Valeria if several options, Birth_mov otherwise
n$BirthOK = as.Date(n$BirthOK)
sub = n %>% 
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>%
  dplyr::mutate(Birth.b = ifelse(!is.na(BirthOK), "Date", NA)) %>% 
  dplyr::group_by(GLT) %>% 
  dplyr::filter(all(c("Date", NA) %in% Birth.b)) %>% 
  ungroup() %>% 
  dplyr::distinct(GLT) %>% 
  pull() 
errors = n %>% 
  dplyr::filter(GLT %in% sub) %>% 
  dplyr::group_by(GLT) %>% 
  dplyr::mutate(Birth.b = ifelse(is.na(BirthOK), zoo::na.locf(BirthOK), BirthOK)) %>% 
  ungroup() %>% 
  as.data.table() # Fill BirthOK when there are NA rows
errors$Birth.b = as.Date(errors$Birth.b)
setDT(n)[errors, c("BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# 3rd ERROR : comparison between the estimated date of emigration and the Birth Date
# SOLUTION : NA
errors = n %>% 
  dplyr::left_join(select(GLT_mov,c(GLT, Estimated.date.of.emigration))) %>% 
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0")
errors = errors %>% 
  dplyr::mutate(diff.emig.birth = difftime(Estimated.date.of.emigration, BirthOK, units="days")) # Compute time difference
rf.birthdates = errors[grepl("^-", errors$diff.emig.birth), ] # Select negative values
rf.birthdates = rf.birthdates %>% 
  dplyr::mutate(Birth.b = NA) %>% 
  dplyr::distinct(GLT, .keep_all = TRUE) %>% 
  dplyr::mutate(error_type="diff_BD_Emig") # Mutate NA
errors.index = errors.index %>% 
  union(select(rf.birthdates,c(GLT, error_type))) # Add to the list of errors
setDT(n)[rf.birthdates, c("BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# 4th ERROR : differences between birth dates and dates of first observation
# SOLUTION : NA
errors = n %>% 
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  dplyr::filter(ObsOrder == 1) %>%  
  dplyr::group_by(GLT) %>% 
  dplyr::mutate(diff.obs.birth = difftime(DateObs, BirthOK, units="days")) %>% 
  ungroup() %>% 
  as.data.table()
rf.birthdates = errors %>% 
  dplyr::filter(grepl("^-", errors$diff.obs.birth)) # Negative values, ie individuals observed before their birth
rf.birthdates = rf.birthdates %>% 
  dplyr::mutate(Birth.b = NA) %>% 
  dplyr::mutate(error_type = "diff_BD_1stObs") # Mutate NA
errors.index = errors.index %>% 
  union(select(rf.birthdates,c(GLT, error_type))) # Add to the list of errors
setDT(n)[rf.birthdates, c("BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# 5th ERROR : individuals whose birth date (from Birth_mov) = date of first observation
# SOLUTION : NA
errors = n %>% 
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  dplyr::select(c(rowid, DateObs, ObsOrder, GLT, Tattoo, Group, Idade, Birth_VR, Birth_mov, BirthOK, Weight)) %>% 
  dplyr::filter(!is.na(Birth_mov) & is.na(Birth_VR)) %>% 
  dplyr::filter(ObsOrder == 1) %>% 
  dplyr::filter(DateObs == BirthOK) %>% 
  distinct(GLT, .keep_all = TRUE) %>% 
  dplyr::mutate(Birth.b = NA) # Select errors
rf.birthdates = errors %>% 
  distinct(GLT, .keep_all = TRUE) %>% 
  dplyr::mutate(error_type = "1stObs_is_BD") # Mutate NA
errors.index = errors.index %>% 
  union(select(rf.birthdates,c(GLT, error_type))) # Add to the list of errors
setDT(n)[errors, c("BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# 6th ERROR : individuals within groups with same birth dates
# SOLUTION : Mutate NA if >= 3 individuals within groups with same bith dates
errors = n %>%
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>%
  dplyr::filter(!is.na(BirthOK) & !is.na(Group)) %>%
  distinct(GLT, .keep_all = TRUE) %>% 
  dplyr::group_by(Group, BirthOK) %>%
  dplyr::filter(n()>=3) %>%
  dplyr::mutate(n=n()) %>% 
  dplyr::arrange(Group, BirthOK) %>%
  as.data.table() %>% 
  dplyr::mutate(Birth.b = NA)
rf.birthdates = errors %>% 
  dplyr::mutate(error_type = ifelse(n == 3 | n == 4, "Grp_sameBD_?P",
                             "Grp_sameBD"))
errors.index = errors.index %>% 
  union(select(rf.birthdates,c(GLT, error_type))) # Add to the list of errors
setDT(n)[errors, c("BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# 7th ERROR : individuals first observed as adults with known birth dates
# SOLUTION : NA
errors = n %>%
  dplyr::filter(Idade == "AD" & ObsOrder == 1 & !is.na(BirthOK)) %>%
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>%
  mutate(Birth.b = NA)
rf.birthdates = errors %>%
  distinct(GLT, .keep_all = TRUE) %>%
  dplyr::mutate(error_type = "Adult_with_BD")
errors.index = errors.index %>%
  union(select(rf.birthdates,c(GLT, error_type))) # Add to the list of errors
setDT(n)[errors, c("BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


### Export birth dates errors
errors.index = errors.index[, list(error_type = paste(error_type, collapse="+")), by = GLT] # Paste multiple errors for same individuals
data.clean = data.clean %>% 
  dplyr::left_join(errors.index) %>% 
  dplyr::rename(BirthError = error_type) %>% 
  dplyr::mutate(BirthError = ifelse(is.na(BirthError), "None", BirthError)) %>% 
  dplyr::mutate(BirthError = ifelse(GLT %in% "?" | GLT %in% "IN" | GLT %in% "FT" | GLT %in% "T0", "UnknownGLT", BirthError)) # Join the errors types to the dataset
rf.birthdates = data.clean %>% 
  dplyr::filter(GLT %in% errors.index$GLT) %>% 
  dplyr::select(c(rowid, GLT, Tattoo, Group, Region, DateObs, ObsOrder, Birth_mov, Birth_VR, Idade, Weight, File, BirthError)) %>% 
  arrange(GLT, ObsOrder) # Create the red flags dataset
 write.xlsx(rf.birthdates, here::here("data","NewlyCreatedData","Checks","red_flags_birthdates1.xlsx"), row.names=FALSE)



### Implementing the Idade (Age categories) column
## A) From the correct birth date
n = n %>% 
  dplyr::mutate(IdadeOK = ifelse(difftime(n$DateObs, n$BirthOK, units="days") <= 60, "IN",
                         ifelse(difftime(n$DateObs, n$BirthOK, units="days") > 60 & difftime(n$DateObs, n$BirthOK, units="days") <= 300, "JU",
                                ifelse(difftime(n$DateObs, n$BirthOK, units="days") > 300 & difftime(n$DateObs, n$BirthOK, units="days") <= 540, "SA",
                                       ifelse(difftime(n$DateObs, n$BirthOK, units="days") > 540, "AD", 
                                              NA)))))

## B) Using the Idade existing field
n$Idade = as.character(n$Idade)
n = n %>% 
  dplyr::mutate(IdadeOK = ifelse(is.na(IdadeOK), Idade, IdadeOK))

## C) Using the GLT weight
n = n %>% 
  dplyr::mutate(IdadeOK = ifelse(is.na(IdadeOK) & Weight > 550, "AD",
                          IdadeOK))

# Append the Idade variable and fill
# CASE 1 : individual known as adults at least once
sub = n %>% 
  dplyr::group_by(GLT) %>%
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  dplyr::filter(IdadeOK=="AD") %>% 
  dplyr::mutate(RefDate = first(DateObs[IdadeOK=="AD"])) %>% 
  ungroup() %>% 
  dplyr::select(GLT,RefDate) %>% 
  distinct() # Extract the earlier date where IdadeOK is known
sub_AD = n %>% 
  dplyr::left_join(sub) %>% 
  dplyr::arrange(GLT, DateObs) %>%
  dplyr::filter(!is.na(RefDate)) %>% 
  dplyr::mutate(diff = DateObs - RefDate,
         diff_days = as.numeric(diff, units = 'days')) # Compute the time difference between the reference date and the observation date
sub_AD = sub_AD %>% 
  dplyr::filter(!grepl("^-", diff_days) & (IdadeOK=="SA" | is.na(IdadeOK))) %>% 
  dplyr::mutate(Idade.b = "AD") %>%
  as.data.table() # Assign "Adult" to every observation
setDT(n)[sub_AD, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset
# CASE 2 : individuals known as juveniles at least once
sub = n %>% 
  dplyr::group_by(GLT) %>%
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  dplyr::filter(IdadeOK=="JU") %>% 
  dplyr::mutate(RefDate = first(DateObs[IdadeOK=="JU"])) %>% 
  ungroup() %>% 
  dplyr::select(GLT,RefDate) %>% 
  distinct() # Extract the earlier date where IdadeOK is known
sub_JU = n %>% 
  dplyr::left_join(sub) %>% 
  dplyr::arrange(GLT, DateObs) %>%
  dplyr::filter(!is.na(RefDate)) %>% 
  dplyr::mutate(diff = DateObs - RefDate,
         diff_days = as.numeric(diff, units = 'days')) # Compute the time difference between the reference date and the observation date
sub_JU = sub_JU %>% 
  dplyr::filter(grepl("^-", diff_days) & (IdadeOK=="SA" | is.na(IdadeOK))) %>% 
  dplyr::mutate(Idade.b = "JU") %>% 
  as.data.table() # Assign "Juv" to every observation
setDT(n)[sub_JU, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset
# CASE 3 : juveniles observed 16 months (480 days) after being noted as juveniles (= adults)
sub_JU = n %>% 
  dplyr::group_by(GLT) %>%
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  dplyr::filter(all(c("JU", NA) %in% IdadeOK)) %>% 
  dplyr::filter(IdadeOK == "JU" | is.na(IdadeOK)) %>% 
  dplyr::mutate(RefDate = first(DateObs[!is.na(IdadeOK)])) %>% 
  ungroup() # Extract the earlier date where IdadeOK is known
sub_JU = sub_JU %>% 
  dplyr::group_by(GLT) %>% 
  dplyr::arrange(GLT, DateObs) %>%
  dplyr::mutate(diff = DateObs - RefDate,
         diff_days = as.numeric(diff, units = 'days')) %>% 
  dplyr::mutate(diff_days = ifelse(is.na(diff_days), 0, diff_days)) # Compute the time difference between the reference date and the observation date
sub_JU = sub_JU %>% 
  dplyr::mutate(Idade.b = ifelse(diff_days > 480, "AD", IdadeOK)) %>% 
  dplyr::filter(Idade.b == "AD") %>% 
  as.data.table() # Assign "Adult" to every observation
setDT(n)[sub_JU, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset
# CASE 4 : sub-adults observed 8 months (240 days) after being noted as sub-adults (= adults)
sub_SA = n %>% 
  dplyr::group_by(GLT) %>%
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  dplyr::filter(all(c("SA", NA) %in% IdadeOK)) %>% 
  dplyr::filter(IdadeOK == "SA" | is.na(IdadeOK)) %>% 
  dplyr::mutate(RefDate = first(DateObs[!is.na(IdadeOK)])) %>% 
  ungroup() # Extract the earlier date where IdadeOK is known
sub_SA = sub_SA %>% 
  dplyr::group_by(GLT) %>% 
  arrange(GLT, DateObs) %>%
  dplyr::mutate(diff = DateObs - RefDate,
         diff_days = as.numeric(diff, units = 'days')) %>% 
  dplyr::mutate(diff_days = ifelse(is.na(diff_days), 0, diff_days)) # Compute the time difference between the reference date and the observation date
sub_SA = sub_SA %>% 
  dplyr::mutate(Idade.b = ifelse(diff_days > 240, "AD", IdadeOK)) %>% 
  dplyr::filter(Idade.b == "AD") %>% 
  as.data.table() # Assign "Adult" to every observation
setDT(n)[sub_SA, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset


## Remaining errors
# 1st ERROR : Age categories incoherences according to the weight
rf.Idade = n %>% 
  dplyr::filter(is.na(Birth_VR)) %>%  
  dplyr::filter(IdadeOK != "AD" & Weight > 550) %>% 
  dplyr::mutate(Idade.b = "AD")
errors.index = rf.Idade %>% 
  dplyr::select(GLT) %>% 
  dplyr::mutate(error_type = "Weight") # List of errors

# 2nd ERROR : Inconstencies between age categories different variables
rf.Idade = n %>% 
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  dplyr::filter(Idade!=IdadeOK) %>% 
  dplyr::distinct(GLT) %>% 
  mutate(error_type = "Diff_Idade")
errors.index = errors.index %>% 
  union(select(rf.Idade,c(GLT, error_type))) # Add to the list of errors

# 3st ERROR : Age categories incoherences through time
rf.Idade = n %>% 
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  dplyr::mutate(IdadeNum = ifelse(IdadeOK == "IN", 1,
                          ifelse(IdadeOK =="JU", 2,
                                 ifelse(IdadeOK =="SA", 3,
                                        ifelse(IdadeOK =="AD", 4,
                                               NA)))))
rf.Idade = rf.Idade %>%
  dplyr::group_by(GLT) %>% 
  dplyr::arrange(ObsOrder) %>% 
  dplyr::mutate(Diff = IdadeNum - dplyr::lag(IdadeNum)) %>% 
  dplyr::filter(! is.na(Diff)) %>% 
  dplyr::summarise(Increasing = all(Diff >= 0))
rf.Idade = rf.Idade %>% 
  dplyr::filter(Increasing == FALSE) %>% 
  distinct(GLT) %>% 
  dplyr::mutate(error_type = "Chronology")
errors.index = errors.index %>% 
  union(select(rf.Idade,c(GLT, error_type))) %>% 
  as.data.table() # Add to the list of errors
# ERRORS
errors.index = errors.index[, list(error_type = paste(error_type, collapse="+")), by = GLT] # Paste multiple errors for same individuals


# Export of red flags
sub = c("AF4","AX22","AX23","AX24","BI1","DI2","DN1","EL2","FG1","IR1",
                 "IR2","JO1","LS1","MB8","MB9","MD1","MD2","NC1","O21","OL30",
                 "PA5","PR1","RL11","RT15","RV34","SD1","SD4","SF9","W21","CM15",
                 "JP23","TM6","TM7")
rf.Idade = errors.index %>% 
  dplyr::filter(GLT %in% sub)
cols = c("IdadeOK","BirthOK")
n[n$GLT %in% rf.Idade, cols] <- NA
rf.Idade = data.clean %>% 
  dplyr::filter(GLT %in% sub) %>% 
  dplyr::select(c(rowid, GLT, Tattoo, Group, Region, DateObs, ObsOrder, Birth_mov, Birth_VR, Idade, Weight, File)) %>% 
  dplyr::left_join(rf.Idade) %>% 
  arrange(GLT, ObsOrder)
 write.xlsx(rf.Idade, here::here("data","NewlyCreatedData","Checks","red_flags_idade1.xlsx"), row.names=FALSE)

# Remaining errors after the first filter
idade.checks = errors.index %>% 
  dplyr::filter(!(GLT %in% sub))
idade.checks = n %>% 
  dplyr::filter(GLT %in% idade.checks$GLT) %>% 
  dplyr::select(rowid, GLT, Tattoo, Group, DateObs, ObsOrder, Weight, Birth_VR, Birth_mov, BirthOK, Idade, IdadeOK) %>% 
  dplyr::left_join(idade.checks) %>%
  dplyr::arrange(GLT, ObsOrder) %>% 
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="T0" & GLT!="FT") %>% 
  dplyr::filter(error_type!="Diff_Idade")
 write.xlsx(idade.checks, here::here("data","NewlyCreatedData","Checks","idade_checks_v2.xlsx"), row.names=FALSE)


## Corrected Idade via manual inspection
# Corrections
idade.checked1 = read_excel(here::here("data","NewlyCreatedData","Checks","idade_checked_v1.xlsx"),
                            na="NA")
idade.checked1 = as.data.table(idade.checked1)
corrections = idade.checked1 %>% 
  dplyr::filter(IdadeOK != IdadeCorrect) %>% 
  dplyr::filter(!is.na(IdadeCorrect)) %>% 
  dplyr::select(rowid, GLT, IdadeCorrect)
setDT(n)[corrections, "IdadeOK" := .(IdadeCorrect), on = "rowid"] # Correct the dataset
# The remaining rows are new errors that need to be checked
idade.checks = idade.checks %>%
  subset(!(GLT %in% corrections$GLT))


## Join the idade error to the dataset
data.clean.final = data.clean %>% 
  dplyr::left_join(select(n,c(rowid,BirthOK,IdadeOK))) %>% 
  dplyr::left_join(select(rf.Idade,c(rowid,error_type))) %>% 
  dplyr::rename(IdadeError = error_type) %>% 
  dplyr::mutate(IdadeError = ifelse(is.na(IdadeError), "None", IdadeError)) %>% 
  dplyr::mutate(IdadeError = ifelse(GLT %in% "?" | GLT %in% "IN" | GLT %in% "FT" | GLT %in% "T0", "UnknownGLT", IdadeError))

 return(data.clean.final)     
 
}
      