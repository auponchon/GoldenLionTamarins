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
  dplyr::mutate(DateObsMY = format(DateObs, "%m-%Y"))
data.clean$DateObs = as.Date(data.clean$DateObs)
data.clean = data.clean %>% 
  dplyr::mutate(
    season = case_when(
      lubridate::month(DateObs) %in% 6:8 ~ 'Dry',
      lubridate::month(DateObs) %in% c(10:12, 1:4) ~ 'Wet',
      lubridate::month(DateObs) %in% c(5, 9) ~ 'Transition'
    )
  ) # Season classification according to Dietz et al (1994)


# Join capture data information
capt.data = read_excel("D:/monas/Git/repo/glt/GoldenLionTamarins/data/RawData/ProcessamentoMLD.xlsx")
capt.data = capt.data %>% 
  dplyr::rename(DateObs = Date) %>% 
  dplyr::rename(Group = Grupo)
capt.data = capt.data %>% 
  dplyr::mutate(DateObsMY = format(DateObs, "%m-%Y"))
data.clean = data.clean %>% 
  dplyr::left_join(select(capt.data,c(GLT, Group, DateObs, BWeight))) %>% 
  dplyr::rename(Weight = BWeight)

# Load GLT movement
GLT_mov = read_excel("D:/monas/Git/repo/glt/GoldenLionTamarins/data/RawData/GLT_movement.xls",
                     na="NA")
GLT_mov = GLT_mov %>% 
  dplyr::rename(GLT = Individual) %>% 
  dplyr::rename(Tattoo = Tatoo)
GLT_mov$Estimated.date.of.emigration = as.Date(GLT_mov$Estimated.date.of.emigration, format="%d/%m/%y")

# Rowid
data.clean = rowid_to_column(data.clean) # Add a rowid column (observation unique id)
data.clean$rowid = paste0("obs",data.clean$rowid)

# Observation order
data.clean = data.clean %>% 
  dplyr::group_by(GLT) %>% 
  dplyr::arrange(GLT, as.Date(DateObs)) %>% 
  dplyr::mutate(ObsOrder = row_number())

# Group location
loc_grp <- read_delim("data/NewlyCreatedData/Nb_obs_gp.csv", delim=";", show_col_types = FALSE)




##### Sex verification
## Update typing errors
data.clean = data.clean %>% 
  dplyr::mutate(SexOK = ifelse(Sex == "m", "M",
                        ifelse(Sex == "M0" | Sex == "nonID" | Sex == "S" | Sex == "NA", NA,
                               Sex)))

## Correct sex errors
# Select duplicated sexes
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

# IF combinaison of one sex and NAs
# Fill in NAs for individuals with another value
# Select GLT with NAs in sex column
sub = dup.sex %>% 
  dplyr::filter(is.na(SexOK)) %>% 
  dplyr::distinct(GLT) %>% 
  dplyr::pull()
# Subset data.clean with the latter GLT, and update the NA value according to the dominant one (na.locf)
errors = data.clean %>% 
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  dplyr::select(rowid, GLT, Tattoo, SexOK) %>% 
  subset(GLT %in% sub) %>% 
  dplyr::group_by(GLT) %>% 
  dplyr::arrange(GLT, SexOK) %>% 
  dplyr::mutate(SexOK = zoo::na.locf(SexOK)) %>% 
  dplyr::rename(Sex.b = SexOK) %>% 
  dplyr::ungroup() %>% 
  as.data.table()
# Update the SexOK in the dataset where observations match those 
setDT(data.clean)[errors, "SexOK" := .(Sex.b), on = "GLT"]


# IF Female + Male
# Remove the NAs errors from the table
dup.sex = dup.sex %>% 
  dplyr::group_by(GLT) %>% 
  dplyr::arrange(GLT, SexOK) %>% 
  dplyr::mutate(SexOK = zoo::na.locf(SexOK)) %>% 
  dplyr::filter(n_distinct(SexOK) > 1)
n = dup.sex %>% 
  dplyr::group_by(GLT, SexOK) %>% 
  dplyr::summarise(n=n())
n = n %>%
  pivot_wider(names_from = SexOK, values_from = n)
errors = n %>% 
  dplyr::select(F, M) %>% 
  dplyr::select(GLT) %>% 
  unlist()
# Red flags
rf.sex = data.clean %>% 
  dplyr::filter(GLT %in% errors) %>% 
  dplyr::arrange(GLT, ObsOrder) %>% 
  dplyr::select(c(rowid, GLT, Tattoo, Group, DateObs, ObsOrder, Sex, File, Weight)) %>% 
  dplyr::mutate(Sex.b = NA)
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
  dplyr::filter(GLT!="LC4" & GLT!="RT1")
# write.xlsx(rf.sex, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/sex_checkV3.xlsx", row.names=FALSE)

# Mutate a new variable precising sex errors in the dataset
data.clean = data.clean %>% 
  dplyr::mutate(SexError = ifelse(GLT %in% rf.sex$GLT, "Duplicated", "None")) %>% 
  dplyr::mutate(SexError = ifelse(is.na(SexOK), NA, SexError))



### Age and life stage verification

## Birth Dates from 1989 to 2001
BirthDates = read_excel("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/BirthDates.xlsx",
                        na="NA")
# Remove duplicated rows (GLT)
BirthDates = BirthDates[!duplicated(BirthDates$GLT), ]
# Check typing errors
data.clean = data.clean %>% 
  dplyr::left_join(select(BirthDates, c(GLT, Birth_VR)))
data.clean %>% 
  dplyr::group_by(Birth_VR) %>%
  dplyr::summarise(no_rows = length(Birth_VR)) %>% 
  print(n=200)
data.clean = data.clean %>% 
  dplyr::mutate(Birth_VR = ifelse(Birth_VR == "?", NA,
                           Birth_VR))
data.clean$Birth_VR = as.Date(data.clean$Birth_VR, format="%d/%m/%y")

## Birth dates post-2001
# Source : GLT_mov
data.clean = data.clean %>% 
  dplyr::rename(Birth_mov = Birth)
data.clean %>%
  dplyr::group_by(Birth_mov) %>%
  dplyr::summarise(no_rows = length(Birth_mov)) %>%
  print(n=350)
data.clean$Birth_mov = as.Date(data.clean$Birth_mov, format="%d/%m/%y")

# Copy dataset
n = data.clean %>%
  dplyr::select(c(rowid, DateObs, ObsOrder, GLT, Tattoo, Group, Idade, Weight, Birth_VR, Birth_mov))

# Manual corrections from below inspection
n$Birth_VR[n$GLT == "RT13"] = "2006-03-01"
n$Birth_VR[n$GLT == "RT14"] = "2006-03-01"


## Check Birth dates errors
# 1th ERROR  : remove duplicates in the Birth_mov dates
# SOLUTION : manual inspection and correction
dup.birth = n %>% 
  dplyr::select(GLT, Birth_mov) %>% 
  dplyr::group_by(GLT) %>%
  dplyr::filter(!is.na(Birth_mov)) %>% 
  dplyr::filter(n_distinct(Birth_mov) > 1) %>%
  ungroup()
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
  separate(Births, c("opt1", "opt2", "opt3"), " or ")

# Create subsets according to the number of different birth dates
# Two different dates
dup.birth2 = dup.birth %>% 
  dplyr::filter(is.na(opt3)) %>% 
  dplyr::select(1:3) %>% 
  as.data.table()
dup.birth2$opt1 = as.Date(x = dup.birth2$opt1, format="%Y-%m-%d")
dup.birth2$opt2 = as.Date(x = dup.birth2$opt2, format="%Y-%m-%d")
dup.birth2 = dup.birth2 %>% 
  dplyr::mutate(diffBirth = abs(difftime(opt1, opt2, units="days")))
dup.birth2 = dup.birth2 %>% 
  dplyr::mutate(Birth.b = ifelse(diffBirth <= 180, opt1, NA))
dup.birth2$Birth.b = as.Date(dup.birth2$Birth.b)
setDT(n)[dup.birth2, "Birth_mov" := .(Birth.b), on = "GLT"] # Correct the dataset
errors.index = dup.birth2 %>% 
  dplyr::filter(is.na(Birth.b)) %>% 
  dplyr::select(GLT)


# Three different dates
dup.birth3 = dup.birth %>% 
  dplyr::filter(!is.na(opt3)) %>% 
  dplyr::select(1:4) %>% 
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
  dplyr::filter(GLT=="1375") %>% 
  dplyr::select(GLT) %>% 
  bind_rows(errors.index) %>% 
  dplyr::mutate(error_type="multiple_BD")


# 2nd ERROR : Incoherences between source birth dates
errors = n %>% 
  dplyr::mutate(diff.b1.b2 = abs(difftime(Birth_mov, Birth_VR, units="days")))
errors %>%
  dplyr::filter(diff.b1.b2 != "0 days" & !is.na(diff.b1.b2)) %>% 
  dplyr::summarise(n_GLT=n_distinct(GLT))
# SOLUTION : keep the more reliable birth date (given by VR)
n = n %>% 
  dplyr::group_by(GLT) %>% 
  dplyr::mutate(BirthOK = ifelse(!is.na(Birth_VR) & !is.na(Birth_mov), Birth_VR,
                              ifelse(is.na(Birth_VR) & !is.na(Birth_mov), Birth_mov,
                                     Birth_VR))) %>% 
  ungroup()
n$BirthOK = as.Date(n$BirthOK)
# Check duplicates
n %>% 
  dplyr::select(GLT, BirthOK) %>% 
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  dplyr::group_by(GLT) %>%
  dplyr::filter(!is.na(BirthOK)) %>% 
  dplyr::filter(n_distinct(BirthOK) > 1) %>%
  ungroup()
# Fill BirthOK with known birth dates
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
  as.data.table()
errors$Birth.b = as.Date(errors$Birth.b)
setDT(n)[errors, c("BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# 3rd ERROR : comparison between the estimated date of emigration and the Birth Date
errors = n %>% 
  dplyr::left_join(select(GLT_mov,c(GLT, Estimated.date.of.emigration))) %>% 
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0")
errors = errors %>% 
  dplyr::mutate(diff.emig.birth = difftime(Estimated.date.of.emigration, BirthOK, units="days"))
rf.birthdates = errors[grepl("^-", errors$diff.emig.birth), ]
# SOLUTION : NA
rf.birthdates = rf.birthdates %>% 
  dplyr::mutate(Birth.b = NA) %>% 
  dplyr::distinct(GLT, .keep_all = TRUE) %>% 
  dplyr::mutate(error_type="diff_BD_Emig")
errors.index = errors.index %>% 
  union(select(rf.birthdates,c(GLT, error_type)))
setDT(n)[rf.birthdates, c("BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# 4th ERROR : differences between birth dates and dates of first observation
errors = n %>% 
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  dplyr::filter(ObsOrder == 1) %>%  
  dplyr::group_by(GLT) %>% 
  dplyr::mutate(diff.obs.birth = difftime(DateObs, BirthOK, units="days")) %>% 
  ungroup() %>% 
  as.data.table()
# Negative values
rf.birthdates = errors %>% 
  dplyr::filter(grepl("^-", errors$diff.obs.birth)) # individuals observed before their birth
# SOLUTION : NA
rf.birthdates = rf.birthdates %>% 
  dplyr::mutate(Birth.b = NA) %>% 
  dplyr::mutate(error_type = "diff_BD_1stObs")
errors.index = errors.index %>% 
  union(select(rf.birthdates,c(GLT, error_type)))
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
  dplyr::mutate(Birth.b = NA)
rf.birthdates = errors %>% 
  distinct(GLT, .keep_all = TRUE) %>% 
  dplyr::mutate(error_type = "1stObs_is_BD")
errors.index = errors.index %>% 
  union(select(rf.birthdates,c(GLT, error_type)))
setDT(n)[errors, c("BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# 6th ERROR : individuals within groups with same birth dates
# N_obs_gp = read_delim("data/NewlyCreatedData/Nb_obs_gp.csv", delim=";")
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
  union(select(rf.birthdates,c(GLT, error_type)))
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
  union(select(rf.birthdates,c(GLT, error_type)))
setDT(n)[errors, c("BirthOK") := .(Birth.b), on = "GLT"] # Correct the dataset


# Export birth dates errors
errors.index = errors.index[, list(error_type = paste(error_type, collapse="+")), by = GLT] # Paste multiple errors for same individuals
data.clean = data.clean %>% 
  dplyr::left_join(errors.index) %>% 
  dplyr::rename(BirthError = error_type) %>% 
  dplyr::mutate(BirthError = ifelse(is.na(BirthError), "None", BirthError)) %>% 
  dplyr::mutate(BirthError = ifelse(GLT %in% "?" | GLT %in% "IN" | GLT %in% "FT" | GLT %in% "T0", "UnknownGLT", BirthError))
rf.birthdates = data.clean %>% 
  dplyr::filter(GLT %in% errors.index$GLT) %>% 
  dplyr::select(c(rowid, GLT, Tattoo, Group, Region, DateObs, ObsOrder, Birth_mov, Birth_VR, Idade, Weight, File, BirthError)) %>% 
  arrange(GLT, ObsOrder)
rf.birthdates %>% dplyr::summarise(n_GLT=n_distinct(GLT))
# write.xlsx(rf.birthdates, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/red_flags_birthdates1.xlsx", row.names=FALSE)



### Implementing the Idade column

## From the correct birth date
n = n %>% 
  dplyr::mutate(IdadeOK = ifelse(difftime(n$DateObs, n$BirthOK, units="days") <= 60, "IN",
                         ifelse(difftime(n$DateObs, n$BirthOK, units="days") > 60 & difftime(n$DateObs, n$BirthOK, units="days") <= 300, "JU",
                                ifelse(difftime(n$DateObs, n$BirthOK, units="days") > 300 & difftime(n$DateObs, n$BirthOK, units="days") <= 540, "SA",
                                       ifelse(difftime(n$DateObs, n$BirthOK, units="days") > 540, "AD", 
                                              NA)))))

## Using the Idade existing field
n$Idade = as.character(n$Idade)
n = n %>% 
  dplyr::mutate(IdadeOK = ifelse(is.na(IdadeOK), Idade, IdadeOK))

## Using the GLT weight
n = n %>% 
  dplyr::mutate(IdadeOK = ifelse(is.na(IdadeOK) & Weight > 550, "AD",
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
# Check Idade incoherences according to the weight
rf.Idade = n %>% 
  dplyr::filter(is.na(Birth_VR)) %>%  
  dplyr::filter(IdadeOK != "AD" & Weight > 550) %>% 
  dplyr::mutate(Idade.b = "AD")
errors.index = rf.Idade %>% 
  dplyr::select(GLT) %>% 
  dplyr::mutate(error_type = "Weight")

# Check inconstencies between Idade variables
rf.Idade = n %>% 
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="FT" & GLT!="T0") %>% 
  dplyr::filter(Idade!=IdadeOK) %>% 
  dplyr::distinct(GLT) %>% 
  mutate(error_type = "Diff_Idade")
errors.index = errors.index %>% 
  union(select(rf.Idade,c(GLT, error_type)))

# Check Idade incoherences through time
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
  dplyr::mutate(Diff = IdadeNum - lag(IdadeNum)) %>% 
  dplyr::filter(! is.na(Diff)) %>% 
  dplyr::summarise(Increasing = all(Diff >= 0))
rf.Idade = rf.Idade %>% 
  dplyr::filter(Increasing == FALSE) %>% 
  distinct(GLT) %>% 
  dplyr::mutate(error_type = "Chronology")
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
  dplyr::filter(GLT %in% sub)
cols = c("IdadeOK","BirthOK")
n[n$GLT %in% rf.Idade, cols] <- NA
# Red flags
rf.Idade = data.clean %>% 
  dplyr::filter(GLT %in% sub) %>% 
  dplyr::select(c(rowid, GLT, Tattoo, Group, Region, DateObs, ObsOrder, Birth_mov, Birth_VR, Idade, Weight, File)) %>% 
  dplyr::left_join(rf.Idade) %>% 
  arrange(GLT, ObsOrder)
rf.Idade %>% dplyr::summarise(n_GLT=n_distinct(GLT))
# write.xlsx(rf.Idade, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/red_flags_idade1.xlsx", row.names=FALSE)

# Remaining checks
idade.checks = errors.index %>% 
  dplyr::filter(!(GLT %in% sub))
idade.checks = n %>% 
  dplyr::filter(GLT %in% idade.checks$GLT) %>% 
  dplyr::select(rowid, GLT, Tattoo, Group, DateObs, ObsOrder, Weight, Birth_VR, Birth_mov, BirthOK, Idade, IdadeOK) %>% 
  dplyr::left_join(idade.checks) %>%
  dplyr::arrange(GLT, ObsOrder) %>% 
  dplyr::filter(GLT!="?" & GLT!="IN" & GLT!="T0" & GLT!="FT") %>% 
  dplyr::filter(error_type!="Diff_Idade")
# write.xlsx(idade.checks, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/idade_checks_v2.xlsx", row.names=FALSE)


## Corrected Idade via manual inspection
# Corrections
idade.checked1 = read_excel("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/idade_checked_v1.xlsx",
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



## Jointure
data_clean_v2 = data.clean %>% 
  dplyr::left_join(select(n,c(rowid,BirthOK,IdadeOK))) %>% 
  dplyr::left_join(select(rf.Idade,c(rowid,error_type))) %>% 
  dplyr::rename(IdadeError = error_type) %>% 
  dplyr::mutate(IdadeError = ifelse(is.na(IdadeError), "None", IdadeError)) %>% 
  dplyr::mutate(IdadeError = ifelse(GLT %in% "?" | GLT %in% "IN" | GLT %in% "FT" | GLT %in% "T0", "UnknownGLT", IdadeError))
# save(data_clean_v2, file="D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/data_clean_v2.RData")






### ANALYSIS
# Load dataset
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/data_clean_v2.RData")
# Load monitored groups
Nb_obs_gp = read_delim("data/NewlyCreatedData/Nb_obs_gp.csv", delim=";", show_col_types = FALSE)
Nb_obs_gp %>% 
  dplyr::summarise(across(c("UMMPs","Farm","Fragment.Region"), ~ n_distinct(.x, na.rm = TRUE)))
Nb_obs_gp %>% 
  dplyr::group_by(UMMPs,Fragment.Region) %>% 
  dplyr::summarise(n_grp = n_distinct(Group)) %>% 
  print(n=50)
# Join the fragment names
data_clean_v2 = data_clean_v2 %>% 
  left_join(select(Nb_obs_gp,c(Group,UMMPs,Farm,Lat,Long,Fragment.Region)))

## STATISTICAL SUMMARY
sub = data_clean_v2 %>% 
  dplyr::group_by(Group) %>% 
  dplyr::filter(!is.na(Group)) %>%
  dplyr::mutate(Monit_Years_grp = diff(range(Year))) %>% 
  dplyr::mutate(Monit_1stYear_grp = min(Year), 
         Monit_LastYear_grp = max(Year)) %>%
  dplyr::mutate(Monit_Period_grp = paste0(unique(c(Monit_1stYear_grp, Monit_LastYear_grp)), collapse = '-')) %>%
  ungroup() %>% 
  as.data.frame()
sub = sub %>% 
  dplyr::group_by(Fragment.Region) %>% 
  dplyr::filter(!is.na(Fragment.Region)) %>%
  dplyr::mutate(Monit_Years_frag = diff(range(Year))) %>% 
  dplyr::mutate(Monit_1stYear_frag = min(Year), 
                Monit_LastYear_frag = max(Year)) %>%
  dplyr::mutate(Monit_Period_frag = paste0(unique(c(Monit_1stYear_frag, Monit_LastYear_frag)), collapse = '-')) %>%
  ungroup() %>% 
  as.data.frame()


## Summary statistics
sub %>% 
  dplyr::summarise_at(c("UMMPs","Fragment.Region","Group","GLT"), n_distinct, na.rm = TRUE)
# By period
sub %>%
  dplyr::filter(any(Monit_1stYear_frag < 2002 & Monit_LastYear_frag > 2018)) %>% 
  #dplyr::filter(Monit_1stYear_frag < 2002 & Monit_LastYear_frag > 2018) %>% 
  dplyr::filter(!is.na(Fragment.Region)) %>% 
  dplyr::summarise_at(c("Fragment.Region","Group","GLT"), n_distinct, na.rm = TRUE)
sub %>%
  dplyr::filter(!is.na(Fragment.Region)) %>% 
  dplyr::filter(Monit_Years_grp >= 6) %>% 
  dplyr::summarise_at(c("Fragment.Region","Group","GLT"), n_distinct, na.rm = TRUE)
sub %>% 
  dplyr::group_by(Monit_Years_grp) %>% 
  dplyr::summarise(n_frag=n_distinct(Fragment.Region),
                   n_grp=n_distinct(Group),
                   n_GLT=n_distinct(GLT)) %>% 
  print(n=100)

# Groups within fragments monitored the same amount of time
sub %>% 
  dplyr::filter(!is.na(Fragment.Region)) %>% 
  dplyr::group_by(Fragment.Region) %>% 
  dplyr::summarise(n_grp = n_distinct(Group), 
                   n_grp_1stDate = n_distinct(Group[Monit_1stYear_grp < 2002 ]),
                   n_grp_LastDate = n_distinct(Group[Monit_LastYear_grp > 2018 ]))



# Plot monitored periods
# By fragment
g <- sub[order(sub$Monit_1stYear_frag,sub$Fragment.Region),]
g$Fragment.Region <- factor(g$Fragment.Region, levels=unique(g$Fragment.Region))
g = g %>% dplyr::filter(!is.na(Fragment.Region))
ggplot(g, aes(x = Monit_1stYear_frag, y = Fragment.Region)) +
  geom_segment(aes(xend = Monit_LastYear_frag, yend = Fragment.Region), colour = "orange") +
  geom_point(size = 2, colour="darkorange") +
  geom_point(aes(x = Monit_LastYear_frag), size = 2, colour="darkorange") +
  theme_bw() +
  theme(legend.position = "none",
        text = element_text(size=10))
# By group
# Work on a subset (a number of fragments)
sub_frag = sub %>% 
  dplyr::group_by(Fragment.Region) %>% 
  dplyr::arrange(Fragment.Region, Monit_1stYear_grp) %>% 
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
  facet_grid(Fragment.Region ~ ., scales = "free_y", space = "free_y", switch = "y") +
  theme(strip.placement = "outside")
# Work on a subset (named fragments)
g = sub %>% 
  dplyr::filter(Fragment.Region == "Afetiva" | Fragment.Region =="Afetiva SP")
g <- g[order(g$Monit_1stYear_grp,g$Group),]
g$Group <- factor(g$Group, levels=unique(g$Group))
ggplot(g, aes(x = Monit_1stYear_grp, y = Group)) +
  geom_segment(aes(xend = Monit_LastYear_grp, yend = Group), colour = "orange") +
  geom_point(size = 2, colour="darkorange") +
  geom_point(aes(x = Monit_LastYear_grp), size = 2, colour="darkorange") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_grid(Fragment.Region ~ ., scales = "free_y", space = "free_y", switch = "y") +
  theme(strip.placement = "outside")


# Subset the sample for demographic analysis
# Doc if_any et if_all : https://stackoverflow.com/questions/70216129/filter-rows-by-groups-where-all-values-are-na
# With the monitoring period
analysis.data = sub %>% 
  dplyr::filter(!is.na(UMMPs)) %>%
  dplyr::filter(!is.na(Group)) %>% 
  dplyr::group_by(Group) %>% 
  dplyr::filter(Monit_Years_grp >= 6) %>% 
  ungroup() %>% 
  as.data.table()
analysis.data %>% 
  dplyr::summarise(n=n(),
                   n_mu = n_distinct(UMMPs),
                   n_frag=n_distinct(Fragment.Region),
                   n_grp = n_distinct(Group), 
                   tot_GLT = n_distinct(GLT))

## Group size
# Statistical summary
sub = analysis.data %>% 
  dplyr::group_by(UMMPs,Group,Year) %>% 
  dplyr::summarise(Grp_size = n_distinct(GLT)) %>% 
  dplyr::mutate(Growth_rate = Grp_size/lag(Grp_size))
ggplot(sub,aes(x = Year, y = Grp_size)) + 
  geom_point() +
  geom_line(data=sub[!is.na(sub$Grp_size),], aes(color=Group)) +
  facet_wrap( ~ UMMPs)


#® Composition
analysis.data %>% 
  dplyr::group_by(Group,GLT) %>% 
  dplyr::summarise(n_rows = n(),
                   count_na = sum(is.na(IdadeOK) | is.na(SexOK)),
                   prop_na=count_na*100/n_rows)