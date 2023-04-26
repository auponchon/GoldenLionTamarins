####### GLT data curing
### Author: Romain Monassier
### Date: 2023

### Libraries
load.lib = c("dplyr","tibble", "data.table",
             "tidyr", "xlsx", "zoo", "readxl",
             "readr", "ggplot2",)
sapply(load.lib,require,character=TRUE)

### Data
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/clean_raw_data_long.RData")
data.clean = as.data.table(data.clean)
summary(data.clean)
data.clean = data.clean %>% 
  mutate(DateObsMY = format(DateObs, "%m-%Y"))
data.clean$DateObs = as.Date(data.clean$DateObs)
Date2period <- function(x, period = 6, sep = " S") {
  ym <- as.yearmon(x)
  paste(as.integer(ym), (cycle(ym) - 1) %/% period + 1, sep = sep)
}
period <- 6
data.clean <- data.clean %>% 
  mutate(DateObsSemester = Date2period(DateObs))


# Join capture data information
capt.data = read_excel("D:/monas/Git/repo/glt/GoldenLionTamarins/data/RawData/ProcessamentoMLD.xlsx")
capt.data = capt.data %>% 
  rename(DateObs = Date) %>% 
  rename(Group = Grupo)
capt.data = capt.data %>% 
  mutate(DateObsMY = format(DateObs, "%m-%Y")) %>% 
  mutate(DateObsSemester = Date2period(DateObs))
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

### Sex verification
# Update typing errors
data.clean = data.clean %>% 
  mutate(SexOK = ifelse(Sex == "m", "M",
                        ifelse(Sex == "M0" | Sex == "nonID" | Sex == "S", NA,
                               Sex)))

# Select individuals with duplicated sexes
dup.sex = data.clean %>% 
  select(GLT, SexOK) %>% 
  group_by(GLT) %>%
  filter(n_distinct(SexOK) > 1) %>%
  ungroup()
n = dup.sex %>% 
  group_by(GLT, SexOK) %>% 
  summarise(n=n())

## Fill in NAs for individuals with another value
# Select GLT with NAs in sex column
sub = dup.sex %>% 
  filter(is.na(SexOK)) %>% 
  select(GLT) %>% 
  pull()
# Subset data.clean with the latter GLT, and update the NA value according to the dominant one (na.locf)
errors = data.clean %>% 
  select(rowid, GLT, SexOK) %>% 
  subset(GLT %in% sub) %>% 
  group_by(GLT) %>% 
  arrange(GLT, SexOK) %>% 
  mutate(SexOK = zoo::na.locf(SexOK)) %>% 
  rename(Sex.b = SexOK) %>% 
  ungroup() %>% 
  as.data.table()
# Update the SexOK in the dataset where observations match those 
setDT(data.clean)[errors, "SexOK" := .(Sex.b), on = "GLT"]


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

# Locate significant uncertainties
# Compute the difference in mentions of both sexes
# Significant uncertainties < 7
n = n %>% 
  mutate(SexOK = ifelse(abs(F-M) < 7,"?","OK")) %>% 
  column_to_rownames("GLT")
 # Select the uncertainties
errors = n %>% 
  filter(SexOK == "?") %>% 
  select(F, M) %>% 
  rownames_to_column(var="GLT") %>% 
  select(GLT) %>% 
  unlist()
# Red flags
rf.sex = data.clean %>% 
  filter(GLT %in% errors) %>% 
  arrange(GLT)
# Sex correction
data.clean$SexOK[data.clean$GLT == "LC4"] <- "F"
data.clean$SexOK[data.clean$GLT == "O16"] <- NA
data.clean$SexOK[data.clean$GLT == "PR15"] <- NA

# Assign the dominant sex for the individuals for which the difference is above 7 and deemed non significant
n_ok = n %>% 
  filter(SexOK == "OK") %>% 
  select(F, M)
n_ok$Sex.b <- apply(n_ok, 1, function(x) paste0(names(n_ok)[x == max(x)])) # For each individual, we implement the most-mentioned sex
n_ok = n_ok %>% 
  rownames_to_column(var="GLT") %>% 
  select(c("GLT","Sex.b"))
setDT(data.clean)[n_ok, "SexOK" := .(Sex.b), on = "GLT"] # Correct the dataset

# Manually select uncertain rows based on visual inspection
rf.sex = data.clean %>% 
  filter(GLT == "RL11" | GLT == "RL13" | GLT == "SF3" | GLT == "SF4" | GLT == "SK19") %>% 
  mutate(Sex.b = NA)
setDT(data.clean)[rf.sex, "SexOK" := .(Sex.b), on = "GLT"] # Correct the dataset


## Export errors 
# write.xlsx(rf.sex, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/sex_checkV2.xlsx", row.names=FALSE)



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
# Filter NA data
n1 = data.clean %>%
  select(c(rowid, DateObs, ObsOrder, GLT, Tattoo, Group, Idade, Weight, Birth_VR)) %>%
  filter(!is.na(Birth_VR) | !is.na(Idade) | !is.na(Weight))
n1 = n1 %>% 
  mutate(Birth_VR = ifelse(Birth_VR == "?", NA,
                           Birth_VR))
n1$Birth_VR = as.Date(n1$Birth_VR, format="%d/%m/%y")


## Birth dates post-2001
# Source : GLT_mov
data.clean = data.clean %>% 
  rename(Birth_mov = Birth)
data.clean %>%
  group_by(Birth_mov) %>%
  summarise(no_rows = length(Birth_mov)) %>%
  print(n=350)
n2 = data.clean %>%
  select(c(rowid, DateObs, ObsOrder, GLT, Tattoo, Group, Idade, Weight, Birth_mov)) %>%
  filter(!is.na(Birth_mov) | !is.na(Idade) | !is.na(Weight))
# n2$Birth_mov = paste0('01/', n2$Birth_mov)
n2$Birth_mov = as.Date(n2$Birth_mov, format="%d/%m/%y")

## Merge both subsets of dates
n = n1 %>% 
  right_join(n2)
# Manual corrections from below inspection
n$Birth_VR[n$GLT == "RT13"] = "2006-03-01"
n$Birth_VR[n$GLT == "RT14"] = "2006-03-01"
n$Birth_VR[n$GLT == "CA9"] <- NA
n$Birth_VR[n$GLT == "SK19"] <- NA
n$Birth_mov[n$GLT == "SK19"] <- NA
n$Birth_mov[n$GLT == "1375"] <- NA
n$Birth_mov[n$GLT == "AF20"] <- "2011-12-01"
n$Birth_mov[n$GLT == "E25"] <- "1999-10-01"
n$Birth_mov[n$GLT == "KE10"] <- "2002-09-01"
n$Birth_mov[n$GLT == "KE4"] <- "2000-10-01"
n$Birth_mov[n$GLT == "RV6"] <- "1999-10-10"
n$Birth_mov[n$GLT == "SP23"] <- "2011-11-01"
n$Birth_mov[n$GLT == "FT"] <- NA
n$Birth_mov[n$GLT == "IN"] <- NA
n$Birth_mov[n$GLT == "T0"] <- NA
GLT_mov$Estimated.date.of.emigration[GLT_mov$GLT == "A7"] <- "1996-05-01"


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
# Three different dates
dup.birth3 = dup.birth %>% 
  filter(is.na(opt4) & !is.na(opt3)) %>% 
  select(1:4) %>% 
  as.data.table()
dup.birth3$opt1 = as.Date(x = dup.birth3$opt1, format="%Y-%m-%d")
dup.birth3$opt2 = as.Date(x = dup.birth3$opt2, format="%Y-%m-%d")
dup.birth3$opt3 = as.Date(x = dup.birth3$opt3, format="%Y-%m-%d")
# Five different dates
dup.birth5 = dup.birth %>% 
  filter(!is.na(opt5)) %>% 
  as.data.table()
dup.birth5$opt1 = as.Date(x = dup.birth5$opt1, format="%Y-%m-%d")
dup.birth5$opt2 = as.Date(x = dup.birth5$opt2, format="%Y-%m-%d")
dup.birth5$opt3 = as.Date(x = dup.birth5$opt3, format="%Y-%m-%d")
dup.birth5$opt4 = as.Date(x = dup.birth5$opt4, format="%Y-%m-%d")
dup.birth5$opt5 = as.Date(x = dup.birth5$opt5, format="%Y-%m-%d")

# 2nd ERROR : Incoherences between source birth dates
errors = n %>% 
  mutate(diff.b1.b2 = abs(difftime(Birth_mov, Birth_VR, units="days")))
errors %>%
  group_by(diff.b1.b2) %>%
  summarise(no_rows = length(diff.b1.b2), n_GLT=n_distinct(GLT)) %>%
  print(n=60)
# SOLUTION : keep the more reliable birth date (given by VR)
n = n %>% 
  mutate(Birth_merge = ifelse(!is.na(Birth_VR) & !is.na(Birth_mov), Birth_VR,
                              ifelse(is.na(Birth_VR) & !is.na(Birth_mov), Birth_mov,
                                     Birth_VR)))
n$Birth_merge = as.Date(n$Birth_merge)


# 3rd ERROR : Erroneous birth dates
# Comparison between the estimated date of emigration and the Birth Date
errors = n %>% 
  left_join(select(GLT_mov,c(GLT, Estimated.date.of.emigration)))
errors = errors %>% 
  mutate(diff.emig.birth = difftime(Estimated.date.of.emigration, Birth_merge, units="days"))
rf.birthdates = errors[grepl("^-", errors$diff.emig.birth), ]
# SOLUTION : Visual inspection and manual correction


# 4th ERROR : differences between birth dates and dates of first observation
errors = n %>% 
  group_by(GLT) %>% 
  mutate(Date1stObs = min(DateObs, na.rm = TRUE)) %>% 
  ungroup() %>% 
  as.data.table()
errors = errors %>% 
  filter(ObsOrder == 1) %>%  
  mutate(diff.obs.birth = difftime(Date1stObs, Birth_merge, units="days"))
# Negative values
rf.birthdates = errors %>% 
  filter(grepl("^-", errors$diff.obs.birth)) # individuals observed before their birth
# SOLUTION : NA
rf.birthdates = rf.birthdates %>% 
  mutate(Birth.b = NA)
setDT(n)[rf.birthdates, c("Birth_VR","Birth_mov","Birth_merge") := .(Birth.b), on = "GLT"] # Correct the dataset


# 5th ERROR : individuals first observed as adults with known birth dates
errors = n %>% 
  filter(Idade == "AD" & ObsOrder == 1 & !is.na(Birth_merge)) %>% 
  mutate(Birth.b = NA)
setDT(n)[errors, c("Birth_VR","Birth_mov","Birth_merge") := .(Birth.b), on = "GLT"] # Correct the dataset


## Implementing the Idade column

# From the correct birth date (Birth_VR)
n = n %>% 
  mutate(IdadeOK = ifelse(difftime(n$DateObs, n$Birth_VR, units="days") <= 365, "JU",
                         ifelse(difftime(n$DateObs, n$Birth_VR, units="days") >365 & difftime(n$DateObs, n$Birth_VR, units="days")<=1095, "SA",
                                ifelse(difftime(n$DateObs, n$Birth_VR, units="days") > 1095, "AD",
                                       NA))))

# Using the GLT weight
n = n %>% 
  mutate(IdadeOK = ifelse(is.na(IdadeOK) & Weight <= 400, "JU",
                          ifelse(is.na(IdadeOK) & Weight > 400, "AD",
                                 IdadeOK)))


# Append the other life stages from known life stages
errors = n %>%
  group_by(GLT) %>%
  filter((any(IdadeOK=="AD") | any(IdadeOK=="JU")) & is.na(Birth_VR)) %>% 
  ungroup() # Individuals with weight-induced life stage
# CASE 1 : individual known as adults at least once
sub_AD = errors %>% 
  group_by(GLT) %>%
  filter(all(c("AD", NA) %in% IdadeOK)) %>% 
  ungroup() %>% 
  filter(IdadeOK == "AD" | is.na(IdadeOK)) %>% 
  group_by(GLT) %>%
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
  as.data.table() # Assign "Adult" to every observation
setDT(n)[sub_AD, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset
# CASE 2 : individuals known as juveniles at least once
sub_JU = errors %>% 
  group_by(GLT) %>%
  filter(all(c("JU", NA) %in% IdadeOK)) %>% 
  ungroup() %>% 
  filter(IdadeOK == "JU" | is.na(IdadeOK)) %>% 
  group_by(GLT) %>%
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
  as.data.table() # Assign "Adult" to every observation
setDT(n)[sub_JU, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset



## IF IDADE IS STILL NA
# Using an inferred birth date
sub = n %>%
  filter(is.na(Birth_merge)) %>% 
  filter((ObsOrder == 1 & IdadeOK == "JU") | (ObsOrder == 1 & Idade == "JU")) %>% 
  mutate(Birth_inferred = ymd(DateObs)-months(3))
n = n %>% left_join(select(sub, c(GLT, Birth_inferred)))

                               
# Using the Idade existing field
n$Idade = as.character(n$Idade)
sub = n %>% 
  filter(!is.na(Idade) & !is.na(IdadeOK))
table(select(sub,c(Idade,IdadeOK)))
sub = sub %>% 
  filter((Idade=="JU" & IdadeOK=="AD") | (Idade=="AD" & IdadeOK=="JU"))
# Append the Idade variable and fill
n = n %>%  mutate(IdadeOK = ifelse(is.na(IdadeOK), Idade, IdadeOK))
# CASE 1 : individual known as adults at least once
sub_AD = n %>% 
  group_by(GLT) %>%
  filter(all(c("AD", NA) %in% IdadeOK)) %>% 
  ungroup() %>% 
  filter(IdadeOK == "AD" | is.na(IdadeOK)) %>% 
  group_by(GLT) %>%
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
  as.data.table() # Assign "Adult" to every observation
setDT(n)[sub_AD, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset
# CASE 2 : individuals known as juveniles at least once
sub_JU = n %>% 
  group_by(GLT) %>%
  filter(all(c("JU", NA) %in% IdadeOK)) %>% 
  ungroup() %>% 
  filter(IdadeOK == "JU" | is.na(IdadeOK)) %>% 
  group_by(GLT) %>%
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
  as.data.table() # Assign "Adult" to every observation
setDT(n)[sub_JU, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset


# Using the Birth_mov field
n = n %>% 
  mutate(IdadeOK = ifelse(is.na(IdadeOK) & difftime(n$DateObs, n$Birth_mov, units="days") <= 365, "JU",
                          ifelse(is.na(IdadeOK) & difftime(n$DateObs, n$Birth_mov, units="days") > 365 & difftime(n$DateObs, n$Birth_mov, units="days")<=1095, "SA",
                                 ifelse(is.na(IdadeOK) & difftime(n$DateObs, n$Birth_mov, units="days") > 1095, "AD",
                                        IdadeOK))))


## Remaining errors
# Check Idade incoherences according to the weight
rf.Idade = n %>% 
  filter(((IdadeOK == "JU" | IdadeOK == "SA") & Weight > 400)) %>% 
  mutate(Idade.b = "AD") %>% 
  mutate(Birth.b = NA)
setDT(n)[rf.Idade, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset
setDT(n)[rf.Idade, c("Birth_VR","Birth_mov","Birth_merge") := .(Birth.b), on = "GLT"]
rf.Idade = n %>% 
  filter((IdadeOK == "AD" & Weight < 400)) %>% 
  mutate(Idade.b = "JU") %>% 
  mutate(Birth.b = NA)
setDT(n)[rf.Idade, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset
setDT(n)[rf.Idade, c("Birth_VR","Birth_mov","Birth_merge") := .(Birth.b), on = "GLT"]


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
birthdates.checks = n %>% 
  filter(GLT %in% rf.Idade) %>% 
  select(GLT, Tattoo, Group, DateObs, ObsOrder, Weight, Birth_VR, Birth_mov, Birth_merge, Idade, IdadeOK) %>% 
  arrange(GLT, ObsOrder)
write.xlsx(birthdates.checks, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/birthdates_checks1.xlsx", row.names=FALSE)


# data.clean = data.clean %>% 
#   left_join(select(n,c(rowid,IdadeOK)))


# Individuals within groups with same birth dates
Nb_obs_gp <- read_delim("data/NewlyCreatedData/Nb_obs_gp.csv", delim=";")
dup.birth.ind = n %>% 
  select(c(DateObs, ObsOrder, GLT, Group, Idade, BirthOK)) %>% 
  distinct(GLT, .keep_all = TRUE) %>% 
  filter(!is.na(BirthOK)) %>% 
  filter(!is.na(Group))
dup.birth.ind = dup.birth.ind %>% 
  group_by(Group, BirthOK) %>%
  filter(n()>1) %>% 
  arrange(Group) %>% 
  left_join(select(Nb_obs_gp,c(Group,N))) %>% 
  rename(N_obs_gp = N)
# Subset well-monitored groups
dup.birth.ind1 = dup.birth.ind %>% 
  filter(N_obs_gp > 20)


## Statistical summaries
# Weight
hist(capt.data$BWeight)
x = n %>% 
  group_by(GLT, IdadeOK) %>% 
  summarise(MWeight = mean(Weight))
ggplot(filter(x, !is.na(MWeight)), aes(x = MWeight, fill = IdadeOK, colour = IdadeOK)) + 
  geom_histogram(alpha=0.7, position="identity")
