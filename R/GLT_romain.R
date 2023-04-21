####### GLT data curing
### Author: Romain Monassier
### Date: 2023

### Libraries
load.lib = c("dplyr","tibble", "data.table",
             "tidyr", "xlsx", "zoo", "readxl",
             "readr")
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
  rename(DateObs = Date)
capt.data = capt.data %>% 
  mutate(DateObsMY = format(DateObs, "%m-%Y")) %>% 
  mutate(DateObsSemester = Date2period(DateObs))
capt.data = capt.data %>% 
  group_by(GLT, DateObsSemester) %>% 
  mutate(SMeanWeight = mean(BWeight))
data.clean = data.clean %>% 
  left_join(select(capt.data,c(GLT, DateObsSemester, SMeanWeight))) %>% 
  rename(Weight = SMeanWeight)

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
sub.na = dup.sex %>% 
  filter(is.na(SexOK)) %>% 
  select(GLT) %>% 
  pull()
# Subset data.clean with the latter GLT, and update the NA value according to the dominant one (na.locf)
correct.na = data.clean %>% 
  select(rowid, GLT, SexOK) %>% 
  subset(GLT %in% sub.na) %>% 
  group_by(GLT) %>% 
  arrange(GLT, SexOK) %>% 
  mutate(SexOK = zoo::na.locf(SexOK)) %>% 
  rename(Sex.b = SexOK) %>% 
  ungroup() %>% 
  as.data.table()
# Update the SexOK in the dataset where observations match those 
setDT(data.clean)[correct.na, "SexOK" := .(Sex.b), on = "GLT"]


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
uncertain = n %>% 
  filter(SexOK == "?") %>% 
  select(F, M) %>% 
  rownames_to_column(var="GLT") %>% 
  select(GLT) %>% 
  unlist()
# Red flags
rf.sex = data.clean %>% 
  filter(GLT %in% uncertain) %>% 
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
  mutate(SexOK = NA) %>% 
  rename(Sex.b = SexOK)
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
#Update typing errors for the Birth_mov field
# data.clean = data.clean %>%
#   mutate(Birth_mov = ifelse(Birth_mov == "'" | Birth_mov == "-" | Birth_mov == "?" | Birth_mov == "??" | Birth_mov == "died"
#                         | Birth_mov == "42614" | Birth_mov == "42615" | Birth_mov == "FA20" | Birth_mov == "RC1"
#                         | Birth_mov == "RC2" | Birth_mov == "RC3", NA,
#                         ifelse(Birth_mov == "10/ 21", "10/21",
#                                ifelse(Birth_mov == "1015", "10/15",
#                                       Birth_mov))))
n2 = data.clean %>%
  select(c(rowid, DateObs, ObsOrder, GLT, Tattoo, Group, Idade, Weight, Birth_mov)) %>%
  filter(!is.na(Birth_mov) | !is.na(Idade) | !is.na(Weight))
# n2$Birth_mov = paste0('01/', n2$Birth_mov)
n2$Birth_mov = as.Date(n2$Birth_mov, format="%d/%m/%y")

## Merge both subsets of dates
n = n1 %>% 
  right_join(n2)


## Check Birth dates errors
# Incoherences between both indicated birth dates
n = n %>% 
  mutate(diff.b1.b2 = abs(difftime(Birth_mov, Birth_VR, units="days")))
n %>%
  group_by(diff.b1.b2) %>%
  summarise(no_rows = length(diff.b1.b2), n_GLT=n_distinct(GLT)) %>%
  print(n=60)
n = n %>% 
  mutate(BirthOK = ifelse(!is.na(Birth_mov) & !is.na(Birth_VR), Birth_VR,
                        Birth_mov))
n$BirthOK = as.Date(n$BirthOK)

# Errors in the movement dataset
# Comparaison between the estimated.date.of.emigration and the Birth Date
n3 = n %>% 
  left_join(select(GLT_mov,c(GLT, Estimated.date.of.emigration)))
n3 = n3 %>% 
  mutate(diff.emig.birth = difftime(Estimated.date.of.emigration, BirthOK, units="days"))
rf.birthdates = n3[grepl("^-", n3$diff.emig.birth), ]
# Visual inspection and manual correction
n3$Estimated.date.of.emigration[n3$GLT == "A7"] <- "1996-05-01"
n$BirthOK[n$GLT == "LB6"] <- "1998-04-01"
n$BirthOK[n$GLT == "CA9"] <- NA
n$BirthOK[n$GLT == "SK19"] <- NA


# Duplicated/equivocal birth dates
dup.birth = n %>% 
  select(GLT, BirthOK) %>% 
  group_by(GLT) %>%
  filter(!is.na(BirthOK)) %>% 
  filter(n_distinct(BirthOK) > 1) %>%
  ungroup()
dup.birth = dup.birth %>% 
  group_by(GLT, BirthOK) %>% 
  summarise(n=n())
dup.birth = dup.birth %>% 
  group_by(GLT) %>% 
  mutate(Births = paste0(BirthOK, collapse = " or ")) %>% 
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
setDT(n)[dup.birth2, "BirthOK" := .(Birth.b), on = "GLT"] # Correct the dataset

# Three different dates
dup.birth3 = dup.birth %>% 
  filter(is.na(opt4) & !is.na(opt3)) %>% 
  select(1:4) %>% 
  as.data.table()
dup.birth3$opt1 = as.Date(x = dup.birth3$opt1, format="%Y-%m-%d")
dup.birth3$opt2 = as.Date(x = dup.birth3$opt2, format="%Y-%m-%d")
dup.birth3$opt3 = as.Date(x = dup.birth3$opt3, format="%Y-%m-%d")
n$BirthOK[n$GLT == "1375"] <- NA
n$BirthOK[n$GLT == "AF20"] <- "2011-12-01"
n$BirthOK[n$GLT == "KE10"] <- "2002-09-01"
n$BirthOK[n$GLT == "SP23"] <- "2011-11-01"

# Five different dates
dup.birth5 = dup.birth %>% 
  filter(!is.na(opt5)) %>% 
  as.data.table()
dup.birth5$opt1 = as.Date(x = dup.birth5$opt1, format="%Y-%m-%d")
dup.birth5$opt2 = as.Date(x = dup.birth5$opt2, format="%Y-%m-%d")
dup.birth5$opt3 = as.Date(x = dup.birth5$opt3, format="%Y-%m-%d")
dup.birth5$opt4 = as.Date(x = dup.birth5$opt4, format="%Y-%m-%d")
dup.birth5$opt5 = as.Date(x = dup.birth5$opt5, format="%Y-%m-%d")
n$BirthOK[n$GLT == "FT"] <- NA
n$BirthOK[n$GLT == "IN"] <- NA
n$BirthOK[n$GLT == "T0"] <- NA


# Differences between birth dates and dates of first observation
n4 = n %>% 
  right_join(select(data.clean,c(GLT, DateObs, ObsOrder))) %>% 
  group_by(GLT) %>% 
  mutate(Date1stObs = min(DateObs, na.rm = TRUE)) %>% 
  ungroup() %>% 
  as.data.table()
n4 = n4 %>% 
  filter(ObsOrder == 1) %>%  
  mutate(diff.obs.birth = difftime(Date1stObs, BirthOK, units="days"))
rf1.birthdates = n4 %>% 
  filter(grepl("^-", n4$diff.obs.birth)) # individuals observed before their birth
rf1.birthdates = rf1.birthdates %>% 
  mutate(Birth.b = NA)
setDT(n)[rf1.birthdates, "BirthOK" := .(Birth.b), on = "GLT"] # Correct the dataset


# Individuals first observed as adults with known birth dates
n5 = n %>% 
  filter(Idade == "AD" & ObsOrder == 1 & !is.na(BirthOK)) %>% 
  mutate(Birth.b = NA)
setDT(n)[n5, "BirthOK" := .(Birth.b), on = "GLT"] # Correct the dataset

# Implement Birth date from first observation juvenile
n6 = n %>% 
  filter(ObsOrder == 1 & Idade == "JU" & is.na(BirthOK))
n6 = n6 %>% 
  mutate(Birth.b = as.Date(DateObs-90))
setDT(n)[n6, "BirthOK" := .(Birth.b), on = "GLT"] # Correct the dataset


## Implementing the Idade column
# From the birth date
n = n %>% 
  mutate(IdadeOK = ifelse(difftime(n$DateObs, n$BirthOK, units="days") <= 365, "JU",
                         ifelse(difftime(n$DateObs, n$BirthOK, units="days") >365 & difftime(n$DateObs, n$BirthOK, units="days")<=1095, "SA",
                                ifelse(difftime(n$DateObs, n$BirthOK, units="days") > 1095, "AD",
                                       NA))))
# Using the GLT weight
n7 = n %>% 
  filter(is.na(IdadeOK) & !is.na(Weight)) %>% 
  mutate(Idade.b = ifelse(Weight <= 500, "JU",
                          ifelse(Weight > 500, "AD",
                                 NA)))
setDT(n)[n7, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset

# Check Idade incoherences according to the weight
rf.Idade = n %>% 
  filter((IdadeOK == "JU" & Weight > 500) | (IdadeOK == "AD" & Weight < 500))
rf.Idade %>% distinct(GLT)
setDT(n)[rf.Idade, "IdadeOK" := .(Idade.b), on = "rowid"] # Correct the dataset

# Using the Idade existing field
n$Idade = as.character(n$Idade)
n = n %>%  mutate(IdadeOK = ifelse(is.na(IdadeOK), Idade, IdadeOK))

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
