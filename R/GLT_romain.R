####### GLT data curing
### Author: Romain Monassier
### Date: 2023

### Libraries
load.lib = c("dplyr","tibble", "data.table",
             "tidyr", "xlsx", "zoo", "readxl")
sapply(load.lib,require,character=TRUE)

### Data
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/clean_raw_data_long.RData")
data.clean = rowid_to_column(data.clean) # Add a rowid column (observation unique id)
data.clean$rowid = paste0("obs",data.clean$rowid)
data.clean = as.data.table(data.clean)
summary(data.clean)

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
# Update the sex in the dataset where observations match those 
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
  filter(GLT == "RL11" | GLT == "RL13" | GLT == "SF3" | GLT == "SF4" | GLT == "SK19")

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
  left_join(select(BirthDates, c(1:2,5)))
data.clean %>% 
  group_by(Birth_VR) %>%
  summarise(no_rows = length(Birth_VR)) %>% 
  print(n=200)
# Filter NA data
n1 = data.clean %>%
  select(c(rowid, DateObs, GLT, Tattoo, Birth_VR, Idade)) %>%
  filter(!is.na(Birth_VR) & Birth_VR != "?")
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
  select(c(rowid, DateObs, GLT, Tattoo, Birth_mov, Idade)) %>%
  filter(!is.na(Birth_mov))
# n2$Birth_mov = paste0('01/', n2$Birth_mov)
n2$Birth_mov = as.Date(n2$Birth_mov, format="%d/%m/%y")

# Check date format
IsDate <- function(mydate, date.format = "%d/%m/%y") {
  tryCatch(!is.na(as.Date(mydate, date.format)),  
           error = function(err) {FALSE})  
}
IsDate(n1$Birth_VR)
IsDate(n2$Birth_mov)

# Load GLT movement
GLT_mov = read_excel("D:/monas/Git/repo/glt/GoldenLionTamarins/data/RawData/GLT_movement.xls",
                     na="NA")
GLT_mov = GLT_mov %>% 
  rename(GLT = Individual) %>% 
  rename(Tattoo = Tatoo)
GLT_mov$Estimated.date.of.emigration = as.Date(GLT_mov$Estimated.date.of.emigration, format="%d/%m/%y")
IsDate(GLT_mov$Estimated.date.of.emigration)

## Merge both subsets of dates
n = n1 %>% 
  right_join(n2)
n = n %>% 
  left_join(select(GLT_mov,c(GLT, Tattoo, Estimated.date.of.emigration)))


## Check Birth dates errors
# Incoherences between both indicated birth dates
n = n %>% 
  mutate(diff.b1.b2 = abs(difftime(Birth_mov, Birth_VR, units="days")))
n %>%
  group_by(diff.b1.b2) %>%
  summarise(no_rows = length(diff.b1.b2)) %>%
  print(n=60)
n = n %>% 
  mutate(Birth = ifelse(!is.na(Birth_mov) & !is.na(Birth_VR), Birth_VR,
                        Birth_mov))
n$Birth = as.Date(n$Birth)

# Errors in the movement dataset
# Comparaison between the estimated.date.of.emigration and the Birth Date
n = n %>% 
  mutate(diff.emig.birth = difftime(Estimated.date.of.emigration, Birth, units="days"))
rf.birthdates = n[grepl("^-", n$diff.emig.birth), ]
n = n %>% 
  filter(!grepl("^-", n$diff.emig.birth))

# Duplicated/equivocal birth dates
dup.birth = n %>% 
  select(GLT, Birth) %>% 
  group_by(GLT) %>%
  filter(n_distinct(Birth) > 1) %>%
  ungroup()
dup.birth = dup.birth %>% 
  group_by(GLT, Birth) %>% 
  summarise(n=n())
dup.birth = dup.birth %>% 
  group_by(GLT) %>% 
  mutate(Births = paste0(Birth, collapse = " or ")) %>% 
  distinct(GLT, Births) %>% 
  ungroup()
dup.birth = dup.birth %>% 
  filter(GLT != "?") %>% 
  separate(Births, c("opt1", "opt2", "opt3", "opt4", "opt5"), " or ")

# Create subsets according to the number of different birth dates
# Two different dates
dup.birth2 = dup.birth %>% 
  filter(is.na(opt3)) %>% 
  select(1:3)
dup.birth2$opt1 = as.Date(x = dup.birth2$opt1, format="%Y-%m-%d")
dup.birth2$opt2 = as.Date(x = dup.birth2$opt2, format="%Y-%m-%d")
dup.birth2 = dup.birth2 %>% 
  mutate(diffBirth = abs(difftime(opt1, opt2, units="days")))
# Three different dates
dup.birth3 = dup.birth %>% 
  filter(is.na(opt4) & !is.na(opt3)) %>% 
  select(1:4)
dup.birth3$opt1 = as.Date(x = dup.birth3$opt1, format="%Y-%m-%d")
dup.birth3$opt2 = as.Date(x = dup.birth3$opt2, format="%Y-%m-%d")
dup.birth3$opt3 = as.Date(x = dup.birth3$opt3, format="%Y-%m-%d")
# Five different dates
dup.birth5 = dup.birth %>% 
  filter(!is.na(opt5))
dup.birth5$opt1 = as.Date(x = dup.birth5$opt1, format="%Y-%m-%d")
dup.birth5$opt2 = as.Date(x = dup.birth5$opt2, format="%Y-%m-%d")
dup.birth5$opt3 = as.Date(x = dup.birth5$opt3, format="%Y-%m-%d")
dup.birth5$opt4 = as.Date(x = dup.birth5$opt4, format="%Y-%m-%d")
dup.birth5$opt5 = as.Date(x = dup.birth5$opt5, format="%Y-%m-%d")


## Implementing the Idade column
n = n %>% 
  mutate(Idade2 = ifelse(difftime(n$DateObs, n$Birth, units="days") <= 365, "JU",
                         ifelse(difftime(n$DateObs, n$Birth, units="days") >365 & difftime(n$DateObs, n$Birth, units="days")<=1095, "SA",
                                ifelse(difftime(n$DateObs, n$Birth, units="days") > 1095, "AD",
                                       Idade))))
data.clean = data.clean %>% 
  left_join(select(n,c(rowid,Idade2)))
