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
  mutate(Sex = ifelse(Sex == "m", "M",
                      Sex))
data.clean = data.clean %>% 
  mutate(Sex = ifelse(Sex == "M0" | Sex == "nonID" | Sex == "S", NA,
                      Sex))

# Select individuals with duplicated sexes
dup.sex = data.clean %>% 
  select(GLT, Sex) %>% 
  group_by(GLT) %>%
  filter(n_distinct(Sex) > 1) %>%
  ungroup()
n = dup.sex %>% 
  group_by(GLT, Sex) %>% 
  summarise(n=n())

## Fill in NAs for individuals with another value
# Select GLT with NAs in sex column
sub.na = dup.sex %>% 
  filter(is.na(Sex)) %>% 
  select(GLT) %>% 
  pull()
# Subset data.clean with the latter GLT, and update the NA value according to the dominant one (na.locf)
correct.na = data.clean %>% 
  select(rowid, GLT, Sex) %>% 
  subset(GLT %in% sub.na) %>% 
  group_by(GLT) %>% 
  arrange(GLT, Sex) %>% 
  mutate(Sex = zoo::na.locf(Sex)) %>% 
  rename(Sex.b = Sex) %>% 
  ungroup() %>% 
  as.data.table()
# Update the sex in the dataset where observations match those 
setDT(data.clean)[correct.na, "Sex" := .(Sex.b), on = "GLT"]

# Remove the NAs errors from the table
dup.sex = dup.sex %>% 
  group_by(GLT) %>% 
  arrange(GLT, Sex) %>% 
  mutate(Sex = zoo::na.locf(Sex)) %>% 
  filter(n_distinct(Sex) > 1)
n = dup.sex %>% 
  group_by(GLT, Sex) %>% 
  summarise(n=n())
n = n %>%
  pivot_wider(names_from = Sex, values_from = n)

# Locate significant uncertainties
# Compute the difference in mentions of both sexes
# Significant uncertainties < 7
n = n %>% 
  mutate(Sex = ifelse(abs(F-M) < 7,"?","OK")) %>% 
  column_to_rownames("GLT")
# Select the uncertainties
rf.sex = n %>% 
  filter(Sex == "?") %>% 
  select(F, M) %>% 
  rownames_to_column(var="GLT") %>% 
  select(GLT) %>% 
  unlist()
rf.sex = data.clean %>% 
  select(c(1:8)) %>% 
  filter(GLT %in% rf.sex) %>% 
  arrange(GLT)


# Assign the dominant sex for the individuals for which the difference is above 7 and deemed non significant
n_ok = n %>% 
  filter(Sex == "OK") %>% 
  select(F, M)
n_ok$Sex.b <- apply(n_ok, 1, function(x) paste0(names(n_ok)[x == max(x)])) # For each individual, we implement the most-mentioned sex
n_ok = n_ok %>% 
  rownames_to_column(var="GLT") %>% 
  select(c("GLT","Sex.b"))
setDT(data.clean)[n_ok, "Sex" := .(Sex.b), on = "GLT"] # Correct the dataset


## Export errors 
write.xlsx(rf.sex, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/sex_checkV2.xlsx", row.names=FALSE)


### Age and life stage verification

## Birth Dates from 1989 to 2001
BirthDates = read_excel("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/BirthDates.xlsx")
# Remove duplicated rows (GLT)
BirthDates = BirthDates[!duplicated(BirthDates$GLT), ]
# Check typing errors
data.clean = data.clean %>% 
  left_join(select(BirthDates, c(1:3,5)))
data.clean %>% 
  group_by(BirthDMY) %>%
  summarise(no_rows = length(Birth)) %>% 
  print(n=200)
# Conversion of Birth variable to Date format
n = data.clean %>% 
  select(c(rowid, DateObs, GLT, BirthDMY, Idade)) %>% 
  filter(!is.na(BirthDMY))
n$BirthDMY = as.Date(n$BirthDMY, format="%d/%m/%y")



## Birth dates post-2001
# Update typing errors for the Birth field
# data.clean %>% 
#   group_by(Birth) %>%
#   summarise(no_rows = length(Birth)) %>% 
#   print(n=300)
# data.clean = data.clean %>% 
#   mutate(Birth = ifelse(Birth == "'" | Birth == "-" | Birth == "?" | Birth == "??" | Birth == "died"
#                         | Birth == "42614" | Birth == "42615" | Birth == "FA20" | Birth == "RC1"
#                         | Birth == "RC2" | Birth == "RC3", NA,
#                         ifelse(Birth == "10/ 21", "10/21",
#                                ifelse(Birth == "1015", "10/15",
#                                       Birth))))
# n = data.clean %>% 
#   select(c(rowid, DateObs, GLT, Birth, Idade)) %>% 
#   filter(!is.na(Birth))
# n$Birth = paste0('01/', n$Birth)
# n$Birth = n$Birth %>% as.Date(n$Birth, format="%d/%m/%y")


IsDate <- function(mydate, date.format = "%d/%m/%y") {
  tryCatch(!is.na(as.Date(mydate, date.format)),  
           error = function(err) {FALSE})  
}
IsDate(n$DateObs)

# Duplicated/equivocal birth dates
dup.birth = n %>% 
  select(GLT, BirthDMY) %>% 
  group_by(GLT) %>%
  filter(n_distinct(BirthDMY) > 1) %>%
  ungroup()
dup.birth = dup.birth %>% 
  group_by(GLT, BirthDMY) %>% 
  summarise(n=n())
dup.birth = dup.birth %>% 
  group_by(GLT) %>% 
  mutate(Births = paste0(BirthDMY, collapse = " or ")) %>% 
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
