####### GLT data curing
### Author: Romain Monassier
### Date: 2023

### Libraries
load.lib = c("dplyr","Hmisc", "tibble", "data.table",
             "tidyr")
sapply(load.lib,require,character=TRUE)

### Data
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/clean_raw_data_long.RData")
data.clean = rowid_to_column(data.clean)
data.clean$rowid = paste0("obs",data.clean$rowid)
data.clean = as.data.frame(data.clean)
summary(data.clean)

### Sex verification
# Update case mistakes
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
  filter(GLT %in% sub.na) %>% 
  group_by(GLT) %>% 
  arrange(GLT, Sex) %>% 
  mutate(Sex = zoo::na.locf(Sex)) %>% 
  ungroup()
# Update the sex in the dataset where observations match those 


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
n = n %>% 
  mutate(Sex = ifelse(abs(F-M) < 5,"?","OK"))



# Export errors 
write.csv(n, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/sex_checkV2.csv", row.names=FALSE)


### Age and life stage verification