####### GLT data curing
### Author: Romain Monassier
### Date: 2023

### Libraries
load.lib = c("dplyr","Hmisc", "tibble", "data.table",
             "tidyr", "xlsx")
sapply(load.lib,require,character=TRUE)

### Data
load("D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/clean_raw_data_long.RData")
data.clean = rowid_to_column(data.clean)
data.clean$rowid = paste0("obs",data.clean$rowid)
data.clean = as.data.table(data.clean)
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
n = n %>% 
  mutate(Sex = ifelse(abs(F-M) < 7,"?","OK")) %>% 
  column_to_rownames("GLT")
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


# Assign the dominant sex where there is one ("OK")
n_ok = n %>% 
  filter(Sex == "OK") %>% 
  select(F, M)
n_ok$Sex.b <- apply(n_ok, 1, function(x) paste0(names(n_ok)[x == max(x)]))
n_ok = n_ok %>% 
  rownames_to_column(var="GLT") %>% 
  select(c("GLT","Sex.b"))
setDT(data.clean)[n_ok, "Sex" := .(Sex.b), on = "GLT"] # Correct the dataset




## Export errors 
write.xlsx(rf.sex, "D:/monas/Git/repo/glt/GoldenLionTamarins/data/NewlyCreatedData/Checks/sex_checkV2.xlsx", row.names=FALSE)


### Age and life stage verification