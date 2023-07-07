library(tidyverse)
library(readxl)
library(stringi)
library(patchwork)
library(Lahman)


# Function to remove accent marks from a string ####
remove_accents <- function(x) {
  stri_trans_general(x, "Latin-ASCII")
}

remove_pattern <- function(data, col, pattern) {
  data %>%
    mutate(!!col := gsub(pattern, "", !!sym(col)))
}

# New Function Cleaning ####
separate_name_into_first_last <- function(data, full_name) {
  data %>% 
    separate(full_name, into = c("first_name", "last_name"), sep = " ")
}
rename_col <- function(data, first, last) {
  data %>% 
    rename(first_name = first, last_name = last)
}
combine_names <- function(data) {
  data %>% mutate(full_name = paste(first_name, last_name, sep = " "))
}
clean_names <- function(data) {
  remove_accents <- function(x) {
    stri_trans_general(x, "Latin-ASCII")
  }
  
  remove_pattern <- function(data, col, pattern) {
    data %>%
      mutate(!!col := gsub(pattern, "", !!sym(col)))
  }
  
  # Remove accents
  data <- data %>%
    mutate_at(vars("first_name"), remove_accents)
  
  data <- data %>%
    mutate_at(vars("last_name"), remove_accents)
  
  # Remove "."
  data <- data %>% 
    remove_pattern("first_name", "\\.")
  
  data <- data %>% 
    remove_pattern("last_name", "\\.")
  
  data <- data %>% 
    remove_pattern("last_name", "\\ Jr")
  
  data <- data %>% 
    remove_pattern("first_name", "\\ ")
  
}

# Ethan's Data ####
e_stuff <- read_excel("Stuff Plus.xlsx", sheet = "Data") %>% 
  separate_name_into_first_last("Name") %>% 
  clean_names() %>% 
  combine_names()

e_movement <- read_excel("Stuff Plus.xlsx", sheet = "Movement") %>% 
  filter(year > 2019) %>% 
  clean_names() %>% 
  combine_names()

e_people_data <- Lahman::People %>%
  rename_col("nameFirst", "nameLast") %>% 
  select(first_name, last_name, playerID) %>% 
  clean_names() %>% 
  combine_names() %>% 
  select(full_name, playerID) %>% 
  rename("Name" = "full_name")

# Importing Data ####
stuff <- read_excel("Stuff Plus.xlsx", sheet = "Data")

stuff_explore <- read_excel("Stuff Plus.xlsx", sheet = "Extended")

movement_dirty <- read_excel("Stuff Plus.xlsx", sheet = "Movement")
movement <- movement_dirty %>% 
  filter(year > 2019)

pitches <- read_excel("Stuff Plus.xlsx", sheet = "Results_Pitch")

# Reformatting Movement (Statcast) ####
movement <- movement %>% 
  mutate(pitch_type = str_replace(pitch_type, "SIFT", "SI"),
         pitch_type = str_replace(pitch_type, "CUKC", "CU"),
         pitch_type = str_replace(pitch_type, "ST", "SL"),
         pitch_type_name = str_replace(pitch_type_name, "Sweeper", "Slider"),
         pitch_type = str_replace(pitch_type, "SV", "SL"),
         pitch_type_name = str_replace(pitch_type_name, "Sluve", "Slider"))

movement <- movement %>% 
  mutate("Name" = paste(first_name, last_name)) %>% 
  select(-first_name, -last_name, -team_name, -pitcher_id, -diff_z, -diff_x)

movement <- movement %>% 
  mutate_at(vars("Name"), remove_accents)

movement <- movement %>%
  mutate(ID = paste0(Name, year, "_", pitch_type))

# Reformatting Stuff (Fangraphs) ####
stuff <- stuff %>% 
  rename(`Stf CUR` = `Stf CU`)

stuff <- stuff %>% 
  mutate(`Stf CU` = case_when(
    `Stf CUR` > -300 ~ `Stf CUR`,
    `Stf KC` > -300 ~ `Stf KC`,
    TRUE ~ NA))

stuff <- stuff %>% 
  select(-`Stf CUR`, -`Stf KC`)

stuff <- stuff %>% 
  rename(FF = `Stf FA`,
         FC = `Stf FC`,
         SI = `Stf SI`,
         SL = `Stf SL`,
         CU = `Stf CU`,
         CH = `Stf CH`,
         FS = `Stf FS`)

stuff <- stuff %>% 
  pivot_longer(c(FF:CH, CU))

stuff <- stuff %>% 
  rename(pitch_type = name, `Stf+ Pitch` = value)

stuff <- stuff %>% 
  mutate(ID = paste0(Name, Season, "_",pitch_type))

stuff <- stuff %>% 
  mutate(Team = str_replace(Team, "ARI", "AZ"),
         Team = str_replace(Team, "CHW", "CWS"),
         Team = str_replace(Team, "KCR", "KC"),
         Team = str_replace(Team, "SDP", "SD"),
         Team = str_replace(Team, "SFG", "SF"),
         Team = str_replace(Team, "TBR", "TB"),
         Team = str_replace(Team, "WSN", "WSH")
         )

stuff <- stuff %>% 
  mutate(ID = paste0(Name, Season, "_",pitch_type))

# Note: for statcast, slurve, sweeper are now categorized as sliders too
# Note: for fangraphs, knuckle and regular curve (priority) are integrated

stuff_join <- stuff %>% 
  select(IP:`Pitching`, `Stf+ Pitch`, ID)

# Aggregation ####
data_init <- movement %>% 
  left_join(stuff_join, by = "ID")

data_init1 <- data_init %>% 
  filter(Name != "Luis Garcia")

stuff_init1 <- stuff %>% 
  filter(Name == "Luis Garcia") %>%
  mutate(ID = paste0(Name, Season, Team, "_",pitch_type)) %>% 
  select(IP:`Pitching`, `Stf+ Pitch`, ID)

movement_init1 <- movement %>%
  filter(Name == "Luis Garcia") %>%
  mutate(ID = paste0(Name, year, team_name_abbrev, "_", pitch_type))

data_init2 <- movement_init1 %>% 
  left_join(stuff_init1, by = "ID")


data1 <- rbind(
  data_init1, data_init2)  

data1 <- data1 %>% 
  select(Name, year:pitch_hand, pitch_type:pitch_type_name, 
         IP:`Stf+ Pitch`, pitcher_break_z:percent_rank_diff_x,
         avg_speed:pitch_per) %>% 
  filter(!is.na(IP))

Data <- data1 %>% 
  left_join(e_people_data, by = c("Name" = "Name"))

Data <- Data %>% 
  filter(playerID != "danisty01")


Data <- Data %>% 
  mutate(pitch_type_name = str_replace(pitch_type_name, "Slurve", "Slider"))

Data <- Data %>% 
  select(-`new name`) %>% 
  filter(!is.na(IP)) %>% 
  filter(!is.na(playerID)) %>% 
  filter(Name != "Luis Garcia")
# Environment Cleaning ####
rm(data_init)
rm(data_init1)
rm(data_init2)
rm(data1)
rm(e_movement)
rm(e_people_data)
rm(e_stuff)
rm(movement)
rm(movement_dirty)
rm(movement_init1)
rm(stuff)
rm(stuff_init1)
rm(stuff_join)
rm(clean_names)
rm(combine_names)
rm(remove_accents)
rm(remove_pattern)
rm(rename_col)
rm(separate_name_into_first_last)

# Nnotes ####
# Removed Luis Garcia, Tyler Danish
# Aggregated xxx