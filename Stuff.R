library(tidyverse)
library(readxl)
library(stringi)

# Function to remove accent marks from a string
remove_accents <- function(x) {
  stri_trans_general(x, "Latin-ASCII")
}

stuff <- read_excel("Stuff Plus.xlsx", sheet = "Data")

stuff_explore <- read_excel("Stuff Plus.xlsx", sheet = "Extended")

movement_dirty <- read_excel("Stuff Plus.xlsx", sheet = "Movement")

movement <- movement_dirty %>% 
  filter(year > 2019)

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
  rename(`Stf+ CUR` = `Stf+ CU`)

stuff <- stuff %>% 
  mutate(`Stf+ CU` = case_when(
    `Stf+ CUR` > -300 ~ `Stf+ CUR`,
    `Stf+ KC` > -300 ~ `Stf+ KC`,
    TRUE ~ NA))

stuff <- stuff %>% 
  select(-`Stf+ CUR`, -`Stf+ KC`)

stuff <- stuff %>% 
  rename(FF = `Stf+ FA`,
         FC = `Stf+ FC`,
         SI = `Stf+ SI`,
         SL = `Stf+ SL`,
         CU = `Stf+ CU`,
         CH = `Stf+ CH`,
         FS = `Stf+ FS`)

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
  select(IP:`Pitching+`, `Stf+ Pitch`, ID)

# Aggregation
data_init <- movement %>% 
  left_join(stuff_join, by = "ID")

data_init <- data %>% 
  filter(Name != "Luis Garcia")


stuff_init1 <- stuff %>% 
  mutate(ID = paste0(Name, Season, Team, "_",pitch_type)) %>% 
  select(IP:`Pitching+`, `Stf+ Pitch`, ID)

movement_init1 <- movement %>%
  mutate(ID = paste0(Name, year, team_name_abbrev, "_", pitch_type))

data_init2 <- movement_init1 %>% 
  filter(Name == "Luis Garcia") %>% 
  left_join(stuff_init1, by = "ID")


data <- rbind(
  data_init, data_init2)  


