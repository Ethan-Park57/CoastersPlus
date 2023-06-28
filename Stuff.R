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


# Note: for statcast, slurve, sweeper are now categorized as sliders too
# Note: for fangraphs, knuckle and regular curve (priority) are integrated



