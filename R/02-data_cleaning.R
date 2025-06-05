library(tidyr)
library(gt)

clean_data <- raw_data %>% 
  mutate(id = row_number()) %>% 
  relocate(id, before = 1) %>% 
  filter(id >= 9) %>% 
  rename("age" = "How old are you (in years)?",
         "edu_lvl" = "What is your highest level of education?",
         "gender" = "What is your gender?",
         "student_state" = "Are you currently studying or working at an university?",
         "university" = "Which University are you affiliated with?",
         "uni_state" = "What is your current occupation at the university? (select most appropriate)", 
         "household_size" = "How many people live in your household?", 
         "bottles_bought" = "During an average week, how many glass bottles/jars etc. does your household buy? Try to estimate a number.",
         "bottles_recycled" = "How many of your households glass bottles/jars etc. are separated and returned to a collection point to be recycled? Try to estimate the amount of bottles.",
         "transport_method" = "When you return your used glass bottles, how do you transport them? Select the method you use the most.",
         "travel_distance" = "How far is the distance from your home to the closest collection point for glass recycling? Please give an estimate in meters.",
         "deposit_opinion" = "Do you think a deposit (\"Pfand\") on glass bottles would increase the percentage of bottles being returned correctly for recycling?",
         "distance_opinion" = "Would you recycle more if your way to the next recycling station was shorter?",
         "comments" = "If you have any comments, you can add them here. Thank you for participating!") %>% 
  relocate(before, .after = last_col()) %>% 
  mutate(edu_lvl = case_when(
    edu_lvl == "Bachelor degree" ~ "Bachelor",
    edu_lvl == "Bald Meister" ~ "Higher technical school",
    edu_lvl == "Completed Apprenticeship (Abgeschlossene Lehre)" ~ "Apprenticeship",
    edu_lvl == "Compulsory Schooling (Obligatorische Grundschule)" ~ "Secondary school",
    edu_lvl == "Fachmonteur und 2 EFZ Abschlüsse, Peacemaker Diplom" ~ "Higher technical school",
    edu_lvl == "Höhere Fachschule" ~ "Higher technical school",
    edu_lvl == "Master degree" ~ "Master",
    edu_lvl == "Matura degree" ~ "Matura",
    TRUE ~ edu_lvl
  )) %>% 
  mutate(bottles_recycled = case_when(
    id == 34 ~ 1, # assuming there was a typo and they meant 1 instead of 156
    id == 24 ~ bottles_bought*bottles_recycled/100, # assuming they answered as a percentage instead of the actual amount
    id == 35 ~ bottles_bought*bottles_recycled/100, # as above
    id == 57 ~ bottles_bought*bottles_recycled/100, # as above
    id == 55 ~ bottles_bought*bottles_recycled/100, # as above
    id == 10 ~ bottles_bought*bottles_recycled/100, # as above
    id == 25 ~ bottles_bought*bottles_recycled/100, # as above
    TRUE ~ bottles_recycled
  )) %>% 
  mutate(transport_method = case_when(
    transport_method == "I return bottles I  unfortunately find daily in hedges, forrest, meadows :-(, by bike" ~ "bicycle",
    transport_method == "By foot" ~ "foot",
    transport_method == "By car" ~ "car",
    transport_method == "By public transport" ~ "public transport",
    transport_method == "By bicycle" ~ "bicycle",
    transport_method == "I don't return my used bottles." ~ "foot",
    TRUE ~ transport_method
  )) %>% 
  mutate(distance_group = case_when(
    travel_distance <= 500 ~ "0 - 500m",
    travel_distance <= 1000 ~ "501 - 1000m",
    travel_distance <= 2000 ~ "1001 - 2000m",
    travel_distance > 2001 ~ "2000m +"
  )) %>% 
  mutate(recycling_percentage = 100*bottles_recycled/bottles_bought) %>% 
  relocate(recycling_percentage, .after = bottles_recycled) %>% 
  mutate(bottles_bought_pp = bottles_bought/household_size) %>% 
  relocate(bottles_bought_pp, .after = bottles_recycled) %>%
  mutate(bottles_recycled_pp = bottles_recycled/household_size) %>% 
  relocate(bottles_recycled_pp, .after = bottles_bought_pp) %>% 
  mutate(recycling_percentage = case_when(
    id == 54 ~ 100, # doesn't buy glass but recycles trash they find
    TRUE ~ recycling_percentage
  ))

#clean_data$travel_duration <- unlist(clean_data$travel_duration)

list_cols <- sapply(clean_data, is.list)
print(names(clean_data)[list_cols])

clean_data <- clean_data[ , !list_cols]

write.csv(clean_data, "data/processed/clean.csv")

levels_distance <- c("0 - 500m", "501 - 1000m", "1001 - 2000m", "2000m +")

clean_data_lvl <- clean_data %>% 
  mutate(distance_group = factor(distance_group, levels = levels_distance))

distance_tbl <- clean_data_lvl %>% 
  filter(!is.na(distance_group)) %>% 
  group_by(distance_group) %>% 
  summarize(count = n(),
            "mean bought bottles" = mean(bottles_bought_pp),
            "mean recycled bottles" = mean(bottles_recycled_pp),
            "mean recycling quota" = mean(recycling_percentage),
            "sd recycling quota" = sd(recycling_percentage))

write.csv(distance_tbl, "data/final/distance_tbl.csv")

