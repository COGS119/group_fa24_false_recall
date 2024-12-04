library(here)
library(tidyverse)
library(jsonlite)

processed_data_directory <- here("..","data","processed_data")
file_name <- "false_recall"

#read experiment data
exp_data <- read_csv(here(processed_data_directory,paste0(file_name,"-alldata.csv"))) %>%
  rename(participant_id=participant)

#double check that participant ids are unique
counts_by_random_id <- exp_data %>%
  group_by(random_id,participant_id) %>%
  count()
#output to track participants
write_csv(counts_by_random_id,here(processed_data_directory,paste0(file_name,"-participant-list.csv")))

demo <- exp_data %>% 
  filter(trial_type == "survey-text"&trial_index==196) %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  select(random_id,age:Q3)

#join into exp_data
exp_data <- exp_data %>%
  left_join(demo)

#filter and select relevant data
processed_data <- exp_data %>%
  filter(trial_type=="survey-likert") %>%
  select(participant_id,random_id,trial_index,time_elapsed,response,word,type,age:Q3) %>%
  mutate(json = map(response, ~ fromJSON(.) %>% as.data.frame())) %>% 
  unnest(json) %>%
  select(-response,-type) %>%
  rename(likert_response=Q0) %>%
  relocate(likert_response,.after=word)

#filter participant ids
filter_ids <- c(
  "1","12","1234","a1","as","nm","p4","p6"
)

processed_data <- processed_data %>%
  mutate(participant_id = trimws(tolower(participant_id))) %>%
  #fix some ids
  mutate(
    participant_id = case_when(
      participant_id == "herson" ~ "heron",
      participant_id == "p73" ~ "giraffe",
      participant_id == "2341" ~ "porcupine",
      participant_id == "a17689315" ~ "rabbit",
      TRUE ~ participant_id
    )
  ) %>%
  filter(!(participant_id %in% filter_ids))
  
#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))
