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
  
#store processed and prepped data
write_csv(processed_data,here(processed_data_directory,paste0(file_name,"-processed-data.csv")))
