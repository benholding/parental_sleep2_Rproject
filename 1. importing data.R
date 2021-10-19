library(tidyverse)
library(readxl)
library(psych)
#1. importing raw data
parentsleep2_data <- list()
parentsleep2_data$morning_questionnaire <- read_excel("rawdata/Cleanedup data - Questionairres - Parent sleep project.xlsx", 
                                                      sheet = "morning answers")

parentsleep2_data$evening_questionnaire <- read_excel("rawdata/Cleanedup data - Questionairres - Parent sleep project.xlsx", 
                                                      sheet = "evening answers")

parentsleep2_data$screening_questionnaire <- read_excel("rawdata/Cleanedup data - Questionairres - Parent sleep project.xlsx", 
                                                      sheet = "Screening Answers")

parentsleep2_data$actigraphy <- read_excel("rawdata/NEW_sleepMeasures_including_mutualAwakenings - Copy.xlsx", 
                                           sheet = "measures")

parentsleep2_data$key <- read_excel("rawdata/PS1_Info_ID_COUPLE_SEX.xlsx")
names(parentsleep2_data$actigraphy) <- gsub("'","",names(parentsleep2_data$actigraphy))

parentsleep2_data$evening_questionnaire <- 
  parentsleep2_data$evening_questionnaire %>% 
  mutate(SLD_32_reverse = as.numeric(reverse.code(-1,SLD_32))) %>% #reverse scoring SLD_32
  mutate(parental_stress_index = as.numeric(rowMeans(select(., c(SLD_30,SLD_31,SLD_33,SLD_32_reverse))))) #mean of the four items 

### 2. making dataset for analysis ###
## subjective sleep ##
# parentsleep2_data$morning_questionnaire$Subjective_sleep_minutes
# parentsleep2_data$morning_questionnaire$SLD_6 #sleep quality (hur har du sovit)
# parentsleep2_data$morning_questionnaire$SLD_11 #number of awakenings (Antal uppvaknanden:)
# 
# ## objective sleep ##
# parentsleep2_data$actigraphy$awakeNum_above_5min
# parentsleep2_data$actigraphy$sleep_efficiency_percent
# parentsleep2_data$actigraphy$actual_sleep_time
# 
# ## questionnaire data ##
# parentsleep2_data$morning_questionnaire$SLD_25 #strategy (Vilken var er planering för att hjälpa barnet under natten?), 
# #1=Jag hade huvudansvaret	
# #2= Min partner hade huvudansvaret	
# #3=Vi hade kommit överens om att dela	
# #4=Vi hade ingen planering	
# #5=annat
# parentsleep2_data$morning_questionnaire$SLD_25b #strategy (Fritextsvar om 5=Annat)
# parentsleep2_data$evening_questionnaire$parental_stress_index

##########
data_p1 <- parentsleep2_data$morning_questionnaire %>% 
  select("ID","Date of morning","Morning","Completed", 
         Subjective_sleep_minutes, sleep_quality = "SLD_6", no_of_awakenings = "SLD_11",
         strategy = "SLD_25_manuallycleaned", strategy_free_text = "SLD_25b")

data_p2 <- parentsleep2_data$evening_questionnaire %>% 
  select("ID","Date of morning","Morning","Completed","parental_stress_index")

data_p3 <- parentsleep2_data$actigraphy %>% 
  select("ID","Date of morning","Morning", actigraphy_data_usable = "usable","actual_sleep_time","sleep_efficiency_percent","awakeNum_above_5min") %>% 
  mutate(actual_sleep_time = as.numeric(actual_sleep_time),
         sleep_efficiency_percent = as.numeric(sleep_efficiency_percent),
         awakeNum_above_5min = as.numeric(awakeNum_above_5min))

data_p4 <- parentsleep2_data$key %>% 
  select(couple = couple_ID, ID, gender = Sex) %>% 
  mutate(gender = as.factor(gender))

parentsleep2_data$dataset_for_analysis <- data_p1 %>% 
  left_join(data_p2, by = c("ID", "Date of morning", "Morning", "Completed")) %>% 
  left_join(data_p3, by = c("ID", "Date of morning", "Morning")) %>% 
  left_join(data_p4, by = "ID") %>% 
  mutate_all( ~ case_when(!is.nan(.x) ~ .x)) %>% 
  filter(str_detect(Completed,"yes|Yes", negate = FALSE)) %>% 
  mutate(actual_sleep_time_hours = actual_sleep_time/60,
         Subjective_sleep_hours = Subjective_sleep_minutes/60) %>% 
  group_by(ID) %>% 
  mutate(within_subj_sleepduration_hours = actual_sleep_time_hours-mean(actual_sleep_time_hours, na.rm = T),
         between_subj_sleepduration_hours = mean(actual_sleep_time_hours, na.rm = T),
         within_subj_sleep_quality = sleep_quality-mean(sleep_quality, na.rm = T),
         between_subj_sleep_quality = mean(sleep_quality, na.rm = T),
         within_subj_Subjective_sleep_hours = Subjective_sleep_hours-mean(Subjective_sleep_hours, na.rm = T),
         between_subj_Subjective_sleep_hours = mean(Subjective_sleep_hours, na.rm = T)) 

parent_strategy_by_couple <- parentsleep2_data$dataset_for_analysis %>% 
  group_by(couple,Morning) %>% 
  summarise(n = n(), sumx=sum(strategy),meanx = mean(strategy), sdx = sd(strategy)) %>% 
  mutate(useable = if_else(n == 2, "yes", "no"),
         one_responsible = if_else(sumx == 3, 1, 0), # 1+2, or 2+1
         agreed_to_share = if_else(sumx == 6 & sdx == 0, 1, 0), # sum should be equal to 3+3, using the standard deviation to make sure there weren't any other combinations
         agree_no_plan = if_else(sumx == 8 & sdx == 0, 1, 0)) %>% #sum should be equal to 4+4, using the standard deviation to make sure there weren't any other combinations
  filter(useable == "yes") %>% 
  group_by(couple,Morning) %>% 
  mutate(shared_strategy_exists = sum(one_responsible,agreed_to_share),
         couple_responses_agree = sum(one_responsible,agreed_to_share,agree_no_plan)) %>% 
  select(couple,Morning,one_responsible,agreed_to_share,agree_no_plan,shared_strategy_exists,couple_responses_agree)

parentsleep2_data$parents_agree_strategy_dataset <- 
  parentsleep2_data$dataset_for_analysis %>% 
  left_join(parent_strategy_by_couple, by = c("couple", "Morning")) %>% 
  mutate(agree_no_plan = as.factor(agree_no_plan),
         agreed_to_share = as.factor(agreed_to_share),
         shared_strategy_exists = as.factor(shared_strategy_exists),
         strategy_factor = as.factor(strategy),
         strategy_factor_1and2combined = fct_collapse(strategy_factor, "1.5" = c("1","2"))) %>% 
  filter(couple_responses_agree == 1)


# save dataset
save(parentsleep2_data, file = "parentsleep2_data.RData")

