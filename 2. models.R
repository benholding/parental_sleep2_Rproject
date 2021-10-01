#load packages
pacman::p_load(dplyr, broom, lme4, lmerTest, sjPlot, emmeans)

#load data
load("parentsleep2_data.RData")

########################################################
######### OBJECTIVE SLEEP -> parental stress ###########
########################################################

#Awakenings over 5mins
wake_PS_model_intercept <- lmer(parental_stress_index ~ 1 + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(awakeNum_above_5min)))
wake_PS_model_sleep <- lmer(parental_stress_index ~ awakeNum_above_5min + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(awakeNum_above_5min)))
wake_PS_model_gender <- lmer(parental_stress_index ~ awakeNum_above_5min + gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(awakeNum_above_5min)))
wake_PS_model_interaction <- lmer(parental_stress_index ~ awakeNum_above_5min*gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(awakeNum_above_5min)))
wake_PS_all_models <- tidy(anova(wake_PS_model_intercept,wake_PS_model_sleep,wake_PS_model_gender,wake_PS_model_interaction)) #intercept best
write.csv(wake_PS_all_models, "tables/S1. parentalstress_awakenings.csv")

#Sleep efficiency
sleepefficiency_PS_model_intercept <- lmer(parental_stress_index ~ 1 + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_efficiency_percent)))
sleepefficiency_PS_model_sleep <- lmer(parental_stress_index ~ sleep_efficiency_percent + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_efficiency_percent)))
sleepefficiency_PS_model_gender <- lmer(parental_stress_index ~ sleep_efficiency_percent + gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_efficiency_percent)))
sleepefficiency_PS_model_interaction <- lmer(parental_stress_index ~ sleep_efficiency_percent*gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_efficiency_percent)))
sleepefficiency_PS_all_models <- tidy(anova(sleepefficiency_PS_model_intercept,sleepefficiency_PS_model_sleep,sleepefficiency_PS_model_gender,sleepefficiency_PS_model_interaction)) #intercept best
write.csv(sleepefficiency_PS_all_models, "tables/S2. parentalstress_sleepefficiency.csv")

#within subjects sleep duration
within_sleepduration_PS_model_intercept <- lmer(parental_stress_index ~ 1 + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(actual_sleep_time)))
within_sleepduration_PS_model_sleep <- lmer(parental_stress_index ~ within_subj_sleepduration_hours + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(actual_sleep_time))) #sig
within_sleepduration_PS_model_gender <- lmer(parental_stress_index ~ within_subj_sleepduration_hours + gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(actual_sleep_time)))
within_sleepduration_PS_model_interaction <- lmer(parental_stress_index ~ within_subj_sleepduration_hours*gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(actual_sleep_time)))
within_sleepduration_PS_all_models <- tidy(anova(within_sleepduration_PS_model_intercept,within_sleepduration_PS_model_sleep,within_sleepduration_PS_model_gender,within_sleepduration_PS_model_interaction)) #sleep best
write.csv(within_sleepduration_PS_all_models, "tables/S3. parentalstress_within_subj_sleepduration.csv")

sjPlot::plot_model(within_sleepduration_PS_model_sleep, type = "pred", show.data = T,title = "",axis.title = c("Actigraphy Sleep Duration (hours)","Parental Stress"))

#between subjects sleep duration
between_sleepduration_PS_model_intercept <- lmer(parental_stress_index ~ 1 + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(actual_sleep_time)))
between_sleepduration_PS_model_sleep <- lmer(parental_stress_index ~ between_subs_sleepduration_hours + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(actual_sleep_time))) 
between_sleepduration_PS_model_gender <- lmer(parental_stress_index ~ between_subs_sleepduration_hours + gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(actual_sleep_time)))
between_sleepduration_PS_model_interaction <- lmer(parental_stress_index ~ between_subs_sleepduration_hours*gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(actual_sleep_time)))
between_sleepduration_PS_all_models <- tidy(anova(between_sleepduration_PS_model_intercept,between_sleepduration_PS_model_sleep,between_sleepduration_PS_model_gender,between_sleepduration_PS_model_interaction)) #intercept best

#########################################################
########## SUBJECTIVE SLEEP -> parental stress ##########
#########################################################

#Subjective awakenings
subj_awakenings_PS_model_intercept <- lmer(parental_stress_index ~ 1 + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(no_of_awakenings)))
subj_awakenings_PS_model_sleep <- lmer(parental_stress_index ~ no_of_awakenings + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(no_of_awakenings)))
subj_awakenings_PS_model_gender <- lmer(parental_stress_index ~ no_of_awakenings + gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(no_of_awakenings)))
subj_awakenings_PS_model_interaction <- lmer(parental_stress_index ~ no_of_awakenings*gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(no_of_awakenings)))
anova(subj_awakenings_PS_model_intercept,subj_awakenings_PS_model_sleep,subj_awakenings_PS_model_gender,subj_awakenings_PS_model_interaction) #intercept best

#within subjects sleep quality
within_subj_sleepquality_PS_model_intercept <- lmer(parental_stress_index ~ 1 + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_quality)))
within_subj_sleepquality_PS_model_sleep <- lmer(parental_stress_index ~ within_subj_sleep_quality + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_quality))) #sig
within_subj_sleepquality_PS_model_gender <- lmer(parental_stress_index ~ within_subj_sleep_quality + gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_quality)))
within_subj_sleepquality_PS_model_interaction <- lmer(parental_stress_index ~ within_subj_sleep_quality*gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_quality)))
anova(within_subj_sleepquality_PS_model_intercept,within_subj_sleepquality_PS_model_sleep,within_subj_sleepquality_PS_model_gender,within_subj_sleepquality_PS_model_interaction) #sleep best
sjPlot::plot_model(within_subj_sleepquality_PS_model_sleep, type = "pred", show.data = T,title = "",axis.title = c("Subjective Sleep Quality","Parental Stress"),jitter=0.05)

#between subjects sleep quality
between_subj_sleepquality_PS_model_intercept <- lmer(parental_stress_index ~ 1 + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_quality)))
between_subj_sleepquality_PS_model_sleep <- lmer(parental_stress_index ~ between_subj_sleep_quality + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_quality))) #sig
between_subj_sleepquality_PS_model_gender <- lmer(parental_stress_index ~ between_subj_sleep_quality + gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_quality)))
between_subj_sleepquality_PS_model_interaction <- lmer(parental_stress_index ~ between_subj_sleep_quality*gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_quality)))
anova(between_subj_sleepquality_PS_model_intercept,between_subj_sleepquality_PS_model_sleep,between_subj_sleepquality_PS_model_gender,between_subj_sleepquality_PS_model_interaction) #sleep best
sjPlot::plot_model(between_subj_sleepquality_PS_model_sleep, type = "pred", show.data = T,title = "",axis.title = c("Subjective Sleep Quality","Parental Stress"),jitter=0.05)

#within sleep duration
within_subj_sleepduration_PS_model_intercept <- lmer(parental_stress_index ~ 1 + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(within_subj_sleepduration_hours)))
within_subj_sleepduration_PS_model_sleep <- lmer(parental_stress_index ~ within_subj_sleepduration_hours + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(within_subj_sleepduration_hours))) #sig
within_subj_sleepduration_PS_model_gender <- lmer(parental_stress_index ~ within_subj_sleepduration_hours + gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(within_subj_sleepduration_hours)))
within_subj_sleepduration_PS_model_interaction <- lmer(parental_stress_index ~ within_subj_sleepduration_hours*gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(within_subj_sleepduration_hours)))
anova(within_subj_sleepduration_PS_model_intercept,within_subj_sleepduration_PS_model_sleep,within_subj_sleepduration_PS_model_gender,within_subj_sleepduration_PS_model_interaction) #sleep best
sjPlot::plot_model(within_subj_sleepduration_PS_model_sleep, type = "pred", show.data = T,title = "",axis.title = c("Subjective Sleep Durations (hours)","Parental Stress"),jitter=0.02)

#between sleep duration
between_subj_sleepduration_PS_model_intercept <- lmer(parental_stress_index ~ 1 + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(between_subj_sleepduration_hours)))
between_subj_sleepduration_PS_model_sleep <- lmer(parental_stress_index ~ between_subj_sleepduration_hours + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(between_subj_sleepduration_hours))) 
between_subj_sleepduration_PS_model_gender <- lmer(parental_stress_index ~ between_subj_sleepduration_hours + gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(between_subj_sleepduration_hours)))
between_subj_sleepduration_PS_model_interaction <- lmer(parental_stress_index ~ between_subj_sleepduration_hours*gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(between_subj_sleepduration_hours)))
anova(between_subj_sleepduration_PS_model_intercept,between_subj_sleepduration_PS_model_sleep,between_subj_sleepduration_PS_model_gender,between_subj_sleepduration_PS_model_interaction) #intercept best

###################################################################
############### STRATEGY (YES/NO) -> stress/sleep #################
###################################################################

#parental stress
strategy_PS_model_intercept <- lmer(parental_stress_index ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_PS_model_sharedstrategy <- lmer(parental_stress_index ~ shared_strategy_exists + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_PS_model_gender <- lmer(parental_stress_index ~ shared_strategy_exists + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_PS_model_interaction <- lmer(parental_stress_index ~ shared_strategy_exists*gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
anova(strategy_PS_model_intercept,strategy_PS_model_sharedstrategy,strategy_PS_model_gender,strategy_PS_model_interaction) #intercept best

# objective sleep duration
strategy_obj_sleepduration_model_intercept <- lmer(actual_sleep_time ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_obj_sleepduration_model_sharedstrategy <- lmer(actual_sleep_time ~ shared_strategy_exists + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_obj_sleepduration_model_gender <- lmer(actual_sleep_time ~ shared_strategy_exists + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists))) #sig
strategy_obj_sleepduration_model_interaction <- lmer(actual_sleep_time ~ shared_strategy_exists*gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
anova(strategy_obj_sleepduration_model_intercept,strategy_obj_sleepduration_model_sharedstrategy,strategy_obj_sleepduration_model_gender,strategy_obj_sleepduration_model_interaction) #gender best
summary(strategy_obj_sleepduration_model_gender)
sjPlot::plot_model(strategy_obj_sleepduration_model_gender, terms = c("shared_strategy_exists","gender"), type = "emm", show.data = T,title = "",axis.title = c("Shared strategy?","Actigraphy Sleep Duration (mins)"),jitter=c(0.2,0),dodge = 0.5, dot.alpha = 0.15)

# objective sleep efficiency
strategy_obj_sleepefficiency_model_intercept <- lmer(sleep_efficiency_percent ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_obj_sleepefficiency_model_sharedstrategy <- lmer(sleep_efficiency_percent ~ shared_strategy_exists + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_obj_sleepefficiency_model_gender <- lmer(sleep_efficiency_percent ~ shared_strategy_exists + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_obj_sleepefficiency_model_interaction <- lmer(sleep_efficiency_percent ~ shared_strategy_exists*gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
anova(strategy_obj_sleepefficiency_model_intercept,strategy_obj_sleepefficiency_model_sharedstrategy,strategy_obj_sleepefficiency_model_gender,strategy_obj_sleepefficiency_model_interaction) #intercept best

#objective awakenings
strategy_obj_awakenings_model_intercept <- lmer(awakeNum_above_5min ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_obj_awakenings_model_sharedstrategy <- lmer(awakeNum_above_5min ~ shared_strategy_exists + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_obj_awakenings_model_gender <- lmer(awakeNum_above_5min ~ shared_strategy_exists + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_obj_awakenings_model_interaction <- lmer(awakeNum_above_5min ~ shared_strategy_exists*gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
anova(strategy_obj_awakenings_model_intercept,strategy_obj_awakenings_model_sharedstrategy,strategy_obj_awakenings_model_gender,strategy_obj_awakenings_model_interaction) #strategy best
summary(strategy_obj_awakenings_model_sharedstrategy)
sjPlot::plot_model(strategy_obj_awakenings_model_sharedstrategy, type = "pred", show.data = T,title = "",axis.title = c("Shared strategy?","Number of Awakenings (> 5 mins)"),jitter=c(0.2,0), colors = "bw",  dot.alpha = 0.15)

#subjective sleep duration
strategy_subj_sleepduration_model_intercept <- lmer(Subjective_sleep_minutes ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_subj_sleepduration_model_sharestrategy <- lmer(Subjective_sleep_minutes ~ shared_strategy_exists + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_subj_sleepduration_model_gender <- lmer(Subjective_sleep_minutes ~ shared_strategy_exists + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists))) #sig
strategy_subj_sleepduration_model_interaction <- lmer(Subjective_sleep_minutes ~ shared_strategy_exists*gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
anova(strategy_subj_sleepduration_model_intercept,strategy_subj_sleepduration_model_sharestrategy,strategy_subj_sleepduration_model_gender,strategy_subj_sleepduration_model_interaction) #gender best
summary(strategy_subj_sleepduration_model_gender)
sjPlot::plot_model(strategy_subj_sleepduration_model_gender, terms = c("shared_strategy_exists","gender"), type = "emm", show.data = T,title = "",axis.title = c("Shared strategy?","Subjective Sleep Duration (mins)"),jitter=c(0.2,0),dodge = 0.5, dot.alpha = 0.15)

#subjective sleep quality
strategy_subj_sleepquality_model_intercept <- lmer(sleep_quality ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_subj_sleepquality_model_sharedstrategy <- lmer(sleep_quality ~ shared_strategy_exists + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_subj_sleepquality_model_gender <- lmer(sleep_quality ~ shared_strategy_exists + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_subj_sleepquality_model_interaction <- lmer(sleep_quality ~ shared_strategy_exists*gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists))) #sig
anova(strategy_subj_sleepquality_model_intercept,strategy_subj_sleepquality_model_sharedstrategy,strategy_subj_sleepquality_model_gender,strategy_subj_sleepquality_model_interaction) #interaction best
summary(strategy_subj_sleepquality_model_interaction)
sjPlot::plot_model(strategy_subj_sleepquality_model_interaction, terms = c("shared_strategy_exists","gender"), type = "int", show.data = T,title = "",axis.title = c("Shared strategy?","Subjective Sleep Quality"),jitter=c(0.2,0.2),dodge = 0.5, dot.alpha = 0.15)

#subjective awakenings
strategy_subj_awakenings_model_intercept <- lmer(no_of_awakenings ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_subj_awakenings_model_sharedstrategy <- lmer(no_of_awakenings ~ shared_strategy_exists + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_subj_awakenings_model_gender <- lmer(no_of_awakenings ~ shared_strategy_exists + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists))) 
strategy_subj_awakenings_model_interaction <- lmer(no_of_awakenings ~ shared_strategy_exists*gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists))) #sig
anova(strategy_subj_awakenings_model_intercept,strategy_subj_awakenings_model_sharedstrategy,strategy_subj_awakenings_model_gender,strategy_subj_awakenings_model_interaction) #interaction best
summary(strategy_subj_awakenings_model_interaction)
sjPlot::plot_model(strategy_subj_awakenings_model_interaction, terms = c("shared_strategy_exists","gender"), type = "int", show.data = T,title = "",axis.title = c("Shared strategy?","Subjective Awakenings"),jitter=c(0.2,0.2),dodge = 0.5, dot.alpha = 0.15)

########################################################################
############### STRATEGIES (1,2,3,4,5) -> stress/sleep #################
########################################################################

#parental stress
beststrategy_PS_model_intercept <- lmer(parental_stress_index ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
beststrategy_PS_model_strategy <- lmer(parental_stress_index ~ strategy_factor + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists))) 
beststrategy_PS_model_gender <- lmer(parental_stress_index ~ strategy_factor + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
beststrategy_PS_model_interaction <- lmer(parental_stress_index ~ strategy_factor*gender + (1|ID) + (1|couple),parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists))) 
anova(beststrategy_PS_model_intercept,beststrategy_PS_model_strategy,beststrategy_PS_model_gender,beststrategy_PS_model_interaction) #intercept best

# objective sleep duration
beststrategy_obj_sleepduration_model_intercept <- lmer(actual_sleep_time ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
beststrategy_obj_sleepduration_model_strategy <- lmer(actual_sleep_time ~ strategy_factor + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
beststrategy_obj_sleepduration_model_gender <- lmer(actual_sleep_time ~ strategy_factor + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
beststrategy_obj_sleepduration_model_interaction <- lmer(actual_sleep_time ~ strategy_factor*gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
anova(beststrategy_obj_sleepduration_model_intercept,beststrategy_obj_sleepduration_model_strategy,beststrategy_obj_sleepduration_model_gender,beststrategy_obj_sleepduration_model_interaction) #gender best
summary(beststrategy_obj_sleepduration_model_gender)
anova(beststrategy_obj_sleepduration_model_gender)
emmeans(beststrategy_obj_sleepduration_model_gender, pairwise ~ strategy_factor)
sjPlot::plot_model(beststrategy_obj_sleepduration_model_gender,  type = "pred", show.data = T,title = "",axis.title = c("Gender","Objective Sleep duration"),jitter=c(0.2,0.2),dodge = 0.5, dot.alpha = 0.15)


# objective sleep efficiency
beststrategy_obj_sleepefficiency_model_intercept <- lmer(sleep_efficiency_percent ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
beststrategy_obj_sleepefficiency_model_strategy <- lmer(sleep_efficiency_percent ~ strategy_factor + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
beststrategy_obj_sleepefficiency_model_gender <- lmer(sleep_efficiency_percent ~ strategy_factor + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
beststrategy_obj_sleepefficiency_model_interaction <- lmer(sleep_efficiency_percent ~ strategy_factor*gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
anova(beststrategy_obj_sleepefficiency_model_intercept,beststrategy_obj_sleepefficiency_model_strategy,beststrategy_obj_sleepefficiency_model_gender,beststrategy_obj_sleepefficiency_model_interaction) #intercept best

#objective awakenings
beststrategy_obj_awakenings_model_intercept <- lmer(awakeNum_above_5min ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
beststrategy_obj_awakenings_model_strategy <- lmer(awakeNum_above_5min ~ strategy_factor + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists))) 
beststrategy_obj_awakenings_model_gender <- lmer(awakeNum_above_5min ~ strategy_factor + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
beststrategy_obj_awakenings_model_interaction <- lmer(awakeNum_above_5min ~ strategy_factor*gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
anova(beststrategy_obj_awakenings_model_intercept,beststrategy_obj_awakenings_model_strategy,beststrategy_obj_awakenings_model_gender,beststrategy_obj_awakenings_model_interaction) #strategy best
summary(beststrategy_obj_awakenings_model_strategy)
emmeans(beststrategy_obj_awakenings_model_strategy, pairwise ~ strategy_factor)
sjPlot::plot_model(beststrategy_obj_awakenings_model_strategy,  type = "pred", show.data = T,title = "",axis.title = c("Which strategy?","Objective Awakenings"),jitter=c(0.2,0.2),dodge = 0.5, dot.alpha = 0.15)


#subjective sleep duration
beststrategy_subj_sleepduration_model_intercept <- lmer(Subjective_sleep_minutes ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
beststrategy_subj_sleepduration_model_strategy <- lmer(Subjective_sleep_minutes ~ strategy_factor + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
beststrategy_subj_sleepduration_model_gender <- lmer(Subjective_sleep_minutes ~ strategy_factor + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
beststrategy_subj_sleepduration_model_interaction <- lmer(Subjective_sleep_minutes ~ strategy_factor*gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
anova(beststrategy_subj_sleepduration_model_intercept,beststrategy_subj_sleepduration_model_strategy,beststrategy_subj_sleepduration_model_gender,beststrategy_subj_sleepduration_model_interaction) #gender best
anova(beststrategy_subj_sleepduration_model_gender)
sjPlot::plot_model(beststrategy_subj_sleepduration_model_gender,  type = "pred", show.data = T,title = "",axis.title = c("Gender","Subjective Sleep Duration"),jitter=c(0.2,0.2),dodge = 0.5, dot.alpha = 0.15)


#subjective sleep quality
beststrategy_subj_sleepquality_model_intercept <- lmer(sleep_quality ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
beststrategy_subj_sleepquality_model_strategy <- lmer(sleep_quality ~ strategy_factor + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
beststrategy_subj_sleepquality_model_gender <- lmer(sleep_quality ~ strategy_factor + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists))) 
beststrategy_subj_sleepquality_model_interaction <- lmer(sleep_quality ~ strategy_factor*gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists))) 
anova(beststrategy_subj_sleepquality_model_intercept,beststrategy_subj_sleepquality_model_strategy,beststrategy_subj_sleepquality_model_gender,beststrategy_subj_sleepquality_model_interaction) #gender best
anova(beststrategy_subj_sleepquality_model_gender)
emmeans(beststrategy_subj_sleepquality_model_gender, pairwise ~ strategy_factor)
sjPlot::plot_model(beststrategy_subj_sleepquality_model_gender,  terms = c("strategy_factor","gender"), type = "emm", show.data = T,title = "",axis.title = c("Which strategy?","Subjective Sleep Quality"),jitter=c(0.2,0.2),dodge = 0.5, dot.alpha = 0.15)
sjPlot::plot_model(beststrategy_subj_sleepquality_model_gender,  terms = c("strategy_factor"), type = "pred", show.data = T,title = "",axis.title = c("Which strategy?","Subjective Sleep Quality"),jitter=c(0.2,0.2),dodge = 0.5, dot.alpha = 0.15)


#subjective awakenings
beststrategy_subj_awakenings_model_intercept <- lmer(no_of_awakenings ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
beststrategy_subj_awakenings_model_strategy <- lmer(no_of_awakenings ~ strategy_factor + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
beststrategy_subj_awakenings_model_gender <- lmer(no_of_awakenings ~ strategy_factor + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists))) 
beststrategy_subj_awakenings_model_interaction <- lmer(no_of_awakenings ~ strategy_factor*gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists))) 
anova(beststrategy_subj_awakenings_model_intercept,beststrategy_subj_awakenings_model_strategy,beststrategy_subj_awakenings_model_gender,beststrategy_subj_awakenings_model_interaction) #gender best
anova(beststrategy_subj_awakenings_model_gender)
emmeans(beststrategy_subj_awakenings_model_gender, pairwise ~ strategy_factor)
sjPlot::plot_model(beststrategy_subj_sleepquality_model_gender,  terms = c("strategy_factor","gender"), type = "emm", show.data = T,title = "",axis.title = c("Which strategy?","Subjective Awakenings"),jitter=c(0.2,0.2),dodge = 0.5, dot.alpha = 0.15)
sjPlot::plot_model(beststrategy_subj_sleepquality_model_gender,  terms = c("strategy_factor"), type = "pred", show.data = T,title = "",axis.title = c("Which strategy?","Subjective Awakenings"),jitter=c(0.2,0.2),dodge = 0.5, dot.alpha = 0.15)

####### comparing the sleep of the main care givers #######
x <- parentsleep2_data$parents_agree_strategy_dataset %>% 
  mutate(solely_responsible = if_else(strategy == 1, 1, 0),
         partner_responsible = if_else(strategy == 2, 1,0)) %>% 
  group_by(couple,gender) %>% 
  summarise(mean_parental_stress_index = mean(parental_stress_index, na.rm=T),
            mean_obj_sleep_minutes = mean(actual_sleep_time, na.rm=T),
            mean_obj_sleep_efficiency = mean(sleep_efficiency_percent, na.rm=T),
            mean_obj_awakenings = mean(awakeNum_above_5min, na.rm=T),
            mean_subj_sleep_minutes = mean(Subjective_sleep_minutes, na.rm=T),
            mean_subj_sleep_quality = mean(sleep_quality, na.rm=T),
            mean_subj_awakenings = mean(no_of_awakenings, na.rm=T),
            nights_of_data = n(),
            how_often_solely_responsible = sum(solely_responsible),
            how_often_partner_responsible = sum(partner_responsible),
            how_often_sharing_responsibility = sum(as.numeric(agreed_to_share)-1), #we have a couple that has 12 nights (couple 10), kind of a shame to throw it out so i haven't
            proportion_solely_responsible = how_often_solely_responsible/nights_of_data,
            proportion_partner_responsible = how_often_partner_responsible/nights_of_data,
            proportion_sharing_responsibility = how_often_sharing_responsibility/nights_of_data)
