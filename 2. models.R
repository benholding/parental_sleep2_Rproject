#load packages
pacman::p_load(dplyr, broom, lme4, lmerTest, sjPlot, emmeans, ggpubr)

#load data
load("parentsleep2_data.RData")


###########################################################################################################################################
######################## AIM 1. how night-to-night sleep predict  parental stress for working mothers and fathers. ########################
###########################################################################################################################################

######### OBJECTIVE SLEEP -> parental stress ###########

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

tab_model(within_sleepduration_PS_model_sleep)
plot_within_sleepduration_PS_model_sleep <- sjPlot::plot_model(within_sleepduration_PS_model_sleep, type = "pred", terms = "within_subj_sleepduration_hours", show.data = T,title = "",axis.title = c("Actigraphy Sleep Duration (hours, within-subject mean centered)","Parental Stress")) +theme_sjplot()

#between subjects sleep duration
between_sleepduration_PS_model_intercept <- lmer(parental_stress_index ~ 1 + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(actual_sleep_time)))
between_sleepduration_PS_model_sleep <- lmer(parental_stress_index ~ between_subj_sleepduration_hours + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(actual_sleep_time))) 
between_sleepduration_PS_model_gender <- lmer(parental_stress_index ~ between_subj_sleepduration_hours + gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(actual_sleep_time)))
between_sleepduration_PS_model_interaction <- lmer(parental_stress_index ~ between_subj_sleepduration_hours*gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(actual_sleep_time)))
between_sleepduration_PS_all_models <- tidy(anova(between_sleepduration_PS_model_intercept,between_sleepduration_PS_model_sleep,between_sleepduration_PS_model_gender,between_sleepduration_PS_model_interaction)) #intercept best
write.csv(between_sleepduration_PS_all_models, "tables/S4. parentalstress_between_subj_sleepduration.csv")

########## SUBJECTIVE SLEEP -> parental stress ##########

#Subjective awakenings
subj_awakenings_PS_model_intercept <- lmer(parental_stress_index ~ 1 + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(no_of_awakenings)))
subj_awakenings_PS_model_sleep <- lmer(parental_stress_index ~ no_of_awakenings + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(no_of_awakenings)))
subj_awakenings_PS_model_gender <- lmer(parental_stress_index ~ no_of_awakenings + gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(no_of_awakenings)))
subj_awakenings_PS_model_interaction <- lmer(parental_stress_index ~ no_of_awakenings*gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(no_of_awakenings)))
subjectivewake_PS_all_models <- tidy(anova(subj_awakenings_PS_model_intercept,subj_awakenings_PS_model_sleep,subj_awakenings_PS_model_gender,subj_awakenings_PS_model_interaction)) #intercept best
write.csv(subjectivewake_PS_all_models, "tables/S5. parentalstress_subjective_awakenings.csv")

#within subjects sleep quality
within_subj_sleepquality_PS_model_intercept <- lmer(parental_stress_index ~ 1 + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_quality)))
within_subj_sleepquality_PS_model_sleep <- lmer(parental_stress_index ~ within_subj_sleep_quality + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_quality))) #sig
within_subj_sleepquality_PS_model_gender <- lmer(parental_stress_index ~ within_subj_sleep_quality + gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_quality)))
within_subj_sleepquality_PS_model_interaction <- lmer(parental_stress_index ~ within_subj_sleep_quality*gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_quality)))
within_subj_sleepquality_PS_all_models<- tidy(anova(within_subj_sleepquality_PS_model_intercept,within_subj_sleepquality_PS_model_sleep,within_subj_sleepquality_PS_model_gender,within_subj_sleepquality_PS_model_interaction)) #sleep best
write.csv(within_subj_sleepquality_PS_all_models, "tables/S6. parentalstress_within_subjective_sleepquality.csv")

tab_model(within_subj_sleepquality_PS_model_sleep)
plot_within_subj_sleepquality_PS_model_sleep <- sjPlot::plot_model(within_subj_sleepquality_PS_model_sleep, type = "pred",terms = "within_subj_sleep_quality", show.data = T,title = "",axis.title = c("Subjective Sleep Quality (within-subject mean centered)","Parental Stress"),jitter=0.05)+theme_sjplot()

#between subjects sleep quality
between_subj_sleepquality_PS_model_intercept <- lmer(parental_stress_index ~ 1 + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_quality)))
between_subj_sleepquality_PS_model_sleep <- lmer(parental_stress_index ~ between_subj_sleep_quality + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_quality))) #sig
between_subj_sleepquality_PS_model_gender <- lmer(parental_stress_index ~ between_subj_sleep_quality + gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_quality)))
between_subj_sleepquality_PS_model_interaction <- lmer(parental_stress_index ~ between_subj_sleep_quality*gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(sleep_quality)))
between_subj_sleepquality_PS_all_models <- tidy(anova(between_subj_sleepquality_PS_model_intercept,between_subj_sleepquality_PS_model_sleep,between_subj_sleepquality_PS_model_gender,between_subj_sleepquality_PS_model_interaction)) #sleep best
write.csv(between_subj_sleepquality_PS_all_models, "tables/S7. parentalstress_between_subjective_sleepquality.csv")

tab_model(between_subj_sleepquality_PS_model_sleep)
plot_between_subj_sleepquality_PS_model_sleep <- sjPlot::plot_model(between_subj_sleepquality_PS_model_sleep, type = "pred",terms =  "between_subj_sleep_quality", show.data = T,title = "",axis.title = c("Subjective Sleep Quality (participant mean)","Parental Stress"),jitter=0.05) +theme_sjplot()

#within sleep duration
within_subj_subjectivesleepduration_PS_model_intercept <- lmer(parental_stress_index ~ 1 + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(within_subj_Subjective_sleep_hours)))
within_subj_subjectivesleepduration_PS_model_sleep <- lmer(parental_stress_index ~ within_subj_Subjective_sleep_hours + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(within_subj_Subjective_sleep_hours))) #sig
within_subj_subjectivesleepduration_PS_model_gender <- lmer(parental_stress_index ~ within_subj_Subjective_sleep_hours + gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(within_subj_Subjective_sleep_hours)))
within_subj_subjectivesleepduration_PS_model_interaction <- lmer(parental_stress_index ~ within_subj_Subjective_sleep_hours*gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(within_subj_Subjective_sleep_hours)))
within_subj_subjectivesleepduration_PS_all_models <- tidy(anova(within_subj_subjectivesleepduration_PS_model_intercept,within_subj_subjectivesleepduration_PS_model_sleep,within_subj_subjectivesleepduration_PS_model_gender,within_subj_subjectivesleepduration_PS_model_interaction)) #sleep best
write.csv(within_subj_subjectivesleepduration_PS_all_models, "tables/S8. parentalstress_within_subjective_sleepduration.csv")

tab_model(within_subj_subjectivesleepduration_PS_model_sleep)
plot_within_subj_subjectivesleepduration_PS_model_sleep <- sjPlot::plot_model(within_subj_subjectivesleepduration_PS_model_sleep, type = "pred", terms = "within_subj_Subjective_sleep_hours",show.data = T,title = "",axis.title = c("Subjective Sleep Duration (hours, within-subject mean centered)","Parental Stress"),jitter=0.02) + theme_sjplot()

#between sleep duration
between_subj_subjectivesleepduration_PS_model_intercept <- lmer(parental_stress_index ~ 1 + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(between_subj_sleepduration_hours)))
between_subj_subjectivesleepduration_PS_model_sleep <- lmer(parental_stress_index ~ between_subj_Subjective_sleep_hours + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(between_subj_sleepduration_hours))) 
between_subj_subjectivesleepduration_PS_model_gender <- lmer(parental_stress_index ~ between_subj_Subjective_sleep_hours + gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(between_subj_sleepduration_hours)))
between_subj_subjectivesleepduration_PS_model_interaction <- lmer(parental_stress_index ~ between_subj_Subjective_sleep_hours*gender + (1|ID) + (1|couple), parentsleep2_data$dataset_for_analysis %>% filter(!is.na(between_subj_sleepduration_hours)))
between_subj_subjectivesleepduration_PS_all_models <- tidy(anova(between_subj_subjectivesleepduration_PS_model_intercept,between_subj_subjectivesleepduration_PS_model_sleep,between_subj_subjectivesleepduration_PS_model_gender,between_subj_subjectivesleepduration_PS_model_interaction)) #intercept best
write.csv(between_subj_subjectivesleepduration_PS_all_models, "tables/S9. parentalstress_between_subjective_sleepduration.csv")

#making an overall table of the significant predictors of parental stress
tab_model(within_sleepduration_PS_model_sleep,
          within_subj_sleepquality_PS_model_sleep,
          between_subj_sleepquality_PS_model_sleep,
          within_subj_subjectivesleepduration_PS_model_sleep)

tab_model(within_sleepduration_PS_model_sleep,
          within_subj_sleepquality_PS_model_sleep,
          between_subj_sleepquality_PS_model_sleep,
          within_subj_subjectivesleepduration_PS_model_sleep, file = "tables/1. significant predictors of parental stress.doc")

sleep_parentalstress_plots <- ggarrange(plot_within_sleepduration_PS_model_sleep, 
          plot_within_subj_subjectivesleepduration_PS_model_sleep, 
          plot_between_subj_sleepquality_PS_model_sleep,
          plot_within_subj_sleepquality_PS_model_sleep)
ggsave(sleep_parentalstress_plots, filename = "plots/figure1. sleep and parental stress.pdf")

#################################################################################################################################
######################## AIM 2a. Does having a strategy at all improve sleep and/or decrease parental stress #######################
#################################################################################################################################

############### STRATEGY (YES/NO) -> stress/sleep #################

#parental stress
strategy_PS_model_intercept <- lmer(parental_stress_index ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_PS_model_sharedstrategy <- lmer(parental_stress_index ~ shared_strategy_exists + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_PS_model_gender <- lmer(parental_stress_index ~ shared_strategy_exists + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_PS_model_interaction <- lmer(parental_stress_index ~ shared_strategy_exists*gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_PS_all_models <- tidy(anova(strategy_PS_model_intercept,strategy_PS_model_sharedstrategy,strategy_PS_model_gender,strategy_PS_model_interaction)) #strategy best (not sig. but it's of interest so)
write.csv(strategy_PS_all_models, "tables/S10. strategy_parentalstress.csv")

summary(strategy_PS_model_sharedstrategy)
tab_model(strategy_PS_model_sharedstrategy)
plot_strategy_PS_model_sharedstrategy <- sjPlot::plot_model(strategy_PS_model_sharedstrategy, terms = "shared_strategy_exists", type = "emm", show.data = T,title = "",axis.title = c("Shared strategy?","Parental stress"),jitter=c(0.2,0),dodge = 0.5, dot.alpha = 0.15) + theme_sjplot()

#objective awakenings
strategy_obj_awakenings_model_intercept <- glmer(awakeNum_above_5min ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)), family = poisson())
strategy_obj_awakenings_model_sharedstrategy <- glmer(awakeNum_above_5min ~ shared_strategy_exists + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)), family = poisson())
strategy_obj_awakenings_model_gender <- glmer(awakeNum_above_5min ~ shared_strategy_exists + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)), family = poisson())
strategy_obj_awakenings_model_interaction <- glmer(awakeNum_above_5min ~ shared_strategy_exists*gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)), family = poisson())
strategy_obj_awakenings_all_models <- tidy(anova(strategy_obj_awakenings_model_intercept,strategy_obj_awakenings_model_sharedstrategy,strategy_obj_awakenings_model_gender,strategy_obj_awakenings_model_interaction)) #strategy best
write.csv(strategy_obj_awakenings_all_models, "tables/S11. strategy_objective_awakenings.csv")

summary(strategy_obj_awakenings_model_interaction)
tab_model(strategy_obj_awakenings_model_interaction)
plot_strategy_obj_awakenings_model_sharedstrategy <- sjPlot::plot_model(strategy_obj_awakenings_model_interaction, terms = c("shared_strategy_exists","gender"), type = "int", show.data = T,title = "",axis.title = c("Shared strategy?","Number of Awakenings (> 5 mins)"),jitter=c(0.2,0),  dot.alpha = 0.15) + theme_sjplot()

#subjective awakenings
strategy_subj_awakenings_model_intercept <- glmer(no_of_awakenings ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)), family = poisson())
strategy_subj_awakenings_model_sharedstrategy <- glmer(no_of_awakenings ~ shared_strategy_exists + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)), family = poisson())
strategy_subj_awakenings_model_gender <- glmer(no_of_awakenings ~ shared_strategy_exists + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)), family = poisson()) 
strategy_subj_awakenings_model_interaction <- glmer(no_of_awakenings ~ shared_strategy_exists*gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)), family = poisson()) #sig
strategy_subj_awakenings_all_models <- tidy(anova(strategy_subj_awakenings_model_intercept,strategy_subj_awakenings_model_sharedstrategy,strategy_subj_awakenings_model_gender,strategy_subj_awakenings_model_interaction)) #interaction best
write.csv(strategy_subj_awakenings_all_models, "tables/S15. strategy_subjective_awakenings.csv")

tab_model(strategy_subj_awakenings_model_gender)
summary(strategy_subj_awakenings_model_gender)
sjPlot::plot_model(strategy_subj_awakenings_model_gender, terms = c("shared_strategy_exists","gender"), type = "emm", show.data = T,title = "",axis.title = c("Shared strategy?","Subjective Awakenings"),jitter=c(0.2,0.2),dodge = 0.5, dot.alpha = 0.15) + theme_sjplot()

# objective sleep efficiency
strategy_obj_sleepefficiency_model_intercept <- lmer(sleep_efficiency_percent ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_obj_sleepefficiency_model_sharedstrategy <- lmer(sleep_efficiency_percent ~ shared_strategy_exists + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_obj_sleepefficiency_model_gender <- lmer(sleep_efficiency_percent ~ shared_strategy_exists + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_obj_sleepefficiency_model_interaction <- lmer(sleep_efficiency_percent ~ shared_strategy_exists*gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_obj_sleepefficiency_all_models <- tidy(anova(strategy_obj_sleepefficiency_model_intercept,strategy_obj_sleepefficiency_model_sharedstrategy,strategy_obj_sleepefficiency_model_gender,strategy_obj_sleepefficiency_model_interaction)) #intercept best
write.csv(strategy_obj_sleepefficiency_all_models, "tables/S12. strategy_objective_sleep_efficiency.csv")

# within-subjects objective sleep duration
strategy_within_obj_sleepduration_model_intercept <- lmer(within_subj_sleepduration_hours ~ 1 + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_within_obj_sleepduration_model_sharedstrategy <- lmer(within_subj_sleepduration_hours ~ shared_strategy_exists + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_within_obj_sleepduration_model_gender <- lmer(within_subj_sleepduration_hours ~ shared_strategy_exists + gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists))) #sig
strategy_within_obj_sleepduration_model_interaction <- lmer(within_subj_sleepduration_hours ~ shared_strategy_exists*gender + (1|ID) + (1|couple), parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_within_obj_sleepduration_all_models <- tidy(anova(strategy_within_obj_sleepduration_model_intercept,strategy_within_obj_sleepduration_model_sharedstrategy,strategy_within_obj_sleepduration_model_gender,strategy_within_obj_sleepduration_model_interaction)) #gender best
write.csv(strategy_within_obj_sleepduration_all_models, "tables/S13. strategy_objective_sleep_duration_within.csv")

#subjective sleep duration (within)
strategy_within_subj_sleepduration_model_intercept <- lm(within_subj_Subjective_sleep_hours ~ 1, parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_within_subj_sleepduration_model_sharestrategy <- lm(within_subj_Subjective_sleep_hours ~ shared_strategy_exists, parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_within_subj_sleepduration_model_gender <- lm(within_subj_Subjective_sleep_hours ~ shared_strategy_exists + gender, parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists))) #sig
strategy_within_subj_sleepduration_model_interaction <- lm(within_subj_Subjective_sleep_hours ~ shared_strategy_exists*gender, parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_within_subj_sleepduration_all_models <- tidy(anova(strategy_within_subj_sleepduration_model_intercept,strategy_within_subj_sleepduration_model_sharestrategy,strategy_within_subj_sleepduration_model_gender,strategy_within_subj_sleepduration_model_interaction)) #gender best
write.csv(strategy_within_subj_sleepduration_all_models, "tables/S18. strategy_subjective_sleepduration_within.csv")

#subjective sleep quality (within)
strategy_within_subj_sleepquality_model_intercept <- lm(within_subj_sleep_quality ~ 1, parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_within_subj_sleepquality_model_sharedstrategy <- lm(within_subj_sleep_quality ~ shared_strategy_exists, parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_within_subj_sleepquality_model_gender <- lm(within_subj_sleep_quality ~ shared_strategy_exists + gender, parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)))
strategy_within_subj_sleepquality_model_interaction <- lm(within_subj_sleep_quality ~ shared_strategy_exists*gender, parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists))) #sig
strategy_within_subj_sleepquality_all_models <- tidy(anova(strategy_within_subj_sleepquality_model_intercept,strategy_within_subj_sleepquality_model_sharedstrategy,strategy_within_subj_sleepquality_model_gender,strategy_within_subj_sleepquality_model_interaction)) 
write.csv(strategy_within_subj_sleepquality_all_models, "tables/S16. strategy_sleepquality_within.csv")

data_for_between_subs_analysis_sharedstrategy_exists <- parentsleep2_data$parents_agree_strategy_dataset %>% filter(!is.na(shared_strategy_exists)) %>% group_by(ID) %>% summarise(gender = first(gender),between_subj_sleepduration_hours = first(between_subj_sleepduration_hours), between_subj_sleep_quality = first(between_subj_sleep_quality), between_subj_Subjective_sleep_hours = first(between_subj_Subjective_sleep_hours), n_nights = n(), n_nights_shared_strategy = sum(as.numeric(shared_strategy_exists)-1)) %>% mutate(prop_nights_shared_strategy = if_else(n_nights_shared_strategy == 0, 0, n_nights_shared_strategy/n_nights))

#between-subjects objective sleep duration
strategy_between_obj_sleepduration_model_intercept <- lm(between_subj_sleepduration_hours ~ 1, data_for_between_subs_analysis_sharedstrategy_exists)
strategy_between_obj_sleepduration_model_sharedstrategy <- lm(between_subj_sleepduration_hours ~ prop_nights_shared_strategy, data_for_between_subs_analysis_sharedstrategy_exists)
strategy_between_obj_sleepduration_model_gender <- lm(between_subj_sleepduration_hours ~ prop_nights_shared_strategy + gender, data_for_between_subs_analysis_sharedstrategy_exists) #sig
strategy_between_obj_sleepduration_model_interaction <- lm(between_subj_sleepduration_hours ~ prop_nights_shared_strategy*gender, data_for_between_subs_analysis_sharedstrategy_exists)
strategy_between_obj_sleepduration_all_models <- tidy(anova(strategy_between_obj_sleepduration_model_intercept,strategy_between_obj_sleepduration_model_sharedstrategy,strategy_between_obj_sleepduration_model_gender,strategy_between_obj_sleepduration_model_interaction)) #gender best
write.csv(strategy_between_obj_sleepduration_all_models, "tables/S14. strategy_objective_sleep_duration_between.csv")

summary(strategy_between_obj_sleepduration_model_gender)
tab_model(strategy_between_obj_sleepduration_model_gender)
plot_strategy_between_obj_sleepduration_model_gender <- sjPlot::plot_model(strategy_between_obj_sleepduration_model_gender, terms = c("prop_nights_shared_strategy","gender"), type = "pred", show.data = T,title = "",axis.title = c("Shared strategy?","Objective Sleep Duration (hours, participant mean)"),jitter=c(0.2,0),dodge = 0.5, dot.alpha = 0.15) + theme_sjplot()

#subjective sleep quality (between)
strategy_between_subj_sleepquality_model_intercept <- lm(between_subj_sleep_quality ~ 1, data_for_between_subs_analysis_sharedstrategy_exists)
strategy_between_subj_sleepquality_model_sharedstrategy <- lm(between_subj_sleep_quality ~ prop_nights_shared_strategy, data_for_between_subs_analysis_sharedstrategy_exists)
strategy_between_subj_sleepquality_model_gender <- lm(between_subj_sleep_quality ~ prop_nights_shared_strategy + gender, data_for_between_subs_analysis_sharedstrategy_exists)#sig
strategy_between_subj_sleepquality_model_interaction <- lm(between_subj_sleep_quality ~ prop_nights_shared_strategy*gender, data_for_between_subs_analysis_sharedstrategy_exists) 
strategy_between_subj_sleepquality_all_models <- tidy(anova(strategy_between_subj_sleepquality_model_intercept,strategy_between_subj_sleepquality_model_sharedstrategy,strategy_between_subj_sleepquality_model_gender,strategy_between_subj_sleepquality_model_interaction)) 
write.csv(strategy_within_subj_sleepquality_all_models, "tables/S17. strategy_sleepquality_between.csv")

summary(strategy_between_subj_sleepquality_model_gender)
plot_strategy_between_obj_sleepduration_model_gender <- sjPlot::plot_model(strategy_between_subj_sleepquality_model_gender, terms = c("prop_nights_shared_strategy", "gender"), type = "pred", show.data = T,title = "",axis.title = c("Shared strategy?","Objective Sleep Duration (hours, participant mean)"),jitter=c(0.2,0),dodge = 0.5, dot.alpha = 0.15) + theme_sjplot()

#subjective sleep duration (between)
strategy_between_subj_sleepduration_model_intercept <- lm(between_subj_sleepduration_hours ~ 1, data_for_between_subs_analysis_sharedstrategy_exists)
strategy_between_subj_sleepduration_model_sharestrategy <- lm(between_subj_sleepduration_hours ~ prop_nights_shared_strategy, data_for_between_subs_analysis_sharedstrategy_exists)
strategy_between_subj_sleepduration_model_gender <- lm(between_subj_sleepduration_hours ~ prop_nights_shared_strategy + gender, data_for_between_subs_analysis_sharedstrategy_exists)
strategy_between_subj_sleepduration_model_interaction <- lm(between_subj_sleepduration_hours ~ prop_nights_shared_strategy*gender, data_for_between_subs_analysis_sharedstrategy_exists)
strategy_between_subj_sleepduration_all_models <- tidy(anova(strategy_between_subj_sleepduration_model_intercept,strategy_between_subj_sleepduration_model_sharestrategy,strategy_between_subj_sleepduration_model_gender,strategy_between_subj_sleepduration_model_interaction)) #gender best
write.csv(strategy_between_subj_sleepduration_all_models, "tables/S19. strategy_subjective_sleepduration_between.csv")

summary(strategy_between_subj_sleepduration_model_gender)
sjPlot::plot_model(strategy_between_subj_sleepduration_model_gender, terms = c("prop_nights_shared_strategy","gender"), type = "pred", show.data = T,title = "",axis.title = c("Shared strategy?","Subjective Sleep Duration (hours, participant mean)"),jitter=c(0.2,0),dodge = 0.5, dot.alpha = 0.15) + theme_sjplot()

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

