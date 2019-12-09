
# library -----------------------------------------------------------------

library(dplyr)
library(data.table)
library(lubridate)
# library(AMR)
library(anytime)
# install.packages("naniar")
library(naniar)
library(lubridate)

# input -------------------------------------------------------------------
hh_path<- "data_analysis/input/20191128_HH Cleaned.csv"
ind_path<-"data_analysis/input/20191128_Indiv Cleaned.csv"
hh_data<-readr::read_csv(file = hh_path)
ind_data<-readr::read_csv(file=ind_path)%>% 
   mutate(individuals_unaccompanied = if_else(unaccompanied_relationship == "guest" |
                                                 unaccompanied_relationship == "don't_know","yes","no",NULL))
# hh_data <- readr::read_csv(hh_path, stringsAsFactors = FALSE, na.strings = c("", " "))
# ind_data <- read.csv(ind_path,stringsAsFactors = FALSE, na.strings = c("", " ")) %>% 
   # mutate(individuals_unaccompanied = if_else(unaccompanied_relationship == "guest" |
                                                 # unaccompanied_relationship == "don't_know","yes","no",NULL))
# hh_data <- read.csv("input/data/20191107_HH.csv")
# ind_data <- read.csv("input/data/20191107_Indiv.csv") %>% mutate(individuals_unaccompanied = if_else(unaccompanied_relationship == "guest" 
                                                                                                      # | unaccompanied_relationship == "don't_know","yes","no",NULL))

hh_data_yes_consent<-hh_data %>% filter(consent_overall == "yes")


hh_data_yes_consent$X_uuid <- hh_data_yes_consent$X_uuid %>% as.character()
ind_data$X_submission__uuid <- ind_data$X_submission__uuid %>% as.character()


hh_with_individual_level_data <- hh_data_yes_consent %>%  left_join(ind_data ,by= c("X_uuid"="X_submission__uuid"))

# household to household ----------------------------------------------------------------

disability_col_names <- c("i.disab_sight", "i.disab_hear", "i.disab_walk", "i.disab_concentrate", 
  "i.disab_selfcare", "i.disab_communicate")

coloum_numeric <- c("health_coping_strategy.use_private", 
  "health_coping_strategy.use_NGOclinic", "health_coping_strategy.travel_otherclinic", 
  "health_coping_strategy.self_medicate", "health_coping_strategy.avoid_help", 
  "health_coping_strategy.take_loan", "health_coping_strategy.private_financial_support", 
  "health_coping_strategy.borrow_money", "health_coping_strategy.sell_assets", 
  "health_coping_strategy.none", "health_coping_strategy.dont_know", 
  "health_coping_strategy.prefer_not",
  "risks_men.kidnapping","risks_women.kidnapping","risks_boys.kidnapping","risks_girls.kidnapping"
  )

health_coping_pay_yes <- c( "health_coping_strategy.use_private", 
                            "health_coping_strategy.take_loan", "health_coping_strategy.private_financial_support", 
                            "health_coping_strategy.borrow_money", "health_coping_strategy.sell_assets")

risk_kidnap_yes <- c("risks_men.kidnapping","risks_women.kidnapping","risks_boys.kidnapping","risks_girls.kidnapping")

risk_armedrecruit <- c("risks_men.armed_recruitment", "risks_women.armed_recruitment", 
  "risks_boys.armed_recruitment", "risks_girls.armed_recruitment")

distress_one_yes <- c("distress.social_withdraw", "distress.change_appetite", 
                      "distress.change_sleep", "distress.bedwetting", "distress.sad_mood", 
                      "distress.aggressive", "distress.physical_complaints")

drugabuse_issue <- c("big_issue", "minor_issue","not_issue")
drugabuse_nonissue <- c("minor_issue", "not_issue")

VAC_report_all <- c("VAC_report.report", "VAC_report.intervene", 
  "VAC_report.nothing", "VAC_report.other", "VAC_report.dont_know", 
  "VAC_report.prefer_not")

rplc_999_to_na<- c("expenditure_food","expenditure_health","expenditure_clothes")


hh_data_yes_consent[,rplc_999_to_na] <- hh_data_yes_consent[,rplc_999_to_na] %>% na_if(999)



hh_data_yes_consent[,coloum_numeric] <- sapply(hh_data_yes_consent[,coloum_numeric],as.numeric)

# sapply(hh_data[,coloum_numeric], class)



 hh_to_hh <- hh_data_yes_consent %>% mutate(
   
   i.disab_sight = if_else(WGQ_sight== "a_lot" | WGQ_sight == "no_sight", 1,0,NULL),
   i.disab_hear = if_else(WGQ_hearing == "a_lot" | WGQ_hearing == "no_hear",1,0,NULL),
   i.disab_walk = if_else(WGQ_walk == "a_lot" | WGQ_walk == "no_walk",1,0, NULL),
   i.disab_concentrate = if_else(WGQ_concentrate == "a_lot" | WGQ_concentrate == "no_walk",1,0,NULL),
   i.disab_selfcare = if_else(WGQ_selfcare == "a_lot" | WGQ_selfcare == "no_selfcare",1,0,NULL),
   i.disab_communicate = if_else(WGQ_communicate == "a_lot" | WGQ_communicate == "no_selfcare",1,0,NULL))
 
 
 hh_to_hh <- hh_to_hh %>% mutate(
    
   disability_col_names_rowsum = rowSums(hh_to_hh[,disability_col_names],na.rm = T),
   i.disability = if_else(disability_col_names_rowsum >0,1,0,NULL),
   i.arrive_bgd = if_else(dmy(arrive_bgd) < dmy("01/08/2017"),"Before Aug 2017",
                          if_else(dmy(arrive_bgd) >= dmy("01/08/2017"),"After Aug 2017","99999",NULL)),
   i.arrive_shelter = if_else(dmy(arrive_shelter) < dmy("01/08/2017"),"Before Aug 2017",
                          if_else(dmy(arrive_shelter) >= dmy("01/08/2017"),"After Aug 2017","99999",NULL)),
   
   health_coping_pay_yes_rowsum = rowSums(hh_to_hh[,health_coping_pay_yes]),
   i.health_coping_pay= if_else(health_coping_pay_yes_rowsum > 0,"yes","no",missing = NULL),
   risk_kidnap_rowsum =rowSums(hh_to_hh[,risk_kidnap_yes]),
   i.risk_kidnap = if_else(risk_kidnap_rowsum >0,"yes","no",NULL),
   risk_armedrecruit_rowsum = rowSums(hh_to_hh[,risk_armedrecruit]),
   i.risk_armedrecruit = if_else(risk_armedrecruit_rowsum>0,"yes","no",NULL),
   i.comm_watchgroups = if_else(watch_day== "yes" | watch_night == "yes","yes","no",NULL),
   distress_one_rowsum = rowSums(hh_to_hh[,distress_one_yes]),
   i.distress_one= if_else(distress_one_rowsum > 0, "yes","no",NULL),
   i.drugabuse_issue= if_else(drug_abuse %in% drugabuse_issue, "yes","no"),
   i.drugabuse_nonissue= if_else(drug_abuse %in% drugabuse_nonissue, "yes","no"),
   i.spent_food = if_else(expenditure_food >= 50,"yes","no",NULL),
   i.spent_health = if_else(expenditure_health >= 50,"yes","no",NULL),
   i.spent_clothes = if_else(expenditure_clothes >= 50,"yes","no",NULL),
   i.household_pregnant = if_else(pregnant_number > 0,"yes","no",NULL),
   i.shelter_lock_both = if_else(shelter_lock.inside == 1 & shelter_lock.outside == 1 ,"yes","no",NULL),
   i.shelter_lock_one =if_else(shelter_lock.inside == 1 | shelter_lock.outside == 1 ,"yes","no",NULL),
   VAC_report_rowsum = rowSums(hh_to_hh[,VAC_report_all]),
   i.VAC_report.report = if_else(VAC_report_rowsum == 1 & VAC_report.report == 1, "yes","No",NULL),
   i.hoh_gender = if_else(hoh == "yes",respondent_gender, hoh_gender, NULL),
   i.hoh_age = if_else(hoh == "yes", respondent_age, hoh_age, missing = NULL ),
   
 )
 
# ind_to_ind --------------------------------------------------------------
 
 infant_food_all<- c("infant_food.water", "infant_food.formula", 
   "infant_food.breast_milk", "infant_food.juice", "infant_food.cow_milk", 
   "infant_food.broth", "infant_food.yoghurt", "infant_food.porridge", 
   "infant_food.tea", "infant_food.solids", "infant_food.none")

 ind_to_ind <- ind_data %>% mutate(
    
    i.age_group = if_else(ind_age %in% 0:17, "0-17",
                          if_else(ind_age %in% 18:59,"18-59",
                                  if_else(ind_age >= 60,"60+","9999",NULL))),
    i.age_group2 = if_else(ind_age < 1, "0",
                          if_else(ind_age %in% 1:4,"1-4",
                                  if_else(ind_age %in% 5:11,"5-11",
                                          if_else(ind_age %in% 12:17,"12-17",
                                                  if_else(ind_age %in% 18:59,"18-59",
                                                          if_else(ind_age >=60,"60+","9999",NULL)))))),
    i.individuals_women_and_children = if_else(ind_gender== "female" | ind_age < 18,"yes","no",NULL),
    i.child_6months = if_else(ind_age_months <=	6,"yes","no",NULL),
    infant_food_rowSum = rowSums(ind_data[, infant_food_all]),
    i.infant_breastmilk_only = if_else( infant_food_rowSum == 1 & infant_food.breast_milk==1,"yes","no",NULL),
    i.child_schoolage = if_else(ind_age %in% 6:14,"yes","no",NULL),
    i.individuals_unaccompanied = individuals_unaccompanied,
    i.child_12months = if_else(ind_age == 0,"yes","no",NULL)
    
 )
 

# household to individual -------------------------------------------------

indi_household <- hh_with_individual_level_data %>% group_by(X_uuid) %>% 
   summarise(
     i.child_under5 = if_else(any(ind_age <= 4),"yes","no",NULL),
     i.male_over5 = if_else(any(ind_gender == "male" & ind_age >= 5),"yes","no",NULL),
     i.female_over5 = if_else(any(ind_gender == "female" & ind_age >= 5),"yes","no",NULL),
     i.female_over12 = if_else(any(ind_gender == "female" & ind_age >= 12),"yes","no",NULL),
     i.household_elderly_member = if_else(any(ind_age >= 60),"yes","no",NULL),
     i.household_seperatedchild = if_else(any(unaccompanied == "yes",na.rm = T),"yes","no",NULL),
     i.household_under18 = if_else(any(ind_age %in% 3:17),"yes","no",NULL),
     i.household_unaccompaniedchild =  if_else(any(individuals_unaccompanied == "yes",na.rm = T), "yes",
                                                    if_else(any(individuals_unaccompanied == "no"),"no","error",NULL))
     )

hh_with_composite<-left_join(hh_to_hh, indi_household, by="X_uuid")
indiv_with_composite<-ind_to_ind    
     
 
 