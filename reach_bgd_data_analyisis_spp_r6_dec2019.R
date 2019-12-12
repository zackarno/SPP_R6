# SPP R6 Data Analysis
# REACH BGD GIS & Data Unit
# 20191209


library(srvyr)
library(dplyr)
library(readr)
library(hypegrammaR)
source("util_functions/make_composite_indicators_SPP_R6_2019.R")
source("util_functions/activate_object_names.R")






#LATEST POP DATA FROM UNHCR FOR WEIGHTING
pop<- read.csv("data_analysis/input/unhcr_pop_stats_20190931.csv", stringsAsFactors = FALSE, na.strings=c("", " ", NA))

#LOAD DAP
sppdap<-read_csv(file = "data_analysis/input/dap_butter_template.csv")


# QUICK CHECKS ON DATA AND DAP (LOOKS GOOD)----------------------------------------

duplicated(hh_with_composite$X_uuid) %>% which()
hh_with_composite$X_uuid %in% indiv_with_composite$X_submission__uuid ==FALSE %>% which()
indiv_with_composite$X_submission__uuid%in% hh_with_composite$X_uuid==FALSE %>% which()

all_colnames<-c(colnames(hh_with_composite), colnames(indiv_with_composite))
sppdap$variable<-sppdap$variable %>% trimws()
sppdap$variable[which(sppdap$variable %in% all_colnames==FALSE)] %>% dput()
na.omit(sppdap$`subset 1`)[which(na.omit(sppdap$`subset 1`) %in% all_colnames==FALSE)] %>% dput()
na.omit(sppdap$`subset 2`)[which(na.omit(sppdap$`subset 2`) %in% all_colnames==FALSE)] %>% dput()
sppdap<-sppdap %>% filter(variable %in% all_colnames==TRUE)




# WEIGHT DATA -------------------------------------------------------------

pop<-pop %>% 
  filter(!is.na(Camp)& is.na(Block)) %>% 
  mutate(
    camp_id=stringr::str_replace(Camp, "Total","") %>% trimws(),
    Total.Families=readr::parse_number(Total.Families)
  ) %>% 
  select(camp_id, Total.Families,Total.Individuals)

#MAKE SURE STRATA IN SF AND DF MATCH
pop$camp_id[pop$camp_id %in% hh_with_composite[[strata]]==FALSE]
hh_with_composite %>% filter(is.na(!!sym(strata)))


weighting<-hypegrammaR::map_to_weighting(sampling.frame = pop, 
                            data.stratum.column = strata,
                            data = hh_with_composite, 
                            sampling.frame.population.column =sf_pop,
                            sampling.frame.stratum.column = sf_strata)

# MAKE HH DESIGN
HH_svy_ob<-map_to_design(data=hh_with_composite, weighting_function = weighting)

# BRING RELEVANT DATA TO INDIVIDUAL REPEAT
indiv_with_composite_and_weights<-indiv_with_composite %>% 
  left_join(data.frame(HH_svy_ob$variables,
                       weight=weights(HH_svy_ob)) %>%
              select(instance_name,respondent_gender,strata,weight),
            by= c("parent_instance_name"="instance_name"))

# MAKE INDIVIDUAL DESIGN
ind_svy_ob<-survey::svydesign(ids = ~ 1,
                              strata =  formula(paste0("~",strata)),
                              weights= ~weight,
                              data = indiv_with_composite_and_weights)



# HH ANALYSIS -------------------------------------------------------------

#VARIABLES IN DAP FOR HH ANALYSIS
sppdap_hh<-sppdap %>% filter(level=="household")

# LOOK AT VARS WHERE THEY WANT TO TO REPLACE NA RATHER THAN SUBSET
sppdap_na_replace<-sppdap %>% filter(!is.na(na_replace)& na_replace!="True NA" )
sppdap_na_replace

butteR::auto_detect_select_multiple()
# since there are only 5 cases and each case  NA  = "no" I am just going too replace it in the data set
HH_svy_ob$variables[,unique(sppdap_na_replace$variable)]<-purrr::map( HH_svy_ob$variables[,unique(sppdap_na_replace$variable)], function(x)ifelse(is.na(x), "no",x))

#THEY WANT CELLPHONE NUMBER (INTEGER) TO BE TREATED AS CATEGORICAL
HH_svy_ob$variables$cellphone_function <-as.character(HH_svy_ob$variables$cellphone_function)

#USE butteR to PERFORM ANALYSIS
hh_dap_analysis_simple<-list() # FILL THIS LIST

#OVERALL
hh_dap_analysis_simple$overall_na_replace_false<-butteR::mean_proportion_table(design = HH_svy_ob,list_of_variables = sppdap_hh$variable,aggregation_level = NULL,round_to = 2,return_confidence = FALSE,na_replace = FALSE)%>%
  mutate(level="overall", level_value=NA) %>% select(level,level_value, everything()) 

#BY CAMP
hh_dap_analysis_simple$by_camp_na_replace_false<-butteR::mean_proportion_table(design = HH_svy_ob,list_of_variables = sppdap_hh$variable,aggregation_level = strata,round_to = 2,return_confidence = FALSE,na_replace = FALSE)%>% 
  mutate(level="camp")%>% select(level,level_value=camp, everything())

#BY REPSONDENT GENDER
hh_dap_analysis_simple$by_resp_gender_na_replace_false<-butteR::mean_proportion_table(design = HH_svy_ob,list_of_variables = sppdap_hh$variable,aggregation_level = "respondent_gender",round_to = 2,return_confidence = FALSE,na_replace = FALSE)%>%
  mutate(level="respondent_gender")%>% select(level,level_value=respondent_gender, everything())

#BIND 3 ANALYSES TOGETHER AND WRITE TO CSV
hh_simple_dap<-bind_rows(hh_dap_analysis_simple)
write.csv(hh_simple_dap,paste0(output_path,isodate,"_HH_Analysis_Basic.csv"))



# HH SUBSETS -------------------------------------------------------

#single subsets - overall and by camp
dap_hh_subsets<-sppdap_hh %>% filter(!is.na(`subset 1`))

#SPLIT SUBSET DAP UP BY SUBSETS
dap_hh_subsets_l<-split(dap_hh_subsets,dap_hh_subsets$`subset 1`)

#OVERALL
hh_single_subset_list<-list() # TO FILL
for(i in 1: length(dap_hh_subsets_l)){
  dap_temp<-dap_hh_subsets_l[[i]]
  single_subset_temp<-unique(dap_temp$`subset 1`)
  var_list_temp<-dap_temp$variable
  hh_single_subset_list[[single_subset_temp]]<-butteR::mean_proportion_table(design = HH_svy_ob,list_of_variables = var_list_temp,aggregation_level = single_subset_temp,round_to = 2,return_confidence = FALSE,na_replace = FALSE) %>% 
    mutate(level="overall",level_value=NA) %>% select(level, level_value, everything())
}

#BY CAMP
hh_single_subset_list_by_camp<-list()
for(i in 1: length(dap_hh_subsets_l)){
  dap_temp<-dap_hh_subsets_l[[i]]
  single_subset_temp<-unique(dap_temp$`subset 1`)
  var_list_temp<-dap_temp$variable
  hh_single_subset_list_by_camp[[single_subset_temp]]<-butteR::mean_proportion_table(design = HH_svy_ob,list_of_variables = var_list_temp,aggregation_level = c(strata,single_subset_temp),round_to = 2,return_confidence = FALSE,na_replace = FALSE)%>% 
    mutate(level="camp") %>% select(level, level_value=camp, everything())
}

#BIND TOGETHER OVERALL SUBSETS
hh_single_subset_overall_list_reformatted<-purrr::map(hh_single_subset_list, function(x) tidyr::gather(x, key="subset",value="subset_value",colnames(x)[3]) %>% mutate(subset_value=as.character(subset_value)) %>% select(level,level_value,subset, subset_value,everything())) 
hh_single_subset_overall_df<-bind_rows(hh_single_subset_overall_list_reformatted)

#BIND BY CAMP TOGETHER BY CAMP SUBSETS
hh_single_subset_by_camp_list_reformatted<-purrr::map(hh_single_subset_list_by_camp, function(x) tidyr::gather(x, key="subset",value="subset_value",colnames(x)[3]) %>% mutate(subset_value=as.character(subset_value)) %>% select(level,level_value,subset, subset_value,everything())) 
hh_single_subset_by_camp_df<-bind_rows(hh_single_subset_by_camp_list_reformatted)

#BIND TOGETHER ALL SUBSETS & WRITE CSV
hh_single_subsets_all<-bind_rows(list(hh_single_subset_overall_df,hh_single_subset_by_camp_df))
write.csv(hh_single_subsets_all,paste0(output_path,isodate,"_HH_Analysis_Single_Subset.csv"))


# HH DOUBLE SUBSET --------------------------------------------------------

dap_hh_double_subsets<-sppdap_hh %>% filter(!is.na(`subset 2`))
dap_hh_double_subsets
 
#THERE ARE ONLY TWO SO INSTEAD OF ITERATING, I WILL JUST DO IT THE SEPARATE
double_subset1_overall<-butteR::mean_proportion_table(design = HH_svy_ob,list_of_variables = "health_coping_strategy", aggregation_level = c("i.household_elderly_member","health_coping"),round_to = 2,return_confidence = FALSE,na_replace = FALSE) 
double_subset1_by_camp<-butteR::mean_proportion_table(design = HH_svy_ob,list_of_variables = "health_coping_strategy", aggregation_level = c(strata,"i.household_elderly_member","health_coping"),round_to = 2,return_confidence = FALSE,na_replace = FALSE) 
#REFORMAT
double_subsets_to_bind<-list()
double_subsets_to_bind$hh_double_subset1_overall_reformatted<-double_subset1_overall %>% 
  tidyr::gather(key="subset_1",value="subset_1_value", i.household_elderly_member) %>%
  tidyr::gather(key= "subset_2", value="subset_2_value",health_coping) %>%
  mutate(level="overall",level_value=NA) %>% 
  select(level, level_value,subset_1: subset_2_value,everything())    

double_subsets_to_bind$hh_double_subset1_by_camp_reformatted<-double_subset1_by_camp %>% 
  tidyr::gather(key="subset_1",value="subset_1_value", i.household_elderly_member) %>%
  tidyr::gather(key= "subset_2", value="subset_2_value",health_coping) %>% 
  mutate(level="camp") %>%rename(level_value=camp) %>% 
  select(level,level_value, subset_1: subset_2_value,everything())    

#BY CAMP
double_subset2_overall<-butteR::mean_proportion_table(design = HH_svy_ob,list_of_variables = "health_expense", aggregation_level = c("i.health_coping_pay","i.household_elderly_member"),round_to = 2,return_confidence = FALSE,na_replace = FALSE)

double_subset2_by_camp<-butteR::mean_proportion_table(design = HH_svy_ob,list_of_variables = "health_expense", aggregation_level = c(strata,"i.health_coping_pay","i.household_elderly_member"),round_to = 2,return_confidence = FALSE,na_replace = FALSE)

#REFORMAT
hh_double_subset2_overall_reformatted<-double_subset2_overall %>% 
  tidyr::gather(key="subset_1",value="subset_1_value", i.health_coping_pay) %>%
  tidyr::gather(key= "subset_2", value="subset_2_value",i.household_elderly_member) %>%
  mutate(level="overall",level_value=NA) %>% 
  select(level, level_value,subset_1: subset_2_value,everything())    


hh_double_subset2_by_camp_reformatted<-double_subset2_by_camp %>% 
  tidyr::gather(key="subset_1",value="subset_1_value", i.health_coping_pay) %>%
  tidyr::gather(key= "subset_2", value="subset_2_value",i.household_elderly_member) %>% 
  mutate(level="camp") %>%rename(level_value=camp) %>% 
  select(level,level_value, subset_1: subset_2_value,everything())    

#BIND TOGETHER ALL HH DOUBLE SUBSETS (ACTUALLY TRIPLE WHEN YOU INCLUDE BY CAMP)
# hh_double_subset_all<-bind_rows(list(hh_double_subset_overall_reformatted,hh_double_subset_by_camp_reformatted))
hh_double_subset_all<-bind_rows(double_subsets_to_bind)
write.csv(hh_double_subset_all,paste0(output_path,isodate,"_HH_Analysis_Double_Subset.csv"))


# Individual Level Analysis -----------------------------------------------
#FILTER DAP
sppdap_indiv<-sppdap %>% filter(level=="individual")
dap_analysis_indiv<-list()
#OVERALL
dap_analysis_indiv$overall_na_replace_false<-butteR::mean_proportion_table(design = ind_svy_ob,
                                                                           list_of_variables = sppdap_indiv$variable,
                                                                           aggregation_level = NULL,
                                                                           round_to = 2,
                                                                           return_confidence = FALSE,
                                                                           na_replace = FALSE) %>%
  mutate(level="overall", level_value=NA) %>% select(level,level_value, everything()) 

#BY CAMP
dap_analysis_indiv$by_camp_na_replace_false<-butteR::mean_proportion_table(design = ind_svy_ob,
                                                                           list_of_variables = sppdap_indiv$variable,
                                                                           aggregation_level = strata,
                                                                           round_to = 2,
                                                                           return_confidence = FALSE,
                                                                           na_replace = FALSE) %>% 
  mutate(level="camp")%>% select(level,level_value=camp, everything())

#BY RESPONDENT GENDER
dap_analysis_indiv$by_resp_gender_na_replace_false<-butteR::mean_proportion_table(design = ind_svy_ob,
                                                                                  list_of_variables = sppdap_indiv$variable,
                                                                                  aggregation_level = "respondent_gender",
                                                                                  round_to = 2,
                                                                                  return_confidence = FALSE,
                                                                                  na_replace = FALSE) %>%
  mutate(level="respondent_gender")%>% select(level,level_value=respondent_gender, everything())

#BIND 3 ANALYSES 
individual_simple_dap<-do.call("rbind", dap_analysis_indiv)
write.csv(individual_simple_dap,paste0(output_path,isodate,"_Indiv_Analysis_Basic.csv"))


# INDIV SUBSET ANALYSES -----------------------------------------------------------

#SUBSET DAP FIRST
dap_indiv_subsets<-sppdap_indiv %>% filter(!is.na(`subset 1`))

#SPLIT SUBSET FOR FORLOOP
dap_indiv_subsets_l<-split(dap_indiv_subsets,dap_indiv_subsets$`subset 1`)
indiv_single_subset_list_overall<-list()
# OVERALL
for(i in 1: length(dap_indiv_subsets_l)){
  dap_temp<-dap_indiv_subsets_l[[i]]
  single_subset_temp<-unique(dap_temp$`subset 1`)
  var_list_temp<-dap_temp$variable
  indiv_single_subset_list_overall[[single_subset_temp]]<-butteR::mean_proportion_table(design = ind_svy_ob,list_of_variables = var_list_temp,aggregation_level = single_subset_temp,round_to = 2,return_confidence = FALSE,na_replace = FALSE) %>% 
    mutate(level="overall",level_value=NA) %>% select(level, level_value, everything())
  
}
#REFORMAT
single_subset_list_reformatted<-purrr::map(indiv_single_subset_list_overall, function(x) tidyr::gather(x, key="subset",value="subset_value",colnames(x)[3]) %>% select(level,level_value,subset, subset_value,everything())) 
individual_single_subset_overall_df<-bind_rows(single_subset_list_reformatted)

#BY CAMP
indiv_single_subset_list_by_camp<-list()
for(i in 1: length(dap_indiv_subsets_l)){
  dap_temp<-dap_indiv_subsets_l[[i]]
  single_subset_temp<-unique(dap_temp$`subset 1`)
  var_list_temp<-dap_temp$variable
  indiv_single_subset_list_by_camp[[single_subset_temp]]<-butteR::mean_proportion_table(design = ind_svy_ob,list_of_variables = var_list_temp,aggregation_level = c(strata,single_subset_temp),round_to = 2,return_confidence = FALSE,na_replace = FALSE)%>% 
    mutate(level="camp") %>% select(level, level_value="camp", everything())
  
}
#REFORMAT
indiv_single_subset_list_by_camp_reformatted<-purrr::map(indiv_single_subset_list_by_camp, function(x) tidyr::gather(x, key="subset",value="subset_value",colnames(x)[3]) %>% select(level,level_value,subset, subset_value,everything())) 
individual_single_subset_by_camp_df<-bind_rows(indiv_single_subset_list_by_camp_reformatted)

#BIND OVERALL AND  BY CAMP INDIV SUBSET TOGETHER AND WRITE CSV
indiv_single_subsets_all<-bind_rows(list(individual_single_subset_overall_df,individual_single_subset_by_camp_df))
write.csv(indiv_single_subsets_all,paste0(output_path,isodate,"_Indiv_Analysis_Single_Subset.csv"))


# INDIVIDUAL DOUBLE SUBSET ------------------------------------------------

dap_indiv_double_subsets<-sppdap_indiv %>% filter(!is.na(`subset 2`))
dap_indiv_double_subsets 

#OVERALL
indiv_double_subset_overall<-butteR::mean_proportion_table(design = ind_svy_ob,list_of_variables =dap_indiv_double_subsets$variable , aggregation_level = c("ind_gender","i.age_group"),round_to = 2,return_confidence = FALSE,na_replace = FALSE) %>% 
  mutate(level="overall", level_value=NA) %>% select(level, level_value,everything())

# BY CAMP
indiv_double_subset_by_camp<-butteR::mean_proportion_table(design = ind_svy_ob,list_of_variables =dap_indiv_double_subsets$variable , aggregation_level = c(strata,"ind_gender","i.age_group"),round_to = 2,return_confidence = FALSE,na_replace = FALSE) %>% mutate(level="camp") %>%  rename(level_value=camp) %>% 
  select(level, level_value, everything())
#BIND TOGETHER AND WRITE CSV
individual_double_subset_all<-bind_rows(list(indiv_double_subset_overall,indiv_double_subset_by_camp))
write.csv(individual_double_subset_all,paste0(output_path,isodate,"_Indiv_Analysis_Double_Subset.csv"))


#ONE PROP TABLE BREAKDOWN.
prop.table(svytable(~ind_gender+i.age_group2, ind_svy_ob)) %>% data.frame()%>%
  mutate(age_group2=paste0("age_", i.age_group2)) %>%
  write.csv(paste0(output_path,isodate,"_Indiv_age_sex_breakdown.csv"))


#ONE PROP TABLE BREAKDOWN.

srvyr::as_survey(ind_svy_ob) %>% 
  group_by(ind_gender, camp) %>% 
  summarise(survey_mean(.))
butteR::mean_proportion_table(design = ind_svy_ob,list_of_variables = "ind_gender",
                              aggregation_level = "camp"
                              
                                )




#AGE SEX BREAKDOWN
prop.table(svytable(~ind_gender+i.age_group2, ind_svy_ob)) %>%
  data.frame()%>%
  mutate(age_group2=paste0("age_", i.age_group2)) %>%
  write.csv(paste0(output_path,isodate,"_Indiv_age_sex_breakdown.csv"))


#AGE SEX BREAKDOWN BY CAMP - I THINK THEY WANT IT SO THEY CAN DO AN AGE-SEX POP PYRAMID PER CAMP SO THAT EACH
#CAMP ADDS UP TO 100 % RATHER THAN THE "age_sex_camp_total_prop_table" i make in line 318
age_sex_by_camp<-svyby(~interaction(ind_gender, i.age_group2),by= ~camp, design = ind_svy_ob, svymean) %>% data.frame() %>% select(-starts_with("se.")) %>% tidyr::gather(key="stat", value="val",interaction.ind_gender..i.age_group2.female.0:interaction.ind_gender..i.age_group2.male.60.) %>% arrange(camp)
#check that it is all good! (yes)
age_sex_by_camp %>% group_by(camp) %>% 
  summarise(asdfg=sum(val))

age_sex_by_camp %>% write.csv(paste0(output_path,isodate,"_Indiv_age_sex_breakdown_by_camp.csv"))



age_sex_camp_total_prop_table <- prop.table(svytable(~ind_gender+camp+i.age_group2, ind_svy_ob)) %>%
  data.frame()%>%
  mutate(age_group2=paste0("age_", i.age_group2)) %>% arrange(camp) 

#GENERATE SOME QUICK GRAPHS FOR AO TO INVESTIGATE/ CHECK FOR WEIRD DATA.
rmarkdown::render('preliminary_analysis_graphics_for_quick_review.Rmd')







