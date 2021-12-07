#------------------------------------------------------------------------------#
# Title: Cleaning PSSP Survey ####
# Purpose: Cleans PSSP survey data, preparing it for analysis 
# Produces: pssp_data.RData (3 data frame: eng_long, val_long, pssp_survey)
# Author: Simon Hoellerbauer
#------------------------------------------------------------------------------#

# Setup ------------------------------------------------------------------------
library(tidyverse)

#### Read In Data ####
pssp_survey <- readxl::read_xlsx("Data/congruence_follow_up_text_del1row.xlsx")

#### Prelim ####
# dropping instrument testing observations from before study start  
pssp_survey <- pssp_survey %>% filter(StartDate >= as.Date("2021-11-08"))

#remove empty columns
pssp_survey$RecipientLastName <- pssp_survey$RecipientFirstName <-
  pssp_survey$RecipientEmail <- pssp_survey$ExternalReference <- NULL

#creating respondent unique ID 
#pssp_survey$ID <- paste0("P", 1:nrow(pssp_survey))

# Reformatting and Cleaning  ---------------------------------------------------

## Re-Naming Attribute and Outcome Variables ####
# for reformatting later

# shared attribute names
conj_attrs <- c("mem1", "lead1", "hq1", "type1", "fund1", "goal1",
                "mem2", "lead2", "hq2", "type2", "fund2", "goal2",
                "order")

### Engagement Conjoint ####

#### Engagement Conjoint Attributes #### 
eng_conj_attrs_num <- c("Q42|Q128|Q132") # question ids
eng_pairs <- paste0("engPair", 1:3) # new variable header
eng_conj_attr_vars <- expand.grid(conj_attrs, eng_pairs)[,2:1] #making all combos
eng_conj_attr_vars <- paste(eng_conj_attr_vars[,1], eng_conj_attr_vars[,2], 
                        sep = "_") # new names
#renaming engagement conjoint attribute variables for pivot_longer later
names(pssp_survey)[grepl(eng_conj_attrs_num, 
                         names(pssp_survey))] <- eng_conj_attr_vars

#### Engagement Conjoint Outcomes ####
eng_conj_outs_num <- c("Q123|Q34|Q184|Q125|Q126|Q186|Q130|Q187|Q188") 
eng_outs <- c("meetingOrg1", "meetingOrg2",
              "meetingForced",
              paste("mech", c("Fun", "Valued", "Resume",
                              "Friends",
                              "Time",
                              "Mock",
                              "Tired"), sep = ""))
eng_conj_out_vars <- expand.grid(eng_outs, eng_pairs)[,2:1] #making all combos
eng_conj_out_vars <- paste(eng_conj_out_vars [,1], eng_conj_out_vars [,2], 
                            sep = "_")

names(pssp_survey)[grepl(eng_conj_outs_num, 
                         names(pssp_survey))] <- eng_conj_out_vars

### Values Conjoint ####

#### Values Conjoint Attributes #### 
val_conj_attrs_num <- c("Q136|Q139|Q143|Q146|Q149|Q152|Q155|Q158|Q161|Q164|Q167|Q170") # question ids
val_pairs <- paste0("valPair", 1:12) # new variable header
val_conj_attr_vars <- expand.grid(conj_attrs, val_pairs)[,2:1] #making all combos
val_conj_attr_vars <- paste(val_conj_attr_vars[,1], val_conj_attr_vars[,2], 
                            sep = "_") # new names
#renaming valagement conjoint attribute variables for pivot_longer later
names(pssp_survey)[grepl(val_conj_attrs_num, 
                         names(pssp_survey))] <- val_conj_attr_vars

#### Values Conjoint Outcome ####
val_conj_outs_num <- c("Q135|Q138|Q142|Q145|Q148|Q151|Q154|Q157|Q160|Q163|Q166|Q169") 
val_outs <- "valuesForced" 
val_conj_out_vars <- paste(val_pairs, val_outs, 
                           sep = "_") #making all combos

names(pssp_survey)[grepl(val_conj_outs_num, 
                         names(pssp_survey))] <- val_conj_out_vars

### Other Variables ####

#rename remaining variables 'by hand'
pssp_survey <- pssp_survey %>% 
  rename(respID = ResponseId,
         pair15_values_why = Q140,
         pair15_meeting_why = Q190,
         gender = Q113,
         yrs_att_UNC = Q1,
         student_pride = Q6_1,
         student_import = Q6_2,
         freetime_weekday = Q8,
         freetime_weekend = Q10,
         live_bfr_UNC = Q5,
         reg_vote = Q114,
         vote_gen20 = Q115,
         involv_org_campus = Q12,
         num_org_involv_c = Q14,
         org_names_c = Q15,
         involv_org_noncampus = Q16,
         num_org_involv_nc = Q17,
         org_names_nc = Q117,
         hrs_engaging = Q19,
         define_CS = Q20,
         define_NGO = Q21,
         import_local = Q22_1,
         import_goal = Q22_2,
         import_freetime = Q22_3,
         import_benefit_you = Q22_4,
         import_feas_goal = Q118_1,
         import_org_reflec_you = Q118_2,
         import_closeness = Q118_3,
         import_values = Q118_4,
         start_org = Q23,
         greek_life = Q24,
         pol_cap_admin = Q25_1,
         pol_cap_UNCGA = Q25_2)

## Recode Variables #### 
#labels for agree variables
agree <- c("Strongly disagree", 
           "Somewhat disagree",
           "Neither agree nor disagree",
           "Somewhat agree",
           "Strongly agree")
#labels for importance variables
important <- c("Not at all important",
               "Slightly important",
               "Moderately important",
               "Very important",
               "Extremely important")

pssp_survey <- pssp_survey %>% 
  mutate(female = ifelse(gender == "Female", 1, 0), #female
         yrs_att_UNC = factor(yrs_att_UNC,
                              levels = c("1", "2", "3", "4", 
                                         "More than 5")),
         student_pride = factor(student_pride, levels = agree),
         student_import = factor(student_import, agree),
         import_local = factor(import_local, important),
         import_goal = factor(import_goal, important),
         import_freetime = factor(import_goal, important),
         import_benefit_you = factor(import_benefit_you, important),
         import_feas_goal = factor(import_feas_goal, important),
         import_org_reflec_you = factor(import_org_reflec_you, important),
         import_closeness = factor(import_closeness, important),
         import_values = factor(import_values, important),
         student_pride_num = as.numeric(student_pride),
         student_import_num = as.numeric(student_import),
         import_local_num = as.numeric(import_local),
         import_goal_num = as.numeric(import_goal),
         import_freetime_num = as.numeric(import_goal),
         import_benefit_you_num = as.numeric(import_benefit_you),
         import_feas_goal_num = as.numeric(import_feas_goal),
         import_org_reflec_you_num = as.numeric(import_org_reflec_you),
         import_closeness_num = as.numeric(import_closeness),
         num_org_involv_c = ifelse(is.na(num_org_involv_c),
                                      0, num_org_involv_c),
         num_org_involv_nc = ifelse(is.na(num_org_involv_nc),
                                       0, num_org_involv_nc),
         reg_vote_bin = ifelse(is.na(reg_vote), NA,
                               ifelse(reg_vote == "Yes", 1, 0)),
         vote_gen20_bin = ifelse(is.na(vote_gen20), NA,
                                  ifelse(vote_gen20 == "I am sure I voted",
                                         1, 0)),
         vote_gen20_inel = ifelse(vote_gen20 == "I am not eligible to vote",
                                   1, 0),
         start_org_bin = ifelse(is.na(start_org), NA,
                                ifelse(start_org == "Yes", 1, 0)),
         greek_life = ifelse(is.na(greek_life), NA,
                             ifelse(greek_life == "Yes", 1, 0)),
         involv_org_campus_bin = ifelse(is.na(involv_org_campus), NA,
                                        ifelse(involv_org_campus == "Yes", 1, 0)),
         involv_org_noncampus_bin = ifelse(is.na(involv_org_noncampus), NA,
                                           ifelse(involv_org_noncampus == "Yes", 1, 0))
  )

# recoding engagement outcome variables
# first non-forced choice meeting questions
likely <- c("Very unlikely",
            "Somewhat unlikely",
            "Neither likely nor unlikely",
            "Very likely",
            "Somewhat likely")
for(var in eng_conj_out_vars[grepl("meetingOrg", eng_conj_out_vars)]){
  pssp_survey[[var]] <- factor(pssp_survey[[var]], levels = likely)
}
# then for mechanism outcome questions
for(var in eng_conj_out_vars[grepl("mech", eng_conj_out_vars)]){
  pssp_survey[[var]] <- factor(pssp_survey[[var]], levels = agree)
}


## creating more  variables useful for subgroup analysis ####
pssp_survey <- pssp_survey %>% 
  mutate(total_freetime = 5 * freetime_weekday + freetime_weekend,
         med_freetime = ifelse(total_freetime >= median(total_freetime, na.rm = T), 
                               1, 0),
         num_org_c_cat = ifelse(num_org_involv_c == 0, 0, 
                                ifelse(num_org_involv_c == 1, 1,
                                       ifelse(num_org_involv_c > 1, 2, NA))))

# Pivot Longer -----------------------------------------------------------------

## Engagement Conjoint ####

#this data frame is at the organization level

#make pair level data set
eng_long <- pssp_survey %>% select(respID, contains("engPair")) %>% 
  pivot_longer(cols = starts_with("engPair"),
               names_to = c("engPair", ".value"),
               names_sep = "_",
               values_drop_na = F) %>% 
  pivot_longer(cols = c(meetingOrg1, meetingOrg2, mem1:goal2),
               names_to = c(".value", "Organization"),
               names_pattern = "(.*)([12])")

#make numeric version of engPair variable
eng_long <- eng_long %>% 
  mutate(pair_num = as.numeric(str_split(engPair, "engPair", simplify = T)[,2]))

# cleaning attribute-levels
eng_long <- eng_long %>% 
  mutate(type = gsub("&lt;b&gt;not&lt;/b&gt;", "not", type),
         lead = gsub("&lt;b&gt;not&lt;/b&gt;", "not", lead))

# clarifying att-level orderings
mem_ord <- c("mainly non-students", "students and non-students",
             "mainly students")
lead_ord <- c("not a student", "a student")
hq_ord <- c("Washington, DC", "Richmond, VA", "Raleigh, NC",
             "Chapel Hill, NC")
type_ord <- c("a chapter of a national organization",
              "not a chapter of a national organization")
fund_ord <- c("donations from national partners",
              "donations from members and community")
goal_ord <- c( "throughout North Carolina", "in the town of Chapel Hill",
               "on campus")

eng_long <- eng_long %>% 
  mutate(mem = factor(mem, levels = mem_ord), 
         lead = factor(lead, levels = lead_ord),
         hq = factor(hq, levels = hq_ord),
         type = factor(type, levels = type_ord),
         fund = factor(fund, levels = fund_ord),
         goal = factor(goal, levels = goal_ord)
  )

#fixed mechanism outcomes for organization 2 (should be NA,
#respondents only asked about organization 1)
eng_long[eng_long$Organization == 2,
         names(eng_long)[grepl("mech", names(eng_long))]] <- NA

#make forced choice outcomes
eng_long <- eng_long %>% 
  mutate(meetingForced_char = meetingForced,
         meetingForced = ifelse(is.na(meetingForced), NA, 
                                as.numeric(str_split(meetingForced, 
                                                     " ",
                                                     simplify = T)[,2] == Organization)))

#make agree and likely outcomes numeric versions (for model fitting)
eng_long <- eng_long %>% 
  mutate(mechFun_num = as.numeric(mechFun),
         mechValued_num = as.numeric(mechFun),
         mechResume_num = as.numeric(mechResume),
         mechFriends_num = as.numeric(mechFriends),
         mechTime_num = as.numeric(mechTime),
         mechMock_num = as.numeric(mechTime),
         mechTired_num = as.numeric(mechTired),
         meetingOrg_num = as.numeric(meetingOrg))

## Values Conjoint ####

#this data frame is at the profile pair level

#make pair level data set (this one will remain here)
val_long <- pssp_survey %>% select(respID, contains("valPair")) %>% 
  pivot_longer(cols = starts_with("valPair"),
               names_to = c("valPair", ".value"),
               names_sep = "_",
               values_drop_na = F)

# make a numeric version of pair
val_long <- val_long %>% 
  mutate(pair_num = as.numeric(str_split(valPair, "valPair", simplify = T)[,2]))

# cleaning attribute-levels
val_long <- val_long %>% 
  mutate(type1 = gsub("&lt;b&gt;not&lt;/b&gt;", "not", type1),
         type2 = gsub("&lt;b&gt;not&lt;/b&gt;", "not", type2),
         lead1 = gsub("&lt;b&gt;not&lt;/b&gt;", "not", lead1),
         lead2 = gsub("&lt;b&gt;not&lt;/b&gt;", "not", lead2))

# clarifying att-level orderings
val_long <- val_long %>% 
  mutate(mem1 = factor(mem1, levels = mem_ord),
         mem2 = factor(mem2, levels = mem_ord), 
         lead1 = factor(lead1, levels = lead_ord),
         lead2 = factor(lead2, levels = lead_ord),
         hq1 = factor(hq1, levels = hq_ord),
         hq2 = factor(hq2, levels = hq_ord),
         type1 = factor(type1, levels = type_ord),
         type2 = factor(type2, levels = type_ord),
         fund1 = factor(fund1, levels = fund_ord),
         fund2 = factor(fund2, levels = fund_ord),
         goal1 = factor(goal1, levels = goal_ord),
         goal2 = factor(goal2, levels = goal_ord)
  )

#making outcomes 1/0
val_long <- val_long %>% 
  mutate(values_org1 = ifelse(is.na(valuesForced), NA,
                           ifelse(valuesForced == "Organization 1", 1, 0)))

# checking straightlining for forced choice questions
table(eng_long$meetingForced_char) %>% prop.table()
table(val_long$valuesForced) %>% prop.table()

# Saving -----------------------------------------------------------------------
save(pssp_survey, eng_long, val_long, file = "Data/pssp_data.RData")
