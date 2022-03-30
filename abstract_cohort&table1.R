#############
## SOFA    ##
#############
############# Author: Yanran Li

##### cohort selection

# libraries
library(dplyr)
library(tidyr)
library(lubridate)
library(bigrquery)
library(dbplyr)
library(table1)
library(MatchIt) 
library(boot)
library(ggplot2)
library(segmented)
library(splines)
library(Hmisc)
library(rms)
library(mgcv)
library(caret)
library(tidyverse)
library(stargazer)

# BigQuery
icu_con <- dbConnect(
  bigrquery::bigquery(),
  project = "physionet-data",
  dataset = "mimic_icu",
)

core_con <- dbConnect(
  bigrquery::bigquery(),
  project = "physionet-data",
  dataset = "mimic_core",
)

derived_con <- dbConnect(
  bigrquery::bigquery(),
  project = "physionet-data",
  dataset = "mimic_derived",
)
icustays <- tbl(icu_con,"icustays") %>% collect(n=Inf)
transfers <- tbl(core_con ,"transfers") %>% collect(n=Inf)
sofa <- tbl(derived_con,"sofa") %>% collect(n=Inf)
admissions <- tbl(core_con,"admissions") %>% collect(n=Inf)
patients <- tbl(core_con,"patients") %>% collect(n=Inf)
sepsis3 <- tbl(derived_con,"sepsis3") %>% collect(n=Inf)

# first time admitted in ICU
first_icu <- icustays %>% group_by(subject_id) %>% 
  select(subject_id, intime) %>% summarise(min(intime))
firsticu <- icustays %>% inner_join(admissions,by=c("hadm_id","subject_id")) %>% 
  inner_join(first_icu, by=c("hadm_id", "intime" = "min(intime)")) %>% 
  select(-c("subject_id","hadm_id","first_careunit","last_careunit","intime","outtime",
            "los","admittime","dischtime","deathtime","admission_type","admission_location",
            "discharge_location","insurance","language","marital_status","ethnicity",
            "edregtime","edouttime","hospital_expire_flag"))
# icustay.join <- icustays %>% left_join(patients[c("subject_id", "gender", 
#                                                   "anchor_age", "anchor_year_group")],
#                                        by=c("subject_id")) %>% 
#   left_join(admissions[c("hadm_id","deathtime", "ethnicity", "language", 
#                          "admission_location", "admission_type")],by=c("hadm_id")) %>% 
#   select(-c("intime","outtime","los"))

# ICU patients' ward stay in MIMIC-IV admitted between 2008 and 2019
length(unique(icustays$stay_id))
# 76540

# ICU patients' ward stay in MIMIC-IV (without hourly sofa score)
# 21

# ICU patients' ward stay in MIMIC-IV (with hourly sofa score)
length(unique(sofa$stay_id))
# 76519

# Hospital patients' ward stay in MIMIC-IV (not first time admitted in ICU)
# 23369

# Hospital patients' ward stay in MIMIC-IV (first time admitted in ICU)
length(unique(firsticu$stay_id))
# 53150

# Hospital patients' ward stay in MIMIC-IV (with hourly information)
a1 <- sofa[c("stay_id","hr","respiration_24hours","coagulation_24hours", "liver_24hours", 
             "cardiovascular_24hours", "cns_24hours", "renal_24hours","sofa_24hours")] %>% 
  left_join(icustays[c("stay_id", "subject_id", "intime","outtime","los")],by=c("stay_id")) %>% 
  left_join(sepsis3[c("stay_id", "sepsis3")],by=c("stay_id")) %>% 
  left_join(patients[c("subject_id", "gender", "anchor_age", "anchor_year_group")],
            by=c("subject_id")) %>% right_join(firsticu,by="stay_id") 
length(unique(a1$stay_id))
# 53150

# Hospital patients' ward stay in MIMIC-IV (without hourly information)
a3 <- a1 %>% filter(is.na(hr))
length(unique(a3$stay_id))
# 15


# Hospital patients' ward stay less than 24 hours in MIMIC-IV
a_less24 <- sofa[c("stay_id","hr","respiration_24hours","coagulation_24hours", "liver_24hours", "cardiovascular_24hours", "cns_24hours", "renal_24hours")] %>% left_join(icustays,by=c("stay_id")) %>% left_join(patients,by=c("subject_id")) %>% left_join(admissions,by=c("hadm_id", "subject_id")) %>% right_join(firsticu,by="stay_id") %>% filter(hr<24)  %>% filter(subject_id %not_in% a$subject_id)
length(unique(a_less24$stay_id))
# 12762

# Hospital patients' ward stay no less than 24 hours in MIMIC-IV (final cohort)
cohort <- sofa[c("stay_id","hr","respiration_24hours","coagulation_24hours", "liver_24hours", 
       "cardiovascular_24hours", "cns_24hours", "renal_24hours","sofa_24hours")] %>% 
  left_join(icustays[c("stay_id", "subject_id", "hadm_id","intime","outtime","los")],
            by=c("stay_id")) %>% 
  left_join(sepsis3[c("stay_id", "sepsis3")],by=c("stay_id")) %>% 
  left_join(patients[c("subject_id", "gender", "anchor_age", "anchor_year_group")],
            by=c("subject_id")) %>% 
  left_join(admissions[c("hadm_id", "subject_id","deathtime", "ethnicity", "language", "admission_location", 
                         "admission_type")],by=c("hadm_id", "subject_id")) %>% 
  right_join(firsticu,by="stay_id") %>% 
  filter(!is.na(hr)) %>% filter(hr==24)
length(unique(cohort$subject_id))
# 40373

##### Table 1
sofa40373 <- mutate(cohort, status = ifelse(is.na(cohort$deathtime) | 
                                              ymd_hms(cohort$deathtime) - 
                                              ymd_hms(cohort$outtime) > ddays(1), "Alive", "Died"), 
                    U_sepsis = ifelse(is.na(sepsis3), 0, 1))


sofa40373$status <- as.factor(sofa40373$status)

sofa40373$ethnicity <- factor(sofa40373$ethnicity, levels=c("HISPANIC/LATINO", 
                                                            "BLACK/AFRICAN AMERICAN", "WHITE", 
                                                            "ASIAN", "AMERICAN INDIAN/ALASKA NATIVE", 
                                                            "UNABLE TO OBTAIN", "UNKNOWN", "OTHER"),  
                              labels=c("HISPANIC/LATINO", "BLACK/AFRICAN AMERICAN", "WHITE", "ASIAN", 
                                       "IND/AK NA", "UNKNOWN","UNKNOWN","UNKNOWN"))

sofa40373$admission_location<- factor(sofa40373$admission_location, 
                                      levels=c("WALK-IN/SELF REFERRAL", "EMERGENCY ROOM", 
                                               "TRANSFER FROM HOSPITAL", "INFORMATION NOT AVAILABLE", 
                                               "PHYSICIAN REFERRAL", "TRANSFER FROM SKILLED NURSING FACILITY", 
                                               "PACU", "PROCEDURE SITE", "CLINIC REFERRAL",
                                               "AMBULATORY SURGERY TRANSFER", "INTERNAL TRANSFER TO OR FROM PSYCH"), 
                                      labels=c("ER/SELF", "ER/SELF", "TRANSFER", "OTHERS", "REFERRAL", "REFERRAL", 
                                               "TRANSFER", "ER/SELF", "REFERRAL","REFERRAL", "TRANSFER"))

sofa40373$gender <- factor(sofa40373$gender, levels=c("F","M"), labels=c("Female", "Male"))
sofa40373$U_sepsis <- factor(sofa40373$U_sepsis, levels=c(1,0), labels=c(1, 0))

label(sofa40373$gender) <- "Sex"
label(sofa40373$anchor_age) <- "Age"
label(sofa40373$ethnicity) <- "Ethnicity"
label(sofa40373$admission_location) <- "Admission Location"
label(sofa40373$respiration_24hours) <- "Respiration SOFA at 24 hrs"
label(sofa40373$coagulation_24hours) <- "Coagulation SOFA at 24 hrs"
label(sofa40373$liver_24hours) <- "Liver SOFA at 24 hrs"
label(sofa40373$cardiovascular_24hours) <- "Cardiovascular SOFA at 24 hrs"
label(sofa40373$cns_24hours) <- "Cns SOFA at 24 hrs"
label(sofa40373$renal_24hours) <- "Renal SOFA at 24 hrs"
label(sofa40373$sofa_24hours) <- "SOFA Total at 24 hrs"
label(sofa40373$U_sepsis) <- "Sepsis"

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- t.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}

T1_40373  <- table1(~gender + anchor_age + ethnicity +  admission_location + respiration_24hours + 
                      coagulation_24hours + liver_24hours + cardiovascular_24hours + 
                      cns_24hours + renal_24hours + sofa_24hours + U_sepsis | status, 
                    data = sofa40373, render.continuous=c(.="Mean (SD)", .="Median [Q1-Q3]"), 
                    overall=F, extra.col=list(`P-value`=pvalue))


### Regression_D1/2/3
all.regression = sofa[c("stay_id","hr","respiration_24hours","coagulation_24hours", "liver_24hours", 
                        "cardiovascular_24hours", "cns_24hours", "renal_24hours","sofa_24hours")] %>% 
  left_join(icustays[c("stay_id", "subject_id", "hadm_id","intime","outtime","los")],
            by=c("stay_id")) %>% 
  left_join(sepsis3[c("stay_id", "sepsis3")],by=c("stay_id")) %>% 
  left_join(patients[c("subject_id", "gender", "anchor_age", "anchor_year_group")],
            by=c("subject_id")) %>% 
  left_join(admissions[c("hadm_id", "subject_id","deathtime", "ethnicity", "language", "admission_location", 
                         "admission_type")],by=c("hadm_id", "subject_id")) %>% 
  right_join(firsticu,by="stay_id") %>% filter(!is.na(hr))
all.regression <- mutate(all.regression, status = ifelse(is.na(all.regression$deathtime) | 
                                              ymd_hms(all.regression$deathtime) - 
                                              ymd_hms(all.regression$outtime) > ddays(1), "Alive", "Died"), 
                    U_sepsis = ifelse(is.na(sepsis3), 0, 1))
all.regression$status <- as.factor(all.regression$status)
all.regression$ethnicity <- factor(all.regression$ethnicity, levels=c("HISPANIC/LATINO", 
                                                            "BLACK/AFRICAN AMERICAN", "WHITE", 
                                                            "ASIAN", "AMERICAN INDIAN/ALASKA NATIVE", 
                                                            "UNABLE TO OBTAIN", "UNKNOWN", "OTHER"),  
                              labels=c("HISPANIC/LATINO", "BLACK/AFRICAN AMERICAN", "WHITE", "ASIAN", 
                                       "UNKNOWN", "UNKNOWN","UNKNOWN","UNKNOWN"))

all.regression$admission_location<- factor(all.regression$admission_location, 
                                      levels=c("WALK-IN/SELF REFERRAL", "EMERGENCY ROOM", 
                                               "TRANSFER FROM HOSPITAL", "INFORMATION NOT AVAILABLE", 
                                               "PHYSICIAN REFERRAL", "TRANSFER FROM SKILLED NURSING FACILITY", 
                                               "PACU", "PROCEDURE SITE", "CLINIC REFERRAL",
                                               "AMBULATORY SURGERY TRANSFER", "INTERNAL TRANSFER TO OR FROM PSYCH"), 
                                      labels=c("ER/SELF", "ER/SELF", "TRANSFER", "OTHERS", "REFERRAL", "REFERRAL", 
                                               "TRANSFER", "ER/SELF", "REFERRAL","REFERRAL", "TRANSFER"))

all.regression$gender <- factor(all.regression$gender, levels=c("F","M"), labels=c("Female", "Male"))
all.regression$U_sepsis <- factor(all.regression$U_sepsis, levels=c(1,0), labels=c(1, 0))

all.regression = all.regression %>%
  mutate(icu_death = ifelse(status == "Alive", 0, 1)) %>%
  mutate(sex = ifelse(gender == "Female", 0, 1)) %>%
  mutate(ethnicity=relevel(ethnicity, ref = 'WHITE')) %>%
  mutate(respiration_24hours_b = ifelse(respiration_24hours == 0, 0, 1)) %>%
  mutate(coagulation_24hours_b = ifelse(coagulation_24hours == 0, 0, 1)) %>%
  mutate(liver_24hours_b = ifelse(liver_24hours == 0, 0, 1)) %>%
  mutate(cns_24hours_b = ifelse(cns_24hours == 0, 0, 1)) %>%
  mutate(renal_24hours_b = ifelse(renal_24hours == 0, 0, 1)) %>%
  mutate(cardiovascular_24hours_b = ifelse(cardiovascular_24hours == 0, 0, 1)) %>%
  mutate(age_div10 = as.integer(anchor_age/10)) 
# 53135 subject_id





## Day1|binary
all.24 = all.regression %>% filter(hr==24)
logistic.24 <- glm(icu_death ~ age_div10 + sex + as.factor(ethnicity) + 
                     respiration_24hours_b + coagulation_24hours_b + 
                     liver_24hours_b + cns_24hours_b + renal_24hours_b + 
                     + cardiovascular_24hours_b +
                     U_sepsis, data = all.24, family = 'binomial') 
summary(logistic.24) #coeffients



## Day4|binary
all.96 = all.regression %>% filter(hr==96)
logistic.96 <- glm(icu_death ~ age_div10 + sex + as.factor(ethnicity) + 
                     respiration_24hours_b + coagulation_24hours_b + 
                     liver_24hours_b + cns_24hours_b + 
                     cardiovascular_24hours_b +renal_24hours_b + U_sepsis, 
                   data = all.96, family = 'binomial') 
summary(logistic.96)


## Day7|binary
all.168 = all.regression %>% filter(hr==168)
logistic.168 <- glm(icu_death ~ age_div10 + sex + as.factor(ethnicity) + 
                     respiration_24hours_b + coagulation_24hours_b + 
                     liver_24hours_b + cns_24hours_b + renal_24hours_b +
                     cardiovascular_24hours_b + U_sepsis, 
                   data = all.168, family = 'binomial') 
summary(logistic.168)


# report 3 binary logistic regression models' summary
stargazer(logistic.24, logistic.96, logistic.168,
          title = "3 binary logistic regression models' summary",
          column.labels=c("Day1","Day4","Day7"),
          align=TRUE, 
          covariate.labels=c("Age(/10)","Sex", "HISPANIC/LATINO","BLACK/AFRICAN AMERICAN",
                             "ASIAN",
                             "UNKNOWN","Respiration","Coagulation","Liver",
                             "CNS","cardiovascular","Renal", "U-sepsis","Intercept"), 
          no.space=TRUE)
#text
stargazer(logistic.24, logistic.96, logistic.168,
          title = "3 binary logistic regression models' summary",
          column.labels=c("Day1","Day4","Day7"),
          apply.coef = exp, type = "text", ci=TRUE, ci.level=0.95, single.row = T,
          covariate.labels=c("Age(/10)","Sex", "HISPANIC/LATINO","BLACK/AFRICAN AMERICAN",
                             "ASIAN",
                             "UNKNOWN","Respiration","Coagulation","Liver",
                             "CNS","cardiovascular","Renal", "U-sepsis","Intercept"), 
          no.space=TRUE)


# Day1/4/7 mortality rate
d147_mortality_rate <- c(mean(all.24$icu_death), mean(all.96$icu_death),
                         mean(all.168$icu_death))
stargazer(d147_mortality_rate,type = "text",
          title = "Mortality Rate in ICU for Day 1/4/7",
          column.labels=c("Day1","Day4","Day7"))





