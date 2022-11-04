library(tidyverse)
library(lubridate)
library(nycflights13)
library(table1)


rawdata<- read_csv("rawdata.csv")
df1 <-rawdata

## 24hours is 1*24*60*60 = 86400 seconds
df1[,'out_in'] <- 5
df1$out_in[difftime(df1$icu_outtime, df1$icu_intime, units = "secs") <  86400] <- 1
df1$out_in[difftime(df1$icu_outtime, df1$icu_intime, units = "secs") >= 86400] <- 0
table(df1$out_in)

df1[,'death_in'] <- 5
df1$death_in[difftime(df1$deathtime, df1$icu_intime, units = "secs") <=  86400] <- 1
df1$death_in[difftime(df1$deathtime, df1$icu_intime, units = "secs") > 86400] <- 0
table(df1$death_in)


df1[,'discharge_in'] <- 5
df1$discharge_in[difftime(df1$dischtime, df1$icu_intime, units = "secs") <=  86400] <- 1
df1$discharge_in[difftime(df1$dischtime, df1$icu_intime, units = "secs") > 86400] <- 0
table(df1$discharge_in)


df2 <-df1[(df1$first_icu_stay=="TRUE"),]
nrow(df1) - nrow(df2)

df3 <-df2[!(df2$out_in==1),]
df4 <-df3[!(df3$death_in==1),]
df5 <-df4[!(df4$discharge_in==1),]
nrow(df2) - nrow(df5)

df6<-df5[!(is.na(df5$cns_24)),]
nrow(df5) - nrow(df6)

drg = read.csv("MIMIC_DRG.csv")

all(df6$stay_id %in% drg)

class(df6$admission_age) # numeric

df7 = df6[df6$admission_age <= 89 ,]




final_df <- df7


## t1: for the usage of Table one(model one)
final_df$t1_ethnicity = final_df$ethnicity
final_df$t1_ethnicity[final_df$ethnicity == 'OTHER' 
                      | final_df$ethnicity == 'UNABLE TO OBTAIN'
                      | final_df$ethnicity == 'UNKNOWN'
                      | final_df$ethnicity == 'AMERICAN INDIAN/ALASKA NATIVE'] <- "OTHER"

final_df[,'t1_gender'] <- 0
final_df$t1_gender[final_df$gender == "M"] <- "Male"
final_df$t1_gender[final_df$gender == "F"] <- "Female"

final_df$t1_sepsis3 = final_df$sepsis3
final_df$t1_sepsis3[final_df$sepsis3 == 'TRUE'] <- "Yes"
final_df$t1_sepsis3[is.na(final_df$sepsis3)] <- "No"

sum(is.na(final_df$deathtime)) #46632
final_df['combinedeathtime'] = final_df$deathtime
sum(is.na(final_df$combinedeathtime)) #46632
#final_df$combinedeathtime[final_df$discharge_location == "HOSPICE" & !is.na(final_df$discharge_location)] <- final_df$dischtime[final_df$discharge_location == "HOSPICE" & !is.na(final_df$discharge_location)]
#sum(is.na(final_df$combinedeathtime)) #45500


final_df[,'t1_icudeath'] <- 0
## 72hours = 72*60*60 = 259200, use seconds for the most accurate calculation
final_df$t1_icudeath[(!is.na(final_df$combinedeathtime)) & ( difftime(final_df$combinedeathtime, final_df$icu_intime, units = "secs") >=0) & (difftime(final_df$icu_outtime, final_df$combinedeathtime, units = "secs") >=0 ) ] <- 1
final_df$t1_icudeath[(!is.na(final_df$combinedeathtime)) & ( difftime(final_df$combinedeathtime, final_df$icu_intime, units = "secs") >=0) & (difftime(final_df$combinedeathtime, final_df$icu_outtime, units = "secs") >0 ) & (difftime(final_df$combinedeathtime, final_df$icu_outtime, units = "secs") <259200 ) ] <- 1

final_df$t1_icudeath[final_df$t1_icudeath == 0] <- "Survived or discharged to other locations within 72 hours of ICU discharge"
final_df$t1_icudeath[final_df$t1_icudeath == 1] <- "ICU death within 72 hours of ICU discharge"

final_df[,'m1_icudeath']<- 0
final_df$m1_icudeath[(!is.na(final_df$combinedeathtime)) & ( difftime(final_df$combinedeathtime, final_df$icu_intime, units = "secs") >=0) & (difftime(final_df$icu_outtime, final_df$combinedeathtime, units = "secs") >=0 ) ] <- 1
final_df$m1_icudeath[(!is.na(final_df$combinedeathtime)) & ( difftime(final_df$combinedeathtime, final_df$icu_intime, units = "secs") >=0) & (difftime(final_df$combinedeathtime, final_df$icu_outtime, units = "secs") >0 ) & (difftime(final_df$combinedeathtime, final_df$icu_outtime, units = "secs") <259200 ) ] <- 1


## for the SOFA_RESP part
final_df[, 'newvent24'] <- 5
final_df$newvent24[final_df$vent_24 == 'InvasiveVent'
                   | final_df$vent_24 == 'HFNC'
                   | final_df$vent_24 == 'Tracheostomy'
                   | final_df$vent_24 == 'NonInvasiveVent'] <- 1
final_df$newvent24[is.na(final_df$vent_24) ]<- 0
final_df$newvent24[final_df$vent_24 == 'SupplementalOxygen'] <- 0
table(final_df$newvent24)   


final_df[, 'newvent168'] <- 5
final_df$newvent168[final_df$vent_168 == 'InvasiveVent'
                    | final_df$vent_168 == 'HFNC'
                    | final_df$vent_168 == 'Tracheostomy'
                    | final_df$vent_168 == 'NonInvasiveVent'] <- 1
final_df$newvent168[is.na(final_df$vent_168) ]<- 0
final_df$newvent168[final_df$vent_168 == 'SupplementalOxygen'] <- 0             
table(final_df$newvent168)                     


final_df[, 'newventlast'] <- 5
final_df$newventlast[final_df$vent_last == 'InvasiveVent'
                     | final_df$vent_last == 'HFNC'
                     | final_df$vent_last == 'Tracheostomy'
                     | final_df$vent_last == 'NonInvasiveVent'] <- 1
final_df$newventlast[is.na(final_df$vent_last) ]<- 0
final_df$newventlast[final_df$vent_168 == 'SupplementalOxygen'] <- 0       
table(final_df$newventlast)





#------Sofa conponents for 24hours
final_df$t1_resp24 = final_df$resp_24
final_df$t1_resp24[final_df$newvent24 == 1] <- "Abnormal"
final_df$t1_resp24[final_df$newvent24 == 0 & final_df$resp_24 == 0] <- "Normal"
final_df$t1_resp24[final_df$newvent24 == 0 & final_df$resp_24 >= 1] <- "Abnormal"

final_df$t1_coag24 = final_df$coag_24
final_df$t1_coag24[final_df$coag_24 == 0] <- "Normal"
final_df$t1_coag24[final_df$coag_24 >= 1] <- "Abnormal"

final_df$t1_liver24 = final_df$liver_24
final_df$t1_liver24[final_df$liver_24 == 0] <- "Normal"
final_df$t1_liver24[final_df$liver_24 >= 1] <- "Abnormal"

final_df$t1_cv24 = final_df$cv_24
final_df$t1_cv24[final_df$cv_24 == 0] <- "Normal"
final_df$t1_cv24[final_df$cv_24 >= 1] <- "Abnormal"

final_df$t1_cns24 = final_df$cns_24
final_df$t1_cns24[final_df$vent_24 == "InvasiveVent"] <- "Mechanical Ventilation (MV)"
final_df$t1_cns24[final_df$vent_24 != "InvasiveVent" & final_df$cns_24 == 0] <- "No MV and normal CNS"
final_df$t1_cns24[is.na(final_df$vent_24) & final_df$cns_24 == 0] <- "No MV and normal CNS"
final_df$t1_cns24[final_df$vent_24 != "InvasiveVent" & final_df$cns_24 >= 1] <- "No MV and abnormal CNS"
final_df$t1_cns24[is.na(final_df$vent_24) & final_df$cns_24 >= 1] <- "No MV and abnormal CNS"



final_df$t1_renal24 = final_df$renal_24
final_df$t1_renal24[final_df$renal_24 == 0] <- "Normal"
final_df$t1_renal24[final_df$renal_24 >= 1] <- "Abnormal"




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

final_df$t1_gender <- factor(final_df$t1_gender, level=c('Male','Female'))
final_df$t1_icudeath <- factor(final_df$t1_icudeath, levels=c("Survived or discharged to other locations within 72 hours of ICU discharge","ICU death within 72 hours of ICU discharge"))
final_df$t1_sepsis3 <- factor(final_df$t1_sepsis3,levels=c("Yes","No"))
final_df$t1_resp24 <- factor(final_df$t1_resp24, levels=c("Abnormal","Normal"))
final_df$t1_coag24 <- factor(final_df$t1_coag24, levels=c("Abnormal","Normal"))
final_df$t1_cv24 <- factor(final_df$t1_cv24, levels=c("Abnormal","Normal"))
final_df$t1_liver24 <- factor(final_df$t1_liver24, levels=c("Abnormal","Normal"))
final_df$t1_renal24 <- factor(final_df$t1_renal24, levels=c("Abnormal","Normal"))
final_df$t1_cns24 <- factor(final_df$t1_cns24, levels=c("Mechanical Ventilation (MV)","No MV and abnormal CNS","No MV and normal CNS"))
final_df$t1_ethnicity <- factor(final_df$t1_ethnicity, levels=c("HISPANIC/LATINO","BLACK/AFRICAN AMERICAN","WHITE","ASIAN","OTHER"))




label(final_df$t1_gender) <- "Gender"
label(final_df$t1_ethnicity) <- "Ethnicity"
label(final_df$t1_sepsis3) <- "Sepsis"
label(final_df$t1_resp24) <- "SOFA - Respiration at 24 hours"
label(final_df$t1_cns24) <- "SOFA-CNS and MV at 24 hours"
label(final_df$t1_cv24) <- "SOFA - Cardiovascular at 24 hours"
label(final_df$t1_coag24) <- "SOFA - Coagulation at 24 hours"
label(final_df$t1_renal24) <- "SOFA - Renal at 24 hours"
label(final_df$t1_liver24) <- "SOFA - Liver at 24 hours"
label(final_df$admission_age) <- "Age"


table1(~ t1_gender + admission_age+ t1_ethnicity + t1_sepsis3 + t1_cns24+ t1_resp24 +t1_coag24 + t1_liver24+ t1_cv24  + t1_renal24
       | t1_icudeath , data=final_df, overall=F,extra.col=list(`P-value`=pvalue),render.missing=NULL,topclass="Rtable1-grid Rtable1-shade Rtable1-times")

#final_df$t1_resp24[final_df$t1_resp24 == "Abnormal(Cause by Ventilation)"]<- "Abnormal"
#final_df$t1_resp24 <- factor(final_df$t1_resp24, levels=c("Abnormal","Normal"))

final_df <- within(final_df, t1_ethnicity <- relevel(factor(t1_ethnicity), ref = "WHITE"))
final_df <- within(final_df, t1_cns24 <- relevel(factor(t1_cns24), ref = "No MV and normal CNS"))
final_df <- within(final_df, t1_coag24 <- relevel(factor(t1_coag24), ref = "Normal"))
final_df <- within(final_df, t1_resp24 <- relevel(factor(t1_resp24), ref = "Normal"))
final_df <- within(final_df, t1_cv24 <- relevel(factor(t1_cv24), ref = "Normal"))
final_df <- within(final_df, t1_renal24 <- relevel(factor(t1_renal24), ref = "Normal"))
final_df <- within(final_df, t1_liver24 <- relevel(factor(t1_liver24), ref = "Normal"))

final_df[,'m1_age'] <- NA
final_df$m1_age <-final_df$admission_age / 10




m1 <- glm(m1_icudeath ~  m1_age + t1_gender + t1_ethnicity + t1_cns24 + t1_resp24+t1_coag24
          +t1_liver24 + t1_cv24 + t1_renal24, data = final_df, family = "binomial"(link=logit))
summary(m1)

m1_OR <- exp(cbind(OR = coef(m1), confint(m1)))
#write.csv(m1_OR,"24hour OR_MIMIC.csv")


#------Sofa conponents for 168(day7)

## 168*60*60 = 604800 secs; 72*60*60 = 259200 secs
# Add the situation on 168 hour, for some intermediate results

df = final_df
df[,'t1_168situation'] <- "Die"
#alive in ICU at 168hour
df$t1_168situation[(difftime(df$dischtime, df$icu_intime, units = "secs")>=604800) & (difftime(df$icu_outtime, df$icu_intime, units = "secs") >=604800) & (!is.na(df$deathtime)) & (difftime(df$deathtime, df$icu_intime, units = "secs") >=604800) ]<- "Alive"
df$t1_168situation[(difftime(df$dischtime, df$icu_intime, units = "secs")>=604800) & (difftime(df$icu_outtime, df$icu_intime, units = "secs") >=604800) & (is.na(df$deathtime)) ] <- "Alive"

#discharge before 168hour
df$t1_168situation[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (!is.na(df$deathtime)) & (difftime(df$deathtime,df$icu_outtime,units="secs") >= 259200)] <-"Discharge"
df$t1_168situation[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (is.na(df$deathtime)) & (difftime(df$dischtime,df$icu_outtime,units="secs") >= 259200)] <-"Discharge"
df$t1_168situation[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (is.na(df$deathtime)) & (difftime(df$dischtime,df$icu_outtime,units="secs") < 259200) & (!is.na(df$discharge_location)) & (df$discharge_location != "HOSPICE")] <-"Discharge"
df$t1_168situation[ (difftime(df$dischtime, df$icu_intime, units = "secs")<604800|difftime(df$icu_outtime, df$icu_intime, units = "secs") <604800) & (is.na(df$deathtime)) & (difftime(df$dischtime,df$icu_outtime,units="secs") < 259200) & (is.na(df$discharge_location)) ] <-"Discharge"



## for the build of 168_table_one
final_df <-df[(df$t1_168situation=="Alive"),]
final_df <-final_df[(!is.na(final_df$resp_168)),]



final_df[,'t1_resp168'] <- 5
final_df$t1_resp168[final_df$newvent168 == 1] <- "Abnormal"
final_df$t1_resp168[final_df$newvent168 == 0 & final_df$resp_168 == 0] <- "Normal"
final_df$t1_resp168[final_df$newvent168 == 0 & final_df$resp_168 >= 1] <- "Abnormal"
table(final_df$t1_resp168)

final_df[,'t1_coag168'] <- 5
final_df$t1_coag168[ final_df$coag_168 == 0] <- "Normal"
final_df$t1_coag168[ final_df$coag_168 >= 1] <- "Abnormal"


final_df[,'t1_liver168'] <- 5
final_df$t1_liver168[ final_df$liver_168 == 0] <- "Normal"
final_df$t1_liver168[ final_df$liver_168 >= 1] <- "Abnormal"


final_df[,'t1_cv168'] <- 5
final_df$t1_cv168[ final_df$cv_168 == 0] <- "Normal"
final_df$t1_cv168[ final_df$cv_168 >= 1] <- "Abnormal"



final_df[,'t1_cns168'] <- 5
final_df$t1_cns168[final_df$vent_168 == "InvasiveVent"] <- "Mechanical Ventilation (MV)"
final_df$t1_cns168[final_df$vent_168 != "InvasiveVent"  &(!is.na(final_df$cns_168)) & (final_df$cns_168 == 0)] <- "No MV and normal CNS"
final_df$t1_cns168[is.na(final_df$vent_168)  &(!is.na(final_df$cns_168)) & (final_df$cns_168 == 0)] <- "No MV and normal CNS"
final_df$t1_cns168[final_df$vent_168 != "InvasiveVent"  &(!is.na(final_df$cns_168)) & (final_df$cns_168 >= 1)] <- "No MV and abnormal CNS"
final_df$t1_cns168[is.na(final_df$vent_168)  &(!is.na(final_df$cns_168)) & (final_df$cns_168 >= 1)] <- "No MV and abnormal CNS"
table(final_df$t1_cns168)



final_df[,'t1_renal168'] <- 5
final_df$t1_renal168[ final_df$renal_168 == 0] <- "Normal"
final_df$t1_renal168[ final_df$renal_168 >= 1] <- "Abnormal"


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

final_df$t1_gender <- factor(final_df$t1_gender, level=c('Male','Female'))
final_df$t1_icudeath <- factor(final_df$t1_icudeath, levels=c("Survived or discharged to other locations within 72 hours of ICU discharge","ICU death within 72 hours of ICU discharge"))
final_df$t1_sepsis3 <- factor(final_df$t1_sepsis3,levels=c("Yes","No"))
final_df$t1_resp168 <- factor(final_df$t1_resp168, levels=c("Abnormal","Normal"))
final_df$t1_coag168 <- factor(final_df$t1_coag168, levels=c("Abnormal","Normal"))
final_df$t1_cv168 <- factor(final_df$t1_cv168, levels=c("Abnormal","Normal"))
final_df$t1_liver168 <- factor(final_df$t1_liver168, levels=c("Abnormal","Normal"))
final_df$t1_renal168 <- factor(final_df$t1_renal168, levels=c("Abnormal","Normal"))
final_df$t1_cns168 <- factor(final_df$t1_cns168, levels=c("Mechanical Ventilation (MV)","No MV and abnormal CNS","No MV and normal CNS"))
final_df$t1_ethnicity <- factor(final_df$t1_ethnicity, levels=c("HISPANIC/LATINO","BLACK/AFRICAN AMERICAN","WHITE","ASIAN","OTHER"))




label(final_df$t1_gender) <- "Gender"
label(final_df$t1_ethnicity) <- "Ethnicity"
label(final_df$t1_sepsis3) <- "Sepsis"
label(final_df$t1_resp168) <- "SOFA - Respiration at 168 hours"
label(final_df$t1_cns168) <- "SOFA-CNS and MV at 168 hours"
label(final_df$t1_cv168) <- "SOFA - Cardiovascular at 168 hours"
label(final_df$t1_coag168) <- "SOFA - Coagulation at 168 hours"
label(final_df$t1_renal168) <- "SOFA - Renal at 168 hours"
label(final_df$t1_liver168) <- "SOFA - Liver at 168 hours"
label(final_df$admission_age) <- "Age"

table1(~ t1_gender + admission_age+ t1_ethnicity + t1_sepsis3 + t1_cns168+ t1_resp168 +t1_coag168 + t1_liver168+ t1_cv168  + t1_renal168
       | t1_icudeath , data=final_df, overall=F,extra.col=list(`P-value`=pvalue),render.missing=NULL,topclass="Rtable1-grid Rtable1-shade Rtable1-times")



##MODEL 2

df2 = final_df

df2[,'m2_age'] <- NA
df2$m2_age <-df2$admission_age / 10

df2[,'m2_gender'] <- NA
df2$m2_gender[df2$gender=="M"] <- 1
df2$m2_gender[df2$gender=="F"] <- 0

df2[,'m2_ethnicity'] <- NA
df2$m2_ethnicity = df2$ethnicity
df2$m2_ethnicity[df2$ethnicity == 'OTHER' 
                 | df2$ethnicity == 'UNABLE TO OBTAIN'
                 | df2$ethnicity == 'UNKNOWN'
                 | df2$ethnicity == 'AMERICAN INDIAN/ALASKA NATIVE'] <- "OTHER"

df2["m2_icudeath"] = df2$m1_icudeath
df2["m2_168situation"] = df2$t1_168situation
table(df2$m2_168situation)   # should expect only ALive

df2[,'m2_resp_168'] <- "RESP=5"
df2$m2_resp_168[ (df2$newvent168== 1)] <- "RESP>0"
df2$m2_resp_168[ (df2$resp_168>=1) &  (df2$newvent168== 0)] <- "RESP>0"
df2$m2_resp_168[ (df2$resp_168==0) &  (df2$newvent168== 0)] <- "RESP=0"
table(df2$m2_resp_168)


df2[,'m2_cv_168'] <- "CV=5"
df2$m2_cv_168[ (df2$cv_168==0)] <- "CV=0"
df2$m2_cv_168[ (df2$cv_168>=1)] <- "CV>0"
table(df2$m2_cv_168)


df2[,'m2_coag_168'] <- "COAG=5"
df2$m2_coag_168[ (df2$coag_168==0)] <- "COAG=0"
df2$m2_coag_168[ (df2$coag_168>=1)] <- "COAG>0"



df2[,'m2_liver_168'] <- "LIVER=5"
df2$m2_liver_168[(df2$liver_168==0)] <- "LIVER=0"
df2$m2_liver_168[(df2$liver_168>=1)] <- "LIVER>0"




df2[,'m2_renal_168'] <- "RENAL=5"
df2$m2_renal_168[ (df2$renal_168==0)] <- "RENAL=0"
df2$m2_renal_168[ (df2$renal_168>=1)] <- "RENAL>0"





df2[,'m2_cnsvent_168'] <- "CNS=5"
df2$m2_cnsvent_168[(df2$m2_168situation =="Alive") & (df2$vent_168== "InvasiveVent")] <- "ON VENT"
df2$m2_cnsvent_168[(df2$m2_168situation =="Alive") & (df2$cns_168==0) & (is.na(df2$vent_168))] <- "Not on Vent and = 0"
df2$m2_cnsvent_168[(df2$m2_168situation =="Alive") & (df2$cns_168==0) & (df2$vent_168!= "InvasiveVent")] <- "Not on Vent and = 0"
df2$m2_cnsvent_168[(df2$m2_168situation =="Alive") & (df2$cns_168>=1) & (is.na(df2$vent_168))] <- "Not on Vent and > 0"
df2$m2_cnsvent_168[(df2$m2_168situation =="Alive") & (df2$cns_168>=1) & (df2$vent_168!= "InvasiveVent")] <- "Not on Vent and > 0"
table(df2$m2_cnsvent_168)



df2 <- within(df2, m2_ethnicity <- relevel(factor(m2_ethnicity), ref = "WHITE"))
df2 <- within(df2, m2_cnsvent_168 <- relevel(factor(m2_cnsvent_168), ref = "Not on Vent and = 0"))
df2 <- within(df2, m2_coag_168 <- relevel(factor(m2_coag_168), ref = "COAG=0"))
df2 <- within(df2, m2_resp_168 <- relevel(factor(m2_resp_168), ref = "RESP=0"))
df2 <- within(df2, m2_cv_168 <- relevel(factor(m2_cv_168), ref = "CV=0"))
df2 <- within(df2, m2_renal_168 <- relevel(factor(m2_renal_168), ref = "RENAL=0"))
df2 <- within(df2, m2_liver_168 <- relevel(factor(m2_liver_168), ref = "LIVER=0"))





m2 <- glm(m2_icudeath ~  m2_age + m2_gender + m2_ethnicity + m2_cnsvent_168 + m2_resp_168+m2_coag_168
          +m2_liver_168 + m2_cv_168 + m2_renal_168, data = df2, family = "binomial"(link=logit))
summary(m2)
m2_OR <- exp(cbind(OR = coef(m2), confint(m2)))
#write.csv(m2_OR,"M2_OR_MIMIC.csv")
