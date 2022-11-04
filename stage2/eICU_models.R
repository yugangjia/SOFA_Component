library(tidyverse)
library(lubridate)
library(table1)

rawdata<- read_csv(file='raw.csv')
table(rawdata$age)
rawdata$age[rawdata$age == "> 89"] <- "90"
table(rawdata$age)
sum(is.na(rawdata$age))
rawdata$age[is.na(rawdata$age)] <- "0"
table(rawdata$age)
rawdata$age <- as.numeric(rawdata$age)
df1 <-rawdata

## in eICU offset are in minutes, 24*60 = 1440
df1[,'out_in'] <- 5
df1$out_in[df1$unitdischargeoffset - df1$unitadmitoffset <  1440] <- 1
df1$out_in[df1$unitdischargeoffset - df1$unitadmitoffset >= 1440] <- 0
table(df1$out_in)


df1[,'discharge_in'] <- 5
df1$discharge_in[df1$hospitaldischargeoffset - df1$unitadmitoffset <  1440] <- 1
df1$discharge_in[df1$hospitaldischargeoffset - df1$unitadmitoffset >= 1440] <- 0
table(df1$discharge_in)


df2 <-df1[(df1$unitvisitnumber==1),]
nrow(df1) - nrow(df2)

df3 <-df2[!(df2$out_in==1),]
df4 <-df3[!(df3$discharge_in==1),]
nrow(df2) - nrow(df4)

df5<-df4[!(is.na(df4$resp_24)),]
nrow(df4) - nrow(df5)

df6 <- df5[df5$age>=16,]


DRG = read.csv("eICU_DRG.csv")
cohort = df6

cohort1 = cohort[! cohort$patientunitstayid %in% DRG$patientunitstayid,]

cohort2 = cohort1[cohort1$age >= 18,]

rawdata = cohort2


df = rawdata
df$icudeath <- 0
df$icudeath[df$unitdischargestatus == "Expired"] <- 1
df$icudeath[df$hospitaldischargestatus == "Expired" & (df$hospitaldischargeoffset - df$unitdischargeoffset < 4320)] <-1
df$icudeath[df$hospitaldischargestatus == "Expired" & (df$hospitaldischargeoffset < df$unitdischargeoffset)] <- 1

## --------------------------------------------------------
df$icudeath[df$icudeath == 0] <- "Survived or discharged to other locations within 72 hours of ICU discharge"
df$icudeath[df$icudeath == 1] <- "ICU death or Discharged to Hospice within 72 hours of ICU discharge"


## --------------------------------------------------------
df$ethnicity[df$ethnicity == "African American"] <- "BLACK/AFRICAN AMERICAN"
df$ethnicity[df$ethnicity == "Asian"] <- "ASIAN"
df$ethnicity[df$ethnicity == "Caucasian"] <- "WHITE"
df$ethnicity[df$ethnicity == "Hispanic"] <- "HISPANIC/LATINO"
df$ethnicity[df$ethnicity == "Native American" | df$ethnicity == "Other/Unknown"] <- "OTHER"


## --------------------------------------------------------
df$gender[df$gender == 0 ] <- "Female"
df$gender[df$gender == 1 ] <- "Male"


## --------------------------------------------------------
# age
df$age[df$age==">89"] <- 90


## --------------------------------------------------------
abnormalvalue = c(1,2,3,4)


## --------------------------------------------------------

df$coag_24[df$coag_24 == 0] <- "Normal"
df$coag_24[df$coag_24 %in% abnormalvalue] <- "Abnormal"

df$liver_24[df$liver_24 == 0] <- "Normal"
df$liver_24[df$liver_24 %in% abnormalvalue] <- "Abnormal"

df$cv_24[df$cv_24 == 0] <- "Normal"
df$cv_24[df$cv_24 %in% abnormalvalue] <- "Abnormal"

df$renal_24[df$renal_24 == 0] <- "Normal"
df$renal_24[df$renal_24 %in% abnormalvalue] <- "Abnormal"


## --------------------------------------------------------
df$resp_24[df$resp_24 == 0] <- "Normal"
df$resp_24[df$resp_24 %in% abnormalvalue] <- "Abnormal"
df$resp_24[!is.na(df$allv24)] <- "Abnormal"


## --------------------------------------------------------

df$cns_24[df$cns_24 == 0] <- "Normal"
df$cns_24[df$cns_24 %in% abnormalvalue] <- "Abnormal"
df$cns_24[!is.na(df$mv24) | !is.na(df$mvevent24)] <- "Mechanical Ventilation (MV)"


## --------------------------------------------------------
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


## --------------------------------------------------------
df$gender <- factor(df$gender, level=c('Male','Female'))
df$icudeath <- factor(df$icudeath, levels=c("Survived or discharged to other locations within 72 hours of ICU discharge","ICU death or Discharged to Hospice within 72 hours of ICU discharge"))
df$resp_24 <- factor(df$resp_24, levels=c("Abnormal","Normal"))
df$coag_24 <- factor(df$coag_24, levels=c("Abnormal","Normal"))
df$cv_24 <- factor(df$cv_24, levels=c("Abnormal","Normal"))
df$liver_24 <- factor(df$liver_24, levels=c("Abnormal","Normal"))
df$renal_24 <- factor(df$renal_24, levels=c("Abnormal","Normal"))
df$cns_24 <- factor(df$cns_24, levels=c("Mechanical Ventilation (MV)","Abnormal","Normal"))
df$ethnicity <- factor(df$ethnicity, levels=c("HISPANIC/LATINO","BLACK/AFRICAN AMERICAN","WHITE","ASIAN","OTHER"))


## --------------------------------------------------------

label(df$gender) <- "Gender"
label(df$ethnicity) <- "Ethnicity"
label(df$resp_24) <- "SOFA - Respiration at 24 hours"
label(df$cns_24) <- "SOFA-CNS and MV at 24 hours"
label(df$cv_24) <- "SOFA - Cardiovascular at 24 hours"
label(df$coag_24) <- "SOFA - Coagulation at 24 hours"
label(df$renal_24) <- "SOFA - Renal at 24 hours"
label(df$liver_24) <- "SOFA - Liver at 24 hours"
label(df$age) <- "Age"


## --------------------------------------------------------
table1(~ gender + age+ ethnicity + cns_24+ resp_24 +coag_24 + liver_24+ cv_24  + renal_24
       | icudeath, data=df,overall=F,extra.col=list(`P-value`=pvalue),render.missing=NULL,topclass="Rtable1-grid Rtable1-shade Rtable1-times")


## --------------------------------------------------------
df[,'m1_age'] <- NA
df$m1_age <-df$age / 10


## --------------------------------------------------------
df <- within(df, ethnicity <- relevel(factor(ethnicity), ref = "WHITE"))
df <- within(df, cns_24 <- relevel(factor(cns_24), ref = "Normal"))
df <- within(df, coag_24 <- relevel(factor(coag_24), ref = "Normal"))
df <- within(df, resp_24 <- relevel(factor(resp_24), ref = "Normal"))
df <- within(df, cv_24 <- relevel(factor(cv_24), ref = "Normal"))
df <- within(df, renal_24 <- relevel(factor(renal_24), ref = "Normal"))
df <- within(df, liver_24 <- relevel(factor(liver_24), ref = "Normal"))


## --------------------------------------------------------
m1 <- glm(icudeath ~  m1_age + gender + ethnicity + cns_24 + resp_24+coag_24
          +liver_24 + cv_24 + renal_24, data = df, family = "binomial"(link=logit))

## --------------------------------------------------------
summary(m1)
m1_OR <- exp(cbind(OR = coef(m1), confint(m1)))

## --------------------------------------------------------
#write.csv(m1_OR,"24hour OR_eICU.csv")


## --------------------------------------------------------
# still alive at 168 hours, 168 hours = 10080 minutes
df$situation168 <- 0
df$situation168[(df$unitdischargeoffset - df$unitadmitoffset >= 10080) & (df$hospitaldischargeoffset - df$unitadmitoffset >= 10080)] <- 1


## --------------------------------------------------------
# remove null sofa scores at 168 hour
df1 = df[df$situation168 ==1,]
df1 = df1[!is.na(df1$patientunitstayid_2),]


## --------------------------------------------------------
df1$coag_168[df1$coag_168 == 0] <- "Normal"
df1$coag_168[df1$coag_168 %in% abnormalvalue] <- "Abnormal"

df1$liver_168[df1$liver_168 == 0] <- "Normal"
df1$liver_168[df1$liver_168 %in% abnormalvalue] <- "Abnormal"

df1$cv_168[df1$cv_168 == 0] <- "Normal"
df1$cv_168[df1$cv_168 %in% abnormalvalue] <- "Abnormal"

df1$renal_168[df1$renal_168 == 0] <- "Normal"
df1$renal_168[df1$renal_168 %in% abnormalvalue] <- "Abnormal"

df1$resp_168[df1$resp_168 == 0] <- "Normal"
df1$resp_168[df1$resp_168 %in% abnormalvalue] <- "Abnormal"
df1$resp_168[!is.na(df1$allv168)] <- "Abnormal"

df1$cns_168[df1$cns_168 == 0] <- "Normal"
df1$cns_168[df1$cns_168 %in% abnormalvalue] <- "Abnormal"
df1$cns_168[!is.na(df1$mv168) | !is.na(df1$mvevent168)] <- "Mechanical Ventilation (MV)"



## --------------------------------------------------------
df1$resp_168 <- factor(df1$resp_168, levels=c("Abnormal","Normal"))
df1$coag_168 <- factor(df1$coag_168, levels=c("Abnormal","Normal"))
df1$cv_168 <- factor(df1$cv_168, levels=c("Abnormal","Normal"))
df1$liver_168 <- factor(df1$liver_168, levels=c("Abnormal","Normal"))
df1$renal_168 <- factor(df1$renal_168, levels=c("Abnormal","Normal"))
df1$cns_168 <- factor(df1$cns_168, levels=c("Mechanical Ventilation (MV)","Abnormal","Normal"))
df1$ethnicity <- factor(df$ethnicity, levels=c("HISPANIC/LATINO","BLACK/AFRICAN AMERICAN","WHITE","ASIAN","OTHER"))


## --------------------------------------------------------
label(df1$resp_168) <- "SOFA - Respiration at 168 hours"
label(df1$cns_168) <- "SOFA-CNS and MV at 168 hours"
label(df1$cv_168) <- "SOFA - Cardiovascular at 168 hours"
label(df1$coag_168) <- "SOFA - Coagulation at 168 hours"
label(df1$renal_168) <- "SOFA - Renal at 168 hours"
label(df1$liver_168) <- "SOFA - Liver at 168 hours"
label(df1$gender) <- "Gender"
label(df1$ethnicity) <- "Ethnicity"
label(df1$age) <- "Age"


## --------------------------------------------------------
table1(~ gender + age+ ethnicity + cns_168+ resp_168 +coag_168 + liver_168+ cv_168  + renal_168
       | icudeath , data=df1, overall=F,extra.col=list(`P-value`=pvalue),render.missing=NULL,topclass="Rtable1-grid Rtable1-shade Rtable1-times")


## --------------------------------------------------------
df1[,'m2_age'] <- NA
df1$m2_age <-df1$age / 10

df1 <- within(df1, ethnicity <- relevel(factor(ethnicity), ref = "WHITE"))
df1 <- within(df1, cns_168 <- relevel(factor(cns_168), ref = "Normal"))
df1 <- within(df1, coag_168 <- relevel(factor(coag_168), ref = "Normal"))
df1 <- within(df1, resp_168 <- relevel(factor(resp_168), ref = "Normal"))
df1 <- within(df1, cv_168 <- relevel(factor(cv_168), ref = "Normal"))
df1 <- within(df1, renal_168 <- relevel(factor(renal_168), ref = "Normal"))
df1 <- within(df1, liver_168 <- relevel(factor(liver_168), ref = "Normal"))





## --------------------------------------------------------
m2 <- glm(icudeath ~  m2_age + gender + ethnicity + cns_168 + resp_168+coag_168
          +liver_168 + cv_168 + renal_168, data = df1, family = "binomial"(link=logit))
summary(m2)
m2_OR <- exp(cbind(OR = coef(m2), confint(m2)))
#write.csv(m2_OR,"M2_OR_eICU.csv")

