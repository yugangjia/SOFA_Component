library(tidyverse)
library(lubridate)
library(nycflights13)


rawdata<- read_csv(file='C:/Users/chenzy/Desktop/MIT new/Check/rawdata.csv')
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

write.csv(df6,'C:/Users/chenzy/Desktop/MIT new/check/NewFigure1.csv')
