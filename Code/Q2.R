library(dplyr)
library(tidyverse)
library(haven)
library(readr)
library(parallel)
library(readxl)
library(stargazer)
library(knitr)
library(estimatr)
library(fixest)
library(cobalt)
rm(list=ls())

setwd("/Users/shubhalakshminag/Dropbox/Cornell coursework/Semester 1/Applied micro 1/PS 1/2022_aem-7010_ps1_pt2_q2_materials")

#--------------Importing data --------------#

BTB_data <- read_dta("BTB_data.dta")
MSA_data <- read_dta("MSA_unempt.dta")
Metro_code <- read_xlsx("MSA_descriptives.xlsx", sheet="MSAs")
State_code <- read_xlsx("MSA_descriptives.xlsx", sheet="States")

#-------------- Explore + clean --------------#

BTB_data %>% glimpse() # 8,773,758 observations and 26 variables 
BTB_data %>% distinct() # 8,472,721 observations 
table(BTB_data$year) # 2004-2014


# Checking for missing values 
sapply(BTB_data, function(x) sum(is.na(x))) # employed has missing values

# Creating an age bracket variable 

BTB_data <- BTB_data %>%
  mutate(
  age_25_34=ifelse(age<=34 & age>=25, 1, 0),
  age_35_64=ifelse(age<=64 & age>=35, 1, 0),
  age_cat=ifelse(age_25_34==1, "Age: 25-34", "Age: 35-64")
)

# Creating region and metro variables

BTB_data <-  BTB_data %>% 
  mutate(
    # Creating region codes
    region=case_when(
      stateFIPS== 9 | stateFIPS== 23 | stateFIPS== 25 | stateFIPS== 33 | stateFIPS== 44 | stateFIPS== 50 | stateFIPS== 34 | stateFIPS== 36 | stateFIPS== 42
      ~ 1,
      stateFIPS== 17 | stateFIPS== 18 | stateFIPS== 26 | stateFIPS== 39 | stateFIPS== 55 | stateFIPS== 19 | stateFIPS== 20 |
        stateFIPS== 27 | stateFIPS== 29 | stateFIPS== 31 | stateFIPS== 38 | stateFIPS== 46
      ~ 2,
      stateFIPS== 10 | stateFIPS== 11 | stateFIPS== 12 | stateFIPS== 13 | stateFIPS== 24 | stateFIPS== 37 | stateFIPS== 45 |
        stateFIPS== 51 | stateFIPS== 54 | stateFIPS== 1 | stateFIPS== 21 | stateFIPS== 28 | stateFIPS== 47 | stateFIPS== 5 |
        stateFIPS== 22 | stateFIPS== 40 | stateFIPS== 48
      ~ 3, 
      stateFIPS== 4 | stateFIPS== 8 | stateFIPS== 16 | stateFIPS== 30 | stateFIPS== 32 | stateFIPS== 35 | stateFIPS== 49 | stateFIPS== 56
      | stateFIPS== 2 | stateFIPS== 6 | stateFIPS== 15 | stateFIPS== 41 | stateFIPS== 53
      ~ 4
    ),
    R_ne = ifelse(region==1, 1, 0),
    R_m = ifelse(region==2, 1, 0),
    R_s = ifelse(region==3, 1, 0),
    R_w = ifelse(region==4, 1, 0),
    metro=ifelse(metroFIPS!=stateFIPS, 1, 0)
  )


#-------------- Table 2 --------------#

age_24_34 <- BTB_data %>% 
  filter(age_25_34==1, male==1, citizen==1, retired==0 & !is.na(employed)) %>% 
  select('BTB', 'employed', 'noHS', 'noCollege', 'college', 'enrolledschool', 'age', 'whiteNH', 'blackNH', 'hisp',
         'R_ne', 'R_m', 'R_w', 'R_s', 'metro') %>% 
  as.data.frame() %>% 
  stargazer(omit.summary.stat = c("min", "max"))

age_35_64 <- BTB_data %>% 
  filter( age_35_64==1, male==1, citizen==1, retired==0 & !is.na(employed)) %>% 
  select('BTB', 'employed', 'noHS', 'noCollege', 'college', 'enrolledschool', 'age', 'whiteNH', 'blackNH', 'hisp',
         'R_ne', 'R_m', 'R_w', 'R_s', 'metro') %>% 
  as.data.frame() %>% 
  stargazer(omit.summary.stat = c("min", "max"))


#-------------- Table 4 --------------#

# Merging BTB data with State data 

BTB_state <-  BTB_data %>%
  filter(age_25_34==1, male==1, citizen==1, retired==0 & noCollege==1) %>% 
  left_join(State_code, by="stateFIPS") %>% 
  # time variable for trends
  group_by(year, month) %>% 
  mutate(time=group_indices()) %>% 
  ungroup() %>% 
  mutate(BTB_only=ifelse(is.na(BTB_max2014), 0, 1),
         # Creating interaction variables for main X
         BTBxWhiteNH=BTB*whiteNH,
         BTBxBlackNH=BTB*blackNH,
         BTBxHisp=BTB*hisp,
         # Creating demongraphy groups 
         group=case_when(
           whiteNH==1 ~ 1,
           blackNH==1 ~ 2,
           hisp==1 ~ 3
         ),
         # Time X region FE
         R_neXtime=R_ne*time,
         R_mXtime=R_m*time,
         R_sXtime=R_s*time,
         R_wXtime=R_w*time
         ) 

# Calculating jurisdictions which ever had a BTB jurisdiction

BTB_Metros <-  BTB_data %>% 
  select(BTB, metroFIPS) %>% 
  distinct() %>% 
  filter(BTB==1) %>% 
  rename(BTB_ever=BTB)

BTB_state <- BTB_state %>% 
  left_join(BTB_Metros) %>% 
  mutate(BTB_ever_reg=ifelse(!is.na(BTB_ever) & BTB_only==1, 1, 0)) 

# Main sample for Table 4

main_sample <- BTB_state

MSA_sample <- BTB_state %>% 
  filter(metroFIPS!=stateFIPS)

BTB_adopt_sample <- BTB_state %>% 
  filter(BTB_ever_reg==1)

# Data for table 8 

col8_sample <- BTB_state %>% 
  mutate(main_sample=1) %>% 
  filter(metroFIPS!=stateFIPS) %>% 
  left_join(MSA_data, join_by(year==year, 
                              month==month, 
                              metroFIPS==metroFIPS)) %>% 
  mutate(UnemploymentRate=ifelse(is.na(UnemploymentRate), 0, UnemploymentRate),
         unemp_missing=ifelse(is.na(UnemploymentRate), 1, 0)) %>% 
  rename(stateFIPS=stateFIPS.x)

# Regressions

# Column 1

column_1 <- feols(employed ~ BTBxWhiteNH + BTBxBlackNH + BTBxHisp + blackNH + hisp | metroFIPS, 
                      data = main_sample, cluster = "stateFIPS")
summary(column_1)


# Column 2

column_2 <- feols(employed ~ BTBxWhiteNH + BTBxBlackNH + BTBxHisp + blackNH + hisp + enrolledschool
                  | metroFIPS + age + highestEdu, 
                  data = main_sample, cluster = "stateFIPS")
summary(column_2)

# Column 3

column_3 <- feols(employed ~ BTBxWhiteNH + BTBxBlackNH + BTBxHisp + blackNH + hisp 
                  | metroFIPS + age + highestEdu + enrolledschool + R_neXtime + R_mXtime +
                    R_wXtime + R_sXtime, 
                  data = main_sample, cluster = "stateFIPS")
  
summary(column_3)

# Column 4

column_4 <- feols(employed ~ BTBxWhiteNH + BTBxBlackNH + BTBxHisp + blackNH + hisp 
                  | metroFIPS + age + highestEdu + enrolledschool + R_neXtime + R_mXtime +
                    R_wXtime + R_sXtime + metroFIPS[time], 
                  data = main_sample, cluster = "stateFIPS")
summary(column_4)

# column 5 

column_5 <- feols(employed ~ BTBxWhiteNH + BTBxBlackNH + BTBxHisp + blackNH + hisp 
                  | metroFIPS + age^group + highestEdu^group  + enrolledschool^group  +
                    R_neXtime^group + R_mXtime^group + R_wXtime^group + R_sXtime^group + metroFIPS[time] +
                    metroFIPS^group[time] , 
                  data = main_sample, cluster = "stateFIPS")
summary(column_5)

# column 6 

column_6 <- feols(employed ~ BTBxWhiteNH + BTBxBlackNH + BTBxHisp + blackNH + hisp 
                  | metroFIPS + age^group + highestEdu^group  + enrolledschool^group  +
                    R_neXtime^group + R_mXtime^group + R_wXtime^group + R_sXtime^group + metroFIPS[time] +
                    metroFIPS^group[time] , 
                  data = MSA_sample, cluster = "stateFIPS")
summary(column_6)

# column 7 

column_7 <- feols(employed ~ BTBxWhiteNH + BTBxBlackNH + BTBxHisp + blackNH + hisp 
                  | metroFIPS + age^group + highestEdu^group  + enrolledschool^group  +
                    R_neXtime^group + R_mXtime^group + R_wXtime^group + R_sXtime^group + metroFIPS[time] +
                    metroFIPS^group[time] , 
                  data = BTB_adopt_sample, cluster = "stateFIPS")
summary(column_7)

# column 8 

column_8 <- feols(employed ~ BTBxWhiteNH + BTBxBlackNH + BTBxHisp + blackNH + hisp + 
                    UnemploymentRate + unemp_missing
                  | metroFIPS + age^group + highestEdu^group  + enrolledschool^group  +
                    R_neXtime^group + R_mXtime^group + R_wXtime^group + R_sXtime^group + metroFIPS[time] +
                    metroFIPS^group[time] , 
                  data = col8_sample, cluster = "stateFIPS")
summary(column_8)

# Final table using stargazer 

etable(column_1, column_2, column_3, column_4, column_5, column_6, column_7, column_8,
       keep=c('BTBxWhiteNH',  'BTBxBlackNH', 'BTBxHisp'),  se.below = T, fitstat =~n, 
       file="/Users/shubhalakshminag/Dropbox/AEM-7010-PS1-Group4/Output/Q2_table4.tex")

# pre-baseline means 

# Main sample 

main_sample %>% 
  filter(whiteNH==1 & BTB==0 & BTB_ever==1) %>% 
  summarise(mean_empl=mean(employed, na.rm = T))

main_sample %>% 
  filter(blackNH==1 & BTB==0 & BTB_ever==1) %>% 
  summarise(mean_empl=mean(employed, na.rm = T))

main_sample %>% 
  filter(hisp==1 & BTB==0 & BTB_ever==1) %>% 
  summarise(mean_empl=mean(employed, na.rm = T))


# BTB sample 

BTB_adopt_sample %>% 
  filter(whiteNH==1 & BTB==0 & BTB_ever==1) %>% 
  summarise(mean_empl=mean(employed, na.rm = T))

BTB_adopt_sample %>% 
  filter(blackNH==1 & BTB==0 & BTB_ever==1) %>% 
  summarise(mean_empl=mean(employed, na.rm = T))

BTB_adopt_sample %>% 
  filter(hisp==1 & BTB==0 & BTB_ever==1) %>% 
  summarise(mean_empl=mean(employed, na.rm = T))

# MSA sample 

MSA_sample %>% 
  filter(whiteNH==1 & BTB==0 & BTB_ever==1) %>% 
  summarise(mean_empl=mean(employed, na.rm = T))

MSA_sample %>% 
  filter(blackNH==1 & BTB==0 & BTB_ever==1) %>% 
  summarise(mean_empl=mean(employed, na.rm = T))

MSA_sample %>% 
  filter(hisp==1 & BTB==0 & BTB_ever==1) %>% 
  summarise(mean_empl=mean(employed, na.rm = T))


#-------------- Tables for Q3 --------------#
balance_covariates_w <- BTB_state %>% 
  filter(group==1) %>% 
  select('BTB', 'employed', 'noHS', 'enrolledschool', 'age',
         'R_ne', 'R_m', 'R_w', 'R_s', 'metro', 'BTB_ever_reg')
  

balance_covariates_b<- BTB_state %>% 
  filter(group==2) %>%
  select('BTB', 'employed', 'noHS', 'enrolledschool', 'age',
         'R_ne', 'R_m', 'R_w', 'R_s', 'metro', 'BTB_ever_reg') 

balance_covariates_h <- BTB_state %>% 
  filter(group==3) %>%
  select('BTB', 'employed', 'noHS', 'enrolledschool', 'age',
         'R_ne', 'R_m', 'R_w', 'R_s', 'metro', 'BTB_ever_reg') 

test_outcomes_w <- lapply(balance_covariates_w[,c(1:10)], function(x) t.test(x ~ BTB_ever_reg, data=balance_covariates_w))
test_outcomes_b<- lapply(balance_covariates_b[,c(1:10)], function(x) t.test(x ~ BTB_ever_reg, data=balance_covariates_b))
test_outcomes_h<- lapply(balance_covariates_h[,c(1:10)], function(x) t.test(x ~ BTB_ever_reg, data=balance_covariates_h))

# Creating the table - white 
table_w <- c()
for(i in 1:10){
  iteration <- c(test_outcomes_w [[i]]$estimate[1], test_outcomes_w [[i]]$estimate[2],  test_outcomes_w[[i]]$p.value)
  table_w <- rbind(table_w, iteration)
}

# Creating the table - black
table_b <- c()
for(i in 1:10){
  iteration <- c(test_outcomes_b[[i]]$estimate[1], test_outcomes_b[[i]]$estimate[2],  test_outcomes_b[[i]]$p.value)
  table_b <- rbind(table_b, iteration)
}


# Creating the table - hisp
table_h <- c()
for(i in 1:10){
  iteration <- c(test_outcomes_h[[i]]$estimate[1], test_outcomes_h[[i]]$estimate[2],  test_outcomes_h[[i]]$p.value)
  table_h <- rbind(table_h, iteration)
}

rbind(table_w, table_b, table_h) %>% 
  kable("latex")

# Counts by groups
BTB_state %>% 
  group_by(group, BTB_ever_reg) %>% 
  summarise(n())















