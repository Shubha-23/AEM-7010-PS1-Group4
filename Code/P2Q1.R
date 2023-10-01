library(readr)
library(data.table)
library(dplyr)

in_path <- getwd()
ck1994 <- read.table(file.path(in_path, 'Data', 'Raw', 'ck1994_data.dat'))
col <- c('SHEET', 'CHAIN', 'CO_OWNED', 'STATE', 'SOUTHJ', 'CENTRALJ',
             'NORTHJ', 'PA1', 'PA2', 'SHORE', 'NCALLS', 'EMPFT', 'EMPPT',
             'NMGRS', 'WAGE_ST', 'INCTIME', 'FIRSTINC', 'BONUS', 'PCTAFF',
             'MEALS', 'OPEN', 'HRSOPEN', 'PSODA', 'PFRY', 'PENTREE', 'NREGS',
             'NREGS11', 'TYPE2', 'STATUS2', 'DATE2', 'NCALLS2', 'EMPFT2',
             'EMPPT2', 'NMGRS2', 'WAGE_ST2', 'INCTIME2', 'FIRSTIN2', 'SPECIAL2',
             'MEALS2', 'OPEN2R', 'HRSOPEN2', 'PSODA2', 'PFRY2', 'PENTREE2', 
             'NREGS2','NREGS112')
            
colnames(ck1994) <- col

# Data preparation
# Replace . with na
str(ck1994)
ck1994[ck1994 == '.'] <- NA
# Adjust data type change string to numeric
ck1994[, c(12:17,19,23:27,31:46)] <- sapply(ck1994[, c(12:17,19,23:27,31:46)], as.numeric)

# Calculate FTE Employment variable
ck1994['FTE'] <- ck1994['EMPFT'] + ck1994['NMGRS'] + 0.5*ck1994['EMPPT'] # for wave 1
ck1994['FTE2'] <- ck1994['EMPFT2'] + ck1994['NMGRS2'] + 0.5*ck1994['EMPPT2'] # for wave 2

# (a)
ck1994['num'] <- 1
setDT(ck1994)
T1_1 <- ck1994 %>% 
        group_by(STATE, CHAIN) %>% 
        summarize(sum = sum(num))
# 1 NJ 0 PA
T1_1['StateSum'] = ifelse(T1_1['STATE']==1,79,331)

# (c)
# Create Variables needed
# Calculate FTE Employment variable
ck1994$FTE <- ck1994$EMPFT + ck1994$NMGRS + 0.5*ck1994$EMPPT # for wave 1
ck1994$FTE2 <- ck1994$EMPFT2 + ck1994$NMGRS2 + 0.5*ck1994$EMPPT2 # for wave 2

# Percentage full-time employees
ck1994$PFT <- ck1994$EMPFT/ck1994$FTE
ck1994$PFT2 <- ck1994$EMPFT2/ck1994$FTE2


# Notice there are NAs
ck1994$WAGEST_425 <- ifelse(ck1994$WAGE_ST ==4.25, 1, 0) 
ck1994$WAGEST2_425 <- ifelse(ck1994$WAGE_ST2 ==4.25, 1, 0) 
ck1994$WAGEST2_505 <- ifelse(ck1994$WAGE_ST2 ==5.05, 1, 0) 

# Price of Full Meal
ck1994$PFM <- ck1994$PSODA + ck1994$PFRY + ck1994$PENTREE
ck1994$PFM2 <- ck1994$PSODA2 + ck1994$PFRY2 + ck1994$PENTREE2

# (c)
# 2 digit
T1_2 <- ck1994 %>% 
  group_by(STATE) %>% 
  summarize(FTE2_mean = mean(FTE2, na.rm = TRUE),
            PFT2_mean = mean(PFT2, na.rm = TRUE),
            WAGEST2_mean = mean(WAGE_ST2, na.rm = TRUE),
            WAGEST2_425_pct = 100 * sum(WAGEST2_425, na.rm = TRUE)/sum(num, na.rm = TRUE), 
            WAGEST2_505_pct = 100 * sum(WAGEST2_505, na.rm = TRUE)/sum(num, na.rm = TRUE), 
            BONUS2_pct = 100 * sum(SPECIAL2, na.rm = TRUE)/sum(num, na.rm = TRUE),# not accurate
            PFM2_mean = mean(PFM2, na.rm = TRUE),
            HRSOPEN2_mean = mean(HRSOPEN2, na.rm = TRUE)) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))


res <- t.test(PFT2 ~ STATE, data = ck1994)





