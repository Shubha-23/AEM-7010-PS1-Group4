library(readr)

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


# Calculate FTE Employment variable
ck1994['FTE'] <- ck1994['EMPFT'] + ck1994['NMGRS'] + 0.5*ck1994['EMPPT'] # for wave 1
ck1994['FTE2'] <- ck1994['EMPFT2'] + ck1994['NMGRS2'] + 0.5*ck1994['EMPPT2'] # for wave 2



