# 7 Further analysis of diagnostic tests ####
rm(list = ls())
require(epiR)
require(dplyr)
baseURL = "https://epicpd.jshiny.com/jdata/epicpd/botswanaVS/coursematerial"
source(paste0(baseURL,"/functions/01_functions.R"))
df <- importTabularOnlineExcel(filePlusExt = "nthiwa_kenya_seroprev.xlsx")
str(df)
View(df)

#subset the dataset to involve data we are interested in ####
df.subset = df[c("study_str","fmd_exp_st", "fmd_exp_sen", "NSP_IZSLER", "NSP_ILRI","sero_O","sero_A","sero_SAT1","sero_SAT2","sgl_mult_ser")]
#create the multiSerotypeField - it was not the sgl_mult_ser field - this was a classification field 

df.subset$sgl_mult_ser.2more = ifelse(df.subset$sgl_mult_ser==5,1,0) # Multiserotype >= 2


# try establish what the fmd_exp_sen variable is for
table(factor(df.subset$fmd_exp_st, labels = c("stNeg","stPos")),factor(df.subset$fmd_exp_sen, labels = c("senNeg","senPos")))

# the fmd_exp_sen variable is where an animal was positive but where structural protein tests were negative - see 3.2 of the manuscript
nrow(df.subset %>% filter(fmd_exp_st == 1,fmd_exp_sen == 0)) # 49 indicated in manuscript, 43 in dataset - misclassified in the dataset
nrow(df.subset %>% filter(fmd_exp_sen == 1)) #938 in dataset, 932 in manuscript - misclassified in the dataset

df.subset %>% filter(fmd_exp_sen == 1 & is.na(sero_O))
#all SP tests were NA - n = 6;
# reclassify to correct dataset
df.subset[which(df.subset$fmd_exp_sen==1 & is.na(df.subset$sero_O)),]$fmd_exp_sen  = 0

# 7.1 Zonal subdivisions - Table 2 ####
df.subset$study_str = factor(df.subset$study_str, levels = c(0, 1, 2), labels = c("Zone 3","Zone 2","Zone 1"))

table(df.subset$study_str, df.subset$sero_SAT1)
table(df.subset$study_str, df.subset$sero_SAT2)
table(df.subset$study_str, df.subset$sero_O)
table(df.subset$study_str, df.subset$sero_A)
table(df.subset$study_str, df.subset$sgl_mult_ser.2more)

# 7.2 Test agreement - NSP ####
#Aside - Kappa statistic
# Coin toss exercise
person1 <- c(0,1,1,1,0)
person2 <- c(1,0,1,0,1)

epi.kappa(table(person1, person2), alternative = "greater", 
          conf.level = 0.95)

df.subset$NSP_ILRI = factor(df.subset$NSP_ILRI, levels = c(0,1), labels = c("NSP_ILRI_Neg","NSP_ILRI_Pos"))
df.subset$NSP_IZSLER = factor(df.subset$NSP_IZSLER, levels = c(0,1), labels = c("NSP_IZSLER_Neg","NSP_IZSLER_Pos"))

epi.kappa(table(df.subset$NSP_IZSLER, df.subset$NSP_ILRI), alternative = "greater", 
          conf.level = 0.95)

#7.3 Diagnostic sensitivity and specificity - Nthiwa et al. Results 3.1 - Para 1
str(df.subset)
#For the epi.tests function the 2X2 table order is important - see ?epi.tests

df.subset$fmd_exp_st = factor(df.subset$fmd_exp_st, levels = c(1,0), labels = c("fmdPos","fmdNeg"))
df.subset$NSP_IZSLER <- relevel(df.subset$NSP_IZSLER, "NSP_IZSLER_Pos") #re-order the test factor to start with Positive
levels(df.subset$NSP_IZSLER)

epi.tests(table(df.subset$NSP_IZSLER,df.subset$fmd_exp_st), conf.level = 0.95)

#Do the same for the ILRI ELISA
levels(df.subset$NSP_ILRI)
df.subset$NSP_ILRI <- relevel(df.subset$NSP_ILRI, "NSP_ILRI_Pos")

epi.tests(table(df.subset$NSP_ILRI,df.subset$fmd_exp_st), conf.level = 0.95)

#Unfortunately the manuscript reported the AP, not the sensitivity of the tests. 

#7.4 McNemar's test ####
# is a test of difference between proportions
mcnemar.test(table(df.subset$NSP_ILRI,df.subset$NSP_IZSLER))


