#4. Measures of Association ####
rm(list = ls())

#Packages required
library(epiR); library(ggplot2); library(scales); library(zoo); library(dplyr)
library(readxl);
library(lme4) #linear mixed effects models

# import data ####
temp = tempfile(fileext = ".xlsx")
dataURL <- "https://epicpd.jshiny.com/jdata/epicpd/botswanaVS/coursematerial/tabular/nthiwa_kenya_seroprev.xlsx"
download.file(dataURL, destfile=temp, mode='wb')
df <- data.frame(readxl::read_excel(temp))
# view structure ####
head(df)
str(df)

# A reminder that the outcome of seroprevalence is fmd_exp_st
sum((df$fmd_exp_st))

#4.1 2-by-2 tables and their use with epirR ####
# Aside # 
class(df$anim_sex)
unique(df$anim_sex) # so a numeric class of 1 and 0, and we have previously seen that 0 = male

# For the Nthiwa manuscript and use in EpiR the order must be female; male
df$anim_sex = factor(df$anim_sex, levels = c(1,0), labels =  c("female","male"))
(prop.table(table(df$anim_sex)))

#4.1.1 Standard Odds ratio with no accounting for other variables (AD/BC) ####
class(df$fmd_exp_st) # The outcome variable can also be changed to a factor for easier reading
df$fmd_exp_st = factor(df$fmd_exp_st, levels = c(1,0), labels = c("Dplus","Dminus"))
(sex.assoc = table(df$anim_sex, df$fmd_exp_st))

(sex.assoc.analysis <- epi.2by2(dat = sex.assoc, method = "cross.sectional", conf.level = 0.95, 
                      units = 100, interpret = TRUE, outcome = "as.columns"))


#4.2 Univariate logistic regression models - Herd level Fixed effects ####
# is what they used for their univariate analysis for selection into a multivariate analysis

#For these models numeric data best - lets re-pull the data
temp = tempfile(fileext = ".xlsx")
dataURL <- "https://epicpd.jshiny.com/jdata/epicpd/botswanaVS/coursematerial/tabular/nthiwa_kenya_seroprev.xlsx"
download.file(dataURL, destfile=temp, mode='wb')
df <- data.frame(readxl::read_excel(temp))
head(df)

# 4.2.1 Sex ####
df$anim_sex = factor(df$anim_sex, levels = c(0,1), labels = c("male","female")) #here the order is different to replicate the manuscript
table(df$anim_sex, df$fmd_exp_st)

glmer.sex = glmer(
  fmd_exp_st~anim_sex+(1|hse_id),
  data=df,
  family = binomial) #the response variable is a success or a failure - binomial is appropriate option

summary(glmer.sex)

glmer.sex.se <- sqrt(diag(vcov(glmer.sex))) #compare with the summary standard errors

# table of estimates with 95% CI
(glmer.sex.tab <- cbind(Est = fixef(glmer.sex), LL = fixef(glmer.sex) - 1.96 * glmer.sex.se, UL = fixef(glmer.sex) + 1.96 *
                glmer.sex.se))
exp(glmer.sex.tab) #to get the Odd ratio instead of log odds

# 4.2.2 Zones ####
head(df)
unique(df$study_str)
#check the coding again
df %>% group_by(study_str) %>% tally()
prop.table(table(df$study_str, df$fmd_exp_st), margin = 1)
#0 = zone 3, 2 = zone 1, 1 = zone 2
df$study_str_fct = factor(df$study_str, levels = c(0, 1, 2), labels = c("zone3", "zone2", "zone1"))
prop.table(table(df$study_str_fct, df$fmd_exp_st), margin = 1)

glmer.zone = glmer(
  fmd_exp_st~study_str_fct+(1|hse_id),
  data=df, 
  family = binomial)

summary(glmer.zone)

(glmer.zone.se <- sqrt(diag(vcov(glmer.zone)))) #compare with the summary standard errors
# table of estimates with 95% CI
(glmer.zone.tab <- cbind(Est = fixef(glmer.zone), 
                         LL = fixef(glmer.zone) - 1.96 * glmer.zone.se, 
                         UL = fixef(glmer.zone) + 1.96 * glmer.zone.se)
)

exp(glmer.zone.tab) #to get the Odd ratio instead of log odds

# 4.2.3 repeat for two of the other variables in the model in Table 1 and present results

#4.3 Chi squared tests of association ####
df$anim_sex = factor(df$anim_sex, levels = c(1,0), labels =  c("female","male"))
(prop.table(table(df$anim_sex)))
df$fmd_exp_st = factor(df$fmd_exp_st, levels = c(1,0), labels = c("Dplus","Dminus"))

#Same evaluation as before for the odds's ratio (this time male associated but for Chi Squared this doesn't matter) - Chi Squared analysis 
# effect

(sex.assoc.analysis <- epi.2by2(dat = sex.assoc, method = "cross.sectional", conf.level = 0.95, 
                                units = 100, interpret = TRUE, outcome = "as.columns"))


# 4.4 Multivariate analysis to look for associations controlling for other variables ####
temp = tempfile(fileext = ".xlsx")
dataURL <- "https://epicpd.jshiny.com/jdata/epicpd/botswanaVS/coursematerial/tabular/nthiwa_kenya_seroprev.xlsx"
download.file(dataURL, destfile=temp, mode='wb')
df <- data.frame(readxl::read_excel(temp))
df$study_str_fct = factor(df$study_str, levels = c(0, 1, 2), labels = c("zone3", "zone2", "zone1"))
df$anim_sex = factor(df$anim_sex, levels = c(0,1), labels = c("male","female")) 
head(df)

# Note they used a stepwise  approach evaluating the model by adding and removing variables until left with significant variables
# Beyond the scope of this course - except to say this technique is not considered the best anymore

head(df)
plyr::numcolwise(sum)(df) # quick way to sum all numeric columns (try colSums(df))
#Now we can find the variables they used
df.m = df %>% select(fmd_exp_st, hse_id, anim_sex, study_str_fct, herd_mgpr, mixcat_wpts)
summary(df.m$anim_sex) #means its a factor
summary(df.m$herd_mgpr) #this is not
df.m$herd_mgpr = factor(df.m$herd_mgpr, levels = c(0,1), labels = c("sedentary","pastoral"))
df.m$mixcat_wpts = factor(df.m$mixcat_wpts)
str(df.m)

glmer.mult = glmer(fmd_exp_st~anim_sex+study_str_fct+herd_mgpr+mixcat_wpts+(1|hse_id),
                   data=df.m, family = binomial) #the response variable is a success or a failure - binomial is appropriate option
summary(glmer.mult)

glmer.mult.se <- sqrt(diag(vcov(glmer.mult))) #compare with the summary standard errors
# table of estimates with 95% CI
(glmer.mult.tab <- cbind(Est = fixef(glmer.mult), LL = fixef(glmer.mult) - 1.96 * glmer.mult.se, UL = fixef(glmer.mult) + 1.96 *
                           glmer.mult.se))
exp(glmer.mult.tab) #to get the Odd ratio instead of log odds
#lets plot that
plot.df = data.frame(exp(glmer.mult.tab))
class(plot.df)
row.names(plot.df)
plot.df$variable = row.names(plot.df)
plot.df
(plot.df$y_at = seq(1,nrow(plot.df),1))


ggplot(data = plot.df, aes(x = Est, y = y_at)) +
  geom_point() + 
  geom_errorbarh(aes(xmax = UL, xmin = LL, height = 0.2)) + scale_y_continuous(breaks = plot.df$y_at, labels = plot.df$variable) +
  labs(x = "Odds ratio", y = "Variable") + 
  scale_x_continuous(breaks = seq(min(as.integer(plot.df$LL))-1, max(as.integer(plot.df$UL))+1, 1), 
                     limits = c(min(as.integer(plot.df$LL))-1, max(as.integer(plot.df$UL))+1)) + 
  geom_vline(xintercept = 1, lwd = 1) + 
  coord_fixed(ratio = 2) + 
  theme(axis.title.y = element_text(vjust = 0))+
  ggtitle("Multivarible analysis - Odd ratio's of non-reference variables")
