# Theme 3: Measures of Disease Frequency ####
# 3.a Packages and Data ####
rm(list=ls()) 
library(epiR); library(ggplot2); library(scales); library(zoo); library(dplyr)
#We're going to make use of the epiR package and use Nthiwa et al. to estimate measures of Frequency
# The dataset is in Excel - for this we use a readxl package

# Getting it from online is slightly more complex than a straight CSV file
library(readxl)
temp = tempfile(fileext = ".xlsx")
dataURL <- "https://epicpd.jshiny.com/jdata/epicpd/botswanaVS/coursematerial/tabular/nthiwa_kenya_seroprev.xlsx"
dataURL <- paste0(tempurl, "nthiwa_kenya_seroprev.xlsx")
download.file(dataURL, destfile=temp, mode='wb')
df <- data.frame(readxl::read_excel(temp))
head(df)
str(df)

### Keep temp file in case ###
dfTemp = df
#df = dfTemp

# 3.1 Exploring the initial results and dealing with some data issues ####
#3.1.1 - Total tested ####
(N.tested = nrow(df))

#3.1.2 - Total herds tested ####
(N.herds.tested <- length(unique(df$hse_id)))

#3.1.3 - task - Have a look at the [herd_no] column - can you find the missing herd? ####
str(df)
length(unique(df$herd_no))
max(df$herd_no)
min(df$herd_no)

# there is a herd missing
# we will need a NOT-IN function
'%!in%' <- function(x,y)!('%in%'(x,y))

dfherd.temp <- unique(df$herd_no) #create a temporary dataset called dfherd.temp of the unique herd_no
length(dfherd.temp)

(temp = data.frame(herd = c(1:390))) #create a fake dataset of all herds through 390
subset(temp, temp$herd %!in% dfherd.temp) # so herd number 200 is missing

#lets explore then to find out which hse_id is associated with different herd numbers - there must be one
temp <- df %>% select(c(hse_id, herd_no))
temp <- unique(temp)
length(unique(temp$hse_id))
length(unique(temp$herd_no))

temp %>% group_by(herd_no) %>% summarise(count = n()) %>% filter(count>1)
subset(temp, temp$herd_no == '20')

#herd number 200 is missing, and herd_no 20 is represented by two different hse_id's - certainly likely that M5 should be herd_no 200
#lets assume that's the mistake and replace
subset(df, hse_id == 'M5')
df[df$hse_id == 'M5',]$herd_no<-200
length(unique(df$herd_no)) #correct now

#3.1.3 - task END ####

#3.1.3 - Sex proportions ####
(prop.table(table(df$anim_sex))) # 0 = male
str(df$anim_sex)
df$anim_sex = factor(df$anim_sex)
levels(df$anim_sex)
levels(df$anim_sex) = c("male","female")
(prop.table(table(df$anim_sex))) # another example of the benefits of factorization in R


#3.1.4 - simple proportion in R - Apparent prevalence at Animal Level ####
str(df)
sum(df$fmd_exp_st)/N.tested #[fmd_exp_st] is the key outcome based on sero-status based on case definition

#using epiR we can get confidence intervals
?epi.conf
# 3.1.4 - Aside: Intraclass Correlation ####

# How many samples were tested per herd? - M&M indicated the target was 3
(avgAnimalsPerHerd = mean(df %>% group_by(hse_id) %>% tally() %>% .$n))

# In this analysis the impact of ICC is defined as the Design effect (deff) - it is the ratio of the required sample size to the effective sample size in perfectly random population
#deff = rho * (m-1) + 1

#3.1.4 - task create a function to establish deff
(D = deff(rho=0.1, m = 3))
D <- 0.1 * (3 - 1) + 1; D

#3.1.4 Aside - END ####

#3.1.4a - Prevalence calculation - Based on design effect ####
epi.conf(as.matrix(cbind(sum(df$fmd_exp_st), N.tested[1])), 
         ctype = "agresti", 
         method = "wilson", 
         conf.level = 0.95,
         design = D,
         N = 10000000) * 100 # here make the overarching population large

#compare to no design effect
epi.conf(as.matrix(cbind(sum(df$fmd_exp_st), N.tested[1])), 
         ctype = "prevalence", method = "fleiss", N = 1000000, 
         design = 1, conf.level = 0.95) * 100

#In this case because the sample size is so small per herd and the estimated ICC is so small the design effect has minimal impact

#3.1.4b - Prevalence calculation - Imperfect tests####
#Testing in Parallel - Aside

#sensitivity in parallel can be estimated by Se1 + Se2 - (Se1*Se2)

#Task create a function for this
#SeParrTwoTest = ??
#SpParrTwoTest = ??

# True prevalence estimates
epi.prev(pos = sum(df$fmd_exp_st), tested = N.tested, 
         se = SeParrTwoTest(0.864, 0.864), 
         sp = SpParrTwoTest(0.981, 0.974), method = "wilson",
         units = 100, conf.level = 0.95)

#you'll see the slight difference in AP estimates given that no design effect was included

# 3.1.5 - zonal differences in prevalence ####
df %>% group_by(study_str) %>% summarise(sum(fmd_exp_st)/n()) #check table 1 to adjust factor levels
#adjust factor levels
class(df$study_str)
df$study_str <- factor(df$study_str)
levels(df$study_str)
levels(df$study_str) <- c("Zone 3", "Zone 2","Zone 1")
df %>% group_by(study_str) %>% summarise(prev = sum(fmd_exp_st)/n())

# Using the epi.conf function again to establish realistic 95% confidence intervals
df.zone = table(df$study_str, df$fmd_exp_st)

epi.conf(as.matrix(cbind(df.zone[,2], rowSums(df.zone))), 
         ctype = "prevalence", 
         method = "fleiss", 
         conf.level = 0.95,
         design = D,
         N = 10000000) * 100 #here make the overarching population large

# Now accounting for imperfect tests
# Here vectors are required lets try using a for loop

outputList = list()

for (i in unique(df$study_str)){
  datatemp = df[df$study_str == i,]
  
  outputList[i] = epi.prev(pos = sum(datatemp$fmd_exp_st), tested = nrow(datatemp), 
         se = SeParrTwoTest(0.864, 0.864), 
         sp = SpParrTwoTest(0.981, 0.974), method = "wilson",
         units = 100, conf.level = 0.95)
}

lapply(outputList, print)
plyr::ldply(outputList, data.frame) #nice way to fit a list to dataframe

# 3.1.6 task - establish the sex proportions with their 95% confidence intervals ####

# 3.1.7 - Prevalence estimates - Botswana context ####

# pull data into R
wahid_outbreaks <- read.csv("https://epicpd.jshiny.com/jdata/epicpd/botswanaVS/coursematerial/tabular/wahid_botswana_outbreaklevel.csv")
wahid_species <- read.csv("https://epicpd.jshiny.com/jdata/epicpd/botswanaVS/coursematerial/tabular/wahid_botswana_specieslevel.csv")

str(wahid_outbreaks)
str(wahid_species)

# 3.1.7.1 Create a dataset only of FMD data ####
wahid_outbreaks.FMD = subset(wahid_outbreaks, diseases == 'Foot and mouth disease virus (Inf. with) ')
wahid_outbreaks.FMD = merge(wahid_outbreaks.FMD, wahid_species)

#drop any data where no cases were reported, or where no susceptible were reported
wahid_outbreaks.FMD <- wahid_outbreaks.FMD[wahid_outbreaks.FMD$totalNcase > 0, ]
wahid_outbreaks.FMD <- wahid_outbreaks.FMD[wahid_outbreaks.FMD$totalSusceptible > 0, ]
#subset the data to make it more manageable
wahid_outbreaks.FMD = wahid_outbreaks.FMD %>% select(oieReference, eventIdOIEReference,totalSusceptible, 
                                                     totalNcase, spicieName,
                                                     latitude, longitude, outbreakStartDate)
head(wahid_outbreaks.FMD)
nrow(wahid_outbreaks.FMD)
length(unique(wahid_outbreaks.FMD$oieReference)) # 97 FMD outbreaks reported

#create a unique id for each oieReference and species since one outbreak may be associated with multiple species
wahid_outbreaks.FMD$uniqueRef = paste0(wahid_outbreaks.FMD$oieReference,"_",wahid_outbreaks.FMD$spicieName)
length(unique(wahid_outbreaks.FMD$uniqueRef)) #just a check
head(wahid_outbreaks.FMD)

#another option to make a unique value per row - add a row number
wahid_outbreaks.FMD$ID = as.integer(rownames(wahid_outbreaks.FMD))
head(wahid_outbreaks.FMD)

wahid_outbreaks.FMD.prev<-epi.conf(as.matrix(cbind(wahid_outbreaks.FMD$totalNcase, wahid_outbreaks.FMD$totalSusceptible)), 
         ctype = "prevalence", 
         method = "fleiss", 
         conf.level = 0.95,
         design = 1,
         N = 10000000) * 100 #here make the overarching population large

head(wahid_outbreaks.FMD.prev) # the new dataset needs to be bound to the original 

wahid_outbreaks.FMD <- cbind(wahid_outbreaks.FMD,wahid_outbreaks.FMD.prev)
wahid_outbreaks.FMD <- wahid_outbreaks.FMD[sort.list(wahid_outbreaks.FMD$est),] # sort the dataset by the prev estimate
wahid_outbreaks.FMD$rank <- 1:nrow(wahid_outbreaks.FMD) #create a rank
head(wahid_outbreaks.FMD)

#3.1.7.2 Plot results - Aside ####
library(ggplot2)
plot = ggplot(data = wahid_outbreaks.FMD, 
              aes(x = rank, y = est)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_point() +
  scale_x_continuous(breaks = wahid_outbreaks.FMD$rank, 
                     labels = wahid_outbreaks.FMD$oieReference, 
                     name = "ID") +
  scale_y_continuous(limits = c(0,100), name = "Prevalence estimates (%)") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot

# ggplot - single continuous variable
baseSinglePlot = ggplot(data = wahid_outbreaks.FMD,
                        aes(est))

baseSinglePlot + geom_density(kernel = "gaussian")
baseSinglePlot + geom_histogram(binwidth = 5)

# ggplot - discrete variable
baseSinglePlot = ggplot(data = wahid_outbreaks.FMD,
                        aes(spicieName))

baseSinglePlot + geom_bar()

# ggplot two continuous variables
baseSinglePlot = ggplot(data = wahid_outbreaks.FMD,
                        aes(x = totalSusceptible, y = totalNcase))

baseSinglePlot + geom_point()
baseSinglePlot + geom_quantile()

#3.1.7.3 Data by year ####
wahid_outbreaks.FMD$outbreakStartDate = as.POSIXct(wahid_outbreaks.FMD$outbreakStartDate, format = "%Y-%m-%d")
wahid_outbreaks.FMD$outbreakYear = format(wahid_outbreaks.FMD$outbreakStartDate, "%Y")

plot = ggplot(data = wahid_outbreaks.FMD, aes(x = rank, y = est)) +
  theme_bw() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  geom_point() +
  scale_x_continuous(breaks = wahid_outbreaks.FMD$rank, 
                     labels = wahid_outbreaks.FMD$oieReference, 
                     name = "ID") +
  scale_y_continuous(limits = c(0,100), name = "Prevalence estimates (%)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot + facet_wrap(~outbreakYear)

# another option
plot.box = ggplot(data = wahid_outbreaks.FMD, aes(x = outbreakYear, y = est)) +
  theme_bw() +
  geom_boxplot() + 
  scale_y_continuous(limits = c(0,100), name = "Prevalence estimates (%)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot.box

#3.1.7.4 Data by longitude and latitude #### 
plot.box.long = ggplot(data = wahid_outbreaks.FMD, aes(x = as.integer(longitude), y = est, group = as.integer(longitude))) +
  theme_bw() +
  geom_boxplot() + 
  scale_y_continuous(limits = c(0,100), name = "Prevalence estimates (%)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  
plot.box.long
plot.box.long + facet_wrap(~as.integer(latitude), ncol = 1)

plot.box.lat = ggplot(data = wahid_outbreaks.FMD, 
                      aes(x = as.integer(latitude), y = est, group = as.integer(latitude))) +
  theme_bw() +
  geom_boxplot() + 
  scale_y_continuous(limits = c(0,100), name = "Prevalence estimates (%)") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot.box.lat

ggplot(data = wahid_outbreaks.FMD,
       aes(x = longitude, y = latitude, color = est)) + 
  geom_point()


# 3.2 Frequency over time - epidemic curves ####
#drop any data where no cases were reported, or where no susceptible were reported

wahid_outbreaks.epiCurve <- wahid_outbreaks
str(wahid_outbreaks.epiCurve)

# epicurves at their most basic require date, count
# include any categorization
wahid_outbreaks.epiCurve <- wahid_outbreaks.epiCurve[,c("reportInfoId", "oieReference","outbreakStartDate","diseases")]
wahid_outbreaks.epiCurve$outbreakStartDate = as.Date(wahid_outbreaks.epiCurve$outbreakStartDate, format = "%Y-%m-%d")

library(zoo) #for yearmon functionality

wahid_outbreaks.epiCurve.plot<- wahid_outbreaks.epiCurve %>%
  # use 'as.yearmon' to create a variable identifying the unique year-month
  # combination in which each observation falls
  mutate(yearmon = as.yearmon(outbreakStartDate)) %>%
  # use that variable to group the data
  group_by(reportInfoId, diseases, yearmon) %>%
  # count the number of observations in each of those year-month bins. if you
  # want to summarise the data some other way, use 'summarise' here instead.
  tally()

head(wahid_outbreaks.epiCurve.plot)
# lets make the disease names simplified
wahid_outbreaks.epiCurve.plot$diseases = factor(wahid_outbreaks.epiCurve.plot$diseases)
levels(wahid_outbreaks.epiCurve.plot$diseases)
summary(wahid_outbreaks.epiCurve.plot$diseases)
newlevels = c("Aeromonas", "Anthrax", "EUS","Bluetongue","Brucella abortus","Dourine","FMD","HPAI","NCD","RVF")
levels(wahid_outbreaks.epiCurve.plot$diseases) = newlevels
summary(wahid_outbreaks.epiCurve.plot$diseases)

ggplot(wahid_outbreaks.epiCurve.plot, 
       aes(x = yearmon, y = n, 
           fill = as.factor(reportInfoId))) + 
  geom_col() + facet_wrap(~diseases)


ggplot() +
  theme_bw() +
  geom_histogram(wahid_outbreaks.epiCurve, 
                 mapping = aes(x = outbreakStartDate, weight = 1), 
                 binwidth = 365, 
                 fill = "#738ca6", colour = "grey", size = 0.1) +
  scale_x_date(breaks = date_breaks("12 months"), labels = date_format("%b %Y"), 
               name = "Date") +
  scale_y_continuous(limits = c(0,50), name = "Number of cases") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


ggplot(wahid_outbreaks.epiCurve, aes(x=outbreakStartDate, fill=diseases)) + 
  stat_bin(binwidth=30, position="identity") + 
  scale_x_date(breaks=date_breaks(width="12 months"))

ggplot(wahid_outbreaks.epiCurve.plot, aes(x=as.Date(yearmon), y=n, fill = diseases)) + 
  geom_bar(stat='identity') +
  labs(x="Case Month", y="Case Count") +
  theme_bw() + 
  scale_y_continuous(limits = c(0,max(wahid_outbreaks.epiCurve.plot$n*1.5)), 
                     breaks = seq(0,max(wahid_outbreaks.epiCurve.plot$n*1.5),10)) + 
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %y", 
               name = "Date") + 
  theme(axis.text.x=element_text(angle=60, hjust=1),
        panel.grid = element_blank())



