#Theme 2: Basic statistics in R ####
rm(list=ls()) 

#2.1 Data wrangling and merging tables ####

# pull data into R
wahid_outbreaks <- read.csv("https://epicpd.jshiny.com/jdata/epicpd/botswanaVS/coursematerial/tabular/wahid_botswana_outbreaklevel.csv")
wahid_species <- read.csv("https://epicpd.jshiny.com/jdata/epicpd/botswanaVS/coursematerial/tabular/wahid_botswana_specieslevel.csv")

# overview of Data
str(wahid_outbreaks)
summary(wahid_outbreaks)

str(wahid_species)
summary(wahid_species)

unique(wahid_outbreaks$outbreakInfoId)

# contingency tables handy to get an overview of data - but also to perform some statistics
# goal in this case to to establish the species, diseases associated with them and outbreak status

# link the outbreak data to the species data
# the common identifier between the outbreaks and species associated is the oieReference column
length(unique(wahid_outbreaks$oieReference))
length(unique(wahid_species$oieReference))
length(wahid_outbreaks$oieReference)
length(wahid_species$oieReference)

# there are some outbreaks that are associated with more than one species
# we want to establish for each outbreak species were cattle involved, either by themselves, or with another 
#species

# lets find them using dplyr
require(dplyr)

(wahid_species.multi <- 
  wahid_species %>% 
  dplyr::group_by(oieReference) %>% 
  tally() %>% 
  filter(n>1))

nrow(wahid_species.multi) #13 outbreaks with multi-species

#now to establish cattle in any of these - goal is cattle or not in each unique outbreak
wahid_species.multi <- subset(wahid_species, subset = oieReference %in% wahid_species.multi$oieReference)
head(wahid_species.multi)

multispecies.withcattle <- data.frame(
  oieReference = unique(subset(wahid_species.multi, 
                               spicieName == 'Cattle')$oieReference)) %>% 
  mutate(classification = 'multispeciesWithCattle',
         species = 'Cattle')

# we're going to need a not in function
'%!in%' <- function(x,y)!('%in%'(x,y))

multispecies.withoutcattle <- data.frame(oieReference = unique(subset(wahid_species.multi,
                                 subset = oieReference %!in% multispecies.withcattle$oieReference)$oieReference)) %>% 
  mutate(classification = 'multispeciesWithoutCattle',
         species = 'non-Cattle')

(wahid_species.multi <- rbind(multispecies.withcattle, multispecies.withoutcattle)) # we now have the multi-species dataframe

#Now to get the single species dataframe
(wahid_species.single <- 
    wahid_species %>% 
    dplyr::group_by(oieReference) %>% 
    tally() %>% 
    filter(n==1))
                            
wahid_species.single <- subset(wahid_species, oieReference %in% wahid_species.single$oieReference) %>% 
  mutate(classification = ifelse(spicieName == "Cattle", "singlespeciesWithCattle", "singlespeciesWithoutCattle"),
         species = ifelse(spicieName == "Cattle", "Cattle", "non-Cattle")) %>% 
  select(oieReference, classification, species)

wahid_species.eval <- rbind(wahid_species.multi, wahid_species.single)
nrow(wahid_species.eval)
head(wahid_species.eval,15)

#we now have a species dataset identifying all outbreaks classifying whether cattle were involved or not
#now to join the two datasets
View(merge(wahid_outbreaks, wahid_species.eval))

# lets select a few columns to use to make things more manageable
# I'll use dplyr - you try using base R#
wahid.eval <- merge(wahid_outbreaks, wahid_species.eval) %>% 
                      select(oieReference, diseases, eventStatus, species, reportInfoIdlink)
str(wahid.eval)
wahid.eval$species <- factor(wahid.eval$species)
str(wahid.eval)

#2.2 -Exploratory Data Analysis - diseases and Cattle based association ####
wahid.eval %>% dplyr::group_by(diseases, species) %>% tally()

# task: find the outbreaks where non-Cattle are associated with FMD and Brucella
subset(wahid.eval, diseases == "Foot and mouth disease virus (Inf. with) " & species == 'non-Cattle')

#2.2.1 Contingency tables in R ####
table(wahid.eval$diseases, wahid.eval$species)
wahid.eval <- wahid.eval %>% mutate(fmd = ifelse(diseases == "Foot and mouth disease virus (Inf. with) ", TRUE, FALSE))
table(wahid.eval$fmd, wahid.eval$species)
prop.table(table(wahid.eval$fmd, wahid.eval$species))
prop.table(table(wahid.eval$fmd, wahid.eval$species))*100
prop.table(table(wahid.eval$fmd, wahid.eval$species), margin = 2)*100
prop.table(table(wahid.eval$fmd, wahid.eval$species), margin = 1)*100

