# 1.4 Apply functions in R ####
# apply a function to a set of cases, either column or rows and by group or not
# alternative to using for loops which are slow on bigger data sets

(c = seq(1,10,1))

for (i in c){
  print(i)
}
#versus
sapply(c, print)

rm(list=ls()) 


# 1.4.1 Import datasets to use ####
# don't worry about the next few lines of code , we will go through them in the next session
wahid_outbreaks <- read.csv("https://epicpd.jshiny.com/jdata/epicpd/botswanaVS/coursematerial/tabular/wahid_botswana_outbreaklevel.csv")
wahid_species <- read.csv("https://epicpd.jshiny.com/jdata/epicpd/botswanaVS/coursematerial/tabular/wahid_botswana_specieslevel.csv")
str(wahid_outbreaks)
str(wahid_species)
wahid.eval <- merge(wahid_outbreaks, wahid_species) #oieReference in both datasets
str(wahid.eval)
# clean up species a little
wahid.eval$spicieName = factor(wahid.eval$spicieName)
levels(wahid.eval$spicieName)
summary(wahid.eval$spicieName)
levels(wahid.eval$spicieName) = 
c(
"Elephant"                                     
,"Pike"                                            
,"African catfish"                          
,"Banded tilapia"                                      
,"Birds"                                                                                              
,"Cattle"                                                                                             
,"Clarias ngamensis"                              
,"Dashtail barb"                                          
,"Equidae"                                                                                            
,"Collared-Dove"                            
,"Farmed fish"
,"Goats"                                                                                              
,"Kudu"                                       
,"Impala"                                                   
,"Mochokidae-Wild fish"                
,"Redbreast tilapia"                                     
,"Ring-necked Dove"                                  
,"Rock Pigeon"                                   
,"Sheep"                                                                                              
,"Sheep/goats"                                                                           
,"Silver catfish"                                   
,"Swine")

summary(wahid.eval$spicieName)
wahid.eval.df = wahid.eval[c("diseases", "animalType", "spicieName", "totalSusceptible", "totalNcase" )]
str(wahid.eval.df)

# 1.4.2 Using apply functions - basic ####
apply(wahid.eval.df["totalSusceptible"], 2, sum) # use of 2 in this case refers to column, rather than row

# 3 arguments - a data frame or matrix, the application to a column (2) or row (1), and the function associated
apply(wahid.eval.df[c(4,5)], 2, sum)
apply(wahid.eval.df[c(4,5)], 2, mean)
apply(wahid.eval.df[c(4,5)], 2, median)

# 1.4.3 Using tapply ####
?tapply
#grouped by functionality
#example - mean number of cases where species is involved
tapply(wahid.eval.df$totalNcase, wahid.eval.df$spicieName, mean)
#consider using na.rm = TRUE
tapply(wahid.eval.df$totalNcase, wahid.eval.df$spicieName, mean, na.rm=TRUE)
# makes no difference here but if there was a NULL value it would be helpful - consider
wahid.eval.df.temp = wahid.eval.df
wahid.eval.df.temp[1,5] <-  NA
tapply(wahid.eval.df.temp$totalNcase, wahid.eval.df.temp$spicieName, mean) #take a look at the Cattle outcome
(temp = tapply(wahid.eval.df.temp$totalNcase, wahid.eval.df.temp$spicieName, mean, na.rm=TRUE))
temp = as.data.frame.table(temp)
colnames(temp) = c("Species","mean")
temp

#aggregate - when multiple variables to compute function against
aggregate(wahid.eval.df[, c("totalSusceptible", "totalNcase")], list(wahid.eval.df$spicieName), mean, na.rm=TRUE)
# or maybe more clearly
aggregate(cbind(totalSusceptible,totalNcase) ~ spicieName, data = wahid.eval.df, FUN = mean, na.rm = TRUE)
# to have multiple inputs
aggregate(cbind(totalSusceptible,totalNcase) ~ spicieName + diseases, data = wahid.eval.df, FUN = mean, na.rm = TRUE)

#lapply and sapply iterate over lists - lapply returns a list where sapply tries to return a vector or matrix

