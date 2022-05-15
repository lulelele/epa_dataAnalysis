# Demo 1.1 ####
getwd()
rm(list=ls()) 
#Working directory is where a project is based

# 1.2 Introduction to R ####
# 1.2.1 R as a calculator ####
8*4 # use Cntrl-Enter to run a line or selected piece of code
(8*4)^2
x = (8*4)^2
x
print(x)

# 1.2.2 Assigning values and datatypes in R ####
# 1.2.2.1 Vectors ####
quantity <- c(12,43,64,87,98,12,23,67,12,34,21) # c=concatenate into a vector; use = or <- (ALT -) to assign in R
quantity
class(quantity)
sum(quantity)
mean(quantity)
sd(quantity)
summary(quantity)

# Example: Diagnostic test results for 3 animals on multiple days
# Data are Ct values for a test that is performed on these animals
test1 <- c(23,40,19)
(test2 <- c(29,40,26)) #notice putting code in brackets prints it out after

# you can name these vectors - lets make a vector of names and assign them
animalnames = c("Animal 1", "Animal 2", "Animal 3")
# assign names to test1 vector
names(test1) = animalnames
names(test2) = animalnames
test1
test2
summary(test1) # it is still a numeric vector
boxplot(c(test1,test2))
summary(c(test1,test2))

#summing vectors 
#by themselves 
sum(test1)
sum(test2)
#but in combination
test1+test2 # you'll see that in this case the vectors are added 'vertically'
# this differs to 
sum(test1) + sum(test2)
# in this case mean may be more useful - in reality we wouldn't really be doing it this way but just for example
(test1+test2)/2

# what happens when these vectors are not the same length
length(test1)
length(test2)
length(test1) == length(test2) # compare using double ==
#add a new value to test1
(test1 = c(test1, 64))
#the names are dropped
test1 + test2
#now that we've added another result to test 1, R does not allow for summing
#part of the advantage of R, internal validation at this level
length(test1) == length(test2) 

#characters can be added to a vector as well 
y<-c("a", "b", "c", "d") 
class(y)
(y<-c("a", "b", "c", "d", 1))
# can't mix these vectors - numeric and character variables cannot mix in a vector (see Lists later)
summary(y)

#deleting a environmental Values/datasets
rm(y)
# or to remove all
rm(list=ls()) 

#selecting components of vectors
test1 <- c(23,40,19)
test2 <- c(29,40,26)
animalnames = c("Animal 1", "Animal 2", "Animal 3")
names(test1) = animalnames
names(test2) = animalnames

#animal 2
test1[3]
test2[3]
test1[c(1,2)]

#or
test1["Animal 2"] # advantage of using Names

# selecting components by comparison
test1>23 # establish all test one values above 23
sum(test1>23) # nice trick in R to count - TRUE = 1; FALSE = 0
filterCt <- test1>23 # use this as a filter
test1[filterCt]

# 1.2.2.2 Matrixes ####
alltest <- c(test1,test2)
View(alltest)
(alltest.matrix <- matrix(alltest, nrow = 2, byrow = TRUE))
#adding names 
rownames(alltest.matrix) <- c("test1", "test2")
colnames(alltest.matrix) <- animalnames
alltest.matrix

#summing in rows
rowSums(alltest.matrix)
rowMeans(alltest.matrix)
totalCt = rowSums(alltest.matrix)

#bind matrixes
cbind(alltest.matrix, totalCt) #cbind - Column bind

#adding new items
test3 <- c(33,40,28)
alltest.matrix = rbind(alltest.matrix,test3) #rbind

#Column actions
colMeans(alltest.matrix)

#selection of items - remember row,column
alltest.matrix
#result from test3 for animal 1
alltest.matrix[3,1]
alltest.matrix["test3","Animal 1"] #same outcome
# Animal 3's results
alltest.matrix[,3] # or
alltest.matrix[,"Animal 3"]

#Arithmetic works for matrix items
#often serology comes as a titre - we can convert that to a logged value for instance
test1<-c(1024,512,8)
test2<-c(512,32,64)
hi.matrix<-matrix(c(test1,test2), nrow = 2, byrow = TRUE)
rownames(hi.matrix) <- c("test1", "test2")
colnames(hi.matrix) <- animalnames
hi.matrix
(hi.matrix.transformed = log2(hi.matrix)) #log2 is an in built function - we will create our own as well

#1.2.2.3 Factors ####
#categorical variables common in our field
(breed = c("Tswana", "Tswana", "Africander", "Tuli", "Tuli", "Tswana"))
factor_breed = factor(breed)
factor_breed

#ordered factors
heat_tol <-c("med", "med", "high", "low","low", "med")
(factor_heat_tol <- factor(heat_tol, order = TRUE, levels = c("low", "med", "high")))

#levels can be changed but the order is NB to get right
#say you want to change med to medium etc
levels(factor_heat_tol)
levels(factor_heat_tol) <- c("low","medium","high")

# using factors assists in summary of data
summary(factor_breed) #compare this to summary(breed)
summary(factor_heat_tol)

#is cow 1 more heat tolerant than cow 5?
factor_heat_tol[1] > factor_heat_tol[5]

# we will be dealing with many factor management later in the course

#1.2.2.4 Dataframe ####
# very commonly used as it is a good stepping stone from software such as MS Excel
# lets take a step back and import some data into R - realistically you wont be making your own

#1.2.2.4.1 - accessing data in R - option 1 - read directly from CSV online ####
wahid_species <- read.csv("https://epicpd.jshiny.com/jdata/epicpd/botswanaVS/coursematerial/tabular/wahid_botswana_specieslevel.csv")
head(wahid_species)
View(wahid_species)
class(wahid_species)
str(wahid_species) #structure
summary(wahid_species)

#continue our exploration of data frames
# extracting rows and columns as above
#6th row
wahid_species[6,]
(totalCases <- wahid_species[,"totalNcase"]) #this turns this specific dataset into a vector
#except now we can use the $ sign to indicate column names
wahid_species$totalNcase

#using a TRUE FALSE vector - let's find all outbreak ID's where cattle are involved
(cattleOutbreaks = wahid_species[wahid_species$spicieName == 'Cattle', "oieReference"])
#or
subset(wahid_species, subset = spicieName == 'Cattle')
subset(wahid_species, subset = spicieName == 'Cattle')$oieReference 
wahid_species_df = ?
#1.2.2.5 Lists ####
#Very efficient way of storing and manipulating data but does sometimes take a bit of patience to work with

mylist = list(wahid_species_df = wahid_species,
          factor_heat_tol_factor = factor_heat_tol,
          hi.matrix.transformed.matrix = hi.matrix.transformed)

mylist$wahid_species_df
mylist$factor_heat_tol_factor
names(mylist)
names(mylist) = c("species","heat","tests")
mylist$tests
class(mylist$tests)

#selecting elements from lists
mylist[[2]]
#double brackets selects from list - single from a vector
mylist[[2]][1]

#by name - 
head(mylist[["species"]])
head(mylist$species)

#1.3.- A brief introduction to functions ####
source("https://epicpd.jshiny.com/jdata/epicpd/botswanaVS/coursematerial/functions/01_functions.R")

getmode #get the mode of a dataset - no built in function for that
getmode(mylist$tests)

celciusTofahrenheit <- function(x){
  (x*9/5)+32
}
temps = c(23,32,45)
(tempsF = celciusTofahrenheit(temps))

#