#Welcome to the R tutorial
##This is called a comment
###Everything after a '#' is not read by the machine
####Great for keeping notes on what you are doing
#The machine can't read this



###############################################################################

#Basic syntax
#Simple operations
2 * 2
#What value should be returned in the console?

10 / 2
#What value should be returned in the console?

#These values are not stored
#Can store values as 'objects' or 'variables

#Stores 120 in the variable weight_kg
weight_kg <- 120

#weight_kg will always return 120
weight_kg


#we can update weight_kg by giving it a new value
weight_kg <- 90
weight_kg


#You can do operations on variables
#Divide weight_kg by 9 and store it as a new variable
var_9 <- weight_kg / 9  #weight_kg divided 9

var_9



###############################################################################
#Functions
#Functions are pre-written scripts that can do complicated tasks easily
#Functions normally take one or more arguments and ovten return a value

weight_kg <- sqrt(10)
weight_kg

#I only want to return 2 significant figures
#Can use another function 'round()'
round(weight_kg)

#Didn't return what I wanted
#Let's look at the arguments it takes
args(round)

#So it takes 'x' which is a value, and digits which indicates the number of
#significant figures
round(weight_kg, digits = 2)
var_round <- round(weight_kg, digits = 2)

###############################################################################
#Vectors are a data type
#Can hold a series of values that are either numbers or strings

ages <- c(50, 60, 65, 82)
ages

barn <- c("cow", "pig", "chicken", "sheep")
barn

facts <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
facts

#Length tells how many elements are in a vector
length(ages)
length(barn)
length(facts)

#Vectors contain the same type of data
#class indicates what kind of object you are working with
class(ages)
class(barn)
class(facts)

#You can add values by using the c() function
ages <- c(ages, 90) #adds it to the end of the vector
ages <- c(30, ages) #adds it to the beginning of the vector
ages

##Subsetting vectors
#barn had these elements
#"cow" "pig" "chicken" "sheep"

#Returns "cow"
barn[1]
barn[2]

#What will following return? Uncomment for the answer
barn[3:2]


#We can also subset on specific conditions
#Lets subset so we only get ages above 50


ages > 50
#This only returned the logical values
#We can get the actual ages by subseting like above with barn
ages[ages > 50]

ages[ages >= 50 & ages <= 65]

#We can further subset with other values
ages[ages > 50 | ages == 30]

#Add the following values to ages: 25, 45, 91, 76
#Then subset ages so only values <= 30 and > 50 are returned
ages <- c(ages, 25, 45, 91, 76)
ages[ages > 50]

###############################################################################
#Reading in a dataframe
#Can assign a dataframe to an object
#Make sure to be in the correct working directory
getwd()


elisa <- read.csv('good_data_ELISA.csv')

#Let's explore this dataframe and get a good feel for it
#head gives the first x rows and columns of the dataframe
head(elisa)
head(elisa, 10)

#str or structure gives what type of data is each column
str(elisa)

#Finally view will open the dataframe in a seperate tab in R Studio
view(elisa)

#There is multiple ways to index and subset dataframes
#Here we will use the libraries dplyr and tidyr
install.packages('dplyr') #install dplyr
install.packages('tidyr') #install tidyr

#Now we load in the libraries
library(dplyr)
library(tidyr)

#Select can be used to select specific columns
#Let's just select the first two columns

select(elisa, Treatment, Concentration)

#Adding '-' in front excludes the variable
select(elisa, -Day, -Month, -Year)

#The filter function selects rows
filter(elisa, Day == 15)

#We can assign the selected and filtered to new variables
elisa2 <- filter(elisa, Concentration >= 1.0)
elisa_sml <- select(elisa2, Treatment, Concentration)
view(elisa_sml)

#As you can see, this is bulky and not easily readable
#We can use 'pipes' to simplify this

elisa %>% 
  filter(Concentration >= 1.0) %>% 
  select(Treatment, Concentration)

#We can also change columns by mutating them
#I won't cover this today, but feel free to ask me or 
#look this up in the future

############################################################
#Test Case
#The elisa dataframe contains three separate repeated experiments
#Subset the dataframe so that each repeat is it's own object 
#with only Treatment and Concentration


#Repeat 1
elisa_r1 = elisa %>% 
  filter(Day == 15) %>% 
  select(Treatment, Concentration)

view(elisa_r1)

#Simple boxplot
library(ggplot2)  #have to call ggplot
ggplot(data = elisa_r1, mapping = aes(x = Treatment, y = Concentration)) + 
  geom_boxplot() +
  geom_jitter(alpha = 0.3, color = "purple")

#We can pipe this too
elisa_r1 %>% 
  ggplot(mapping = aes(x = Treatment, y = Concentration)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, color = "tomato")

#To get a bar chart with error bars takes some more manipulation

#Let's make an object that contains the summary stats
#We can use piping to use the functions group_by and summarise
elisa_r1_sum <- elisa_r1 %>% 
  group_by(Treatment) %>%  #Grouping by treatment group
  summarise(mean_Conc = mean(Concentration), #calculates mean by group
            sd_Conc = sd(Concentration),  #calculates SD by group
            n_Conc = n(), #returns sample size per group
            SE_Conc = sd(Concentration)/sqrt(n())) #calculates SE by group

#From the summary statistics, we can now make a bar chart w/ error bars
#Using ggplot
elisa_r1_sum %>% 
  ggplot(aes(x = Treatment, y = mean_Conc)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_Conc - sd_Conc, ymax = mean_Conc + sd_Conc),
                width=0.2)


###################################################
#One-Way Anova example

#From our dataset we need to get the different treatment groups
#Can use the 'levels' function
levels(elisa_r1)

#We need to switch this from character to factor
library(dplyr)

#Let's get the summary statistics of the dataset
group_by(elisa_r1, Treatment) %>% 
  summarise(
    count = n(),
    mean = mean(Concentration),
    sd = sd(Concentration)
  )

#Save One-Way Anova as an object res.aov
res.aov <- aov(Concentration ~ Treatment, data = elisa_r1)

#Print the summary of the AOV object
summary(res.aov)

#TukeyHSD multiple pairwise-comparisons 
#This is a function native to R
TukeyHSD(res.aov) #Prints to the console

#Checking homogeneity of variances
plot(res.aov, 1)

#Levene's test is less sensitive from departures of the normal distribution
install.packages('car')
library(car)
leveneTest(Concentration ~ Treatment, data = elisa_r1)
