adult <- read.csv("adult_sal.csv")

head(adult)

library(dplyr)

adult <- select(adult, -X)

head(adult)

str(adult)

summary(adult)

table(adult$type_employer)

# Data cleaning

# Combine employer type

unemp <- function(job){
  job <- as.character(job)
  if (job == "Never-worked" | job == "Without-pay"){
    return("Unemployed")
  }else{
    return(job)
  }
}

# Apply

adult$type_employer <- sapply(adult$type_employer, unemp)

table(adult$type_employer)

# Group self-employed and State/Local Gov

group_emp <- function(job){
  job <- as.character(job)
  if (job == "Local-gov" | job == "State-gov"){
    return("SL-gov")
  } else if(job == "Self-emp-inc" | job == "Self-emp-not-inc"){
    return("self-emp")
  }else{
    return(job)
  }
}

# apply group_emp function
adult$type_employer <- sapply(adult$type_employer, group_emp)
# Check
table(adult$type_employer)

# Marital Status 
table(adult$marital)

# Change to married, not married, never married

group_marital <- function(mar){
  mar <- as.character(mar)
  if (mar == "Separated" | mar == "Divorced" | mar == "Widowed"){
    return("Not-Married")
  }else if(mar == "Never-married"){
    return(mar)
  }else{
    return("Married")
  }
}

# Now apply that to marital

adult$marital <- sapply(adult$marital, group_marital)

# check

table(adult$marital)

# Only 3 categories, wunderbar

# Country col

table(adult$country)

# Need to group countries, continent seems appropriate.

levels(adult$country)

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if(ctry %in% Asia){
    return("Asia")
  }else if(ctry %in% North.America){
    return("North.America")
  }else if(ctry %in% Europe){
    return("Europe")
  }else if(ctry %in% Latin.and.South.America){
    return("Latin.and.South.America")
  }else{
    return("Other")
  }
}

# apply that to country col
adult$country <- sapply(adult$country, group_country)

# check 
table(adult$country)

# Look at structure again
str(adult)

# change to factors
adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)

# Check structure again for factor
str(adult)

# Dealing with Missing Data

library(Amelia)

adult[adult == "?"] <- NA

table(adult$type_employer)

# Factor again to remove NA values

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$occupation <- sapply(adult$occupation,factor)


missmap(adult)

# Change up the missmap

missmap(adult, y.at=c(1), y.labels = c(""), col=c("yellow", "black"))

# drop more missing values

adult <- na.omit(adult)

# check missmap again

missmap(adult, y.at=c(1), y.labels = c(""), col=c("yellow", "black"))

# No Yellow, good to go

# Exploritory Data Analysis/Visualization

str(adult)

library(ggplot2)
library(dplyr)

ggplot(data = adult, aes(x = age))+
  geom_histogram(aes(fill=income), color = "black", binwidth = 1)+
  theme_bw()

ggplot(data = adult, aes(x = hr_per_week))+
  geom_histogram()+
  theme_bw()

names(adult)[names(adult)=="country"] <- "region"

str(adult)

head(adult)

ggplot(adult, aes(region))+
  geom_bar(aes(fill=income), color = "black")+
  theme_bw()+
  theme(axis.text.x= element_text(angle = 90, hjust = 1))

# Building the mdoel

library(caTools)
set.seed(101)

sample <- sample.split(adult$income, SplitRatio = 0.70)

train <- subset(adult, sample == T)

test <- subset(adult, sample == F)

# Explore documentation
?glm

model <- glm(income ~ ., family = binomial(logit), data = train)

summary(model)

# Probably need to reduce education/occupation factor levels

# Adding step function

new.step.model <- step(model)

# Output:

# Step:  AIC=14112.05
# income ~ age + type_employer + fnlwgt + education + education_num + 
#  marital + occupation + relationship + race + sex + capital_gain + 
#  capital_loss + hr_per_week + region

summary(new.step.model)

# creating confusion matrix

test$predicted.income <- predict(model, newdata = test, type = "response")

table(test$income, test$predicted.income > 0.5)

# Output:
#       FALSE TRUE
# <=50K  6372  548
# >50K    872 1423

# Model Accuracy

(6372+1423)/(6372+1423+548+872)

# > (6372+1423)/(6372+1423+548+872)
# [1] 0.8459034

# Other performance metrics

#recall
6732/(6372+548)
# [1] 0.9728324

# precision
6732/(6372+872)
# [1] 0.9293208

