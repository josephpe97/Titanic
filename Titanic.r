# Cleaning up the Workspace

rm(list = ls())

# Setting a directory

setwd('C:/Documentos/Treinamento R/Titanic')

# Packages

require(ggplot2)
require(dplyr)

# Importing data

data <- read.csv('titanic3.csv')

# Data Clean

data$PassengerId <- as.factor(data$PassengerId)
data$Survived <- as.factor(data$Survived)
data$Pclass <- as.factor(data$Pclass)

# Analysing the Data

## Analysing the number of Survives by Ticket Class

ggplot(data = data,
       aes(x = Pclass)) + geom_bar(aes(fill = Survived)) +
  theme_bw()+
  labs(title = 'Survived by Ticket Class',
       x = 'Ticket Class',
       y = 'Count') 
  
## One of the question is -> The rate of Survived of Woman is greater the Man?

ggplot(data = data,
       aes(x = Sex)) + geom_bar(aes(fill = Survived)) +
  theme_bw() +
  labs(title = 'Survived by Gender',
       x = 'Gender',
       y = 'Count')

## But, this is correct to every single Ticket Class?

ggplot(data = data,
       aes(x = Sex)) + geom_bar(aes(fill = Survived)) +
  theme_bw() +
  labs(title = 'Survived by Gender and Ticket Class',
       x = 'Gender',
       y = 'Count') +
  facet_grid(~ Pclass)

## What was the mean Age?

ggplot(data = data,
       aes(x = Age)) + geom_histogram(bins = 20, fill = 'blue') +
  theme_bw() +
  labs(title = "Dispersion of Passengers' Age ",
       x = 'Age',
       y = 'Count')

## Older people died in more proportion than youngers?

ggplot(data = data,
       aes(x = Age)) + geom_histogram(bins = 20, aes(fill = Survived)) +
  theme_bw() +
  labs(title = 'Survived by Age',
       x = 'Age',
       y = 'Count') +
  coord_flip()

## It was the Same to every Class?

ggplot(data = data,
       aes(x = Age)) + geom_histogram(bins = 20, aes(fill = Survived)) +
  theme_bw() +
  labs(title = 'Survived by Age and Ticket Class',
       x = 'Age',
       y = 'Count') +
  facet_grid(~ Pclass)

## What was the proportion of Kids in the Titanic?

### Creating an new variable called Kid - if the Age is lower than 12, then
### she or he is a kid, otherwise they're not.

data$Kid <- ifelse(data$Age <= 12,'Kid','Adult')

## Kids had a higher survived tax?

ggplot(data = subset(data, !is.na(data$Kid) == T),
       aes(x = Kid)) + geom_bar(aes(fill = Survived)) +
  theme_bw() +
  labs(title = 'Survived by being a Kid',
       x = '',
       y = 'Count')

## Kids had a higher survived tax if we consider the Ticket Class?

ggplot(data = subset(data, !is.na(data$Kid) == T),
       aes(x = Kid)) + geom_bar(aes(fill = Survived)) +
  theme_bw() +
  labs(title = 'Survived by being a Kid or Not - By Ticket Class',
       x = '',
       y = 'Count') +
  facet_grid(~ Pclass)

## Are there any relationship between the Ticket Class and the Embarked?

ggplot(data = subset(data, data$Embarked != ''),
       aes(x = Pclass)) + geom_bar(aes(fill = Embarked))+
  theme_bw() +
  labs(title = 'Ticket Class by Embarked City',
       x = 'Ticket Class',
       y = 'Count')

## Is there any relationship between the Survived rate and the Embarked?

ggplot(data = data,
       aes(x = Survived)) + geom_bar(aes(fill = Embarked)) +
  theme_bw() +
  labs(title = 'Survived by Embarked City',
       x = '',
       y = '')

## Passengers with siblings were more propense to die?

ggplot(data = data,
       aes(x = SibSp)) + geom_bar(aes(fill = Survived)) +
  theme_bw() +
  labs(title = 'Survived by Siblings Embarked',
       x = '',
       y = 'Count')

## It occurs in all class?

ggplot(data = data,
       aes(x = SibSp)) + geom_bar(aes(fill = Survived)) +
  theme_bw() +
  labs(title = 'Survived by Siblings Embarked',
       x = '',
       y = 'Count') +
  facet_grid(~ Pclass)

## What about the parents embarked?

ggplot(data = data,
       aes(x = Parch)) + geom_bar(aes(fill = Survived))
