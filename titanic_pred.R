library(caret)
library(Hmisc)

readData <- function(path.name, file.name, column.types, missing.types) {
        read.csv( paste(path.name, file.name, sep=""), 
                  colClasses=column.types,
                  na.strings=missing.types )
}

getTitle <- function(data) {
        title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
        title.comma.end <- title.dot.start + attr(title.dot.start, "match.length")-1
        data$Title <- substr(data$Name, title.dot.start+2, title.comma.end-1)
        return (data$Title)
}   

imputeMedian <- function(impute.var, filter.var, var.levels) {
        for (v in var.levels) {
                impute.var[ which( filter.var == v)] <- impute(impute.var[ 
                        which( filter.var == v)])
        }
        return (impute.var)
}

changeTitles <- function(data, old.titles, new.title) {
        for (honorific in old.titles) {
                data$Title[ which( data$Title == honorific)] <- new.title
        }
        return (data$Title)
}

ProbSurv <- function(model, newdata) {
        predict.glm(model, newdata, type="response")
}

# read in data

Titanic.path <- "/Users/hsuel/Documents/Coursera/JHU_Data_Science/DataProducts/shiny-app/"
train.data.file <- "train.csv"
missing.types <- c("NA", "")
train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)

train.raw <- readData(Titanic.path, train.data.file, 
                      train.column.types, missing.types)
df.train <- train.raw

# impute missing Age data using median of Age by title
df.train$Title <- getTitle(df.train)
df.train$Title <- changeTitles(df.train, 
                               c("Capt", "Col", "Don", "Dr", 
                                 "Jonkheer", "Lady", "Major", 
                                 "Rev", "Sir"),
                               "Noble")
df.train$Title <- changeTitles(df.train, c("the Countess", "Ms"), 
                               "Mrs")
df.train$Title <- changeTitles(df.train, c("Mlle", "Mme"), "Miss")
df.train$Title <- as.factor(df.train$Title)
titles.na.train <- c("Dr", "Master", "Mrs", "Miss", "Mr")
df.train$Age <- imputeMedian(df.train$Age, df.train$Title, 
                             titles.na.train)

# fill in missing embarkation data using most common port
df.train$Embarked[which(is.na(df.train$Embarked))] <- 'S'

# impute missing fare data using median of class
df.train$Fare[ which( df.train$Fare == 0 )] <- NA
df.train$Fare <- imputeMedian(df.train$Fare, df.train$Pclass, 
                              as.numeric(levels(df.train$Pclass)))

# basic logistic regression model; model give scores of .76077
Titanic.logit.1 <- glm(Survived ~ Sex + Age + Pclass + Embarked, data = df.train, family=binomial(logit))