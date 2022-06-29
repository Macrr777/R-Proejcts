library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(e1071)
library(randomForest)
library(rpart)
library(data.table)
install.packages("expss")
library(expss)
train <- fread("C:/Users/choud/Desktop/Titanic Machine learning from Disater/train.csv")
test <- fread("C:/Users/choud/Desktop/Titanic Machine learning from Disater/test.csv")

# Changing some data to factors for easier correlation creations
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)
test$Sex <- as.factor(test$Sex)
test$Embarked <- as.factor(test$Embarked)
print(cor(train$Pclass, train$Survived))
print(cor(as.numeric(train$Sex), train$Survived))
print(cor(as.numeric(train$Embarked), train$Survived))
graph <- train %>% dplyr::select(Survived, Sex, Pclass, Fare, Embarked, SibSp, Parch)
graph$Sex <- as.numeric(graph$Sex)
graph$Embarked <- as.numeric(graph$Embarked)
corrplot(cor(graph), type ='lower', method ='number', col='black', cl.pos='n')
rm(graph)
training <- as.data.frame(train) %>% dplyr::select(Survived, Sex, Pclass)
nb <- naiveBayes(as.factor(Survived)~., data=training) #This Creates our Model
nb_prediction <- predict(nb, test, type="class") #testing on our test set
#Creating a function to output submission
out <- function(prediction, originalSet) {
  output <- originalSet %>% dplyr::select(PassengerId)
  output <- mutate(output, Survived = prediction)
  write.csv(output, 'submission.csv', row.names=FALSE)
}
out(nb_prediction, test)
rm(training, nb_prediction, nb)
graph <- data.table(Pclass=integer(), Sex=factor(levels=c("female", "male"), labels=c("F", "M")), Survived=integer(), NotSurvived=integer())
genders <- as.factor(c("female", "male"))
classes <- c(1, 2, 3)
for(x in classes) {
  for(y in genders) {
    tempPos <- train %>% dplyr::select(Pclass, Sex, Survived) %>% dplyr::filter(Pclass==x) %>% dplyr::filter(Sex==y) %>% dplyr::filter(Survived==1)
    tempPos <- dplyr::summarise(tempPos, n=n())
    tempNeg <- train %>% dplyr::select(Pclass, Sex, Survived) %>% dplyr::filter(Pclass==x) %>% dplyr::filter(Sex==y) %>% dplyr::filter(Survived==0)
    tempNeg <- dplyr::summarise(tempNeg, n=n())
    graph <- add_row(graph, Pclass=x, Sex=y, Survived=as.numeric(tempPos), NotSurvived=as.numeric(tempNeg))
    rm(tempPos, tempNeg)
  }
}
graph <- graph %>% dplyr::mutate(psurvived = as.numeric(Survived/(Survived+NotSurvived)))
ggplot(data=graph, aes(x=Pclass, y=psurvived, fill=Sex)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_text(aes(label=round(psurvived, digits=2)), vjust=1.6, color="white", position = position_dodge(0.9), size=3.5)
rm(graph, genders, classes, x, y)
#Viewing survival rates by Age
graph <- data.table(Age=numeric(), Sex=factor(levels=c("female", "male"), labels=c("F", "M")), Survived=integer(), NotSurvived=integer())
genders <- as.factor(c("female", "male"))
ages <- train %>% dplyr::select(Age) %>% dplyr::distinct() %>% tidyr::drop_na()
for(x in ages$Age) {
  for(y in genders) {
    tempPos <- train %>% dplyr::select(Age, Sex, Survived) %>% dplyr::filter(Age==x) %>% dplyr::filter(Sex==y) %>% dplyr::filter(Survived==1)
    tempPos <- dplyr::summarise(tempPos, n=n())
    tempNeg <- train %>% dplyr::select(Age, Sex, Survived) %>% dplyr::filter(Age==x) %>% dplyr::filter(Sex==y) %>% dplyr::filter(Survived==0)
    tempNeg <- dplyr::summarise(tempNeg, n=n())
    graph <- add_row(graph, Age=x, Sex=y, Survived=as.numeric(tempPos), NotSurvived=as.numeric(tempNeg))
    rm(tempPos, tempNeg)
  }
}
graph <- graph %>% dplyr::mutate(psurvived = as.numeric(Survived/(Survived+NotSurvived)))
ggplot(data=train, aes(Age)) +
  geom_density(alpha=0.3, aes(fill=factor(Survived)))
rm(graph, genders, ages, x ,y)
testing <- train
testing$Title <- sub(' ', '', sapply(train$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]}))
table(testing$Title)
#lets show the test set too just incase it has titles not here
temp <- test
temp$Title <- sub(' ', '', sapply(test$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]}))
table(temp$Title)
rm(temp)
testing$Title[testing$Title %in% c('Mme', 'Mlle')] <- 'Mme' #French Fem
#testing$Title[testing$Title %in% c('Mme', 'Miss')] <- 'Miss' #Unmarried Fem
#testing$Title[testing$Title %in% c('Mlle', 'Mrs')] <- 'Mrs' #Married Fem
testing$Title[testing$Title %in% c('Lady', 'Jonkheer', 'the Countess', 'Dona')] <- 'Lady' #Noble Fem
testing$Title[testing$Title %in% c('Sir', 'Don', 'Capt', 'Col', 'Major')] <- 'Sir' #Noble/Military Male
table(testing$Title)
testing <- testing %>% dplyr::mutate(Minor = (Age<=16))
testing$Minor <- testing$Minor %>% tidyr::replace_na(FALSE)
testing <- testing %>% dplyr::mutate(LowerClassWoman = (Pclass!=3 & Sex=='female'))
testing$Title <- as.factor(testing$Title)
graph <- testing %>% dplyr::select(Survived, Sex, Pclass, Minor, LowerClassWoman, Title)
graph$Sex <- as.numeric(graph$Sex)
graph$Title <- as.numeric(graph$Title)
corrplot(cor(graph), type ='lower', method ='number', col='black', cl.pos='n')
rm(graph)
training <- as.data.frame(testing) %>% dplyr::select(Survived, Sex, LowerClassWoman)
nb <- naiveBayes(as.factor(Survived)~., data=training) #This Creates our Model
nbPrediction <- predict(nb, test, type="class") #testing on our test set
#Creating a function to output submission
out <- function(prediction, originalSet) {
  output <- originalSet %>% dplyr::select(PassengerId)
  output <- mutate(output, Survived = prediction)
  write.csv(output, 'submission.csv', row.names=FALSE)
}
testing <- test %>% dplyr::mutate(LowerClassWoman = (Pclass!=3 & Sex=='female'))
out(nbPrediction, testing)
rm(nbPrediction, nb)
training <- train
training <- train %>% dplyr::mutate(LowerClassWoman = (Pclass!=3 & Sex=='female'))
training <- training %>% dplyr::mutate(Minor = (Age<=16))
training$Minor <- training$Minor %>% tidyr::replace_na(FALSE)
training$Title <- sub(' ', '', sapply(train$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]}))
training$Title[training$Title %in% c('Mme', 'Mlle')] <- 'Mme' #French Fem
training$Title[training$Title %in% c('Lady', 'Jonkheer', 'the Countess', 'Dona')] <- 'Lady' #Noble Fem
training$Title[training$Title %in% c('Sir', 'Don', 'Capt', 'Col', 'Major')] <- 'Sir' #Noble/Military Male
training$Title <- as.factor(training$Title)

testing <- testing %>% dplyr::mutate(Minor = (Age<=16))
testing$Minor <- testing$Minor %>% tidyr::replace_na(FALSE)
testing$Title <- sub(' ', '', sapply(test$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]}))
testing$Title[testing$Title %in% c('Mme', 'Mlle')] <- 'Mme' #French Fem
testing$Title[testing$Title %in% c('Lady', 'Jonkheer', 'the Countess', 'Dona')] <- 'Lady' #Noble Fem
testing$Title[testing$Title %in% c('Sir', 'Don', 'Capt', 'Col', 'Major')] <- 'Sir' #Noble/Military Male
testing$Title <- as.factor(testing$Title)

set.seed(777)

rf <- randomForest(as.factor(Survived) ~ Sex + Pclass + Title + LowerClassWoman + Embarked,
                   data=training, importance=TRUE, ntree=2000)
varImpPlot(rf)
#ensuring all factor levels are the same
levels(testing$Embarked) <- levels(training$Embarked)
levels(testing$Title) <- levels(training$Title)

rfPrediction <- predict(rf, testing)
out(rfPrediction, testing)
rm(rf, rfPrediction)
Name <- c("Pclass", "Name", "Sex", "Age", "SibSp", "Parch", "Ticket", "Fare", "Cabin", "Embarked")
NACount <- c(sum(is.na(train$Pclass)), sum(is.na(train$Name)), sum(is.na(train$Sex)), sum(is.na(train$Age)), sum(is.na(train$SibSp)), sum(is.na(train$Parch)), count_if("", train$Ticket), sum(is.na(train$Fare)), count_if("", train$Cabin), count_if("", train$Embarked))
temp <- cbind(Name, NACount)
htmlTable(temp)
rm(temp)
dtAge <- rpart(Age ~ Pclass + SibSp + Parch + Fare + Embarked + Title, 
               data=training[!is.na(training$Age),], method="anova")
training$Age[is.na(training$Age)] <- predict(dtAge, training[is.na(training$Age),])
testing$Age[is.na(testing$Age)] <- predict(dtAge, testing[is.na(testing$Age),])

training$Embarked[is.na(training$Embarked)] <- 'S'

testing$Fare[is.na(testing$Fare)] <- median(training$Fare)

rm(dtAge)
rf <- randomForest(as.factor(Survived) ~ Sex + Pclass + Age + SibSp + Parch + Fare + Embarked,
                   data=training, importance=TRUE, ntree=3000)
varImpPlot(rf)
rfPredict <- predict(rf, testing)
out(rfPredict, testing)

rm(rf, rfPredict)

