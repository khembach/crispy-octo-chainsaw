# Import the training set: train
train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
train <- read.csv(train_url)

# Import the testing set: test
test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
test <- read.csv(test_url) 


library(rpart)
library(rattle)

my_tree <- rpart(Survived ~ Sex + Age, 
                 data = train.data, 
                 method ="class")
plot(my_tree)
text(my_tree)

test.data <- read.csv("~/Titanic/test.csv")
genderclassmodel <- read.csv("~/Titanic/genderclassmodel.csv")
gendermodel <- read.csv("~/Titanic/gendermodel.csv")
train.data <- read.csv("~/Titanic/train.csv")
fit <- glm(Survived ~ Pclass + Sex, data=train.data, family=binomial)
summary(fit)
fit2 <- glm(Survived ~ Pclass + Sex + Age, data=train.data, family=binomial)
summary(fit2)

sub.train.data <- train.data[1:3]
sub.train.data <- cbind(sub.train.data, train.data[5:8])
sub.train.data <- cbind(sub.train.data, Fare = train.data$Fare)
sub.train.data <- cbind(sub.train.data, Embarked = train.data$Embarked)
test.data.na$Age <- test.data$Age

# Fix the NA in the Age field of the test data
test.data.na$Age <- ifelse(is.na(test.data$Age), 
                           mean(train.data$Age, na.rm=TRUE), test.data.na$Age);

test.data.na[153,]$Fare <- median(train.data$Fare)

# 
train.data.na <- train.data
train.data.na$Age <- ifelse(is.na(train.data$Age), 
                           mean(train.data$Age, na.rm=TRUE), test.data.na$Age);
summary(train.data.na)
test.data.na[153,]$Fare <- median(train.data$Fare)

fit3 <- glm(Survived ~ ., data=sub.train.data, family=binomial)
summary(fit3)
fit3 <- update(fit3, .~.-PassengerId)
summary(fit3)
fit3 <- update(fit3, .~.-Embarked)
summary(fit3)
fit3 <- update(fit3, .~.-Parch)
summary(fit3)
fit3 <- update(fit3, .~.-Fare)
summary(fit3)
fit3 <- update(fit3, .~.-SibSp)
summary(fit3)
glm.titanic <- data.frame(PassengerId = test.data.na$PassengerId, 
                          Survived = round(predict(fit3, test.data.na, type="response")))
glm.titanic.test <- data.frame(PassengerId = train.data.na$PassengerId, 
                          Survived.pred = round(predict(fit3, train.data.na, type="response")),
                          Survived.actual = train.data.na$Survived)
table(glm.titanic.test$Survived.pred - glm.titanic.test$Survived.actual)

write.csv(round(predict(fit3, test.data.na, type="response")), file="glm_titanic.csv",
          row.names=FALSE) 


