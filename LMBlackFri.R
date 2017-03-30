
## setting working directory
path <-  "C:\\Users\\ankit.talele\\Desktop\\Black Friday"
setwd(path)

## loading libraries
library(dummies)
library(plyr)
library(xgboost)

## loading data
train <- read.csv("./train.csv", stringsAsFactors=F)
test <- read.csv("./test.csv", stringsAsFactors=F)


## cleaning data

# removing categories 19 and 20
X_train <- subset(train, !Product_Category_1 %in% c(19,20))
X_test <- test
str(X_train)
str(X_test)
# onehot-encoding city variable
X_train <- dummy.data.frame(X_train, names=c("City_Category"), sep="_")
X_test <- dummy.data.frame(X_test, names=c("City_Category"), sep="_")

# converting age variable to numeric
X_train$Age[X_train$Age == "0-17"] <- "15"
X_train$Age[X_train$Age == "18-25"] <- "21"
X_train$Age[X_train$Age == "26-35"] <- "30"
X_train$Age[X_train$Age == "36-45"] <- "40"
X_train$Age[X_train$Age == "46-50"] <- "48"
X_train$Age[X_train$Age == "51-55"] <- "53"
X_train$Age[X_train$Age == "55+"] <- "60"

X_test$Age[X_test$Age == "0-17"] <- "15"
X_test$Age[X_test$Age == "18-25"] <- "21"
X_test$Age[X_test$Age == "26-35"] <- "30"
X_test$Age[X_test$Age == "36-45"] <- "40"
X_test$Age[X_test$Age == "46-50"] <- "48"
X_test$Age[X_test$Age == "51-55"] <- "53"
X_test$Age[X_test$Age == "55+"] <- "60"

X_train$Age <- as.integer(X_train$Age)
X_test$Age <- as.integer(X_test$Age)

# converting stay in current city to numeric
X_train$Stay_In_Current_City_Years[X_train$Stay_In_Current_City_Years == "4+"] <- "4"
X_test$Stay_In_Current_City_Years[X_test$Stay_In_Current_City_Years == "4+"] <- "4"

X_train$Stay_In_Current_City_Years <- as.integer(X_train$Stay_In_Current_City_Years)
X_test$Stay_In_Current_City_Years <- as.integer(X_test$Stay_In_Current_City_Years)

# converting gender to binary
X_train$Gender <- ifelse(X_train$Gender == "F", 1, 0)
X_test$Gender <- ifelse(X_test$Gender == "F", 1, 0)

# feature representing the count of each user
user_count <- ddply(X_train, .(User_ID), nrow)
names(user_count)[2] <- "User_Count"
X_train <- merge(X_train, user_count, by="User_ID")
X_test <- merge(X_test, user_count, all.x=T, by="User_ID")
#View(X_test)
#View(X_train)
# feature representing the count of each product
product_count <- ddply(X_train, .(Product_ID), nrow)
names(product_count)[2] <- "Product_Count"
X_train <- merge(X_train, product_count, by="Product_ID")
X_test <- merge(X_test, product_count, all.x=T, by="Product_ID")
X_test$Product_Count[is.na(X_test$Product_Count)] <- 0

# feature representing the average Purchase of each product
product_mean <- ddply(X_train, .(Product_ID), summarize, Product_Mean=mean(Purchase))
X_train <- merge(X_train, product_mean, by="Product_ID")
X_test <- merge(X_test, product_mean, all.x=T, by="Product_ID")
X_test$Product_Mean[is.na(X_test$Product_Mean)] <- mean(X_train$Purchase)

# feature representing the proportion of times the user purchases the product more than the product's average
X_train$flag_high <- ifelse(X_train$Purchase > X_train$Product_Mean,1,0)
user_high <- ddply(X_train, .(User_ID), summarize, User_High=mean(flag_high))
X_train <- merge(X_train, user_high, by="User_ID")
X_test <- merge(X_test, user_high, by="User_ID")
#####################################################################################################################################
library(dplyr)
sam <- subset(X_test, select = c(User_ID,Product_Category_1,Product_Category_2,Product_Category_3))
X_test <- subset(X_test, select = -c(Product_Category_1,Product_Category_2,Product_Category_3))
sam[["Product_Category_2"]][is.na(sam[["Product_Category_2"]])] <- 0
sam[["Product_Category_3"]][is.na(sam[["Product_Category_3"]])] <- 0
#View(sam)
str(sam)
as <-sam %>%
  group_by(User_ID) %>%
  group_by(Product_Category_1) %>%
  mutate(Product_Category_2 = ifelse(Product_Category_2 == 0,round(mean(Product_Category_2),digits = 0),Product_Category_2),
         Product_Category_3 = ifelse(Product_Category_3 == 0,round(mean(Product_Category_3),digits = 0),Product_Category_3))
sapply(as, function(y) sum(length(which(is.na(y)))))
as<- as.data.frame(as)
colnames(X_test)
colnames(as)
class(as)
class(X_test)
X_test <- (cbind(as,X_test))
#X_test <- as.data.frame(X_test)
#X_teste <- merge(x= X_test, y= as, by = "User_ID", all.x = T)
View(X_test)
head(X_test)
sapply(as, function(y) sum(length(which(is.na(y)))))

sam <- subset(X_train, select = c(User_ID,Product_Category_1,Product_Category_2,Product_Category_3))
X_train <- subset(X_train, select = -c(Product_Category_1,Product_Category_2,Product_Category_3))
sam[["Product_Category_2"]][is.na(sam[["Product_Category_2"]])] <- 0
sam[["Product_Category_3"]][is.na(sam[["Product_Category_3"]])] <- 0
#View(sam)
str(sam)
as <-sam %>%
  group_by(User_ID) %>%
  group_by(Product_Category_1) %>%
  mutate(Product_Category_2 = ifelse(Product_Category_2 == 0,round(mean(Product_Category_2),digits = 0),Product_Category_2),
         Product_Category_3 = ifelse(Product_Category_3 == 0,round(mean(Product_Category_3),digits = 0),Product_Category_3))
sapply(as, function(y) sum(length(which(is.na(y)))))
colnames(X_train)
colnames(as)
as<- as.data.frame(as)
class(as)
class(X_train)
X_train <- cbind(as,X_train)
View(X_train)

# subsetting columns for submission
submit <- X_test[,c("User_ID","Product_ID")]

# target variable
y <- X_train$Purchase
names(X_train)
train4lm <- subset(X_train, select =  -c(Product_ID,flag_high,User_ID))
train4lm <- train4lm[, -5]
train4lm <- train4lm[, -10]
colnames(train4lm)


lm <- lm(Purchase ~. , data = train4lm)
summary(lm)
pred <- predict(lm, X_test, interval = "predict",na.action=na.pass)
submissionlm <- cbind(submit,pred) 
submissionlm <- submissionlm[,-c(4,5)]
colnames(submissionlm)[3] <- "Purchase"
View(submissionlm)
write.csv(submissionlm, "lmsubmission.csv",row.names = F)

