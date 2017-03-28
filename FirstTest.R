path <-  "C:\\Users\\ankit.talele\\Desktop\\Black Friday"
setwd(path)
train <- read.csv("train.csv",header = TRUE)
test <- read.csv("test.csv", header = TRUE)
dim(train)
dim(test)
library(DataExplorer)
#GenerateReport(train)
train[,is.na(Product_Category_3)]
sum(is.na(train$Product_Category_3))
sapply(train, function(y) sum(is.na(y)))
sapply(test, function(y) sum(is.na(y)))
unique(train$Product_Category_2)
unique(train$Product_Category_3)
train$label <- 1
test$label <- 0
head(test)
primary <- train[,c(1,12)]
train <- train[,-12]
merged_data <- rbind(train,test)
dim(merged_data)
sapply(merged_data, function(y) sum(is.na(y)))
#sapply(merged_data, function(y) median(y,na.rm = T))
median(merged_data$Product_Category_1, na.rm = T)
f=function(x){
  x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
  x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
  x #display the column
}
merged_data_mean=data.frame(apply(merged_data,2,f))
sapply(merged_data, function(x) unique(x))
merged_data$Product_Category_2 <- merged_data_mean$Product_Category_2
merged_data$Product_Category_3 <- merged_data_mean$Product_Category_3
str(merged_data)
merged_data[,c(5,8,9,10,11,12)]<-lapply(merged_data[,c(5,8,9,10,11,12)],factor)
sapply(merged_data,class)
train <- merged_data[merged_data$label == 1,]
str(train)
train <- train[,-12]
train$purchase <- primary$Purchase
test <- merged_data[merged_data$label == 0,]
str(test)
test <- test[,-12]
#GenerateReport(merged_data)

############## subset 75:25

## 75% of the sample size
smp_size <- floor(0.75 * nrow(train))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

train75 <- train[train_ind, ]
test25 <- train[-train_ind, ]
label <- test25$purchase
test25 <- test25[,-12]
#str(test25)
memory.limit(1200000)

model <- lm (purchase ~ ., data = train75)
gc()
