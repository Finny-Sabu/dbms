mystring <- "Hello world!!"
print (mystring)

setwd("D:/Sreenivas")

dir()


ls()

x<-9
class(x)

x

print(x)

x<-'s'
is.character(x)


is.integer(x)


y<-'3.14'
as.integer(y)

##execute from here

x<- c(12.5,89.2,41.9,7.8)
y<- vector("logical", length = 10)
length(x)

y

y<-c(2,3,4,6)

10*x

x*y

x^y

m<-matrix(c(12,56,89,47,36,49,73,45,31), nrow=3, ncol=3)
m


dim(m)

attributes(m)

m<- matrix(c(11,12,13,14,15,16,17,18,19), nrow=3, ncol=3, byrow=TRUE)
m

x<-c(1,5,9)
y<-c(45,90,22)
cbind(x,y)

rbind(x,y)

p<-5*m
p

n<-matrix(c(5,6,7,9,8,4,3,0,1), nrow=3, ncol=3)
n

q<-m+n
q

o<-matrix(c(78,56,12,54,32,98), nrow=3, ncol=2)
o

r<- m %*% o
r

mdash<-t(m)
mdash

s<-matrix(c(45,56,78,23,4,9,1,2,3), nrow=3, ncol=3, byrow=TRUE)
s_det<-det(s)
s_det


x<-list(9, "s", TRUE, 7-8i)
x


status<-c("low", "high", "medium", "high", "low")
x<-factor(status, ordered=TRUE, levels = c("low", "medium", "high"))
x

student_id_sree<-c(1,5,9)
student_names<-c("Sree", "Omkar", "Mayuresh")
position<-c("first", "second", "third")
data <- data.frame(student_id_sree, student_names, position)
data


data$student_id_sree

nrow(data)

ncol(data)

names(data)

smoke<-matrix(c(5,8,3,6,4,7,9,2,0),nrow=3, byrow = TRUE)
colnames(smoke)<-c("high", "low", "middle")
rownames(smoke)<-c("current", "former", "never")
smoke <- as.table(smoke)
smoke
 
data_sree <- read.table("dataSree.csv", sep=",", header = T)
data_sree

dim(data_sree)

head(data_sree, 2)

tail(data_sree, 2)

z<- data.frame(a=5, b=9, c=pi)
write.csv(z, file="data_sree")


install.packages("XLConnect")
library(XLConnect)

dataS<- XLConnect::readWorksheetFromFile("employeeDetails_sree.xlsx", sheet=1)
dataS


dataY <- dataS[1:2,]
dataY


data2<- read_excel("employeeDetails_sree.xlsx", sheet=1)
data2

data3<-data2[1:2,]
write_xlsx(data3, "e2.xlsx")
data3

data<-data.frame(Name=character(), Age=numeric())

data <- edit(data)
data


### Experiment 2 ###

setwd("C:/Users/sreen/OneDrive/Desktop")
getwd()


mydata<-read_excel("carsDetails.xlsx", sheet=1)
head(mydata,5)

install.packages("readxlsx")
library(readxlsx)


install.packages("XLConnect")
library(XLConnect)
install.packages("readxl")
library(readxl)
install.packages("writexl")
library(writexl)

setwd("C:/Users/sreen/OneDrive/Desktop")


mydata <- mtcars
head(mydata)

mydata1 <- mydata[1:6, 1:5]
mydata1


require(dplyr)
mydata1 = rename(mydata1, horse_power = hp)
mydata1


mydata1$new_hp1 <- mydata1$horse_power * 0.5
colnames(mydata1)


mydata


data2 =read.table(file="C:/Users/sreen/OneDrive/Desktop/missingCol1.csv", sep=",")
data2

data2 = read.csv(file="C:/Users/sreen/OneDrive/Desktop/missingCol1.csv", col.names = c("SrNo", "Name", "Salary", "DOJ", "Dept"))
data2

NA+4

V <- c(1,2,NA,4)
median(V)

median(V, na.rm = T)

is.na(V)

naVals <- is.na(V)
V[!naVals]

V[complete.cases(V)]


data3 <- read.csv(file="C:/Users/sreen/OneDrive/Desktop/missingCol1.csv", na.strings = "")
data3


dataCompleteCases <- data3[complete.cases(data3),]
dataCompleteCases


install.packages("Hmisc")
library(Hmisc)



x<- c(17,45,19,NA,56,NA,45)
x <- impute(x, fun=mean)
x


x<- impute(x, fun = median)
x


gender_vector<- c("Male", "Female", "Female", "Male", "Male")
class(gender_vector)


factor_gender_vector <- factor(gender_vector)
class(factor_gender_vector)


day_vector <- c('evening', 'morning', 'afternoon', 'midday', 'midnight', 'evening')

factor_day <- factor(day_vector, ordered = TRUE, levels = c('morning', 'midday', 'afternoon', 'evening', 'midnight'))
factor_day


age<- c(19,23,45,36,16,50)
salary<- c(10323,45687,20316,10200,2000,95621)
gender<- c("male", "male", "trans", "female", "male", "female")


employee<- data.frame(age, salary, gender)
employee


wfact = cut(employee$age, 3, labels = c('young', 'medium', 'aged'))
table(wfact)


##regression analysis##
##experiment 3##

library(ggplot2)
mydata<- mtcars
names(mydata)

dim(mydata)

mydata <- mydata[sample(nrow(mydata),),]
head(mydata)


trainData <- mydata[1:20,]
testData <- mydata[21:32,]

fit = lm(mpg ~ hp, data=mtcars)
summary(fit)

prediction <- predict(fit, newdata = testData)
df1 <- data.frame(prediction, testData$mpg)
head(df1)

cor(prediction, testData$mpg)

plot(mtcars$hp, mtcars$mpg)


ggplot(fit, aes(hp, mpg))+
  geom_point()+
  stat_smooth(method = lm, se = FALSE)+
  geom_segment(aes(xend = hp, yend = .fitted), color = "red", size = 0.3)



lmmodel1 <- lm(mpg ~ hp+cyl+gear+wt, data = trainData)
summary(lmmodel1)


preds_new <- predict(lmmodel1, newdata = testData)
df2 <- data.frame(preds_new, testData$mpg)
head(df2)

cor(preds_new, testData$mpg)

plot(mtcars$hp+mtcars$cyl+mtcars$gear+mtcars$wt, mtcars$mpg)


ggplot(fit, aes(mtcars$hp+mtcars$cyl+mtcars$gear+mtcars$wt, mpg))+
  geom_point()+
  stat_smooth(method = lm, se = FALSE)+
  geom_segment(aes(xend = hp, yend = .fitted), color = "red", size = 0.3)



##experiment 4##
##classification##


library(e1071)
library("klaR")


library("caret")


library(ggplot2)
data("iris")
head(iris)


unique(iris$Species)


pairs(iris[1:4], main="Iris Data (red=setosa, green=versicolor, blue=virginica)",
      pch=21, bg=c("red", "green3", "blue")[unclass(iris$Species)])


index = sample(nrow(iris), floor(nrow(iris) * 0.7))
train = iris[index,]
test = iris[-index,]

xTrain = train [,-5]
yTrain = train$Species

xTest = test[,-5]
yTest = test$Species

model = train(xTrain, yTrain, 'nb', trControl=trainControl(method='cv', number=10))
model


prop.table(table(predict(model$finalModel,xTest)$class,yTest))


df <- data(iris)
head(iris)

ran <- sample(1:nrow(iris), 0.9 * nrow(iris))
nor <-function(x) { (x -min(x))/(max(x)-min(x)) }
iris_norm <- as.data.frame(lapply(iris[,c(1,2,3,4)], nor))
summary(iris_norm)

iris_train <- iris_norm[ran,]
iris_test <- iris_norm[-ran,]
iris_target_category <- iris[ran,5]
iris_test_category <- iris[-ran,5]
library(class)
pr <- knn(iris_train,iris_test,cl=iris_target_category,k=13)
tab <- table(pr,iris_test_category)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)


##experiment 5##

my_data <- read.csv("data_apriori.csv")
trans <- split(my_data$Products, my_data$Customer_Id, "transactions")
head(trans)


library(arules)
install.packages("arules")


rules = apriori (trans, parameter= list(support=0.5, confidence=0.9, maxlen=3, minlen=2 ))


inspect (rules)




##experiment 6##
##clustering##


head(iris)
 
library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color=Species))+ geom_point()


set.seed(20)
irisCluster <- kmeans(iris[,3:4], 3, nstart = 20)
irisCluster


table(irisCluster$cluster, iris$Species)

irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point()

data1 <- iris
head(data,8)

aggCluster <- hcluster(dist(iris[,3:4]))
aggCluster

plot(clusters)


clusterCut<- cutree(clusters, 3)
table(clusterCut, iris$Species)

clusters <- hclust(dist(iris[,3:4]), method = 'average')
plot(aggCluster)

clusterCut <- cutree(clusters, 3)
table(clusterCut, iris$Species)
ggplot(iris, aes(Petal.Length, Petal.Width, color = irisCluster$cluster)) + geom_point(alpha =0.4, size = 3.5)+
  goem_point(col = clusterCut) + scale_color_manual(values=c('black', 'red', 'green'))


##omkar
## Prac 6

head(iris)

library(ggplot2)
ggplot(iris, aes(Petal.Length, Petal.Width, color=Species)) + geom_point()

set.seed(20)
irisCluster <- kmeans(iris[, 3:4], 3, nstart = 20)
irisCluster

table(irisCluster$cluster, iris$Species)

irisCluster$cluster <- as.factor(irisCluster$cluster)
ggplot(iris, aes(Petal.Length, Petal.Width, color= irisCluster$cluster)) + geom_point()


head(iris) 
clusters <- hclust(dist(iris[, 3:4])) 
plot(clusters)

clusterCut <- cutree(clusters, 3)
table(clusterCut, iris$Species)

clusters <- hclust(dist(iris[,3:4]), method = 'average') 
plot(clusters) 

clusterCut <- cutree(clusters, 3) 
table(clusterCut,iris$Species) 

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + geom_point(alpha= 0.4, size = 3.5) + geom_point(col = clusterCut) + scale_color_manual(values = c('black','red', 'green'))
