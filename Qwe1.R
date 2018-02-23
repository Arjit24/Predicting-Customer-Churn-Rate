library(data.table) # for set names
install.packages("ROSE")# for under/over sampling

# Attach dataset
qwe<-read.csv(file.choose(),header = T , stringsAsFactors = F)
str(qwe)
View(qwe)

#Data Cleaning

#Convert "-" to 0
for(i in 1:13){
  qwe[,i] <- (gsub("-","0",qwe[,i]))
}

#Concerting (x) into negatives -x
convert.brackets <- function(x){
  if(grepl("\\(.*\\)", x)){
    paste0("-", gsub("\\(|\\)", "", x))
  } else {
    x
  }
}


for(i in 1:13){
  qwe[i]<-sapply(qwe[,i], convert.brackets, USE.NAMES = F) 
  qwe[,i] <- (gsub(",","",qwe[,i]))# replacing commas with spaces
  qwe[,i] <- (gsub(" ","",qwe[,i]))# deleting spaces for numeric conversions
  qwe[,i]<-as.numeric(qwe[,i]) 
 
}


#change column names
oldnames<-colnames(qwe)
newnames<-c("Id","Age","Churn", 
            "CHICurrent","CHIchange", 
            "SCcurrent","SCchange",
            "SPcurrent","SPchange",
            "LoginChange","BlogsChange",
            "ViewChange","LastLoginChange")

setnames(qwe,oldnames,newnames)
View(qwe)
str(qwe)
attach(qwe)

qwe$Age1<-0
qwe$Age1<-ifelse(Age<6,"less than 6", ifelse (Age<=14,"between 6 & 14","greater Than 14"))
class(qwe$Age1)
View(qwe)
qwe1<-subset(qwe, Churn ==1)
t<-table(qwe1$Churn,qwe1$Age1)
t1<-table(Churn,qwe$Age1)

n<-barplot(t,beside =T , col=("steelblue") , ylim =c(0,200), main = "Churn vs Customer Age")
text(n, 0, round(t, 1),cex=2,pos=3) 
box()

#As shown in the graph we can say that Wall's belief about the dependence of churn rates on customer age supported by the data, as the churn rate is maximum between age 6 $ 14

#scaling data , min max transformation
qweS<-qwe
maxs = apply(qweS[ ,2:13], 2, max) # apply(x, margin, function). If margin = 1, function is
#applied on the rows and when margin = 2, function is applied on the columns
mins = apply(qweS[ , 2:13], 2, min)

qweS= as.data.frame(scale(qweS[ ,2:13], center = mins, scale = maxs - mins))
qweS$Churn<-as.factor(qweS$Churn)
str(qweS)
View(qweS)
table(qweS$Churn)


set.seed(1234)

#Random Forest

qwe2<-qwe[,2:13]
str(qwe2)
#Creating model without adjusting unbalanced data - Unscaled
library(randomForest)
str(qweF1)
qweF1<-qwe2
index = sample(2,nrow(qweF1), replace= T, prob = c(0.7,0.3)) 
train1 = qweF1[index==1,] 
test1 = qweF1[index==2,] 

rf = randomForest(Churn ~ . , data = train1, ntree = 100, mtry = 5, proximity =
                    TRUE, replace = TRUE, importance = TRUE ) 
print(rf)
pred<-predict(rf,newdata=test1)
accuracy<-table(pred,test1$Churn)
sum(diag(accuracy))/sum(accuracy)
#accuracy is 0.94684 , but recall is low, 0.06

# performing undersampling to adjust unbalanced data
qweU <- ovun.sample(Churn ~ ., data = qwe2, method = "under", N = 646 , seed = 1)$data
qweF1<-qweU
index = sample(2,nrow(qweF1), replace= T, prob = c(0.7,0.3)) 
train1 = qweF1[index==1,] 
test1 = qweF1[index==2,] 

rf = randomForest(Churn ~ . , data = train1, ntree = 100, mtry = 5, proximity =
                    TRUE, replace = TRUE, importance = TRUE ) 
print(rf)
pred<-predict(rf,newdata=test1)
accuracy<-table(pred,test1$Churn)
sum(diag(accuracy))/sum(accuracy)
i<-importance(rf)
v<-varImpPlot(rf)
#accuracy is 0.66 , recall is 0.63



#performing oversampling to adjust unbalanced data
qweO <- ovun.sample(Churn ~ ., data = qwe2, method = "over", N = 12048 , seed = 5)$data
table(qweO$Churn)
qweF1<-qweO
index = sample(2,nrow(qweF1), replace= T, prob = c(0.7,0.3)) 
train1 = qweF1[index==1,] 
test1 = qweF1[index==2,] 
table(qweF1$Churn)
rf = randomForest(Churn ~ . , data = train1, ntree = 100, mtry = 5, proximity =
                    TRUE, replace = TRUE, importance = TRUE ) 
print(rf)
plot(rf)
pred<-predict(rf,newdata=test1)
accuracy<-table(pred,test1$Churn)
sum(diag(accuracy))/sum(accuracy)
#0.9881 accuracy , recall -0.96


#performing both under and oversampling to adjust unbalanced data
qweB <- ovun.sample(Churn ~ ., data = qweS, method = "both", N = 2500 , seed = 5)$data
table(qweB$Churn)
qweF1<-qweB
index = sample(2,nrow(qweF1), replace= T, prob = c(0.7,0.3)) 
train1 = qweF1[index==1,] 
test1 = qweF1[index==2,] 
table(qweF1$Churn)
rf = randomForest(Churn ~ . , data = train1, ntree = 100, mtry = 5, proximity =
                    TRUE, replace = TRUE, importance = TRUE ) 
plot(rf)
print(rf)
pred<-predict(rf,newdata=test1)
accuracy<-table(pred,test1$Churn)
sum(diag(accuracy))/sum(accuracy)
#accuracy - 0.90 , recall -0.94


#Logistic Model

#Creating model without adjusting unbalanced data - Unscaled
qwe$Churn<-as.factor(qwe$Churn)
str(qwe)
qweF1<-qwe[,2:13]
str(qweF1)
index = sample(2,nrow(qweF1), replace= T, prob = c(0.7,0.3)) 
train1 = qweF1[index==1,] 
test1 = qweF1[index==2,] 

modelF1<-glm(Churn~.,data = train1[,-1], family = "binomial")
summary(modelF1)
p1<-predict(modelF1, newdata=test1, type="response")
test1$prob<-p1
test1$p<-ifelse(test1$prob<0.5,0,1)
test1$p<-as.factor(test1$p)
accuracy <- table(test1$p,test1$Churn)
sum(diag(accuracy))/sum(accuracy)
#accuracy is 0.94561, but recall is 0

#Creating model on Scaled data 
qweF1<-qweS
index = sample(2,nrow(qweF1), replace= T, prob = c(0.7,0.3)) 
train1 = qweF1[index==1,] 
test1 = qweF1[index==2,] 
str(train1)
str(test1)
modelF1<-glm(Churn~.,data = train1, family = "binomial")
summary(modelF1)
p1<-predict(modelF1, newdata=test1, type="response")
test1$prob<-p1
test1$p<-ifelse(test1$prob<0.5,0,1)
test1$p<-as.factor(test1$p)
accuracy <- table(test1$p,test1$Churn)
sum(diag(accuracy))/sum(accuracy)
#0.9522587, again recall is 0

# performing undersampling to adjust unbalanced data
qwe2<-qwe[,2:13]
str(qwe2)
library(ROSE)
qweU <- ovun.sample(Churn ~ ., data = qweS, method = "under", N = 646 , seed = 1)$data
table(qweU$Churn)
str(qweU)
qweF1<-qweU
index = sample(2,nrow(qweF1), replace= T, prob = c(0.7,0.3)) 
train1 = qweF1[index==1,] 
test1 = qweF1[index==2,] 
str(train1)
str(test1)
modelF1<-glm(Churn~.,data = train1, family = "binomial")
summary(modelF1)
p1<-predict(modelF1, newdata=test1, type="response")
test1$prob<-p1
test1$p<-ifelse(test1$prob<0.5,0,1)
test1$p<-as.factor(test1$p)
accuracy <- table(test1$p,test1$Churn)
sum(diag(accuracy))/sum(accuracy)
#accuracy 0.5837321, recall - 0.65

#performing oversampling to adjust unbalanced data
str(qweS)
qweO <- ovun.sample(Churn ~ ., data = qweS, method = "over", N = 12048 , seed = 5)$data
table(qweO$Churn)
qweF1<-qweO
index = sample(2,nrow(qweF1), replace= T, prob = c(0.7,0.3)) 
train1 = qweF1[index==1,] 
test1 = qweF1[index==2,] 
modelF1<-glm(Churn~.,data = train1, family = "binomial")
summary(modelF1)
p1<-predict(modelF1, newdata=test1, type="response")
test1$prob<-p1
test1$p<-ifelse(test1$prob<0.5,0,1)
test1$p<-as.factor(test1$p)
accuracy <- table(test1$p,test1$Churn)
sum(diag(accuracy))/sum(accuracy)
#accuracy 0.5931034 , recall -0.59

#performing both under and oversampling to adjust unbalanced data

qweB <- ovun.sample(Churn ~ ., data = qweS, method = "both", N = 2500 , seed = 5)$data
table(qweB$Churn)
qweF1<-qweB
index = sample(2,nrow(qweF1), replace= T, prob = c(0.7,0.3)) 
train1 = qweF1[index==1,] 
test1 = qweF1[index==2,] 
modelF1<-glm(Churn~.,data = train1, family = "binomial")
summary(modelF1)
p1<-predict(modelF1, newdata=test1, type="response")
test1$prob<-p1
test1$p<-ifelse(test1$prob<0.5,0,1)
test1$p<-as.factor(test1$p)
accuracy <- table(test1$p,test1$Churn)
accuracy
sum(diag(accuracy))/sum(accuracy)
#accuracy - 0.5590969 , recall -0.65

#hence, Model performance is evaluated on both accurac and recall. As the unbalanced data give zero recall, it is not good for prediction,
#therefore we choose model with greater accuracy and recall that is, logistic regression with undersampling



#prediction for 100 customers with higest churn probability 


qweF1<-qweS
index = sample(2,nrow(qweF1), replace= T, prob = c(0.7,0.3)) 
train1 = qweF1[index==1,] 
test1 = qweF1[index==2,] 
str(train1)
str(test1)
modelF1<-glm(Churn~.,data = train1, family = "binomial")
summary(modelF1)
step(modelF1,direction = c("both", "backward", "forward"))

p2<-predict(modelF1, newdata = qweS, type = "response")
p2
qwe$prob<-p2
View(qwe)

top<-qwe[,c(1,14)]
View(top)
top100<-top[order(-top$prob),]
View(top100)
top100<-top100[1:100,]
View(top100)
head(top100)

#based on randome forest undersampled model, we can use function importance to find the top three drivers of the churn
#finding importance() of rf, "i" variable was used earlier
i
#from the output, the top three drivers are - Age , CHICurrent, LastLoginCHange