#Assignment 3 Part 2
args <- commandArgs(trailingOnly = TRUE)

trd<- read.table(args[1], header = T)
ted<-read.table(args[2], header = T)

paste("Type of the first argument is")
class(trd)
paste("Type of the second argument is")
class(trd)

require(e1071)
require(class)
require(MASS)
require(klaR)
require(caret)

#library(e1071)


paste("names of columns of dataset 1 are ")
names(trd)
paste("names of columns of dataset 2 are")
names(ted)

paste( "number of cols of dataset1 ", ncol(trd), "number of rows of dataset 1", nrow(trd), sep = "  ")

paste(" number of cols  of test data", ncol(ted),"number of rows of dataset 2", nrow(ted), sep = " ") 

#given that the last column is always class and binary with value 0 or 1
#splitting training data frame into two halves, one with zeros the other with 1's 
trndata_0<-subset(trd,trd[, ncol(trd)]== 0 )
trndata_1<-subset(trd,trd[, ncol(trd)]== 1 )
paste("Num of rows of train data subset with zeros", nrow(trndata_0), sep = " ")
paste("Num of rows of train data subset with ones", nrow(trndata_1), sep = " ")
s<-(nrow(trndata_1)+nrow(trndata_0))
paste("Proving no data is lost, sum of count of num of rows of above subsets =", s, sep = " ")

#>>>>> #for iterating over all columns except last
num_it<-ncol(trndata_0)-1

#for getting printing format
a<-names(trd)
b<-names(ted)
paste("Training data counts of zeros and 1's per each slice based on class", sep = " ")

for (i in 1:num_it) {
  
  
  print(paste(" DataFrame0 num of zeros of ", a[i], "column =",sum(trndata_0[i]==0), sep = " " ))
  print(paste(" DataFrame0 num of ones of ", a[i], "column =",sum(trndata_0[i]==1),sep = " " ))
  
  print(paste("******* ", sep = " "))
  
  print(paste(" DataFrame1 num of zeros of ", a[i], "column =",sum(trndata_1[i]==0),sep = " " ))
  print(paste(" DataFrame1 num of ones of ", a[i], "column =",sum(trndata_1[i]==1),sep = " " ))
  
  print(paste("##### ", sep = " "))
  
}
paste("******************************************** ", sep = " ")
paste("printing CLass wise probabilities", sep = " ")

traindata_0<-trndata_0
traindata_1<-trndata_1
traindata<-trd
tnum_it<-num_it
################################################################################################################################

######### Part 1 main printing ###########
sizeoftraindata_class0<-nrow(traindata_0) # number of rows of class zero

#printing the probabilities of each attribute per class
#probability of Class =0 for training data is 
trndata_prob_for_class_0<-nrow(traindata_0)/nrow(traindata)

paste("P(Class = 0) :", trndata_prob_for_class_0)

for (i in 1:tnum_it) {
  
  
  zeros_traindata_att<-sum(traindata_0[i]==0) #num of zeros in attribute i when class is 0 
  prob_zeros_in_attr_when_class_is_0<- (zeros_traindata_att)/(sizeoftraindata_class0)
  print(paste(" Probability of Attribute ", a[i], " =0 | Class = 0)", prob_zeros_in_attr_when_class_is_0, sep = " "))
  
  ones_traindata_att<-sum(traindata_0[i]==1) # num of ones in attribute i when class is 0
  prob_ones_in_attr_when_class_is_0<- (ones_traindata_att)/(sizeoftraindata_class0)
  print(paste(" Probability of Attribute ", a[i], " =1 | Class = 0)", prob_ones_in_attr_when_class_is_0, sep = " " ))
  
  print(paste("******* ", sep = " "))
  
  
}

paste("--------------------------------------------------------", sep = " ")

#probability of Class =1 for training data is

trndata_prob_for_class_1<-nrow(traindata_1)/nrow(traindata)
sizeoftraindata_class1<-nrow(traindata_1) #number of rows of class 1

paste("P(Class = 1) :", trndata_prob_for_class_1, sep = " ")
for (i in 1:tnum_it) {
  
  
  zeros_traindata_att_1<-sum(traindata_1[i]==0) #num of zeros in attribute i when class is 1 
  prob_zeros_in_attr_when_class_is_1<- (zeros_traindata_att_1)/(sizeoftraindata_class1)
  print(paste(" Probability of Attribute ", a[i], " =0 | Class = 1)", prob_zeros_in_attr_when_class_is_1, sep = " "))
  
  ones_traindata_att_1<-sum(traindata_1[i]==1) # num of ones in attribute i when class is 1
  prob_ones_in_attr_when_class_is_1<- (ones_traindata_att_1)/(sizeoftraindata_class1)
  print(paste(" Probability of Attribute ", a[i], " =1 | Class = 1)", prob_ones_in_attr_when_class_is_1, sep = " " ))
  
  print(paste("******* ", sep = " "))
  
  
}


#naive bayes

trn_nb<-trd
tes_nb<-ted

trn_nb<-trn_nb[complete.cases(trn_nb),]
tes_nb<-tes_nb[complete.cases(tes_nb),]

trn_nb[]<-lapply(trn_nb, factor)
tes_nb[]<-lapply(tes_nb, factor)

# trn_nb$class<-as.factor(trn_nb$class) this is for converting class column,  
# tes_nb$class<-as.factor(tes_nb$class)
#ntcol<-ncol(trn_nb)
split=0.80
trainIndex <- createDataPartition(trn_nb$class, p=split, list=FALSE)
data_train <- trn_nb[ trainIndex,]
data_test <- trn_nb[-trainIndex,]

# train a naive bayes model
#f <- (paste(tail(names(trn_nb), 1))) accessing last column name. 
model <- NaiveBayes(class~., data=data_train)
# make predictions
x_test <- data_test[,1:(ncol(data_test)-1)]
y_test <- data_test[,ncol(data_test)]

predictions <- predict(model, x_test)
# summarize results
#confusionMatrix(predictions$class, y_test)

cm<-confusionMatrix(predictions$class, y_test)

overall.accuracy <- cm$overall['Accuracy']
paste("Training data accuracy on ", overall.accuracy*100, sep =" " )


#test data 
split=0.80
#trainIndex <- createDataPartition(trn_nb$class, p=split, list=FALSE)
#data_train <- trn_nb[ trainIndex,]
#data_test <- trn_nb[-trainIndex,]
data_test<-tes_nb
# train a naive bayes model
#model <- NaiveBayes(class~., data=data_train)
# make predictions
x_testd <- data_test[,1:(ncol(data_test)-1)]
y_testd <- data_test[,ncol(data_test)]
predictions <- predict(model, x_testd)
# summarize results
cm<-confusionMatrix(predictions$class, y_testd)
overall.accuracy <- cm$overall['Accuracy']
overall.accuracy*100
paste("Testing data accuracy  ", overall.accuracy*100, sep =" " )

