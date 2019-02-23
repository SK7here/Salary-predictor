rm(list = ls()) #To Clear environment

setwd("C:/College materials/Softwares/R software/Datasets") #Set corresponding dir
dataset=read.csv("Salary_Data.csv",header=T)

#install.packages("caTools") #If caTools package not installed
library(caTools) 

set.seed(70) #To lock the code
split=sample.split(dataset$Salary,SplitRatio=0.8)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)

regressor=lm(formula = Salary~YearsExperience,data=training_set) 
summary(regressor) #Linear model

y_test =predict(regressor,newdata = test_set)
y_train = predict(regressor,newdata=training_set)

print(y_train)
#Result
print(y_test)

#install.packages("ggplot2")  #If ggplot2 package not installed
library(ggplot2)

#Training data in graph format
ggplot() + 
  geom_point(aes(x=training_set$YearsExperience,y=training_set$Salary),colour="red") +
  geom_line(aes(x=training_set$YearsExperience,y=y_train),colour="blue")+
  xlab("Years Of Experience") + ylab("Salary") + 
  ggtitle("Linear regression sample - Training set")

#Testing data in graph format
ggplot() + 
  geom_point(aes(x=test_set$YearsExperience,y=test_set$Salary),colour="red") +
  geom_line(aes(x=training_set$YearsExperience,y=y_train),colour="blue")+
  xlab("Years Of Experience") + ylab("Salary")
  ggtitle("Linear regression sample - Test set")
  

  
