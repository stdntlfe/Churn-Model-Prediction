
#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)
train=read.csv(file.choose(),sep=',',header = T)

test=read.csv(file.choose(),sep=',',header = T)
data=rbind(train,test)
View(data)
head(data)
str(data)
#Missing Value Trweatment
table(is.na(data))




#**************************Variable transformation****************
table(is.na(data))
for(i in 1:ncol(data)){
  
  if(class(data[,i]) == 'factor'){
    
    data[,i] = factor(data[,i], 
                      
  labels=(1:length(levels(factor(data[,i])))))
    
  }
}

View(data)
#************************Outlier*********************************************

# BoxPlots - Distribution and Outlier Check
numeric_index = sapply(data,is.numeric) #selecting only numeric

numeric_data = data[,numeric_index]

cnames = colnames(numeric_data)
cnames
library(ggplot2)

for (i in 1:length(cnames))
   {
     assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Churn"), data = subset(data))+ 
             stat_boxplot(geom = "errorbar", width = 0.5) +
              geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                           outlier.size=1, notch=FALSE) +
              theme(legend.position="bottom")+
              labs(y=cnames[i],x="Churn")+
              ggtitle(paste("Box plot of churn for",cnames[i])))
   }
   
  # ## Plotting plots together
gridExtra::grid.arrange(gn1,gn5,gn2,gn3,ncol=4)
gridExtra::grid.arrange(gn6,gn7,gn4,gn10,ncol=4)
gridExtra::grid.arrange(gn8,gn9,gn11,gn12,ncol=4)
gridExtra::grid.arrange(gn13,gn14,gn15,gn16,ncol=4)
 
                                   
# # #loop to remove from all variables
 for(i in cnames){
   print(i)
   val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
   print(length(val))
   data = data[which(!data[,i] %in% val),]
 }

 for(i in cnames){
  val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  print(length(val))
   data[,i][data[,i] %in% val] = NA
 }
 
data = knnImputation(data, k = 3)
table(is.na(data))
library(corrgram)
##################################Feature Selection################################################
## Correlation Plot 
corrgram(data[,numeric_index], order = F,upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

## Chi-squared Test of Independence
factor_index = sapply(data,is.factor)
factor_index
factor_data = data[,factor_index]
factor_data
names(factor_data)

for (i in 1:4)
{
  print(names(factor_data)[i])
  print(chisq.test(table(factor_data$Churn,factor_data[,i])),simulate.p.value = TRUE)
}

## Dimension Reduction
data = subset(data,select = -c(phone.number))

##################################Feature Scaling################################################
#Normality check
qqnorm(data$account.length)
hist(data$number.vmail.messages)

#Normalisation
cnames = c("account.length","area.code","number.vmail.messages",
           "total.day.minutes","total.day.calls","total.day.charge",
           "total.eve.minutes","total.eve.calls","total.eve.charge",
           "total.night.minutes","total.night.calls","total.night.charge",
           "total.intl.minutes","total.intl.calls","total.intl.charge",
           "number.customer.service.calls")

#for(i in cnames){
 # print(i)
#  data[,i] = (data[,i] - min(data[,i]))/(max(data[,i] - min(data[,i])))
#}

# #Standardisation
 for(i in cnames){
   print(i)
   data[,i] = (data[,i] - mean(data[,i]))/sd(data[,i])
 }
View(data)


#############################################Sampling#############################################
# ##Simple Random Sampling
 data_sample = data[sample(nrow(data), 1000, replace = F), ]


#Divide data into train and test using stratified sampling method
set.seed(1234)
library(caret)
sampl = createDataPartition(data$Churn, p = .80, list = FALSE)
train = data[ sampl,]
test  = data[-sampl,]
View(test)
# Random Forest
RF_model = randomForest(Churn ~ ., train, importance = TRUE, ntree = 500)

#Presdict test data using random forest model
RF_Predictions = predict(RF_model, test[,-20])

##Evaluate the performance of classification model
ConfMatrix_RF = table(test$Churn, RF_Predictions)
confusionMatrix(ConfMatrix_RF)


