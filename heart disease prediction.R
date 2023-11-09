##########################################GROUP 6################################################
library(plyr)
library(readr)
library(caret)
library(ggplot2)
library(repr)
library(dplyr)
library(AICcmodavg)
library("ggpubr")
library("neuralnet")

##################################################################################################
###########################################Data Preparation#######################################
##################################################################################################
#read data
hd<-read.csv("C:\\Users\\kheec\\Downloads\\KD24203 Data mining&W\\project\\heart_2020_cleaned.csv")
str(hd[1,])
summary(hd)
head(hd)

#Encoding categorical labels
hd$HeartDisease<-ifelse(hd$HeartDisease=="Yes",1,0)
hd$Smoking<-ifelse(hd$Smoking=="Yes",1,0)
hd$AlcoholDrinking<-ifelse(hd$AlcoholDrinking=="Yes",1,0)
hd$Stroke<-ifelse(hd$Stroke=="Yes",1,0)
hd$DiffWalking<-ifelse(hd$DiffWalking=="Yes",1,0)
hd$Sex<-ifelse(hd$Sex=="Male",1,0)
unique(hd$AgeCategory)
hd$AgeCategory = factor(hd$AgeCategory,
                     levels = c('18-24','25-29','30-34','35-39','40-44','45-49',
                                '50-54','55-59','60-64','65-69','70-74','75-79','80 or older'),
                     labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13))
unique(hd$Race)
hd$Race = factor(hd$Race,
                      levels = c('White','Black','Asian','American Indian/Alaskan Native','Other','Hispanic'),
                      labels = c(1, 2, 3, 4, 5, 6))
unique(hd$Diabetic)
hd$Diabetic = factor(hd$Diabetic,
                     levels = c('Yes','No','No, borderline diabetes','Yes (during pregnancy)'),
                     labels = c(1, 2, 3, 4))
hd$PhysicalActivity<-ifelse(hd$PhysicalActivity=="Yes",1,0)
unique(hd$GenHealth)
hd$GenHealth = factor(hd$GenHealth,
                     levels = c('Very good','Fair','Good','Poor','Excellent'),
                     labels = c(1, 2, 3, 4, 5))
hd$Asthma<-ifelse(hd$Asthma=="Yes",1,0)
hd$KidneyDisease<-ifelse(hd$KidneyDisease=="Yes",1,0)
hd$SkinCancer<-ifelse(hd$SkinCancer=="Yes",1,0)

#Convert factor to numeric
hd$AgeCategory=as.numeric(hd$AgeCategory)
hd$Race=as.numeric(hd$Race)
hd$Diabetic=as.numeric(hd$Diabetic)
hd$GenHealth=as.numeric(hd$GenHealth)

#Check again the data types of variables
str(hd)

#Check for missing values
sapply(hd, function(x) sum(is.na(x)))
hd<-hd[!duplicated(hd),]
hd

#######################################Data sampling##############################################
set.seed(500)
hd_new<-sample(1:nrow(hd),500)
hd_new<-hd[hd_new,]

#########################################Feature Selection########################################
###############################################Anova##############################################
#One way anova
BMI.aov<-aov(HeartDisease~BMI, data=hd_new)
summary(BMI.aov)
Sex.aov<-aov(HeartDisease~Sex, data=hd_new)
summary(Sex.aov)

#Two way anova
two.way<-aov(HeartDisease~BMI+AgeCategory,data=hd_new)
summary(two.way)

#Multiple variables
n<-names(hd_new[,2:18])
form<-as.formula(paste("HeartDisease~",paste(n[!n %in% "use"],collapse="+")))
anova<-aov(form,data=hd)
summary(anova)
anova1<-aov(HeartDisease~BMI+Smoking+AlcoholDrinking+Stroke+PhysicalHealth+MentalHealth+
              DiffWalking+Sex+AgeCategory+Diabetic+PhysicalActivity+Asthma+KidneyDisease+
              SkinCancer,data=hd_new)
summary(anova1)

#Find the best-fit model
model.set<-list(BMI.aov,Sex.aov,two.way,anova,anova1)
model.names<-c("BMI.aov","Sex.aov","two.way","anova","anova1")
aictab(model.set, modnames = model.names)

##################################################################################################
##############################################Data Mining#########################################
###########################################Data visualization#####################################
##################################################################################################
library(scales)
########################### Heart Data For Plotting ##############################
set.seed(500)
hd1<-read.csv("C:\\Users\\kheec\\Downloads\\KD24203 Data mining&W\\project\\heart_2020_cleaned.csv")
hd_plot<-sample(1:nrow(hd1),500)
hd_plot<-hd1[hd_plot,]
hd_plot

#Separate Heart Disease into Yes or No
yes <- hd_plot[hd_plot$HeartDisease == 'Yes',]
no <- hd_plot[hd_plot$HeartDisease == 'No',]

################################################################################
######################### Histogram of BMI #####################################
p1<-hist(yes$BMI)
p2<-hist(no$BMI)

plot( p1, col=rgb(0,0,1,1/4), freq = FALSE, xlab = "BMI", main = "Histogram of BMI")
plot( p2, col="yellow", freq = FALSE, add=T)

line1 <- density(yes$BMI)
polygon(line1, border="blue" , lwd = 1)

line2 <- density(no$BMI)
polygon(line2, border="red" , lwd = 1)

legend("topright",
       c('Heart Disease','No Heart Disease'),
       fill = c(rgb(0,0,1,1/4),"yellow"), 
       cex = 0.7)
legend("right",
       c('Heart Disease','No Heart Disease'),
       lty = c(1,1),
       col = c("blue","red"),
       cex = 0.7)

################################################################################
######################### Histogram of Physical Health #########################
p3<-hist(yes$PhysicalHealth)

p4<-hist(no$PhysicalHealth)

plot( p4, col="yellow", freq = FALSE, xlab = "Physical Health", main = "Histogram of Physical Health")
plot( p3, col=rgb(0,0,1,1/4), freq = FALSE, add=T)

line3 <- density(yes$PhysicalHealth)
polygon(line3, border="blue" , lwd = 1)

line4 <- density(no$PhysicalHealth)
polygon(line4, border="red" , lwd = 1)

legend("topright",
       c('Heart Disease','No Heart Disease'),
       fill = c(rgb(0,0,1,1/4),"yellow"), 
       cex = 0.7)
legend("right",
       c('Heart Disease','No Heart Disease'),
       lty = c(1,1),
       col = c("blue","red"),
       cex = 0.7)

################################################################################
######################### Histogram of Mental Health ###########################
p5<-hist(yes$MentalHealth)

p6<-hist(no$MentalHealth)

plot( p6, col="yellow", freq = FALSE, xlab = "Mental Health", main = "Histogram of Mental Health")
plot( p5, col=rgb(0,0,1,1/4), freq = FALSE,  add=T)

line5 <- density(yes$MentalHealth)
polygon(line5, border="blue" , lwd = 1)

line6 <- density(no$MentalHealth)
polygon(line6, border="red" , lwd = 1)

legend("top",
       c('Heart Disease','No Heart Disease'),
       fill = c(rgb(0,0,1,1/4),"yellow"), 
       cex = 0.7)
legend("center",
       c('Heart Disease','No Heart Disease'),
       lty = c(1,1),
       col = c("blue","red"),
       cex = 0.7)

################################################################################
######################### Histogram of Sleep Time ##############################
p7<-hist(yes$SleepTime)

p8<-hist(no$SleepTime)

plot( p7, col=rgb(0,0,1,1/4), freq = FALSE, xlab = "Sleep Time", main = "Histogram of Sleep Time")
plot( p8, col="yellow", freq = FALSE,  add=T)

line7 <- density(yes$SleepTime)
polygon(line7, border="blue" , lwd = 1)

line8 <- density(no$SleepTime)
polygon(line8, border="red" , lwd = 1)

legend("topleft",
       c('Heart Disease','No Heart Disease'),
       fill = c(rgb(0,0,1,1/4),"yellow"), 
       cex = 0.7)
legend("left",
       c('Heart Disease','No Heart Disease'),
       lty = c(1,1),
       col = c("blue","red"),
       cex = 0.7)

################################################################################
########################### Heart Disease By Smoking ###########################
ggplot(hd_plot, 
       aes(x = factor(Smoking, levels = c("Yes","No")),
           fill = factor(HeartDisease, 
                         levels = c("Yes","No"),
                         labels = c("Yes", 
                                    "No")))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent",fill="Heart Disease",
       x = "Smoking",
       title = "Heart Disease By Smoking") +
  theme_minimal()

################################################################################
################## Heart Disease By Alcohol Drinking ###########################
ggplot(hd_plot, 
       aes(x = factor(AlcoholDrinking, levels = c("Yes","No")),
           fill = factor(HeartDisease, 
                         levels = c("Yes","No"),
                         labels = c("Yes", 
                                    "No")))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent",fill="Heart Disease",
       x = "Alcohol Drinking",
       title = "Heart Disease By Alcohol Drinking") +
  theme_minimal()

################################################################################
################## Heart Disease By Stroke #####################################
ggplot(hd_plot, 
       aes(x = factor(Stroke, levels = c("Yes","No")),
           fill = factor(HeartDisease, 
                         levels = c("Yes","No"),
                         labels = c("Yes", 
                                    "No")))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent",fill="Heart Disease",
       x = "Stroke",
       title = "Heart Disease By Stroke") +
  theme_minimal()

################################################################################
################## Heart Disease By Difficult Walking ###########################
ggplot(hd_plot, 
       aes(x = factor(DiffWalking, levels = c("Yes","No")),
           fill = factor(HeartDisease, 
                         levels = c("Yes","No"),
                         labels = c("Yes", 
                                    "No")))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent",fill="Heart Disease",
       x = "Difficult Walking",
       title = "Heart Disease By Difficult Walking") +
  theme_minimal()

################################################################################
################## Heart Disease By Age Category ###############################
ggplot(hd_plot, 
       aes(x = factor(AgeCategory, levels = c("18-24","25-29","30-34","35-39",
                                              "40-44","45-49","50-54","55-59",
                                              "60-64","65-69","70-74","75-79",
                                              "80 or older")),
           fill = factor(HeartDisease, 
                         levels = c("Yes","No"),
                         labels = c("Yes", 
                                    "No")))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent",fill="Heart Disease",
       x = "Age Category",
       title = "Heart Disease By Age Category") +
  theme_minimal()

################################################################################
######################### Heart Disease By Race ################################
ggplot(hd_plot, 
       aes(x = factor(Race, levels = c("American Indian/Alaskan Native","Asian",
                                       "Black","Hispanic","Other","White")),
           fill = factor(HeartDisease, 
                         levels = c("Yes","No"),
                         labels = c("Yes", 
                                    "No")))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent",fill="Heart Disease",
       x = "Race",
       title = "Heart Disease By Race") +
  theme_minimal()

################################################################################
######################### Heart Disease By Diabetic ############################
ggplot(hd_plot, 
       aes(x = factor(Diabetic, levels = c("Yes","Yes (during pregnancy)",
                                           "No","No, borderline diabetes")),
           fill = factor(HeartDisease, 
                         levels = c("Yes","No"),
                         labels = c("Yes", 
                                    "No")))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent",fill="Heart Disease",
       x = "Diabetic",
       title = "Heart Disease By Diabetic") +
  theme_minimal()

################################################################################
################## Heart Disease By Physical Activity ###########################
ggplot(hd_plot, 
       aes(x = factor(PhysicalActivity, levels = c("Yes","No")),
           fill = factor(HeartDisease, 
                         levels = c("Yes","No"),
                         labels = c("Yes", 
                                    "No")))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent",fill="Heart Disease",
       x = "Physical Activity",
       title = "Heart Disease By Physical Activity") +
  theme_minimal()

################################################################################
###################### Heart Disease By General Health #########################
ggplot(hd_plot, 
       aes(x = factor(GenHealth, levels = c("Poor","Fair","Good","Very good",
                                            "Excellent")),
           fill = factor(HeartDisease, 
                         levels = c("Yes","No"),
                         labels = c("Yes", 
                                    "No")))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent",fill="Heart Disease",
       x = "General Health",
       title = "Heart Disease By General Health") +
  theme_minimal()

################################################################################
##################### Heart Disease By Asthma ##################################
ggplot(hd_plot, 
       aes(x = factor(Asthma, levels = c("Yes","No")),
           fill = factor(HeartDisease, 
                         levels = c("Yes","No"),
                         labels = c("Yes", 
                                    "No")))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent",fill="Heart Disease",
       x = "Asthma",
       title = "Heart Disease By Asthma") +
  theme_minimal()

################################################################################
#################### Heart Disease By Kidney Disease ###########################
ggplot(hd_plot, 
       aes(x = factor(KidneyDisease, levels = c("Yes","No")),
           fill = factor(HeartDisease, 
                         levels = c("Yes","No"),
                         labels = c("Yes", 
                                    "No")))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent",fill="Heart Disease",
       x = "Kidney Disease",
       title = "Heart Disease By Kidney Disease") +
  theme_minimal()

################################################################################
####################### Heart Disease By Skin Cancer ###########################
ggplot(hd_plot, 
       aes(x = factor(SkinCancer, levels = c("Yes","No")),
           fill = factor(HeartDisease, 
                         levels = c("Yes","No"),
                         labels = c("Yes", 
                                    "No")))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Percent",fill="Heart Disease",
       x = "Skin Cancer",
       title = "Heart Disease By Skin Cancer") +
  theme_minimal()

################################################################################
# Box plots
# Plot Age Category against Heart Disease
ggboxplot(hd_new, x = "HeartDisease", y = "AgeCategory", 
          color = "HeartDisease", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("1", "0"),
          ylab = "AgeCategory", xlab = "HeartDisease")

# Mean plots
# Plot Age Category against Heart Disease
ggline(hd_new, x = "HeartDisease", y = "AgeCategory", 
       add = c("mean_se", "jitter"), 
       order = c("1", "0"),
       ylab = "AgeCategory", xlab = "HeartDisease")

#Plot count of heart disease against Age Category
hd_new %>% group_by(AgeCategory, HeartDisease) %>% summarise(count = n()) %>%
  ggplot() + geom_bar(aes(AgeCategory, count,   fill = as.factor(HeartDisease)), stat = "Identity") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ylab("Count") + xlab("AgeCategory") + labs(fill = "Heart Disease")

#Plot Sex against Age Category map with the condition of heart disease and Mental Health
options(repr.plot.width = 20, repr.plot.height = 8) 
hd_new %>% ggballoonplot(x = "AgeCategory", y = "Sex",
                     size = "MentalHealth", size.range = c(4, 20), fill = "HeartDisease",show.label = FALSE,
                     ggtheme = theme_bw()) +
  scale_fill_viridis_c(option = "C") + 
  theme(axis.text.x = element_text(angle = 90, size = 10)) +
  ggtitle("Sex against AgeCategory Map") + labs(fill = "Heart Disease")


##################################################################################################
###########################################Building models########################################
########################################Artificial Neural Network#################################
##################################################################################################

hd_new[2:18]<-scale(hd_new[2:18]) #Set the input variables into same scale
summary(hd_new) #All variables have a mean equal to 0

#Divide data into train and test datasets
set.seed(12345)
data<-sample(2,nrow(hd_new),replace=TRUE,prob=c(0.7,0.3))
train.data<-hd_new[data==1,]
test.data<-hd_new[data==2,]

ggplot(train.data, aes(x=factor(HeartDisease)))+geom_bar()+ggtitle("Class distribution in train set")
ggplot(test.data, aes(x=factor(HeartDisease)))+geom_bar()+ggtitle("Class distribution in test set")

###################Create first neural network model(1 hidden layer, 4 nodes)#############################
set.seed(12345)
#Implements neural network on training data
n<-names(hd_new[,2:18])
form<-as.formula(paste("HeartDisease~",paste(n[!n %in% "use"],collapse="+")))
nn1<-neuralnet(form,
              data=train.data,
              hidden=4, err.fct="ce", linear.output=FALSE)
summary(nn1)
nn1$response[1:20]
nn1$net.result[[1]][1:20]
nn1$result.matrix
plot(nn1)

#Compute the predicted values for training set
trainPred1<-neuralnet::compute(nn1,nn1$covariate)$net.result
trainPred1<-apply(trainPred1,c(1),round)
#Create confusion matrix on train data
confusionMatrix(table(trainPred1,train.data$HeartDisease,dnn=c("Predicted","Actual")))

#Compute the predicted values for testing set
testPred1<-neuralnet::compute(nn1,test.data[,1:18])$net.result
testPred1<-apply(testPred1,c(1),round)
#Create confusion matrix on test data
confusionMatrix(table(testPred1,test.data$HeartDisease,dnn=c("Predicted","Actual")))

###########Create second neural network model(2 hidden layer, 6 nodes)####################################
set.seed(12345)
#Implements neural network on training data
n<-names(hd_new[,2:18])
form<-as.formula(paste("HeartDisease~",paste(n[!n %in% "use"],collapse="+")))
nn2<-neuralnet(form,
              data=train.data,
              hidden=c(6,2), err.fct="ce", linear.output=FALSE)
nn2$net.result[[1]][1:20]
nn2$result.matrix
plot(nn2)

#Compute the predicted values for training set
trainPred2<-neuralnet::compute(nn2,nn2$covariate)$net.result
trainPred2<-apply(trainPred2,c(1),round)
#Create confusion matrix on train data
confusionMatrix(table(trainPred2,train.data$HeartDisease,dnn=c("Predicted","Actual")))

#Compute the predicted values for testing set
testPred2<-neuralnet::compute(nn2,test.data[,1:18])$net.result
testPred2<-apply(testPred2,c(1),round)
#Create confusion matrix on test data
confusionMatrix(table(testPred2,test.data$HeartDisease,dnn=c("Predicted","Actual")))

###########Create third neural network model(3 hidden layer, 6 nodes)####################################
set.seed(12345)
#Implements neural network on training data
n<-names(hd_new[,2:18])
form<-as.formula(paste("HeartDisease~",paste(n[!n %in% "use"],collapse="+")))
nn3<-neuralnet(form,
              data=train.data,
              hidden=c(6,3), err.fct="ce", linear.output=FALSE)
nn3$net.result[[1]][1:20]
nn3$result.matrix
plot(nn3)

#Compute the predicted values for training set
trainPred3<-neuralnet::compute(nn3,nn3$covariate)$net.result
trainPred3<-apply(trainPred3,c(1),round)
#Create confusion matrix on train data
confusionMatrix(table(trainPred3,train.data$HeartDisease,dnn=c("Predicted","Actual")))

#Compute the predicted values for testing set
testPred3<-neuralnet::compute(nn3,test.data[,1:18])$net.result
testPred3<-apply(testPred3,c(1),round)
#Create confusion matrix on test data
confusionMatrix(table(testPred3,test.data$HeartDisease,dnn=c("Predicted","Actual")))


##################################################################################################
###########################################Evaluation#############################################
#########################################Of The Models############################################
##################################################################################################
#############################################MSE##################################################
pr.nn1 <- compute(nn1,test.data[,1:18])
pr.nn1_ <- pr.nn1$net.result*(max(hd_new$HeartDisease)-min(hd_new$HeartDisease))+min(hd_new$HeartDisease)
test.r1 <- (test.data$HeartDisease)*(max(hd_new$HeartDisease)-min(hd_new$HeartDisease))+min(hd_new$HeartDisease)
MSE.nn1 <- sum((test.r1 - pr.nn1_)^2)/nrow(test.data)

pr.nn2 <- compute(nn2,test.data[,1:18])
pr.nn2_ <- pr.nn2$net.result*(max(hd_new$HeartDisease)-min(hd_new$HeartDisease))+min(hd_new$HeartDisease)
test.r2 <- (test.data$HeartDisease)*(max(hd_new$HeartDisease)-min(hd_new$HeartDisease))+min(hd_new$HeartDisease)
MSE.nn2 <- sum((test.r2 - pr.nn2_)^2)/nrow(test.data)

pr.nn3 <- compute(nn3,test.data[,1:18])
pr.nn3_ <- pr.nn3$net.result*(max(hd_new$HeartDisease)-min(hd_new$HeartDisease))+min(hd_new$HeartDisease)
test.r3 <- (test.data$HeartDisease)*(max(hd_new$HeartDisease)-min(hd_new$HeartDisease))+min(hd_new$HeartDisease)
MSE.nn3 <- sum((test.r3 - pr.nn3_)^2)/nrow(test.data)

print(paste(MSE.nn1,MSE.nn2,MSE.nn3))

############################################END###################################################





