library(readxl)
k2.blood <- read_excel("C:/Users/15189/Desktop/Jeannes data.xlsx")
attach(k2.blood)

#Turning all the attributes that are categorical in factors
# Hb in CVP: Hb in CVP (1 = Low; 2 = medium, 3 = high)
k2.blood$`Hb in CVP` <- as.factor(k2.blood$`Hb in CVP`)

# 1 is WT and 2 is K2
k2.blood$Genotype <- as.factor(k2.blood$Genotype) 



attach(k2.blood)

library(caret)
#setting seed so analysis can be repeated
#creating validation index that is 75% of the cleveland data
val.index <- createDataPartition(k2.blood$Genotype, p=0.5, list = FALSE)
#creating training data
blood.train <- k2.blood[val.index,]
#creating test data
blood.test <- k2.blood[-val.index,]


#Storing the actual HeartDisease results in actual.heart to compare to models
#predicitons
actual.blood <- blood.test$Genotype
actual.blood.train <- blood.train$Genotype

# Naive Bayes
#loading Naive Bayes library
library(e1071)
#creating Naive Bayes model
blood.naive <- naiveBayes(Genotype ~ BPM + `Height of SVP`, data = blood.train)
#predicitng using Naive Bayes model
predict.bayes <- predict(blood.naive, blood.test, type = "class")
#creating confusion matrix to get misclassification rate
table(predict.bayes,actual.blood)

# actual.blood
# predi  AB kcnh2a
# AB     11      0
# kcnh2a  1     12

# ------------------------


# logistic model
#creating logistic model
blood.logit <- glm(Genotype ~ BPM + `Height of SVP`, data = blood.train, family = binomial)

plot(Genotype ~ BPM)
plot <-ggplot(blood.test, aes(x=BPM, y=Genotype, color = Genotype)) + 
  geom_jitter(binaxis='y', stackdir='center', width = 0.1, height= 0.1)
plot
summary(blood.logit)
#making predicitons using logisistic model. This creates an object with probilites
#for the test individualsBPM 
predict.logit <- predict(blood.logit, blood.test , type ="response")
#creating an objext with the predicitons that are all NO
pred.blood <- rep("AB", length(predict.logit))
#Overwriting the NO's with Yes that had a probabilty above 0.5
pred.blood[predict.logit > 0.5] = "kcnh2a"
#creating a confusion matrix to compare and get miss class rate
actual <- k2.blood$Genotype
table(pred.blood , actual.blood)

# actual.blood
# pred AB kcnh2a
# No  12      2
# Yes  0     10



#--------------------
library(rpart)
library(rpart.plot)
#creating model using a decsion tree
blood.tree <- rpart(Genotype ~ BPM + `Height of SVP`, data = blood.train, 
                    method = "class", control=rpart.control(minsplit= 5, cp=.000001))
#finding the best CP value to use for pruning
tree.best.blood <- blood.tree$cptable[which.min(blood.tree$cptable[,"xerror"]),"CP"]
#pruning the tree back for better predictions
pruned.blood <- prune(blood.tree, cp=tree.best.blood)
#graphing the tree
prp(pruned.blood,
    faclen=0, 
    extra=1, 
    roundint=F, 
    digits=5)
#making prections with the tree model
pred.tree <- predict(pruned.blood, blood.test, "class")
#creating confusion matrix to find correct classification rate
table(pred.tree, actual.blood)

# actual.blood
# pred.tree AB kcnh2a
# AB     11      0
# kcnh2a  1     12


#===========================================================================















#Creating Binomial models for neutrophil data set
library(readxl)
neutrophil <- read_excel("C:/Users/15189/Desktop/Neutrophil Data/Master WT and kcn data for figures.xlsx")
neutrophil$Genotype <- as.factor(neutrophil$Genotype) 

attach(neutrophil)
library(caret)

#creating validation index that is 75% of the cleveland data
val.index <- createDataPartition(neutrophil$Genotype, p=0.75, list = FALSE)
#creating training data
neutrophil.train <- neutrophil[val.index,]
#creating test data
neutrophil.test <- neutrophil[-val.index,]

#Storing the actual HeartDisease results in actual.heart to compare to models
#predicitons
actual.neutrophil <- neutrophil.test$Genotype


# Naive Bayes
#loading Naive Bayes library
library(e1071)
#creating Naive Bayes model
neutrophil.naive <- naiveBayes(Genotype ~ `Neutrophil Area (%)`, data = neutrophil.train)
#predicitng using Naive Bayes model
predict.bayes <- predict(neutrophil.naive, neutrophil.test, type = "class")
#creating confusion matrix to get misclassification rate
table(predict.bayes,actual.neutrophil)

Ac# actual.neutrophil
# predict.bayes kcnh2a Wild-type
# kcnh2a        10         2
# Wild-type      3        10

# Missclass rate: 20%


# logistic model
#creating logistic model
neutrophil.logit <- glm(Genotype ~ `Neutrophil Area (%)`, data = neutrophil.train, family = binomial)
summary(neutrophil.logit)
#making predicitons using logisistic model. This creates an object with probilites
#for the test individuals
predict.logit <- predict(neutrophil.logit, neutrophil.test, type ="response")
#creating an objext with the predicitons that are all NO
pred.neutrophil <- rep("kcnh2a", length(predict.logit))
#Overwriting the NO's with Yes that had a probabilty above 0.5
pred.neutrophil[predict.logit > 0.5] = "WT"
#creating a confusion matrix to compare and get missclass rate
table(pred.neutrophil , actual.neutrophil)


# actual.neutrophil
# pred.neutrophil kcnh2a Wild-type
# kcnh2a     10         4
# WT          3         8

# Miss class rate 28%
#

#Looking at graph and plotting equation:
plot <-ggplot(neutrophil.train,aes(x=`Neutrophil Area (%)`, y=Genotype, color = Genotype)) + 
  geom_jitter(binaxis='y', stackdir='center', width = 0.05, height= 0.05) + 
  scale_color_manual(labels = c("Wild-type", expression(italic('kcnh2a')) ), values = c("coral", "cyan 3")) + theme_classic()
plot 

plot(`Neutrophil Area (%)`, Genotype, pch = 20 )
lines(Genotype ~ `Neutrophil Area (%)`, )

# CART
#loading library's to use for CART
library(rpart)
library(rpart.plot)

#creating model using a decsion tree
neutrophil.tree <- rpart(Genotype ~ `Neutrophil Area (%)`, data = neutrophil.train, 
                    method = "class", control=rpart.control(minsplit= 15, cp=.0000001))
#finding the best CP value to use for pruning
tree.best.neutrophil <- neutrophil.tree$cptable[which.min(neutrophil.tree$cptable[,"xerror"]),"CP"]
#pruning the tree back for better predictions
pruned.neutrophil <- prune(neutrophil.tree, cp=tree.best.neutrophil)
#graphing the tree
prp(pruned.neutrophil,
    faclen=0, 
    extra=1, 
    roundint=F, 
    digits=5)
#making prections with the tree model
pred.tree <- predict(pruned.neutrophil, neutrophil.test, "class")
#creating confusion matrix to find correct classification rate
table(pred.tree, actual.neutrophil)

# actual.neutrophil
# pred.tree   kcnh2a Wild-type
# kcnh2a        10         2
# Wild-type      3        10
#20% misscalss

library(e1071)
neutrophil.svm <- svm(formula = Genotype ~ `Neutrophil Area (%)` ,
                 data = neutrophil.train,
                 type = 'C-classification',
                 kernel = 'linear')
svm.pred <- predict(neutrophil.svm, neutrophil.test)
table(svm.pred, actual.neutrophil)
# actual.neutrophil
# svm.pred    kcnh2a Wild-type
# kcnh2a        10         3
# Wild-type      3         9
#miss class: 24%

#polynomial
neutrophil.svm <- svm(formula = Genotype ~ `Neutrophil Area (%)` ,
                      data = neutrophil.train,
                      type = 'C-classification',
                      kernel = 'polynomial')
svm.pred <- predict(neutrophil.svm, neutrophil.test)
table(svm.pred, actual.neutrophil)
# actual.neutrophil
# svm.pred    kcnh2a Wild-type
# kcnh2a        26        19
# Wild-type      0         5
# 38% missclass

#radial basis
neutrophil.svm <- svm(formula = Genotype ~ `Neutrophil Area (%)` ,
                      data = neutrophil.train,
                      type = 'C-classification',
                      kernel = 'radial')
svm.pred <- predict(neutrophil.svm, neutrophil.test)
table(svm.pred, actual.neutrophil)
# actual.neutrophil
# svm.pred    kcnh2a Wild-type
# kcnh2a        18         3
# Wild-type      8        21
#miss class: 22%

#sigmoid
neutrophil.svm <- svm(formula = Genotype ~ `Neutrophil Area (%)` ,
                      data = neutrophil.train,
                      type = 'C-classification',
                      kernel = 'sigmoid')
svm.pred <- predict(neutrophil.svm, neutrophil.test)
table(svm.pred, actual.neutrophil)
# actual.neutrophil
# svm.pred    kcnh2a Wild-type
# kcnh2a        21         7
# Wild-type      5        17

#miss class: 24%


#==========================================================================















# Predicitng New genotype uisng full model
#creating logistic model
neutrophil.logit <- glm(Genotype ~ `Neutrophil Area (%)`, data = neutrophil, family = binomial)
summary(neutrophil.logit)
#making predicitons using logisistic model. This creates an object with probilites
#for the test individuals

k2_mixed <- read_excel("Mixed kcnh2 + and kcnk5b/k2 mixed.xlsx")

attach(k2_mixed)

predict.logit <- predict(neutrophil.logit, k2_mixed, type ="response")
predict.logit
#creating an objext with the predicitons that are all NO
pred.neutrophil <- rep("kcnh2a", length(predict.logit))
#Overwriting the NO's with Yes that had a probabilty above 0.5 # Yes is WT
pred.neutrophil[predict.logit > 0.5] = "WT"
pred.neutrophil


# Predicitng new with cart
library(rpart)
library(rpart.plot)

#creating model using a decsion tree
neutrophil.tree <- rpart(Genotype ~ `Neutrophil Area (%)`, data = neutrophil, 
                         method = "class", control=rpart.control(minsplit= 5, cp=.0000001))
#finding the best CP value to use for pruning
tree.best.neutrophil <- neutrophil.tree$cptable[which.min(neutrophil.tree$cptable[,"xerror"]),"CP"]
#pruning the tree back for better predictions
pruned.neutrophil <- prune(neutrophil.tree, cp=tree.best.neutrophil)
#graphing the tree
prp(pruned.neutrophil,
    faclen=0, 
    extra=1, 
    roundint=F, 
    digits=5)
#making prections with the tree model
pred.tree <- predict(pruned.neutrophil, k2_mixed, "class")
pred.tree


# Naive Bayes
#loading Naive Bayes library
library(e1071)
#creating Naive Bayes model
neutrophil.naive <- naiveBayes(Genotype ~ `Neutrophil Area (%)`, data = neutrophil)
#predicitng using Naive Bayes model
predict.bayes <- predict(neutrophil.naive, k2_mixed, type = "class")
predict.bayes


#Using SVM
library(e1071)
neutrophil.svm <- svm(formula = Genotype ~ `Neutrophil Area (%)` ,
                      data = neutrophil,
                      type = 'C-classification',
                      kernel = 'linear')
svm.pred <- predict(neutrophil.svm, k2_mixed)
svm.pred
# actual.neutrophil
# svm.pred    kcnh2a Wild-type
# kcnh2a        22         7
# Wild-type      4        17
#miss class: 22%

#polynomial
neutrophil.svm <- svm(formula = Genotype ~ `Neutrophil Area (%)` ,
                      data = neutrophil,
                      type = 'C-classification',
                      kernel = 'polynomial')
svm.pred <- predict(neutrophil.svm, k2_mixed)
svm.pred
# actual.neutrophil
# svm.pred    kcnh2a Wild-type
# kcnh2a        26        19
# Wild-type      0         5
# 38% missclass

#radial basis
neutrophil.svm <- svm(formula = Genotype ~ `Neutrophil Area (%)` ,
                      data = neutrophil,
                      type = 'C-classification',
                      kernel = 'radial')
svm.pred <- predict(neutrophil.svm, k2_mixed)
svm.pred
# actual.neutrophil
# svm.pred    kcnh2a Wild-type
# kcnh2a        18         3
# Wild-type      8        21
#miss class: 22%

#sigmoid
neutrophil.svm <- svm(formula = Genotype ~ `Neutrophil Area (%)` ,
                      data = neutrophil,
                      type = 'C-classification',
                      kernel = 'sigmoid')
svm.pred <- predict(neutrophil.svm, k2_mixed)
svm.pred

