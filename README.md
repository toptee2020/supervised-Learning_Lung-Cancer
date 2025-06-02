# supervised-Learning_Lung-Cancer

Load the Require Library 
```{r, warning=FALSE, message=FALSE}
library(ggplot2)
library(dplyr)
library(car)
library(GGally)
library(tidyverse)
library(lme4)
library(MASS)
library(survey)
library(pscl)
library(effects)
library(caret)
library(ROCR)
```

Take the Data Into R 
```{r, message=FALSE, warning=FALSE}
Lung_cancer<-read.csv("survey lung cancer.csv", header = TRUE)
str(Lung_cancer)
```


#Data Preparation and Data Visualization
```{r, message=FALSE, warning=FALSE, fig.cap="A box plot showing Age distribution of Lung Cancer Patients"}
lung_cancer2<-Lung_cancer[,-c(1,2,3)] %>% mutate_if(is.numeric, as.factor)
lung_cancer3<-Lung_cancer[,c(1,2)] %>% mutate_if(is.character, as.factor)
Full_data<-data.frame(cbind(lung_cancer3, lung_cancer2, Lung_cancer$AGE))
head(Full_data)

#Visualize the Factors 
ggplot(Lung_cancer,aes(x=LUNG_CANCER, AGE, fill= LUNG_CANCER)) + geom_boxplot(outlier.color="blue", outlier.shape = 16,outlier.size=2,notch=FALSE) 

#descriptive statistics
summary(Full_data)
```

#FITTING THE MODEL (LOGISTICS REGRESSION MODEL)


```{r message=FALSE, warning=FALSE}
LUNG_CANCER<-Full_data$LUNG_CANCER
log_Model<-glm(LUNG_CANCER ~.,data = Full_data[,-1], family = "binomial")
summary(log_Model)
```


#Stepwise Model Selection
```{r, message=FALSE, warning=FALSE}
null<-glm(LUNG_CANCER~1,data=Full_data[,-1],family=binomial)
step(null,scope=list(lower=null,upper=log_Model),direction="both")

#pick the model with lowest AIC
Step_model <- stepAIC(log_Model, direction = "both", trace = FALSE)
summary(Step_model)
```

#Convert the coefficients to odds-ratios
```{r, message=FALSE, warning=FALSE}
exp(coef(Step_model))

#Confidence interval of odds-ratios

exp(cbind(OR=coef(Step_model),confint(Step_model)))
```


Anova Test to Determine Goodness of Fit
```{r, message=FALSE, warning=FALSE}
anova(Step_model,test="Chisq")
```


Wald Test to determine if predictors are significant:
```{r, message=FALSE, warning=FALSE}
regTermTest(Step_model,"SMOKING")
regTermTest(Step_model,"YELLOW_FINGERS")
regTermTest(Step_model,"PEER_PRESSURE")
regTermTest(Step_model,"CHRONIC.DISEASE")
regTermTest(Step_model,"FATIGUE")
regTermTest(Step_model,"ALLERGY")
regTermTest(Step_model,"ALCOHOL.CONSUMING")
regTermTest(Step_model,"COUGHING")
regTermTest(Step_model,"SWALLOWING.DIFFICULTY")
```

VIF for Collinearity
```{r, message=FALSE, warning=FALSE}
vif(Step_model)

#Determining the Pseudo-Rsq
pR2(Step_model)
```

Plotting the effects of SMOKING, PEER_PRESSURE, and OTHERS to predict survival of Lung Cancer
```{r, message=FALSE, warning=FALSE}
plot(allEffects(Step_model), 1, ylab = "Probability of Lung Cancer")
plot(allEffects(Step_model), 2, ylab = "Probability of Lung Cancer")
plot(allEffects(Step_model), 3, ylab = "Probability of Lung Cancer")
plot(allEffects(Step_model), 4, ylab = "Probability of Lung Cancer")
plot(allEffects(Step_model), 5, ylab = "Probability of Lung Cancer")
```

Cross Validation to obtain accuracy of model
```{r, message=FALSE, warning=FALSE}
Train<-createDataPartition(Full_data$LUNG_CANCER,p=0.8,list=FALSE)
training<-Full_data[Train,]
testing<-Full_data[-Train,]
```

PREDICT THE PROBABILITY (p) OF LUNG CANCER POSITIVITY 
```{r, message=FALSE, warning=FALSE}
log_Model2<-glm(training$LUNG_CANCER ~.,data = training[,-1], family = "binomial")
Step_model2 <- stepAIC(log_Model2, direction = "both", trace = FALSE)

p<- predict(Step_model2, newdata = testing[,-1], type = "response")
p_classes <- ifelse(p > 0.5, "pos","neg")
head(p_classes)

varImp(Step_model2)

pr<-prediction(p,testing$LUNG_CANCER)
prf<-performance(pr,measure="tpr",x.measure="fpr")
plot(prf)

#Area under Curve
auc<-performance(pr,measure="auc")
auc<-auc@y.values[[1]]
auc
```
