############################################################################
#  1/4  -  IMPORTING & PREPARING THE DATASET
############################################################################

# Loading packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(data.table) 
library(gridExtra)
library(caret)
library(rpart)
library(randomForest)

# Importing the liver disease dataset and checking variable names
liver <- read.csv(file="https://storage.googleapis.com/kagglesdsdata/datasets/2607/4342/indian_liver_patient.csv?X-Goog-Algorithm=GOOG4-RSA-SHA256&X-Goog-Credential=gcp-kaggle-com%40kaggle-161607.iam.gserviceaccount.com%2F20201105%2Fauto%2Fstorage%2Fgoog4_request&X-Goog-Date=20201105T055539Z&X-Goog-Expires=259199&X-Goog-SignedHeaders=host&X-Goog-Signature=83668e7650a88fd8505bb90a57794a7da52c1bcebc3137b2ab8a39ac5e63825b88e87c941496b5198aaaf74a0f4ba930e7b159d0668500d8dbc1ff57af0ef79fe74aabc2bb04a43ebbd269d3ef9193598767a6450454c849daa023e6746828d667c304b84959befbf4100ef55372a103e3cd7495923dbd3c0aa4b4b8aef32c4b6291c49b802b137ce451417b7ab1c63c8de80e9a07219ee9e3e1a687cb7ec06c229e364705c69a764f41085a172a527b348dc4ee8747074fdec9e18dbc766ffc45d882e2c61234fa137f25518b95455a494dd6dca7fba69880ed51f1a99c37292d573ea63a20bf0aa809c6ee4e9eb66a47f544ddfe3f375d32dd5181e98ffeaa")
names(liver)

# Renaming variables based on medical shorthand notation
colnames(liver) <- c("age", "sex", "t_bilirubin", "d_bilirubin",
                     "ALP", "ALT", "AST", "protein", "albumin", "ag_ratio", "disease")
liver$sex[liver$sex=="Male"] <- "M"
liver$sex[liver$sex=="Female"] <- "F"

# Recoding 'disease' into '0' (no liver disease) and 1 (liver disease)
liver$disease[liver$disease==2] <- 0


############################################################################
#  2/4  -  EXPLORATORY DATA ANALYSIS (EDA)
############################################################################

# Overview of dataset
summary(liver)

# Binary variables
table(liver$sex)
table(liver$disease)

# Histograms of all other variables
graph1 <- ggplot(liver, aes(x=age)) +
  geom_histogram(binwidth=5, colour="black")

graph2 <- ggplot(liver, aes(x=t_bilirubin)) +
  geom_histogram(binwidth=1, colour="black")

graph3 <- ggplot(liver, aes(x=d_bilirubin)) +
  geom_histogram(binwidth=1, colour="black")

graph4 <- ggplot(liver, aes(x=ALP)) +
  geom_histogram(binwidth=20, colour="black")

graph5 <- ggplot(liver, aes(x=ALT)) +
  geom_histogram(binwidth=50, colour="black")

graph6 <- ggplot(liver, aes(x=AST)) +
  geom_histogram(binwidth=50, colour="black")

graph7 <- ggplot(liver, aes(x=protein)) +
  geom_histogram(binwidth=0.5, colour="black")

graph8 <- ggplot(liver, aes(x=albumin)) +
  geom_histogram(binwidth=0.5, colour="black")

graph9 <- ggplot(liver, aes(x=ag_ratio)) +
  geom_histogram(binwidth=0.2, colour="black")

grid.arrange(graph1, graph2, graph3, graph4, graph5, graph6, graph7,
             graph8, graph9, ncol=3)

# Creating a 'log' version of our dataset, in which skewed variables are log transformed
# +1 is applied to bilirubin variables, since these contain 0 values (which cannot be log-transformed)
liver_log <- liver
liver_log$t_bilirubin <- log(1+ liver_log$t_bilirubin)
liver_log$d_bilirubin <- log(1+ liver_log$d_bilirubin)
liver_log$ALP <- log(liver_log$ALP)
liver_log$ALT <- log(liver_log$ALT)
liver_log$AST <- log(liver_log$AST)
# Log data will not be explored further - but we will be training models with it later


# -------------------------------------------------------------------------
#  COMPARING VARIABLES BETWEEN DISEASED AND NON-DISEASED
# -------------------------------------------------------------------------


# Histograms of variables: diseased vs non-diseased
graph1 <- ggplot(liver, aes(x=age)) +
  geom_histogram(binwidth=5, colour="black") +
  facet_grid(.~disease) 

graph2 <- ggplot(liver, aes(x=t_bilirubin)) +
  geom_histogram(binwidth=0.2, colour="black") +
  facet_grid(.~disease) 

graph3 <- ggplot(liver, aes(x=d_bilirubin)) +
  geom_histogram(binwidth=0.2, colour="black") +
  facet_grid(.~disease) 

graph4 <- ggplot(liver, aes(x=ALP)) +
  geom_histogram(binwidth=0.5, colour="black") +
  facet_grid(.~disease) 

graph5 <- ggplot(liver, aes(x=ALT)) +
  geom_histogram(binwidth=0.5, colour="black") +
  facet_grid(.~disease) 

graph6 <- ggplot(liver, aes(x=AST)) +
  geom_histogram(binwidth=0.5, colour="black") +
  facet_grid(.~disease) 

graph7 <- ggplot(liver, aes(x=protein)) +
  geom_histogram(binwidth=0.5, colour="black") +
  facet_grid(.~disease) 

graph8 <- ggplot(liver, aes(x=albumin)) +
  geom_histogram(binwidth=0.5, colour="black") +
  facet_grid(.~disease) 

graph9 <- ggplot(liver, aes(x=ag_ratio)) +
  geom_histogram(binwidth=0.2, colour="black") +
  facet_grid(.~disease) 

grid.arrange(graph1, graph2, graph3, graph4, ncol=2)
grid.arrange(graph5, graph6, graph7, graph8, graph9, ncol=2)

# We now want to formally compare variables between groups.
# First, we check if variables are normally distributed
shapiro.test(liver$age)
shapiro.test(liver$t_bilirubin)
shapiro.test(liver$d_bilirubin)
shapiro.test(liver$ALP)
shapiro.test(liver$ALT)
shapiro.test(liver$AST)
shapiro.test(liver$protein)
shapiro.test(liver$albumin)
shapiro.test(liver$ag_ratio)
# Since p<0.05 for all, we conclude that variables are not normally distributed

# Secondly, we apply the appropriate comparative tests between groups
liverdisease <- liver %>% filter(disease==1)
livernodisease <- liver %>% filter(disease==0)

wilcox.test(liverdisease$age, livernodisease$age)
wilcox.test(liverdisease$t_bilirubin, livernodisease$t_bilirubin)
wilcox.test(liverdisease$d_bilirubin, livernodisease$d_bilirubin)
wilcox.test(liverdisease$ALP, livernodisease$ALP)
wilcox.test(liverdisease$ALT, livernodisease$ALT)
wilcox.test(liverdisease$AST, livernodisease$AST)
wilcox.test(liverdisease$protein, livernodisease$protein)
wilcox.test(liverdisease$albumin, livernodisease$albumin)
wilcox.test(liverdisease$ag_ratio, livernodisease$ag_ratio)

table(liverdisease$sex)
table(livernodisease$sex)
chisq.test(as.table(matrix(c(92, 324, 50, 117),ncol=2)))

# All significantly different except for protein (p=0.44) and sex (p=0.06)


############################################################################
#  3/4  -  GENERATING MODELS
############################################################################

# Partition into training set (80% of total) and test set (20% of total)
# Repeated for original dataset and for log-transformed dataset
set.seed(1, sample.kind="Rounding")

index <- createDataPartition(y = liver$disease, times = 1, p = 0.2, list = FALSE)
train <- liver[-index,]
test <- liver[index,]

index <- createDataPartition(y = liver_log$disease, times = 1, p = 0.2, list = FALSE)
train_log <- liver_log[-index,]
test_log <- liver_log[index,]


# -------------------------------------------------------------------------
#  LOGISTIC REGRESSION MODEL
# -------------------------------------------------------------------------


logistic <- glm(disease ~ age + sex + t_bilirubin + d_bilirubin + ALP + ALT + AST +
                  protein + albumin + ag_ratio, data=train, family=binomial)

summary(logistic)
exp(coef(logistic)) # Adjusted Odds ratios

# Log-transformed version 
logistic_log <- glm(disease ~ age + sex + t_bilirubin + d_bilirubin + ALP + ALT + AST +
                  protein + albumin + ag_ratio, data=train_log, family=binomial)


# -------------------------------------------------------------------------
#  CLASSIFICATION TREE
# -------------------------------------------------------------------------


ctree <- rpart(disease ~ age + sex + t_bilirubin + d_bilirubin + ALP + ALT + AST +
        protein + albumin + ag_ratio, data=train, method="class")

plot(ctree, margin=0.03)
text(ctree, cex=0.6)

ctree$variable.importance # Importance of variables

# Log-transformed version 
ctree_log <- rpart(disease ~ age + sex + t_bilirubin + d_bilirubin + ALP + ALT + AST +
                  protein + albumin + ag_ratio, data=train_log, method="class")


# -------------------------------------------------------------------------
#  RANDOM FOREST
# -------------------------------------------------------------------------


# The randomForest function rejects NA values, and hence we explicitly remove them
trainforest <- na.omit(train)

set.seed(1, sample.kind="Rounding")
rforest <- randomForest(disease ~ age + sex + t_bilirubin + d_bilirubin + ALP + ALT + AST +
                  protein + albumin + ag_ratio, data=trainforest)

varImpPlot(rforest)

# Log-transformed version 
trainforest_log <- na.omit(train_log)

set.seed(1, sample.kind="Rounding")
rforest_log <- randomForest(disease ~ age + sex + t_bilirubin + d_bilirubin + ALP + ALT + AST +
                 protein + albumin + ag_ratio, data=trainforest_log)



############################################################################
#  4/4  -  EVALUATING MODELS ON VALIDATION DATASET
############################################################################

# -------------------------------------------------------------------------
#  MAKING PREDICTIONS ON THE TEST SET
# -------------------------------------------------------------------------

# Logistic regression, original variables
prob1 <- logistic %>% predict(test, type = "response")
logisticpred <- ifelse(prob1 > 0.5, 1, 0)

# Logistic regression, log-transformed variables
prob1log <- logistic_log %>% predict(test_log, type = "response") 
logisticpred_log <- ifelse(prob1log > 0.5, 1, 0)

# Classification tree, original variables
prob2 <- ctree %>% predict(test) %>% as.matrix()
ctreepred <- ifelse(prob2[,1] < 0.5, 1, 0)

# Classification tree, log-transformed variables
prob2log <- ctree_log %>% predict(test_log) %>% as.matrix()
ctreepred_log <- ifelse(prob2log[,1] < 0.5, 1, 0)

# Random forest, original variables
prob3 <- rforest %>% predict(test)
rforestpred <- ifelse(prob3 > 0.5, 1, 0)

# Random forest, log-transformed variables
prob3log <- rforest_log %>% predict(test_log)
rforestpred_log <- ifelse(prob3log > 0.5, 1, 0)


# -------------------------------------------------------------------------
#  ASSESSING THE PERFORMANCE OF PREDICTIONS
# -------------------------------------------------------------------------

# Ensuring disease variable is in factor form (for confusionMatrix function)
test$disease <- as.factor(test$disease)
test_log$disease <- as.factor(test_log$disease)


### LOGISTIC REGRESSION MODELS -----
logisticpred <- as.factor(logisticpred)
logisticpred_log <- as.factor(logisticpred_log)

# Trained on original variables:
confusionMatrix(data=logisticpred, reference=test$disease, positive="1") 
# Trained on log-transformed variables:
confusionMatrix(data=logisticpred_log, reference=test_log$disease, positive="1")


### CLASSIFICATION TREES -----
ctreepred <- as.factor(ctreepred)
ctreepred_log <- as.factor(ctreepred_log)

# Trained on original variables:
confusionMatrix(data=ctreepred, reference=test$disease, positive="1")
# Trained on log-transformed variables:
confusionMatrix(data=ctreepred_log, reference=test_log$disease, positive="1") 


### RANDOM FORESTS -----
rforestpred <- as.factor(rforestpred)
rforestpred_log <- as.factor(rforestpred_log)

# Trained on original variables:
confusionMatrix(data=rforestpred, reference=test$disease, positive="1") 
# Trained on log-transformed variables:
confusionMatrix(data=rforestpred_log, reference=test_log$disease, positive="1") 
