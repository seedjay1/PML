# following along with the Week 2 lecture

# set up the data
library(caret)
library(kernlab)
data(spam)
inTrain <- createDataPartition(y = spam$type
                               , p = .75
                               , list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]


set.seed(42)
folds <- createFolds(y = spam$type
                     , k = 10
                     , list = TRUE
                     , returnTrain = TRUE)
sapply(folds, length)

# only difference is returntrain
set.seed(42)
folds <- createFolds(y = spam$type
                     , k = 10
                     , list = TRUE
                     , returnTrain = FALSE)
sapply(folds, length)


# resample
set.seed(42)
folds <- createResample(y = spam$type
                        , times = 10
                        , list = TRUE)
sapply(folds, length)

############

# plotting predictors (wage data)

library(ISLR)
data(Wage)

inTrain <- createDataPartition(y = Wage$wage
                               , p = .7
                               , list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

# feature plot
fp <- featurePlot(x = training[,c("age", "education", "jobclass")]
            , y = training[, "wage"]
            , plot = "pairs")
print(fp)

# qplot
qp <- qplot(age, wage, data=training)
print(qp)

qp2 <- qplot(age, wage, color=jobclass, data=training)
print(qp2)

qp3 <- qplot(age, wage, color=education, data=training)
qp3 <- qp3 + geom_smooth(method="lm", formula=y~x)
print(qp3)


# make wage bins for separate analysis
library(Hmisc)
cutWage <- cut2(training$wage, g=3)
print(table(cutWage))

bp1 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot"))
print(bp1)

# NOW TRY THIS WITH A VIOLIN PLOT IN ORDER TO SEE THE POINTS
g <- ggplot(training, aes(x=cutWage, y=age, fill=cutWage)) + 
  theme(legend.position="none"
        , panel.background = element_rect(fill='grey')
        , plot.background = element_rect(fill='darkseagreen')
        , plot.title = element_text(hjust = 0.5)
  ) +
  ggtitle('Age Distribution By Wage Group') +
  labs(x="Wage Group", y="Age") +
  geom_violin(trim=TRUE) +
  scale_fill_brewer(palette="Blues") + 
  geom_boxplot(width=0.05) + 
  geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = .1)

print(g)
