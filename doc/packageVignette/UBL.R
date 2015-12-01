## ----setup, include=FALSE, cache=FALSE-----------------------------------
library(knitr)
## set global chunk options
opts_chunk$set(cache.path="cache/UBL-",dev="pdf",fig.path="figures/UBL-", fig.align="center", fig.show="hold",tidy=FALSE,size="footnotesize",message=FALSE)
options(replace.assign=TRUE,width=90)
##
##library(devtools)
##load_all("~/Software/R/MyPackages/UBLn")
library(uba)
library(UBL)
library(ggplot2)
library(reshape2)
library(xtable)
library(gridExtra)
library(plotrix)
library(performanceEstimation)

## ----message=FALSE, eval=FALSE-------------------------------------------
## library(devtools)  # You need to install this package!
## install_github("ltorgo/UBL",ref="master") # to install the stable release

## ----message=FALSE, eval=FALSE-------------------------------------------
## library(devtools)  # You need to install this package!
## install_github("ltorgo/UBL",ref="develop") # to install the development release

## ----message=FALSE, eval=FALSE-------------------------------------------
## library(devtools)
## # to install the development release
## install_git("https://github.com/ltorgo/UBL",branch="develop")
## # to install the stable release
## install_git("https://github.com/ltorgo/UBL",branch="master")

## ------------------------------------------------------------------------
library(UBL)

## ----iris_data-----------------------------------------------------------
library(UBL)  # Loading our infra-structure
library(e1071) # packge containing the svm we will use
data(iris)                      # The data set we are going to use
# transforming into a multiclass imbalanced problem
data <- iris[-c(91:125), c(1, 2, 5)] 
table(data$Species) 

## ----iris_svm_unbalanced-------------------------------------------------
set.seed(123)
samp <- sample(1:nrow(data), nrow(data)*0.7)
train <- data[samp,]
test <- data[-samp,]

model <- svm(Species~., train)
preds <- predict(model,test)
table(preds, test$Species) # confusion matrix

## ----iris_svm_unbalanced2------------------------------------------------
# change the examples in train by apllying the smote strategy
newtrain <- smoteClassif(Species~., train, C.perc="balance")

# generate a new model with the changed data
newmodel <- svm(Species~., newtrain)
preds <- predict(newmodel,test)
table(preds, test$Species)


## ----iris_svm_unbalanced3------------------------------------------------
# apply random under-sampling strategy
newtrain2 <- randOverClassif(Species~., train, C.perc="balance")

#generate a new model with the modified data set
newmodel2 <- svm(Species~., newtrain2)
preds <- predict(newmodel2, test)
table(preds, test$Species)


## ----iris_ru-------------------------------------------------------------
library(UBL)  # Loading our infra-structure
library(e1071) # packge containing the svm we will use
data(iris)                      # The data set we are going to use
# transforming into a multiclass imbalanced problem
data <- iris[-c(91:125), c(1, 2, 5)] 
# check the unbalanced data
table(data$Species)

## now, using random under-sampling to create a more 
## "balanced problem" automatically

newData <- randUnderClassif(Species ~ ., data)
table(newData$Species)

## ----Iris_RU1,fig.cap="The impact of random under-sampling strategy.",out.width="0.8\\textwidth", echo=FALSE----
  par(mfrow = c(1, 2))
  plot(data[, 1], data[, 2], pch = as.integer(data[, 3]), col=as.integer(data[,3]),
       main = "Original Data")
  plot(newData[, 1], newData[, 2], pch = as.integer(newData[,3]), col=as.integer(newData[,3]),
       main = "Under-sampled Data")

## ----another_iris_exRU---------------------------------------------------
  RUmy.ir <- randUnderClassif(Species~., data, list(setosa=0.3, versicolor=0.7))
  RUB.ir <- randUnderClassif(Species~., data, "balance")
  RUE.ir <- randUnderClassif(Species~., data, "extreme")


## ----RU_tab, echo=FALSE, results='asis'----------------------------------
nm <- c("Original","RUmy.ir", "RUB.ir","RUE.ir")

res <- c(table(data$Species),table(RUmy.ir$Species), table(RUB.ir$Species), table(RUE.ir$Species))

m <- matrix(res, nrow=4,ncol=3, byrow=TRUE, dimnames=list(nm, names(table(data$Species))))

xt <-xtable(m, caption="Number of examples in each class for different parameters of random under-sampling strategy.",label="tab:RU_tab")
print(xt,comment = FALSE, type = 'latex')


## ----Iris_RU2, fig.cap="Random Under-sampling strategy for different parameters values.", echo=FALSE----
DF <- rbind(data.frame(Dat="Original",obs=data$Species), data.frame(Dat="RUmy.ir", obs=RUmy.ir$Species), data.frame(Dat="RUB.ir", obs=RUB.ir$Species), data.frame(Dat="RUE.ir", obs=RUE.ir$Species))
g1 <- ggplot(DF,aes(x=obs, fill=Dat, colour=Dat))+geom_histogram(binwidth=1, position="dodge", aes(group=Dat), colour="black")

g2 <- ggplot(DF,aes(x=Dat, fill=obs, colour=obs))+geom_histogram(binwidth=1, position="fill", aes(group=obs), colour="black")

plots <- list(g1, g2)

do.call(grid.arrange, plots)

## ----iris_RO-------------------------------------------------------------
## now using random over-sampling to create a 
## data with more 600% of examples in the 
## virginica class
RO.U1<- randOverClassif(Species ~ ., data, 
                        C.perc=list(virginica=5))
RO.U2<- randOverClassif(Species ~ ., data, 
                        C.perc=list(versicolor=4, virginica=2.5))
RO.B <- randOverClassif(Species ~ ., data, C.perc="balance")
RO.E <- randOverClassif(Species ~ ., data, C.perc="extreme")

## ----RO_tab, echo=FALSE, results='asis'----------------------------------

nm <- c("Original","RO.U1", "RO.U2", "RO.B","RO.E")

res <- c(table(data$Species),table(RO.U1$Species), table(RO.U2$Species), table(RO.B$Species), table(RO.E$Species))

m <- matrix(res, nrow=5,ncol=3, byrow=TRUE, dimnames=list(nm, names(table(data$Species))))


xt <-xtable(m, caption="Number of examples in each class for different Random over-sampling parameters.",label="tab:RO_tab")
print(xt,comment = FALSE, type = 'latex')


## ----IrisRO,fig.cap="The impact of random over-sampling Strategy.",out.width="0.8\\textwidth", echo=FALSE----
  par(mfrow = c(2, 2))
  plot(data[, 1], data[, 2], pch =as.integer(data[, 3]), col=as.integer(data[,3]),
       main = "Original Data")
  plot(jitter(RO.U1[, 1]), jitter(RO.U1[, 2]), pch=as.integer(RO.U1[,3]), col=as.integer(RO.U1[,3]),
       main = "over-sampled Data with user defined parameter")
  plot(jitter(RO.B[, 1]), jitter(RO.B[, 2]), pch=as.integer(RO.B[,3]), col=as.integer(RO.B[,3]),
       main = "over-sampled Data with balance parameter")
  plot(jitter(RO.E[, 1]), jitter(RO.E[, 2]), pch=as.integer(RO.E[,3]), col=as.integer(RO.E[,3]),
       main = "over-sampled Data with extreme parameter")

## ----Iris_RO2, fig.cap="Impact of Random over-sampling strategy for different parameters values.", echo=FALSE----
DF <- rbind(data.frame(Dat="Original",obs=data$Species), data.frame(Dat="RO.U1", obs=RO.U1$Species), data.frame(Dat="RO.B", obs=RO.B$Species), data.frame(Dat="RO.E", obs=RO.E$Species))
g1 <- ggplot(DF,aes(x=obs, fill=Dat, colour=Dat))+geom_histogram(binwidth=1, position="dodge", aes(group=Dat), colour="black")

g2 <- ggplot(DF,aes(x=Dat, fill=obs, colour=obs))+geom_histogram(binwidth=1, position="fill", aes(group=obs), colour="black")

plots <- list(g1, g2)

do.call(grid.arrange, plots)

## ----iris_TL-------------------------------------------------------------
# using the default in all parameters
  ir <- TomekClassif(Species~., data)
# using chebyshev distance metric, and selecting only two classes to under-sample
  irCheb <- TomekClassif(Species~., data, dist="Chebyshev", 
                         Cl=c("virginica", "setosa"))
# using Manhattan distance, enable the removal of examples from all classes, and
# select to break the link by only removing the example from the majority class
  irManM <- TomekClassif(Species~., data, dist="Manhattan", Cl="all", rem="maj") 
  irManB <- TomekClassif(Species~., data, dist="Manhattan", Cl="all", rem="both")

# check the new irCheb data set
summary(irCheb[[1]])

# check the indexes of the examples removed:
irCheb[[2]]

## ----TL_table, echo=FALSE, results='asis'--------------------------------

nm <- c("Original","ir", "irCheb", "irManM","irManB")

res <- c(table(data$Species),table(ir[[1]]$Species), table(irCheb[[1]]$Species), table(irManM[[1]]$Species), table(irManB[[1]]$Species))

m <- matrix(res, nrow=5,ncol=3, byrow=TRUE, dimnames=list(nm, names(table(data$Species))))

xt <-xtable(m, caption="Number of examples in each class for different Tomek Links parameters.",label="tab:TL_table")
print(xt,comment = FALSE, type = 'latex')


## ----TL_difPar, fig.cap="Impact of Tomek links strategy in versicolor and virginica classes.", echo=FALSE----
# visualizing the two classes that have changed (virginica and versicolor)
dO <- subset(data, Species!="setosa")
dC <- subset(irCheb[[1]], Species!="setosa")
dM <- subset(irManM[[1]], Species!="setosa")
dB <- subset(irManB[[1]], Species!="setosa")

par(mfrow = c(2, 2))
plot(dO[,1], dO[,2], col=as.integer(dO[,3]), main="original data")
plot(dC[,1], dC[,2], col=as.integer(dC[,3]), main="Tomek links with Chebyshev distance")
plot(dM[,1], dM[,2], col=as.integer(dM[,3]), main="Tomek links removing majority class")
plot(dB[,1], dB[,2], col=as.integer(dB[,3]), main="Tomek links removing both examples")


## ----Iris_CNN------------------------------------------------------------
set.seed(123)
  myCNN <- CNNClassif(Species~., data, Cl=c("setosa", "virginica"))
  CNN1 <- CNNClassif(Species~., data, Cl="smaller")
  CNN2 <- CNNClassif(Species~., data, Cl="versicolor")
  CNN3 <- CNNClassif(Species~., data, dist="Chebyshev", Cl="virginica")

# check the new data set obtained in CNN1
summary(CNN1[[1]]$Species)

# check the classes which were considered important
CNN1[[2]]

# check the classes which were considered unimportant
CNN1[[3]]


## ----CNN_table, echo=FALSE, results='asis'-------------------------------

nm <- c("Original","myCNN", "CNN1", "CNN2","CNN3")

res <- c(table(data$Species),table(myCNN[[1]]$Species), table(CNN1[[1]]$Species), table(CNN2[[1]]$Species), table(CNN3[[1]]$Species))

m <- matrix(res, nrow=5,ncol=3, byrow=TRUE, dimnames=list(nm, names(table(data$Species))))


xt <-xtable(m, caption="Number of examples in each class for different CNN parameters.",label="tab:CNN_table")
print(xt,comment = FALSE, type = 'latex')



## ----CNN_plot, echo=FALSE, fig.cap="Impact of CNN method for different values of parameter Cl."----
par(mfrow=c(2,2))
plot(data[,1], data[,2], col=as.integer(data[,3]), main="original data")
plot(myCNN[[1]][,1], myCNN[[1]][,2], col=as.integer(myCNN[[1]][,3]), main="setosa and virginica are the  important classes")
plot(CNN1[[1]][,1], CNN1[[1]][,2], col=as.integer(CNN1[[1]][,3]), main="Smaller classes automatically estimated")
plot(CNN2[[1]][,1], CNN2[[1]][,2], col=as.integer(CNN2[[1]][,3]), main="versicolor is the most important class")

## ----Iris_OSS------------------------------------------------------------
set.seed(1234)

ir2 <- OSSClassif(Species~., data, dist="p-norm", p=3, Cl="virginica")
ir3 <- OSSClassif(Species~., data, Cl=c("versicolor", "virginica"), start="Tomek")
ir4 <- OSSClassif(Species~., data)

summary(ir2$Species)
summary(ir3$Species)
summary(ir4$Species)


## ----oss_table, echo=FALSE, results='asis'-------------------------------
nm <- c("Original","ir2", "ir3","ir4")

res <- c(table(data$Species),table(ir2$Species), table(ir3$Species), table(ir4$Species))

m <- matrix(res, nrow=4, ncol=3, byrow=TRUE, dimnames=list(nm, names(table(data$Species))))

xt <-xtable(m, caption="Number of examples in each class for different OSS parameters.",label="tab:oss_table")
print(xt,comment = FALSE, type = 'latex')


## ----OSS_plot, fig.cap="OSS techniques applied to a multiclass imbalanced problem.", echo=FALSE----
par(mfrow=c(2,2))
plot(data[,1], data[,2], col=as.integer(data[,3]), main="original data")
plot(ir2[,1], ir2[,2], col=as.integer(ir2[,3]), main="virginica is the most important class")
plot(ir3[,1], ir3[,2], col=as.integer(ir3[,3]), main="versicolor and virginica are the important classes. First apply Tomek links")
plot(ir4[,1], ir4[,2], col=as.integer(ir4[,3]), main="default values applied")

## ----Iris_ENN------------------------------------------------------------
  set.seed(123)
  Man5 <- ENNClassif(Species~., data, k=5, dist="Manhattan", Cl="all") 
  Default <- ENNClassif(Species~., data)
  ChebSub7 <- ENNClassif(Species~., data, k=7, dist="Chebyshev", 
                         Cl=c("virginica", "setosa"))
  ChebAll7 <- ENNClassif(Species~., data, k=7, dist="Chebyshev")
  HVDM3 <- ENNClassif(Species~., data, k=3, dist="HVDM")

## ----iris_ENN_table, results='asis', echo=FALSE--------------------------
nm <- c("Original","Man5", "Default","ChebSub7", "ChebAll7", "HVDM3")

res <- c(table(data$Species),table(Man5[[1]]$Species), table(Default[[1]]$Species), table(ChebSub7[[1]]$Species), table(ChebAll7[[1]]$Species), table(HVDM3[[1]]$Species))

m <- matrix(res, nrow=6,ncol=3, byrow=TRUE, dimnames=list(nm, names(table(Man5[[1]]$Species))))

xt <-xtable(m, caption="Number of examples in each class for different parameters of ENN strategy.",label="tab:iris_ENN_table")
print(xt,comment = FALSE, type = 'latex')

## ----ir_ENN_plot, echo=FALSE, fig.cap="Impact in the Original data set of several parameters for ENN strategy",out.height="0.5\\textheight"----
DF <- rbind(data.frame(Dat="Original",obs=as.factor(data$Species)), data.frame(Dat="Manhattan5NN", obs=as.factor(Man5[[1]]$Species)), data.frame(Dat="Default", obs=as.factor(Default[[1]]$Species)), data.frame(Dat="Chebyshev7NNSub", obs=as.factor(ChebSub7[[1]]$Species)), data.frame(Dat="Chebyshev7NNAll", obs=as.factor(ChebAll7[[1]]$Species)), data.frame(Dat="HVDM3", obs=as.factor(HVDM3[[1]]$Species)))

g1 <- ggplot(DF,aes(x=obs, fill=Dat, colour=Dat))+geom_histogram(binwidth=1, position="dodge", aes(group=Dat), colour="black")


g2 <- ggplot(DF,aes(x=Dat, fill=obs, colour=obs))+geom_histogram(binwidth=1, position="fill", aes(group=obs), colour="black")+theme(axis.text.x  = element_text(angle=90, vjust=0.5))

plots <- list(g1, g2)

do.call(grid.arrange, plots)


## ----ENN_cats------------------------------------------------------------
library(MASS)
data(cats)
# check the data set
summary(cats$Sex)

# Change the data set using ENN strategy
newdata1 <- ENNClassif(Sex~., cats)
newdata2 <- ENNClassif(Sex~., cats, Cl="M")
# check the number of examples in each class
summary(newdata1[[1]]$Sex)
summary(newdata2[[1]]$Sex)

# check visually the examples distribution
g <-ggplot(cats, aes(Bwt, Hwt, col=Sex))+geom_point()+ggtitle("Original data set")

# check visually the impact of the strategies
g1 <-ggplot(newdata1[[1]], aes(Bwt, Hwt, col=Sex))+
     geom_point()+ggtitle("First modified data set")
g2 <-ggplot(newdata2[[1]], aes(Bwt, Hwt, col=Sex))+
     geom_point()+ggtitle("Second modified data set")

do.call(grid.arrange, list(g,g1,g2))


## ----Iris_NCL------------------------------------------------------------
set.seed(1234)
ir.M1 <- NCLClassif(Species~., data, k=3, dist="p-norm", p=1, Cl="smaller")
ir.M2<- NCLClassif(Species~., data, k=1, dist="p-norm", p=1, Cl="smaller")
ir.Def <- NCLClassif(Species~., data)
ir.Ch <- NCLClassif(Species~., data, k=7, dist="Chebyshev", Cl="virginica")
ir.Eu <- NCLClassif(Species~., data, k=3, dist="Euclidean", 
                    Cl=c("setosa", "virginica"))

## ----iris_NCL_table, results='asis', echo=FALSE--------------------------
nm <- c("Original","ir.M1", "ir.M2","ir.Def", "ir.Ch", "ir.Eu")

res <- c(table(data$Species),table(ir.M1$Species), table(ir.M2$Species), table(ir.Def$Species), table(ir.Ch$Species), table(ir.Eu$Species))

m <- matrix(res, nrow=6, ncol=3, byrow=TRUE, dimnames=list(nm, names(table(Man5[[1]]$Species))))

xt <-xtable(m, caption="Number of examples in each class for different parameters of NCL strategy.",label="tab:iris_NCL_table")
print(xt,comment = FALSE, type = 'latex')

## ----NCL_plot,fig.cap="NCL techniques applied to a multiclass imbalanced problem.", echo=FALSE----
g1 <- ggplot(data, aes(x=data[,1], y=data[,2],color=data[,3]))+xlab("Length")+ylab("Width")+geom_point()+ggtitle("Original data")+scale_color_discrete(name="Species")
g2 <- ggplot(ir.M1, aes(x=ir.M1[,1], y=ir.M1[,2],color=ir.M1[,3]))+xlab("Length")+ylab("Width")+geom_point()+ggtitle("Manhattan distance and 3-NN")+scale_color_discrete(name="Species")
g3 <- ggplot(ir.M2, aes(x=ir.M2[,1], y=ir.M2[,2],color=ir.M2[,3]))+xlab("Length")+ylab("Width")+geom_point()+ggtitle("Manhattan distance and 1-NN")+scale_color_discrete(name="Species")
g4 <- ggplot(ir.Def, aes(x=ir.Def[,1], y=ir.Def[,2],color=ir.Def[,3]))+xlab("Length")+ylab("Width")+geom_point()+ggtitle("Default values")+scale_color_discrete(name="Species")
g5 <- ggplot(ir.Ch, aes(x=ir.Ch[,1], y=ir.Ch[,2],color=ir.Ch[,3]))+xlab("Length")+ylab("Width")+geom_point()+ggtitle("Chebyshev dist., 7-NN")+scale_color_discrete(name="Species")
g6 <- ggplot(ir.Eu, aes(x=ir.Eu[,1], y=ir.Eu[,2],color=ir.Eu[,3]))+xlab("Length")+ylab("Width")+geom_point()+ggtitle("Euclidean distance, 3-NN")+scale_color_discrete(name="Species")

plots <- list(g1, g2, g3, g4, g5, g6)

do.call(grid.arrange, plots)

## ----ir_GN---------------------------------------------------------------
set.seed(1234)
irB<- gaussNoiseClassif(Species~., data, C.perc="balance")
irE <- gaussNoiseClassif(Species~., data,C.perc="extreme")
irU1 <- gaussNoiseClassif(Species~., data,
                          C.perc=list(setosa=0.3, versicolor=1.5, virginica=4),
                          pert=0.5, repl=TRUE)
irU2 <- gaussNoiseClassif(Species~., data,
                          C.perc=list(versicolor=3, virginica=2), 
                          pert=0.05)

## ----iris_GN_table, results='asis',echo=FALSE----------------------------
nm <- c("Original", "irB", "irE","irU1", "irU2")

nm2 <- list(data, irB, irE,irU1, irU2)

res <- c(table(data$Species), table(irB$Species), table(irE$Species), table(irU1$Species), table(irU2$Species))

m <- matrix(res, nrow=5,ncol=3, byrow=TRUE, dimnames=list(nm, names(table(data$Species))))

xt <-xtable(m, caption="Number of examples in each class for different parameters of Gaussian Noise strategy.",label="tab:iris_GN_table")
print(xt,comment = FALSE, type = 'latex')


## ----ir_GN_plot, echo=FALSE, fig.cap="Impact in the Original data set of several parameters in Gaussian noise strategy. ",out.height="0.5\\textheight"----
DF <- rbind(data.frame(Dat="Original",obs=data$Species), data.frame(Dat="irB", obs=irB$Species), data.frame(Dat="irE", obs=irE$Species), data.frame(Dat="irU1", obs=irU1$Species), data.frame(Dat="irU2", obs=irU2$Species))
g1 <- ggplot(DF,aes(x=obs, fill=Dat, colour=Dat))+geom_histogram(binwidth=1, position="dodge", aes(group=Dat), colour="black")

g2 <- ggplot(DF,aes(x=Dat, fill=obs, colour=obs))+geom_histogram(binwidth=1, position="fill", aes(group=obs), colour="black")

plots <- list(g1, g2)

do.call(grid.arrange, plots)


## ----ir_GN_plot2, echo=FALSE, fig.cap="The examples distribution for different parameters in Gaussian Noise strategy."----
  par(mfrow = c(2, 2))
  plot(data[, 1], data[, 2], pch =as.integer(data[, 3]), col=as.integer(data[,3]),
       main = "Original Data")
  plot(irB[,1], irB[,2], pch=as.integer(irB[,3]), col=as.integer(irB[,3]),
       main = "Balanced Data")
  plot(irE[,1], irE[,2], pch=as.integer(irE[,3]), col=as.integer(irE[,3]),
       main = "Data with Inverted Frequencies")
  plot(irU1[,1], irU1[,2], pch=as.integer(irU1[,3]), col=as.integer(irU1[,3]),
       main = "Data with User Defined Percentages")

## ----smote_illust, echo=FALSE, fig.cap="Generation of synthetic examples through Smote algorithm.\\label{smote_illust}", out.height="0.4\\textheight", out.width="0.7\\textwidth"----
#generate an artificial data set to illustrate the smote algorithm
set.seed(234)
plot(1:40, rnorm(40,2),pch="-", col="blue",xlab="", ylab="", xaxt="n", yaxt="n")
points(1:40, rnorm(40,2), pch="-", col="blue")
points(1:40, rnorm(40,2), pch="-", col="blue")
set.seed(123)
points(c(10:32, 17, 17,19), c(rnorm(24,-0.3,0.5),0,0.5), pch="+", col="red")
lines(c(17,19), c(0,0.5), col="orange", lwd=2)
draw.circle(19,0.5,4.7,nv=100,border=NULL,col=NA,lty=3,lwd=1)
points(17.6, 0.25*17.6-4.25, pch="+")

## ----smote---------------------------------------------------------------
  mysmote1 <- smoteClassif(Species~., data, 
                           C.perc=list(setosa=0.6, virginica=1.5))
  mysmote2 <- smoteClassif(Species~., data, 
                           C.perc=list(setosa=0.2, versicolor=4), repl=TRUE)
  mysmote3 <- smoteClassif(Species~., data, 
                           C.perc=list(virginica=6, versicolor=2))
  smoteB <- smoteClassif(Species~., data, 
                           C.perc="balance")
  smoteE <- smoteClassif(Species~., data, 
                           C.perc="extreme")


## ----smote_table, echo=FALSE, results='asis'-----------------------------
nm <- c("Original", "mysmote1", "mysmote2","mysmote3", "smoteB", "smoteE")

res <- c(table(data$Species), table(mysmote1$Species), table(mysmote2$Species), table(mysmote3$Species), table(smoteB$Species), table(smoteE$Species))

m <- matrix(res, nrow=6,ncol=3, byrow=TRUE, dimnames=list(nm, names(table(data$Species))))

xt <-xtable(m, caption="Number of examples in each class for different parameters of smote strategy.",label="tab:iris_smote_table")
print(xt,comment = FALSE, type = 'latex')


## ----smote_plot_hist, echo=FALSE, fig.cap="Impact in the Original data set of several parameters in smote strategy. ",out.height="0.5\\textheight"----
DF <- rbind(data.frame(Dat="Original",obs=data$Species), data.frame(Dat="mysmote1", obs=mysmote1$Species), data.frame(Dat="mysmote2", obs=mysmote2$Species), data.frame(Dat="mysmote3", obs=mysmote3$Species), data.frame(Dat="smoteB", obs=smoteB$Species), data.frame(Dat="smoteE", obs=smoteE$Species))
g1 <- ggplot(DF,aes(x=obs, fill=Dat, colour=Dat))+geom_histogram(binwidth=1, position="dodge", aes(group=Dat), colour="black")


g2 <- ggplot(DF,aes(x=Dat, fill=obs, colour=obs))+geom_histogram(binwidth=1, position="fill", aes(group=obs), colour="black")

plots <- list(g1, g2)

do.call(grid.arrange, plots)

## ----smote_plot, fig.cap="Smote strategy applied with different parameters", echo=FALSE----
par(mfrow = c(3, 2))
plot(data[, 1], data[, 2], pch = as.integer(data[, 3]), col=as.integer(data[,3]),
        main = "Original Data")
plot(mysmote1[, 1], mysmote1[, 2], pch = as.integer(mysmote1[,3]), col=as.integer(mysmote1[,3]),
        main = "User settings1")
plot(mysmote2[, 1], mysmote2[, 2], pch = as.integer(mysmote2[,3]), col=as.integer(mysmote2[,3]),
        main = "User settings2")
plot(mysmote3[, 1], mysmote3[, 2], pch = as.integer(mysmote3[,3]), col=as.integer(mysmote3[,3]),
        main = "User settings3")
plot(smoteB[, 1], smoteB[, 2], pch = as.integer(smoteB[,3]), col=as.integer(smoteB[,3]),
        main = "Balanced Data")
plot(smoteE[, 1], smoteE[, 2], pch = as.integer(smoteE[,3]), col=as.integer(smoteE[,3]),
        main = "Extreme ")


## ----relev_ex, fig.cap="Example of a relevance function", echo=FALSE-----
  y <- 0:10

   rel <- matrix(0,ncol=3,nrow=0)
   rel <- rbind(rel,c(0,1,0))
   rel <- rbind(rel,c(3,0,0))
   rel <- rbind(rel,c(6,1,0))
   rel <- rbind(rel,c(7,0.5,1))
   rel <- rbind(rel,c(10,0,0))
phiF.args <- phi.control(y,method="range",control.pts=rel)
#y.phi <- phi(y,phi.parms=phiF.args)


yrange <- range(y)
yplot <- seq(yrange[1],yrange[2],len=100)
yplot.phi <- phi(yplot,phi.parms=phiF.args)

plot(yplot,yplot.phi,type="l",
     ylab=expression(phi(y)),xlab=expression(y))
abline(h=0.5, lty=2, col=4)

text(x=9, y=0.52, labels="relevance threshold", cex=0.8, col=4)
mtext("0.5",side=2,col=4)
points(1.5,0.5, col=4)
points(4.5,0.5, col=4)
points(7,0.5, col=4)
#abline(h=-0.01,lwd=2, col=2)
lines(c(-0.2,1.5), c(-0.01,-0.01), col="green", lwd=3)
lines(c(1.5,4.5), c(-0.01,-0.01), col="red", lwd=3)
lines(c(4.5,7), c(-0.01,-0.01), col="green", lwd=3)
lines(c(7,10.2), c(-0.01,-0.01), col="red", lwd=3)

lines(c(1.5,1.5), c(0.5,0), col=4, lty=2)
lines(c(4.5,4.5), c(0.5,0), col=4, lty=2)
lines(c(7,7), c(0.5,0), col=4, lty=2)


## ----method_range--------------------------------------------------------
# relevance function represented in the previous example

## method: range
# the user should provide a matrix with y, phi(y), phi'(y)

rel <- matrix(0,ncol=3,nrow=0)

# for the target value of zero the relevance function should be one and
# the derivative at that point should be zero
rel <- rbind(rel,c(0,1,0)) 

# for the value three the relevance assigned is zero and the derivative is zero
rel <- rbind(rel,c(3,0,0))
rel <- rbind(rel,c(6,1,0))
rel <- rbind(rel,c(7,0.5,1))
rel <- rbind(rel,c(10,0,0))
# after defining the relevance function the user may obtain the 
# phi values as follows:

# use method "range" when defining a matrix
phiF.args <- phi.control(y,method="range",control.pts=rel)

# obtain the relevance values for the target variable y
y.phi <- phi(y,phi.parms=phiF.args)


## ----method_extremes-----------------------------------------------------

## method: extremes

## for considering only the high extremes
phiF.args <- phi.control(y,method="extremes",extr.type="high")
y.phi <- phi(y,phi.parms=phiF.args)

## for considering only the low extremes
phiF.args <- phi.control(y,method="extremes",extr.type="low")
y.phi <- phi(y,phi.parms=phiF.args)

## for considering both extreme types (low and high)
phiF.args <- phi.control(y,method="extremes",extr.type="both")
y.phi <- phi(y,phi.parms=phiF.args)



## ----dat-----------------------------------------------------------------
# use algae data set with NA's removed
library(DMwR)
data(algae)
clean.algae <- algae[complete.cases(algae),]

# We start by using the automatic method for the relevance function
# Since this is the default behaviour, we can simply not mention the
# "rel" parameter

algB <- randUnderRegress(a7~., clean.algae, C.perc="balance")
algE <- randUnderRegress(a7~., clean.algae, C.perc="extreme")

# the automatic method for the relevance function provides only one bump 
# with values to be under-sampled, thus we only need to indicate one percentage
algMy <- randUnderRegress(a7~., clean.algae, C.perc=list(0.5))


## ----RU_ex1, fig.cap="Relevance function and density of the target variable in the original and new data sets using Random Under-sampling strategy", echo=FALSE----
par(mar = c(5,5,2,5))
plot(density(clean.algae$a7), xlab=expression(y), main="")
lines(density(algB$a7), col=3)
lines(density(algE$a7), col=4)
lines(density(algMy$a7), col=6)

y <- sort(resp(a7~.,clean.algae))

pc <- phi.control(y, method="extremes")
y.relev <- phi(y,pc)

par(new=TRUE)
plot(y,y.relev, lty=2, col=2, lwd=3, axes=F, ylab=NA, xlab=NA,type="l")
axis(4, xaxt="n",col =2, col.axis =2 , lwd = 2)
mtext(expression(phi(y)),4, col=2, line=3)
legend(22,0.9, c("clean.algae", "algB", "algE", "algMy",expression(phi())), col=c(1,3,4,6,2), lty=c(1,1,1,1,2), lwd=c(1,1,1,1,2), bty="n", text.col=c(1,1,1,1,2))

## ----rel_RU--------------------------------------------------------------

rel <- matrix(0,ncol=3,nrow=0)

# add zero relevance for the target values before five
rel <- rbind(rel, c(0,0,0))
rel <- rbind(rel,c(4,0,0))

# add maximum relevance for the target values close to 5
rel <- rbind(rel, c(5,1,0))

# add some unimportant target values 

rel <- rbind(rel, c(7,0,0))
rel <- rbind(rel, c(18,0,0))

# add maximum relevance to points close to and above 20

rel <- rbind(rel, c(20,1,0))
rel <- rbind(rel, c(30,1,0))


## ----RRU-----------------------------------------------------------------
RUnew <- randUnderRegress(a7~., clean.algae, rel=rel, thr.rel=0.7,
                          C.perc=list(0.2,0.8))
RUB2 <- randUnderRegress(a7~., clean.algae, rel=rel, thr.rel=0.7, 
                         C.perc="balance")
RUE2 <- randUnderRegress(a7~., clean.algae, rel=rel, thr.rel=0.7, 
                         C.perc="extreme")

## ----RU_ex2, fig.cap="Relevance function and density of the target variable in the original and new data sets with Random Under-sampling strategy.", echo=FALSE----
par(mar = c(5,5,2,5))
plot(density(clean.algae$a7),xlab=expression(y), main="")
lines(density(RUnew$a7), col=3)
lines(density(RUB2$a7), col=4)
lines(density(RUE2$a7), col=6)

y <- sort(resp(a7~.,clean.algae))

pc <- phi.control(y, method="range", control.pts=rel)
y.relev <- phi(y,pc)

par(new=TRUE)
plot(y,y.relev, lty=2, col=2, lwd=3, axes=F, ylab=NA, xlab=NA,type="l")
axis(4, xaxt="n",col =2, col.axis =2 , lwd = 2)
mtext(expression(phi(y)),4, col=2, line=3)
legend(22,0.9, c("clean.algae", "RUnew", "RUB2", "RUE2",expression(phi())), col=c(1,3,4,6,2), lty=c(1,1,1,1,2), lwd=c(1,1,1,1,2), bty="n", text.col=c(1,1,1,1,2))


## ----RU_table, echo=FALSE, results='asis'--------------------------------

nm <- list(clean.algae, algB, algE, algMy, RUnew, RUB2, RUE2)
names <- c("clean.algae", "algB", "algE", "algMy", "RUnew", "RUB2", "RUE2")
m <- matrix(sapply(nm, nrow),nrow=1, ncol=7, dimnames=list("nr. examples", names))

xt <-xtable(m, caption="Total number of examples in each data set for different parameters of random under-sampling strategy.",label="tab:RUReg_table")

print(xt,comment = FALSE, type = 'latex')

## ----RO_autoRel----------------------------------------------------------
# using the automatic method for defining the relevance function and
# the default threshold of 0.5
Alg.my <- randOverRegress(a7~., clean.algae, C.perc=list(2.5))
Alg.Bal <- randOverRegress(a7~., clean.algae, C.perc="balance")
Alg.Ext0.5 <- randOverRegress(a7~., clean.algae, C.perc="extreme")

# change the relevance threshold to 0.9
Alg.Ext0.9 <- randOverRegress(a7~., clean.algae, thr.rel=0.9, C.perc="extreme")

## ----RO_ex1, fig.cap="Relevance function and density of the target variable in the original and new data sets using Random over-sampling strategy.", echo=FALSE----
par(mar = c(5,5,2,5))
plot(density(clean.algae$a7), xlab=expression(y), main="")
lines(density(Alg.my$a7), col=3)
lines(density(Alg.Bal$a7), col=4)
lines(density(Alg.Ext0.5$a7), col=6)
lines(density(Alg.Ext0.9$a7), col=7)

y <- sort(resp(a7~.,clean.algae))

pc <- phi.control(y, method="extremes")
y.relev <- phi(y,pc)

par(new=TRUE)
plot(y,y.relev, lty=2, col=2, lwd=3, axes=F, ylab=NA, xlab=NA,type="l")
axis(4, xaxt="n",col =2, col.axis =2 , lwd = 2)
mtext(expression(phi(y)),4, col=2, line=3)
legend(22,0.9, c("clean.algae", "Alg.my", "Alg.Bal", "Alg.Ext0.5", "Alg.Ext0.9",expression(phi())), col=c(1,3,4,6,7,2), lty=c(1,1,1,1,1,2), lwd=c(1,1,1,1,1,2), bty="n", text.col=c(1,1,1,1,1,2))

## ----RO_table, echo=FALSE, results='asis'--------------------------------

nm <- list(clean.algae, Alg.my, Alg.Bal, Alg.Ext0.5, Alg.Ext0.9)
names <- c("clean.algae", "Alg.my", "Alg.Bal", "Alg.Ext0.5", "Alg.Ext0.9")
m <- matrix(sapply(nm, nrow), nrow=1, ncol=5, dimnames=list("nr. examples", names))

xt <-xtable(m, caption="Total number of examples in each data set for different parameters of random over-sampling strategy.",label="tab:ROReg_table")

print(xt,comment = FALSE, type = 'latex')

## ----GN_1----------------------------------------------------------------
# relevance function estimated automatically has two bumps
# defining the desired percentages of under and over-sampling to apply
C.perc=list(0.5, 3)
# define the relevance threshold
thr.rel=0.8
mygn <- gaussNoiseRegress(a7~., clean.algae, thr.rel=thr.rel, C.perc=C.perc)
gnB <- gaussNoiseRegress(a7~., clean.algae, thr.rel=thr.rel, C.perc="balance")
gnE <- gaussNoiseRegress(a7~., clean.algae, thr.rel=thr.rel, C.perc="extreme")


## ----GN_plot1, fig.cap="Relevance function and density of the target variable in the original and new data sets using Gaussian noise strategy.", echo=FALSE----
par(mar = c(5,5,2,5))
plot(density(clean.algae$a7), xlab=expression(y), main="")
lines(density(mygn$a7), col=3)
lines(density(gnB$a7), col=4)
lines(density(gnE$a7), col=6)

names <- c("clean.algae", "mygn", "gnB", "gnE")
y <- sort(resp(a7~.,clean.algae))
pc <- phi.control(y, method="extremes")
y.relev <- phi(y,pc)

par(new=TRUE)
plot(y,y.relev, lty=2, col=2, lwd=1, axes=F, ylab=NA, xlab=NA,type="l")
axis(4, xaxt="n",col =2, col.axis =2 , lwd = 1)
mtext(expression(phi(y)),4, col=2, line=3)
legend(22,0.9, c("clean.algae", "mygn", "gnB", "gnE",expression(phi())), col=c(1,3,4,6,2), lty=c(1,1,1,1,2), lwd=c(1,1,1,1,1), bty="n", text.col=c(1,1,1,1,2))

## ----GN_plot2,fig.cap="The impact of Gaussian Noise strategy.",out.width="0.8\\textwidth", echo=FALSE----
par(mfrow = c(2, 2))
plot(clean.algae[,16], clean.algae[,6], col= ifelse(clean.algae[, 18]>5,2,3), pch=ifelse(clean.algae[,18]>5,"+", "-"), main="Original data", xlim=c(0,45), ylim=c(0,400))
plot(mygn[,16], mygn[,6], col= ifelse(mygn[, 18]>5,2,3), pch=ifelse(mygn[,18]>5,"+", "-"), main="user specified percentages", xlim=c(0,45), ylim=c(0,400))
par(xpd=TRUE)
legend("topright", inset=c(0,0),col=c(2,3), c("important/rare cases", "normal/uninteresting cases"),pch=c("+", "-"), bty="n")
plot(gnB[,16], gnB[,6], col= ifelse(gnB[, 18]>5,2,3),pch=ifelse(gnB[,18]>5,"+", "-"), main="balance method", xlim=c(0,45), ylim=c(0,400))
plot(gnE[,16], gnE[,6], col= ifelse(gnE[, 18]>5,2,3), pch=ifelse(gnE[,18]>5,"+", "-"), main="extreme method", xlim=c(0,45), ylim=c(0,400))


## ----gn_balance_pert_eval------------------------------------------------
# the default uses the value of 0.1 for "pert" parameter
gnB1 <- gaussNoiseRegress(a7~., clean.algae, thr.rel=thr.rel, C.perc="balance")

# try two different values for "pert" parameter
gnB2 <- gaussNoiseRegress(a7~., clean.algae, thr.rel=thr.rel, C.perc="balance",
                          pert=0.5)
gnB3 <- gaussNoiseRegress(a7~., clean.algae, thr.rel=thr.rel, C.perc="balance",
                          pert=0.01)


## ----GN_plot3,fig.cap="Impact of changing the pert parameter in Gaussian Noise strategy.",out.width="0.8\\textwidth", echo=FALSE----
par(mfrow = c(2, 2))
plot(clean.algae[,16], clean.algae[,6], col= ifelse(clean.algae[, 18]>5,2,3), pch=ifelse(clean.algae[,18]>5,"+", "-"), main="Original data", xlim=c(-5,45), ylim=c(0,400))
plot(gnB1[,16], gnB1[,6], col= ifelse(gnB1[, 18]>5,2,3), pch=ifelse(gnB1[,18]>5,"+", "-"), main="pert=0.1", xlim=c(-5,45), ylim=c(0,400))
par(xpd=TRUE)
legend("topright", inset=c(0,0),col=c(2,3), c("important/rare cases", "normal/uninteresting cases"),pch=c("+", "-"), bty="n")
plot(gnB2[,16], gnB2[,6], col= ifelse(gnB2[, 18]>5,2,3),pch=ifelse(gnB2[,18]>5,"+", "-"), main="pert=0.5", xlim=c(-5,45), ylim=c(0,400))
plot(gnB3[,16], gnB3[,6], col= ifelse(gnB3[, 18]>5,2,3), pch=ifelse(gnB3[,18]>5,"+", "-"), main="pert=0.01", xlim=c(-5,45), ylim=c(0,400))


## ----smoteR_rel1, fig.cap="Relevance function obtained automatically for the clean.algae data set"----

# we will use the automatic method for defining the relevance function and will
# set the relevance threshold to 0.8 
# this method splits the data set in two: a first range of values normal and less
# important and a second range with the interesting cases

# to check this, we can plot the relevance function obtained automatically
# as follows:

y <- sort(clean.algae$a7)
phiF.args <- phi.control(y,method="extremes",extr.type="both")
y.phi <- phi(y,phi.parms=phiF.args)

# plot the relevance function
plot(y,y.phi,type="l",
     ylab=expression(phi(y)),xlab=expression(y))

#add the relevance threshold to the plot
abline(h=0.8, col=3, lty=2)

## ----smoteR_ex1----------------------------------------------------------
# we have two bumps: the first must be under-sampled and the second over-sampled. 
# Thus, we can chose the following percentages: 
thr.rel=0.8
C.perc=list(0.1, 8) 
# using these percentages and the relevance threshold of 0.8 with all the other parameters default values
# it is necessary to set the distance function to "HEOM" because the data set contains nominal and numeric features
mysm <- smoteRegress(a7~., clean.algae, thr.rel=thr.rel, dist="HEOM", C.perc=C.perc)

# use the automatic method for obtaining a balanced data set
smB <- smoteRegress(a7~., clean.algae, thr.rel=thr.rel, dist="HEOM", C.perc="balance")

# use the automatic method for invert the frequencies of the bumps
smE <- smoteRegress(a7~., clean.algae, thr.rel=thr.rel, dist="HEOM", C.perc="extreme")


## ----smoteR_plot1, fig.cap="Relevance function and density of the target variable in the original and new data sets using smoteR strategy.", echo=FALSE----
par(mar = c(5,5,2,5))
plot(density(clean.algae$a7), xlab=expression(y), main="")
lines(density(mysm$a7), col=3)
lines(density(smB$a7), col=4)
lines(density(smE$a7), col=6)

names <- c("clean.algae", "mysm", "smB", "smE")
y <- sort(resp(a7~.,clean.algae))
pc <- phi.control(y, method="extremes")
y.relev <- phi(y,pc)

par(new=TRUE)
plot(y,y.relev, lty=2, col=2, lwd=1, axes=F, ylab=NA, xlab=NA,type="l")
axis(4, xaxt="n",col =2, col.axis =2 , lwd = 1)
mtext(expression(phi(y)),4, col=2, line=3)
legend(22,0.9, c("clean.algae", "mysm", "smB", "smE",expression(phi())), col=c(1,3,4,6,2), lty=c(1,1,1,1,2), lwd=c(1,1,1,1,1), bty="n", text.col=c(1,1,1,1,2))

## ----smoteR_count1, echo=FALSE-------------------------------------------
# check the number of examples in each bump of relevance
y <- sort(resp(a7~.,clean.algae))
pc <- phi.control(y, method="extremes")
y.relev <- phi(y,pc)

y.rel.mysm <- phi(resp(a7~.,mysm),pc)
y.rel.smB <- phi(resp(a7~.,smB),pc)
y.rel.smE <- phi(resp(a7~.,smE),pc)

imp <-list(y.relev, y.rel.mysm, y.rel.smB, y.rel.smE)
res <- sapply(imp, function(x)c(length(which(x<thr.rel)), length(which(x>=thr.rel))))

m <- matrix(t(res), nrow=4, ncol=2, dimnames=list(names, c("first bump", "second bump")))

## ----smoteR_1, echo=FALSE, results='asis'--------------------------------
xt <-xtable(m, caption="Number of examples in each bump of relevance for different parameters of smoteR strategy.",label="tab:smoteR_1")
print(xt,comment = FALSE, type = 'latex')


## ----smoteR_1bar, echo=FALSE, fig.cap="Impact in the distribution of examples for several parameters in smoteR strategy. ",out.height="0.5\\textheight"----
DF <- rbind(data.frame(Dat="Original", obs=c(rep("first.bump",res[1]), rep("second.bump", res[2]))), data.frame(Dat="mysm", obs=c(rep("first.bump",res[3]), rep("second.bump", res[4]))), data.frame(Dat="smB", obs=c(rep("first.bump",res[5]), rep("second.bump", res[6]))), data.frame(Dat="smE", obs=c(rep("first.bump",res[7]), rep("second.bump", res[8]))))

g1 <- ggplot(DF,aes(x=obs, fill=Dat, colour=Dat))+geom_histogram(binwidth=1, position="dodge", aes(group=Dat), colour="black")

g2 <- ggplot(DF,aes(x=Dat, fill=obs, colour=obs))+geom_histogram(binwidth=1, position="fill", aes(group=obs), colour="black")

plots <- list(g1, g2)

do.call(grid.arrange, plots)


## ----smoteR_fig2,fig.cap="The impact of smoteR strategy.",out.width="0.8\\textwidth", echo=FALSE----
  par(mfrow = c(2, 2))
plot(clean.algae[,5], clean.algae[,6], col= ifelse(clean.algae[, 18]>5,2,3), pch=ifelse(clean.algae[,18]>5,"+", "-"), main="Original data", xlab="mnO2", ylab="Cl", ylim=c(0,400))
plot(mysm[,5], mysm[,6], col= ifelse(mysm[, 18]>5,2,3), ylim=c(0,400), pch=ifelse(mysm[,18]>5,"+", "-"), main="user specified percentages", xlab="mnO2", ylab="Cl")
par(xpd=TRUE)
legend("topright", inset=c(0,0),col=c(2,3), c("important/rare cases", "normal/uninteresting cases"),pch=c("+", "-"), bty="n")
plot(smB[,5], smB[,6], col= ifelse(smB[, 18]>5,2,3),pch=ifelse(smB[,18]>5,"+", "-"), main="balance method", xlab="mnO2", ylab="Cl", ylim=c(0,400))
plot(smE[,5], smE[,6], col= ifelse(smE[, 18]>5,2,3), pch=ifelse(smE[,18]>5,"+", "-"), main="extreme method", xlab="mnO2", ylab="Cl", ylim=c(0,400))


## ----IS_1----------------------------------------------------------------
# relevance function estimated automatically has two bumps
# using the strategy with threshold definition
C.perc=list(0.2,6)
myIS <- ImpSampRegress(a7~., clean.algae, thr.rel=0.8,C.perc=C.perc)
ISB <- ImpSampRegress(a7~., clean.algae, thr.rel=0.8, C.perc="balance")
ISE <- ImpSampRegress(a7~., clean.algae, thr.rel=0.8, C.perc="extreme")


## ----IS_plot1, fig.cap="Relevance function and density of the target variable in the original and new data sets using Importance Sampling strategy.", echo=FALSE----
par(mar = c(5,5,2,5))
plot(density(clean.algae$a7), xlab=expression(y), main="")
lines(density(myIS$a7), col=3)
lines(density(ISB$a7), col=4)
lines(density(ISE$a7), col=6)

names <- c("clean.algae", "myIS", "ISB", "ISE")
y <- sort(resp(a7~.,clean.algae))
pc <- phi.control(y, method="extremes")
y.relev <- phi(y,pc)

par(new=TRUE)
plot(y,y.relev, lty=2, col=2, lwd=1, axes=F, ylab=NA, xlab=NA,type="l")
axis(4, xaxt="n",col =2, col.axis =2 , lwd = 1)
mtext(expression(phi(y)),4, col=2, line=3)
legend(22,0.9, c(names,expression(phi())), col=c(1,3,4,6,2), lty=c(1,1,1,1,2), lwd=c(1,1,1,1,1), bty="n", text.col=c(1,1,1,1,2))

## ----IS_plot2,fig.cap="Impact of Importance Sampling strategy.",out.width="0.8\\textwidth", echo=FALSE----
par(mfrow = c(2, 2))
plot(clean.algae[,5], clean.algae[,6], col= ifelse(clean.algae[, 18]>5,2,3), pch=ifelse(clean.algae[,18]>5,"+", "-"), main="Original data",ylim=c(0,400))
plot(jitter(myIS[,5]), jitter(myIS[,6]), col= ifelse(myIS[, 18]>5,2,3), pch=ifelse(myIS[,18]>5,"+", "-"), main="User-defined percentages",ylim=c(0,400))
par(xpd=TRUE)
legend("topright", inset=c(0,0),col=c(2,3), c("important/rare cases", "normal/uninteresting cases"),pch=c("+", "-"), bty="n")
plot(jitter(ISB[,5]), jitter(ISB[,6]), col= ifelse(ISB[, 18]>5,2,3),pch=ifelse(ISB[,18]>5,"+", "-"), main="balance",ylim=c(0,400))
plot(jitter(ISE[,5]), jitter(ISE[,6]), col= ifelse(ISE[, 18]>5,2,3), pch=ifelse(ISE[,18]>5,"+", "-"), main="extreme",ylim=c(0,400))


## ----IS_2----------------------------------------------------------------
# relevance function is also estimated automatically
# the default is not to use a relevance threshold and to assign equal 
# importance to under and over-sampling, i.e., U=0.5 and O=0.5
ISD <- ImpSampRegress(a7~., clean.algae) 
IS1 <- ImpSampRegress(a7~., clean.algae, U=0.9, O=0.2)
IS2 <- ImpSampRegress(a7~., clean.algae, U=0.5, O=0.8)


## ----IS_plot3, fig.cap="Relevance function and density of the target variable in the original and new data sets using Importance Sampling strategy.", echo=FALSE----
par(mar = c(5,5,2,5))
plot(density(clean.algae$a7), xlab=expression(y), main="")
lines(density(ISD$a7), col=3)
lines(density(IS1$a7), col=4)
lines(density(IS2$a7), col=6)

names <- c("clean.algae", "ISD", "IS1", "IS2")
y <- sort(resp(a7~.,clean.algae))
pc <- phi.control(y, method="extremes")
y.relev <- phi(y,pc)

par(new=TRUE)
plot(y,y.relev, lty=2, col=2, lwd=1, axes=F, ylab=NA, xlab=NA,type="l")
axis(4, xaxt="n",col =2, col.axis =2 , lwd = 1)
mtext(expression(phi(y)),4, col=2, line=3)
legend(22,0.9, c(names,expression(phi())), col=c(1,3,4,6,2), lty=c(1,1,1,1,2), lwd=c(1,1,1,1,1), bty="n", text.col=c(1,1,1,1,2))

## ----IS_plot4,fig.cap="Impact of Importance Sampling strategy.",out.width="0.8\\textwidth", echo=FALSE----
par(mfrow = c(2, 2))
plot(clean.algae[,5], clean.algae[,6], col= ifelse(clean.algae[, 18]>5,2,3), pch=ifelse(clean.algae[,18]>5,"+", "-"), main="Original data",ylim=c(0,400))
plot(jitter(ISD[,5]), jitter(ISD[,6]), col= ifelse(ISD[, 18]>5,2,3), pch=ifelse(ISD[,18]>5,"+", "-"), main="U=0.5 O=0.5",ylim=c(0,400))
par(xpd=TRUE)
legend("topright", inset=c(0,0),col=c(2,3), c("important/rare cases", "normal/uninteresting cases"),pch=c("+", "-"), bty="n")
plot(jitter(IS1[,5]), jitter(IS1[,6]), col= ifelse(IS1[, 18]>5,2,3),pch=ifelse(IS1[,18]>5,"+", "-"), main="U=0.9 O=0.2",ylim=c(0,400))
plot(jitter(IS2[,5]), jitter(IS2[,6]), col= ifelse(IS2[, 18]>5,2,3), pch=ifelse(IS2[,18]>5,"+", "-"), main="U=0.5 O=0.8",ylim=c(0,400))


## ----numeric_measures_ex-------------------------------------------------
data <- iris[-c(91:125),]
# using the default of smote to invert the frequencies of the data set
set.seed(123)
sm.Eu <- smoteClassif(Species~., data, dist="Euclidean", 
                      C.perc="extreme", k=3)
set.seed(123)
sm.Man1 <- smoteClassif(Species~., data, dist="Manhattan",
                        C.perc="extreme", k=3)
set.seed(123)
sm.Man2 <- smoteClassif(Species~., data, dist="p-norm", p=1,
                        C.perc="extreme", k=3)
set.seed(123)
sm.5norm <- smoteClassif(Species~., data, dist="p-norm", p=5, 
                         C.perc="extreme", k=3)
set.seed(123)
sm.Cheb <- smoteClassif(Species~., data, dist="Chebyshev", 
                        C.perc="extreme", k=3)
set.seed(123)
sm.Canb <- smoteClassif(Species~., data, dist="Canberra", 
                        C.perc="extreme", k=3)

## ----dist_num, fig.cap="Impact of using different distance functions with smote strategy.", echo=FALSE----
par(mfrow=c(3,2))
plot(data[,1], data[,2], pch=as.integer(data[,5]), col=as.integer(data[,5]), xlim=c(4,8), ylim=c(2,4.5), main="Original data")
plot(sm.Eu[,1], sm.Eu[,2], pch=as.integer(sm.Eu[,5]), col=as.integer(sm.Eu[,5]), xlim=c(4,8), ylim=c(2,4.5), main="Euclidean distance")
plot(sm.Man1[,1], sm.Man1[,2], pch=as.integer(sm.Man1[,5]), col=as.integer(sm.Man1[,5]), xlim=c(4,8), ylim=c(2,4.5), main="Manhattan distance")
plot(sm.5norm[,1], sm.5norm[,2], pch=as.integer(sm.5norm[,5]), col=as.integer(sm.5norm[,5]), xlim=c(4,8), ylim=c(2,4.5), main="Minkowsky with r=5")
plot(sm.Cheb[,1], sm.Cheb[,2], pch=as.integer(sm.Cheb[,5]), col=as.integer(sm.Cheb[,5]), xlim=c(4,8), ylim=c(2,4.5), main="Chebyshev distance")
plot(sm.Canb[,1], sm.Canb[,2], pch=as.integer(sm.Canb[,5]), col=as.integer(sm.Canb[,5]), xlim=c(4,8), ylim=c(2,4.5), main="Canberra distance")



## ----overlap_ex----------------------------------------------------------
# build a data set with all nominal features
library(DMwR)
data(algae)
clean.algae <- algae[complete.cases(algae),1:3]

# speed is considered the target class
summary(clean.algae)
ndat1 <- ENNClassif(speed~., clean.algae, dist="Overlap",  Cl=c("high", "medium"))
ndat2 <- ENNClassif(speed~., clean.algae, dist="Overlap",  Cl="all")

#all the smaller classes are the most important
ndat3 <- NCLClassif(speed~., clean.algae, dist="Overlap",  Cl="smaller")
# the most important classes are "high" and "low"
ndat4 <- NCLClassif(speed~., clean.algae, dist="Overlap",  Cl=c("high", "low"))

ndat5 <- smoteClassif(speed~., clean.algae, dist="Overlap", C.perc="balance")

## ----dist_overlap, fig.cap="Using Overlap distance function with different strategies on a data set with only nominal features.", echo=FALSE----
g1 <- ggplot(clean.algae, aes(x = speed, fill=size))+ geom_bar()+ggtitle("Original data")

g2 <- ggplot(ndat2[[1]], aes(x = speed, fill=size))+ geom_bar()+ggtitle("ENN strategy")

g3 <- ggplot(ndat3, aes(x = speed, fill=size))+ geom_bar()+ggtitle("NCL strategy")

g4 <- ggplot(ndat5, aes(x = speed, fill=size))+ geom_bar()+ggtitle("smote strategy")

plots <- list(g1, g2, g3, g4)

do.call(grid.arrange, plots)

## ----HEOM_ex-------------------------------------------------------------
# build a data set with nominal and numeric features
library(DMwR)
data(algae)
clean.algae <- algae[complete.cases(algae),1:5]

# speed is the target class
summary(clean.algae)
enn <- ENNClassif(speed~., clean.algae, dist="HEOM",  Cl="all", k=5)[[1]]
#consider all the smaller classes as the most important
ncl <- NCLClassif(speed~., clean.algae, dist="HEOM",  Cl="smaller")
sm <- smoteClassif(speed~., clean.algae, dist="HEOM", C.perc="balance")

## ----dist_heom, fig.cap="Using HEOM distance function with different strategies on a data set with both nominal and numeric features.", echo=FALSE----
par(mfrow = c(2, 2))
plot(clean.algae$mxPH, clean.algae$mnO2,pch=as.integer(clean.algae$speed), col=as.integer(clean.algae$speed), xlim=c(7,9.5), ylim=c(1.5,13.5), main="original data")
plot(enn$mxPH, enn$mnO2,pch=as.integer(enn$speed), col=as.integer(enn$speed), xlim=c(7,9.5), ylim=c(1.5,13.5), main="ENN strategy applied")
plot(ncl$mxPH, ncl$mnO2,pch=as.integer(ncl$speed), col=as.integer(ncl$speed), xlim=c(7,9.5), ylim=c(1.5,13.5), main="NCL strategy applied")
plot(sm$mxPH, sm$mnO2,pch=as.integer(sm$speed), col=as.integer(sm$speed), xlim=c(7,9.5), ylim=c(1.5,13.5), main="smote strategy applied")

## ----HVDM_ex-------------------------------------------------------------
# build a data set with both nominal and numeric features
library(DMwR)
data(algae)
clean.algae <- algae[complete.cases(algae),c(1:6)]

# speed is considered the target class
summary(clean.algae)

dat1 <- smoteClassif(speed~., clean.algae, dist="HVDM", C.perc="extreme")

dat2 <- NCLClassif(speed~., clean.algae, k=3, dist="HVDM", Cl="smaller")

dat3 <- TomekClassif(speed~., clean.algae, dist="HVDM", Cl="all", rem="both")

## ----dist_HVDM, fig.cap="Using HVDM distance function with different strategies.", echo=FALSE----
g1 <- ggplot(clean.algae, aes(x = mxPH, y=mnO2, col=speed))+ geom_point()+ggtitle("Original data")

g2 <- ggplot(dat1, aes(x = mxPH, y=mnO2, col=speed))+ geom_point()+ggtitle("smote'd data")

g3 <- ggplot(dat2, aes(x = mxPH, y=mnO2, col=speed))+ geom_point()+ggtitle("NCL strategy")

g4 <- ggplot(dat3[[1]], aes(x = mxPH, y=mnO2, col=speed))+ geom_point()+ggtitle("Tomek links strategy")

plots <- list(g1, g2, g3, g4)

do.call(grid.arrange, plots)

## ----dist_HVDM2, fig.cap="Using different distance functions with  smote strategy.", echo=FALSE----
set.seed(123)
d1 <- smoteClassif(speed~., clean.algae, dist="HVDM", C.perc="extreme")
set.seed(123)
d2 <- smoteClassif(speed~., clean.algae, dist="HEOM", C.perc="extreme")

# in order to use metrics for numeric attributes only we must remove the nominal attributes
set.seed(123)
d3 <- smoteClassif(speed~., clean.algae[,3:6], dist="Euclidean", C.perc="extreme")
set.seed(123)
d4 <- smoteClassif(speed~., clean.algae[,3:6], dist="Chebyshev", C.perc="extreme")
set.seed(123)
d5 <- smoteClassif(speed~., clean.algae[,3:6], dist="Manhattan", C.perc="extreme")


g1 <- ggplot(clean.algae, aes(x = mxPH, y=mnO2, col=speed))+ geom_point()+ggtitle("Original data")

g2 <- ggplot(d1, aes(x = mxPH, y=mnO2, col=speed))+ geom_point()+ggtitle("HVDM distance")

g3 <- ggplot(d2, aes(x = mxPH, y=mnO2, col=speed))+ geom_point()+ggtitle("HEOM distance")

g4 <- ggplot(d3, aes(x = mxPH, y=mnO2, col=speed))+ geom_point()+ggtitle("Euclidean")

g5 <- ggplot(d4, aes(x = mxPH, y=mnO2, col=speed))+ geom_point()+ggtitle("Chebyshev distance")

g6 <- ggplot(d5, aes(x = mxPH, y=mnO2, col=speed))+ geom_point()+ggtitle("Manhattan distance")


plots <- list(g1, g2, g3, g4, g5, g6)

do.call(grid.arrange, plots)

## ----load_res1,echo=FALSE------------------------------------------------

load("allResults2Class.Rdata")

## ----res2class, warning=FALSE, echo=FALSE--------------------------------
topPerformers(subset(res, workflows=glob2rx('*svm*|*random*')), maxs=rep(TRUE,dim(res[[1]][[1]]@iterationsScores)[2]))

## ----ionosphereRank, echo=FALSE------------------------------------------
rankWorkflows(subset(res, tasks='Ionosphere', workflows=glob2rx('*svm*|*random*')), maxs=rep(TRUE,dim(res[[1]][[1]]@iterationsScores)[2]))

## ----Ionosphere_plot1,echo=FALSE, fig.cap="Results of Ionosphere data for strategies Gaussian Noise and smote on variants of svm learner"----
plot(subset(res, tasks='Ionosphere', workflows=glob2rx('*none.svm*|*GN.svm*|*smote.svm*'), metrics=glob2rx('F$')))

#metricsSummary(subset(res, workflows=glob2rx('*svm*'), tasks='Ionosphere'),summary='median')

## ----summ_Ionosphere_F,  eval=FALSE--------------------------------------
## summary(subset(res, tasks='Ionosphere',
##                     workflows=glob2rx('*svm*'),
##                     metrics=glob2rx('F$')))

## ----thor_plot1, echo=FALSE,fig.cap="Results of thor data on variants of svm learner", warning=FALSE----
plot(subset(res, tasks='pima', workflows=glob2rx('*svm*'),metrics=glob2rx('F$')))
#plot(subset(res, tasks='thor', workflows=glob2rx('*svm*'),metrics='F', partial=FALSE))

## ----pimaRankRes, echo=FALSE---------------------------------------------
rankWorkflows(subset(res, tasks='pima', workflows=glob2rx('*svm*|*random*')), maxs=rep(TRUE,dim(res[[1]][[1]]@iterationsScores)[2]))

## ----pima_plot1, echo=FALSE, fig.cap="Results of pima data for strategies Random under-sampling, Gaussian Noise and smote on variants of svm learner"----
plot(subset(res, tasks='pima',workflows=glob2rx('*none.svm|RU.svm|smote.svm|GN.svm*'), metrics=glob2rx('F$')))
#summary(subset(res, tasks='pima', workflows=glob2rx('*earth*'),metrics='F'))

#metricsSummary(subset(res, workflows=glob2rx('*svm*'), tasks='pima'),summary='median')


## ----pima_plot2, fig.cap="Results of pima data set for random Forest variants on original data set and data with ENN and Tomek links strategies applied.",echo=FALSE----
plot(subset(res, tasks='pima',workflows=glob2rx('*none.random|Tomek.random|ENN.random*'), metrics=glob2rx('F$')))


## ----load_res2,echo=FALSE------------------------------------------------

load("allResultsRegress.Rdata")

## ----resRegress, echo=FALSE----------------------------------------------
topPerformers(res,maxs=rep(TRUE,dim(res[[1]][[1]]@iterationsScores)[2]))

## ----rankWFRegress, echo=FALSE-------------------------------------------
rankWorkflows(res,maxs=rep(TRUE,dim(res[[1]][[1]]@iterationsScores)[2]))

## ----algae_plot1, fig.cap="Results on algae data set for strategies none, random over-sampling and random under-sampling for random forest learning variants",echo=FALSE----
plot(subset(res, tasks='a1',workflows=glob2rx('*none.random|RO.rand|RU.rand*')))

## ----acceleration_plot1, fig.cap="Results on acceleration data set for none and random over-sampling strategies with random forest learning variants",echo=FALSE----
plot(subset(res, tasks='acceleration',workflows=glob2rx('*none.random|RO.rand*'), metrics='F'))

## ----boston_plot1, fig.cap="Results on boston data set for none and Importance Sampling strategy on random forest learning variants",echo=FALSE----
plot(subset(res, tasks='boston',workflows=glob2rx('*none.random|IS.rand*'), metrics='F'))

