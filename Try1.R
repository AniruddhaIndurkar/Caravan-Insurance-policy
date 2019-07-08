getwd()
setwd(dir = "~/Data Science/Semester 2/FIT5149_ADA/Assignment/")
list.files()
library(ggplot2)
library(dplyr)
library(data.table)
library(gridExtra)

### Reading the data
car.data.train=read.csv("ticdata2000.txt",sep="\t",header = F)
car.data.test=read.csv("ticeval2000.txt",sep="\t",header = F)
car.data.target=read.csv("tictgts2000.txt",sep="\t",header = F)

### Converting to data frame
car.data.train=as.data.frame(car.data.train)
car.data.test=as.data.frame(car.data.test)
car.data.target=as.data.frame(car.data.target)

### We make use of data table library to work on the data
car.data.train=as.data.table(car.data.train)
car.data.test=as.data.table(car.data.test)
car.data.target=as.data.table(car.data.target)
gc()

headers=read.csv(file = "Book1.csv")
print(headers)
headers=as.data.table(headers)
headernames=headers[,tstrsplit(Nr.Name.Description.Domain, ' ')]
headernames$V2

names(car.data.train)<-headernames$V2  
car.data.train
names(car.data.test)<-headernames$V2[1:85]

str(car.data.train)
summary(car.data.train)
apply(car.data.train, 2, function(x) unique(x))

## From the data description we see that the entire data is categorical
## Thus, we convert the entire data into factors

car.data.train[,1:86:=lapply(.SD,as.factor),.SDcols=1:86]

str(car.data.train)
head(car.data.train)




summary(car.data.train)
car.data.train<-as.data.table(car.data.train)
####

p1<-ggplot(car.data.train)+
  aes(x=MOSTYPE,fill=as.factor(car.data.train$CARAVAN))+geom_bar()+
  scale_fill_manual(values=c("light green","blue"))+guides(fill=guide_legend(title="Caravan"))


p2<-ggplot(car.data.train)+
  aes(x=MAANTHUI,fill=as.factor(car.data.train$CARAVAN))+geom_bar()+
  scale_fill_manual(values=c("light green","blue"))+guides(fill=guide_legend(title="Caravan"))

p3<-ggplot(car.data.train)+
  aes(x=MGEMOMV,fill=as.factor(car.data.train$CARAVAN))+geom_bar()+
  scale_fill_manual(values=c("light green","blue"))+guides(fill=guide_legend(title="Caravan"))

grid.arrange(p1,p2,p3)


pairs(car.data.train[,10:15])
car.data.train

cor(as.numeric(car.data.train$MOSTYPE),as.numeric(car.data.train$MAANTHUI))

fit1=glm(data=car.data.train,CARAVAN~.,family=binomial)
summary(fit1)
step(fit1)

### Since there are 86 parameters we initially run a simple logistic model
### After making the first test, we see that the parameters that are significant shown by the
### model include average income, age, married, education level, social class, rented,
### Home owners,

fit2=glm(data=car.data.train,formula=CARAVAN~MGEMLEEF+MGEMOMV+MRELGE+MINKGEM+MINK123M, family=binomial)
summary(fit2)

options(scipen=999)


list(as.factor(car.data.train[,1]))
car.data.train$MOSTYPE
eval(parse(text="car.data.train$MOSTYPE"))

### CHi squared test
j=1
pvalue=c()
attr_Chi<-c()
for (i in colnames(car.data.train[,1:36])){
  x=c()
  
  
  x[j]=paste("car.data.train",sep="$",i)
  z=eval(parse(text=x[j]))
  p=chisq.test(x=z,y=car.data.train$CARAVAN)
  print(j)
  attr_Chi[j]=p$statistic
  pvalue[j]=p$p.value
  print("==================")
  j=j+1
}
p$statistic
Chisqvalues=as.data.frame(cbind(pvalue,attr_Chi,colnames(car.data.train[,1:36])))
colnames(car.data.train[,1:85])
head(headernames)

str(Chisqvalues)
print(Chisqvalues)
#######
print(Chisqvalues$pvalue)
str(Chisqvalues)
Chisqvalues$pvalue<-as.numeric(as.character(Chisqvalues$pvalue))
Chisqvalues$attr_Chi<-as.numeric(as.character(Chisqvalues$attr_Chi))
Chisqvalues$V3<-as.character(Chisqvalues$V3)

variables<-Chisqvalues[Chisqvalues$pvalue<0.01,]
variables<-variables$V3

print(variables)
colnames(car.data.train[,37:64])


### K-fold function

# Create *k* cross-validation folds for the data *d*.
# Parameters:
#   d:  data (data frame)
#   k:  number of folds (default 10)
# Returns:
#   id: vector of values, from 1 to k,
#       indicating the relevant fold for each row of *d*
fold <- function(d, k = 10) {
  
  # The number of rows in *d*.
  r <- nrow(d)
  
  # The number of rows per fold.
  n <- r %/% k
  
  # Minimally "over-sample" from the values 1 to *k*,
  # by "shuffling" *n* + 1 repetitions of the values 1 to *k*,
  # and "take" *r* values (to account for an uneven division of *r*).
  id <- sample(rep(1:k, n + 1))[1:r]
  
  return(id)
}

## Cross validatoon under AUC

# Cross-validate AUC for the predictors specified in *f*,
# for a logistic regression model, for the data *d*.
# Parameters:
#   d: data (data frame)
#   f: formula specifying the predictors
#   k: number of folds (default 10)
#   s: random seed (default 1)
# Returns:
#   list:
#     - AUC
#     - AUC standard error
#     - ROC curve (roc object)
xv <- function(d, f, k = 10, s = 1) {
  
  # Set the random seed.
  set.seed(s)
  
  # Create *k* cross-validation folds for the data *d*.
  id <- fold(d, k)
  
  # Initialise the set of all prediction probabilities,
  # for all *k* cross-validation folds.
  p_ <- NULL
  
  # Initialise the set of all "true" class values,
  # for all *k* cross-validation folds.
  t_ <- NULL
  
  # For each of *k* folds:
  for (i in 1:k) {
    
    # Define the training set.
    tr <- d[i != id, ]
    
    # Define the test set.
    te <- d[i == id, ]
    
    # Fit a logistic regression model for the specified predictors.
    m <- glm(f, tr, family = binomial)
    
    # Apply the model to the test set.
    p <- predict(m, te, "response")
    
    # Accumulate the prediction probabilities.
    p_ <- c(p_, p)
    
    # Accumulate the "true" class values.
    t_ <- c(t_, as.vector(te$qu))
  }
  
  # Compute the ROC curve for the set of all prediction probabilities.
  r <- roc(t_, p_)
  
  # Compute AUC.
  a <- pROC::auc(r)
  
  # Compute AUC standard error.
  se <- sqrt(var(r))
  
  return(list("AUC" = a, "SE" = se, "ROC" = r))
}

# Perform forward stepwise selection using cross-validated AUC,
# for a logistic regression model, for the data *d*.
# Parameters:
#   d: data (data frame)
# Returns:
#   best (data frame):
#     - step number
#     - predictor name
#     - AUC
#     - AUC standard error
step_xv <- function(d) {
  
  # The set of predictors.
  x <- colnames(d[1:(ncol(d) - 1)])
  
  # The response.
  y <- colnames(d[ncol(d)])
  
  # The number of predictors.
  n <- length(x)
  
  # Initialise the return value (data frame).
  best <- data.frame("Step" = 1:n, "Var" = "", "AUC" = 0, "SE" = 0,
                     stringsAsFactors = FALSE)
  
  # For each of *n* predictors:
  for (i in 1:n) {
    
    # Start a new plot.
    plot.new()
    plot.window(xlim = c(1, 0), ylim = c(0, 1))
    grid()
    box()
    abline(1, -1, col = "blue", lty = 2)
    l <- seq(0,1,0.2)
    axis(1, at = rev(l), labels = format(l, 2))
    axis(2)
    title(main = "ROC Curves", xlab = "FPR", ylab = "TPR")
    
    # The number of predictors not yet in the model.
    m <- length(x)
    
    # For each of *m* predictors not yet in the model:
    for (j in 1:m) {
      
      # Define the formula for the current model plus the predictor.
      f <- paste(y, paste(c(best$Var[1:i], x[j]), collapse = " + "),
                 sep = " ~ ")
      f <- formula(f)
      
      # Cross-validate AUC for the candidate model.
      xv_j <- xv(d, f)
      
      # Plot the ROC curve.
      plot(xv_j$ROC, col = "grey", lwd = 1, add = TRUE)
      
      # If the AUC is larger than the current largest AUC,
      # update the best candidate model.
      if (xv_j$AUC > best$AUC[i]) {
        best$AUC[i] <- xv_j$AUC
        best$SE[i] <- xv_j$SE
        best_r <- xv_j$ROC
        best_j <- j
      }
    }
    
    # Add the best predictor to the current model.
    best$Var[i] <- x[best_j]
    
    # Remove the best predictor,
    # from the set of predictors not yet in the model.
    x <- x[-best_j]
    
    # Plot the ROC curve for the new model.
    plot(best_r, col = "red", lwd = 1, add = TRUE)
    text(0, 0, paste("AUC: ", round(best$AUC[i], 6), sep = ""),
         pos = 2, col = "red")
    mtext(paste("Step ", i, ": Add *", best$Var[i], "*", sep = ""))
  }
  
  return(best)
}

# Define a two-column (with equally-sized rows) plotting area.
par(mfrow = c(2, 2))

# Perform stepwise selection by cross-validated AUC,
# for a logistic regression model, for the data *d*.
st <- step_xv(d)


### 
chisq.test(car.data.train[,1:85],car.data.train$CARAVAN)


cor_ <- as.data.frame.table(cor(car.data.train[ , -ncol(car.data.train)]))


cor(cardf[,-ncol(cardf)])
cardf<-as.data.frame(car.data.train)


cor_df<-as.data.frame.table(cor(as.data.frame(car.data.train[,37:85])))


cor_df[cor_df$Freq>0.95]

cor_df[cor_df$Freq>0.95,]
sig_cor<-cor_df[cor_df$Var1!=cor_df$Var2 & (cor_df$Freq>0.95|cor_df$Freq<(-0.95)),]



fit=glm(data=car.data.train,formula=CARAVAN~MOSTYPE+MOSHOOFD+MGODPR+MRELGE+
          MOPLHOOG+MOPLLAAG+MBERHOOG+MBERMIDD+MBERARBG+MBERARBO+MSKA+MSKC+MSKD+
          MHHUUR+MHKOOP+MAUT1+MINK123M+MINKGEM+MAUT0+MZFONDS+MZPART,family="binomial")

summary(fit)
step(fit)





