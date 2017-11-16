library(tidyverse)
library(modelr)
require(dplyr)
require(data.world)
require(MASS)
require(ISLR)
require(ggplot2)
library(leaps)
library(glmnet)
require(tree)
require(randomForest)
require(gbm)

data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50OnRhcnJhbnRybCIsImlzcyI6ImFnZW50OnRhcnJhbnRybDo6MDE1OTQxYzQtNTUyZC00YjI3LWIxNGEtYzllN2ExMjYxN2FiIiwiaWF0IjoxNTA1MzEzMjAyLCJyb2xlIjpbInVzZXJfYXBpX3dyaXRlIiwidXNlcl9hcGlfcmVhZCJdLCJnZW5lcmFsLXB1cnBvc2UiOnRydWV9.vWrAbNkyU0mhgsdXXL-bxESWzppmpm8wguw9uI7pJ64ZsDtovi8kbWbPYS5pPcX8DDnVMuYxJJWHhqdxv--R_w"))
#vignette("quickstart", package = "data.world")

project <- "https://data.world/tarrantrl/parkinsons-telemonitoring"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons_telemonitoring"),
  dataset = project
)

project <- "https://data.world/marcusgabe-ut/parkinsons-data"
df_b <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons"),
  dataset = project
)

summary(df)

#Add binary column for gender
df <- df %>% dplyr::mutate(sex2 = ifelse(sex == "true", 1, 0)) 

#Add column for age. 1 = age <= 65, 0 otherwise
df_age <- df %>% dplyr::mutate(age2 = ifelse(age <= 65, 1, 0)) %>% dplyr::select(., -subject, -sex, as.factor(sex))

attach(df)
head(df)

df_nums = df %>% dplyr::select(-sex,-subject)
head(df_nums)
df_tot_nums = df_nums %>% dplyr::select(-motor_updrs)
dim(df_tot_nums)
train=sample(1:nrow(df_tot_nums),2937)

### Boosting ###
# use boosting first to get an idea of what variables are important
# shrinkage of each tree you make, interaction depth is number of random predictors you use at each split
boost.parkinsons=gbm(total_updrs~.,data=df_tot_nums[train,],distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4)

# gives you relative importance of each predictor (like PCA but easier to interpret)
summary(boost.parkinsons)
#plot(boost.parkinsons,i="lstat")
#plot(boost.parkinsons,i="rm")


n.trees=seq(from=100,to=10000,by=100)
# use number of trees (going by hundreds) to make predictions
predmat=predict(boost.parkinsons,newdata=df_tot_nums[-train,],n.trees=n.trees)
dim(predmat)
# boosting error
berr=with(df_tot_nums[-train,],apply( (predmat-total_updrs)^2,2,mean))
plot(n.trees,berr,pch=19,ylab="Mean Squared Error", xlab="# Trees",main="Boosting Test Error")
abline(h=min(berr),col="red")


## do random forest for total updrs

rf.total=randomForest(total_updrs~.,data=df_tot_nums,subset=train)
rf.total

dim(df_tot_nums)
oob.err=double(19)
test.err=double(19)
for(mtry in 1:19){
  fit=randomForest(total_updrs~.,data=df_tot_nums,subset=train,ntree=400)
  oob.err[mtry]=fit$mse[400]
  pred=predict(fit,df_tot_nums[-train,])
  test.err[mtry]=with(df_tot_nums[-train,],mean((total_updrs-pred)^2))
  cat(mtry," ")
}
matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("OOB","Test"),pch=19,col=c("red","blue"))

## do k means clustering between subject and total updrs

x = df%>%dplyr::select(subject,total_updrs)
km.out=kmeans(x,42)
km.out
par(mfrow = c(1,2))
plot(x,col=km.out$cluster,cex=2,main = "K means clustering", pch=1,lwd=2)
plot(x,col=df$subject,cex=2,main = "Actual subject",pch=1,lwd=2)

# ggplot alternative:
km.out$cluster
dfcluster <- data.frame(x, km.out$cluster)
names(dfcluster)
dfcluster2 <- dplyr::bind_cols(dfcluster, data.frame(df$subject))

kmeansubject <- dfcluster2 %>% ggplot(aes(x=age, y=total_updrs, colour = as.factor(km.out.cluster))) + geom_point()
actualsubject <- dfcluster2 %>% ggplot(aes(x=age, y=total_updrs, colour = as.factor(dfsubject.subject))) + geom_point()

df1 <- data.frame(km.out$centers, km.out$size)
names(df1)
dfcluster2 %>% ggplot() + geom_point(mapping = aes(x=age, y=total_updrs, colour = as.factor(km.out.cluster))) + geom_point(data=df1, mapping=aes(age, total_updrs, size=km.out.size))

df %>% ggplot(aes(x=age, y = total_updrs, colour = sex2)) + geom_point()

x = df%>%dplyr::select(age,total_updrs)
km.out=kmeans(x,6)
km.out
par(mfrow = c(1,2))
plot(x,col=km.out$cluster,cex=2,main = "K means clustering", pch=1,lwd=2)
plot(x,col=df$subject,cex=2,main = "Actual subject",pch=1,lwd=2)

x = df%>%dplyr::select(dfa,total_updrs)
km.out=kmeans(x,6)
km.out
par(mfrow = c(1,2))
plot(x,col=km.out$cluster,cex=2,main = "K means clustering", pch=1,lwd=2)
plot(x,col=df$subject,cex=2,main = "Actual subject",pch=1,lwd=2)

x = df%>%dplyr::select(subject,dfa)
km.out=kmeans(x,42)
km.out
par(mfrow = c(1,2))
plot(x,col=km.out$cluster,cex=2,main = "K means clustering", pch=1,lwd=2)
plot(x,col=df$subject,cex=2,main = "Actual subject",pch=1,lwd=2)



# random forest cross validation function : rfcv(dplyr::select(train, -target_var), train$target_var, cv.fold=5)$error.cv
# picks 1, squareroot p, half p, and all p -- mtry arguement (all p is the same as bagging)

# ******make sure to turn predictions into a data frame, can then bind it to the test data to plot

## MSE for boosting
# model$train.error[#trees] -> boost.qperf$train.error[10000] -> error from last tree because trees build off each other
# pred.err.boost=with(pdf.boost, mean((throughput-pred.boost)^2))
# then squareroot that for the test mse

#boost.qperf=gbm(throughput~., data=train, distribution="gaussian",)

### K means ###

x=cbind(age, test_time, motor_updrs, total_updrs, jitter, jitter_abs, jitter_rap, jitter_ppq5, jitter_ddp, shimmer, shimmer_db, shimmer_apq3, shimmer_apq5, shimmer_apq11, shimmer_dda, nhr, hnr, rpde, dfa, ppe)
km.out=kmeans(x,4,nstart=15)
km.out
km.out$cluster

#set.seed(101)
#x=matrix(rnorm(100*2),100,2) # simulating data x - 2D array with 100 rows with normally distributed numbers
xmean=matrix(rnorm(8,sd=4),4,2) # 2D with only 4 rows and a big standard deviation
which=sample(1:4,100,replace=TRUE) # array of random 1-4 numbers 100 times - K = 4
x=x+xmean[which,] # assign points in x to these numbers
plot(x,col=which,pch=19) # plot!

# ggplot alternative:
library(dplyr)
require(ggplot2)
df <- data.frame(x, which) # make a dataframe
names(df) # 3 colums - 2 from x and one from which
df %>% ggplot(aes(x=X1, y=X2, colour = as.factor(which))) + geom_point() # as.factor makes it treat which as discreet, not continuous



### PCA ###

pca.out=prcomp(df_nums, scale=TRUE)
pca.out
names(pca.out)
biplot(pca.out, scale=0)

library(pls)
set.seed(2)
pca.fit = pcr(total_updrs~., data=df_nums, scale=TRUE) # scale=TRUE is very important - NORMALIZES DATA such that the variance for every predictor is the same, giving each predictor equal weight when creating components
pca.fit
summary(pca.fit)
# this package does cross validation for all the models for each number of principal components
validationplot(pca.fit, val.type="MSEP") # you can see how the MSEP improves with the addition of each component. This graph shows that PCA might not be worth while
biplot(pca.fit, scale=.5)

library(tidyverse)
pca.fit = prcomp(df_nums, scale=TRUE) # don't say what to predict
pca.fit
summary(pca.fit)
#validationplot(pca.fit, val.type="MSEP") # this function doesn't do cross validation
biplot(pca.fit, scale=.5)