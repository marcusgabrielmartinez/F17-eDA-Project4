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

data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Om1hcmN1c2dhYmUtdXQiLCJpc3MiOiJhZ2VudDptYXJjdXNnYWJlLXV0OjowYjE2NDQzOC1mYzRlLTRhNDktYWY1MC1iMWU1YjViYmIzYzMiLCJpYXQiOjE0ODQ4NjgyNjMsInJvbGUiOlsidXNlcl9hcGlfcmVhZCIsInVzZXJfYXBpX3dyaXRlIl0sImdlbmVyYWwtcHVycG9zZSI6dHJ1ZX0.Eb9i31mYAv6zQGjlze-PbiBJ_5_JNBDIZn51wcPnnNPny_ih2SSN9Ur_LVyRltEbrReNXM5b371XWrmMiexEKw"))
#vignette("quickstart", package = "data.world")

project <- "https://data.world/tarrantrl/parkinsons-telemonitoring"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons_telemonitoring"),
  dataset = project
)

summary(df)

#Add binary column for gender
df_sex <- df %>% dplyr::select(., -subject) %>% dplyr::mutate(sex2 = ifelse(sex == "true", 1, 0)) %>% dplyr::mutate(age = as.factor(age))

#Add column for age. 1 = age <= 65, 0 otherwise
df_age <- df %>% dplyr::select(., -subject) %>% dplyr::mutate(age2 = ifelse(age <= 65, 1, 0)) %>% dplyr::mutate(sex = as.factor(sex))

attach(df)
attach(df_sex)
attach(df_age)

#Pairs Chart#
pairs_df <- df %>% dplyr::select(., age, motor_updrs, total_updrs, jitter, shimmer, nhr, hnr, dfa)

pairs(pairs_df)

##Decision Trees Section##

#Build and plot tree for sex.
tree.sex=tree(sex2~.-sex,data=df_sex)
summary(tree.sex)
plot(tree.sex)
text(tree.sex,pretty=0)

#Build and plot tree for age
tree.age=tree(age2~.-age,data=df_age)
summary(tree.age)
plot(tree.age)
text(tree.age,pretty=0)

#Print details of sex tree.
tree.sex

#Print details of age tree.
tree.age

#Test/training for sex tree.

set.seed(1011)

train=sample(1:nrow(df_sex),2937)
tree.age=tree(sex2~.-sex,df_sex,subset=train)

plot(tree.sex);text(tree.sex,pretty=0)

tree.pred=predict(tree.sex,df_sex[-train,],type="class")
with(df_sex[-train,],table(tree.pred,sex2))

#Test/training for age tree.
set.seed(1011)

train=sample(1:nrow(df),2937)
tree.age=tree(as.factor(age2)~.-age,df_age,subset=train)

plot(tree.age);text(tree.age,pretty=0)

tree.pred=predict(tree.age,df_age[-train,],type="class")
with(df_age[-train,],table(tree.pred,age2))

#Pruning for sex tree.

cv.sex=cv.tree(tree.sex,FUN=prune.misclass)
cv.sex
plot(cv.sex)
prune.sex=prune.misclass(tree.sex,best=13)
plot(prune.sex);text(prune.sex,pretty=0)


tree.pred=predict(prune.sex,df_sex[-train,],type="class")
with(df_sex[-train,],table(tree.pred,sex2))

#Pruning for age tree.

cv.age=cv.tree(tree.age,FUN=prune.misclass)
cv.age
plot(cv.age)

prune.age=prune.misclass(tree.age,best=13)
plot(prune.age);text(prune.age,pretty=0)


tree.pred=predict(prune.age,df_age[-train,],type="class")
with(df_age[-train,],table(tree.pred,age2))

##Random Forests Section##

# Random Forests for Age
set.seed(101)
dim(df_age)
train=sample(1:nrow(df_age),2937)

rf.age=randomForest(as.factor(age2)~.-age,data=df_age,subset=train)
rf.age

oob.err=double(20)
test.err=double(20)
for(mtry in 1:20){
  fit=randomForest(age2~.-age,data=df_age,subset=train,mtry=mtry,ntree=400)
  oob.err[mtry]=fit$mse[400]
  pred=predict(fit,df_age[-train,])
  test.err[mtry]=with(df_age[-train,],mean((age2-pred)^2))
  cat(mtry," ")
}
matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("OOB","Test"),pch=19,col=c("red","blue"))

#Random Forests for Sex
set.seed(101)
dim(df_sex)
train=sample(1:nrow(df_sex),2937)

rf.sex=randomForest(as.factor(sex2)~.-sex,data=df_sex,subset=train)
rf.sex

oob.err=double(20)
test.err=double(20)
for(mtry in 1:20){
  fit=randomForest(sex2~.-sex,data=df_sex,subset=train,mtry=mtry,ntree=400)
  oob.err[mtry]=fit$mse[400]
  pred=predict(fit,df_sex[-train,])
  test.err[mtry]=with(df_sex[-train,],mean((sex2-pred)^2))
  cat(mtry," ")
}
matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("OOB","Test"),pch=19,col=c("red","blue"))

##Boosting Section##
boost.age=gbm(age2~.-age,data=df_age[train,],distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4)
summary(boost.age)
plot(boost.age,i="lstat")
plot(boost.age,i="rm")

n.trees=seq(from=100,to=10000,by=100)
predmat=predict(boost.age,newdata=df_age[-train,],n.trees=n.trees)

berr=with(df_age[-train,],apply( (predmat-age2)^2,2,mean))
plot(n.trees,berr,pch=19,ylab="Mean Squared Error", xlab="# Trees",main="Boosting Test Error")
abline(h=min(test.err),col="red")
