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

data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Om1hcmN1c2dhYmUtdXQiLCJpc3MiOiJhZ2VudDptYXJjdXNnYWJlLXV0OjowYjE2NDQzOC1mYzRlLTRhNDktYWY1MC1iMWU1YjViYmIzYzMiLCJpYXQiOjE0ODQ4NjgyNjMsInJvbGUiOlsidXNlcl9hcGlfcmVhZCIsInVzZXJfYXBpX3dyaXRlIl0sImdlbmVyYWwtcHVycG9zZSI6dHJ1ZX0.Eb9i31mYAv6zQGjlze-PbiBJ_5_JNBDIZn51wcPnnNPny_ih2SSN9Ur_LVyRltEbrReNXM5b371XWrmMiexEKw"))
#vignette("quickstart", package = "data.world")

project <- "https://data.world/tarrantrl/parkinsons-telemonitoring"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons_telemonitoring"),
  dataset = project
)

summary(df)

#Add binary column for gender
df_sex <- df %>% dplyr::mutate(sex2 = ifelse(sex == "true", 1, 0)) %>% dplyr::select(., -subject)

#Add column for age. 1 = age <= 65, 0 otherwise
df_age <- df %>% dplyr::mutate(age2 = ifelse(age <= 65, 1, 0)) %>% dplyr::select(., -subject)

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

# ##Support Vector Machine Section## Not working!
# 
# set.seed(10111)
# x=matrix(rnorm(40),20,2)
# y=rep(c(-1,1),c(10,10))
# x[y==1,]=x[y==1,]+1
# plot(x,col=y+3,pch=19)
# 
# library(e1071)
# #dat=data.frame(x,y=as.factor(y))
# svmfit=svm(sex2~.,data=df,kernel="linear",cost=10,scale=FALSE)
# print(svmfit)
# plot(svmfit,df)
# 
# make.grid=function(x,n=75){
#   grange=apply(x,2,range)
#   x1=seq(from=grange[1,1],to=grange[2,1],length=n)
#   x2=seq(from=grange[1,2],to=grange[2,2],length=n)
#   expand.grid(X1=x1,X2=x2)
# }
# xgrid=make.grid(x)
# ygrid=predict(svmfit,xgrid)
# plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
# points(x,col=y+3,pch=19)
# points(x[svmfit$index,],pch=5,cex=2)
# 
# beta=drop(t(svmfit$coefs)%*%x[svmfit$index,])
# beta0=svmfit$rho
# plot(xgrid,col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
# points(x,col=y+3,pch=19)
# points(x[svmfit$index,],pch=5,cex=2)
# abline(beta0/beta[2],-beta[1]/beta[2])
# abline((beta0-1)/beta[2],-beta[1]/beta[2],lty=2)
# abline((beta0+1)/beta[2],-beta[1]/beta[2],lty=2)

