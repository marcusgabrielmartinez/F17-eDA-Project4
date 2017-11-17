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
require(gbm)
library(pls)

data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Om1hcmN1c2dhYmUtdXQiLCJpc3MiOiJhZ2VudDptYXJjdXNnYWJlLXV0OjowYjE2NDQzOC1mYzRlLTRhNDktYWY1MC1iMWU1YjViYmIzYzMiLCJpYXQiOjE0ODQ4NjgyNjMsInJvbGUiOlsidXNlcl9hcGlfcmVhZCIsInVzZXJfYXBpX3dyaXRlIl0sImdlbmVyYWwtcHVycG9zZSI6dHJ1ZX0.Eb9i31mYAv6zQGjlze-PbiBJ_5_JNBDIZn51wcPnnNPny_ih2SSN9Ur_LVyRltEbrReNXM5b371XWrmMiexEKw"))
#vignette("quickstart", package = "data.world")

#main data set
project <- "https://data.world/tarrantrl/parkinsons-telemonitoring"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons_telemonitoring"),
  dataset = project
)

summary(df)

#2nd data set with status
project <- "https://data.world/marcusgabe-ut/parkinsons-data"
df_b <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons"),
  dataset = project
)
df_b1 = dplyr::select(df_b,-name)
df_b1 = df_b1 %>% dplyr::mutate(status2 = ifelse(status == "true", 1, 0))
df_b1 = dplyr::select(df_b1,-status)

summary(df_b1)


#Add binary column for gender
df_sex <- df %>% dplyr::mutate(sex2 = ifelse(sex == "true", 1, 0)) %>% dplyr::select(., -subject) %>% dplyr::select(., -sex)

#Add column for age. 1 = age <= 65, 0 otherwise
df_age <- df %>% dplyr::mutate(age2 = ifelse(age <= 65, 1, 0)) %>% dplyr::select(., -subject, -sex)


#remove sex
df1 <- df %>% dplyr::select(., -sex)

attach(df)
attach(df_sex)
attach(df_age)
attach(df1)

View(df)
View(df_sex)
View(df1)
View(df_b1)

#Pairs Chart#
pairs_df <- df %>% dplyr::select(., age, motor_updrs, total_updrs, jitter, shimmer, nhr, hnr, dfa)

pairs(pairs_df)


### Boosting Section ###

# 1: Boosting predicting sex2 using df_sex
train=sample(1:nrow(df_sex),2937)

boost.df_sex=gbm(sex2~.,data=df_sex[train,],distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4)
summary(boost.df_sex)
plot(boost.df_sex,i="jitter_abs")
plot(boost.df_sex,i="age")

n.trees=seq(from=100,to=10000,by=100)
predmat=predict(boost.df_sex,newdata=df_sex[-train,],n.trees=n.trees)
dim(predmat)
berr=with(df_sex[-train,],apply( (predmat-sex2)^2,2,mean))
plot(n.trees,berr,pch=19,ylab="Mean Squared Error", xlab="# Trees",main="Boosting Test Error")
#test.err=double(13)
#abline(h=min(test.err),col="red")


# 2: Boosting predicting status2 using df_b1 (2nd data set)
train=sample(1:nrow(df_b1),97)

boost.df_b1=gbm(status2~.,data=df_b1[train,],distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4)
summary(boost.df_b1)
plot(boost.df_b1,i="spread1")
plot(boost.df_b1,i="ppe")

n.trees=seq(from=100,to=10000,by=100)
predmat=predict(boost.df_b1,newdata=df_b1[-train,],n.trees=n.trees)
dim(predmat)
berr=with(df_b1[-train,],apply( (predmat-status2)^2,2,mean))
plot(n.trees,berr,pch=19,ylab="Mean Squared Error", xlab="# Trees",main="Boosting Test Error")


### Decision Trees

# 1: Build and plot tree for sex.
tree.sex=tree(sex2~.,data=df_sex)
summary(tree.sex)
plot(tree.sex)
text(tree.sex,pretty=0)
tree.sex

#Test/training for sex tree.
set.seed(1011)

train=sample(1:nrow(df_sex),2937)
tree.sex=tree(as.factor(sex2)~.-sex,df_sex,subset=train)

plot(tree.sex);text(tree.sex,pretty=0)

#predict with test data
tree.pred=predict(tree.sex,df_sex[-train,],type="class")
with(df_sex[-train,],table(tree.pred,sex2))

#Pruning for sex tree.
cv.sex=cv.tree(tree.sex,FUN=prune.tree)
cv.sex
plot(cv.sex) #lowest misclassification error rate at tree of size 17 
prune.sex=prune.tree(tree.sex,best=17)
plot(prune.sex);text(prune.sex,pretty=0)

tree.pred=predict(prune.sex,df_sex[-train,],type="class")
with(df_sex[-train,],table(tree.pred,sex2))


# 2: Decision Tree for status using df_b1
tree.status=tree(status2~.,data=df_b1)
summary(tree.status)
plot(tree.status)
text(tree.status,pretty=0)
tree.status

#Test/training for status tree.
set.seed(1011)

train=sample(1:nrow(df_b1),97)
tree.status=tree(as.factor(status2)~.,df_b1,subset=train)

plot(tree.status);text(tree.status,pretty=0)

#predict with test data
tree.pred=predict(tree.status,df_b1[-train,],type="class")
with(df_b1[-train,],table(tree.pred,status2))

#Pruning for status tree.
cv.status=cv.tree(tree.status,FUN=prune.tree)
cv.status
plot(cv.status) #lowest misclassification error rate at tree of size 
prune.status=prune.tree(tree.status,best=6)
plot(prune.status);text(prune.status,pretty=0)

tree.pred=predict(prune.status,df_b1[-train,],type="class")
with(df_b1[-train,],table(tree.pred,status2))


### PCA

pca.out=prcomp(df_age, scale=TRUE)
pca.out
names(pca.out)
biplot(pca.out, scale=0)

set.seed(2)
pca.fit = pcr(age2~., data=df_age, scale=TRUE)
pca.fit
summary(pca.fit)
validationplot(pca.fit, val.type="MSEP")
biplot(pca.fit, scale=.5)

pca.fit = prcomp(df_age, scale=TRUE)
pca.fit
summary(pca.fit)
biplot(pca.fit, scale=.5)
