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

data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Om1hcmN1c2dhYmUtdXQiLCJpc3MiOiJhZ2VudDptYXJjdXNnYWJlLXV0OjowYjE2NDQzOC1mYzRlLTRhNDktYWY1MC1iMWU1YjViYmIzYzMiLCJpYXQiOjE0ODQ4NjgyNjMsInJvbGUiOlsidXNlcl9hcGlfcmVhZCIsInVzZXJfYXBpX3dyaXRlIl0sImdlbmVyYWwtcHVycG9zZSI6dHJ1ZX0.Eb9i31mYAv6zQGjlze-PbiBJ_5_JNBDIZn51wcPnnNPny_ih2SSN9Ur_LVyRltEbrReNXM5b371XWrmMiexEKw"))
#vignette("quickstart", package = "data.world")

project <- "https://data.world/tarrantrl/parkinsons-telemonitoring"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons_telemonitoring"),
  dataset = project
)

summary(df)

#Add binary column for gender
df_sex <- df %>% dplyr::mutate(sex2 = ifelse(sex == "true", 1, 0)) %>% dplyr::select(., -subject) %>% dplyr::select(., -sex)

#Add column for age. 1 = age <= 65, 0 otherwise
df_age <- df %>% dplyr::mutate(age2 = ifelse(age <= 65, 1, 0)) %>% dplyr::select(., -subject)


#remove sex
df1 <- df %>% dplyr::select(., -sex)

attach(df)
attach(df_sex)
attach(df_age)
attach(df1)

View(df)
View(df_sex)
View(df1)

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


### Decision Tree

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
