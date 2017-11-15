library(ggplot2)
require(dplyr)
require(ISLR)
require(boot)
library(tidyverse)
library(modelr)
require(MASS)
library(leaps)
library(glmnet)
require(gbm)
require(data.world)
project <- "https://data.world/tarrantrl/parkinsons-telemonitoring"
data.world::set_config(cfg_env("DW_API"))
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons_telemonitoring"),
  dataset = project
)

# boosting
###########
df = dplyr::select(df, -subject)
df <- df %>% dplyr::mutate(sex2 = ifelse(sex == "true", 1, 0))
df = dplyr::select(df, -sex)
#attach(df)
set.seed(1)
train = sample(nrow(df), 97)
test = df[-train,]

boost.df=gbm(age~.,data=df[train,],distribution="gaussian",n.trees=10000,shrinkage=0.01,interaction.depth=4)
boost.df
print(boost.df)
summary(boost.df)

# pca?
dimnames(df)
apply(df,2,mean)
apply(df,2, var)
pca.out=prcomp(df, scale=TRUE)
pca.out
names(pca.out)
biplot(pca.out, scale=0)

dfsubject <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons_telemonitoring"),
  dataset = project
)
# k means
x = dfsubject%>%dplyr::select(age, total_updrs)
km.out=kmeans(x,42)
km.out
par(mfrow = c(1, 2))
plot(x,col=km.out$cluster,cex=2,main = "K means clustering", pch=1,lwd=2)
plot(x,col=dfsubject$subject,cex=2,main = "Actual subject",pch=1,lwd=2)
# ggplot alternative:
km.out$cluster
dfcluster <- data.frame(x, km.out$cluster)
names(dfcluster)
dfcluster2 <- dplyr::bind_cols(dfcluster, data.frame(dfsubject$subject))

kmeansubject <- dfcluster2 %>% ggplot(aes(x=age, y=total_updrs, colour = as.factor(km.out.cluster))) + geom_point()
actualsubject <- dfcluster2 %>% ggplot(aes(x=age, y=total_updrs, colour = as.factor(dfsubject.subject))) + geom_point()

df1 <- data.frame(km.out$centers, km.out$size)
names(df1)
dfcluster2 %>% ggplot() + geom_point(mapping = aes(x=age, y=total_updrs, colour = as.factor(km.out.cluster))) + geom_point(data=df1, mapping=aes(age, total_updrs, size=km.out.size))

df %>% ggplot(aes(x=age, y = total_updrs, colour = sex2)) + geom_point()

# hierchical 
hc.complete=hclust(dist(x),method="complete")
hc.single=hclust(dist(x),method="single")
hc.average=hclust(dist(x),method="average")
par(mfrow = c(3, 1))
plot(hc.complete)
plot(hc.single)
plot(hc.average)

hc.cut=cutree(hc.complete,42)
table(hc.cut,dfsubject$subject)
hc.cut
which
table(hc.cut,km.out$cluster)

