library(tidyverse)
library(modelr)
require(dplyr)
require(data.world)
require(MASS)
require(ISLR)
require(ggplot2)
library(leaps)
library(glmnet)

data.world::set_config(save_config(auth_token = "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJwcm9kLXVzZXItY2xpZW50Om1hcmN1c2dhYmUtdXQiLCJpc3MiOiJhZ2VudDptYXJjdXNnYWJlLXV0OjowYjE2NDQzOC1mYzRlLTRhNDktYWY1MC1iMWU1YjViYmIzYzMiLCJpYXQiOjE0ODQ4NjgyNjMsInJvbGUiOlsidXNlcl9hcGlfcmVhZCIsInVzZXJfYXBpX3dyaXRlIl0sImdlbmVyYWwtcHVycG9zZSI6dHJ1ZX0.Eb9i31mYAv6zQGjlze-PbiBJ_5_JNBDIZn51wcPnnNPny_ih2SSN9Ur_LVyRltEbrReNXM5b371XWrmMiexEKw"))
#vignette("quickstart", package = "data.world")

project <- "https://data.world/tarrantrl/parkinsons-telemonitoring"
df <- data.world::query(
  data.world::qry_sql("SELECT * FROM parkinsons_telemonitoring"),
  dataset = project
)

summary(df)

#Add binary column for gender
df <- df %>% dplyr::mutate(sex2 = ifelse(sex == "true", 1, 0))

attach(df)

#Pairs Chart#
pairs_df <- df %>% dplyr::select(., age, motor_updrs, total_updrs, jitter, shimmer, nhr, hnr, dfa)

pairs(pairs_df)

##PCA Section##

# Examples from Chapter 6
Hitters=na.omit(Hitters)
with(Hitters,sum(is.na(Salary)))

library(pls)
set.seed(2)
pca.fit = pcr(Salary~., data=Hitters, scale=TRUE)
pca.fit
summary(pca.fit)
validationplot(pca.fit, val.type="MSEP")
biplot(pca.fit, scale=.5)

library(tidyverse)
H = Hitters %>% dplyr::select(-League, -NewLeague, -Division)
pca.fit = prcomp(H, scale=TRUE)
pca.fit
summary(pca.fit)
validationplot(pca.fit, val.type="MSEP")
biplot(pca.fit, scale=.5)

# Examples from Chapter 10
dimnames(USArrests)
apply(USArrests,2,mean)
apply(USArrests,2, var)
pca.out=prcomp(USArrests, scale=TRUE)
pca.out
names(pca.out)
biplot(pca.out, scale=0)