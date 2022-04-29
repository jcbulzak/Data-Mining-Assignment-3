library(caret)
library(tidyverse)
library(ggplot2)
library(glmnet)
library(gbm)
library(ggmap)
library(rpart)
library(rpart.plot)
library(randomForest)
library(rsample)
library(MASS)
library(modelr)
library(fastDummies)
library(scales)
library(here)
library(knitr)
library(readr)
library(dplyr)
library(knitr)
library(readr)
library(rmarkdown)
library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(rsample)
library(randomForest)
library(lubridate)
library(modelr)
library("devtools")
library(gbm)
library(kableExtra)
library(dplyr)
library(plyr)
library(magrittr)
library(rsample)
library(caret)


# green certification
greens <- read.csv(file.path("Data", "greenbuildings.csv"))

# generate relevant variables
greens = greens %>% 
  mutate(rent_psf = Rent * leasing_rate)


sapply(greens, function(x) sum(is.na(x)))
greens = na.exclude(greens)

#test split
green_split = initial_split(greens, prop = 0.8)
green_train = training(green_split)
green_test = testing(green_split)

#lasso model
greenY <- green_train$rent_psf
greenX <- model.matrix(rent_psf ~ (.-CS_PropertyID - cluster - Rent - leasing_rate - LEED - Energystar)^2 -1, data = green_train)

greenLasso <- cv.glmnet(x = greenX,y = greenY ,alpha = 1, nfold = 20, trace.it = 1, standardize = FALSE)

green_coef = coef(greenLasso) %>% 
  as.matrix() %>% 
  as.data.frame()%>% 
  mutate(mag = abs(s1)) %>% 
  filter(mag > 0)

green_coef <- tibble::rownames_to_column(green_coef, "VALUE")
green_coef = green_coef[2:nrow(green_coef),1]
green_coef
sqrt(greenLasso$cvm[greenLasso$lambda == greenLasso$lambda.min])

#random forest
green_forest = randomForest(rent_psf ~ (.- CS_PropertyID - LEED - Energystar - Rent - leasing_rate),
                            data = green_train, importance = TRUE, 
                            na.action = na.exclude)
modelr::rmse(green_forest, green_test)

yhat_test = predict(green_forest, green_test)
forest_howgood <- plot(yhat_test, green_test$rent_psf)

# boost
boost1 = gbm(rent_psf ~ (.- CS_PropertyID - LEED - Energystar - Rent - leasing_rate), 
             data = green_train,
             interaction.depth=5, n.trees=500, shrinkage=.05)

# Look at error curve -- stops decreasing much after ~300
gbm.perf(boost1)

# copy other dengue code for rmse stuff here
yhat_test_gbm = predict(boost1, green_test, n.trees=200)

# knitr doesn't like this
# #gradient boosted RMSE
# sqrt(mean((test.target - predictions)^2))

# rmse comparison
lasso <- sqrt(greenLasso$cvm[greenLasso$lambda == greenLasso$lambda.min])
boost1 <- rmse(boost1, green_test)
forest <- rmse(green_forest, green_test)

rmse <- c(lasso,boost1,forest)
models <- c("Lasso", "Boosted", "Random Forest")

green_rmsetable <- cbind(Model = models, RMSE = rmse)%>% 
  as.data.frame() %>% 
  kable(caption = "Out-of-Sample Model Performance")

#var importance and partial dependencies 
varimpforest <- varImpPlot(green_forest)

pd_age <-partialPlot(green_forest, green_test, 'age', las=1)
pd_size <- partialPlot(green_forest, green_test, 'size', las=1)
pd_stories <- partialPlot(green_forest, green_test, 'stories', las=1)
pd_cityrent <- partialPlot(green_forest, green_test, 'City_Market_Rent', las=1)
pd_grate <- partialPlot(green_forest, green_test, 'green_rating', las=1)