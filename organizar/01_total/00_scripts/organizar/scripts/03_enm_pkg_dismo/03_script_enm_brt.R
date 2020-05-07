### script enm - dismo - brt ###

# mauricio vancine
# 19-04-2018

# preparate r -------------------------------------------------------------
# memory
rm(list = ls())

# packages
library(dismo)
library(gbm)

# data --------------------------------------------------------------------
data(Anguilla_train)
head(Anguilla_train)

# fit model ---------------------------------------------------------------
# dismo
brt_dismo_elith <- dismo::gbm.step(data = Anguilla_train, gbm.x = 3:13, gbm.y = 2,
                                   family = "bernoulli", tree.complexity = 5,
                                   learning.rate = 0.005, bag.fraction = 0.5)
brt_dismo_elith
length(brt_dismo_elith$fitted)

brt_dismo <- dismo::gbm.step(data = Anguilla_train, gbm.x = 3:13, gbm.y = 2, family = "bernoulli")
brt_dismo

# gbm
brt_gbm <- gbm::gbm(Angaus ~ ., distribution = "bernoulli", data = Anguilla_train[, -1])
brt_gbm


# predict -----------------------------------------------------------------
# data
data(Anguilla_grids)
plot(Anguilla_grids[[1]])
Method <- factor('electric' , levels = levels(Anguilla_train$Method))
add <- data.frame(Method)

# predict 
# elith
brt_dismo_elith_predict <- predict(Anguilla_grids, brt_dismo_elith, const=add,
                                   n.trees = brt_dismo_elith$gbm.call$best.trees, type = "response")
brt_dismo_elith_predict
plot(brt_dismo_elith_predict)

# dismo
brt_dismo_predict <- predict(Anguilla_grids, brt_dismo, const=add, n.trees = brt_dismo$gbm.call$best.trees, type = "response")
brt_dismo_predict
plot(brt_dismo_predict)

brt_gbm_pred <- raster::predict(Anguilla_grids, brt_gbm, const=add, n.trees = 100, type = "response")
brt_gbm_pred
plot(brt_gbm_pred)

cor(brt_dismo_elith_predict[] %>% na.omit, brt_gbm_pred[] %>% na.omit)
cor(brt_dismo_elith_predict[] %>% na.omit, brt_dismo_predict[] %>% na.omit)
cor(brt_dismo_predict[] %>% na.omit, brt_gbm_pred[] %>% na.omit)
