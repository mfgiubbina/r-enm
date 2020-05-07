# sdm
library(sdm)

# preparate data
sdm_data <- sdm::sdmData(pb ~ ., 
                         train = rbind(train, test),
                         bg = list(n = 10000, method = 'gRandom', remove = TRUE))
sdm_data

sdmData(species~., train=sp, predictors= bio, bg=list(n=10000,method='gRandom',remove=TRUE))

# Example 3: fit using 5 models, and 
# evaluates using 10 runs of subsampling replications taking 30 percent as test:
sdm_fit <- sdm::sdm(pb ~ ., 
                    data = sdm_data,
                    methods = c("mlp"),
                    replication = "sub",
                    test.percent = 30,
                    n = 10)


sdm_fit <- sdm::sdm(pb ~ ., 
                    data = sdm_data,
                    replication = "sub",
                    test.percent = 30,
                    n = 10,
                    methods = c(
                      
                      # only presence - envelop
                      "bioclim", 
                      "bioclim.dismo", 
                      
                      # only presence - distance
                      "domain.dismo", 
                      "mahal.dismo", 
                      
                      # presence/background - statisctics
                      "maxlike",
                      
                      # presence/background - machine learning
                      "maxent", 
                      
                      # presence/absence - statisctics
                      "gam", 
                      "glm", 
                      "mars",
                      
                      # presence/absence - machine learning
                      "brt", 
                      "cart",
                      "fda", 
                      "mda",
                      "mlp", 
                      "rbf", 
                      "rpart", 
                      "glmnet", 
                      
                      "rf",
                      "svm"
                    ),
)
sdm_fit

# information
getModelInfo(sdm_fit)

# predition
sdm_pred <- predict(object = sdm_fit, newdata = var) 
sdm_pred

# ensemble
ens <- sdm::ensemble(x = sdm_fit,
                     newdata = var,
                     setting = list(method = "weighted", stat = "AUC"))
ens
plot(ens)


# methods 
sdm::getmethodNames() %>% names