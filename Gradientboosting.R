###########################################################################
#Gradient Boosting
###########################################################################
old <- Sys.time() # starting time
concrete <- read.table("Concrete_Data.csv", header=TRUE, sep=",", na.strings=" ")

#The column headers were changed.
names(concrete) <- c("Cement", "Slag", "Flyash", "Water", "Superplasticizer", "Coarse", "Fine", "Age", "Strength")

#Removing the duplicate rows
concrete <- concrete[!duplicated(concrete),]

#for reproducibility
set.seed(123)

#Splitting the dataset into training and test dataset.
U <- runif(n=nrow(concrete))
concrete.set <- ifelse(U <= 0.75, yes=1, no=2)

concrete.train = concrete[concrete.set==1,]
concrete.test = concrete[concrete.set==2,]

#constructing experimental grid of hyperparameters
grid = expand.grid(
  shrinkage = c(.05, 0.1, 0.3),
  interaction.depth = seq(1,9, by=2),
  bag.fraction = seq(0.45, 0.95, by=0.1), 
  optimal_trees = 0,               
  min_cverror = 0                     
)

#grid search
for(i in 1:nrow(grid)) {
  #Training the model
    set.seed(123)
    gbm.tune <- gbm(
    formula = Strength ~ Cement+Slag+Flyash+Water+Superplasticizer+Coarse+Fine+Age,
    distribution = "gaussian",
    data = concrete.train,
    n.trees = 2500,
    interaction.depth = grid$interaction.depth[i],
    shrinkage = grid$shrinkage[i],
    bag.fraction = grid$bag.fraction[i],
    cv.folds=5
)
    grid$optimal_trees[i] <- which.min(gbm.tune$cv.error)
    grid$min_cverror[i] <- sqrt(min(gbm.tune$cv.error))
    
}

  grid %>% 
  dplyr::arrange(min_cverror) %>%
  head(30)
  
  #Optimal values of hypermarameters: n.trees=1052,interaction.depth=9,shrinkage=0.05, bag.fraction = 0.65
  #Fitting the final model
  library(gbm)
  gbm.concretes <- gbm(data=concrete.train, Strength ~ Cement+Slag+Flyash+Water+Superplasticizer+Coarse+Fine+Age, distribution="gaussian", n.trees=1052,interaction.depth=9,shrinkage=0.05, bag.fraction = 0.65)
  gbm.concretes
  
  #Relative variable importance plot
  win.graph(h=7, w=6)
  summary(gbm.concretes)
  
  gbm.pred.test = predict(gbm.concretes, newdata=concrete.test, n.trees=1052)
  gbm.MSPE <- mean((gbm.pred.test - concrete.test$Strength)^2)
  gbm.MSPE
  
  gbm.pred.train=predict(gbm.concretes, newdata = concrete.train, n.trees=1052)
  gbm.sMSE <- mean((gbm.pred.train - concrete.train$Strength)^2)
  gbm.sMSE
  
  rss1 <- sum((gbm.pred.test - concrete.test$Strength) ^ 2)  ## residual sum of squares
  tss1 <- sum((concrete.test$Strength - mean(concrete.test$Strength)) ^ 2)  ## total sum of squares
  rsq1 <- 1 - rss1/tss1
  rsq1
  
  new <- Sys.time() - old 
  print(new) 
  
  
##############################################################################################
#Gradient boosting with 'Cement' and 'Water' dropped and water-cement ratio added
##############################################################################################
oldwc <- Sys.time() # starting time
concretewc.train = cbind(concrete.train$Water/concrete.train$Cement, concrete.train[,c(-1,-4)])
concretewc.test = cbind(concrete.test$Water/concrete.test$Cement,concrete.test[,c(-1,-4)])
  
colnames(concretewc.train)[which(colnames(concretewc.train)=="concrete.train$Water/concrete.train$Cement")] <- "wcratio"
colnames(concretewc.test)[which(colnames(concretewc.test)=="concrete.test$Water/concrete.test$Cement")] <- "wcratio"


#constructing experimental grid of hyperparameters
gridwc = expand.grid(
  shrinkage = c(.05, 0.1, 0.3),
  interaction.depth = seq(1,9, by=2),
  bag.fraction = seq(0.45, 0.95, by=0.1), 
  optimal_trees = 0,               
  min_cverror = 0                     
)

#grid search
for(i in 1:nrow(gridwc)) {
  #Training model
  set.seed(123)
  gbmwc.tune <- gbm(
    formula = Strength ~ wcratio+Slag+Flyash+Superplasticizer+Coarse+Fine+Age,
    distribution = "gaussian",
    data = concretewc.train,
    n.trees = 2500,
    interaction.depth = gridwc$interaction.depth[i],
    shrinkage = gridwc$shrinkage[i],
    bag.fraction = gridwc$bag.fraction[i],
    cv.folds=5
  )
  gridwc$optimal_trees[i] <- which.min(gbmwc.tune$cv.error)
  gridwc$min_cverror[i] <- sqrt(min(gbmwc.tune$cv.error))
  
}

gridwc %>% 
  dplyr::arrange(min_cverror) %>%
  head(10)
#Optimal values of hypermarameters: n.trees=1861,interaction.depth=3,shrinkage=0.1, bag.fraction = 0.85

#Fitting final model
library(gbm)
gbmwc.concretes <- gbm(data=concretewc.train, Strength ~ wcratio+Slag+Flyash+Superplasticizer+Coarse+Fine+Age, distribution="gaussian", n.trees=1861,interaction.depth=3,shrinkage=0.1, bag.fraction = 0.85)
gbmwc.concretes

#Relative variable importance plot
win.graph(h=7, w=6)
summary(gbmwc.concretes)

gbmwc.pred.test = predict(gbmwc.concretes, newdata=concretewc.test, n.trees=1861)
gbmwc.MSPE <- mean((gbmwc.pred.test - concretewc.test$Strength)^2)
gbmwc.MSPE

gbmwc.pred.train=predict(gbmwc.concretes, newdata = concretewc.train, n.trees=1861)
gbmwc.sMSE <- mean((gbmwc.pred.train - concretewc.train$Strength)^2)
gbmwc.sMSE

rss1wc <- sum((gbmwc.pred.test - concretewc.test$Strength) ^ 2)  ## residual sum of squares
tss1wc <- sum((concretewc.test$Strength - mean(concretewc.test$Strength)) ^ 2)  ## total sum of squares
rsq1wc <- 1 - rss1wc/tss1wc
rsq1wc

newwc <- Sys.time() - oldwc 
print(newwc)
