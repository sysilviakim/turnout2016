source("fuzzy_forest_utility.R")

load("./data/turnout.Rda")
mc <- makeCluster(cl)
registerDoParallel(mc)

# We are mimicking caret package's choice of mtry grid
# while adding another choice from the Breiman randomForest package ============
for (choice in c("train", "train_robust")) {
  mtrys <-
    c(
      floor(sqrt(ncol(turn_list[[choice]] %>% dplyr::select(-depvar)))),
      2, 
      floor((2 + (dim(turn_list[[choice]])[2] - 1)) / 2),
      dim(turn_list[[choice]])[2]
    )
  
  # Run random forest separately for each mtry and store the output ============
  rf.time.cv <- system.time(
    turn.rf <- 
      train(
        as.factor(depvar) ~ .,
        metric = "ROC",
        method = "rf",
        importance = T,
        proximity = F,
        ntree = 1000,
        tuneGrid = data.frame(.mtry = expand.grid(.mtry = mtrys)),
        trControl = tc,
        data = turn_list[[choice]]
      )
  )
  mtrys <- turn.rf$bestTune$mtry
  
  save(
    list = c(
      "rf.time.cv", "mtrys", "tc", "turn.rf", "mtrys"
    ),
    file = ifelse(
      choice == "train", "./output/rfcv.Rda", "./output/rfcv_robust.Rda"
    )
  )
}

stopCluster(mc)
print("Random forest cross-validation complete.")