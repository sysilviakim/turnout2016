source('fuzzy_forest_utility.R')

mc <- makeCluster(cl)
registerDoParallel(mc)
load("./data/turnout.Rda")
for (choice in c("train", "train_robust")) {
  # Load cross validation results ==============================================
  if (choice == "train") {
    load("./output/ffcv.Rda")
    load("./output/rfcv.Rda")
  } else {
    load("./output/ffcv_robust.Rda")
    load("./output/rfcv_robust.Rda")
  }
  
  # Logit ======================================================================
  logit.time <- system.time(
    turn.logit <-
      train(
        as.factor(depvar) ~ .,
        metric = "ROC",
        method = "glm",
        family = "binomial",
        trControl = tc,
        data = turn_list[[choice]]
      )
  )
  save(
    list = c("logit.time", "turn.logit"),
    file = ifelse(
      choice == "train", 
      "./output/turn.logit.Rda", "./output/turn.logit_robust.Rda"
    )
  )
  print("Logit run complete.")
  
  # CART =======================================================================
  cart.time <- system.time(
    turn.cart <-
      train(
        as.factor(depvar) ~ .,
        metric = "ROC",
        method = "rpart",
        trControl = tc,
        data = turn_list[[choice]]
      )
  )
  save(
    list = c("cart.time", "turn.cart"),
    file = ifelse(
      choice == "train", 
      "./output/turn.cart.Rda", "./output/turn.cart_robust.Rda"
    )
  )
  print("CART run complete.")
  
  # Random Forest ==============================================================
  rf.time <- system.time(
    turn.rf <-
      train(
        as.factor(depvar) ~ .,
        metric = "ROC",
        method = "rf",
        importance = T,
        ntree = 1000,
        tuneGrid = data.frame(.mtry = mtrys),
        trControl = tc,
        data = turn_list[[choice]]
      )
  )
  save(
    list = c("rf.time", "turn.rf"),
    file = ifelse(
      choice == "train", "./output/turn.rf.Rda", "./output/turn.rf_robust.Rda"
    )
  )
  print("RF run complete.")
 
  # Fuzzy Forest ===============================================================
  ## Analysis of scale free topology for soft-thresholding
  ff.time <- system.time(
    turn.ff <-
      ff_full(
        fxn = "ff",
        X = turn_list[[choice]] %>% dplyr::select(-depvar),
        y = turn_list[[choice]]$depvar,
        controls = controls[final_choice_ff, ],
        cluster = cl
      )
  )
  save(
    list = c("ff.time", "turn.ff"),
    file = ifelse(
      choice == "train", "./output/turn.ff.Rda", "./output/turn.ff_robust.Rda"
    )
  )
  registerDoSEQ()  
  print("FF run complete.")
}
stopCluster(mc)

