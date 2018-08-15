source("fuzzy_forest_utility.R")
load("./data/turnout.Rda")

for (choice in c("train", "train_robust")) {
  # Analysis of scale free topology for soft-thresholding ======================
  p <- WGCNA::pickSoftThreshold(turn_list[[choice]])
  x <- NULL
  if (!(is.na(p$powerEstimate))) x <- p$powerEstimate
  power_vec <- unique(c(4, 6), x)
  
  # Hyperparameters ============================================================
  ## We will later cross validate these choices
  ## With default minModuleSize, you get only the gray and turquoise module.
  controls <-
    expand.grid(
      mtry_factor = c(1),
      min_ntree = c(1000),
      ntree_factor = c(1),
      nodesize = c(1),
      final_ntree = c(1000),        ## Default is 500
      number_selected = c(20),      ## Default is 5
      power = power_vec,            ## Default is 6
      drop_fraction = c(0.5, 0.25), ## Default is 0.25
      minModuleSize = c(3),         ## Default is min(20, ncol(X) / 2).
      keep_fraction = c(0.25),      ## Default is 0.05
      nThreads = c(cl),
      TOMType = c("unsigned")
    )
  print(controls)
  
  # Covariates and dependent variable ==========================================
  X_basic <- turn_list[[choice]] %>% dplyr::select(-depvar)
  y_basic <- turn_list[[choice]]$depvar
  class(X_basic) <- "data.frame"
  set.seed(123)
  cv_index <- caret::createFolds(y_basic, k = 10) ## List of indices
  
  # Fuzzy forest loop ==========================================================
  cv_perf_wff <- cv_perf_ff <- list()
  for (i in seq(1, nrow(controls))) {
    output <- list()
    ## Parameter controls
    param_list <- ff_params(controls, i)
    ## 10-fold CV
    for (j in 1:10) {
      X <- X_basic[-unlist(cv_index[j]), ]
      y <- y_basic[-unlist(cv_index[j])]
      X_test <- X_basic[unlist(cv_index[j]), ]
      y_test <- y_basic[unlist(cv_index[j])]
      module <- blockwiseModules(
        X,
        power = controls$power[i],
        minModuleSize = controls$minModuleSize[i],
        nThreads = controls$nThreads[i]
      )
      print(paste0("Module calculated: spec. ", i, ", ", j, "-th iter."))
      model.ff <-
        ff(
          X, y,
          module_membership = module$colors,
          num_processors = cl,
          screen_params = param_list$screen_params,
          select_params = param_list$select_params,
          final_ntree = controls$final_ntree[i]
        )
      print(paste0("ff complete: spec. ", i, ", ", j, "-th iter."))
      ## Metric is ROC (i.e., AUC) 
      ff_predict <- predict(model.ff$final_rf, newdata = X_test, type = "prob")
      ff_rocr <- ROCR::prediction(ff_predict[, "Voted"], y_test)
      ff_auc <- performance(ff_rocr, "auc")
      ## Store output 
      output[[j]] <-
        list(
          X = X, y = y, X_test = X_test, y_test = y_test,
          model.ff = model.ff, module = module, param_list = param_list,
          ff_predict = ff_predict, ff_rocr = ff_rocr, ff_auc = ff_auc  
        )
      gc(reset = T)
    }
    ## Average performance?
    cv_perf_ff[[i]] <-
      unlist(lapply(output, function(x) x$ff_auc@y.values[[1]]))
    print(cv_perf_ff[[i]])
  }
  
  # Final choice of models =====================================================
  final_choice_ff <-
    which(unlist(lapply(cv_perf_ff, mean)) ==
            max(unlist(lapply(cv_perf_ff, mean))))
  ## In the rare case that there are 2+ specifications with same max level
  if (length(final_choice_ff) > 1) final_choice_ff = max(final_choice_ff)
  
  save(
    list = c(
      "controls", "cv_index", "cv_perf_ff", "final_choice_ff"
    ),
    file = ifelse(
      choice == "train", "./output/ffcv.Rda", "./output/ffcv_robust.Rda"
    )
  )

  registerDoSEQ()  
}

print("Fuzzy forest cross-validation complete.")