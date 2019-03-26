# Basic libraries ==============================================================
library("plyr")
library("tidyverse")
library("magrittr")
library("Rmisc")
library("ggthemes")
library("assertthat")
library("extrafont")
library("fontcm")
library("xtable")
loadfonts()
if (!require("Kmisc")) devtools::install_github("sysilviakim/Kmisc")
library("Kmisc")

# Machine learning libraries ===================================================
library("caret")
library("fuzzyforest")
library("ROCR")
library("pROC")
library("parallel")
library("doParallel") ## doParallel::registerDoParallel
library("WGCNA")

# Setup ========================================================================
options(
  mc.cores = parallel::detectCores(),
  java.parameters = "-Xmx32g",
  digits = 4,
  scipen = 100
)
ppi <- 300
load("cces_label.Rda")

# Functions (non-graphics) =====================================================
## Reproducible seeds for trainControl -----------------------------------------
repseeds <- function(folds = 10, from = 1e+04, seed = 123) {
  set.seed(seed)
  ## (n_repeats * nresampling) + 1
  seeds <- vector(mode = "list", length = folds + 1)
  for (i in 1:folds)
    seeds[[i]] <- sample.int(n = from, from)
  seeds[[folds + 1]] <- sample.int(n = from, 1)
  return(seeds)
}

## Making independent variables ------------------------------------------------
indvar_generate <- function(df) {
  indvar1_common <-
    c("inputstate", "birthyr", "gender", "educ", "votereg", "race", "employ",
      "marstat", "pid7", "ideo5", "pew_bornagain", "pew_religimp", 
      "pew_churatd", "religpew", "child18", "newsint", "faminc", "immstat",
      "union", "unionhh", c(names(df)[grepl("healthins_", names(df))]))
  indvar2_pre <-
    c(
      paste0("CC16_30", seq(2, 4)), paste0("CC16_32", seq(6, 8)),
      names(df)[grepl("CC16_320|CC16_321|CC16_330|CC16_332", names(df))],
      paste0("CC16_331_", c(1, 2, 3, 7, 9)), "CC16_335",
      names(df)[grepl("CC16_333|CC16_334|CC16_337_", names(df))],
      paste0("CC16_340", c("i", "h", "g", "e", "d", "c", "b", "a")),
      paste0("CC16_351", c("B", "E", "F", "G", "H", "I", "K"))
    )
  indvar3_post <-
    c(names(df)[grepl("CC16_414", names(df))], "CC16_415r", "CC16_416r")
  return(
    list(
      indvar1_common = indvar1_common,
      indvar2_pre = indvar2_pre, indvar3_post = indvar3_post
    )
  )
}

## Verifying whether the respondent has a correct knowledge of the state senate
## party status ----------------------------------------------------------------
state_senate_verify <- function(df) {
  output <- df %>%
    dplyr::mutate(
      CC16_321c_orig = CC16_321c,
      CC16_321d_orig = CC16_321d,
      ## Which party has majority in ... State Senate
      ## Republicans 1 Democrats 2
      state_senate = case_when(
        .$inputstate == 1 ~ 1,  # Alabama,  Republican
        .$inputstate == 2 ~ 1,  # Alaska,   Republican
        .$inputstate == 4 ~ 1,  # Arizona,  Republican
        .$inputstate == 5 ~ 1,  # Arkansas, Republican
        .$inputstate == 6 ~ 2,  # California,  Democrat
        .$inputstate == 8 ~ 1,  # Colorado,    Republican
        .$inputstate == 9 ~ 2,  # Connecticut, Democrat
        .$inputstate == 10 ~ 2, # Delaware,    Democrat
        .$inputstate == 11 ~ 2, # District of Columbia, Democrat
        .$inputstate == 12 ~ 1, # Florida,   Republican
        .$inputstate == 13 ~ 1, # Georgia,   Republican
        .$inputstate == 15 ~ 2, # Hawaii,    Democrat
        .$inputstate == 16 ~ 1, # Idaho,     Republican
        .$inputstate == 17 ~ 2, # Illinois,  Democrat
        .$inputstate == 18 ~ 1, # Indiana,   Democrat
        .$inputstate == 19 ~ 2, # Iowa,      Democrat
        .$inputstate == 20 ~ 1, # Kansas,    Republican
        .$inputstate == 21 ~ 1, # Kentucky,  Republican
        .$inputstate == 22 ~ 1, # Louisiana, Republican (2015 election)
        .$inputstate == 23 ~ 1, # Maine,     Republican
        .$inputstate == 24 ~ 2, # Maryland,  Democrat
        .$inputstate == 25 ~ 2, # Massachusetts, Democrat
        .$inputstate == 26 ~ 1, # Michigan,    Republican
        .$inputstate == 27 ~ 2, # Minnesota,   Democrat (changed to Rep in 2016)
        .$inputstate == 28 ~ 1, # Mississippi, Republican
        .$inputstate == 29 ~ 1, # Missouri,    Republican
        .$inputstate == 30 ~ 1, # Montana,     Republican
        .$inputstate == 31 ~ 1, # Nebraska,    Republican
        .$inputstate == 32 ~ 1, # Nevada, Republican (Changed to Dem in 2016)
        .$inputstate == 33 ~ 1, # New Hampshire, Republican
        .$inputstate == 34 ~ 2, # New Jersey,    Democrat (2017 election)
        .$inputstate == 35 ~ 2, # New Mexico,    Democrat
        .$inputstate == 36 ~ 2, # New York, power-sharing agreement
        .$inputstate == 37 ~ 1, # North Carolina, Republican
        .$inputstate == 38 ~ 1, # North Dakota,   Republican
        .$inputstate == 39 ~ 1, # Ohio,     Republican
        .$inputstate == 40 ~ 1, # Oklahoma, Republican
        .$inputstate == 41 ~ 2, # Oregon,   Democrat
        .$inputstate == 42 ~ 1, # Pennsylvania,   Republican
        .$inputstate == 44 ~ 2, # Rhode Island,   Democrat
        .$inputstate == 45 ~ 1, # South Carolina, Republican
        .$inputstate == 46 ~ 1, # South Dakota,   Republican
        .$inputstate == 47 ~ 1, # Tennesee, Republican
        .$inputstate == 48 ~ 1, # Texas,    Republican
        .$inputstate == 49 ~ 1, # Utah,     Republican
        .$inputstate == 50 ~ 2, # Vermont,  Democrat
        .$inputstate == 51 ~ 1, # Virginia, Republican
        .$inputstate == 53 ~ 1, # Washington, power-sharing agreement
        .$inputstate == 54 ~ 1, # West Virginia, Republican
        .$inputstate == 55 ~ 1, # Wisconson, Republican
        .$inputstate == 56 ~ 1  # Wyoming,   Republican
      ),
      CC16_321c = ifelse(
        CC16_321c == state_senate, 1,
        ifelse(
          (CC16_321c %in% c(1, 3) & state_senate == 2) |
            (CC16_321c %in% c(2, 3) & state_senate == 1),
          2, CC16_321c
        )
      ),
      # Which party has majority in ... Lower Chamber
      # https://ballotpedia.org/Alabama_House_of_Representatives
      # https://ballotpedia.org/State_legislative_elections,_2016
      state_assembly = case_when(
        .$inputstate == 1 ~ 1,  # Alabama,  Republican
        .$inputstate == 2 ~ 1,  # Alaska,   Republican (turned Dem in 2016)
        .$inputstate == 4 ~ 1,  # Arizona,  Republican
        .$inputstate == 5 ~ 1,  # Arkansas, Republican
        .$inputstate == 6 ~ 2,  # California,  Democrat
        .$inputstate == 8 ~ 2,  # Colorado,    Democrat
        .$inputstate == 9 ~ 2,  # Connecticut, Democrat
        .$inputstate == 10 ~ 2, # Delaware,    Democrat
        .$inputstate == 11 ~ 2, # District of Columbia, Democrat
        .$inputstate == 12 ~ 1, # Florida,   Republican
        .$inputstate == 13 ~ 1, # Georgia,   Republican
        .$inputstate == 15 ~ 2, # Hawaii,    Democrat
        .$inputstate == 16 ~ 1, # Idaho,     Republican
        .$inputstate == 17 ~ 2, # Illinois,  Democrat
        .$inputstate == 18 ~ 1, # Indiana,   Democrat
        .$inputstate == 19 ~ 1, # Iowa,      Republican
        .$inputstate == 20 ~ 1, # Kansas,    Republican
        .$inputstate == 21 ~ 2, # Kentucky,  Democrat (changed to Rep in 2016)
        .$inputstate == 22 ~ 1, # Louisiana, Republican
        .$inputstate == 23 ~ 2, # Maine,     Democrat
        .$inputstate == 24 ~ 2, # Maryland,  Democrat
        .$inputstate == 25 ~ 2, # Massachusetts, Democrat
        .$inputstate == 26 ~ 1, # Michigan,      Republican
        .$inputstate == 27 ~ 1, # Minnesota,     Republican
        .$inputstate == 28 ~ 1, # Mississippi,   Republican
        .$inputstate == 29 ~ 1, # Missouri,      Republican
        .$inputstate == 30 ~ 1, # Montana,       Republican
        .$inputstate == 31 ~ 1, # Nebraska,      Republican (Unicameral)
        .$inputstate == 32 ~ 1, # Nevada, Republican (Changed to Dem in 2016)
        .$inputstate == 33 ~ 1, # New Hampshire, Republican
        .$inputstate == 34 ~ 2, # New Jersey,    Democrat
        .$inputstate == 35 ~ 1, # New Mexico, Republican (Changed to Dem 2016)
        .$inputstate == 36 ~ 2, # New York, power-sharing agreement
        .$inputstate == 37 ~ 1, # North Carolina, Republican
        .$inputstate == 38 ~ 1, # North Dakota,   Republican
        .$inputstate == 39 ~ 1, # Ohio,     Republican
        .$inputstate == 40 ~ 1, # Oklahoma, Republican
        .$inputstate == 41 ~ 2, # Oregon,   Democrat
        .$inputstate == 42 ~ 1, # Pennsylvania,   Republican
        .$inputstate == 44 ~ 2, # Rhode Island,   Republican
        .$inputstate == 45 ~ 1, # South Carolina, Republican
        .$inputstate == 46 ~ 1, # South Dakota,   Republican
        .$inputstate == 47 ~ 1, # Tennesee, Republican
        .$inputstate == 48 ~ 1, # Texas,    Republican
        .$inputstate == 49 ~ 1, # Utah,     Republican
        .$inputstate == 50 ~ 2, # Vermont,  Republican
        .$inputstate == 51 ~ 1, # Virginia, Republican
        .$inputstate == 53 ~ 2, # Washington,    Democrat
        .$inputstate == 54 ~ 1, # West Virginia, Republican
        .$inputstate == 55 ~ 1, # Wisconson, Republican
        .$inputstate == 56 ~ 1  # Wyoming,   Republican
      ),
      CC16_321d = ifelse(
        CC16_321d == state_assembly, 1,
        ifelse(
          (CC16_321d %in% c(1, 3) & state_assembly == 2) |
            (CC16_321d %in% c(2, 3) & state_assembly == 1),
          2, CC16_321d
        )
      )
    )
  return(output)
}

## Prepare for one-hot encoding (exclude ordinal Likert scales) ----------------
factorize_vars <- function(df, exceptions, varlist) {
  output <- df %>%
    dplyr::mutate_if(is.integer, as.factor) %>%
    dplyr::mutate_if(is.numeric, as.factor)
  exceptions <- as.vector(unlist(intersect(exceptions, varlist)))
  output %<>%
    dplyr::mutate_at(vars(exceptions), funs(as.character)) %>%
    dplyr::mutate_at(vars(exceptions), funs(as.numeric))
}

## Strip character-variables ---------------------------------------------------
charvar_strip <- function(df) {
  charactercols <- names(df)[which(lapply(df, class) == "character")]
  charactercols <- as.vector(charactercols)
  output <- df %>% dplyr::select(-one_of(charactercols))
  return(output)
}

## One-hot encoding ------------------------------------------------------------
one_hot <- function(df) {
  x <- predict(caret::dummyVars(~., df, fullRank = T), df)
  output <- as_data_frame(x)
  return(output)
}

## Entire routine --------------------------------------------------------------
ff_data_routine <- function(df,
                            dep_var,
                            levels,
                            labels,
                            double_vars,
                            na_substitute = 88,
                            seed = 100) {
  # Take only specified variables
  df <- df[, c(dep_var, unlist(indvar_generate(df)))]
  # Checking that there are no zero-variance columns
  assert_that(
    sum(unlist(lapply(df, function(x) length(unique(x)) < 2))) == 0
  )
  # Strip character-variables
  df <- charvar_strip(df)
  assert_that(sum(lapply(df, class) == "character") == 0)
  # Turn NA into a certain value that is not used as a response in survey
  df[is.na(df)] <- na_substitute
  # Some variables are cardinal, but otherwise prep for one-hot encoding
  df <- factorize_vars(df,
    exceptions = double_vars,
    varlist = c(setdiff(names(df), dep_var))
  )
  # Set up dependent variable
  df$depvar <-
    factor(df[[dep_var]], levels = levels, labels = labels)
  df %<>%
    dplyr::select(depvar, everything()) %>%
    dplyr::select(-!!as.name(dep_var))
  # One-hot encoding
  # The basic specification is a subset of the opt specification
  df <- one_hot(df)
  df %<>%
    # Perfect collinearity problem
    dplyr::select(
      -CC16_321c_orig.4, -CC16_321c_orig.88,
      -CC16_321d_orig.4, -CC16_321d_orig.88, -pew_churatd.88
    )
  # Dependent variable will be simply called "dependent variable"
  names(df)[grepl("depvar", names(df))] <- "depvar"
  df$depvar <-
    factor(df$depvar, levels = c(0, 1), labels = labels)
  # Partition training and testing data (80:20 and 50:50)
  set.seed(seed)
  index0.8 <- caret::createDataPartition(df$depvar, p = 0.8, list = FALSE)
  train0.8 <- df[ index0.8, ]
  test0.8 <- df[-index0.8, ]
  set.seed(seed)
  index0.5 <- caret::createDataPartition(df$depvar, p = 0.5, list = FALSE)
  train0.5 <- df[ index0.5, ]
  test0.5 <- df[-index0.5, ]
  return(
    list(
      ## 80:20 is the default
      train = train0.8, test = test0.8, index = index0.8,
      train_robust = train0.5, test_robust = test0.5, index_robust = index0.5
    )
  )
}

## Setting up parameters for fuzzy forest --------------------------------------
ff_params <- function(controls, fxn) {
  screen_params <-
    screen_control(
      drop_fraction = controls$drop_fraction,
      keep_fraction = controls$keep_fraction,
      min_ntree = controls$min_ntree,
      ntree_factor = controls$ntree_factor,
      mtry_factor = controls$mtry_factor
    )
  select_params <-
    select_control(
      drop_fraction = controls$drop_fraction,
      number_selected = controls$number_selected,
      min_ntree = controls$min_ntree,
      ntree_factor = controls$ntree_factor,
      mtry_factor = controls$mtry_factor
    )
  return(list(screen_params = screen_params, select_params = select_params))
}

## Entire fuzzy forest routine -------------------------------------------------
ff_full <- function(fxn = "ff", X, y,
                    cluster = paralle::detectCores() - 1,
                    controls) {
  param_list <- ff_params(controls, i)
  if (fxn == "ff") {
    module <- blockwiseModules(
      X,
      power = controls$power,
      minModuleSize = controls$minModuleSize,
      nThreads = controls$nThreads
    )
    method <-
      ff(X, y,
         module_membership = module$colors,
         num_processors = cluster,
         screen_params = param_list$screen_params,
         select_params = param_list$select_params,
         final_ntree = controls$final_ntree)
  } else if (fxn == "wff") {
    module <-
      WGCNA_control(power = controls$power,
                    minModuleSize = controls$minModuleSize,
                    nThreads = controls$nThreads,
                    TOMType = controls$TOMType)
    method <-
      wff(X, y,
          WGCNA_params = param_list$module_WGCNA,
          num_processors = cluster,
          screen_params = param_list$screen_params,
          select_params = param_list$select_params,
          final_ntree = controls$final_ntree,
          nodesize = controls$nodesize)
  }
  return(list(module = module, method = method, fxn = fxn))
}

## Performance calculation routine ---------------------------------------------
perf_routine <- function(method, x, test) {
  if (method %in% c("ff", "wff")) x <- x$method$final_rf
  if (method == "rf") x <- x$finalModel
  output <- list()
  output[["method"]] <- method
  output[["pred.probs"]] <- pred.probs <-
    predict(x,
      newdata = test %>% dplyr::select(-depvar),
      type = "prob"
    )
  output[["pred.factor"]] <- pred.factor <-
    factor(as.vector(ifelse(pred.probs[, "Voted"] < 0.5, "Not", "Voted")))
  output[["cf.matrix"]] <- confusionMatrix(pred.factor, test$depvar)
  output[["pred.compare"]] <- pred.compare <- prediction(
    pred.probs[, "Voted"],
    as.numeric(test$depvar) - 1
  )
  output[["auc"]] <- performance(pred.compare, "auc")@y.values[[1]]
  output[["ci.auc.delong"]] <- ci.auc(
    as.numeric(test$depvar) - 1,
    pred.probs[, "Voted"]
  )
  output[["ci.auc.bootstrap"]] <- ci.auc(
    as.numeric(test$depvar) - 1,
    pred.probs[, "Voted"],
    method = "bootstrap"
  )
  output[["perf"]] <- performance(pred.compare, "tpr", "fpr")
  output[["prec"]] <- performance(pred.compare, "prec", "rec")
  output[["f1"]] <- performance(pred.compare, "f")
  return(output)
}

## Variable importance ---------------------------------------------------------
varimp_routine <- function(method, choice_top = 20, x) {
  if (method %in% c("ff", "wff")) {
    x <- x$method$final_rf$importance %>%
      as.data.frame() %>%
      dplyr::select(Overall = MeanDecreaseAccuracy)
  } else if (method == "rf") {
    x <- varImp(x)$importance %>%
      dplyr::select(Overall = Voted, -Not)
  } else {
    x <- varImp(x)$importance
  }
  output <- x %>%
    Kmisc::rowid_matrix_to_df() %>%
    dplyr::arrange(desc(Overall)) %>%
    dplyr::filter(row_number() < (choice_top + 1)) %>%
    dplyr::select(!!as.name(method) := Overall, everything())
  return(output)
}

## Screening strength routine --------------------------------------------------
screen_routine <- function(varimp.df, method, i, train, test, mtrys) {
  vars <-
    (varimp.df %>%
      dplyr::arrange(desc(!!as.name(method))) %>%
      dplyr::filter(row_number() < (i + 1)))$rownames
  x <- train(
    depvar ~ .,
    metric = "ROC",
    method = "rf",
    importance = TRUE,
    proximity = FALSE,
    ntree = 1000,
    tuneGrid = data.frame(.mtry = expand.grid(.mtry = mtrys)),
    trControl = tc,
    data = train %>% dplyr::select(depvar, vars)
  )
  output <- list()
  output[["method"]] <- method
  output[["pred.probs"]] <- pred.probs <-
    predict(x,
      newdata = test %>% dplyr::select(-depvar),
      type = "prob"
    )
  output[["pred.factor"]] <- pred.factor <-
    factor(as.vector(ifelse(pred.probs[, "Voted"] < 0.5, "Not", "Voted")))
  output[["cf.matrix"]] <- confusionMatrix(pred.factor, test$depvar)
  output[["pred.compare"]] <- pred.compare <- prediction(
    pred.probs[, "Voted"],
    as.numeric(test$depvar) - 1
  )
  output[["auc"]] <- performance(pred.compare, "auc")@y.values[[1]]
  output[["ci.auc.delong"]] <- ci.auc(
    as.numeric(test$depvar) - 1,
    pred.probs[, "Voted"]
  )
  output[["ci.auc.bootstrap"]] <- ci.auc(
    as.numeric(test$depvar) - 1,
    pred.probs[, "Voted"],
    method = "bootstrap"
  )
  output[["perf"]] <- performance(pred.compare, "tpr", "fpr")
  output[["prec"]] <- performance(pred.compare, "prec", "rec")
  output[["f1"]] <- performance(pred.compare, "f")
  return(output)
}

# Functions (graphics) =========================================================
## Plot logit-cart-rf-ff ROC curve comparison ----------------------------------
roc_comparison <- function(outcome,
                           levels = c("ff", "logit", "cart", "rf"),
                           labels,
                           linetype = c(
                             "solid", "dotdash", "dotted", "dashed"
                           ),
                           size = 0.7,
                           position = c(0.8, 0.2)) {
  p <- lapply(
    outcome,
    function(z)
      data.frame(
        x = unlist(z$perf@x.values),
        y = unlist(z$perf@y.values),
        Method = rep(z$method, length(unlist(z$perf@x.values)))
      ) %>%
        mutate_if(is.factor, as.character)
  ) %>%
    bind_rows() %>%
    dplyr::mutate(
      Method = factor(Method, levels = levels, labels = labels)
    ) %>%
    ggplot(., aes(x = x, y = y, colour = Method, linetype = Method)) +
    geom_line(size = size) +
    scale_linetype_manual(values = linetype) +
    xlab("False Positive Rate") +
    ylab("True Positive Rate") +
    ggtitle("ROC Curves and AUCs") +
    theme(legend.position = position)
  return(p)
}

## Screen strength comparison plots --------------------------------------------
screen_comparison <- function(df, ylab,
                              levels = c("FF", "Logit", "CART", "RF"),
                              labels = c(
                                "Fuzzy Forest     ",
                                "Logit     ",
                                "CART     ",
                                "Random Forest     "
                              ),
                              linetype = c(
                                "solid", "dotdash", "dotted", "dashed"
                              ),
                              size = 0.7,
                              position = c(0.8, 0.2)) {
  p <- df %>%
    mutate_if(is.factor, as.character) %>%
    dplyr::mutate(
      Method = factor(Method, levels = levels, labels = labels)
    ) %>%
    ggplot(., aes(
      x = Number, y = value, group = Method, colour = Method, linetype = Method
    )) +
    geom_line(size = size) +
    scale_linetype_manual(values = linetype) +
    xlab("Number of Top Variables Selected from Each Method") +
    ylab(ylab) +
    ggtitle(
      paste0(
        "Random Forest Prediction Performance Using \n",
        "Top Important Variables Selected by Each Method"
      )
    ) +
    theme(legend.position = position)
  return(p)
}

## varImpPlot for fuzzy forests ------------------------------------------------
varImpPlotFF <- function(x, cces_label) {
  p <- x$method$feature_list %>% 
    dplyr::mutate(
      variable_importance = DMwR::ReScaling(variable_importance, 0, 100)
    ) %>% 
    dplyr::select(-module_membership) %>%
    rowwise() %>% 
    dplyr::mutate(
      label = (cces_label %>% dplyr::filter(variable == feature_name))$label
    ) %>%
    ggplot() + 
    aes(
      x = variable_importance,
      y = reorder(label, variable_importance)
    ) +
    geom_point() +
    ylab("Variables") +
    xlab(paste0("Variable Importance",
                "\nMean Decrease in Accuracy, OOB Estimates, ",
                "Rescaled from 0 to 100"))
  return(p)
}

## Using scale_colour_grey on top of pdf_default or png_default ----------------
plot_grey <- function(p) {
  p <- p +
    scale_colour_grey() +
    theme(
      legend.direction = "vertical",
      legend.key.width = unit(3, "line"),
      legend.title = ggplot2::element_blank()
    )
  return(p)
}

# For caret package, a trainControl object =====================================
tc <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary, ## Provides ROC summary stats
  allowParallel = TRUE,
  verboseIter = FALSE,
  seeds = repseeds(), ## Reproducible seeds
  classProbs = TRUE
)

# Clusters
cl <- parallel::detectCores() / 2
