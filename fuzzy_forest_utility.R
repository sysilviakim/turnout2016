# Basic libraries ==============================================================
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
      CC16_321c = ifelse(CC16_321c == state_senate, 1,
        ifelse((CC16_321c %in% c(1, 3) & state_senate == 2) |
          (CC16_321c %in% c(2, 3) & state_senate == 1),
        2, CC16_321c)
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
      CC16_321d = ifelse(CC16_321d == state_assembly, 1,
        ifelse((CC16_321d %in% c(1, 3) & state_assembly == 2) |
          (CC16_321d %in% c(2, 3) & state_assembly == 1),
        2, CC16_321d)
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

## Variable labels -------------------------------------------------------------
data(fips)

## Make sure to check the labels with CCES 2016 Guide!! Preliminary labeling
cces_label <-
  list(
    rownames = c(
      paste0("inputstate.", fips$stfips), "birthyr", "gender.2", "educ.2", 
      paste0("votereg.", c(2, 3)), "race.2", paste0("employ.", c(2, 5, 8, 9)),
      "marstat.5", paste0("pid7.", c(4, 7, 8)), paste0("ideo5.", c(4, 5, 6)),
      "child18.2", "newsint.4", paste0("immstat.", c(2, 4, 5)), 
      paste0("healthins_", c(1, 2, 6), ".2"), 
      paste0("CC16_302.", c(2, 4)), paste0("CC16_304.", c(2, 4, 6)),
      paste0("CC16_326.", c(2, 4, 88)), "CC16_327.2",
      paste0("CC16_328.", c(2, 4, 88)),
      paste0("CC16_321", c("a.4", "b.4", "c.4", "c.88", "d.2", "d.4", "d.88")),
      paste0("CC16_320", c("a.3", "a.4", "a.5", "b.3", "b.4", "b.5", "c.2",
                           "c.5", "d.5", "d.88", "e.5", "e.88", "f.2", "f.5",
                           "g.4", "g.5", "g.88", "h.5", "h.88")),
      paste0("CC16_330", c("d.2", "e.2")),
      paste0("CC16_332", c("a.2", "b.2", "c.2", "d.2", "e.2")),
      paste0("CC16_331_", c("1.2", "2.2", "3.2", "7.2")), "CC16_335.2", 
      paste0("CC16_333", c("a.2", "a.88", "b.2", "b.88", 
                           "c.2", "c.88", "d.2", "d.88")), 
      paste0("CC16_334", c("a.2", "c.2", "d.2")),
      paste0("CC16_337_", c("1.3", "1.88", "2.2", "2.3", "2.88",
                            "3.2", "3.3", "3.88")), 
      paste0("CC16_340", c("i.8", "i.88", "h.7", "h.8", "h.88", "g.7", "g.8",
                           "g.88", "e.8", "d.7", "d.8", "c.5", "c.7", "c.8",
                           "b.8", "b.88", "a.2", "a.4", "a.6", "a.7", "a.8")), 
      paste0("CC16_351", c("B.2", "F.2", "I.2", "K.2")),
      paste0("CC16_414_", c("1.2", "2.2", "5.2", "7.2")), "CC16_415r"
    ),
    label = c(
      unlist(lapply(fips$stname, simple_cap)), "Birth year", "Gender", 
      "High-school graduate", "Not registered to vote",
      "Not sure about my voter reg. status", "Black",
      "Employed part-time", "Retired", "Student", "Employment other", "Single",
      paste0("7-pt Party ID: ", c("Independent", "Strong rep.", "Not sure")),
      paste0("My political viewpoint is: ", 
             c("conservative", "very conservative", "not sure")), 
      "No children under 18", "Interested in politics: hardly at all",
      paste0("Immigrant ", c("non-citizen", "2nd gen.", "3rd gen.")),
      paste0("Health insurance not through ", 
             c("job/family employer", "Medicare/Medicaid")), 
      "No health insurance",
      paste0("Last year nat'l economy has: gotten ", c("better", "worse")), 
      paste0("Project economy next year: ", 
             c("somewhat better", "somewhat worse", "not sure")), 
      "Voted Romney in 2012", "Voted not for Obama/Romney in 2012", 
      "Not voted in 2012", "Not voted in primaries 2016", 
      "Voted for Sanders in primary", "Voted for Trump in primary", 
      "Skipped answering primary vote status 2016",
      paste0("Not sure which party has majority: ", 
             c("House", "Senate", "State Senate")), 
      "Skipped which party has majority: State Senate",
      "Correct about which party has majority: Lower Chamber",
      "Not sure which party has majority: Lower Chamber",
      "Skipped which party has majority: Lower Chamber",
      "Somewhat disapprove of Obama", "Strongy disapprove of Obama",
      "Obama approval not sure", "Somewhat disapprove of Congress",
      "Strongy disapprove of Congress", "Congress approval not sure", 
      "Supreme court approval: somewhat approve", 
      "Supreme court approval: not sure",
      "Governor approval: not sure", "Governor approval: skipped",
      "Legislature approval: not sure", "Legislature approval: skipped",
      "Rep approval: somewhat approve", "Rep Party approval: not sure",
      paste0("Own-state Senator 1 approval: ",
             c("strongly disapprove", "not sure", "skipped")),
      paste0("Own-state Senator 2 approval: ", c("not sure", "skipped")),
      "Oppose banning assault rifles", 
      "Oppose making easier concealed gun permit", 
      paste0("Oppose abortion = ", 
             c("choice", "only when rape", "all prohibit after 20th week")),
      "Oppose employers declining coverage of abortions in insurance",
      "Oppose prohibiting federal funds for abortion",
      "Grant status to all illegal immigrants who had jobs/taxes/no crime?",
      "Increase patrol for immigration?", 
      "Grant status to immigrant children with US high school diploma?",
      "Identify and deport illegal immigration?", "Oppose gay marriage",
      "Oppose EPA regulating CO2", "Skipped EPA regulate CO2", 
      "Oppose raising required fuel efficiency",
      "Skipped raising required fuel efficiency",
      "Oppose requiring min. renewable fuel",
      "Skipped requiring min. renewable fuel",
      "Oppose strengthening Clean Air Act",
      "Skipped question on strengthening Clean Air Act", 
      "Oppose eliminating min. sentences for non-violent drug offenders",
      "Oppose increasing police 10%", 
      "Oppose increasing prison sentences for 2+ crimes felons",
      "Cut defense spending << others to cover federal deficit", 
      "Skipped answering: cut defense spending to cover fed. deficit?",
      "Cut domestic spending = 2nd preference to cover federal deficit",
      "Cut domestic spending = last preference to cover federal deficit", 
      "Skipped answering: cut domestic spending to cover fed. deficit?",
      "Raise taxes = 2nd choice to cover federal deficit",
      "Raise taxes << others to cover federal deficit",
      "Skipped answering raise taxes to cover fed. deficit",
      paste0("Supreme court polit. scale = ", c("not sure", "skipped")), 
      paste0("Rep party polit. scale = ", 
             c("very conservative", "not sure", "skipped")), 
      paste0("Dem party polit. scale = ", 
             c("very conservative", "not sure", "skipped")), 
      "Trump polit. scale = not sure",
      paste0("Clinton polit. scale = ", c("very conservative", "not sure")), 
      paste0("Obama polit. scale = ", 
             c("somewhat conservative", "very conservative", "not sure")), 
      paste0("Governor polit. scale = ", c("not sure", "skipped")), 
      paste0("I would rate myself: ", 
             c("liberal", "middle of road", "conservative", 
               "very conservative", "not sure")), 
      "Vote for Trans-Pacific Partnership Act?", 
      "Vote for Highway and Transportation Funding Act?",
      "Oppose repealing Affordable Care Act", 
      "Oppose raising minimum wage",
      "Approve of military deploy to ensure supply of oil?",
      "Approve of military deploy to destroy terrorist camp?",
      "Oppose military deployment to protect allies",
      "Approve of military deploy to ... none?",
      "Cutting spending preferred to raising taxes"
    )
  )
cces_label <- 
  data.frame(variable = cces_label$rownames, label = cces_label$label) %>%
  mutate_if(is.factor, as.character)

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