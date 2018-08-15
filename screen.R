source("fuzzy_forest_utility.R")

load("./data/turnout.Rda")
load("./output/turn.logit_robust.Rda")
load("./output/turn.cart_robust.Rda")
load("./output/turn.rf_robust.Rda")
load("./output/turn.ff_robust.Rda")
top20_list <- list()

# Split the test set again =====================================================
set.seed(123)
test_index <-
  createDataPartition(turn_list$test_robust$depvar, p = 0.8, list = FALSE)
train <- turn_list$test_robust[test_index, ]
test <- turn_list$test_robust[-test_index, ]

# Variable importance ==========================================================
varimp.df <-
  lapply(
    c("logit", "cart", "rf", "ff"),
    function(y)
      varimp_routine(
        method = y,
        choice_top = 20,
        x = eval(parse(text = paste("turn", y, sep = ".")))
      )
  ) %>%
  bind_rows() %>%
  dplyr::group_by(rownames) %>%
  dplyr::summarise(
    logit = sum(logit, na.rm = T),
    cart = sum(cart, na.rm = T),
    rf = sum(rf, na.rm = T),
    ff = sum(ff, na.rm = T)
  ) %>%
  dplyr::mutate(ff = DMwR::ReScaling(ff, 0, 100)) %>%
  dplyr::filter(logit + cart + rf + ff > 0)
varimp.df <- cces_label(varimp.df)

# Run each method only with top selected variables, from top 3 to top 20 =======
mc <- makeCluster(cl)
registerDoParallel(mc)
for (method in c("logit", "cart", "rf", "ff")) {
  for (i in seq(3, 20)) {
    top20_list[[method]][[i]] <-
      screen_routine(
        varimp.df = varimp.df,
        method = method,
        i = i,
        train = train,
        test = test,
        mtrys = floor(sqrt(ncol(train %>% dplyr::select(-depvar))))
      )
    print(paste0("method choice ", method, " and seq ", i))
  }
}
stopCluster(mc)

# Performance metrics ==========================================================
top20_auc <- lapply(
  c("logit", "cart", "rf", "ff"),
  function(x) data.frame(
      x = unlist(lapply(
        top20_list[[x]],
        function(x)
          round(as.numeric(x$ci.auc.bootstrap), digits = 4) %>%
            paste(collapse = " ")
      ))
    ) %>% dplyr::select(!!as.name(x) := x)
) %>%
  dplyr::bind_cols() %>%
  dplyr::select(
    Logit = logit,
    CART = cart,
    RF = rf, 
    FF = ff
  ) %>%
  dplyr::mutate(Number = row_number()) %>% ## Starts from 3
  reshape2::melt(., id.vars = "Number", variable.name = "Method") %>%
  tidyr::separate(value, c("min", "value", "max"), 
                  sep = " ") %>%
  dplyr::filter(!is.na(max)) %>%
  dplyr::mutate(
    value = as.numeric(value),
    min = as.numeric(min),
    max = as.numeric(max)
  )
save(
  list = c("top20_list", "varimp.df", "top20_auc"),
  file = "./output/top20_screened.Rda"
)

# Plotting =====================================================================
p <- screen_comparison(top20_auc, ylab = "AUC")
pdf(
  width = 6,
  height = 4,
  file = "./figures/screen_auc.pdf"
)
plot_grey(pdf_default(p)) + 
  ## This number is very difficult to choose! Had to do many trials
  theme(legend.position = c(0.80, 0.20))
dev.off()

# Table ========================================================================
names(top20_auc) <- c("Number of Variables Used", "Method",
                      "Lower Bd. (95% conf.)", "Mean", "Upper Bd. (95% conf.)")

print("Screening strength of each method summarized.")

