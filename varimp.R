source('fuzzy_forest_utility.R')

load("./data/turnout.Rda")
load("./output/turn.logit.Rda")
load("./output/turn.cart.Rda")
load("./output/turn.rf.Rda")
load("./output/turn.ff.Rda")

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
  dplyr::filter(logit + cart + rf + ff > 0) %>%
  rowwise() %>% 
  dplyr::left_join(., cces_label, by = c("rownames" = "variable")) 

# Counterfactuals ==============================================================
ff <- turn.ff$method$final_rf
vars <- turn.ff$method$feature_list$feature_name
test <- turn_list$test %>% dplyr::select(vars)

## Actual turnout in test set: 74.43%
round(prop.table(table(turn_list$test$depvar)) * 100, digits = 4)
## Predicted/fitted turnout in test set: 83.3%
p <- round(prop.table(table(predict(ff, test))) * 100, digits = 4)
## counterfactuals for each policy position variable
comb <- 
  expand.grid(
    c(1), 
    vars[grepl("CC16_333|CC16_351I|CC16_337|CC16_330|CC16_415r", vars)],
    stringsAsFactors = FALSE
  )

counterfactual <- data.frame()
for (i in seq(nrow(comb))) {
  var <- comb[i, ]$Var2
  val <- comb[i, ]$Var1
  val <- ifelse(var == "birthyr", ifelse(val == 0, 20, 60), val)
  val <- ifelse(var == "CC16_415r", ifelse(val == 0, 0, 100), val)
  c <- round(
    prop.table(table(predict(
      ff, test %>% dplyr::mutate(!!as.name(var) := val)
    ))) * 100, 
    digits = 10
  )
  temp <- 
    data.frame(
      ## Paper's labels translated to be more legible/intuitive
      label = paste0("Everybody ", 
                     (cces_label %>% dplyr::filter(variable == var))$label),
      out = ifelse(p["Voted"] < c["Voted"], "uparrow", "downarrow"),
      value = c["Voted"], 
      delta = c["Voted"] - p["Voted"]
    ) %>%
    mutate_if(is.factor, as.character)
  counterfactual <- dplyr::bind_rows(counterfactual, temp)
}

print(
  xtable(
    counterfactual,
    digits = 2,
    caption = 
      "Counterfactuals, Top 20 Variables Selected by Fuzzy Forests",
    label = "tab:counterfactual"
  ),
  include.rownames = FALSE,
  table.placement = "hbt!",
  booktabs = TRUE,
  caption.placement = "top",
  hline.after = c(0),
  align = "l|llllllll",
  file = "./tables/counterfactual.tex"
)

# Plotting =====================================================================
p <- varImpPlotFF(turn.ff, cces_label = cces_label)
pdf(
  width = 8,
  height = 4,
  file = "./figures/varimp.ff.pdf"
)
plot_grey(pdf_default(p))
dev.off()

# Modules ======================================================================
ff.module <- left_join(
  turn.ff$method$module_membership %>%
    rowwise() %>% 
    dplyr::mutate(
      label = 
        ifelse(
          nrow(cces_label %>% dplyr::filter(variable == feature_name)) == 0,
          NA, (cces_label %>% dplyr::filter(variable == feature_name))$label
        )
    ) %>%
    dplyr::select(features = feature_name, everything()),
  turn.ff$method$survivor_list %>%
    bind_rows(.id = "module")
) %>%
  dplyr::filter(
    !(module == "grey" & is.na(mod_varlist))
  ) %>%
  dplyr::group_by(module) %>%
  dplyr::arrange(desc(mod_varlist)) %>%
  dplyr::filter(row_number() < 15) %>%
  ungroup() %>%
  dplyr::arrange(
    module, desc(mod_varlist)
  )
save(list = c("ff.module", "varimp.df"), file = "./output/varimp.module.Rda")

print("Variable importance and FF modules summarized.")
