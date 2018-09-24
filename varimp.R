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
  dplyr::mutate(
    label = (cces_label %>% dplyr::filter(variable == rownames))$label
  )

# Counterfactuals ==============================================================
ff <- turn.ff$method$final_rf
vars <- turn.ff$method$feature_list$feature_name
test <- turn_list$test %>% dplyr::select(vars)

## Actual turnout in test set: 74.43%
round(prop.table(table(turn_list$test$depvar)) * 100, digits = 4)
## Predicted/fitted turnout in test set: 83.3%
p <- round(prop.table(table(predict(ff, test))) * 100, digits = 4)
## counterfactuals for each:
comb <- expand.grid(c(0, 1), vars, stringsAsFactors = FALSE)

sink("./tables/counterfactual.txt")
for (i in seq(nrow(comb))) {
  var <- comb[i, ]$Var2
  val <- comb[i, ]$Var1
  val <- ifelse(var == "birthyr", ifelse(val == 0, 20, 60), val)
  val <- ifelse(var == "CC16_415r", ifelse(val == 0, 0, 100), val)
  c <- round(
    prop.table(table(predict(
      ff, test %>% dplyr::mutate(!!as.name(var) := val)
    ))) * 100, 
    digits = 4
  )
  print(paste0(
    "Turnout ", ifelse(p["Voted"] < c["Voted"], "increases", "decreases"), 
    " when ", (cces_label %>% dplyr::filter(variable == var))$label, 
    " is ", val, " to ", c["Voted"], "%."
  ))
}
sink()

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