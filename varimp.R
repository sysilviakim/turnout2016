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
  dplyr::filter(logit + cart + rf + ff > 0)
varimp.df <- cces_label(varimp.df)

# Plotting =====================================================================
p <- varImpPlotFF(turn.ff)
pdf(
  width = 8,
  height = 4,
  file = "./figures/varimp.ff.pdf"
)
plot_grey(pdf_default(p))
dev.off()

# Modules ======================================================================
ff.module <- left_join(
  cces_label(turn.ff$method$module_membership, input_var = "feature_name") %>%
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