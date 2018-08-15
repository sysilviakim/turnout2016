source('fuzzy_forest_utility.R')

load("./data/turnout.Rda")
load("./output/turn.logit.Rda")
load("./output/turn.cart.Rda")
load("./output/turn.rf.Rda")
load("./output/turn.ff.Rda")

# List of performance metrics ==================================================
outcome <-
  lapply(
    c("ff", "logit", "cart", "rf"),
    function(y)
      perf_routine(
        method = y,
        x = eval(parse(text = paste("turn", y, sep = "."))),
        test = turn_list$test
      )
  )

# In a summary dataframe =======================================================
perf <-
  data.frame(
    Method = c("Fuzzy Forest", "Logit", "CART", "Random Forest"),
    AUC = unlist(lapply(outcome, function(x)
      x$auc)),
    Accuracy = unlist(lapply(outcome, function(x)
      x$cf.matrix$overall[c("Accuracy")])),
    Precision = unlist(lapply(outcome, function(x)
      x$cf.matrix$byClas[c("Precision")])),
    Recall = unlist(lapply(outcome, function(x)
      x$cf.matrix$byClas[c("Recall")])),
    F1 = round(unlist(
      lapply(outcome, function(x)
        x$cf.matrix$byClas[c("F1")])
    ),
    digits = 4
    ),
    Runtime = round(c(
      ff.time[3], logit.time[3], cart.time[3], rf.time[3]
    ) / 60,
    digits = 1
    )
  )

names(perf)[c(ncol(perf) - 1, ncol(perf))] <-
  c("F1 Score", "Runtime (min.)")
save(perf, file = "./output/perf.Rda")

# Tables and figures showing performance comparison ============================
print(
  xtable(
    perf,
    digits = c(0, rep(4, ncol(perf) - 1), 1),
    caption = "Classification Performance Comparison",
    label = "tab:turn-perf"
  ),
  include.rownames = FALSE,
  table.placement = "hbt!",
  booktabs = TRUE,
  caption.placement = "top",
  hline.after = c(0),
  align = "l|lllllr",
  file = "./tables/perf.tex"
)

# Plotting =====================================================================
p <- roc_comparison(
  outcome,
  labels = c(
    paste0("Fuzzy Forest (", round(perf$AUC[1], digits = 4), ")"),
    paste0("Logit (", round(perf$AUC[2], digits = 4), ")"),
    paste0("CART (", round(perf$AUC[3], digits = 4), ")"),
    paste0("Random Forest (", round(perf$AUC[4], digits = 4), ")")
  )
)
pdf(
  width = 6,
  height = 4,
  file = "./figures/roc.pdf"
)
plot_grey(pdf_default(p)) + 
  theme(legend.position = c(0.75, 0.25))
dev.off()

print("Performance summarized.")