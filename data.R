source('fuzzy_forest_utility.R')

# Original data ================================================================
turn <-
  read.table(
    "./data/CCES16_Common_OUTPUT_Feb2018_VV.tab",
    header = TRUE,
    fill = TRUE,
    quote = "",
    sep = "\t",
    stringsAsFactors = FALSE
  )

# FIPS =========================================================================
turn %<>%
  ## Currently, FIPS code is "01001", "01003", ... turn into numeric.
  dplyr::mutate(countyfips = as.integer(noquote(gsub("\"", "", countyfips))),
                countyfips = ifelse(is.na(countyfips), 0, countyfips))

# Using contextual variables, wrangle CC16_321 variables =======================
## 1: Got it right, 2: Got it wrong, 4: Not sure
turn <- state_senate_verify(turn)
assert_that(length(table(turn$CC16_321c)) == 3)

# multrace_99 is all "not asked" but some are NA: no need, anyway ==============
turn %<>% dplyr::select(-multrace_99)

# Can we predict if someone will have turn out? Validated version ==============
turn %<>%
  ## Both pre- and post-survey waves answered (81.9%)
  dplyr::filter(tookpost == 1) %>%
  ## p. 126 of CCES Guide 2016: validation, 3rd method
  dplyr::filter((CL_matched %in% c("\"Y\"", "\"N\"")) &
                  ## There are three blank obs (CL_matched == "")
                  ## Kick out unmatched, self-reported voters
                  ## If CL_matched is N, CC16_401 %in% (1, 2, 3, 4, 5), so...
                  !(CL_matched == "\"N\"" & (CC16_401 %in% c(5, 8, 9)))) %>%
  dplyr::mutate(
    ## Type 1 non-voter: matched non-voters
    ## Catalist - 2016 General election voting method
    ## absentee, early, mail, polling, unknown
    turnout = ifelse(CL_E2016GVM == "", 0, 1),
    ## Type 2 non-voter: non-matched and not registered to vote
    turnout = ifelse(CL_matched == "\"N\"" &
                          votereg_post == 2, 0, turnout),
    ## 2: not registered to vote. 3: don't know
    turnout = ifelse(CL_matched == "\"N\"" &
                          CC16_401 %in% c(1, 2, 3, 4), 0, turnout)
  )

# Train/test splits into a list ================================================
turn_list <- ff_data_routine(
  turn,
  dep_var = "turnout",
  levels = c(0, 1),
  labels = c("Not", "Voted"),
  double_vars = c(
    "child18num",
    "birthyr",
    "citylength_1",
    "citylength_2",
    "CC16_415r",
    "CC16_416r",
    "CC16_417c"
  )
)

save(turn_list,
     file = paste0("./data/", "turnout.Rda"))