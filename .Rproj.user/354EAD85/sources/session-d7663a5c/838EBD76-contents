# 04_analysis.R ----------------------------------------------------------
# staggered difference-in-differences models
# primary and stratified analyses
# cannabis legalization and youth cannabis use, YRBS 2005-2023
# author: precious esie

# 1. did estimation function ---------------------------------------------
# fits Callaway and Sant'Anna (2021) group-time ATTs
# returns list of: overall ATT (group aggregation), event study (dynamic)
# seed set inside function for reproducibility across all model calls
# control group: not-yet-treated; estimation method: doubly robust

run_did <- function(data, gname, max_e, control_grp = "notyettreated") {
  set.seed(100)
  
  mp <- att_gt(
    yname = "mj_current",
    tname = "survyear",
    idname = "group_id",
    gname = gname,
    data = data,
    control_group = control_grp,
    base_period = "universal",
    panel = TRUE,
    allow_unbalanced_panel = TRUE,  # retains maximum observations
    est_method = "dr"
  )
  
  att <- aggte(mp, type = "group", na.rm = TRUE)
  event_study <- aggte(mp, type = "dynamic", na.rm = TRUE,
                       min_e = -4, max_e = max_e)
  
  list(att = att, event_study = event_study)
}

# 2. main models ---------------------------------------------------------
# G1 = survey cycle of nonmedical legalization
# G2 = survey cycle of retail dispensary opening
# max_e differs by exposure: more post-periods available for G1

main_models <- list(
  rcl = run_did(panel_overall_analysis, "G1", max_e = 3),
  retail = run_did(panel_overall_analysis, "G2", max_e = 2)
)

# 3. stratified models ---------------------------------------------------
# sex_models and grade_models are nested lists: exposure (rcl/retail) x stratum
# map_depth(..., 2, ...) in extract step applies at the stratum level

run_stratified <- function(data, strat_var, gname, max_e) {
  levels <- unique(data[[strat_var]])
  
  map(levels, function(lvl) {
    run_did(
      data = filter(data, .data[[strat_var]] == lvl),
      gname = gname,
      max_e = max_e
    )
  }) %>%
    set_names(levels)
}

sex_models <- list(
  rcl = run_stratified(panel_bysex_analysis, "sexf", "G1", max_e = 3),
  retail = run_stratified(panel_bysex_analysis, "sexf", "G2", max_e = 2)
)

grade_models <- list(
  rcl = run_stratified(panel_bygrade_analysis, "grade_num", "G1", max_e = 3),
  retail = run_stratified(panel_bygrade_analysis, "grade_num", "G2", max_e = 2)
)

# 4. extract results -----------------------------------------------------

extract_results <- function(model) {
  att_df <- data.frame(
    att = model$att$overall.att,
    se = model$att$overall.se
  ) %>%
    mutate(
      ci_l = att - qnorm(0.975) * se,
      ci_u = att + qnorm(0.975) * se
    )
  
  es_df <- data.frame(
    time = model$event_study$egt,
    att = model$event_study$att.egt,
    se = model$event_study$se.egt,
    crit_val = model$event_study$crit.val.egt
  ) %>%
    mutate(
      ci_l = att - crit_val * se,
      ci_u = att + crit_val * se,
      period = ifelse(time < 0, "Pre", "Post")
    )
  
  list(att = att_df, event_study = es_df)
}

results_main <- map(main_models, extract_results)
results_sex <- map_depth(sex_models, 2, extract_results)
results_grade <- map_depth(grade_models, 2, extract_results)