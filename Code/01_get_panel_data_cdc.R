# 01_get_panel_data_cdc.R ------------------------------------------------
# constructs state-year panel data from CDC-provided YRBS data
# output: panel_overall.csv, panel_bysex.csv, panel_bygrade.csv
# cannabis legalization and youth cannabis use, YRBS 2005-2023
# author: precious esie

# 1. load and clean data -------------------------------------------------
# second filter on mj_current occurs after variable creation below

dat_raw <- read_sas(paste0(path_data_cdc, "yrbs2023.sas7bdat")) %>%
  filter(!is.na(grade), !is.na(sex)) %>%
  mutate(
    mj_current=case_when(
      q48 == 1 ~ 0,
      q48 %in% 2:6  ~ 1
    ) %>% as.factor(),
    grade_num=as.factor(grade + 8),
    sexf=case_when(
      sex == 1 ~ "female",
      sex == 2 ~ "male"
    ) %>% as.factor(),
    yearf=as.factor(year)
  ) %>%
  filter(!is.na(mj_current), year >= 2005)

# 2. survey design -------------------------------------------------------

dsn <- svydesign(
  id = ~PSU,
  weight = ~weight,
  strata = ~stratum,
  data = dat_raw,
  nest = TRUE  # required when PSUs are nested within strata across sites
)

# 3. prevalence and count functions --------------------------------------

get_prevalence <- function(design, group_vars) {
  svyby(
    formula = ~mj_current,
    by = as.formula(paste("~", paste(group_vars, collapse=" + "))),
    FUN = svyciprop,
    vartype = "ci",
    method = "logit",  # avoids CI estimates outside [0,1]
    design = design,
    na.rm = TRUE
  ) %>%
    as_tibble() %>%
    rename(mj_current_ci_l=ci_l, mj_current_ci_u=ci_u)
}

get_counts <- function(data, group_vars) {
  data %>%
    filter(!is.na(mj_current)) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      total_n = n(),
      mj_current_0 = sum(mj_current == 0, na.rm=TRUE),
      mj_current_1 = sum(mj_current == 1, na.rm=TRUE),
      .groups="drop"
    )
}

# 4. state-year prevalence estimates -------------------------------------
# group_vars passed as join key to avoid hardcoding by stratification

panel_overall <- get_prevalence(dsn, c("year", "sitecode")) %>%
  left_join(
    get_counts(dat_raw, c("year", "sitecode")),
    by = c("year", "sitecode")
  ) %>%
  left_join(survey_year_xwalk, by="year")

panel_bysex <- get_prevalence(dsn, c("year", "sitecode", "sexf")) %>%
  left_join(
    get_counts(dat_raw, c("year", "sitecode", "sexf")),
    by = c("year", "sitecode", "sexf")
  ) %>%
  left_join(survey_year_xwalk, by="year")

panel_bygrade <- get_prevalence(dsn, c("year", "sitecode", "grade_num")) %>%
  left_join(
    get_counts(dat_raw, c("year", "sitecode", "grade_num")),
    by = c("year", "sitecode", "grade_num")
  ) %>%
  left_join(survey_year_xwalk, by="year")

# 5. save ----------------------------------------------------------------

write.csv(panel_overall, paste0(path_panel, "panel_overall.csv"), row.names = FALSE)
write.csv(panel_bysex, paste0(path_panel, "panel_bysex.csv"), row.names = FALSE)
write.csv(panel_bygrade, paste0(path_panel, "panel_bygrade.csv"), row.names = FALSE)