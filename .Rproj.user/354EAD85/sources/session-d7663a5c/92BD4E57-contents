# 03_data_management.R --------------------------------------------------
# merges panel data with legalization timelines
# creates treatment indicators and analysis-ready datasets
# cannabis legalization and youth cannabis use, YRBS 2005-2023
# author: precious esie

# 1. load panel data -----------------------------------------------------
# multiply prevalence estimates by 100 to convert to percentage

read_panel <- function(file) {
  read.csv(paste0(path_panel, file)) %>%
    mutate(across(
      c("mj_current", "mj_current_ci_l", "mj_current_ci_u"),
      ~ . * 100
    ))
}

panel_overall <- read_panel("panel_overall.csv")
panel_bysex <- read_panel("panel_bysex.csv")
panel_bygrade <- read_panel("panel_bygrade.csv")

# 2. load legalization data ----------------------------------------------

state_legalization <- read.csv(paste0(path_panel, "state_legalization.csv")) %>%
  mutate(
    # align legalization years to nearest YRBS survey cycle
    # even years shift to next odd year; odd years shift two forward
    rcl_adj = ifelse(rcl %% 2 == 0, rcl + 1, rcl + 2),
    retail_adj = ifelse(retail %% 2 == 0, retail + 1, retail + 2)
  ) %>%
  left_join(survey_year_xwalk, by = c("rcl_adj" = "year")) %>%
  rename(rcl_survyear = survyear) %>%
  left_join(survey_year_xwalk, by = c("retail_adj" = "year")) %>%
  rename(retail_survyear = survyear) %>%
  mutate(
    # exclude states that legalized after 2022
    rcl_adj = ifelse(rcl_adj > 2023, NA, rcl_adj),
    rcl_survyear = ifelse(rcl_survyear > 17, NA, rcl_survyear),
    retail_adj = ifelse(retail_adj > 2023, NA, retail_adj),
    retail_survyear = ifelse(retail_survyear > 17, NA, retail_survyear)
  )

# 3. merge and create treatment indicators -------------------------------
# mcl (medical cannabis) is retained in calendar years for use as a
# covariate only — not aligned to survey cycles

add_treatment_vars <- function(data) {
  data %>%
    # right join ensures all survey years are present even if missing in panel
    right_join(survey_year_xwalk, ., by = "year") %>%
    left_join(state_legalization, by = "sitecode") %>%
    mutate(
      mcl_yn = case_when(
        mcl <= year  ~ 1,
        mcl > year   ~ 0,
        is.na(mcl)   ~ 0
      ),
      trt_rcl = case_when(
        rcl_adj <= year  ~ 1,
        rcl_adj > year   ~ 0,
        is.na(rcl_adj)   ~ 0
      ) %>% as.factor(),
      trt_retail = case_when(
        retail_adj <= year ~ 1,
        retail_adj > year  ~ 0,
        is.na(retail_adj)  ~ 0
      ) %>% as.factor(),
      ever_trt_rcl = ifelse(!is.na(rcl_adj), 1, 0) %>% as.factor(),
      ever_trt_retail = ifelse(!is.na(retail_adj), 1, 0) %>% as.factor(),
      trt_time_rcl = case_when(
        !is.na(rcl_survyear)  ~ survyear - rcl_survyear,
        is.na(rcl_survyear)   ~ 999,
        rcl_survyear > 17     ~ 999
      ),
      trt_time_retail = case_when(
        !is.na(retail_survyear) ~ survyear - retail_survyear,
        is.na(retail_survyear)  ~ 999,
        retail_survyear > 17    ~ 999
      ),
      # G1/G2: first survey cycle of treatment; 0 = never treated
      # integer required by did package
      G1 = coalesce(rcl_survyear, 0L),
      G2 = coalesce(retail_survyear, 0L)
    ) %>%
    group_by(sitecode) %>%
    mutate(group_id = cur_group_id()) %>%
    ungroup() %>%
    filter(year >= 2005) %>%
    # remove observations with 4+ survey cycles post-legalization
    filter(trt_time_rcl < 4 | trt_time_rcl == 999)
}

panel_overall_analysis <- add_treatment_vars(panel_overall)
panel_bysex_analysis <- add_treatment_vars(panel_bysex)
panel_bygrade_analysis <- add_treatment_vars(panel_bygrade)

# 4. save ----------------------------------------------------------------

saveRDS(panel_overall_analysis, paste0(path_panel, "panel_overall_analysis.rds"))
saveRDS(panel_bysex_analysis, paste0(path_panel, "panel_bysex_analysis.rds"))
saveRDS(panel_bygrade_analysis, paste0(path_panel, "panel_bygrade_analysis.rds"))

message("Analysis datasets saved to ", path_panel)