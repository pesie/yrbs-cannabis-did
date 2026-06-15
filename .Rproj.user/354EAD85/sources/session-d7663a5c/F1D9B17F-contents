# 02_get_panel_data_states.R ---------------------------------------------
# constructs state-year panel data from state-provided YRBS data
# states: CT, DE, LA, OH, RI, SD
# output: panel_overall_states.csv, 
#         panel_bysex_states.csv,
#         panel_bygrade_states.csv
# cannabis legalization and youth cannabis use, YRBS 2005-2023
# author: precious esie

# 1. data processing function --------------------------------------------

process_state_data <- function(data, cannabis_var, race_var, survey_year) {
  cannabis_var <- enquo(cannabis_var)
  race_var <- enquo(race_var)
  
  tmp <- data %>%
    select(site, stratum, psu, weight, q3, q2, !!race_var, !!cannabis_var) %>%
    mutate(
      mj_current = case_when(
        (!!cannabis_var) == 1 ~ 0,
        (!!cannabis_var) %in% 2:6 ~ 1,
        TRUE                       ~ NA_real_
      ) %>% as.factor(),
      grade_num = case_when(
        q3 == 1 ~ 9,
        q3 == 2 ~ 10,
        q3 == 3 ~ 11,
        q3 == 4 ~ 12,
        TRUE    ~ NA_real_
      ) %>% as.factor(),
      sexf = case_when(
        q2 == 1 ~ "female",
        q2 == 2 ~ "male",
        TRUE ~ NA_character_
      ) %>% as.factor(),
      year = survey_year,
      yearf = as.factor(survey_year)
    ) %>%
    rename(sitecode = site)
  
  # race recode varies by variable name across survey years
  if (quo_name(race_var) == "q4") {
    tmp <- tmp %>%
      mutate(
        racef = case_when(
          (!!race_var) == 6             ~ "1_white",
          (!!race_var) == 3             ~ "2_black",
          (!!race_var) %in% c(4, 7)    ~ "3_hispanic",
          (!!race_var) %in% c(1,2,5,8) ~ "4_other",
          TRUE                          ~ NA_character_
        ) %>% as.factor()
      )
  } else {
    tmp <- tmp %>%
      mutate(
        racef = case_when(
          (!!race_var) == 5             ~ "1_white",
          (!!race_var) == 3             ~ "2_black",
          (!!race_var) %in% c(6, 7)    ~ "3_hispanic",
          (!!race_var) %in% c(1,2,4,8) ~ "4_other",
          TRUE                          ~ NA_character_
        ) %>% as.factor()
      )
  }
  
  tmp %>%
    select(sitecode, year, yearf, stratum, psu, weight,
           grade_num, sexf, racef, mj_current)
}

# 2. file loading function -----------------------------------------------

load_state_files <- function(metadata, file_type = c("sas", "csv")) {
  file_type <- match.arg(file_type)
  
  map_dfr(1:nrow(metadata), function(i) {
    df <- tryCatch({
      if (file_type == "sas") read_sas(metadata$file_path[i])
      else read.csv(metadata$file_path[i]) %>% as_tibble()
    }, error = function(e) {
      message("Error reading: ", metadata$file_path[i])
      return(NULL)
    })
    
    if (is.null(df)) return(NULL)
    
    names(df) <- tolower(names(df))
    
    process_state_data(
      df,
      !!sym(metadata$cannabis_var[i]),
      !!sym(metadata$race_var[i]),
      metadata$year[i]
    )
  })
}

# 3. metadata files ------------------------------------------------------
# file_type arg removed from make_metadata — passed separately to
# load_state_files instead

make_metadata <- function(paths, years) {
  data.frame(
    file_path = paths,
    year = years,
    stringsAsFactors = FALSE
  ) %>%
    left_join(cannabis_var_xwalk, by = "year")
}

metadata_CT <- make_metadata(
  paths = paste0(path_data_states, "Connecticut/ct_yrbs",
                 c(2005,2007,2009,2011,2013,2015,2017,2019,2021,2023),
                 ".sas7bdat"),
  years = c(2005,2007,2009,2011,2013,2015,2017,2019,2021,2023)
)

metadata_DE <- make_metadata(
  paths = paste0(path_data_states, "Delaware/de_yrbs",
                 c(2005,2007,2009,2011,2013,2015,2017,2021,2023),
                 ".sas7bdat"),
  years = c(2005,2007,2009,2011,2013,2015,2017,2021,2023)
)

metadata_LA <- make_metadata(
  paths = paste0(path_data_states, "Louisiana/la_yrbs",
                 c(2009,2011,2013,2017,2019,2021,2023),
                 ".sas7bdat"),
  years = c(2009,2011,2013,2017,2019,2021,2023)
)

metadata_OH <- make_metadata(
  paths = paste0(path_data_states, "Ohio/oh_yrbs",
                 c(2005,2007,2011,2013,2019,2021,2023),
                 ".sas7bdat"),
  years = c(2005,2007,2011,2013,2019,2021,2023)
)

metadata_RI <- make_metadata(
  paths = paste0(path_data_states, "Rhode Island/ri_yrbs",
                 c(2005,2007,2009,2011,2013,2015,2017,2019,2021,2023),
                 ".sas7bdat"),
  years = c(2005,2007,2009,2011,2013,2015,2017,2019,2021,2023)
)

metadata_SD <- make_metadata(
  paths = paste0(path_data_states, "South Dakota/sd_yrbs",
                 c(2013,2015,2019,2021,2023),
                 ".csv"),
  years = c(2013,2015,2019,2021,2023)
)

# 4. load state data -----------------------------------------------------

dat_CT <- load_state_files(metadata_CT, "sas")
dat_DE <- load_state_files(metadata_DE, "sas")
dat_LA <- load_state_files(metadata_LA, "sas")
dat_OH <- load_state_files(metadata_OH, "sas")
dat_RI <- load_state_files(metadata_RI, "sas")
dat_SD <- load_state_files(metadata_SD, "csv") %>%
  mutate(sitecode = str_trim(sitecode, side = "right"))  # trailing space in raw data

# 5. survey design objects -----------------------------------------------
# required when PSUs are nested within strata across sites

make_design <- function(data) {
  svydesign(
    id = ~psu,
    weight = ~weight,
    strata = ~stratum,
    data = data,
    nest = TRUE
  )
}

designs <- list(
  CT = make_design(dat_CT),
  DE = make_design(dat_DE),
  LA = make_design(dat_LA),
  OH = make_design(dat_OH),
  RI = make_design(dat_RI),
  SD = make_design(dat_SD)
)

raw_data <- list(
  CT = dat_CT,
  DE = dat_DE,
  LA = dat_LA,
  OH = dat_OH,
  RI = dat_RI,
  SD = dat_SD
)

# 6. state-year prevalence estimates -------------------------------------

get_prevalence_state <- function(design, group_vars) {
  svyby(
    formula = ~mj_current,
    by = as.formula(paste("~", paste(group_vars, collapse = " + "))),
    FUN = svyciprop,
    vartype = "ci",
    method = "logit",  # avoids CI estimates outside [0,1]
    design = design,
    na.rm = TRUE
  ) %>%
    as_tibble() %>%
    rename(mj_current_ci_l = ci_l, mj_current_ci_u = ci_u)
}

get_counts_state <- function(data, group_vars) {
  data %>%
    filter(!is.na(mj_current)) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      total_n = n(),
      mj_current_0 = sum(mj_current == 0, na.rm = TRUE),
      mj_current_1 = sum(mj_current == 1, na.rm = TRUE),
      .groups = "drop"
    )
}

strat_vars <- list(
  overall = c("year", "sitecode"),
  bysex = c("year", "sitecode", "sexf"),
  bygrade = c("year", "sitecode", "grade_num")
)

# iterate over stratifications then over states
# state name from names(designs) used to index raw_data
collapsed <- map(strat_vars, function(grp_vars) {
  map2_dfr(names(designs), designs, function(state, dsn) {
    prev <- get_prevalence_state(dsn, grp_vars)
    counts <- get_counts_state(raw_data[[state]], grp_vars)
    left_join(prev, counts, by = grp_vars)
  })
})

# 7. save ----------------------------------------------------------------

write.csv(collapsed$overall, paste0(path_panel, "panel_overall_states.csv"), row.names = FALSE)
write.csv(collapsed$bysex, paste0(path_panel, "panel_bysex_states.csv"), row.names = FALSE)
write.csv(collapsed$bygrade, paste0(path_panel, "panel_bygrade_states.csv"), row.names = FALSE)
