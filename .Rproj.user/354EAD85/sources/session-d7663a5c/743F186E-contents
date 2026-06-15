# 05_figures.R -----------------------------------------------------------
# all figures for manuscript and supplemental materials
# cannabis legalization and youth cannabis use, YRBS 2005-2023
# author: precious esie

# 1. theme ---------------------------------------------------------------

theme_yrbs <- function() {
  theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 10)
    )
}

scaleFUN <- function(x) sprintf("%.1f", x)

# 2. figure 1: mean cannabis use over time by treatment status -----------
# panel a: by rcl status
# panel b: by retail status

rcl_means <- panel_overall_analysis %>%
  group_by(ever_trt_rcl, year) %>%
  summarise(mj_current = mean(mj_current), .groups = "drop")

retail_means <- panel_overall_analysis %>%
  group_by(ever_trt_retail, year) %>%
  summarise(mj_current = mean(mj_current), .groups = "drop")

fig1a <- ggplot(rcl_means,
                aes(x = year, y = mj_current,
                    group = ever_trt_rcl, color = ever_trt_rcl)) +
  geom_point(size = 3) +
  geom_line(lty = 3, linewidth = 0.5) +
  scale_color_manual(values = c("black", "dodgerblue"),
                     labels = c("No", "Yes")) +
  scale_x_continuous(breaks = seq(2005, 2023, by = 2)) +
  scale_y_continuous(breaks = seq(0, 30, by = 10),
                     limits = c(0, 30), labels = scaleFUN) +
  labs(y = "Past month cannabis use (%)", x = "",
       color = "Legalized cannabis before 2023") +
  theme_yrbs()

fig1b <- ggplot(retail_means,
                aes(x = year, y = mj_current,
                    group = ever_trt_retail, color = ever_trt_retail)) +
  geom_point(size = 3) +
  geom_line(lty = 3, linewidth = 0.5) +
  scale_color_manual(values = c("black", "darkorchid"),
                     labels = c("No", "Yes")) +
  scale_x_continuous(breaks = seq(2005, 2023, by = 2)) +
  scale_y_continuous(breaks = seq(0, 30, by = 10),
                     limits = c(0, 30), labels = scaleFUN) +
  labs(y = "Past month cannabis use (%)", x = "",
       color = "Opened dispensaries before 2023") +
  theme_yrbs()

# 3. figure 2: event study plots — main models ---------------------------

plot_event_study <- function(es_df, title = NULL) {
  ggplot(es_df, aes(x = time, y = att)) +
    geom_hline(yintercept = 0, lty = 1) +
    geom_vline(xintercept = -0.5, lty = 2) +
    geom_errorbar(aes(ymin = ci_l, ymax = ci_u),
                  linewidth = 0.5, width = 0.1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = seq(min(es_df$time), max(es_df$time), by = 1)) +
    scale_y_continuous(labels = number_format(accuracy = 0.1)) +
    labs(title = title,
         x = "Survey waves from cannabis policy",
         y = "Coefficient estimates") +
    theme_yrbs()
}

fig2a <- plot_event_study(results_main$rcl$event_study,
                          title = "Nonmedical legalization")
fig2b <- plot_event_study(results_main$retail$event_study,
                          title = "Retail dispensary opening")

# 4. figure 3: overall ATT forest plots ----------------------------------
# imap_dfr carries stratum name through as group label

plot_att_forest <- function(results_list, title = NULL) {
  att_df <- imap_dfr(results_list,
                     ~ mutate(.x$att, group = .y)) %>%
    mutate(group = factor(group, levels = rev(unique(group))))
  
  ggplot(att_df, aes(x = att, y = group)) +
    geom_vline(xintercept = 0, lty = 2, linewidth = 0.5) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = ci_l, xmax = ci_u), height = 0.2) +
    labs(title = title, x = "Overall effect estimate", y = NULL) +
    theme_yrbs()
}

fig3a <- plot_att_forest(results_sex$rcl,
                         title = "By sex: nonmedical legalization")
fig3b <- plot_att_forest(results_sex$retail,
                         title = "By sex: retail dispensary opening")
fig3c <- plot_att_forest(results_grade$rcl,
                         title = "By grade: nonmedical legalization")
fig3d <- plot_att_forest(results_grade$retail,
                         title = "By grade: retail dispensary opening")

# 5. figure 4: individual state plots ------------------------------------
# one plot per treated state showing treated state vs all control states
# vertical line marks year of nonmedical legalization

state_list <- list(
  list("CO",  2012, "Colorado"),
  list("AK",  2014, "Alaska"),
  list("CA",  2016, "California"),
  list("MA",  2016, "Massachusetts"),
  list("NV",  2016, "Nevada"),
  list("MI",  2018, "Michigan"),
  list("VT",  2018, "Vermont"),
  list("IL",  2019, "Illinois"),
  list("AZB", 2020, "Arizona"),
  list("MT",  2020, "Montana"),
  list("NJ",  2020, "New Jersey"),
  list("CT",  2021, "Connecticut"),
  list("NM",  2021, "New Mexico"),
  list("NYA", 2021, "New York"),
  list("VA",  2021, "Virginia"),
  list("MO",  2022, "Missouri"),
  list("MD",  2022, "Maryland"),
  list("RI",  2022, "Rhode Island")
)

plot_state <- function(site, legalization_year, label) {
  panel_overall_analysis %>%
    filter(ever_trt_rcl == 0 | sitecode == site) %>%
    mutate(ever_trt_rcl = factor(ever_trt_rcl,
                                 levels = c("1", "0"),
                                 ordered = TRUE)) %>%
    ggplot(aes(x = year, y = mj_current,
               group = sitecode, color = ever_trt_rcl,
               alpha = ever_trt_rcl)) +
    geom_vline(xintercept = legalization_year, lty = 2, linewidth = 0.5) +
    geom_point(size = 3) +
    geom_line(lty = 3, linewidth = 0.5) +
    scale_color_manual(values = c("dodgerblue", "gray50")) +
    scale_alpha_manual(values = c(1, 0.15)) +
    scale_x_continuous(breaks = seq(2005, 2023, by = 2)) +
    scale_y_continuous(breaks = seq(0, 30, by = 10),
                       limits = c(0, 30), labels = scaleFUN) +
    labs(title = label, y = "Past month cannabis use (%)", x = "") +
    guides(color = "none", alpha = "none") +
    theme_yrbs()
}

fig4_list <- map(state_list, ~ plot_state(.x[[1]], .x[[2]], .x[[3]]))
names(fig4_list) <- map_chr(state_list, ~ .x[[3]])

# 6. save figures --------------------------------------------------------

ggsave(paste0(path_results, "fig1a_rcl_trends.png"), fig1a, width = 8, height = 5, dpi = 150)
ggsave(paste0(path_results, "fig1b_retail_trends.png"), fig1b, width = 8, height = 5, dpi = 150)
ggsave(paste0(path_results, "fig2a_es_rcl.png"), fig2a, width = 7, height = 5, dpi = 150)
ggsave(paste0(path_results, "fig2b_es_retail.png"), fig2b, width = 7, height = 5, dpi = 150)
ggsave(paste0(path_results, "fig3a_att_sex_rcl.png"), fig3a, width = 6, height = 4, dpi = 150)
ggsave(paste0(path_results, "fig3b_att_sex_retail.png"), fig3b, width = 6, height = 4, dpi = 150)
ggsave(paste0(path_results, "fig3c_att_grade_rcl.png"), fig3c, width = 6, height = 4, dpi = 150)
ggsave(paste0(path_results, "fig3d_att_grade_retail.png"), fig3d, width = 6, height = 4, dpi = 150)

walk2(fig4_list, names(fig4_list), ~ ggsave(
  paste0(path_results, "fig4_", tolower(gsub(" ", "_", .y)), ".png"),
  .x, width = 7, height = 5, dpi = 150
))
