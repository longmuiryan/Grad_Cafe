# =============================================================================
# Title: Animated Grad Cafe tables and charts 
# Date: November 07, 2020
# Description: 
# =============================================================================

rm(list = ls())
setwd("~/Desktop/Git/Grad_Cafe")

# =============================================================================
# 1. packages
# =============================================================================

library(tidyverse)
library(reshape2)
library(lubridate)
library(kableExtra)
library(stringr)
library(ggplot2)

# =============================================================================
# 2. hard codes
# =============================================================================

data_path <- "/Users/ryanlongmuir/Desktop/Python/grad_cafe/data/"
clean_data.df <- read.csv(paste0(data_path, "clean_grad_cafe_step2_10262020.csv"))

# =============================================================================
# 3. functions
# =============================================================================
# =============================================================================
# 4. data visualizations 
# =============================================================================

# ----------------------------------------------------------------------------
# 4.3.2 comparing mean quant scores 
# ----------------------------------------------------------------------------

acad_perf.df <- clean_data.df %>%
  filter(
    acc %in% c('accepted', 'rejected') & 
      !is.na(rank_tier) & 
      season > 12 
  ) %>% 
  group_by(acc, season, rank_tier) %>%
  summarise(
    mean_gpa = mean(gpa, na.rm = T),
    pct50_gpa = quantile(gpa, probs = .50, na.rm = T),
    mean_quant = mean(quant, na.rm = T),
    pct50_quant = quantile(quant, probs = .50, na.rm = T),
    mean_verbal = mean(verbal, na.rm = T),
    pct50_verbal = quantile(verbal, probs = .50, na.rm = T),
    mean_awa = mean(awa, na.rm = T),
    pct50_awa = quantile(awa, probs = .50, na.rm = T),
    count = n()
  ) %>%
  mutate_at(vars(contains("gpa"), contains("awa")), round, 2) %>%
  mutate_at(vars(contains("verbal"), contains("quant")), round) %>%
  arrange(acc, rank_tier) 

# plot quant data
acc_vs_decl_quant_bar.pl <- ggplot(acad_perf.df, aes(
  x = rank_tier, y = mean_quant, fill = acc
)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_cartesian(ylim = c(162, 170)) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.5),
    text = element_text(size = 14)
  ) 

acc_vs_decl_quant_bar_anim.pl <- acc_vs_decl_quant_bar.pl + 
  transition_states(
    season,
    transition_length = 1,
    state_length = 1
  ) +
  labs(
    title = "Mean GRE quant score by rank \n Season: {closest_state}",
    x = " ", y = "Score", fill = "Decision"
  ) 

#  ----------------------------------------------------------------------------
# 4.2 how do the accepted and rejected change over time? assuming that
# candidates rely less on gre scores and more on RA positionss and
#  and additional math courses, we should expect the shared regions to
# overlap more
# ----------------------------------------------------------------------------

acc_vs_decl_elipse.pl <- clean_data.df %>%
  filter(
      acc %in% c("accepted", "rejected") & 
      gpa > 3 & 
      quant > 155 
  ) %>%
  select(acc, quant, gpa, season) %>%
  filter_all(function(x) {!is.na(x)}) %>%
  ggplot(aes(x = quant, y = gpa, color = acc)) +
  geom_point() +
  stat_ellipse(
    geom = "polygon",
    alpha = .5, aes(fill = acc)
  ) + 
  guides(
    color = F
  )

acc_vs_decl_elipse_anim.pl <- acc_vs_decl_elipse.pl + 
  transition_states(
    season,
    transition_length = 1,
    state_length = 1
  ) + 
  labs(
    x = "GRE Quant",
    y = "GPA",
    fill = "Decision",
    title = "Student chartacteristics by admission decision \n Year:{closest_state}"
  ) 

#  ----------------------------------------------------------------------------
# 4.3 how do the accepted and rejected change over time? assuming that
# candidates rely less on gre scores and more on RA positionss and
#  and additional math courses, we should expect the shared regions to
# overlap more
# - Although pretty, I woudln't call this a very informative plot. I think 
# it would benefit from being animated 
# ----------------------------------------------------------------------------

# prepare data
acc_vs_decl_count.pl <- clean_data.df %>%
  filter(
    season > 12 &
      acc %in% c("accepted", "rejected") & 
      gpa > 3 & 
      quant > 155
  ) %>% 
  select(acc, quant, gpa, season) %>%
  filter_all(function(x) {!is.na(x)}) %>%
  ggplot(aes(x = quant, y = gpa, color = acc)) +
  geom_count(aes(size = after_stat(prop), group = "acc"), alpha = .5) +
  facet_wrap("acc") +
  scale_size_area(max_size = 10) + 
  guides(size = F) 

acc_vs_decl_count.pl + 
  transition_states(
    season,
    transition_length = 2,
    state_length = 1
  ) + 
  labs(
    x = "GRE Quant",
    y = "GPA",
    fill = "Decision",
    title = "Student chartacteristics by admission decision \n Year:{closest_state}"
  ) 



