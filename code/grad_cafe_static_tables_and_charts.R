# =============================================================================
# Title: Grad Cafe tables and charts 
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

# =============================================================================
# 2. hard codes
# =============================================================================

data_path <- "/Users/ryanlongmuir/Desktop/Python/grad_cafe/data/"
clean_data.df <- read.csv(paste0(data_path, "clean_grad_cafe_step2_10262020.csv")) %>% 
  select(-c(X, note))

# =============================================================================
# 3. functions
# =============================================================================

# =============================================================================
# 4. data visualizations 
# =============================================================================

# ----------------------------------------------------------------------------
# 4.1 code book 
# - This is good part of the paper to discuss data limitations and parsing 
#   techniques
# - 
# ----------------------------------------------------------------------------

var_names.v <- names(clean_data.df)
var_desc.v <- c(
  "Name of institution",
  "Name of program",
  "Rank of program by U.S. News and World Report",
  "Rank tier",
  "Season of application",
  "Date of decision",
  "Admission decision",
  "Grade point average",
  "GRE verbal score", 
  "GRE quant score", 
  "GRE awa score", 
  "Whether an applicant is an international student", 
  "Whether an applicant received funding"
)

codebook.pl <- data.frame(
  var_names.v,
  var_desc.v
) %>%
  kable(
    caption = "Codebook", 
    col.names = c("Variable", "Description"), 
    align = "l"
  ) %>%
  kable_styling()

# test 
codebook.pl

# ----------------------------------------------------------------------------
# 4.2 missing value questsions 
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# 4.2.1  how much data is missing overall 
# ----------------------------------------------------------------------------

missing_data.df <- clean_data.df %>%
  mutate_all(function(x) is.na(x)) %>%
  summarise_all(function(x) round(mean(x)*100)) %>%
  melt()

missing_data_all.pl <- ggplot(missing_data.df, aes(variable, value)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.5),
    text = element_text(size = 14)
  ) + 
  labs(title = "Proportion of missing values", y = "Percent", x = " ")

# test 
missing_data_all.pl 

# ----------------------------------------------------------------------------
# 4.2.2 are students who are accepted reporting their score more often
# than those who are rejected? 
# ----------------------------------------------------------------------------

missing_data.df <- clean_data.df %>%
  filter(!is.na(acc)) %>%
  group_by(acc) %>% 
  mutate_all(function(x) is.na(x)) %>%
  summarise_all(function(x) round(mean(x)*100)) %>%
  select(gpa, acc, quant, awa, verbal, fund) %>% 
  melt()

missing_data_acc.pl <- ggplot(missing_data.df, aes(variable, value, fill = acc)) +
  geom_bar(stat = "identity") +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.5),
    text = element_text(size = 14)
  ) +
  facet_wrap('acc') +
  guides(fill = F) + 
  labs(
    title = "Proportion of missing values", y = "Percent",  x = " "
  )

# test 
missing_data_acc.pl

# ----------------------------------------------------------------------------
# 4.3 what are the average academic standings among student accepted/declined
# to these top universities
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# 4.3.1  simple table organized by rank
# ----------------------------------------------------------------------------

acad_perf.df <- clean_data.df %>%
  filter(
    acc %in% c('accepted', 'rejected') & 
      !is.na(rank_tier) 
  ) %>% 
  group_by(rank_tier, acc) %>%
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

summaryStatistics.pl <- acad_perf.df %>%
  kable(
    caption = "Summary statistics",
    align = "l",
    col.names = c(
      "Rank tier",
      "Decision",
      "Mean GPA",
      "Median GPA",
      "Mean GRE Quant",
      "Median GRE Quant",
      "Mean GRE Verbal",
      "Median GRE Verbal",
      "Mean GRE AWA",
      "Median GRE AWA",
      "Count"
    )
  ) %>%
  kable_styling()


# test 
summaryStatistics.pl 



# ----------------------------------------------------------------------------
# 4.3.2 comparing mean quant scores 
# ----------------------------------------------------------------------------

# plot quant data
acc_vs_decl_quant_bar.pl <- ggplot(acad_perf.df, aes(
  x = rank_tier, y = mean_quant, fill = acc
)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_cartesian(ylim = c(162, 170)) +
  labs(
    title = "Mean GRE quant score by rank",
    x = " ", y = "Score", fill = "Decision"
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.5),
    text = element_text(size = 14)
  ) +
  geom_hline(yintercept = 166, size = 1.25, linetype = "dashed") +
  geom_text(aes(label = paste(count, "Obs.")),
            color = "white", fontface = "bold",
            position = position_dodge2(width = .9), vjust = .75, hjust = 1.15, angle = 90
  )

acc_vs_decl_quant_bar.pl

# ----------------------------------------------------------------------------
# 4.3.2 comparing mean verbal scores 
# ----------------------------------------------------------------------------

acc_vs_decl_verbal_bar.pl <- ggplot(acad_perf.df, aes(
  x = rank_tier, y = mean_verbal, fill = acc
)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  coord_cartesian(ylim = c(155, 170)) +
  labs(
    title = "Mean Verbal Reason Scores",
    x = " ", y = "Score", fill = "Decision"
  ) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.5),
    text = element_text(size = 14)
  ) +
  geom_hline(yintercept = 161, size = 1.25, linetype = "dashed") +
  geom_text(aes(label = paste(count, "Obs.")),
            color = "white", fontface = "bold",
            position = position_dodge2(width = .9), vjust = .75, hjust = 1.15, angle = 90
  )

acc_vs_decl_verbal_bar.pl

#  ----------------------------------------------------------------------------
# 4.2 how do the accepted and rejected change over time? assuming that
# candidates rely less on gre scores and more on RA positionss and
#  and additional math courses, we should expect the shared regions to
# overlap more
# ----------------------------------------------------------------------------

acc_vs_decl_elipse.pl <- clean_data.df %>%
  filter(
    season == 20 &
      acc %in% c("accepted", "rejected") & 
      gpa > 3 & 
      quant > 155 
  ) %>%
  select(acc, quant, gpa) %>%
  filter_all(function(x) {!is.na(x)}) %>%
  ggplot(aes(x = quant, y = gpa, color = acc)) +
  geom_point() +
  stat_ellipse(
    geom = "polygon",
    alpha = .5, aes(fill = acc)
  ) + 
  labs(
    x = "GRE Quant",
    y = "GPA",
    fill = "Decision"
  ) + 
  guides(
    color = F
  )

acc_vs_decl_elipse.pl

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
    season > 15 &
      acc %in% c("accepted", "rejected") & 
      gpa > 3 & 
      quant > 155
  ) %>% 
  select(acc, quant, gpa) %>%
  filter_all(function(x) {!is.na(x)}) %>%
  ggplot(aes(x = quant, y = gpa, color = acc)) +
  geom_count(aes(size = after_stat(prop), group = "acc"), alpha = .5) +
  facet_wrap("acc") +
  scale_size_area(max_size = 10) + 
  guides(size = F) + 
  labs(
    x = "GRE Quant",
    y = "GPA",
    color = "Decision"
  )

acc_vs_decl_count.pl

# ----------------------------------------------------------------------------
# 4.4 when are admissions decision being released
# - we find that students still hear back from a school within 
#   the same amount if time over 5 years. In other words, there 
#   is no evidence to suggest COVID-19 effected the admission decision 
#   timeline in 2020 
# -----------------------------------------------------------------------------

decision_date.df <- clean_data.df %>%
  filter(season > 15 & acc %in% c("accepted", "rejected", "wait list")) %>% 
  mutate(acc_days = as.numeric(date - as.Date("2010-01-01")) %% 365) %>%
  group_by(season, acc) %>%
  summarise(decision_days = mean(acc_days, na.rm = T), count = n()) %>% 
  mutate_if(is.numeric, round) %>% 
  arrange(acc, season)

# test 
decision_date.df 


