# =============================================================================
# Title: GradCafe data preparation 
# Date: November 07, 2020
# Description: 
# =============================================================================

# =============================================================================
# Packages  
# =============================================================================

# =============================================================================
# Section 
# =============================================================================

# -----------------------------------------------------------------------------
# Subsection 
# -----------------------------------------------------------------------------

rm(list = ls())
setwd("~/Desktop/Git/Grad_Cafe")

# =============================================================================
# 1. packages
# =============================================================================

library(tidyverse)
library(reshape2)
library(lubridate)
library(kableExtra)

# =============================================================================
# 2. Hard codes
# =============================================================================

# =============================================================================
# 3. Functions
# =============================================================================

# =============================================================================
# 4. Code 
# =============================================================================

# ----------------------------------------------------------------------------
# 4.1 pull in data
# ----------------------------------------------------------------------------

# list of programs
raw_econ_programs.df <- read_csv("data/usnews_ranking.csv")

# scraped data 
# note: there are some parsing failers because of the variable season, however, 
# these observations would have been filtered out later
raw_scraped_data.df <- read_csv("data/clean_grad_cafe_step1.csv.gz")

# ----------------------------------------------------------------------------
# 4.2 cleaning data 
#   - replacing values (e.g., 0 GRE Verbal -> NA)
#   - grabing individuals schools (i.e., removing repondents who report multiple
#     schools at once)
#   - merge in rank (and maybe reported gre scores later)
# ----------------------------------------------------------------------------

econ_programs.df <- raw_econ_programs.df %>% 
  mutate_if(is.character, tolower) %>% 
  mutate(
    rank_tier = case_when(
      rank %in% 1:10 ~ 1, 
      rank %in% 11:20 ~ 2, 
      rank %in% 21:30 ~ 3, 
      rank %in% 31:40 ~ 4
    )
  )

clean_data.df <- raw_scraped_data.df %>% 
  mutate_if(is.character, tolower) %>% 
  mutate_at(vars(quant, awa, verbal, gpa), as.numeric) %>% 
  mutate(
    verbal = ifelse(verbal >= 130 & verbal <= 170, verbal, NA),
    quant = ifelse(quant >= 130 & quant <= 170, quant, NA),
    awa = ifelse(awa >= 1 & awa <= 6, awa, NA),
    gpa = ifelse(gpa >= 1 & gpa <= 4, gpa, NA), 
    acc = ifelse(acc %in% c('accepted', 'rejected', 'wait list'), acc, 'other'),
    date = as.Date(date, format("%d %b %Y")),
    int = case_when(
      int == 'a' ~ "american",
      int == 'u' ~ "international, with us degree",
      int == 'i' ~ "international, without us degree",
      int %in% c('o', '?') ~ "other"
    ),
    fund = case_when(
      str_detect(note, "with|partial|some") & str_detect(note, "fund") ~ "partial",
      str_detect(note, regex("full|good|3\\d|2\\d|1\\d")) & str_detect(note, "fund") ~ "full",
      str_detect(note, "no|waitlist|wait list|without") & str_detect(note, "fund") ~ "none",
      str_detect(note, "fund") ~ "unclear",
      TRUE ~ NA_character_
    )
  ) %>% 
  right_join(econ_programs.df, by = 'inst') %>% 
  select(inst, prg, rank, rank_tier, season, date, acc, gpa, quant,
         verbal, awa, int, fund, note)

# ----------------------------------------------------------------------------
# 4.3 Inspect funding notese 
# ----------------------------------------------------------------------------

funding_notes.df <- clean_data.df %>% 
  filter(!is.na(fund) & acc == "accepted") %>% 
  select(inst, acc, fund, note)

clean_data.df %>% 
  filter(!is.na(fund) & acc == "accepted") %>% 
  group_by(fund) %>% 
  summarise(n =n())


# ----------------------------------------------------------------------------
# 4.4 Output dataset 
# ----------------------------------------------------------------------------

write.csv(clean_data.df, "data/clean_grad_cafe_step2.csv.gz"))
