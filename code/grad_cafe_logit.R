
# =============================================================================
# Title: 
# Date: November 10, 2020
# Description: 
# =============================================================================

rm(list = ls())
setwd("~/Desktop/Git/Grad_Cafe")
data_path <- "/Users/ryanlongmuir/Desktop/Python/grad_cafe/data/"

# =============================================================================
# Packages  
# =============================================================================

library(tidyverse)
library(faraway)
library(ResourceSelection)
library(pROC) 
library(xtable)
library(kableExtra)

# =============================================================================
# Functions  
# =============================================================================

xkabledply <- function(modelsmmrytable, title="Table", digits = 4,
  pos="left", bso="striped") { 
  modelsmmrytable %>%
    xtable() %>% 
    kable(caption = title, digits = digits) %>%
    kable_styling(bootstrap_options = bso, full_width = FALSE, position = pos)
}

# =============================================================================
# Code 
# =============================================================================

# -----------------------------------------------------------------------------
# Read in data  
# -----------------------------------------------------------------------------

clean_data.df <- read.csv(paste0(data_path,"clean_grad_cafe_step2_10262020.csv")) 

model_data.df <- clean_data.df %>% 
  select(acc, gpa, quant, verbal, awa, int, season, rank_tier) %>% 
  filter_all(function(x) !is.na(x)) %>% 
  filter(acc %in% c("accepted", "rejected")) %>% 
  mutate(acc = ifelse(acc == "accepted", 1, 0)) 

# -----------------------------------------------------------------------------
# Model acc
# -----------------------------------------------------------------------------

# estimate model (steps 1 through 4)
acc.m <- glm(acc ~ acc + gpa + quant + verbal, data = model_data.df, family = binomial)
summary(acc.m)

# Hosmer-Lemeshow
ht <- hoslem.test(model_data.df$acc, fitted(acc.m)) # hoslem test
cbind(ht$expected, ht$observed) 

# ROC & AUC 
p <- predict(acc.m, type = c("response"))
h <- roc(acc ~ p, data =  model_data.df)
auc(h) 
plot(h)

# -----------------------------------------------------------------------
# Logistic model
# -----------------------------------------------------------------------

# Estimate model 
acc.m <- glm(acc ~ + gpa + quant + verbal, model_data.df, family = binomial)
xkabledply(acc.m, title = "Logistic Regression")

# Accuracy 
cm <- regclass::confusion_matrix(acc.m)
accuracy_acc.m <- (cm[1,1]+cm[2,2])/(cm[1,1] + cm[1,2] + cm[2,1] + cm[2,2])*100
cm %>% kable(caption = "Table 4: Confusion Matrix of Logistic Regression") %>% kable_styling()

# ROC-AUC
wine_reviews.df$prob <- predict(acc.m, type=c("response"))
roc <- pROC::roc(p88 ~ prob, data=wine_reviews.df)
plot(roc, main = "Figure 11: ROC-AUC Curve") 

Scale dataset 
wine_reviews_z <- uzscale(wine_reviews.df, append=0, excl=c("p88"))

# Sample and subset 
wine_reviews_sample <- sample(2, nrow(wine_reviews_z), replace = TRUE, prob = c(0.75, 0.25))
wine_reviews_train <- filter(wine_reviews_z, wine_reviews_sample == 1) 
wine_reviews_test <- filter(wine_reviews_z, wine_reviews_sample == 2)

# Estimate logistic model using training data 
wine_reviews_logit <- glm(p88 ~ . -points, data = wine_reviews_train, family = binomial)
# summary(wine_reviews_logit)

wine_reviews_logit$xlevels[["taster_name"]] <- union(wine_reviews_logit$xlevels[["taster_name"]], levels(wine_reviews_test$taster_name))

# Use cutoff rule to classify type 
wine_reviews_test <- wine_reviews_test %>%
  mutate(
    p88_logit_p = predict(wine_reviews_logit, wine_reviews_test, type = c("response")),
    p88_logit = ifelse(p88_logit_p > 0.5, 1, 0)
  )

# Calculate accuracy
t <- table(wine_reviews_test$p88, wine_reviews_test$p88_logit)
# (t[1,1] + t[2,2]) / nrow(wine_reviews_test)
