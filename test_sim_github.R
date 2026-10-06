rm(list=ls())
set.seed(1234)

# F1 similarity and fairness eval simulation
pre_process <- c("drop", "class_weights", "none") 
post_process <- c('threshold', "none")
model <- c("lr", "xgb")
outcomes <- c('ideation', "attempt")
imps <- c("iter", "knn")

# This is the number of combinations that we do in the modeling pipeline for the manuscripts 
grid <- expand.grid(outcoome = outcomes, 
                    imputation = imps, 
                    models = model,
                    pre = pre_process, 
                    post = post_process)


dist_df <- data.frame()

# Sim:
# For Xgboost, dropping race, no post processing 
# it will have a fair model (eo <= 10 were we hypothesize test it and p value < 0.05)
# and f1 score no different from doing no pre and post processing for XgBoost 

n_samples <- 1000
mean_diff <- 0.01
for(out in 1:length(outcomes)){
  for(imp in 1:length(imps)){
    for(mod in 1:length(model)){
      for(pre in 1:length(pre_process)){
        for(post in 1:length(post_process)){
          
          if(outcomes[out] == "ideation" & imps[imp] == "iter" & model[mod] == "xgb" & pre_process[pre ] == "none" & post_process[post] == "none"){
            
            dist_df <- dist_df %>%
              bind_rows(
                tibble(
                  f1 = rnorm(n = n_samples, mean = 0.30, sd = 0.05),
                  eo = rnorm(n = n_samples, mean = 0.25, sd = 0.05), # Unfair model
                  outcome = outcomes[out], 
                  imputation = imps[imp], 
                  model_type = model[mod],
                  pre_type =  pre_process[pre ],
                  post_type =  post_process[post]
                ) %>%
                  mutate(
                    eo = abs(eo)
                  )
              )
            
            
          } else if (outcomes[out] == "ideation" & imps[imp] == "iter" &  model[mod] == "xgb" & pre_process[pre ] == "drop" & post_process[post] == "none"){
            
            dist_df <- dist_df %>%
              bind_rows(
                tibble(
                  f1 = rnorm(n = n_samples, mean = 0.30 - mean_diff, sd = 0.05), # slightly worse f1 than doing no pre or post processing 
                  eo = rnorm(n = n_samples, mean = 0.08, sd = 0.03), # but fair 
                  outcome = outcomes[out], 
                  imputation = imps[imp], 
                  model_type = model[mod],
                  pre_type =  pre_process[pre ],
                  post_type =  post_process[post]
                ) %>%
                  mutate(
                    eo = abs(eo)
                  )
              )
            
            
          } else { # not writing every other combination so just doing this
            
            dist_df <- dist_df %>%
              bind_rows(
                tibble(
                  f1 = rnorm(n = n_samples, mean = 0.60, sd = 0.30), 
                  eo = rnorm(n = n_samples, mean = 0.40, sd = 0.24), 
                  outcome = outcomes[out], 
                  imputation = imps[imp], 
                  model_type = model[mod],
                  pre_type =  pre_process[pre ],
                  post_type =  post_process[post]
                ) %>%
                  mutate(
                    eo = abs(eo)
                  )
              )
            
            
          }
        }
      }
    }
  }
}


# We model F1 score with interactions for every combination
outcome_model <- lm(
  f1 ~ outcome * imputation * model_type * pre_type *  post_type,
  data = dist_df 
)

# and then compare the means
emm <- emmeans(
  outcome_model,
  ~ pre_type *  post_type *  outcome *  imputation  * model_type
)

# We have an F1 score difference of -0.01 between doing pre and post processing  - doing no pre and post processing 
# And its non sig. at alpha = 0.05 
pairs(emm, adjust = "none") %>%
  as.data.frame() %>%
  separate(contrast, into = c("left", "right"), sep = "-") %>%
  mutate(
    left = as.character(left), 
    right = as.character(right), # reference 
    left_last3 = sapply(strsplit(trimws(left), "\\s+"), function(x) {
      paste(tail(x, 3), collapse = " ")
    }),
    right_last3 = sapply(strsplit(trimws(right), "\\s+"), function(x) {
      paste(tail(x, 3), collapse = " ")
    }),
    match_last3 = left_last3 == right_last3,
    
    first2_right = sapply(strsplit(tolower(trimws(right)), "\\s+"), function(x) {
      paste(head(x, 2), collapse = " ")
    }),
    
    starts_with_none_none = first2_right == "none none"
    
  ) %>%
  filter(match_last3 == TRUE & starts_with_none_none == TRUE) %>%
  arrange(right) %>%
  filter( left_last3 == 'ideation iter xgb') %>%
  filter(p.value >= 0.05)  %>%
  mutate(
    real_mean_diff = mean_diff
  ) %>%
  select(
    model_update = left, comprasion_model = right, real_mean_diff,  estimate ,t.ratio, SE , p.value, 
  ) 

# When hypothesize testing that same model that was deemed no difference from doing no pre and post processing 
# "There is sufficient evidence to conclude that mean is less than 0.10" when doing an one sided hypothesize test 
# Where this is true because we draw random values from a normal dist. with mu =  0.08 and sigma = 0.03

dist_df %>%
  group_by(outcome,imputation, model_type, pre_type, post_type) %>%
  summarise(
    ttest = list(t.test(eo,
                        mu = 0.10, 
                        alternative = "less")),
    .groups = "drop"
  ) %>%
  mutate(
    mean = sapply(ttest, \(x) x$estimate),
    t_statistic = sapply(ttest, \(x) x$statistic),
    p_value = sapply(ttest, \(x) x$p.value),
    conf_low = sapply(ttest, \(x) x$conf.int[1])
  ) %>%
  select(-ttest) %>%
  filter(p_value <= 0.05)


