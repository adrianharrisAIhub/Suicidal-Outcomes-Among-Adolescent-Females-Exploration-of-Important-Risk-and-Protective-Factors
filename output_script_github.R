rm(list=ls())
set.seed(1234)

library(naniar)
library(rstatix)

setwd("YOUR WD")

threshold_df <- read.csv('YOUR PATH/Data/raw_threshold_table.csv') %>% clean_names()
overall_df <- read.csv('YOUR PATH/Data/raw_model_output_table.csv')%>% clean_names()
race_df <- read.csv('YOUR PATH/Data/raw_race_model_output_table.csv')%>% clean_names()

raw_race_df <- read.csv('YOUR PATH/Data/raw_race_model_dist_output.csv') %>% clean_names() %>%
  select(
    -threshold
  )

f1_scores <- read.csv('YOUR PATH/Data/hypo_testing.csv') %>% clean_names()
binary_modeling_data <- read.csv('YOUR PATH/Data/yrbs2023_female_only_binary.csv') %>%
  select(
    -X
  )

shap_sigs <-  read.csv('YOUR PATH/Data/sig_finds.csv')
# Littles MCAR test
littles_test <- binary_modeling_data %>% 
  select(
    -PSU, -stratum, -weight
  ) %>%  
  mcar_test(.) 

littles_test 
write.csv(littles_test, "YOUR PATH/Tables/littles_test.csv")


# sTable X and Table 1 
weighted_design_table_overall <- svydesign(ids = ~PSU, data = binary_modeling_data, strata = ~stratum, weights = ~weight, nest=TRUE)

options(survey.lonely.psu = "adjust")

overall_table <- weighted_design_table_overall %>%
  tbl_svysummary(
    include = c(-"PSU", -"stratum",- "weight", -'ideation',-'suicideatt_qn29', -'sexuality',-'grade' ),
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})"),
      all_categorical() ~ "{n_unweighted} ({p}%)"
    ),
    digits = list(all_continuous() ~ 1,
                  all_categorical() ~ c(0,1)),
    missing = "no")  %>%
  modify_header(all_stat_cols() ~ "**{level} N = {n_unweighted}**") %>%
  add_stat_label(
    label = all_categorical() ~ "No. (%)"
  ) %>%
  add_n("{N_nonmiss_unweighted}")


weighted_design_table_race <- svydesign(ids = ~PSU, data = binary_modeling_data  %>% filter(complete.cases(race4)), strata = ~stratum, weights = ~weight, nest=TRUE)

by_race_table <- weighted_design_table_race  %>%
  tbl_svysummary(by = "race4",
                 include = c(-"PSU", -"stratum",- "weight", -'ideation',-'suicideatt_qn29', -'sexuality',-'grade' ),
                 statistic = list(
                   all_continuous() ~ c("{mean} ({sd})"),
                   all_categorical() ~ "{n_unweighted} ({p}%)"
                 ),
                 digits = list(all_continuous() ~ 1,
                               all_categorical() ~ c(0,1)),
                 missing = "no")  %>%
  add_stat_label(
    label = all_categorical() ~ "No. (%)"
  ) %>%
  add_n("{N_nonmiss_unweighted}") %>%
  modify_header(all_stat_cols() ~ "**{level} N = {n_unweighted}**") %>%
  add_p(
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
  bold_p(t = 0.05)

combinded_weighted_table_one <- tbl_merge(tbls = list(overall_table,by_race_table))
combinded_weighted_table_one

write.csv(combinded_weighted_table_one  %>% as.tibble() %>% replace(is.na(.), ""), "YOUR PATH/Tables/table_1_weighted_full.csv")


overall_table <- weighted_design_table_overall %>%
  tbl_svysummary(
    include = c('ideation','suicideatt_qn29','race4', 'sexuality','grade' ),
    statistic = list(
      all_continuous() ~ c("{mean} ({sd})"),
      all_categorical() ~ "{n_unweighted} ({p}%)"
    ),
    digits = list(all_continuous() ~ 1,
                  all_categorical() ~ c(0,1)),
    missing = "no")  %>%
  modify_header(all_stat_cols() ~ "**{level} N = {n_unweighted}**") %>%
  add_stat_label(
    label = all_categorical() ~ "No. (%)"
  ) %>%
  add_n("{N_nonmiss_unweighted}")

by_race_table <- weighted_design_table_race  %>%
  tbl_svysummary(by = "race4",
                 include = c('ideation','suicideatt_qn29','race4', 'sexuality','grade' ),
                 statistic = list(
                   all_continuous() ~ c("{mean} ({sd})"),
                   all_categorical() ~ "{n_unweighted} ({p}%)"
                 ),
                 digits = list(all_continuous() ~ 1,
                               all_categorical() ~ c(0,1)),
                 missing = "no")  %>%
  add_stat_label(
    label = all_categorical() ~ "No. (%)"
  ) %>%
  add_n("{N_nonmiss_unweighted}") %>%
  modify_header(all_stat_cols() ~ "**{level} N = {n_unweighted}**") %>%
  add_p(
    pvalue_fun = ~ style_pvalue(.x, digits = 2)
  ) %>%
  bold_p(t = 0.05)

combinded_weighted_table_one <- tbl_merge(tbls = list(overall_table,by_race_table))
combinded_weighted_table_one

write.csv(combinded_weighted_table_one  %>% as.tibble() %>% replace(is.na(.), ""), "YOUR PATH/Tables/table_1_weighted_small.csv")


# Disparities Calculation 
# If p value < 0.05 then there is some difference
anova_race_df <- raw_race_df %>% 
  group_by(metric, outcome,imputation,model, pre_process, post_process)  %>% 
  group_modify(~ {
    fit <- aov(metric_value ~ group, data = .x)
    broom::tidy(fit)
  }) %>%
  ungroup() %>%
  filter(term == "group")

# Then, pairwise we find which groups differ from each other to get a disparities score 
base_dis <- raw_race_df %>%
  group_by(metric, outcome,imputation,  model, pre_process, post_process)  %>% 
  tukey_hsd(metric_value ~ group)

disparity_df <- base_dis %>%
  ungroup() %>%
  left_join(
    anova_race_df %>%
      rename(anova_p_value = p.value) %>%
      select(
        -term, -df, -sumsq, -meansq, -statistic
      )
  ) %>%
  group_by(outcome, imputation, model, pre_process, post_process) %>%
  summarise(
    dispar_score = mean(p.adj.signif != "ns" & anova_p_value < 0.05), 
    count = sum(p.adj.signif != "ns" & anova_p_value < 0.05), 
    n_total = n()
  ) %>%
  ungroup() %>%
  arrange(dispar_score)


# Comparing pre, post or both to no pre and post
# F1 compare
outcome_model <- lm(
  f1 ~ outcome * imputation * model * pre_process *  post_process,
  data = f1_scores
)

emm <- emmeans(
  outcome_model,
  ~ pre_process *  post_process *  outcome *  imputation  * model
)

model_compare_table <- pairs(emm, adjust = "bonferroni") %>% 
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
  filter(p.value >= 0.05)  %>% # Models that were the same 
  select(
    model_update = left, comprasion_model = right, estimate ,t.ratio, SE , p.value, 
  )  %>%
  bind_rows(
    
    
    pairs(emm, adjust = "bonferroni") %>%
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
      filter(p.value <= 0.05 & estimate > 0) %>% # Models that were better
      mutate(
        p.value = round(p.value, 3)
      ) %>%
      select(
        model_update = left, comprasion_model = right, estimate ,t.ratio, SE , p.value, 
      ) 
    
  )


model_compare_table


final_models <- disparity_df %>% # disparities info
  select(
    -count, -n_total
  ) %>%
  left_join(
    
    threshold_df %>% 
      group_by(
        outcome, imputation,   pre_process, post_process, model
      ) %>%
      mutate(
        n_missing = sum(is.na(threshold_1) | is.na(threshold_2))
      ) %>%
      ungroup() %>%
      mutate(
        not_valid = ifelse(n_missing > 0, "not_valid", "valid") # Finding only valid solutions 
      ) %>%
      group_by(outcome, imputation,   pre_process, post_process, model) %>%
      slice(1) %>%
      ungroup() %>%
      select(
        outcome,imputation, pre_process, post_process, model,not_valid
      ) 
    
  ) %>%
  left_join(
    
    model_compare_table %>%  # Models that were the same or better
      select(
        model_update
      ) %>%
      separate(model_update,
               into = c("pre_process", "post_process", "outcome", "imputation", "model"),
               sep = " ") %>%
      mutate(
        f1_compare = 'yes'
      )
    
  ) %>%
  mutate(
    base_model = ifelse(
      pre_process == "none" & post_process == "none", 1, 0
    ), 
    pass = case_when(
      base_model == 1 ~ "pass", # keeping all baseline model
      f1_compare == "yes" ~ "pass",  # keeping all models that were the same or better
      
      TRUE ~ "no pass"
    )
  ) %>%
  filter(pass == "pass") 
  
look_up_table <- final_models %>%
  group_by(outcome, imputation) %>%
  slice(which.min(dispar_score)) %>%
  ungroup() %>%
  mutate(
    best = 'best'
  ) %>%
  select(
    -not_valid, -f1_compare,- base_model, -pass, - dispar_score
  )


write.csv(look_up_table, "YOUR PATH/Tables/look_up_table.csv")

overall_model_base <- overall_df %>%
  select(
    -threshold
  ) %>%
  left_join(
    final_models 
  ) %>%
  filter(pass == "pass" & metric !=  "Prevalence" & metric != "EO") %>%
  select(
    metric, estimate, lower_bound, upper_bound,  outcome, imputation,   pre_process, post_process,  model,
  ) %>%
  mutate(
    estimate = format(round(estimate, 2), nsmall  = 2), 
    lower_bound = format(round(lower_bound , 2), nsmall  = 2), 
    upper_bound =  format(round(upper_bound, 2), nsmall  = 2), 
    combine_est =  paste(estimate, paste0("(", lower_bound, ",", upper_bound, ")") )
  ) %>%
  select(
    metric, combine_est, outcome,  imputation,  pre_process,  post_process,  model
  ) %>%
  pivot_wider(
    names_from = metric,
    values_from = combine_est
  ) %>%
  left_join(
    final_models %>%
      select(
        outcome,  imputation,  pre_process,  post_process,  model, dispar_score
      )
  ) %>%
  left_join(
    look_up_table
  ) %>%
  arrange(outcome, imputation)

main_text_overall_model_tab <- overall_model_base %>%
  filter(imputation == "iter") %>%
  select(
    -imputation,
  )

main_text_overall_model_tab

write.csv(main_text_overall_model_tab, "YOUR PATH/Tables/main_text_overall_model_tab.csv")

sup_text_overall_model_tab <- overall_model_base %>%
  filter(imputation == "knn") %>%
  select(
    -imputation,
  )

sup_text_overall_model_tab 
write.csv(sup_text_overall_model_tab, "YOUR PATH/Tables/sup_text_overall_model_tab.csv")

race_model_metrics_table <-  race_df %>%
  left_join(
    look_up_table
  ) %>%
  filter(!is.na(best)) %>%
  mutate(
    estimate = format(round(estimate, 2), nsmall  = 2), 
    lower = format(round(lower , 2), nsmall  = 2), 
    upper =  format(round(upper, 2), nsmall  = 2), 
    combine_est =  paste(estimate, paste0("(", lower, ",", upper, ")") )
  ) %>%
  select(
    metric, group, combine_est, outcome,  imputation,  pre_process,  post_process,  model
  ) %>%
  pivot_wider(
    names_from = metric,
    values_from = combine_est
  ) 


main_text_race_model_tab <- race_model_metrics_table %>% filter(imputation == "iter") %>%
  select(
    -imputation,- pre_process, -post_process,- model 
  )
sup_text_race_model_tab <- race_model_metrics_table %>% filter(imputation == "knn") %>%
  select(
    -imputation, -pre_process, -post_process, -model 
  )

main_text_race_model_tab
write.csv(main_text_race_model_tab, "YOUR PATH/Tables/main_text_race_model_tab.csv")


sup_text_race_model_tab
write.csv(sup_text_race_model_tab, "YOUR PATH/Tables/sup_text_race_model_tab.csv")

# Used to write the results for the figures
shap_sigs %>% 
  filter(imp == "iter") %>%
  select(
    Feature, group, outcome, 
  ) %>% 
  pivot_wider(
    names_from = Feature,
    values_from = outcome,
    values_fn = ~ paste(unique(.x), collapse = ", ")
  ) %>%
  mutate(group = factor(group, levels = c("Overall", "White", "Black", "Latine", "Other"))) %>%
  arrange(group) %>%
  View()

shap_sigs %>% 
  filter(imp == "knn") %>%
  select(
    Feature, group, outcome, 
  ) %>% 
  pivot_wider(
    names_from = Feature,
    values_from = outcome,
    values_fn = ~ paste(unique(.x), collapse = ", ")
  ) %>%
  mutate(group = factor(group, levels = c("Overall", "White", "Black", "Latine", "Other"))) %>%
  arrange(group) %>%
  View()

