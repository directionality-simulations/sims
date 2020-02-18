sims_df <- read.csv('../sims_output/sims_df.csv')

library(tidyverse)
library(ggplot2)
library(data.table)

sims_df$case <- as.character(sims_df$case)

# Calculate bias in b1 ----------------------------------------------------


sims_df <- sims_df %>%
  mutate(bias_subtractor = case_when(
    # True b1 = 0.082
    case == 'case1' ~ 0.082,
    # For all other cases, true b1 = 0
    case == 'case2' ~ 0,
    case == 'case3_symm' ~ 0,
    case == 'case3_lagx' ~ 0,
    case == 'case3_lagy' ~ 0
  )) %>%
  mutate(bias_b1 = 
           b1 - bias_subtractor)


# Calculation Proportion Significant --------------------------------------

# alpha = 0.05

# number of times out of 500 that are below 0.05. 

proportion_df <- sims_df %>%
  group_by(case, model, sample_size, lag_amount) %>%
  summarise(
    prop_sig = (sum(p_value < 0.05) / 500)
  )

sims_df <- left_join(sims_df, proportion_df)


# Relabel sample size so graphs look pretty -------------------------------

sims_df$sample_size <- as.character(sims_df$sample_size)
sims_df <- sims_df %>%
  mutate(ss_pretty = case_when(
    sample_size == '72' ~ 'N = 72',
    sample_size == '150' ~ 'N = 150',
    sample_size == '250' ~ 'N = 250',
    sample_size == '350' ~ 'N = 350',
    sample_size == '450' ~ 'N = 450',
    sample_size == '550' ~ 'N = 550'
  ))
sims_df$ss_pretty <- factor(sims_df$ss_pretty, levels = c('N = 72', 
                                                          'N = 150',
                                                          'N = 250',
                                                          'N = 350',
                                                          'N = 450',
                                                          'N = 550'))




# Rough appearance of tables ----------------------------------------------


sims_df %>%
  group_by(case, model, ss_pretty, lag_amount) %>%
  summarise(
    bias_b1 = mean(bias_b1),
    prop_sig = mean(prop_sig),
    se = mean(se)
  )


# Have to make it untidy for final table = ( ------------------------------------



# Bias in b1 --------------------------------------------------------------


b1_wider <- sims_df %>%
  group_by(case, model, ss_pretty, lag_amount) %>%
  summarise(
    bias_b1 = mean(bias_b1)
  ) %>%
  arrange(ss_pretty, lag_amount) %>%
  spread(model, bias_b1) %>%
  arrange(ss_pretty, lag_amount) %>%
  unite(together, partial, wo, sep = '++') %>%
  spread(case, together) %>%
  separate(case1, sep = '\\++', into = c('c1_partial', 'c1_wopartial')) %>%
  separate(case2, sep = '\\++', into = c('c2_partial', 'c2_wopartial')) %>%
  separate(case3_lagx, sep = '\\++', into = c('c3_lagx_partial', 'c3_lagx_wopartial')) %>%
  separate(case3_lagy, sep = '\\++', into = c('c3_lagy_partial', 'c3_lagy_wopartial')) %>%
  separate(case3_symm, sep = '\\++', into = c('c3_symm_partial', 'c3_symm_wopartial'))

names(b1_wider) <- c('Sample Size', 'Lag', 
                     'Partial', 'Without Partial', 
                     'Partial', 'Without Partial', 
                     'Partial', 'Without Partial',
                     'Partial', 'Without Partial',
                     'Partial', 'Without Partial')

library(kableExtra)

b1_wider %>%
  kable(booktabs = T,
        caption = 'Bias in the estimate of b1 across cases, models, sample sizes, and true lag amounts.') %>%
  kable_styling(latex_options = c('hold_position')) %>%
  add_header_above(c("", "", "Case 1" = 2, "Case 2" = 2, "Case 3 - Lag X" = 2, "Case 3 - Lag Y" = 2, "Case 3 - Symm" = 2))



# Standard Error ----------------------------------------------------------


se_wider <- sims_df %>%
  group_by(case, model, ss_pretty, lag_amount) %>%
  summarise(
    se = mean(se)
  ) %>%
  arrange(ss_pretty, lag_amount) %>%
  spread(model, se) %>%
  arrange(ss_pretty, lag_amount) %>%
  unite(together, partial, wo, sep = '++') %>%
  spread(case, together) %>%
  separate(case1, sep = '\\++', into = c('c1_partial', 'c1_wopartial')) %>%
  separate(case2, sep = '\\++', into = c('c2_partial', 'c2_wopartial')) %>%
  separate(case3_lagx, sep = '\\++', into = c('c3_lagx_partial', 'c3_lagx_wopartial')) %>%
  separate(case3_lagy, sep = '\\++', into = c('c3_lagy_partial', 'c3_lagy_wopartial')) %>%
  separate(case3_symm, sep = '\\++', into = c('c3_symm_partial', 'c3_symm_wopartial'))

names(se_wider) <- c('Sample Size', 'Lag', 
                     'Partial', 'Without Partial', 
                     'Partial', 'Without Partial', 
                     'Partial', 'Without Partial',
                     'Partial', 'Without Partial',
                     'Partial', 'Without Partial')

library(kableExtra)

se_wider %>%
  kable(booktabs = T,
        caption = 'Standard error in the b1 estimate across cases, models, sample sizes, and true lag amounts.') %>%
  kable_styling(latex_options = c('hold_position')) %>%
  add_header_above(c("", "", "Case 1" = 2, "Case 2" = 2, "Case 3 - Lag X" = 2, "Case 3 - Lag Y" = 2, "Case 3 - Symm" = 2))



# Prop Significant --------------------------------------------------------


prop_wider <- sims_df %>%
  group_by(case, model, ss_pretty, lag_amount) %>%
  summarise(
    prop_sig = mean(prop_sig)
  ) %>%
  arrange(ss_pretty, lag_amount) %>%
  spread(model, prop_sig) %>%
  arrange(ss_pretty, lag_amount) %>%
  unite(together, partial, wo, sep = '++') %>%
  spread(case, together) %>%
  separate(case1, sep = '\\++', into = c('c1_partial', 'c1_wopartial')) %>%
  separate(case2, sep = '\\++', into = c('c2_partial', 'c2_wopartial')) %>%
  separate(case3_lagx, sep = '\\++', into = c('c3_lagx_partial', 'c3_lagx_wopartial')) %>%
  separate(case3_lagy, sep = '\\++', into = c('c3_lagy_partial', 'c3_lagy_wopartial')) %>%
  separate(case3_symm, sep = '\\++', into = c('c3_symm_partial', 'c3_symm_wopartial'))

names(prop_wider) <- c('Sample Size', 'Lag', 
                     'Partial', 'Without Partial', 
                     'Partial', 'Without Partial', 
                     'Partial', 'Without Partial',
                     'Partial', 'Without Partial',
                     'Partial', 'Without Partial')

library(kableExtra)

prop_wider %>%
  kable(booktabs = T,
        caption = 'Proportion of significant b1 estimates across cases, models, sample sizes, and true lag amounts.') %>%
  kable_styling(latex_options = c('hold_position')) %>%
  add_header_above(c("", "", "Case 1" = 2, "Case 2" = 2, "Case 3 - Lag X" = 2, "Case 3 - Lag Y" = 2, "Case 3 - Symm" = 2))

