

# Run on multiple processors for better speed ---------------------------

library(furrr)
plan(multiprocess)

# Load simulation functions for each case ---------------------------------

# Case 1
source('case_functions/case1_func.R')

# Case 2
source('case_functions/case2_func.R')

# Case 3
source('case_functions/case3_func_symm.R')
source('case_functions/case3_func_lagx.R')
source('case_functions/case3_func_lagy.R')


# Load the parameters to be mapped over -----------------------------------

# I.e., sample size and causal lag length (0 through 5)

source('parameters/parameters.R')


# Run simulations for case 1 ----------------------------------------------

case1 <- future_map2(param_map$sample_sizes, param_map$lag_amounts, sims_function_case1)

# Run simulations for case 2 ----------------------------------------------

case2 <- future_map2(param_map$sample_sizes, param_map$lag_amounts, sims_function_case2)

# Run simulations for case 3 ----------------------------------------------

# symmetrical effect from z to x and y
case3_symm <- future_map2(param_map$sample_sizes, param_map$lag_amounts, sims_function_case3_symm)

# instant z to x effect but lagged effect of z on y
case3_lagy <- future_map2(param_map$sample_sizes, param_map$lag_amounts, sims_function_case3_lagy)

# instant z to y effect but lagged effect of z on x
case3_lagx <- future_map2(param_map$sample_sizes, param_map$lag_amounts, sims_function_case3_lagx)



# Put results into data set and tidy it up --------------------------------

library(data.table)

# case 1
c1_df <- rbindlist(case1)

# case 2
c2_df <- rbindlist(case2)

# case 3
c3_symm_df <- rbindlist(case3_symm)
c3_lagy <- rbindlist(case3_lagy)
c3_lagx <- rbindlist(case3_lagx)

# merge all
sims_df <- bind_rows(c1_df,
                     c2_df,
                     c3_symm_df,
                     c3_lagy,
                     c3_lagx)

# tidy
sims_df <- sims_df %>%
  gather(b1_wo_partial, b1_partial,
         se_wo_partial, se_partial,
         p_wo_partial, p_partial,
         key = 'result_model', value = 'value') %>%
  separate(result_model, into = c('result', 'model'), sep = '_') %>%
  mutate(row_help = rep(1:9000, 60)) %>%
  spread(result, value) %>%
  dplyr::select(-row_help) %>%
  dplyr::select(case, model, sample_size, lag_amount, b1, se, p) %>%
  rename(p_value = p)
  

# Save it for plotting --------------------------------------------
write.csv(sims_df, file = 'sims_output/sims_df.csv', row.names = F)

# Create case and variable description files --------------------------------------

cases <- unique(sims_df[, 'case'])
case_descriptions <- c('dgp: x causes y',
                       'dgp: y causes x',
                       'dgp: z causes x and y by the same lag amount',
                       'dgp: z causes x concurrently and y by some lag amount',
                       'dgp: z causes y concurrently and x by some lag amount')

case_explain <- data.frame(
  'Name' = c(cases),
  'Description' = c(case_descriptions)
)

variables <- names(sims_df)
description <- c('The causal structure in the dgp',
                 'The type of estimated model',
                 'The sample size',
                 'The lag length of the exogenous to endogneous effect in the dgp',
                 'The coefficient relating x to y in the estimated models',
                 'The standard error on b1 in the estimated models',
                 'The p value on b1 in the estimated models')

variable_descripts <- data.frame(
  'Variables' = c(variables),
  'Description' = c(description)
)

model_types <- c('A model where y is regressed on x',
                 'A model where y is regressed on x and prior y')
model_names <- c('wo_partial',
                 'partial')

model_t <- data.frame(
  'Name' = c(model_names),
  'Description' = c(model_types)
)

library(xtable)
vd_table <- xtable(variable_descripts, caption = 'Variable Descriptions')
vd_out <- print.xtable(vd_table, type = 'latex', include.rownames = F)
m_table <- xtable(model_t, caption = 'Note on Models')
m_out <- print.xtable(m_table, include.rownames = F)
cat('\\documentclass{article}\n', 
    '\\begin{document}\n', 
    c(vd_out, paste('\n')),
    c(m_out, paste('\n')),
    '\\end{document}', 
    file = 'variable_descriptions/variable_descriptions.md')

c_table <- xtable(case_explain)
c_out <- print.xtable(c_table, type = 'latex', include.rownames = F)
cat('\\documentclass{article}\n', 
    '\\begin{document}\n', 
    c(c_out, paste('\n')),
    '\\end{document}', 
    file = 'variable_descriptions/case_descriptions.md')



# save environment  -------------------------------------------------------

save.image(file = 'sims_output/sims_environment.RData')


