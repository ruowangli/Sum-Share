# Sum-Share

Sum-Share requires two rounds of communication, thus it needs at least four functions to be implemented in the package. The four steps are:
1. Calculate summary statistics for the covariates in each data (distributed_site_age_gender_table.R).
2. Combine the summary statistics for the covariates in all data and return a covariate table (distributed_site_age_gender_table_combine.R).
3. Calculate test statistics in each data, with the coavariate table being one of the required files (distributed_site_covariate_score_hessian.R).
4. Combined the test statistics for all data and calculate a final p-value (distributed_site_covariate_score_hessian_combine.R).

As of now, each step has its own R script. To speed up the calculation, some of the R scripts also calls several custom C++ functions (score_hessian.cpp).



