# Sum-Share

Sum-Share requires two rounds of communication, thus it needs at least four steps:
1. Calculate summary statistics for the covariates in each data. 
2. Combine the summary statistics for the covariates in all data and return a covariate table. 
3. Calculate test statistics in each data, with the coavariate table being one of the required files. 
4. Combined the test statistics for all data and calculate a final p-value. 

The core algorithm is implemented in sum_share.cpp).



