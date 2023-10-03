# Pertussis_IgG
Code (JAGS, Rscript) for the Bayesian mixture latent class model on IgG Pertussis diagnosis

JAGS code  

--- normal_model - All covariates included (age, gender - fixed effect, region - random effect)  

--- normal_model_age_rm - Covariate age excluded  

--- normal_model_no_cov - No covariates included (uniform(0,1) prior distribution placed on prevalence)  


Rscipt
--- code to load data, run JAGS code and generate graphs
