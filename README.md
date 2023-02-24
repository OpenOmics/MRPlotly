# MRPlotly

TwoSampleMR is a package for performing Mendelian randomization using GWAS summary data. Here are functions to make interactive plots using plotly. 
The plotting functions are similar to that in TwoSampleMR. The main functions are 

1. `scatter.plotly` : gives the scatter plots
2. `funnel.plotly` : gives the funnel plots
3. `res.loo.plotly` : gives the LOO plots
4. `forest.plotly` : forest plot of single SNP MR analysis

Install the package using `devtools` as

```
devtools::install_github('spaul-genetics/MRPlotly')
```

A complete example:

```
library(TwoSampleMR)
bmi_exp_dat <- extract_instruments(outcomes = c('ieu-a-2', 'ieu-a-9', 'ieu-a-12'))
chd_out_dat <- extract_outcome_data(snps = bmi_exp_dat$SNP, outcomes = 'ieu-a-7')
dat <- harmonise_data(bmi_exp_dat, chd_out_dat)
res_single <- mr_singlesnp(dat)
res = mr(dat)
res_loo <- mr_leaveoneout(dat)

forest.plotly.single(res_single)
funnel.plotly(res_single)
scatter.plotly(res, dat)
res.loo.plotly(res_loo)
```
