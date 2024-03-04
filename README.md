# Generalised Shared Component Model

This repository contains the code and modelled results described in the manuscript "Creating area level indices of behaviours impacting cancer in Australia with a Bayesian generalised shared component model" by James Hogg, Susanna Cramb, Jessica Cameron, Peter Baade and Kerrie Mengersen. Preprint available on [arXiv](https://arxiv.org/abs/2403.00319). 

### Input features

The input features are available in `InputFeatures.csv`. The dataset contains a 11 columns, with the first identifying the 2016 SA2s. The remaining columns are estimates and standard deviations (SD) for the five cancer risk factor estimates. 

### AIBIC product

The Area Indices of Behaviors Impacting Cancer (AIBIC) product are available in `AIBIC_Product.xlsx`. This dataset contains four sheets for each of the four indices. Index 1 and 2 are provided for completeness, Index 3 is the most applicable. Each sheet contains the following columns. 

- `SA2_2016`: The 2016 9-digit SA2 codes
- `STATE_2016`: The 2016 state names
- `score_point`: Posterior median of raw scores
- `score_lower`: Lower limit of 95% highest posterior density interval (HPDI) for raw scores
- `score_upper`: Upper limit of 95% HPDI for raw scores

Plus the 12 columns for each of `national_*` and `state_*`.

- `*percentile_point`: Median of posterior percentiles
- `*percentile_lower`: Lower limit of 95% HPDI for posterior percentiles
- `*percentile_upper`: Upper limit of 95% HPDI for posterior percentiles
- `*percentile_PPAbove80`: Probability that posterior percentile is above 80th
- `*percentile_PPAbove95`: Probability that posterior percentile is above 95th
- `*percentile_PPAbove99`: Probability that posterior percentile is above 99th
- `*rank_point`: Median of posterior rank
- `*rank_lower`: Lower limit of 95% HPDI for posterior rank
- `*rank_upper`: Upper limit of 95% HPDI for posterior rank
- `*rank_PPtop10`: Probability that posterior rank is in top 10
- `*rank_PPtop20`: Probability that posterior rank is in top 20
- `*rank_PPtop100`: Probability that posterior rank is in top 100
