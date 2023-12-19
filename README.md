# Generalised Shared Component Model

This repository contains the code and modelled results described in the manuscript "Generalising the Shared Component Model: Creating the Health Determinants for Cancer Indexes for Areas" by James Hogg, Jessica Cameron, Susanna Cramb, Peter Baade and Kerrie Mengersen.

### Input features

The input features are available in `InputFeatures.csv`. The dataset contains a 11 columns, with the first identifying the 2016 SA2s. The remaining columns are estimates and standard deviations (SD) for the five cancer risk factor estimates. 

### HDCIA product

The Health Determinants for Cancer Indexes for Areas (HDCIA) product are available in `HDCIAProduct.csv`. This dataset contains the following 37 columns. NOTE: Index 1 and 2 are provided for completeness, Index 3 is the most applicable. 

<!--from the authors. in the dataset `ModelledEstimates.csv`.--->

- `SA2_2016`: The 2016 9-digit SA2 codes

Plus the following 6 columns for each of the four indices (`Index1_*` - `Index4_*`)

- `*_point`: Posterior mean
- `*_lower`: Lower limit of 95% highest posterior density interval (HPDI)
- `*_upper`: Upper limit of 95% HPDI
- `*_Prob_PercentileAbove80`: Probability that posterior percentile is above 80th
- `*_Prob_PercentileAbove95`: Probability that posterior percentile is above 95th
- `*_Prob_PercentileAbove99`: Probability that posterior percentile is above 99th

And the following 3 columns for each of the four scores (`Scores1_*` - `Scores4_*`)

- `*_point`: Posterior mean
- `*_lower`: Lower limit of 95% highest posterior density interval (HPDI)
- `*_upper`: Upper limit of 95% HPDI
