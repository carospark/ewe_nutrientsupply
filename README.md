# ewe_nutrientsupply
### Code for: "Global effect of extreme weather events on nutrient supply" manuscript

This collection of scripts and functions was developed to analyse the effects of extreme weather events (EWEs) on global nutrient supply using Superposed Epoch Analysis (SEA). This notebook provides an example of the calculation based on one nutrient (sodium).

### Data 

The following datasets are processed:

1) Global Expanded Nutrient model (GENuS)

- URL: https://dataverse.harvard.edu/dataverse/GENuS
- Paper: Smith MR, Micha R, Golden CD, Mozaffarian D, Myers SS. Global Expanded Nutrient Supply (GENuS) Model: A New Method for Estimating the Global Dietary Supply of Nu- trients. PLoS One. 2016; 11(1): e0146976.

2) International Disasters Database (EM-DAT)

- URL: https://www.emdat.be/database
- EM-DAT: The Emergency Events Database - Université catholique de Louvain (UCL) - CRED, D. Guha-Sapir - www.emdat.be, Brussels, Belgium.


### Part 1: Data preparation

#### Step 1: Generating the full EWE dataset

The EWE data from EM-DAT was processed so that one clean dataset with the following information was generated: single-year occurrence, multiyear occurrence, total persons affected, total financial damage, and population/GDP statistics 

```{r}
source("sea_analysis/1.multiyear_events.R")
```


#### Step 2: Calculating 90th percentiles

The 90th percentile for total damage, total persons affected, and the proportion of people affected per population or financial damage per GDP was determined.

```{r}
source("sea_analysis/2.thresholds_popdam.R")
```


#### Step 3: Generating 5-year windows for sodium
```{r}
source("sea_analysis/3.5yr_window_nuts.R")
```


### Part 2: Data analysis

#### Step 4: Combine EWE and nutrient supply data in SEA

```{r}
source("sea_analysis/4.5yr_window_EWEs.R")
```

#### Step 5: Composite 5 year windows globally and by subgroup

```{r}
source("sea_analysis/5.5yr_windows_composited.R")
```


#### Step 6: Generate bootstrapped CIs for SEA estimates

```{r}
source("sea_analysis/6.bootstrapped_cis.R")
```


#### Step 7: Determine the distribution Kernel density estimator for comparison groups

```{r}
source("sea_analysis/7.1000_cntrls_distribution_density.R")
```


#### Step 8: Apply kernel density estimator to generate 1000 bootstrapped comparison groups

```{r}
source("sea_analysis/8.1000_controls_2.R")
```
