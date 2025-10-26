# RDD Replication Package

# All output files are saved in the SAME folder as the scripts!
No subfolders, no complicated directory structure. Everything in one place.

## 📁 Project Structure


## 🚀 Quick Start

### Install R Packages
```r
install.packages(c("rdrobust", "dplyr", "tidyr", "readr", "ggplot2"))
```

### Run Analyses

```r
# Step 1: Basics
source("baseline.R")
source("population_take.R")
source("congress_take.R")

# Step 2: Heterogeneity
source("adabyyear.R") → Table A.1 & Figure A.1
source("adabystate.R") → Table A.2 & Figure A.2
source("dembyyear.R")  → Figure 1 & Table B.1 & Figure B.1
source("dembystate.R") → Figure 2 & Table B.2 & Figure B.2

# Step 3: Robustness
source("pool_bandwidth.R") → Figure C.1 & Table C.1 & Figure C.2 & Table C.2
source("pool_order.R") → Figure C.3 & Table C.3 & Figure C.4 & Table C.4
source("waldtest.R") → Table1

# Step 4: Weighting
source("population_state_weight_DEM_ADA.R")
source("population_year_weight_DEM_ADA.R")
source("state_weight.R") → Figure 4 & Figure D.3 & Table D.3 & Figure D.4 & Table D.4
source("year_weight.R") → Figure 3 & Figure D.1 & Table D.1 & Figure D.2 & Table D.2

# Step 5: Final
source("ada_implicit.R") → Figure E.3 & Figure E.4
source("dem_implicit.R") → Figure E.5
source("AppendixE.R") → Table E.1 & Figure E.1 & Figure E.2
```

# Level 1 (run first):
- baseline.R → creates `baseline_pooled_results.csv`
- adabyyear.R → creates `ada_by_year_detailed_results.csv`
- adabystate.R → creates `ada_by_state_detailed_results.csv`
- dembyyear.R → creates `democrat_by_year_detailed_results.csv`
- dembystate.R → creates `democrat_by_state_detailed_results.csv`

# Level 2 (need Level 1 files):
- pool_bandwidth.R, pool_order.R → need `baseline_pooled_results.csv`
- population_state_weight_DEM_ADA.R → needs state analysis results
- population_year_weight_DEM_ADA.R → needs year analysis results

# Level 3 (need Level 2 files):
- year_weight.R → needs year analyses + population weighting results
- ada_implicit.R → needs state analyses + population weighting results
- dem_implicit.R → needs state analyses + population weighting results

# Level 4 (needs many files):
- AppendixE.R → needs year and state analyses

## What You'll Get

### CSV Output Files (~50 files):
- `baseline_pooled_results.csv`
- `ada_by_year_detailed_results.csv`
- `ada_by_state_detailed_results.csv`
- `democrat_by_year_detailed_results.csv`
- `democrat_by_state_detailed_results.csv`
- `pop_weighted_detailed_ada_by_state.csv`
- `pop_weighted_detailed_democrat_by_state.csv`
- `population_weighted_detailed_ada_by_year.csv`
- `population_weighted_detailed_democrat_by_year.csv`
- ... and many more!

### PNG Figures (~30 files):
- Forest plots
- Time series plots
- Comparison plots
- Weight distribution plots

### Core Analyses:
- baseline.R - Pooled RDD estimates
- adabyyear.R - ADA by year
- adabystate.R - ADA by state
- dembyyear.R - Democrat by year
- dembystate.R - Democrat by state

### Robustness:
- pool_bandwidth.R - Bandwidth sensitivity
- pool_order.R - Polynomial order test
- waldtest.R - Heterogeneity tests

### Weighting:
- population_state_weight_DEM_ADA.R - Population weights (state)
- population_year_weight_DEM_ADA.R - Population weights (year)
- state_weight.R - Congressional seat weights
- year_weight.R - Year-level weighting comparison

### Implicit Weights:
- ada_implicit.R - Implicit weights for ADA
- dem_implicit.R - Implicit weights for Democrat

### Data Prep:
- population_take.R - Prepare population data
- congress_take.R - Prepare congressional seat data

### Appendix:
- AppendixE.R - Appendix tables and figures


