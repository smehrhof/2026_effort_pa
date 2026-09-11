# Computational and neuropsychiatric dimensions of motivation differentially predict everyday physical activity

**Sara Z. Mehrhof & Camilla L. Nord**

This repository contains the analysis code associated with the manuscript:

> **Mehrhof, S. Z. & Nord, C. L.** *Computational and neuropsychiatric dimensions of motivation differentially predict everyday physical activity.*

The repository contains code for data preparation, computational model fitting, primary statistical analyses, and sensitivity analyses.

## Repository structure

```text
├── data/
│
└── code/
    ├── 1_sample_descriptives.R
    ├── 2_model_fitting.R
    ├── 3_main_analysis.R
    ├── 4_sensitivity_analysis.R
    │
    ├── functions/
    │
    └── stan/
```

## Scripts

### 1. `1_sample_descriptives.R`

This script generates descriptive statistics for the study sample.

#### Required data

The script expects the required input data to be located in the `data/` directory.

```text
data/
├── cleaned_data.RDS
└── excluded_data.RDS
```

---

### 2. `2_model_fitting.R`

This script fits the computational models of effort-based decision-making.

The script uses the Stan model code stored in the `stan/` directory and fits the computational models to the behavioural task data.

#### Required data

The script expects the required input data to be located in the `data/` directory.

```text
data/
└── cleaned_data.RDS
```

#### Required Stan code

```text
code/
└── stan/
    └── ed_m3_parabolic.stan
```

#### Required functions

The script may also use custom helper functions stored in:

```text
code/
└── functions/
```

#### Output

The script generates fitted computational models and parameter estimates used in subsequent analyses.

---

### 3. `3_main_analysis.R`

This script conducts the primary statistical analyses reported in the manuscript.

#### Required data

The script expects the processed analytical data and computational parameter estimates generated in the previous steps.

```text
data/
└── combined_data.RDS
```

#### Required functions

The script uses custom helper and plotting functions stored in:

```text
code/
└── functions/
    └── plotting_funs.R
```

#### Output

The script generates the primary statistical results and figures reported in the manuscript.

---

### 4. `4_sensitivity_analysis.R`

This script conducts sensitivity analyses assessing the robustness of the primary findings.

#### Required data

The script expects the analytical dataset and computational parameter estimates generated in the preceding analysis steps.

```text
data/
└── combined_data.RDS
```

#### Output

The script generates the results of the sensitivity analyses.

## Analysis workflow

The analysis scripts should be run in the following order:

1. `1_sample_descriptives.R`
2. `2_model_fitting.R`
3. `3_main_analysis.R`
4. `4_sensitivity_analysis.R`


## Citation

If you use or adapt code from this repository, please cite:

> Mehrhof, S. Z., & Nord, C. L. *Computational and neuropsychiatric dimensions of motivation differentially predict everyday physical activity.*

