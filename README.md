# pdm

**A reproducible framework for monthly spatial prioritization of deforestation risk using data-driven modeling in R**

---

## Overview

`pdm` is an R package designed to support **environmental decision-making** by identifying and prioritizing areas at risk of **deforestation**.

The package operationalizes a modeling pipeline that:

* Generates spatial predictors from environmental datasets
* Trains predictive models for land-use change and fire occurrence
* Produces **prioritization maps** to support monitoring and intervention strategies
* Evaluates model performance through temporal validation and error metrics

This framework was developed with a focus on **Amazon biome applications**, but it is fully adaptable to other regions.

---

## Motivation

Deforestation and fire dynamics are complex, multi-scale processes influenced by climate, accessibility, and land-use pressures.

While many models exist, there is a gap between **research prototypes** and **operational tools** that:

* Run consistently over time
* Produce standardized outputs
* Support decision-making workflows

`pdm` bridges this gap by providing a **reproducible, modular, and extensible pipeline**.

---

## Installation

```r
# install.packages("devtools")
devtools::install_github("albers/prioritizedeforestationhotspots") # Necessary in this first package version
devtools::install_github("LucasUfmg/pdm")
```

---

## Workflow

The package follows a structured pipeline:

1. **Data preparation**

   * Spatial covariates (e.g., land cover, climate, accessibility)
   * Temporal aggregation (monthly/annual)

2. **Feature engineering**

   * Generation of derived variables
   * Standardization and filtering

3. **Model fitting**

   * Statistical or machine learning models
   * Configurable training periods

4. **Prediction and prioritization**

   * Spatially explicit predictions
   * Ranking of high-risk areas

5. **Evaluation**

   * Commission error
   * Variable importance
   * Temporal performance diagnostics

---

## Minimal Example

```r
library(pdm)
library(prioritizedeforestationhotspots)
# Run model with default parameters for September 2022
pdm::run_pipeline(folder = "your_folder_path",
                  mes_inicial = 9,mes_final = 9, ano_inicial = 2022,
                  run_download = T,run_prep = T,run_prio = T)

# Outputs
names(result)
#> "priority_map" "commission_error" "variable_importance" "monthly_error"

# Plot prioritization map
plot(result$priority_map)
```

---

## Outputs

The model generates:

* **Prioritization maps**: spatial ranking of risk areas
* **Commission error metrics**: evaluation of false positives
* **Variable importance**: model interpretability
* **Temporal diagnostics**: monthly performance trends

These outputs are designed to support:

* Environmental monitoring programs
* Policy targeting and enforcement
* Research on land-use dynamics

---

## Reproducibility

The package was built with reproducibility as a core principle:

* Fully scriptable workflow
* Deterministic outputs given fixed inputs
* Modular functions for each processing step

---

## Technical Stack

* R (>= 4.x)
* Spatial analysis: `sf`, `terra`
* Data manipulation: `dplyr`
* Modeling: customizable (user-defined or built-in methods)

---

## Author

Lucas Santos
Data Scientist | Environmental Modeling | Spatial Analysis

* Experience in **Amazon deforestation and fire modeling**
* Background in academic and applied research environments
* Former PhD researcher (University of California, Irvine)

---

## Potential Applications

* Monitoring deforestation frontiers
* Fire risk prediction
* Conservation planning
* Environmental policy support
* Early warning systems

---

## Future Work

* Integration with real-time data pipelines
* Expansion to other biomes and regions
* Incorporation of advanced ML models
* Deployment as a decision-support tool

---

## License

MIT License

---

## Contact

For collaborations or academic inquiries, feel free to reach out.
