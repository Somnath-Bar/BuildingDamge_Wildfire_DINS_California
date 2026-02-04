# BuildingDamage_Wildfire_DINS_California

This project investigates the impact of wildfires on building structural damage across California using a comprehensive, data-driven framework. Leveraging over 100,000 CAL FIRE Damage Inspection (DINS) records from 2013 to 2024, we integrate environmental, structural, and exposure factors to predict wildfire-induced building damage.  

The framework combines statistical modeling, causal analysis, and machine learning to identify key drivers of damage and assess community vulnerability in wildfire-prone landscapes. The goal is to provide insights for risk mitigation, urban planning, and improving resilience in Wildland-Urban Interface (WUI) areas.

All data processing and analysis codes are included in this repository, along with the datasets used for processing.


california-wildfire-building-damage/
│
├── README.md
├── LICENSE
├── environment.yml (or requirements.txt)
├── .gitignore
│
├── data/
│   ├── raw/              # Original data (not tracked)
│   ├── processed/        # Cleaned data
│   └── README.md         # Data documentation
│
├── src/
│   ├── 00_setup.R
│   ├── 01_data_preprocessing.R
│   ├── 02_exploratory_analysis.R
│   ├── 03_statistical_tests.R
│   ├── 04_random_forest_model.R
│   ├── 05_shap_analysis.R
│   ├── 06_spatial_prediction.R
│   ├── 07_validation_LA_fires.R
│   └── utils/
│       ├── spatial_utils.R
│       ├── model_utils.R
│       └── plot_utils.R
│
├── notebooks/
│   ├── 01_Data_Exploration.Rmd
│   ├── 02_Model_Development.Rmd
│   └── 03_Results_Visualization.Rmd
│
├── results/
│   ├── figures/
│   ├── tables/
│   └── models/
│
├── tests/
│   └── test_functions.R
│
└── docs/
    ├── methods.md
    └── supplementary_info.md
