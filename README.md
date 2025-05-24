# FTIR-WatershedSOM

Code for manuscript: Integration of FTIR-based soil degradation indices with stochastic modelling to assess spatial patterns of organic matter-sediment dynamics in a Mediterranean watershed 

# Overview
This repository contains the R code and data for the manuscript titled "Integration of FTIR-based soil degradation indices with stochastic modelling to assess spatial patterns of organic matter-sediment dynamics in a Mediterranean watershed – Insights from the Northern Apennines". The study explores relationships between Soil Organic Matter (SOM) properties, environmental factors, and soil erosion in a Mediterranean watershed using FTIR-based proxies and Random Forest modeling.

# Abstract
In this study, we explored the relationships between Soil Organic Matter (SOM) properties, serving as potential indicators of soil degradation and erosion, and environmental, geomorphic, and hydrological characteristics in an agricultural-forested Mediterranean watershed. SOM composition of fluvial sediments sampled across the watershed was analyzed using FTIR spectroscopy to calculate FTIR-based proxies for the relative hydrophobicity of SOM, Cation Exchange Capacity (CEC), and organic-matter-cation associations. To investigate geospatial relationships between SOM composition influencing erosion susceptibility and the factors driving its variability at the watershed scale, such as terrain characteristics, soil properties, lithological, and LULC data, we used a Random Forest modeling approach. Our findings indicate that the size and configuration of the contributing areas associated with the sampling points played a crucial role in interpreting the relationships between SOM composition and environmental factors. Oak, hornbeam, and chestnut forests influence hydrophobic organic matter accumulation, making soils more prone to water erosion, where clay content potentially intensifies erosion susceptibility under particular climatic conditions. Moreover, SOM chemical components were spatially linked to sediment dynamics and organic matter connectivity across the watershed, with topographic features such as elevation and channel network base level being key factors. Also, CEC was found to be a potential indicator of soil erosion in geomorphologically active areas. Lastly, carbonate-rich soils appeared to positively influence organic-matter-cation associations, potentially enhancing aggregate stability and reducing erosion susceptibility. This study provides significant new insights into the complex relationships between SOM composition, environmental predictors, and soil erosion in Mediterranean watersheds, supporting novel research hypotheses and perspectives from both a scientific and applicative point of view.

# Repository Contents
som_ftir_modeling.R: R script containing the full analysis, including data loading, feature selection using the FSelector package, Random Forest modeling with the caret package, model evaluation with the ithir package, and feature importance and Accumulated Local Effects (ALE) plots using the iml package.

data/: Folder containing the input datasets:
Experiment1.csv: Data for Experiment 1.

Experiment2.csv: Data for Experiment 2.

Experiment3.csv: Data for Experiment 3.

requirements.txt: Lists the required R packages to run the analysis.

# Data Description
The datasets (Experiment1.csv, Experiment2.csv, Experiment3.csv) contain measurements from a Mediterranean watershed, with the following structure:
Columns 1-4: Targeted variables:

CH.C.O: - A/B = proxy for relative hydrophobicity of SOM (1)

CH.C.O.C: - A/D = proxy for relative hydrophobicity of SOM (2).

C.O.C.O.C: - B/D = proxy for relative Cation Exchange Capacity.

OMcat.C.O.C: - C/D = proxy for relative organic matter-cation associations

Columns 5-44: Feature variables, including terrain characteristics, soil properties, lithological data, and land use/land cover (LULC) data.

Note: If you cannot access the data files due to restrictions, contact Manuel (manuel.lalicata01@universitadipavia.it) for details on the dataset structure to replicate the analysis with similar data.

# Usage Instructions
Clone the Repository:
bash

git clone https://github.com/Odunayo3/FTIR-WatershedSOM.git
cd FTIR-WatershedSOM

Install R and Dependencies:
Ensure R is installed (version 4.0 or higher recommended).

Install required packages listed in requirements.txt:
R

install.packages(readLines("requirements.txt"))

Run the Analysis:
Open som_ftir_modeling.R in R or RStudio.

Ensure the data/ folder with Experiment1.csv, Experiment2.csv, and Experiment3.csv is in the working directory.

Run the script to perform feature selection, train Random Forest models, evaluate performance, and generate feature importance and ALE plots.

Outputs:
Model Files: Saved as .rds files in the working directory (e.g., CH.C.O_df1_model.rds, CH.C.O_df1_result.rds).

Plots: Feature importance plots and ALE plots for each experiment and response variable (e.g., CH.C.O, CH.C.O.C, C.O.C.O.C, OMcat.C.O.C) are displayed and can be saved manually.

Metrics: Model performance metrics are computed using the ithir::goof function and saved in .rds files.

# Reproducibility Notes
The script uses a Random Forest model with 10-fold LOOCV repeated 50 times (caret::trainControl).

Feature selection is performed using the FSelector package’s rank.correlation and cutoff.k functions to select the top 10 predictors.

The NSE function from the ithir package is used for model evaluation. If unavailable, you can define a custom Nash-Sutcliffe Efficiency function:
R

NSE <- function(sim, obs) {
  1 - sum((obs - sim)^2) / sum((obs - mean(obs))^2)
}

Ensure the working directory is set to the repository root to correctly load the CSV files from the data/ folder.

## Using Pre-Trained Models
To apply the pre-trained Random Forest models to new data, use the `use_trained_models.R` script:
- Prepare your dataset with the same predictor columns as in `data/Experiment*.csv`.
- Place your dataset in the `data/` folder (e.g., `data/new_data.csv`).
- Run `use_trained_models.R` in R or RStudio to generate predictions for `CH.C.O`, `CH.C.O.C`, `C.O.C.O.C`, and `OMcat.C.O.C` across all three experiments.
- Predictions are saved as `predictions_new_data.csv`.
See the script for detailed instructions on data preparation.

# License
This project is licensed under the MIT License (LICENSE).
# Contact
For questions about the code, data, or manuscript, contact Odunayo (odunayodavid.adeniyi01@universitadipavia.it).

# Citation

If you use this code or data, please cite:
La Licata et al. (2025). Integration of FTIR-based soil degradation indices with stochastic modelling to assess spatial patterns of organic matter-sediment dynamics in a Mediterranean watershed – Insights from the Northern Apennines.



