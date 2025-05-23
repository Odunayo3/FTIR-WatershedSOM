# Title: Using Pre-Trained Random Forest Models for Soil Degradation Analysis
# Authors: Manuel La Licata, Odunayo D. Adeniyi, Ruth H. Ellerbrock, Nisha Bhattarai, 
#          Alberto Bosino, Natalie Papke, Jörg Schaller, Michael Maerker
# Code written by: Odunayo David Adeniyi
# Description: R script to load pre-trained Random Forest models and make predictions 
#              on new data for soil degradation indices in a Mediterranean watershed
# Manuscript: Assessing spatial patterns of organic matter-sediment dynamics in a Mediterranean watershed
# Date: January 2025

# ------------------- Setup and Requirements -------------------
# This script uses pre-trained Random Forest models saved as .rds files
# Required packages: caret (for model loading and predictions)
# Install required package if not already installed
if (!require(caret)) {
  install.packages("caret")
}
library(caret)

# ------------------- Instructions for New Data Preparation -------------------
# To use the pre-trained models, prepare your dataset as a CSV file with the same structure as the original data:
# - Columns 1-4: Response variables (CH.C.O, CH.C.O.C, C.O.C.O.C, OMcat.C.O.C) - these can be NA for prediction
# - Columns 5-44: Predictor variables, including terrain characteristics (e.g., Elevation, Aspect, VDCN), 
#                 soil properties (e.g., Clay, Sand, SOC.stock), lithological data (e.g., Carbonate.Turbidites),
#                 and land use/land cover (e.g., Oak..hornbeam.and.chestnuts.forests, Beech.forests)
# - Ensure column names match exactly those in the original datasets (Experiment1.csv, Experiment2.csv, Experiment3.csv)
# - Save your dataset in the data/ folder, e.g., as 'new_data.csv'

# Example column names (based on Experiment1.csv, Experiment2.csv, Experiment3.csv):
# CH.C.O, CH.C.O.C, C.O.C.O.C, OMcat.C.O.C, SOC.stock, Clay, Sand, Elevation, Aspect, VDCN, CNBL, 
# Oak..hornbeam.and.chestnuts.forests, Beech.forests, Carbonate.Turbidites, Downslope.Distance.Gradient, etc.

# ------------------- Load New Data -------------------
# Specify the path to your new dataset
new_data_path <- "data/new_data.csv"  # Update with your file name
new_data <- read.csv(new_data_path)   # Load new dataset

# Validate data structure
expected_predictors <- c("Oak..hornbeam.and.chestnuts.forests", "VDCN", "CNBL", "Elevation", 
                         "Beech.forests", "Carbonate.Turbidites", "Aspect", "Clay", 
                         "Bare.surfaces...rocky.outcrops", "Sand", "Downslope.Distance.Gradient", 
                         "Coniferous.forests", "Sparce.vegetation...in.evolution", 
                         "Anthropic.areas", "Altitude.difference", "Profile.curvature", 
                         "Stream.Power.Index", "General.curvature", "SOC.stock", "SOC", "Area")
missing_cols <- setdiff(expected_predictors, colnames(new_data))
if (length(missing_cols) > 0) {
  stop(paste("Error: New data is missing required columns:", paste(missing_cols, collapse = ", ")))
}

# Subset new data to include only predictors used in the models
# Note: Models use different subsets of predictors for each response variable and experiment
# The script will select the necessary columns for each model

# ------------------- Load Pre-Trained Models -------------------
# Load models from the model/ folder for all experiments and response variables
# Experiment 1 models
model_CH.C.O_1 <- readRDS("model/CH.C.O_df1_model.rds")      # CH.C.O model, Exp 1
model_CH.C.O.C_1 <- readRDS("model/CH.C.O.C_df1_model.rds")  # CH.C.O.C model, Exp 1
model_C.O.C.O.C_1 <- readRDS("model/C.O.C.O.C_df1_model.rds")  # C.O.C.O.C model, Exp 1
model_OMcat.C.O.C_1 <- readRDS("model/OMcat.C.O.C_df1_model.rds")  # OMcat.C.O.C model, Exp 1

# Experiment 2 models
model_CH.C.O_2 <- readRDS("model/CH.C.O_df2_model.rds")      # CH.C.O model, Exp 2
model_CH.C.O.C_2 <- readRDS("model/CH.C.O.C_df2_model.rds")  # CH.C.O.C model, Exp 2
model_C.O.C.O.C_2 <- readRDS("model/C.O.C.O.C_df2_model.rds")  # C.O.C.O.C model, Exp 2
model_OMcat.C.O.C_2 <- readRDS("model/OMcat.C.O.C_df2_model.rds")  # OMcat.C.O.C model, Exp 2

# Experiment 3 models
model_CH.C.O_3 <- readRDS("model/CH.C.O_df3_model.rds")      # CH.C.O model, Exp 3
model_CH.C.O.C_3 <- readRDS("model/CH.C.O.C_df3_model.rds")  # CH.C.O.C model, Exp 3
model_C.O.C.O.C_3 <- readRDS("model/C.O.C.O.C_df3_model.rds")  # C.O.C.O.C model, Exp 3
model_OMcat.C.O.C_3 <- readRDS("model/OMcat.C.O.C_df3_model.rds")  # OMcat.C.O.C model, Exp 3

# ------------------- Define Predictor Subsets for Each Model -------------------
# Each model uses a specific set of predictors based on feature selection from the original analysis
predictors_CH.C.O_1 <- c("Oak..hornbeam.and.chestnuts.forests", "VDCN", "CNBL", "Elevation", 
                         "Beech.forests", "Carbonate.Turbidites", "Aspect", "Clay", 
                         "Bare.surfaces...rocky.outcrops", "Sand")
predictors_CH.C.O.C_1 <- c("Oak..hornbeam.and.chestnuts.forests", "CNBL", "VDCN", "Elevation", 
                           "Beech.forests", "Aspect", "Carbonate.Turbidites", 
                           "Bare.surfaces...rocky.outcrops", "Clay", "Downslope.Distance.Gradient")
predictors_C.O.C.O.C_1 <- c("CNBL", "VDCN", "Elevation", "Oak..hornbeam.and.chestnuts.forests", 
                            "Beech.forests", "Downslope.Distance.Gradient", "Coniferous.forests", 
                            "Aspect", "Sparce.vegetation...in.evolution", "Carbonate.Turbidites")
predictors_OMcat.C.O.C_1 <- c("CNBL", "Oak..hornbeam.and.chestnuts.forests", "Elevation", 
                              "VDCN", "Downslope.Distance.Gradient", "Anthropic.areas", 
                              "Aspect", "Beech.forests", "Carbonate.Turbidites", "Altitude.difference")

predictors_CH.C.O_2 <- c("Oak..hornbeam.and.chestnuts.forests", "CNBL", "Elevation", "Aspect", 
                         "VDCN", "Beech.forests", "Carbonate.Turbidites", "Clay", 
                         "SOC.stock", "Anthropic.areas")
predictors_CH.C.O.C_2 <- c("Oak..hornbeam.and.chestnuts.forests", "CNBL", "VDCN", "Elevation", 
                           "Beech.forests", "Aspect", "Carbonate.Turbidites", 
                           "Bare.surfaces...rocky.outcrops", "Clay", "Downslope.Distance.Gradient")
predictors_C.O.C.O.C_2 <- c("CNBL", "Elevation", "Beech.forests", "Profile.curvature", 
                            "VDCN", "Downslope.Distance.Gradient", "Coniferous.forests", 
                            "Carbonate.Turbidites", "Aspect", "Oak..hornbeam.and.chestnuts.forests")
predictors_OMcat.C.O.C_2 <- c("CNBL", "Elevation", "Oak..hornbeam.and.chestnuts.forests", 
                              "Downslope.Distance.Gradient", "Aspect", "Beech.forests", 
                              "Profile.curvature", "Anthropic.areas", "Carbonate.Turbidites", 
                              "Coniferous.forests")

predictors_CH.C.O_3 <- c("Beech.forests", "CNBL", "Aspect", "Elevation", 
                         "Oak..hornbeam.and.chestnuts.forests", "Carbonate.Turbidites", 
                         "VDCN", "Stream.Power.Index", "SOC.stock", "General.curvature")
predictors_CH.C.O.C_3 <- c("CNBL", "Elevation", "Beech.forests", "Aspect", 
                           "Oak..hornbeam.and.chestnuts.forests", "VDCN", 
                           "Carbonate.Turbidites", "Coniferous.forests", "SOC.stock", 
                           "Downslope.Distance.Gradient")
predictors_C.O.C.O.C_3 <- c("CNBL", "Elevation", "Coniferous.forests", "Beech.forests", 
                            "Aspect", "Downslope.Distance.Gradient", "VDCN", 
                            "Oak..hornbeam.and.chestnuts.forests", "SOC", "SOC.stock")
predictors_OMcat.C.O.C_3 <- c("CNBL", "Elevation", "Beech.forests", "Coniferous.forests", 
                              "Aspect", "Downslope.Distance.Gradient", 
                              "Oak..hornbeam.and.chestnuts.forests", 
                              "Bare.surfaces...rocky.outcrops", "Carbonate.Turbidites", "Area")

# ------------------- Make Predictions -------------------
# Function to make predictions with a model and validate data
make_predictions <- function(model, data, predictors, response_name) {
  # Subset data to include only the required predictors
  if (!all(predictors %in% colnames(data))) {
    stop(paste("Error: Missing required predictors for", response_name, ":", 
               paste(setdiff(predictors, colnames(data)), collapse = ", ")))
  }
  data_subset <- data[, predictors, drop = FALSE]
  # Ensure no missing values in predictors
  if (any(!complete.cases(data_subset))) {
    warning(paste("Warning: Rows with missing values in predictors for", response_name, 
                  "will be excluded from predictions"))
    data_subset <- data_subset[complete.cases(data_subset), , drop = FALSE]
  }
  # Make predictions
  predictions <- predict(model, newdata = data_subset)
  return(predictions)
}

# Make predictions for each model
cat("Making predictions for new data...\n")

# Experiment 1 predictions
predictions_CH.C.O_1 <- make_predictions(model_CH.C.O_1, new_data, predictors_CH.C.O_1, "CH.C.O Exp 1")
predictions_CH.C.O.C_1 <- make_predictions(model_CH.C.O.C_1, new_data, predictors_CH.C.O.C_1, "CH.C.O.C Exp 1")
predictions_C.O.C.O.C_1 <- make_predictions(model_C.O.C.O.C_1, new_data, predictors_C.O.C.O.C_1, "C.O.C.O.C Exp 1")
predictions_OMcat.C.O.C_1 <- make_predictions(model_OMcat.C.O.C_1, new_data, predictors_OMcat.C.O.C_1, "OMcat.C.O.C Exp 1")

# Experiment 2 predictions
predictions_CH.C.O_2 <- make_predictions(model_CH.C.O_2, new_data, predictors_CH.C.O_2, "CH.C.O Exp 2")
predictions_CH.C.O.C_2 <- make_predictions(model_CH.C.O.C_2, new_data, predictors_CH.C.O.C_2, "CH.C.O.C Exp 2")
predictions_C.O.C.O.C_2 <- make_predictions(model_C.O.C.O.C_2, new_data, predictors_C.O.C.O.C_2, "C.O.C.O.C Exp 2")
predictions_OMcat.C.O.C_2 <- make_predictions(model_OMcat.C.O.C_2, new_data, predictors_OMcat.C.O.C_2, "OMcat.C.O.C Exp 2")

# Experiment 3 predictions
predictions_CH.C.O_3 <- make_predictions(model_CH.C.O_3, new_data, predictors_CH.C.O_3, "CH.C.O Exp 3")
predictions_CH.C.O.C_3 <- make_predictions(model_CH.C.O.C_3, new_data, predictors_CH.C.O.C_3, "CH.C.O.C Exp 3")
predictions_C.O.C.O.C_3 <- make_predictions(model_C.O.C.O.C_3, new_data, predictors_C.O.C.O.C_3, "C.O.C.O.C Exp 3")
predictions_OMcat.C.O.C_3 <- make_predictions(model_OMcat.C.O.C_3, new_data, predictors_OMcat.C.O.C_3, "OMcat.C.O.C Exp 3")

# ------------------- Combine and Save Predictions -------------------
# Combine predictions into a data frame
predictions_df <- data.frame(
  CH.C.O_Exp1 = predictions_CH.C.O_1,
  CH.C.O.C_Exp1 = predictions_CH.C.O.C_1,
  C.O.C.O.C_Exp1 = predictions_C.O.C.O.C_1,
  OMcat.C.O.C_Exp1 = predictions_OMcat.C.O.C_1,
  CH.C.O_Exp2 = predictions_CH.C.O_2,
  CH.C.O.C_Exp2 = predictions_CH.C.O.C_2,
  C.O.C.O.C_Exp2 = predictions_C.O.C.O.C_2,
  OMcat.C.O.C_Exp2 = predictions_OMcat.C.O.C_2,
  CH.C.O_Exp3 = predictions_CH.C.O_3,
  CH.C.O.C_Exp3 = predictions_CH.C.O.C_3,
  C.O.C.O.C_Exp3 = predictions_C.O.C.O.C_3,
  OMcat.C.O.C_Exp3 = predictions_OMcat.C.O.C_3
)

# Save predictions to a CSV file
write.csv(predictions_df, "predictions_new_data.csv", row.names = FALSE)
cat("Predictions saved to 'predictions_new_data.csv'\n")

# ------------------- Notes -------------------
# - Predictions are made for each response variable (CH.C.O, CH.C.O.C, C.O.C.O.C, OMcat.C.O.C) 
#   across three experiments.
# - Ensure your input data has no missing values in the required predictors to avoid errors.
# - If you encounter issues, verify that column names match exactly and that data types are consistent 
#   (e.g., numeric for continuous variables, factor for categorical if applicable).
# - For further assistance, contact [your email or corresponding author’s contact information].
