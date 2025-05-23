# Title: Integration of FTIR-based soil degradation indices with stochastic modelling
# Authors: Manuel La Licata, Odunayo D. Adeniyi, Ruth H. Ellerbrock, Nisha Bhattarai, 
#          Alberto Bosino, Natalie Papke, Jörg Schaller, Michael Maerker 
# Code written by: Odunayo David Adeniyi
# Description: R code for feature selection and random forest modeling of soil degradation indices
# Manuscript: Assessing spatial patterns of organic matter-sediment dynamics in a Mediterranean watershed
# Date: January 2025

# ------------------- Data Loading -------------------
# Read CSV files containing data for three experiments
# Each file includes response variables (columns 1-4) and predictors (columns 5-44)
df1 = read.csv('data/Experiment1.csv')  # Experiment 1 dataset
df2 = read.csv('data/Experiment2.csv')  # Experiment 2 dataset
df3 = read.csv('data/Experiment3.csv')  # Experiment 3 dataset

# ------------------- Data Preparation for Experiment 1 -------------------
# Subset data for each response variable, keeping relevant predictors (columns 5-44)
# CH.C.O: Relative hydrophobicity of SOM
# CH.C.O.C, C.O.C.O.C: Proxies for organic-matter-cation associations
# OMcat.C.O.C: Proxy for Cation Exchange Capacity (CEC)
CH.C.O_df1 = df1[, c(1, 5:44)]      # Subset for CH.C.O (column 1)
CH.C.O.C_df1 = df1[, c(2, 5:44)]    # Subset for CH.C.O.C (column 2)
C.O.C.O.C_df1 = df1[, c(3, 5:44)]   # Subset for C.O.C.O.C (column 3)
OMcat.C.O.C_df1 = df1[, c(4, 5:44)] # Subset for OMcat.C.O.C (column 4)

# ------------------- Data Preparation for Experiment 2 -------------------
# Same subsetting process for Experiment 2 dataset
CH.C.O_df2 = df2[, c(1, 5:44)]      # Subset for CH.C.O
CH.C.O.C_df2 = df2[, c(2, 5:44)]    # Subset for CH.C.O.C
C.O.C.O.C_df2 = df2[, c(3, 5:44)]   # Subset for C.O.C.O.C
OMcat.C.O.C_df2 = df2[, c(4, 5:44)] # Subset for OMcat.C.O.C

# ------------------- Data Preparation for Experiment 3 -------------------
# Same subsetting process for Experiment 3 dataset
CH.C.O_df3 = df3[, c(1, 5:44)]      # Subset for CH.C.O
CH.C.O.C_df3 = df3[, c(2, 5:44)]    # Subset for CH.C.O.C
C.O.C.O.C_df3 = df3[, c(3, 5:44)]   # Subset for C.O.C.O.C
OMcat.C.O.C_df3 = df3[, c(4, 5:44)] # Subset for OMcat.C.O.C

# ------------------- Feature Selection for Experiment 1 -------------------
# Load FSelector package for correlation-based feature ranking
library(FSelector)

# Feature selection for CH.C.O (Experiment 1)
# Rank predictors based on correlation with CH.C.O
w1_CH.C.O <- rank.correlation(CH.C.O ~ ., CH.C.O_df1)
print(w1_CH.C.O)  # Display correlation rankings
subset <- cutoff.k(w1_CH.C.O, 10)  # Select top 10 predictors
positions <- match(subset, colnames(CH.C.O_df1))  # Get column indices of selected predictors
positions_str <- paste(positions, collapse = ", ")  # Create string of indices for display
CH.C.O_train_1 <- CH.C.O_df1[, c(1, positions)]  # Subset data with selected predictors
print(positions_str)  # Print column indices
f <- as.simple.formula(subset, "CH.C.O")  # Create formula for modeling
print(f)  # Display formula
# Define formula with top 10 predictors for CH.C.O
CH.C.O_df1_fo <- CH.C.O ~ Oak..hornbeam.and.chestnuts.forests + VDCN + CNBL + 
  Elevation + Beech.forests + Carbonate.Turbidites + Aspect + 
  Clay + Bare.surfaces...rocky.outcrops + Sand

# Feature selection for CH.C.O.C (Experiment 1)
w1_CH.C.O.C <- rank.correlation(CH.C.O.C ~ ., CH.C.O.C_df1)
print(w1_CH.C.O.C)  # Display correlation rankings
subset <- cutoff.k(w1_CH.C.O.C, 10)  # Select top 10 predictors
positions <- match(subset, colnames(CH.C.O.C_df1))  # Get column indices
positions_str <- paste(positions, collapse = ", ")  # Create string of indices
CH.C.O.C_train_1 <- CH.C.O.C_df1[, c(1, 29, 17, 18, 9, 28, 23, 39, 33, 5, 11)]  # Subset with specific columns
print(positions_str)  # Print column indices
f <- as.simple.formula(subset, "CH.C.O.C")  # Create formula
print(f)  # Display formula
# Define formula for CH.C.O.C
CH.C.O.C_df1_fo <- CH.C.O.C ~ Oak..hornbeam.and.chestnuts.forests + CNBL + VDCN + 
  Elevation + Beech.forests + Aspect + Carbonate.Turbidites + 
  Bare.surfaces...rocky.outcrops + Clay + Downslope.Distance.Gradient

# Feature selection for C.O.C.O.C (Experiment 1)
w1_C.O.C.O.C <- rank.correlation(C.O.C.O.C ~ ., C.O.C.O.C_df1)
print(w1_C.O.C.O.C)  # Display correlation rankings
subset <- cutoff.k(w1_C.O.C.O.C, 10)  # Select top 10 predictors
positions <- match(subset, colnames(C.O.C.O.C_df1))  # Get column indices
positions_str <- paste(positions, collapse = ", ")  # Create string of indices
C.O.C.O.C_train_1 <- C.O.C.O.C_df1[, c(1, 29, 39, 34, 23, 27, 17, 18, 28, 9, 11)]  # Subset with specific columns
f <- as.simple.formula(subset, "C.O.C.O.C")  # Create formula
print(f)  # Display formula
# Define formula for C.O.C.O.C
C.O.C.O.C_df1_fo <- C.O.C.O.C ~ CNBL + VDCN + Elevation + Oak..hornbeam.and.chestnuts.forests + 
  Beech.forests + Downslope.Distance.Gradient + Coniferous.forests + 
  Aspect + Sparce.vegetation...in.evolution + Carbonate.Turbidites

# Feature selection for OMcat.C.O.C (Experiment 1)
w1_OMcat.C.O.C <- rank.correlation(OMcat.C.O.C ~ ., OMcat.C.O.C_df1)
print(w1_OMcat.C.O.C)  # Display correlation rankings
subset <- cutoff.k(w1_OMcat.C.O.C, 10)  # Select top 10 predictors
positions <- match(subset, colnames(OMcat.C.O.C_df1))  # Get column indices
positions_str <- paste(positions, collapse = ", ")  # Create string of indices
OMcat.C.O.C_train_1 <- OMcat.C.O.C_df1[, c(1, 18, 9, 17, 11, 23, 29, 28, 39, 10, 32)]  # Subset with specific columns
f <- as.simple.formula(subset, "OMcat.C.O.C")  # Create formula
print(f)  # Display formula
# Define formula for OMcat.C.O.C
OMcat.C.O.C_df1_fo <- OMcat.C.O.C ~ CNBL + Oak..hornbeam.and.chestnuts.forests + Elevation + 
  VDCN + Downslope.Distance.Gradient + Anthropic.areas + Aspect + 
  Beech.forests + Carbonate.Turbidites + Altitude.difference

# ------------------- Feature Selection for Experiment 2 -------------------
# Feature selection for CH.C.O (Experiment 2)
w2_CH.C.O <- rank.correlation(CH.C.O ~ ., CH.C.O_df2)
print(w2_CH.C.O)  # Display correlation rankings
subset <- cutoff.k(w2_CH.C.O, 10)  # Select top 10 predictors
positions <- match(subset, colnames(CH.C.O_df2))  # Get column indices
positions_str <- paste(positions, collapse = ", ")  # Create string of indices
CH.C.O_train_2 <- CH.C.O_df2[, c(1, 29, 28, 39, 32, 18, 9, 23, 17, 2, 5)]  # Subset with specific columns
f <- as.simple.formula(subset, "CH.C.O")  # Create formula
print(f)  # Display formula
# Define formula for CH.C.O
CH.C.O_df2_fo <- CH.C.O ~ Oak..hornbeam.and.chestnuts.forests + CNBL + Elevation + 
  Aspect + VDCN + Beech.forests + Carbonate.Turbidites + Clay + 
  SOC.stock + Anthropic.areas

# Feature selection for CH.C.O.C (Experiment 2

)
w2_CH.C.O.C <- rank.correlation(CH.C.O.C ~ ., CH.C.O.C_df2)
print(w2_CH.C.O.C)  # Display correlation rankings
subset <- cutoff.k(w2_CH.C.O.C, 10)  # Select top 10 predictors
positions <- match(subset, colnames(CH.C.O.C_df2))  # Get column indices
positions_str <- paste(positions, collapse = ", ")  # Create string of indices
CH.C.O.C_train_2 <- CH.C.O.C_df2[, c(1, 29, 28, 39, 33, 23, 17, 18, 9, 5, 11)]  # Subset with specific columns
f <- as.simple.formula(subset, "CH.C.O.C")  # Create formula
print(f)  # Display formula
# Define formula for CH.C.O.C
CH.C.O.C_df2_fo <- CH.C.O.C ~ Oak..hornbeam.and.chestnuts.forests + CNBL + VDCN + 
  Elevation + Beech.forests + Aspect + Carbonate.Turbidites + 
  Bare.surfaces...rocky.outcrops + Clay + Downslope.Distance.Gradient

# Feature selection for C.O.C.O.C (Experiment 2)
w2_C.O.C.O.C <- rank.correlation(C.O.C.O.C ~ ., C.O.C.O.C_df2)
print(w2_C.O.C.O.C)  # Display correlation rankings
subset <- cutoff.k(w2_C.O.C.O.C, 10)  # Select top 10 predictors
positions <- match(subset, colnames(C.O.C.O.C_df2))  # Get column indices
positions_str <- paste(positions, collapse = ", ")  # Create string of indices
C.O.C.O.C_train_2 <- C.O.C.O.C_df2[, c(1, 18, 9, 28, 19, 17, 11, 27, 39, 23, 29)]  # Subset with specific columns
f <- as.simple.formula(subset, "C.O.C.O.C")  # Create formula
print(f)  # Display formula
# Define formula for C.O.C.O.C
C.O.C.O.C_df2_fo <- C.O.C.O.C ~ CNBL + Elevation + Beech.forests + Profile.curvature + 
  VDCN + Downslope.Distance.Gradient + Coniferous.forests + 
  Carbonate.Turbidites + Aspect + Oak..hornbeam.and.chestnuts.forests

# Feature selection for OMcat.C.O.C (Experiment 2)
w2_OMcat.C.O.C <- rank.correlation(OMcat.C.O.C ~ ., OMcat.C.O.C_df2)
print(w2_OMcat.C.O.C)  # Display correlation rankings
subset <- cutoff.k(w2_OMcat.C.O.C, 10)  # Select top 10 predictors
positions <- match(subset, colnames(OMcat.C.O.C_df2))  # Get column indices
positions_str <- paste(positions, collapse = ", ")  # Create string of indices
OMcat.C.O.C_train_2 <- OMcat.C.O.C_df2[, c(1, 29, 32, 39, 27, 28, 18, 23, 11, 9, 19)]  # Subset with specific columns
f <- as.simple.formula(subset, "OMcat.C.O.C")  # Create formula
print(f)  # Display formula
# Define formula for OMcat.C.O.C
OMcat.C.O.C_df2_fo <- OMcat.C.O.C ~ CNBL + Elevation + Oak..hornbeam.and.chestnuts.forests + 
  Downslope.Distance.Gradient + Aspect + Beech.forests + Profile.curvature + 
  Anthropic.areas + Carbonate.Turbidites + Coniferous.forests

# ------------------- Feature Selection for Experiment 3 -------------------
# Feature selection for CH.C.O (Experiment 3)
w3_CH.C.O <- rank.correlation(CH.C.O ~ ., CH.C.O_df3)
print(w3_CH.C.O)  # Display correlation rankings
subset <- cutoff.k(w3_CH.C.O, 10)  # Select top 10 predictors
positions <- match(subset, colnames(CH.C.O_df3))  # Get column indices
positions_str <- paste(positions, collapse = ", ")  # Create string of indices
CH.C.O_train_3 <- CH.C.O_df3[, c(1, 29, 39, 21, 28, 23, 9, 17, 16, 2, 18)]  # Subset with specific columns
f <- as.simple.formula(subset, "CH.C.O")  # Create formula
print(f)  # Display formula
# Define formula for CH.C.O
CH.C.O_df3_fo <- CH.C.O ~ Beech.forests + CNBL + Aspect + Elevation + Oak..hornbeam.and.chestnuts.forests + 
  Carbonate.Turbidites + VDCN + Stream.Power.Index + SOC.stock + 
  General.curvature

# Feature selection for CH.C.O.C (Experiment 3)
w3_CH.C.O.C <- rank.correlation(CH.C.O.C ~ ., CH.C.O.C_df3)
print(w3_CH.C.O.C)  # Display correlation rankings
subset <- cutoff.k(w3_CH.C.O.C, 10)  # Select top 10 predictors
positions <- match(subset, colnames(CH.C.O.C_df3))  # Get column indices
positions_str <- paste(positions, collapse = ", ")  # Create string of indices
CH.C.O.C_train_3 <- CH.C.O.C_df3[, c(1, 9, 28, 23, 29, 17, 18, 11, 2, 39, 27)]  # Subset with specific columns
f <- as.simple.formula(subset, "CH.C.O.C")  # Create formula
print(f)  # Display formula
# Define formula for CH.C.O.C
CH.C.O.C_df3_fo <- CH.C.O.C ~ CNBL + Elevation + Beech.forests + Aspect + Oak..hornbeam.and.chestnuts.forests + 
  VDCN + Carbonate.Turbidites + Coniferous.forests + SOC.stock + 
  Downslope.Distance.Gradient

# Feature selection for C.O.C.O.C (Experiment 3)
w3_C.O.C.O.C <- rank.correlation(C.O.C.O.C ~ ., C.O.C.O.C_df3)
print(w3_C.O.C.O.C)  # Display correlation rankings
subset <- cutoff.k(w3_C.O.C.O.C, 10)  # Select top 10 predictors
positions <- match(subset, colnames(C.O.C.O.C_df3))  # Get column indices
positions_str <- paste(positions, collapse = ", ")  # Create string of indices
C.O.C.O.C_train_3 <- C.O.C.O.C_df3[, c(1, 17, 18, 29, 2, 28, 9, 11, 3, 27, 23)]  # Subset with specific columns
f <- as.simple.formula(subset, "C.O.C.O.C")  # Create formula
print(f)  # Display formula
# Define formula for C.O.C.O.C
C.O.C.O.C_df3_fo <- C.O.C.O.C ~ CNBL + Elevation + Coniferous.forests + Beech.forests + 
  Aspect + Downslope.Distance.Gradient + VDCN + Oak..hornbeam.and.chestnuts.forests + 
  SOC + SOC.stock

# Feature selection for OMcat.C.O.C (Experiment 3)
w3_OMcat.C.O.C <- rank.correlation(OMcat.C.O.C ~ ., OMcat.C.O.C_df3)
print(w3_OMcat.C.O.C)  # Display correlation rankings
subset <- cutoff.k(w3_OMcat.C.O.C, 10)  # Select top 10 predictors
positions <- match(subset, colnames(OMcat.C.O.C_df3))  # Get column indices
positions_str <- paste(positions, collapse = ", ")  # Create string of indices
OMcat.C.O.C_train_3 <- OMcat.C.O.C_df3[, c(1, 18, 9, 28, 39, 29, 23, 11, 24, 33, 27)]  # Subset with specific columns
f <- as.simple.formula(subset, "OMcat.C.O.C")  # Create formula
print(f)  # Display formula
# Define formula for OMcat.C.O.C
OMcat.C.O.C_df3_fo <- OMcat.C.O.C ~ CNBL + Elevation + Beech.forests + Coniferous.forests + 
  Aspect + Downslope.Distance.Gradient + Oak..hornbeam.and.chestnuts.forests + 
  Bare.surfaces...rocky.outcrops + Carbonate.Turbidites + Area

# ------------------- Random Forest Modeling -------------------
# Define training control for Random Forest models
# Use 10-fold cross-validation with 50 repeats
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 50,
                     savePredictions = "final", returnResamp = "all",
                     verboseIter = FALSE)
# Define tuning grid for mtry parameter (number of variables to consider at each split)
tg <- expand.grid(mtry = c(2:10))

# ------------------- Modeling for Experiment 1 -------------------
# CH.C.O: Leave-one-out cross-validation (LOOCV) for Random Forest
CH.C.O_df1_looPred <- numeric(nrow(CH.C.O_df1))  # Initialize prediction vector
for (i in 1:nrow(CH.C.O_df1)) {
  # Train Random Forest model, excluding the i-th row
  CH.C.O_df1_model <- caret::train(CH.C.O_df1_fo, CH.C.O_df1[-i, ], trControl = ctrl,
                                   method = "rf", tuneGrid = tg)
  # Predict for the i-th row
  CH.C.O_df1_looPred[i] <- predict(CH.C.O_df1_model, newdata = CH.C.O_df1[i, ])
}
# Evaluate model performance using goof function from ithir package
CH.C.O_df1_result <- ithir::goof(predicted = CH.C.O_df1_looPred, observed = CH.C.O_df1$CH.C.O, type = 'spec', plot.it = TRUE)
saveRDS(CH.C.O_df1_result, 'CH.C.O_df1_result.rds')  # Save model performance metrics
saveRDS(CH.C.O_df1_model, 'CH.C.O_df1_model.rds')    # Save trained model
NSE(sim = CH.C.O_df1_looPred, obs = CH.C.O_df1$CH.C.O)  # Calculate Nash-Sutcliffe Efficiency

# CH.C.O.C: LOOCV for Random Forest
CH.C.O.C_df1_looPred <- numeric(nrow(CH.C.O.C_df1))  # Initialize prediction vector
for (i in 1:nrow(CH.C.O.C_df1)) {
  CH.C.O.C_df1_model <- caret::train(CH.C.O.C_df1_fo, CH.C.O.C_df1[-i, ], trControl = ctrl,
                                     method = "rf", tuneGrid = tg)
  CH.C.O.C_df1_looPred[i] <- predict(CH.C.O.C_df1_model, newdata = CH.C.O.C_df1[i, ])
}
CH.C.O.C_df1_result <- ithir::goof(predicted = CH.C.O.C_df1_looPred, observed = CH.C.O.C_df1$CH.C.O.C)
saveRDS(CH.C.O.C_df1_result, 'CH.C.O.C_df1_result.rds')  # Save model performance metrics
saveRDS(CH.C.O.C_df1_model, 'CH.C.O.C_df1_model.rds')    # Save trained model
NSE(sim = CH.C.O.C_df1_looPred, obs = CH.C.O.C_df1$CH.C.O.C)  # Calculate NSE

# C.O.C.O.C: LOOCV for Random Forest
C.O.C.O.C_df1_looPred <- numeric(nrow(C.O.C.O.C_df1))  # Initialize prediction vector
for (i in 1:nrow(C.O.C.O.C_df1)) {
  C.O.C.O.C_df1_model <- caret::train(C.O.C.O.C_df1_fo, C.O.C.O.C_df1[-i, ], trControl = ctrl,
                                      method = "rf", tuneGrid = tg)
  C.O.C.O.C_df1_looPred[i] <- predict(C.O.C.O.C_df1_model, newdata = C.O.C.O.C_df1[i, ])
}
C.O.C.O.C_df1_result <- ithir::goof(predicted = C.O.C.O.C_df1_looPred, observed = C.O.C.O.C_df1$C.O.C.O.C)
saveRDS(C.O.C.O.C_df1_result, 'C.O.C.O.C_df1_result.rds')  # Save model performance metrics
saveRDS(C.O.C.O.C_df1_model, 'C.O.C.O.C_df1_model.rds')    # Save trained model

# OMcat.C.O.C: LOOCV for Random Forest
OMcat.C.O.C_df1_looPred <- numeric(nrow(OMcat.C.O.C_df1))  # Initialize prediction vector
for (i in 1:nrow(OMcat.C.O.C_df1)) {
  OMcat.C.O.C_df1_model <- caret::train(OMcat.C.O.C_df1_fo, OMcat.C.O.C_df1[-i, ], trControl = ctrl,
                                        method = "rf", tuneGrid = tg)
  OMcat.C.O.C_df1_looPred[i] <- predict(OMcat.C.O.C_df1_model, newdata = OMcat.C.O.C_df1[i, ])
}
OMcat.C.O.C_df1_result <- ithir::goof(predicted = OMcat.C.O.C_df1_looPred, observed = OMcat.C.O.C_df1$OMcat.C.O.C)
saveRDS(OMcat.C.O.C_df1_result, 'OMcat.C.O.C_df1_result.rds')  # Save model performance metrics
saveRDS(OMcat.C.O.C_df1_model, 'OMcat.C.O.C_df1_model.rds')    # Save trained model

# ------------------- Modeling for Experiment 2 -------------------
# CH.C.O: LOOCV for Random Forest
CH.C.O_df2_looPred <- numeric(nrow(CH.C.O_df2))  # Initialize prediction vector
for (i in 1:nrow(CH.C.O_df2)) {
  CH.C.O_df2_model <- caret::train(CH.C.O_df2_fo, CH.C.O_df2[-i, ], trControl = ctrl,
                                   method = "rf", tuneGrid = tg)
  CH.C.O_df2_looPred[i] <- predict(CH.C.O_df2_model, newdata = CH.C.O_df2[i, ])
}
CH.C.O_df2_result <- ithir::goof(predicted = CH.C.O_df2_looPred, observed = CH.C.O_df2$CH.C.O)
saveRDS(CH.C.O_df2_result, 'CH.C.O_df2_result.rds')  # Save model performance metrics
saveRDS(CH.C.O_df2_model, 'CH.C.O_df2_model.rds')    # Save trained model

# CH.C.O.C: LOOCV for Random Forest
CH.C.O.C_df2_looPred <- numeric(nrow(CH.C.O.C_df2))  # Initialize prediction vector
for (i in 1:nrow(CH.C.O.C_df2)) {
  CH.C.O.C_df2_model <- caret::train(CH.C.O.C_df2_fo, CH.C.O.C_df2[-i, ], trControl = ctrl,
                                     method = "rf", tuneGrid = tg)
  CH.C.O.C_df2_looPred[i] <- predict(CH.C.O.C_df2_model, newdata = CH.C.O.C_df2[i, ])
}
CH.C.O.C_df2_result <- ithir::goof(predicted = CH.C.O.C_df2_looPred, observed = CH.C.O.C_df2$CH.C.O.C)
saveRDS(CH.C.O.C_df2_result, 'CH.C.O.C_df2_result.rds')  # Save model performance metrics
saveRDS(CH.C.O.C_df2_model, 'CH.C.O.C_df2_model.rds')    # Save trained model

# C.O.C.O.C: LOOCV for Random Forest
C.O.C.O.C_df2_looPred <- numeric(nrow(C.O.C.O.C_df2))  # Initialize prediction vector
for (i in 1:nrow(C.O.C.O.C_df2)) {
  C.O.C.O.C_df2_model <- caret::train(C.O.C.O.C_df2_fo, C.O.C.O.C_df2[-i, ], trControl = ctrl,
                                      method = "rf", tuneGrid = tg)
  C.O.C.O.C_df2_looPred[i] <- predict(C.O.C.O.C_df2_model, newdata = C.O.C.O.C_df2[i, ])
}
C.O.C.O.C_df2_result <- ithir::goof(predicted = C.O.C.O.C_df2_looPred, observed = C.O.C.O.C_df2$C.O.C.O.C)
saveRDS(C.O.C.O.C_df2_result, 'C.O.C.O.C_df2_result.rds')  # Save model performance metrics
saveRDS(C.O.C.O.C_df2_model, 'C.O.C.O.C_df2_model.rds')    # Save trained model

# OMcat.C.O.C: LOOCV for Random Forest
OMcat.C.O.C_df2_looPred <- numeric(nrow(OMcat.C.O.C_df2))  # Initialize prediction vector
for (i in 1:nrow(OMcat.C.O.C_df2)) {
  OMcat.C.O.C_df2_model <- caret::train(OMcat.C.O.C_df2_fo, OMcat.C.O.C_df2[-i, ], trControl = ctrl,
                                        method = "rf", tuneGrid = tg)
  OMcat.C.O.C_df2_looPred[i] <- predict(OMcat.C.O.C_df2_model, newdata = OMcat.C.O.C_df2[i, ])
}
OMcat.C.O.C_df2_result <- ithir::goof(predicted = OMcat.C.O.C_df2_looPred, observed = OMcat.C.O.C_df2$OMcat.C.O.C)
saveRDS(OMcat.C.O.C_df2_result, 'OMcat.C.O.C_df2_result.rds')  # Save model performance metrics
saveRDS(OMcat.C.O.C_df2_model, 'OMcat.C.O.C_df2_model.rds')    # Save trained model

# ------------------- Modeling for Experiment 3 -------------------
# Remove rows with missing values for Experiment 3 datasets
CH.C.O_df3 <- CH.C.O_df3[complete.cases(CH.C.O_df3), ]  # Ensure no NA values
# CH.C.O: LOOCV for Random Forest
CH.C.O_df3_looPred <- numeric(nrow(CH.C.O_df3))  # Initialize prediction vector
for (i in 1:nrow(CH.C.O_df3)) {
  CH.C.O_df3_model <- caret::train(CH.C.O_df3_fo, CH.C.O_df3[-i, ], trControl = ctrl,
                                   method = "rf", tuneGrid = tg)
  CH.C.O_df3_looPred[i] <- predict(CH.C.O_df3_model, newdata = CH.C.O_df3[i, ])
}
CH.C.O_df3_result <- ithir::goof(predicted = CH.C.O_df3_looPred, observed = CH.C.O_df3$CH.C.O)
saveRDS(CH.C.O_df3_result, 'CH.C.O_df3_result.rds')  # Save model performance metrics
saveRDS(CH.C.O_df3_model, 'CH.C.O_df3_model.rds')    # Save trained model

CH.C.O.C_df3 <- CH.C.O.C_df3[complete.cases(CH.C.O.C_df3), ]  # Ensure no NA values
# CH.C.O.C: LOOCV for Random Forest
CH.C.O.C_df3_looPred <- numeric(nrow(CH.C.O.C_df3))  # Initialize prediction vector
for (i in 1:nrow(CH.C.O.C_df3)) {
  CH.C.O.C_df3_model <- caret::train(CH.C.O.C_df3_fo, CH.C.O.C_df3[-i, ], trControl = ctrl,
                                     method = "rf", tuneGrid = tg)
  CH.C.O.C_df3_looPred[i] <- predict(CH.C.O.C_df3_model, newdata = CH.C.O.C_df3[i, ])
}
CH.C.O.C_df3_result <- ithir::goof(predicted = CH.C.O.C_df3_looPred, observed = CH.C.O.C_df3$CH.C.O.C)
saveRDS(CH.C.O.C_df3_result, 'CH.C.O.C_df3_result.rds')  # Save model performance metrics
saveRDS(CH.C.O.C_df3_model, 'CH.C.O.C_df3_model.rds')    # Save trained model

C.O.C.O.C_df3 <- C.O.C.O.C_df3[complete.cases(C.O.C.O.C_df3), ]  # Ensure no NA values
# C.O.C.O.C: LOOCV for Random Forest
C.O.C.O.C_df3_looPred <- numeric(nrow(C.O.C.O.C_df3))  # Initialize prediction vector
for (i in 1:nrow(C.O.C.O.C_df3)) {
  C.O.C.O.C_df3_model <- caret::train(C.O.C.O.C_df3_fo, C.O.C.O.C_df3[-i, ], trControl = ctrl,
                                      method = "rf", tuneGrid = tg)
  C.O.C.O.C_df3_looPred[i] <- predict(C.O.C.O.C_df3_model, newdata = C.O.C.O.C_df3[i, ])
}
C.O.C.O.C_df3_result <- ithir::goof(predicted = C.O.C.O.C_df3_looPred, observed = C.O.C.O.C_df3$C.O.C.O.C)
saveRDS(C.O.C.O.C_df3_result, 'C.O.C.O.C_df3_result.rds')  # Save model performance metrics
saveRDS(C.O.C.O.C_df3_model, 'C.O.C.O.C_df3_model.rds')    # Save trained model

OMcat.C.O.C_df3 <- OMcat.C.O.C_df3[complete.cases(OMcat.C.O.C_df3), ]  # Ensure no NA values
# OMcat.C.O.C: LOOCV for Random Forest
OMcat.C.O.C_df3_looPred <- numeric(nrow(OMcat.C.O.C_df3))  # Initialize prediction vector
for (i in 1:nrow(OMcat.C.O.C_df3)) {
  OMcat.C.O.C_df3_model <- caret::train(OMcat.C.O.C_df3_fo, OMcat.C.O.C_df3[-i, ], trControl = ctrl,
                                        method = "rf", tuneGrid = tg)
  OMcat.C.O.C_df3_looPred[i] <- predict(OMcat.C.O.C_df3_model, newdata = OMcat.C.O.C_df3[i, ])
}
OMcat.C.O.C_df3_result <- ithir::goof(predicted = OMcat.C.O.C_df3_looPred, observed = OMcat.C.O.C_df3$OMcat.C.O.C)
saveRDS(OMcat.C.O.C_df3_result, 'OMcat.C.O.C_df3_result.rds')  # Save model performance metrics
saveRDS(OMcat.C.O.C_df3_model, 'OMcat.C.O.C_df3_model.rds')    # Save trained model

# ------------------- Feature Importance Analysis -------------------
# Load iml package for feature importance and Accumulated Local Effects (ALE) plots
library(iml)

# Prepare predictor datasets (exclude response variables)
CH.C.O_XX1 <- CH.C.O_train_1[which(names(CH.C.O_train_1) != "CH.C.O")]  # Predictors for CH.C.O, Exp 1
CH.C.O_XX2 <- CH.C.O_train_2[which(names(CH.C.O_train_2) != "CH.C.O")]  # Predictors for CH.C.O, Exp 2
CH.C.O_XX3 <- CH.C.O_train_3[which(names(CH.C.O_train_3) != "CH.C.O")]  # Predictors for CH.C.O, Exp 3

CH.C.O.C_XX1 <- CH.C.O.C_train_1[which(names(CH.C.O.C_train_1) != "CH.C.O.C")]  # Predictors for CH.C.O.C, Exp 1
CH.C.O.C_XX2 <- CH.C.O.C_train_2[which(names(CH.C.O.C_train_2) != "CH.C.O.C")]  # Predictors for CH.C.O.C, Exp 2
CH.C.O.C_XX3 <- CH.C.O.C_train_3[which(names(CH.C.O.C_train_3) != "CH.C.O.C")]  # Predictors for CH.C.O.C, Exp 3

C.O.C.O.C_XX1 <- C.O.C.O.C_train_1[which(names(C.O.C.O.C_train_1) != "C.O.C.O.C")]  # Predictors for C.O.C.O.C, Exp 1
C.O.C.O.C_XX2 <- C.O.C.O.C_train_2[which(names(C.O.C.O.C_train_2) != "C.O.C.O.C")]  # Predictors for C.O.C.O.C, Exp 2
C.O.C.O.C_XX3 <- C.O.C.O.C_train_3[which(names(C.O.C.O.C_train_3) != "C.O.C.O.C")]  # Predictors for C.O.C.O.C, Exp 3

OMcat.C.O.C_XX1 <- OMcat.C.O.C_train_1[which(names(OMcat.C.O.C_train_1) != "OMcat.C.O.C")]  # Predictors for OMcat.C.O.C, Exp 1
OMcat.C.O.C_XX2 <- OMcat.C.O.C_train_2[which(names(OMcat.C.O.C_train_2) != "OMcat.C.O.C")]  # Predictors for OMcat.C.O.C, Exp 2
OMcat.C.O.C_XX3 <- OMcat.C.O.C_train_3[which(names(OMcat.C.O.C_train_3) != "OMcat.C.O.C")]  # Predictors for OMcat.C.O.C, Exp 3

# Feature importance for CH.C.O across experiments
CH.C.O.predictor1 <- Predictor$new(CH.C.O_df1_model, data = CH.C.O_XX1, y = CH.C.O_train_1$CH.C.O)  # Predictor object for Exp 1
CH.C.O.predictor2 <- Predictor$new(CH.C.O_df2_model, data = CH.C.O_XX2, y = CH.C.O_train_2$CH.C.O)  # Predictor object for Exp 2
CH.C.O.predictor3 <- Predictor$new(CH.C.O_df3_model, data = CH.C.O_XX3, y = CH.C.O_train_3$CH.C.O)  # Predictor object for Exp 3
CH.C.O_imp.1 <- FeatureImp$new(CH.C.O.predictor1, loss = "rmse", n.repetitions = 1000)  # Feature importance with 1000 permutations
CH.C.O_imp.2 <- FeatureImp$new(CH.C.O.predictor2, loss = "rmse", n.repetitions = 1000)
CH.C.O_imp.3 <- FeatureImp$new(CH.C.O.predictor3, loss = "rmse", n.repetitions = 1000)
# Plot feature importance for CH.C.O
CH.C.O.p1 <- plot(CH.C.O_imp.1) + ggtitle("CH.C.O Exp 1")  # Plot for Exp 1
CH.C.O.p2 <- plot(CH.C.O_imp.2) + ggtitle("CH.C.O Exp 2")  # Plot for Exp 2
CH.C.O.p3 <- plot(CH.C.O_imp.3) + ggtitle("CH.C.O Exp 3")  # Plot for Exp 3
gridExtra::grid.arrange(CH.C.O.p1, CH.C.O.p2, CH.C.O.p3, nrow = 1)  # Arrange plots in one row

# Feature importance for CH.C.O.C across experiments
CH.C.O.C.predictor1 <- Predictor$new(CH.C.O.C_df1_model, data = CH.C.O.C_XX1, y = CH.C.O.C_train_1$CH.C.O.C)
CH.C.O.C.predictor2 <- Predictor$new(CH.C.O.C_df2_model, data = CH.C.O.C_XX2, y = CH.C.O.C_train_2$CH.C.O.C)
CH.C.O.C.predictor3 <- Predictor$new(CH.C.O.C_df3_model, data = CH.C.O.C_XX3, y = CH.C.O.C_train_3$CH.C.O.C)
CH.C.O.C_imp.1 <- FeatureImp$new(CH.C.O.C.predictor1, loss = "rmse", n.repetitions = 1000)
CH.C.O.C_imp.2 <- FeatureImp$new(CH.C.O.C.predictor2, loss = "rmse", n.repetitions = 1000)
CH.C.O.C_imp.3 <- FeatureImp$new(CH.C.O.C.predictor3, loss = "rmse", n.repetitions = 1000)
# Plot feature importance for CH.C.O.C
CH.C.O.C.p1 <- plot(CH.C.O.C_imp.1) + ggtitle("CH.C.O.C Exp 1")
CH.C.O.C.p2 <- plot(CH.C.O.C_imp.2) + ggtitle("CH.C.O.C Exp 2")
CH.C.O.C.p3 <- plot(CH.C.O.C_imp.3) + ggtitle("CH.C.O.C Exp 3")
gridExtra::grid.arrange(CH.C.O.C.p1, CH.C.O.C.p2, CH.C.O.C.p3, nrow = 1)

# Feature importance for C.O.C.O.C across experiments
C.O.C.O.C.predictor1 <- Predictor$new(C.O.C.O.C_df1_model, data = C.O.C.O.C_XX1, y = C.O.C.O.C_train_1$C.O.C.O.C)
C.O.C.O.C.predictor2 <- Predictor$new(C.O.C.O.C_df2_model, data = C.O.C.O.C_XX2, y = C.O.C.O.C_train_2$C.O.C.O.C)
C.O.C.O.C.predictor3 <- Predictor$new(C.O.C.O.C_df3_model, data = C.O.C.O.C_XX3, y = C.O.C.O.C_train_3$C.O.C.O.C)
C.O.C.O.C_imp.1 <- FeatureImp$new(C.O.C.O.C.predictor1, loss = "rmse", n.repetitions = 1000)
C.O.C.O.C_imp.2 <- FeatureImp$new(C.O.C.O.C.predictor2, loss = "rmse", n.repetitions = 1000)
C.O.C.O.C_imp.3 <- FeatureImp$new(C.O.C.O.C.predictor3, loss = "rmse", n.repetitions = 1000)
# Plot feature importance for C.O.C.O.C
C.O.C.O.C.p1 <- plot(C.O.C.O.C_imp.1) + ggtitle("C.O.C.O.C Exp 1")
C.O.C.O.C.p2 <- plot(C.O.C.O.C_imp.2) + ggtitle("C.O.C.O.C Exp 2")
C.O.C.O.C.p3 <- plot(C.O.C.O.C_imp.3) + ggtitle("C.O.C.O.C Exp 3")
gridExtra::grid.arrange(C.O.C.O.C.p1, C.O.C.O.C.p2, C.O.C.O.C.p3, nrow = 1)

# Feature importance for OMcat.C.O.C across experiments
OMcat.C.O.C.predictor1 <- Predictor$new(OMcat.C.O.C_df1_model, data = OMcat.C.O.C_XX1, y = OMcat.C.O.C_train_1$OMcat.C.O.C)
OMcat.C.O.C.predictor2 <- Predictor$new(OMcat.C.O.C_df2_model, data = OMcat.C.O.C_XX2, y = OMcat.C.O.C_train_2$OMcat.C.O.C)
OMcat.C.O.C.predictor3 <- Predictor$new(OMcat.C.O.C_df3_model, data = OMcat.C.O.C_XX3, y = OMcat.C.O.C_train_3$OMcat.C.O.C)
OMcat.C.O.C_imp.1 <- FeatureImp$new(OMcat.C.O.C.predictor1, loss = "rmse", n.repetitions = 1000)
OMcat.C.O.C_imp.2 <- FeatureImp$new(OMcat.C.O.C.predictor2, loss = "rmse", n.repetitions = 1000)
OMcat.C.O.C_imp.3 <- FeatureImp$new(OMcat.C.O.C.predictor3, loss = "rmse", n.repetitions = 1000)
# Plot feature importance for OMcat.C.O.C
OMcat.C.O.C.p1 <- plot(OMcat.C.O.C_imp.1) + ggtitle("OMcat.C.O.C Exp 1")
OMcat.C.O.C.p2 <- plot(OMcat.C.O.C_imp.2) + ggtitle("OMcat.C.O.C Exp 2")
OMcat.C.O.C.p3 <- plot(OMcat.C.O.C_imp.3) + ggtitle("OMcat.C.O.C Exp 3")
gridExtra::grid.arrange(OMcat.C.O.C.p1, OMcat.C.O.C.p2, OMcat.C.O.C.p3, nrow = 1)

# ------------------- Accumulated Local Effects (ALE) Plots -------------------
# Generate ALE plots to visualize the effect of predictors on each response variable
# Experiment 1
CH.C.O_ale1 <- iml::FeatureEffects$new(CH.C.O.predictor1)  # ALE for CH.C.O, Exp 1
plot(CH.C.O_ale1)  # Display ALE plot
CH.C.O.C_ale1 <- iml::FeatureEffects$new(CH.C.O.C.predictor1)  # ALE for CH.C.O.C, Exp 1
plot(CH.C.O.C_ale1)
C.O.C.O.C_ale1 <- iml::FeatureEffects$new(C.O.C.O.C.predictor1)  # ALE for C.O.C.O.C, Exp 1
plot(C.O.C.O.C_ale1)
OMcat.C.O.C_ale1 <- iml::FeatureEffects$new(OMcat.C.O.C.predictor1)  # ALE for OMcat.C.O.C, Exp 1
plot(OMcat.C.O.C_ale1)

# Experiment 2
CH.C.O_ale2 <- iml::FeatureEffects$new(CH.C.O.predictor2)  # ALE for CH.C.O, Exp 2
plot(CH.C.O_ale2)
CH.C.O.C_ale2 <- iml::FeatureEffects$new(CH.C.O.C.predictor2)  # ALE for CH.C.O.C, Exp 2
plot(CH.C.O.C_ale2)
C.O.C.O.C_ale2 <- iml::FeatureEffects$new(C.O.C.O.C.predictor2)  # ALE for C.O.C.O.C, Exp 2
plot(C.O.C.O.C_ale2)
OMcat.C.O.C_ale2 <- iml::FeatureEffects$new(OMcat.C.O.C.predictor2)  # ALE for OMcat.C.O.C, Exp 2
plot(OMcat.C.O.C_ale2)

# Experiment 3
CH.C.O_ale3 <- iml::FeatureEffects$new(CH.C.O.predictor3)  # ALE for CH.C.O, Exp 3
plot(CH.C.O_ale3)
CH.C.O.C_ale3 <- iml::FeatureEffects$new(CH.C.O.C.predictor3)  # ALE for CH.C.O.C, Exp 3
plot(CH.C.O.C_ale3)
C.O.C.O.C_ale3 <- iml::FeatureEffects$new(C.O.C.O.C.predictor3)  # ALE for C.O.C.O.C, Exp 3
plot(C.O.C.O.C_ale3)
OMcat.C.O.C_ale3 <- iml::FeatureEffects$new(OMcat.C.O.C.predictor3)  # ALE for OMcat.C.O.C, Exp 3
plot(OMcat.C.O.C_ale3)
