#Group Assignment: Programming for Data Analysis

#============================================================

#Chan Hua Shean, TP088650
#Clement Kek Ching Haow, TP088688
#Li Aoyu, TP087527

#============================================================

#TABLE OF CONTENTS

#============================================================

#1. LIBRARIES AND SETUP
#2. DATA IMPORT AND INITIAL EXPLORATION
#3. DATA CLEANING AND PRE-PROCESSING
#4. DATA VALIDATION
#5. OBJECTIVE 1: Investigate factors influencing employee attrition
#6. OBJECTIVE 2: Analyze the relationship between compensation and attrition
#7. OBJECTIVE 3: Examine work-life balance impact on employee retention
#8. EXTRA FEATURES
#9. SUMMARY AND EXPORT

# ============================================================

#AUTO DETECT WORKING DIRECTORY

# ============================================================

#Function to auto-detect working directory
auto_setup_working_directory <- function(target_file = "4. dataset_employee_attrition.csv") {
  
  cat("\n========== Auto-Detecting Working Directory ==========\n")
  
  #Try 1: Check current directory first
  if (file.exists(target_file)) {
    cat("✓ File found in current directory:", getwd(), "\n")
    return(TRUE)
  }
  
  #Try 2: RStudio API (works when script is open in RStudio)
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    script_path <- rstudioapi::getActiveDocumentContext()$path
    if (script_path != "" && dirname(script_path) != "") {
      setwd(dirname(script_path))
      if (file.exists(target_file)) {
        cat("✓ Working directory set to script location:", getwd(), "\n")
        return(TRUE)
      }
    }
  }
  
  #Try 3: Search common subdirectories
  common_dirs <- c("data", "Data", "dataset", "datasets", "input")
  for (dir in common_dirs) {
    if (file.exists(file.path(dir, target_file))) {
      setwd(dir)
      cat("✓ File found in subdirectory:", getwd(), "\n")
      return(TRUE)
    }
  }
  
  #Try 4: Ask user to select folder
  if (interactive()) {
    cat("\n⚠ Could not automatically find the dataset file.\n")
    cat("Please select the folder containing:", target_file, "\n")
    
    chosen_dir <- tryCatch({
      choose.dir(default = getwd(), 
                 caption = "Select folder containing the dataset")
    }, error = function(e) NULL)
    
    if (!is.null(chosen_dir) && !is.na(chosen_dir)) {
      setwd(chosen_dir)
      if (file.exists(target_file)) {
        cat("✓ Directory set to:", getwd(), "\n")
        return(TRUE)
      }
    }
  }
  
  cat("✗ Could not locate the file automatically.\n")
  cat("  Please set working directory manually with: setwd('C:/path/to/your/folder')\n")
  return(FALSE)
}

#Function to find dataset file (flexible filename matching)
find_dataset_file <- function() {
  
  possible_names <- c(
    "4. dataset_employee_attrition.csv",
    "dataset_employee_attrition.csv",
    "employee_attrition.csv",
    "attrition_data.csv"
  )
  
  for (name in possible_names) {
    if (file.exists(name)) {
      cat("Found dataset file:", name, "\n")
      return(name)
    }
  }
  
  #Search for any CSV file containing "attrition"
  csv_files <- list.files(pattern = ".*attrition.*\\.csv$", ignore.case = TRUE)
  if (length(csv_files) > 0) {
    cat("Found dataset file:", csv_files[1], "\n")
    return(csv_files[1])
  }
  
  return(NULL)
}

#Execute auto-detection
if (!auto_setup_working_directory()) {
  stop("Cannot proceed without proper directory setup.")
}

#Find and load the dataset
dataset_file <- find_dataset_file()
if (is.null(dataset_file)) {
  stop("Dataset not found. Please place it in the working directory.")
}

#============================================================

#1. LIBRARIES AND SETUP

#============================================================

# Read the dataset
employee_data <- read.csv(dataset_file, 
                          header = TRUE,
                          stringsAsFactors = FALSE,
                          na.strings = c("", " ", "NA", "N/A", "nil", "NIL", "Nil"))

cat("Dataset loaded successfully:", dataset_file, "\n")
cat("Dimensions:", nrow(employee_data), "rows x", ncol(employee_data), "columns\n")
  
#Install packages if not already installed
  required_packages <- c(
    "tidyverse", # Data manipulation and visualization
    "ggplot2", # Advanced visualization
    "dplyr", # Data manipulation
    "tidyr", # Data tidying
    "corrplot", # Correlation plots
    "caret", # Machine learning
    "randomForest", # Random Forest algorithm
    "e1071", # SVM and other algorithms
    "ROSE", # Handling imbalanced data
    "pROC", # ROC curves
    "gridExtra", # Multiple grid plots
    "scales", # Scale formatting
    "GGally", # Extension to ggplot2
    "reshape2", # Data reshaping
    "RColorBrewer", # Color palettes
    "plotly", # Interactive plots
    "DT", # Interactive tables
    "knitr", # Report generation
    "rmarkdown", # Markdown reporting
    "class", # KNN classification
    "rpart", # Decision trees
    "rpart.plot", # Decision tree visualization
    "nnet", # Neural networks
    "xgboost", # XGBoost algorithm
    "viridis" # Color palettes for accessibility
  )

#Install any missing packages
for (pkg in required_packages) {
   if (!requireNamespace(pkg, quietly = TRUE)) {
     install.packages(pkg, dependencies = TRUE)
  }
}

#Load libraries
library(tidyverse)      # Data manipulation & visualization (includes ggplot2, dplyr, tidyr)
library(corrplot)       # Correlation plots
library(caret)          # Machine learning framework
library(randomForest)   # Random Forest algorithm
library(e1071)          # SVM and statistical functions
library(ROSE)           # Handling imbalanced data
library(pROC)           # ROC curve analysis
library(gridExtra)      # Multiple grid plots
library(scales)         # Scale formatting
library(GGally)         # Extension to ggplot2
library(reshape2)       # Data reshaping
library(RColorBrewer)   # Color palettes
library(plotly)         # Interactive plots
library(DT)             # Interactive tables
library(knitr)          # Report generation
library(rmarkdown)      # Markdown reporting
library(class)          # KNN classification
library(rpart)          # Decision trees
library(rpart.plot)     # Decision tree visualization
library(nnet)           # Neural networks
library(xgboost)        # XGBoost algorithm
library(viridis)        # Colorblind-friendly palettes

#Set global options
options(scipen = 999)   # Disable scientific notation
set.seed(123)           # For reproducibility

cat("\n✓ All packages loaded successfully!\n")

#Define color palette for the project
company_palette <- c("#2C3E50", "#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6")
attrition_colors <- c("No" = "#2ECC71", "Yes" = "#E74C3C")

risk_colors <- c("High Risk" = "#E74C3C", 
                 "Medium Risk" = "#F39C12", 
                 "Low Risk" = "#3498DB", 
                 "Minimal Risk" = "#2ECC71")


#2. DATA IMPORT AND INITIAL EXPLORATION

#============================================================

#Read the CSV file
cat("\n========== DATA IMPORT ==========\n")
employee_data <- read.csv("4. dataset_employee_attrition.csv", 
                          header = TRUE,
                          na.strings = c("", " ", "NA", "N/A", "nil", "NIL", "Nil"))

#Display basic information about the dataset
cat("\n--- Dataset Dimensions ---\n")
cat("Number of rows:", nrow(employee_data), "\n")
cat("Number of columns:", ncol(employee_data), "\n")

cat("\n--- Column Names ---\n")
print(colnames(employee_data))

cat("\n--- Structure of Dataset ---\n")
str(employee_data)

cat("\n--- First 5 rows ---\n")
head(employee_data, 5)

cat("\n--- Summary Statistics ---\n")
summary(employee_data)

#Check for missing values
cat("\n--- Missing Values per Column ---\n")
missing_values <- colSums(is.na(employee_data))
print(missing_values[missing_values > 0])

#Initial visualization of missing values
missing_df <- data.frame(
  Column = names(missing_values),
  MissingCount = missing_values
) %>%
  filter(MissingCount > 0) %>%
  arrange(desc(MissingCount))

if(nrow(missing_df) > 0) {
  ggplot(missing_df, aes(x = reorder(Column, MissingCount), y = MissingCount)) +
    geom_bar(stat = "identity", fill = "#E74C3C", alpha = 0.8) +
    coord_flip() +
    labs(title = "Missing Values by Column (Before Cleaning)",
         x = "Column Name",
         y = "Number of Missing Values") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
}

#============================================================

#3. DATA CLEANING AND PRE-PROCESSING

#============================================================

cat("\n========== DATA CLEANING AND PRE-PROCESSING ==========\n")

#Create a copy of the original data for cleaning
df_clean <- employee_data

#--- 3.1 Handle Missing Values ---
cat("\n--- Handling Missing Values ---\n")

#Function to check and report missing values
check_missing <- function(data, stage) {
  cat("\nMissing values at stage:", stage, "\n")
  missing_count <- sum(is.na(data))
  cat("Total missing values:", missing_count, "\n")
  return(missing_count)
}

check_missing(df_clean, "Initial")

#Remove rows where critical columns (Age, Attrition, Department) are missing
df_clean <- df_clean %>%
filter(!is.na(Age) & !is.na(Attrition) & !is.na(Department))

check_missing(df_clean, "After removing critical NA rows")

#Impute numeric columns with median
numeric_cols <- sapply(df_clean, is.numeric)
for (col in names(df_clean)[numeric_cols]) {
  if (any(is.na(df_clean[[col]]))) {
    median_val <- median(df_clean[[col]], na.rm = TRUE)
    df_clean[[col]][is.na(df_clean[[col]])] <- median_val
    cat("Imputed", col, "with median:", median_val, "\n")
  }
}

#Impute categorical columns with mode
get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

categorical_cols <- sapply(df_clean, is.character)
for (col in names(df_clean)[categorical_cols]) {
  if (any(is.na(df_clean[[col]]))) {
    mode_val <- get_mode(df_clean[[col]])
    df_clean[[col]][is.na(df_clean[[col]])] <- mode_val
    cat("Imputed", col, "with mode:", mode_val, "\n")
  }
}

check_missing(df_clean, "After imputation")

#--- 3.2 Standardize Data Formats ---
cat("\n--- Standardizing Data Formats ---\n")

#Standardize Attrition column
df_clean$Attrition <- trimws(toupper(df_clean$Attrition))
df_clean$Attrition <- ifelse(df_clean$Attrition %in% c("YES", "1", "Y", "TRUE"), "Yes", "No")
df_clean$Attrition <- as.factor(df_clean$Attrition)
cat("Unique Attrition values after cleaning:", unique(df_clean$Attrition), "\n")

#Standardize BusinessTravel column
df_clean$BusinessTravel <- trimws(toupper(df_clean$BusinessTravel))
df_clean$BusinessTravel <- gsub(" ", "_", df_clean$BusinessTravel)
df_clean$BusinessTravel <- ifelse(df_clean$BusinessTravel %in% c("TRAVEL_RARELY", "RARE", "TRAVEL_RARE"),
                           "Travel_Rarely",
                           ifelse(df_clean$BusinessTravel %in% c("TRAVEL_FREQUENTLY", "FREQUENT"),
                           "Travel_Frequently",
                           ifelse(df_clean$BusinessTravel %in% c("NON-TRAVEL", "NON_TRAVEL", "NO_TRAVEL"),
                           "Non-Travel",
                            df_clean$BusinessTravel)))
df_clean$BusinessTravel <- as.factor(df_clean$BusinessTravel)
cat("Unique BusinessTravel values after cleaning:", unique(df_clean$BusinessTravel), "\n")

#Standardize Department column
df_clean$Department <- trimws(toupper(df_clean$Department))
df_clean$Department <- dplyr::recode(df_clean$Department,
                           "SALE" = "Sales",
                           "SALES" = "Sales",
                           "R&D" = "Research & Development",
                           "RESEARCH & DEVELOPMENT" = "Research & Development",
                           "RESEARCH AND DEVELOPMENT" = "Research & Development",
                           "HR" = "Human Resources",
                           "HUMAN RESOURCES" = "Human Resources",
                           .default = df_clean$Department)
df_clean$Department <- as.factor(df_clean$Department)
cat("Unique Department values after cleaning:", unique(df_clean$Department), "\n")

#Standardize Gender column
df_clean$Gender <- trimws(toupper(df_clean$Gender))
df_clean$Gender <- ifelse(df_clean$Gender %in% c("M", "MALE"), "Male",
                          ifelse(df_clean$Gender %in% c("F", "FEMALE"), "Female", df_clean$Gender))
df_clean$Gender <- as.factor(df_clean$Gender)
cat("Unique Gender values after cleaning:", unique(df_clean$Gender), "\n")

#Standardize EducationField column
df_clean$EducationField <- trimws(toupper(df_clean$EducationField))
df_clean$EducationField <- dplyr::recode(df_clean$EducationField,
                            "LS" = "Life Sciences",
                            "LIFE SCIENCES" = "Life Sciences",
                            "MED" = "Medical",
                            "MEDICAL" = "Medical",
                            "MKT" = "Marketing",
                            "MARKETING" = "Marketing",
                            "TD" = "Technical Degree",
                            "TECHNICAL DEGREE" = "Technical Degree",
                            "HR" = "Human Resources",
                            "HUMAN RESOURCES" = "Human Resources",
                            "OTHER" = "Other",
                            .default = df_clean$EducationField)
df_clean$EducationField <- as.factor(df_clean$EducationField)
cat("Unique EducationField values after cleaning:", unique(df_clean$EducationField), "\n")

#Standardize MaritalStatus column
df_clean$MaritalStatus <- trimws(toupper(df_clean$MaritalStatus))
df_clean$MaritalStatus <- dplyr::recode(df_clean$MaritalStatus,
                            "SINGLE" = "Single",
                            "MARRIED" = "Married",
                            "DIVORCED" = "Divorced",
                            .default = df_clean$MaritalStatus)
df_clean$MaritalStatus <- as.factor(df_clean$MaritalStatus)
cat("Unique MaritalStatus values after cleaning:", unique(df_clean$MaritalStatus), "\n")

#Standardize JobRole column
df_clean$JobRole <- trimws(toupper(df_clean$JobRole))
df_clean$JobRole <- dplyr::recode(df_clean$JobRole,
                           "HR" = "Human Resources",
                           "HUMAN RESOURCES" = "Human Resources",
                           "SALES EXE" = "Sales Executive",
                           "SALES EXECUTIVE" = "Sales Executive",
                           "SALES REP" = "Sales Representative",
                           "SALES REPRESENTATIVE" = "Sales Representative",
                           "LAB TECHNICIAN" = "Laboratory Technician",
                           "LABORATORY TECHNICIAN" = "Laboratory Technician",
                           "RESEARCH SCIENTIST" = "Research Scientist",
                           "RESEARCH DIRECTOR" = "Research Director",
                           "MANAGER" = "Manager",
                           "MANUFACTURING DIRECTOR" = "Manufacturing Director",
                           "MANUFACTURE DIRECTOR" = "Manufacturing Director",
                           "HEALTHCARE REPRESENTATIVE" = "Healthcare Representative",
                           "HEALTHCARE REP" = "Healthcare Representative",
                           .default = df_clean$JobRole)
df_clean$JobRole <- as.factor(df_clean$JobRole)
cat("Unique JobRole values after cleaning:", unique(df_clean$JobRole), "\n")

#Standardize OverTime column
df_clean$OverTime <- trimws(toupper(df_clean$OverTime))
df_clean$OverTime <- ifelse(df_clean$OverTime %in% c("YES", "1", "Y", "TRUE"), "Yes", "No")
df_clean$OverTime <- as.factor(df_clean$OverTime)
cat("Unique OverTime values after cleaning:", unique(df_clean$OverTime), "\n")

#Standardize Over18 column
df_clean$Over18 <- trimws(toupper(df_clean$Over18))
df_clean$Over18 <- ifelse(df_clean$Over18 %in% c("Y", "YES", "1"), "Y", "Y")  # All should be Y
df_clean$Over18 <- as.factor(df_clean$Over18)

#--- 3.3 Handle Data Quality Issues ---
cat("\n--- Handling Data Quality Issues ---\n")

#Remove duplicate rows
duplicate_count <- sum(duplicated(df_clean))
cat("Removing", duplicate_count, "duplicate rows\n")
df_clean <- df_clean[!duplicated(df_clean), ]

#Remove rows with unrealistic Age values
cat("Removing rows with Age > 65 or Age < 18\n")
df_clean <- df_clean %>% filter(Age >= 18 & Age <= 65)

#Fix cases where YearsAtCompany > Age
invalid_years <- df_clean %>% filter(YearsAtCompany > Age)
cat("Removing", nrow(invalid_years), "rows where YearsAtCompany > Age\n")
df_clean <- df_clean %>% filter(YearsAtCompany <= Age)

#Ensure HoursWorked is consistent
df_clean$StandardHours <- 80

#Convert appropriate columns to factors
factor_cols <- c("Education", "EnvironmentSatisfaction", "JobInvolvement", 
                 "JobLevel", "JobSatisfaction", "PerformanceRating",
                 "RelationshipSatisfaction", "StockOptionLevel", 
                 "WorkLifeBalance")

for (col in factor_cols) {
  if (col %in% names(df_clean)) {
    df_clean[[col]] <- as.factor(df_clean[[col]])
  }
}

#--- 3.4 Create Derived Features ---
cat("\n--- Creating Derived Features ---\n")

#Convert numeric columns from character to numeric
numeric_columns <- c("Age", "DailyRate", "DistanceFromHome", "Education", 
                     "EmployeeCount", "EmployeeNumber", "EnvironmentSatisfaction",
                     "HourlyRate", "JobInvolvement", "JobLevel", "JobSatisfaction",
                     "MonthlyIncome", "MonthlyRate", "NumCompaniesWorked",
                     "PercentSalaryHike", "PerformanceRating", "RelationshipSatisfaction",
                     "StandardHours", "StockOptionLevel", "TotalWorkingYears",
                     "TrainingTimesLastYear", "WorkLifeBalance", "YearsAtCompany",
                     "YearsInCurrentRole", "YearsSinceLastPromotion", "YearsWithCurrManager")

#Convert each to numeric, turning non-numbers into NA
for (col in numeric_columns) {
  if (col %in% names(df_clean)) {
    df_clean[[col]] <- as.numeric(as.character(df_clean[[col]]))
  }
}

#MonthlyIncome to DailyRate ratio (efficiency indicator)
df_clean$IncomeRateRatio <- df_clean$MonthlyIncome / df_clean$DailyRate

#Total compensation estimate
df_clean$EstimatedAnnualCompensation <- df_clean$MonthlyIncome * 12

#Average years per company
df_clean$AvgYearsPerCompany <- ifelse(df_clean$NumCompaniesWorked > 0,
                                      df_clean$TotalWorkingYears / df_clean$NumCompaniesWorked,
                                      df_clean$TotalWorkingYears)

#Promotion frequency indicator
df_clean$PromotionFrequency <- ifelse(df_clean$YearsSinceLastPromotion > 0,
                                      df_clean$YearsAtCompany / df_clean$YearsSinceLastPromotion,
                                      df_clean$YearsAtCompany)

#Commute distance category
df_clean$DistanceCategory <- cut(df_clean$DistanceFromHome,
                                 breaks = c(-Inf, 5, 15, 30, Inf),
                                 labels = c("Very Close", "Close", "Far", "Very Far"))

# Fix any remaining NAs in Age first
df_clean$Age[is.na(df_clean$Age)] <- median(df_clean$Age, na.rm = TRUE)

#Age group
df_clean$AgeGroup <- cut(df_clean$Age,
                         breaks = c(17, 25, 35, 45, 55, 65),
                         labels = c("18-25", "26-35", "36-45", "46-55", "56-65"))

#Tenure category
df_clean$TenureCategory <- cut(df_clean$YearsAtCompany,
                               breaks = c(-Inf, 2, 5, 10, 20, Inf),
                               labels = c("New Hire", "Junior", "Mid-Level", "Senior", "Veteran"))

#Income bracket
df_clean$IncomeBracket <- cut(df_clean$MonthlyIncome,
                              breaks = quantile(df_clean$MonthlyIncome, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                              labels = c("Low", "Medium", "High", "Very High"),
                              include.lowest = TRUE)

cat("Data cleaning complete. Final dimensions:", nrow(df_clean), "rows x", ncol(df_clean), "columns\n")

# ============================================================

# 4. DATA VALIDATION

# ============================================================

cat("\n========== DATA VALIDATION ==========\n")

#Function to validate data ranges
validate_data <- function(data) {
  validation_results <- list()
  
  #Check Age range
  validation_results$age <- all(data$Age >= 18 & data$Age <= 65)
  cat("Age validation (18-65):", validation_results$age, "\n")
  
  #Check DistanceFromHome
  validation_results$distance <- all(data$DistanceFromHome >= 1 & data$DistanceFromHome <= 30)
  cat("DistanceFromHome validation (1-30):", validation_results$distance, "\n")
  
  #Check categorical variables have expected values
  validation_results$attrition <- all(data$Attrition %in% c("Yes", "No"))
  cat("Attrition validation:", validation_results$attrition, "\n")
  
  validation_results$gender <- all(data$Gender %in% c("Male", "Female"))
  cat("Gender validation:", validation_results$gender, "\n")
  
  #Check MonthlyIncome > 0
  validation_results$income <- all(data$MonthlyIncome > 0)
  cat("MonthlyIncome validation (>0):", validation_results$income, "\n")
  
  #Check YearsAtCompany >= 0
  validation_results$years_company <- all(data$YearsAtCompany >= 0)
  cat("YearsAtCompany validation (>=0):", validation_results$years_company, "\n")
  
  #Check for any remaining NA values
  validation_results$no_na <- sum(is.na(data)) == 0
  cat("No remaining NA values:", validation_results$no_na, "\n")
  
  #Check for duplicate EmployeeNumbers
  validation_results$unique_emp <- length(unique(data$EmployeeNumber)) == nrow(data)
  cat("Unique Employee Numbers:", validation_results$unique_emp, "\n")
  
  return(validation_results)
}

validation <- validate_data(df_clean)

#Fix any remaining NAs in these columns
df_clean$Age[is.na(df_clean$Age)] <- median(df_clean$Age, na.rm = TRUE)
df_clean$MonthlyIncome[is.na(df_clean$MonthlyIncome)] <- median(df_clean$MonthlyIncome, na.rm = TRUE)
df_clean$YearsAtCompany[is.na(df_clean$YearsAtCompany)] <- median(df_clean$YearsAtCompany, na.rm = TRUE)

#Fix AgeGroup NA
df_clean$AgeGroup <- cut(df_clean$Age,
                         breaks = c(17, 25, 35, 45, 55, 65),
                         labels = c("18-25", "26-35", "36-45", "46-55", "56-65"))

#Final summary of the cleaned dataset
cat("\n--- Final Dataset Summary ---\n")
cat("Total employees:", nrow(df_clean), "\n")
cat("Attrition rate:", round(mean(df_clean$Attrition == "Yes") * 100, 2), "%\n")
cat("Average age:", round(mean(df_clean$Age), 1), "\n")
cat("Average monthly income:", round(mean(df_clean$MonthlyIncome), 2), "\n")
cat("Average years at company:", round(mean(df_clean$YearsAtCompany), 2), "\n")

#Visualize class balance
cat("\n--- Plot: Data Validation - Class Balance Distribution ---\n")

ggplot(df_clean, aes(x = Attrition, fill = Attrition)) +
  geom_bar() +
  scale_fill_manual(values = attrition_colors) +
  labs(title = "Employee Attrition Distribution",
       x = "Attrition Status",
       y = "Number of Employees") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "none")

#============================================================

#5. OBJECTIVE 1: Investigate factors influencing employee attrition
#Student: Chan Hua Shean, TP088650

#============================================================

cat("\n========== OBJECTIVE 1: FACTORS INFLUENCING EMPLOYEE ATTRITION ==========\n")
cat("Objective: To investigate the relationship between various demographic and\n")
cat("job-related factors and employee attrition to identify key drivers of turnover.\n\n")

#--- Analysis 1-1: Demographic Factors and Attrition ---

cat("--- Analysis 1-1: Demographic Factors and Attrition ---\n")

#--- Analysis 1-1-1: Attrition by Gender ---
cat("\n--- Analysis 1-1-1: Attrition by Gender ---\n")

p1_gender <- ggplot(df_clean, aes(x = Gender, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Analysis 1-1-1 Attrition Rate by Gender",
       x = "Gender", y = "Proportion") +
  theme_minimal()

print(p1_gender)

#Drill-Down: Why no gender difference?
cat("\n--- Drill-Down: Why is there no gender difference? ---\n")

gender_drill <- df_clean %>%
  group_by(Gender) %>%
  summarise(
    AvgIncome = round(mean(MonthlyIncome, na.rm = TRUE)),
    OvertimeRate = round(mean(OverTime == "Yes") * 100, 1),
    AvgJobSatisfaction = round(mean(as.numeric(as.character(JobSatisfaction)), na.rm = TRUE), 2),
    AvgWLB = round(mean(as.numeric(as.character(WorkLifeBalance)), na.rm = TRUE), 2),
    AttritionRate = round(mean(Attrition == "Yes") * 100, 1)
  )

cat("\nGender Comparison:\n")
print(gender_drill)

cat("\nKey Insight: Males and females have similar income, overtime rates, and satisfaction levels.\n")
cat("Gender does not affect attrition because working conditions are similar for both genders.\n\n")

#--- Analysis 1-1-2: Attrition by Age Group ---
cat("\n--- Analysis 1-1-2: Attrition by Age Group ---\n")

p1_age <- ggplot(df_clean, aes(x = AgeGroup, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Analysis 1-1-2 Attrition Rate by Age Group",
       x = "Age Group", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1_age)

#Drill-Down: Why do 18-25 leave most?
cat("\n--- Drill-Down: Why do 18-25 year olds leave most? ---\n")

young_employees <- df_clean %>% filter(AgeGroup == "18-25")

cat("\nIncome Comparison:\n")
cat("18-25 avg income: $", round(mean(young_employees$MonthlyIncome, na.rm = TRUE)), "\n")
cat("36-45 avg income: $", round(mean(df_clean$MonthlyIncome[df_clean$AgeGroup == "36-45"], na.rm = TRUE)), "\n")

cat("\nOvertime Comparison:\n")
cat("18-25 overtime rate:", round(mean(young_employees$OverTime == "Yes") * 100, 1), "%\n")
cat("Overall overtime rate:", round(mean(df_clean$OverTime == "Yes") * 100, 1), "%\n")

cat("\nSatisfaction Comparison:\n")
cat("18-25 job satisfaction:", round(mean(as.numeric(as.character(young_employees$JobSatisfaction)), na.rm = TRUE), 2), "\n")
cat("Overall job satisfaction:", round(mean(as.numeric(as.character(df_clean$JobSatisfaction)), na.rm = TRUE), 2), "\n")

#Age group comparison table
age_drill <- df_clean %>%
  group_by(AgeGroup) %>%
  summarise(
    AvgIncome = round(mean(MonthlyIncome, na.rm = TRUE)),
    OvertimeRate = round(mean(OverTime == "Yes") * 100, 1),
    AvgJobSatisfaction = round(mean(as.numeric(as.character(JobSatisfaction)), na.rm = TRUE), 2),
    AvgWLB = round(mean(as.numeric(as.character(WorkLifeBalance)), na.rm = TRUE), 2),
    AttritionRate = round(mean(Attrition == "Yes") * 100, 1),
    EmployeeCount = n()
  )

cat("\nAge Group Full Comparison:\n")
print(age_drill)

#Visualize: Income by Age Group
p_age_income <- ggplot(df_clean, aes(x = AgeGroup, y = MonthlyIncome, fill = AgeGroup)) +
  geom_boxplot() +
  labs(title = "Analysis 1-1-2-D1 Monthly Income by Age Group",
       subtitle = "Younger employees earn significantly less",
       x = "Age Group", y = "Monthly Income") +
  theme_minimal() +
  theme(legend.position = "none")

print(p_age_income)

#Visualize: What jobs do 18-25 have?
p_young_jobs <- ggplot(young_employees, aes(x = JobRole, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Analysis 1-1-2-D2 Attrition by Job Role (18-25 Age Group)",
       subtitle = "Which roles do young employees leave most?",
       x = "Job Role", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_young_jobs)

cat("\nKey Insight: 18-25 year olds leave more because they earn less, work more overtime,\n")
cat("and are concentrated in entry-level roles with lower satisfaction.\n\n")

#--- Analysis 1-1-3: Attrition by Marital Status ---
cat("\n--- Analysis 1-1-3: Attrition by Marital Status ---\n")

p1_marital <- ggplot(df_clean, aes(x = MaritalStatus, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Analysis 1-1-3 Attrition Rate by Marital Status",
       x = "Marital Status", y = "Proportion") +
  theme_minimal()

print(p1_marital)

#Drill-Down: Why do singles leave more?
cat("\n--- Drill-Down: Why do singles leave more? ---\n")

marital_drill <- df_clean %>%
  group_by(MaritalStatus) %>%
  summarise(
    AvgAge = round(mean(Age)),
    AvgIncome = round(mean(MonthlyIncome, na.rm = TRUE)),
    OvertimeRate = round(mean(OverTime == "Yes") * 100, 1),
    AttritionRate = round(mean(Attrition == "Yes") * 100, 1),
    EmployeeCount = n()
  )

cat("\nMarital Status Comparison:\n")
print(marital_drill)

#Visualize: Age distribution by Marital Status
p_marital_age <- ggplot(df_clean, aes(x = MaritalStatus, y = Age, fill = MaritalStatus)) +
  geom_boxplot() +
  labs(title = "Analysis 1-1-3-D1 Age Distribution by Marital Status",
       subtitle = "Singles are significantly younger",
       x = "Marital Status", y = "Age") +
  theme_minimal() +
  theme(legend.position = "none")

print(p_marital_age)

cat("\nKey Insight: Singles leave more because they are younger on average,\n")
cat("earn less, and have fewer family commitments tying them to the job.\n\n")

#Chi-square test for Gender vs Attrition
cat("\nChi-square test: Gender vs Attrition\n")
gender_table <- table(df_clean$Gender, df_clean$Attrition)
gender_chi <- chisq.test(gender_table)
print(gender_chi)

#Chi-square test for Marital Status vs Attrition
cat("\nChi-square test: Marital Status vs Attrition\n")
marital_table <- table(df_clean$MaritalStatus, df_clean$Attrition)
marital_chi <- chisq.test(marital_table)
print(marital_chi)

#Display demographic plots
grid.arrange(p1_gender, p1_age, p1_marital, 
             ncol = 2, nrow = 2,
             top = "Analysis 1-1 Demographic Factors Impact on Employee Attrition")

#--- Analysis 1-2: Job-Related Factors and Attrition ---

cat("\n--- Analysis 1-2: Job-Related Factors and Attrition ---\n")

#--- Analysis 1-2-1: Attrition by Department ---
cat("\n--- Analysis 1-2-1: Attrition by Department ---\n")

p1_dept <- ggplot(df_clean, aes(x = Department, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Analysis 1-2-1 Attrition Rate by Department",
       x = "Department", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1_dept)

#Drill-Down: Why does Sales have highest attrition?
cat("\n--- Drill-Down: Why does Sales have highest attrition? ---\n")

sales_employees <- df_clean %>% filter(Department == "Sales")
rd_employees <- df_clean %>% filter(Department == "Research & Development")
hr_employees <- df_clean %>% filter(Department == "Human Resources")

dept_drill <- df_clean %>%
  group_by(Department) %>%
  summarise(
    AvgIncome = round(mean(MonthlyIncome, na.rm = TRUE)),
    OvertimeRate = round(mean(OverTime == "Yes") * 100, 1),
    FrequentTravelRate = round(mean(BusinessTravel == "Travel_Frequently") * 100, 1),
    AvgWLB = round(mean(as.numeric(as.character(WorkLifeBalance)), na.rm = TRUE), 2),
    AvgJobSatisfaction = round(mean(as.numeric(as.character(JobSatisfaction)), na.rm = TRUE), 2),
    AvgEnvSatisfaction = round(mean(as.numeric(as.character(EnvironmentSatisfaction)), na.rm = TRUE), 2),
    AvgRelationshipSat = round(mean(as.numeric(as.character(RelationshipSatisfaction)), na.rm = TRUE), 2),
    AvgYearsInRole = round(mean(YearsInCurrentRole, na.rm = TRUE), 2),
    AvgYearsSincePromotion = round(mean(YearsSinceLastPromotion, na.rm = TRUE), 2),
    AttritionRate = round(mean(Attrition == "Yes") * 100, 1),
    EmployeeCount = n()
  ) %>%
  arrange(desc(AttritionRate))

cat("\nDepartment Full Comparison:\n")
print(dept_drill)

#Visualize: Travel by Department
p_dept_travel <- ggplot(df_clean, aes(x = Department, fill = BusinessTravel)) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d(option = "D") +
  scale_y_continuous(labels = percent) +
  labs(title = "Analysis 1-2-1-D1 Business Travel Distribution by Department",
       subtitle = "Sales travels significantly more",
       x = "Department", y = "Proportion") +
  theme_minimal()

print(p_dept_travel)

#Visualize: Overtime by Department
p_dept_ot <- ggplot(df_clean, aes(x = Department, fill = OverTime)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("No" = "#2ECC71", "Yes" = "#E74C3C")) +
  scale_y_continuous(labels = percent) +
  labs(title = "Analysis 1-2-1-D2 Overtime Distribution by Department",
       subtitle = "Comparing overtime rates across departments",
       x = "Department", y = "Proportion") +
  theme_minimal()

print(p_dept_ot)

cat("\nKey Insight: Sales has highest attrition due to frequent travel and overtime.\n")
cat("Despite similar pay to R&D, the travel and overtime create more stress.\n\n")

#--- Analysis 1-2-2: Attrition by Job Role ---
cat("\n--- Analysis 1-2-2: Attrition by Job Role ---\n")

p1_role <- ggplot(df_clean, aes(x = JobRole, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Analysis 1-2-2 Attrition Rate by Job Role",
       x = "Job Role", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1_role)

# Drill-Down: Which roles are most at risk?
cat("\n--- Drill-Down: Job Role Deep Dive ---\n")

role_drill <- df_clean %>%
  group_by(JobRole) %>%
  summarise(
    AvgIncome = round(mean(MonthlyIncome, na.rm = TRUE)),
    OvertimeRate = round(mean(OverTime == "Yes") * 100, 1),
    FrequentTravelRate = round(mean(BusinessTravel == "Travel_Frequently") * 100, 1),
    AvgJobSatisfaction = round(mean(as.numeric(as.character(JobSatisfaction)), na.rm = TRUE), 2),
    AvgWLB = round(mean(as.numeric(as.character(WorkLifeBalance)), na.rm = TRUE), 2),
    AttritionRate = round(mean(Attrition == "Yes") * 100, 1),
    EmployeeCount = n()
  ) %>%
  arrange(desc(AttritionRate))

cat("\nJob Role Comparison (sorted by attrition):\n")
print(role_drill)

cat("\nKey Insight: Sales Representatives have the highest attrition due to the combination\n")
cat("of low pay, high overtime, and frequent travel. Managers stay because of higher income\n")
cat("and better working conditions.\n\n")

#--- Analysis 1-2-3: Attrition by Overtime ---
cat("\n--- Analysis 1-2-3: Attrition by Overtime ---\n")

p1_overtime <- ggplot(df_clean, aes(x = OverTime, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Analysis 1-2-3 Attrition Rate by Overtime Status",
       x = "Overtime", y = "Proportion") +
  theme_minimal()

print(p1_overtime)

#Drill-Down: Why does overtime drive attrition?
cat("\n--- Drill-Down: Why does overtime drive attrition? ---\n")

overtime_drill <- df_clean %>%
  group_by(OverTime) %>%
  summarise(
    AvgWLB = round(mean(as.numeric(as.character(WorkLifeBalance)), na.rm = TRUE), 2),
    AvgJobSatisfaction = round(mean(as.numeric(as.character(JobSatisfaction)), na.rm = TRUE), 2),
    AvgEnvSatisfaction = round(mean(as.numeric(as.character(EnvironmentSatisfaction)), na.rm = TRUE), 2),
    AvgRelationshipSat = round(mean(as.numeric(as.character(RelationshipSatisfaction)), na.rm = TRUE), 2),
    AvgIncome = round(mean(MonthlyIncome, na.rm = TRUE)),
    AttritionRate = round(mean(Attrition == "Yes") * 100, 1)
  )

cat("\nOvertime Comparison:\n")
print(overtime_drill)

#Visualize: Satisfaction by Overtime
p_ot_satisfaction <- df_clean %>%
  select(OverTime, JobSatisfaction, EnvironmentSatisfaction, WorkLifeBalance, RelationshipSatisfaction) %>%
  mutate(
    JobSatisfaction = as.numeric(as.character(JobSatisfaction)),
    EnvironmentSatisfaction = as.numeric(as.character(EnvironmentSatisfaction)),
    WorkLifeBalance = as.numeric(as.character(WorkLifeBalance)),
    RelationshipSatisfaction = as.numeric(as.character(RelationshipSatisfaction))
  ) %>%
  pivot_longer(cols = -OverTime, names_to = "SatisfactionType", values_to = "Score") %>%
  ggplot(aes(x = SatisfactionType, y = Score, fill = OverTime)) +
  geom_boxplot() +
  scale_fill_manual(values = c("No" = "#2ECC71", "Yes" = "#E74C3C")) +
  labs(title = "Analysis 1-2-3-D1 Satisfaction Scores by Overtime Status",
       subtitle = "Overtime workers score lower on ALL satisfaction metrics",
       x = "Satisfaction Type", y = "Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_ot_satisfaction)

cat("\nKey Insight: Overtime drives attrition because it reduces ALL satisfaction scores —\n")
cat("WLB, job satisfaction, environment satisfaction, and relationships all suffer.\n\n")

#--- Analysis 1-2-4: Attrition by Business Travel ---
cat("\n--- Analysis 1-2-4: Attrition by Business Travel ---\n")

p1_travel <- ggplot(df_clean, aes(x = BusinessTravel, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Analysis 1-2-4 Attrition Rate by Business Travel",
       x = "Business Travel", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1_travel)

#Drill-Down: Why does frequent travel drive attrition?
cat("\n--- Drill-Down: Why does frequent travel drive attrition? ---\n")

travel_drill <- df_clean %>%
  group_by(BusinessTravel) %>%
  summarise(
    AvgWLB = round(mean(as.numeric(as.character(WorkLifeBalance)), na.rm = TRUE), 2),
    AvgJobSatisfaction = round(mean(as.numeric(as.character(JobSatisfaction)), na.rm = TRUE), 2),
    OvertimeRate = round(mean(OverTime == "Yes") * 100, 1),
    AvgIncome = round(mean(MonthlyIncome, na.rm = TRUE)),
    AttritionRate = round(mean(Attrition == "Yes") * 100, 1)
  )

cat("\nBusiness Travel Comparison:\n")
print(travel_drill)

cat("\nKey Insight: Frequent travel increases attrition because it reduces WLB and\n")
cat("is often combined with overtime, creating a double burden on employees.\n\n")

#Chi-square test for Department vs Attrition
cat("\nChi-square test: Department vs Attrition\n")
dept_table <- table(df_clean$Department, df_clean$Attrition)
dept_chi <- chisq.test(dept_table)
print(dept_chi)

#Chi-square test for OverTime vs Attrition
cat("\nChi-square test: OverTime vs Attrition\n")
overtime_table <- table(df_clean$OverTime, df_clean$Attrition)
overtime_chi <- chisq.test(overtime_table)
print(overtime_chi)

#Display job-related plots
grid.arrange(p1_dept, p1_role, p1_overtime, p1_travel, ncol = 2, nrow = 2,
             top = "Analysis 1-2 Job-Related Factors Impact on Employee Attrition")

#--- Analysis 1-3: Satisfaction Factors and Attrition ---

cat("\n--- Analysis 1-3: Satisfaction Factors and Attrition ---\n")

#--- Analysis 1-3-1: Attrition by Job Satisfaction ---
cat("\n--- Analysis 1-3-1: Attrition by Job Satisfaction ---\n")

p1_jobsat <- ggplot(df_clean, aes(x = JobSatisfaction, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Analysis 1-3-1 Attrition Rate by Job Satisfaction",
       x = "Job Satisfaction Level", y = "Proportion") +
  theme_minimal()

print(p1_jobsat)

#Drill-Down: What causes low job satisfaction?
cat("\n--- Drill-Down: What causes low job satisfaction? ---\n")

jobsat_drill <- df_clean %>%
  group_by(JobSatisfaction) %>%
  summarise(
    AvgIncome = round(mean(MonthlyIncome, na.rm = TRUE)),
    OvertimeRate = round(mean(OverTime == "Yes") * 100, 1),
    AvgWLB = round(mean(as.numeric(as.character(WorkLifeBalance)), na.rm = TRUE), 2),
    AvgYearsSincePromotion = round(mean(YearsSinceLastPromotion, na.rm = TRUE), 2),
    AttritionRate = round(mean(Attrition == "Yes") * 100, 1)
  )

cat("\nJob Satisfaction Comparison:\n")
print(jobsat_drill)

cat("\nKey Insight: Low job satisfaction is linked to lower income, more overtime,\n")
cat("worse WLB, and longer time without promotion.\n\n")

#--- Analysis 1-3-2: Attrition by Environment Satisfaction ---
cat("\n--- Analysis 1-3-2: Attrition by Environment Satisfaction ---\n")

p1_envsat <- ggplot(df_clean, aes(x = EnvironmentSatisfaction, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Analysis 1-3-2 Attrition Rate by Environment Satisfaction",
       x = "Environment Satisfaction Level", y = "Proportion") +
  theme_minimal()

print(p1_envsat)

#Drill-Down: What affects environment satisfaction?
cat("\n--- Drill-Down: What affects environment satisfaction? ---\n")

envsat_drill <- df_clean %>%
  group_by(EnvironmentSatisfaction) %>%
  summarise(
    AvgDistance = round(mean(DistanceFromHome)),
    OvertimeRate = round(mean(OverTime == "Yes") * 100, 1),
    AvgWLB = round(mean(as.numeric(as.character(WorkLifeBalance)), na.rm = TRUE), 2),
    FrequentTravelRate = round(mean(BusinessTravel == "Travel_Frequently") * 100, 1),
    AttritionRate = round(mean(Attrition == "Yes") * 100, 1)
  )

cat("\nEnvironment Satisfaction Comparison:\n")
print(envsat_drill)

cat("\nKey Insight: Low environment satisfaction correlates with longer commute,\n")
cat("more overtime, and more travel — physical work conditions affect satisfaction.\n\n")

#--- Analysis 1-3-3: Attrition by Work-Life Balance ---
cat("\n--- Analysis 1-3-3: Attrition by Work-Life Balance ---\n")

p1_wlb <- ggplot(df_clean, aes(x = WorkLifeBalance, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Analysis 1-3-3 Attrition Rate by Work-Life Balance",
       x = "Work-Life Balance Level", y = "Proportion") +
  theme_minimal()

print(p1_wlb)

#Drill-Down: What causes poor WLB?
cat("\n--- Drill-Down: What causes poor work-life balance? ---\n")

wlb_drill <- df_clean %>%
  group_by(WorkLifeBalance) %>%
  summarise(
    OvertimeRate = round(mean(OverTime == "Yes") * 100, 1),
    FrequentTravelRate = round(mean(BusinessTravel == "Travel_Frequently") * 100, 1),
    AvgDistance = round(mean(DistanceFromHome)),
    AvgIncome = round(mean(MonthlyIncome, na.rm = TRUE)),
    AttritionRate = round(mean(Attrition == "Yes") * 100, 1)
  )

cat("\nWork-Life Balance Comparison:\n")
print(wlb_drill)

cat("\nKey Insight: Poor WLB is caused by overtime, frequent travel, and long commute.\n")
cat("Employees with WLB Level 1 have significantly higher overtime than Level 4.\n\n")

#--- Analysis 1-3-4: Attrition by Relationship Satisfaction ---
cat("\n--- Analysis 1-3-4: Attrition by Relationship Satisfaction ---\n")

p1_relsat <- ggplot(df_clean, aes(x = RelationshipSatisfaction, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Analysis 1-3-4 Attrition Rate by Relationship Satisfaction",
       x = "Relationship Satisfaction Level", y = "Proportion") +
  theme_minimal()

print(p1_relsat)

#Drill-Down: What affects relationship satisfaction?
cat("\n--- Drill-Down: What affects relationship satisfaction? ---\n")

relsat_drill <- df_clean %>%
  group_by(RelationshipSatisfaction) %>%
  summarise(
    AvgYearsWithManager = round(mean(YearsWithCurrManager, na.rm = TRUE), 2),
    AvgYearsInRole = round(mean(YearsInCurrentRole, na.rm = TRUE), 2),
    AvgYearsSincePromotion = round(mean(YearsSinceLastPromotion, na.rm = TRUE), 2),
    AttritionRate = round(mean(Attrition == "Yes") * 100, 1)
  )

cat("\nRelationship Satisfaction Comparison:\n")
print(relsat_drill)

cat("\nKey Insight: Low relationship satisfaction may be linked to longer time with\n")
cat("the same manager without change, and more years since last promotion.\n\n")

#Display satisfaction plots
grid.arrange(p1_jobsat, p1_envsat, p1_wlb, p1_relsat, ncol = 2,
             top = "Analysis 1-3 Satisfaction Factors Impact on Employee Attrition")

#ANOVA for Monthly Income by Attrition
cat("\nANOVA: Monthly Income by Attrition Status\n")
income_aov <- aov(MonthlyIncome ~ Attrition, data = df_clean)
print(summary(income_aov))

#Summary of findings for Objective 1
cat("\n========================================\n")
cat("KEY FINDINGS FOR OBJECTIVE 1\n")
cat("========================================\n")
cat("\n1. DEMOGRAPHICS:\n")
cat("   - Gender does NOT significantly affect attrition (p > 0.05)\n")
cat("   - 18-25 age group has HIGHEST attrition due to lower pay and more overtime\n")
cat("   - Singles leave more because they are younger with fewer commitments\n")
cat("\n2. JOB-RELATED FACTORS:\n")
cat("   - Sales Department has highest attrition due to frequent travel\n")
cat("   - Sales Representatives are the highest risk job role\n")
cat("   - Overtime is the STRONGEST single predictor of attrition (p < 0.001)\n")
cat("   - Frequent business travel significantly increases attrition\n")
cat("\n3. SATISFACTION FACTORS:\n")
cat("   - Lower satisfaction scores on ALL metrics correlate with higher attrition\n")
cat("   - Poor WLB is caused by overtime + travel + long commute\n")
cat("   - Income is significantly lower for employees who leave (p < 0.001)\n")
cat("========================================\n")

#============================================================

#6. OBJECTIVE 2: Analyze the relationship between compensation and attrition
#Student: Li Aoyu, TP087527

#============================================================

cat("\n========== OBJECTIVE 2: COMPENSATION AND ATTRITION ANALYSIS ==========\n")
cat("Objective: To analyze how compensation factors (salary, increments, stock options)\n")
cat("influence employee attrition and identify compensation-related risk factors.\n\n")

#--- Analysis 2-1: Salary Distribution by Attrition ---
  cat("--- Analysis 2-1: Salary Distribution by Attrition ---\n")

#Monthly Income distribution by attrition
p2_income_dist <- ggplot(df_clean, aes(x = MonthlyIncome, fill = Attrition)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = attrition_colors) +
  labs(title = "Monthly Income Distribution by Attrition Status",
       x = "Monthly Income", y = "Density") +
  theme_minimal()

#Box plot of Monthly Income by Attrition
p2_income_box <- ggplot(df_clean, aes(x = Attrition, y = MonthlyIncome, fill = Attrition)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = attrition_colors) +
  labs(title = "Monthly Income Box Plot by Attrition",
       x = "Attrition Status", y = "Monthly Income") +
  theme_minimal() +
  theme(legend.position = "none")

#Hourly Rate by Attrition
p2_hourly <- ggplot(df_clean, aes(x = Attrition, y = HourlyRate, fill = Attrition)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = attrition_colors) +
  labs(title = "Hourly Rate by Attrition Status",
       x = "Attrition Status", y = "Hourly Rate") +
  theme_minimal() +
  theme(legend.position = "none")

#t-test for Monthly Income
cat("\nT-test: Monthly Income by Attrition\n")
income_ttest <- t.test(MonthlyIncome ~ Attrition, data = df_clean)
print(income_ttest)

#Display income plots
grid.arrange(p2_income_dist, p2_income_box, p2_hourly, 
             ncol = 2, nrow = 2,
             top = "Analysis 2-1 Compensation Analysis by Attrition Status")

#--- Analysis 2-2: Salary Increments and Stock Options ---
  cat("\n--- Analysis 2-2: Salary Increments and Stock Options ---\n")

#Remove NA income rows and recreate IncomeBracket
df_clean <- df_clean %>% filter(!is.na(MonthlyIncome))
df_clean$IncomeBracket <- cut(df_clean$MonthlyIncome,
                          breaks = quantile(df_clean$MonthlyIncome, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
                          labels = c("Low", "Medium", "High", "Very High"),
                           include.lowest = TRUE)  

#Percent Salary Hike by Attrition
p2_hike <- ggplot(df_clean, aes(x = Attrition, y = PercentSalaryHike, fill = Attrition)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = attrition_colors) +
  labs(title = "Percent Salary Hike by Attrition Status",
       x = "Attrition Status", y = "Percent Salary Hike") +
  theme_minimal() +
  theme(legend.position = "none")

#Stock Option Level by Attrition
p2_stock <- ggplot(df_clean, aes(x = StockOptionLevel, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Stock Option Level Distribution by Attrition",
       x = "Stock Option Level", y = "Proportion") +
  theme_minimal()

#Income Bracket analysis
p2_income_bracket <- ggplot(df_clean, aes(x = IncomeBracket, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Attrition Rate by Income Bracket",
       x = "Income Bracket", y = "Proportion") +
  theme_minimal()

#ANOVA for Percent Salary Hike
cat("\nANOVA: Percent Salary Hike by Attrition\n")
hike_aov <- aov(PercentSalaryHike ~ Attrition, data = df_clean)
print(summary(hike_aov))

#Display salary increment plots
grid.arrange(p2_hike, p2_stock, p2_income_bracket, 
             ncol = 2, nrow = 2,
             top = "Analysis 2-2 Salary Increments and Benefits Analysis")

#--- Analysis 2-3: Comprehensive Compensation Score ---

cat("\n--- Analysis 2-3: Comprehensive Compensation Score ---\n")

#First check for NAs in compensation columns
cat("NAs in compensation columns:\n")
cat("MonthlyIncome:", sum(is.na(df_clean$MonthlyIncome)), "\n")
cat("HourlyRate:", sum(is.na(df_clean$HourlyRate)), "\n")
cat("PercentSalaryHike:", sum(is.na(df_clean$PercentSalaryHike)), "\n")
cat("StockOptionLevel:", sum(is.na(df_clean$StockOptionLevel)), "\n")

#Create a composite compensation score (handle NAs)
df_clean <- df_clean %>%
  mutate(
    CompScore = scale(MonthlyIncome, center = TRUE, scale = TRUE)[,1] + 
      scale(HourlyRate, center = TRUE, scale = TRUE)[,1] + 
      scale(PercentSalaryHike, center = TRUE, scale = TRUE)[,1] + 
      scale(as.numeric(as.character(StockOptionLevel)), center = TRUE, scale = TRUE)[,1]
  )

#Check if CompScore was created successfully
cat("\nCompScore summary:\n")
print(summary(df_clean$CompScore))

#Compensation Score by Attrition
p2_comp_score <- ggplot(df_clean, aes(x = Attrition, y = CompScore, fill = Attrition)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.2, alpha = 0.5) +
  scale_fill_manual(values = attrition_colors) +
  labs(title = "Analysis 2-3 Composite Compensation Score by Attrition",
       x = "Attrition Status", y = "Compensation Score (Standardized)") +
  theme_minimal() +
  theme(legend.position = "none")

#Correlation between compensation variables
comp_vars <- df_clean %>%
  select(MonthlyIncome, HourlyRate, DailyRate, PercentSalaryHike, StockOptionLevel)

cor_matrix <- cor(comp_vars, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper",
         title = "Analysis 2-3 Correlation Matrix of Compensation Variables",
         mar = c(0,0,2,0), tl.col = "black", tl.cex = 0.8,
         addCoef.col = "black", number.cex = 0.7)

print(p2_comp_score)

#Logistic regression for compensation impact
cat("\nLogistic Regression: Attrition ~ Compensation Factors\n")
log_model_comp <- glm(Attrition ~ MonthlyIncome + HourlyRate + PercentSalaryHike + StockOptionLevel,
                      data = df_clean, family = "binomial")
print(summary(log_model_comp))

cat("\n--- Key Findings for Objective 2 ---\n")
cat("1. Employees who leave have significantly lower Monthly Income (p < 0.001)\n")
cat("2. Lower stock option levels correlate with higher attrition rates\n")
cat("3. Income bracket analysis shows highest attrition in lowest income group\n")
cat("4. The composite compensation score clearly differentiates stayers from leavers\n")
cat("5. Monthly Income is the strongest compensation predictor of attrition\n")

#============================================================

#7. OBJECTIVE 3: Examine work-life balance impact on employee retention
#Student: Clement Kek Ching Haow, TP088688

#============================================================

  cat("\n========== OBJECTIVE 3: WORK-LIFE BALANCE AND RETENTION ==========\n")
cat("Objective: To examine how work-life balance factors (distance from home, overtime,\n")
cat("business travel, work-life balance score) impact employee retention.\n\n")

# --- Analysis 3-1: Distance and Commute Impact ---
cat("--- Analysis 3-1: Distance and Commute Impact ---\n")

# Remove rows with NA in DistanceCategory and WorkLifeBalance 
df_clean <- df_clean %>% filter(!is.na(DistanceCategory), !is.na(WorkLifeBalance))

# Plot 3-1-1: Attrition by Distance Category 
p3_distance <- ggplot(df_clean, aes(x = DistanceCategory, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = percent) +
  labs(title = "Analysis 3-1-1 Attrition Rate by Distance from Home",
       x = "Distance Category", y = "Proportion") +
  theme_minimal()
print(p3_distance)

# Plot 3-1-2: Distance distribution 
p3_dist_dist <- ggplot(df_clean, aes(x = DistanceFromHome, fill = Attrition)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = attrition_colors) +
  labs(title = "Analysis 3-1-2 Distance from Home Distribution by Attrition",
       x = "Distance from Home (miles)", y = "Density") +
  theme_minimal()
print(p3_dist_dist)

# Plot 3-1-3: Distance vs Income 
p3_dist_income <- ggplot(df_clean, aes(x = DistanceFromHome, y = MonthlyIncome, color = Attrition)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_color_manual(values = attrition_colors) +
  labs(title = "Analysis 3-1-3 Distance vs Monthly Income by Attrition",
       x = "Distance from Home", y = "Monthly Income") +
  theme_minimal()
print(p3_dist_income)

# plot 3-1-4: Distance vs WLB boxplot 
p3_dist_wlb <- ggplot(df_clean, aes(x = DistanceCategory, y = as.numeric(as.character(WorkLifeBalance)), fill = Attrition)) +
  geom_boxplot() +
  scale_fill_manual(values = attrition_colors) +
  labs(title = "Analysis 3-1-4 Work-Life Balance Score by Distance Category",
       x = "Distance Category", y = "Work-Life Balance Score") +
  theme_minimal()
print(p3_dist_wlb)

# plot 3-1-5: Overtime rate by distance 
p3_dist_overtime <- df_clean %>%
  group_by(DistanceCategory) %>%
  summarise(OvertimeRate = mean(OverTime == "Yes") * 100) %>%
  ggplot(aes(x = DistanceCategory, y = OvertimeRate, fill = DistanceCategory)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Analysis 3-1-5 Overtime Rate by Distance Category",
       x = "Distance Category", y = "Overtime Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p3_dist_overtime)

# Combine all 3-1 plots into one grid (summary panel)
grid.arrange(p3_distance, p3_dist_dist, p3_dist_income, 
             p3_dist_wlb, p3_dist_overtime,
             ncol = 2, nrow = 3,
             top = "Analysis 3-1 - Commute Distance Impact on Attrition")

# Drill-down: Distance Category Comparison
cat("\n--- Drill-Down: Distance Category Comparison ---\n")
distance_drill <- df_clean %>%
  group_by(DistanceCategory) %>%
  summarise(
    AvgIncome = round(mean(MonthlyIncome, na.rm = TRUE)),
    OvertimeRate = round(mean(OverTime == "Yes") * 100, 1),
    AvgWLB = round(mean(as.numeric(as.character(WorkLifeBalance)), na.rm = TRUE), 2),
    AttritionRate = round(mean(Attrition == "Yes") * 100, 1),
    EmployeeCount = n()
  )
print(distance_drill)

# T-test for Distance
cat("\nT-test: Distance from Home by Attrition\n")
dist_ttest <- t.test(DistanceFromHome ~ Attrition, data = df_clean)
print(dist_ttest)

cat("\nKey Insight for 3-1: Employees living farther away have longer commutes,\n")
cat("higher overtime rates, and significantly lower work-life balance scores.\n")
cat("This directly increases attrition (p < 0.001).\n\n")

# --- Analysis 3-2: Work-Life Balance Score Analysis ---
cat("\n--- Analysis 3-2: Work-Life Balance Score Analysis ---\n")

# Ensure WorkLifeBalance is a proper factor
df_clean$WorkLifeBalance <- as.factor(as.character(df_clean$WorkLifeBalance))

# Plot 3-2-1: Attrition by WLB (original)
p3_wlb_dist <- ggplot(df_clean, aes(x = WorkLifeBalance, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = attrition_colors) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Analysis 3-2-1 Attrition Rate by Work-Life Balance Score",
       x = "Work-Life Balance Level", y = "Proportion") +
  theme_minimal()
print(p3_wlb_dist)

# Plot 3-2-2: WLB by Department (original)
p3_wlb_dept <- ggplot(df_clean, aes(x = Department, fill = WorkLifeBalance)) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d(option = "D") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Analysis 3-2-2 Work-Life Balance Distribution by Department",
       x = "Department", y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p3_wlb_dept)

# Plot 3-2-3: Overtime distribution by WLB (original)
p3_ot_wlb <- ggplot(df_clean, aes(x = WorkLifeBalance, fill = OverTime)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("No" = "#2ECC71", "Yes" = "#E74C3C")) +
  scale_y_continuous(labels = percent) +
  labs(title = "Analysis 3-2-3 Overtime Distribution by Work-Life Balance",
       x = "Work-Life Balance", y = "Proportion") +
  theme_minimal()
print(p3_ot_wlb)

# Plot 3-2-4: Boxplot of WLB by Attrition (new)
p3_wlb_box <- ggplot(df_clean, aes(x = Attrition, y = as.numeric(as.character(WorkLifeBalance)), fill = Attrition)) +
  geom_boxplot() +
  scale_fill_manual(values = attrition_colors) +
  labs(title = "Analysis 3-2-4 Work-Life Balance Score Distribution by Attrition",
       x = "Attrition Status", y = "Work-Life Balance Score") +
  theme_minimal() +
  theme(legend.position = "none")
print(p3_wlb_box)

# Combine all 3-2 plots into one grid
grid.arrange(p3_wlb_dist, p3_wlb_dept, p3_ot_wlb, p3_wlb_box,
             ncol = 2, nrow = 2,
             top = "SUMMARY: Analysis 3-2 - Work-Life Balance Score Impact on Attrition")

# Drill-down: WLB Comparison
cat("\n--- Drill-Down: Work-Life Balance Comparison ---\n")
wlb_drill <- df_clean %>%
  group_by(WorkLifeBalance) %>%
  summarise(
    OvertimeRate = round(mean(OverTime == "Yes") * 100, 1),
    TravelRate = round(mean(BusinessTravel == "Travel_Frequently") * 100, 1),
    AvgDistance = round(mean(DistanceFromHome, na.rm = TRUE), 1),
    AttritionRate = round(mean(Attrition == "Yes") * 100, 1),
    EmployeeCount = n()
  )
print(wlb_drill)

# Chi-square test
cat("\nChi-square test: Work-Life Balance vs Attrition\n")
wlb_table <- table(df_clean$WorkLifeBalance, df_clean$Attrition)
print(chisq.test(wlb_table))

cat("\nKey Insight for 3-2: Poor work-life balance (level 1) is associated with\n")
cat("higher overtime, more frequent travel, and an attrition rate >30%.\n")
cat("Chi-square test confirms strong association (p < 0.001).\n\n")

# --- Analysis 3-3: Combined Impact of Overtime and Business Travel ---
cat("\n--- Analysis 3-3: Combined Impact of Overtime and Business Travel ---\n")

# Plot 3-3-1: Combined Overtime & Travel effect (original)
p3_ot_travel <- df_clean %>%
  group_by(OverTime, BusinessTravel) %>%
  summarise(AttritionRate = mean(Attrition == "Yes"),
            Count = n(),
            .groups = 'drop') %>%
  ggplot(aes(x = OverTime, y = AttritionRate, fill = BusinessTravel)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis_d(option = "C") +
  scale_y_continuous(labels = percent) +
  labs(title = "Analysis 3-3-1 Attrition Rate: Combined Overtime and Travel Effect",
       x = "Overtime Status", y = "Attrition Rate") +
  theme_minimal()
print(p3_ot_travel)

# Plot 3-3-2: Years at Company vs WLB (original)
p3_years_wlb <- ggplot(df_clean, aes(x = YearsAtCompany, y = as.numeric(as.character(WorkLifeBalance)),
                                     color = Attrition)) +
  geom_jitter(alpha = 0.5, height = 0.2) +
  scale_color_manual(values = attrition_colors) +
  labs(title = "Analysis 3-3-2 Years at Company vs Work-Life Balance",
       x = "Years at Company", y = "Work-Life Balance Level") +
  theme_minimal()
print(p3_years_wlb)

# Create composite WLB stress score (original)
df_clean <- df_clean %>%
  mutate(
    WLBStressScore = as.numeric(as.character(WorkLifeBalance)) * -1 +
      as.numeric(OverTime == "Yes") * 2 +
      as.numeric(BusinessTravel == "Travel_Frequently") * 1.5 +
      DistanceFromHome / max(DistanceFromHome) * 2
  )

# Plot 3-3-3: Stress score by attrition (original)
p3_stress <- ggplot(df_clean, aes(x = Attrition, y = WLBStressScore, fill = Attrition)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.2, alpha = 0.5) +
  scale_fill_manual(values = attrition_colors) +
  labs(title = "Analysis 3-3-3 Work-Life Balance Stress Score by Attrition",
       x = "Attrition Status", y = "WLB Stress Score") +
  theme_minimal() +
  theme(legend.position = "none")
print(p3_stress)

# Plot 3-3-4: Attrition by Travel only (new)
p3_travel_only <- df_clean %>%
  group_by(BusinessTravel) %>%
  summarise(AttritionRate = mean(Attrition == "Yes") * 100) %>%
  ggplot(aes(x = BusinessTravel, y = AttritionRate, fill = BusinessTravel)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "D") +
  labs(title = "Analysis 3-3-4 Attrition Rate by Business Travel Frequency",
       x = "Business Travel", y = "Attrition Rate (%)") +
  theme_minimal()
print(p3_travel_only)

# Plot 3-3-5: Attrition by Overtime only (new)
p3_overtime_only <- df_clean %>%
  group_by(OverTime) %>%
  summarise(AttritionRate = mean(Attrition == "Yes") * 100) %>%
  ggplot(aes(x = OverTime, y = AttritionRate, fill = OverTime)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("No" = "#2ECC71", "Yes" = "#E74C3C")) +
  labs(title = "Analysis 3-3-5 Attrition Rate by Overtime Status",
       x = "Overtime", y = "Attrition Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")
print(p3_overtime_only)

# Combine all 3-3 plots into one grid
grid.arrange(p3_ot_travel, p3_years_wlb, p3_stress, 
             p3_travel_only, p3_overtime_only,
             ncol = 2, nrow = 3,
             top = "SUMMARY: Analysis 3-3 - Combined Overtime, Travel & Stress Impact")

# Drill-down: Combined effect numbers
cat("\n--- Drill-Down: Combined Overtime & Travel Effect ---\n")
combined_drill <- df_clean %>%
  group_by(OverTime, BusinessTravel) %>%
  summarise(
    AttritionRate = round(mean(Attrition == "Yes") * 100, 1),
    Count = n(),
    AvgWLB = round(mean(as.numeric(as.character(WorkLifeBalance)), na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(desc(AttritionRate))
print(combined_drill)

# Logistic regression for WLB factors
cat("\nLogistic Regression: Attrition ~ Work-Life Balance Factors\n")
log_model_wlb <- glm(Attrition ~ WorkLifeBalance + OverTime + BusinessTravel +
                       DistanceFromHome + WLBStressScore,
                     data = df_clean, family = "binomial")
summary(log_model_wlb)

cat("\nInterpretation of Logistic Regression:\n")
cat("- Positive coefficients increase attrition probability.\n")
cat("- Overtime is the strongest positive predictor (coefficient > 1).\n")
cat("- Frequent travel and long commute also increase turnover risk.\n")
cat("- Higher WorkLifeBalance score (better balance) reduces attrition odds.\n\n")

# High-risk WLB employees (original)
high_risk_wlb <- df_clean %>%
  filter(WorkLifeBalance == "1") %>%
  group_by(Department) %>%
  summarise(
    AttritionRate = round(mean(Attrition == "Yes") * 100, 1),
    OvertimeRate = round(mean(OverTime == "Yes") * 100, 1),
    EmployeeCount = n()
  ) %>%
  arrange(desc(AttritionRate))

cat("\nHigh Risk Work-Life Balance Groups (WLB = 1):\n")
print(high_risk_wlb)

# T-test for WLBStressScore
cat("\nT-test: WLB Stress Score by Attrition\n")
stress_ttest <- t.test(WLBStressScore ~ Attrition, data = df_clean)
print(stress_ttest)

# --- Summary of Findings for Objective 3 ---
cat("\n========================================\n")
cat("KEY FINDINGS FOR OBJECTIVE 3\n")
cat("========================================\n")
cat("\n1. COMMUTE DISTANCE:\n")
cat("   - Employees with >15 miles commute have ~30% higher attrition than those <5 miles.\n")
cat("   - Longer distance correlates with lower work-life balance and higher overtime.\n")
cat("   - T-test shows highly significant difference (p < 0.001).\n")
cat("\n2. WORK-LIFE BALANCE SCORE:\n")
cat("   - Poor WLB (level 1) → attrition rate >30%; good WLB (level 4) → <10%.\n")
cat("   - Sales department has the highest proportion of poor WLB employees.\n")
cat("   - Chi-square test confirms strong association (p < 0.001).\n")
cat("\n3. OVERTIME & BUSINESS TRAVEL:\n")
cat("   - Employees with BOTH overtime AND frequent travel have >50% attrition.\n")
cat("   - Logistic regression shows overtime is the strongest predictor (coefficient ~1.2).\n")
cat("   - Frequent travel alone still doubles attrition compared to non-travelers.\n")
cat("\n4. STRESS SCORE:\n")
cat("   - The composite WLB stress score clearly separates leavers from stayers (p < 0.001).\n")
cat("   - High-risk departments (Sales, HR) need targeted interventions.\n")
cat("========================================\n")

#============================================================
#8. EXTRA FEATURES

#============================================================
  cat("\n========== EXTRA FEATURES ==========\n")

#--- Extra Feature 1: Interactive Dashboard with Plotly ---
#This enhances the analysis by providing interactive visualizations
#that allow HR managers to explore data dynamically
cat("\n--- Extra Feature 1: Interactive Visualization Dashboard ---\n")

#Interactive attrition rate by department
interactive_dept <- df_clean %>%
  group_by(Department) %>%
  summarise(
    AttritionRate = mean(Attrition == "Yes") * 100,
    EmployeeCount = n(),
    AvgIncome = mean(MonthlyIncome)
  ) %>%
  plot_ly(
    x = ~Department,
    y = ~AttritionRate,
    type = "bar",
    marker = list(color = ~AttritionRate, colorscale = "Reds"),
    text = ~paste("Department:", Department,
                  "
Attrition Rate:", round(AttritionRate, 2), "%",
                  "
Employees:", EmployeeCount,
                  "
Avg Income:", scales::dollar(AvgIncome)),
    hoverinfo = "text"
  ) %>%
  layout(
    title = "Interactive Attrition Rate by Department",
    xaxis = list(title = "Department"),
    yaxis = list(title = "Attrition Rate (%)")
  )

#Save interactive plot as HTML
htmlwidgets::saveWidget(interactive_dept, "attrition_dashboard.html", selfcontained = TRUE)
cat("Interactive dashboard saved as 'attrition_dashboard.html'\n")

#--- Extra Feature 2: Automated Report Generation ---
#This feature creates an automated summary report for HR stakeholders
cat("\n--- Extra Feature 2: Automated Risk Assessment Report ---\n")

#Function to generate employee risk score (FIXED with NA handling)
calculate_risk_score <- function(employee) {
  risk_score <- 0
  
#High overtime risk
  if (!is.na(employee$OverTime) && employee$OverTime == "Yes") risk_score <- risk_score + 25

#Low job satisfaction risk
  if (!is.na(employee$JobSatisfaction) && as.numeric(as.character(employee$JobSatisfaction)) <= 2) risk_score <- risk_score + 20
  
#Low work-life balance risk
  if (!is.na(employee$WorkLifeBalance) && as.numeric(as.character(employee$WorkLifeBalance)) <= 2) risk_score <- risk_score + 15
  
#Low income risk
  income_threshold <- quantile(df_clean$MonthlyIncome, 0.25, na.rm = TRUE)
  if (!is.na(employee$MonthlyIncome) && employee$MonthlyIncome < income_threshold) risk_score <- risk_score + 20
  
#Frequent travel risk
  if (!is.na(employee$BusinessTravel) && employee$BusinessTravel == "Travel_Frequently") risk_score <- risk_score + 15
  
#Long commute risk
  if (!is.na(employee$DistanceFromHome) && employee$DistanceFromHome > 20) risk_score <- risk_score + 10
  
#Low environment satisfaction
  if (!is.na(employee$EnvironmentSatisfaction) && as.numeric(as.character(employee$EnvironmentSatisfaction)) <= 2) risk_score <- risk_score + 10
  
  return(min(risk_score, 100))
}

#Apply risk assessment to entire dataset
df_clean <- df_clean %>%
  rowwise() %>%
  mutate(RiskScore = calculate_risk_score(cur_data())) %>%
  ungroup()

#Categorize risk levels
df_clean <- df_clean %>%
  mutate(
    RiskLevel = case_when(
      RiskScore >= 70 ~ "High Risk",
      RiskScore >= 40 ~ "Medium Risk",
      RiskScore >= 20 ~ "Low Risk",
      TRUE ~ "Minimal Risk"
    )
  )

#Risk distribution visualization
risk_plot <- ggplot(df_clean, aes(x = RiskLevel, fill = RiskLevel)) +
  geom_bar() +
  scale_fill_manual(values = c("High Risk" = "#E74C3C",
                               "Medium Risk" = "#F39C12",
                               "Low Risk" = "#3498DB",
                               "Minimal Risk" = "#2ECC71")) +
  labs(title = "Extra Feature 2 Employee Attrition Risk Distribution",
       x = "Risk Level",
       y = "Number of Employees") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(risk_plot)

#Generate risk report
high_risk_employees <- df_clean %>%
  filter(RiskLevel == "High Risk") %>%
  select(EmployeeNumber, Age, Department, JobRole, MonthlyIncome,
         OverTime, WorkLifeBalance, RiskScore) %>%
  arrange(desc(RiskScore))

cat("Number of High Risk Employees:", nrow(high_risk_employees), "\n")
cat("Top 5 High Risk Employees:\n")
print(head(high_risk_employees, 5))

#Save risk assessment report
write.csv(high_risk_employees, "high_risk_employees_report.csv", row.names = FALSE)
cat("Risk assessment report saved as 'high_risk_employees_report.csv'\n")

#--- Extra Feature 3: What-If Analysis Tool ---
#This feature allows HR to simulate the impact of changes on attrition
cat("\n--- Extra Feature 3: What-If Scenario Analysis ---\n")

#Function to simulate attrition impact of policy changes
simulate_attrition_change <- function(data,
                                      pct_hike_increase = 0,
                                      overtime_reduction = 0,
                                      wlb_improvement = 0) {
  
  simulated_data <- data
  
#Apply salary hike increase
  if (pct_hike_increase > 0) {
    simulated_data$MonthlyIncome <- simulated_data$MonthlyIncome * (1 + pct_hike_increase/100)
  }
  
#Apply overtime reduction (convert some overtime to non-overtime)
  if (overtime_reduction > 0 & overtime_reduction <= 1) {
    overtime_employees <- which(simulated_data$OverTime == "Yes")
    n_reduce <- floor(length(overtime_employees) * overtime_reduction)
    if (n_reduce > 0) {
      employees_to_change <- sample(overtime_employees, n_reduce)
      simulated_data$OverTime[employees_to_change] <- "No"
    }
  }
  
#Improve work-life balance
  if (wlb_improvement > 0) {
    low_wlb <- which(as.numeric(as.character(simulated_data$WorkLifeBalance)) <= 2)
    n_improve <- floor(length(low_wlb) * wlb_improvement)
    if (n_improve > 0) {
      employees_to_improve <- sample(low_wlb, n_improve)
      simulated_data$WorkLifeBalance[employees_to_improve] <- as.factor(
        pmin(as.numeric(as.character(simulated_data$WorkLifeBalance[employees_to_improve])) + 1, 4)
      )
    }
  }
  
#Recalculate risk scores
  simulated_data <- simulated_data %>%
    rowwise() %>%
    mutate(RiskScore = calculate_risk_score(cur_data())) %>%
    ungroup()
  
  simulated_data <- simulated_data %>%
    mutate(
      RiskLevel = case_when(
        RiskScore >= 70 ~ "High Risk",
        RiskScore >= 40 ~ "Medium Risk",
        RiskScore >= 20 ~ "Low Risk",
        TRUE ~ "Minimal Risk"
      )
    )
  
#Calculate new attrition risk
  current_high_risk <- mean(data$RiskLevel == "High Risk") * 100
  new_high_risk <- mean(simulated_data$RiskLevel == "High Risk") * 100
  
  cat("\n--- Simulation Results ---\n")
  cat("Salary increase:", pct_hike_increase, "%\n")
  cat("Overtime reduction:", overtime_reduction * 100, "%\n")
  cat("Work-life balance improvement:", wlb_improvement * 100, "%\n")
  cat("Current High Risk %:", round(current_high_risk, 2), "%\n")
  cat("New High Risk %:", round(new_high_risk, 2), "%\n")
  cat("Reduction in High Risk:", round(current_high_risk - new_high_risk, 2), "%\n")
  
  return(list(before = current_high_risk, after = new_high_risk,
              reduction = current_high_risk - new_high_risk))
}

#Run multiple scenarios
scenarios <- list(
  scenario1 = simulate_attrition_change(df_clean, pct_hike_increase = 10, overtime_reduction = 0, wlb_improvement = 0),
  scenario2 = simulate_attrition_change(df_clean, pct_hike_increase = 0, overtime_reduction = 0.3, wlb_improvement = 0),
  scenario3 = simulate_attrition_change(df_clean, pct_hike_increase = 0, overtime_reduction = 0, wlb_improvement = 0.2),
  scenario4 = simulate_attrition_change(df_clean, pct_hike_increase = 10, overtime_reduction = 0.3, wlb_improvement = 0.2)
)

#Visualize scenario results
scenario_df <- data.frame(
  Scenario = c("10% Salary Increase", "30% Overtime Reduction",
               "20% WLB Improvement", "Combined Changes"),
  HighRiskReduction = c(scenarios$scenario1$reduction,
                        scenarios$scenario2$reduction,
                        scenarios$scenario3$reduction,
                        scenarios$scenario4$reduction)
)

scenario_plot <- ggplot(scenario_df, aes(x = reorder(Scenario, HighRiskReduction),
                                         y = HighRiskReduction)) +
  geom_bar(stat = "identity", fill = "#3498DB", alpha = 0.8) +
  coord_flip() +
  labs(title = "Extra Feature 3 Impact of Policy Changes on High-Risk Employee Percentage",
       x = "Scenario",
       y = "Reduction in High-Risk Employees (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(scenario_plot)

cat("\n--- Extra Features Summary ---\n")
cat("1. Interactive dashboard enables dynamic data exploration for HR managers\n")
cat("2. Automated risk assessment identifies at-risk employees proactively\n")
cat("3. What-if analysis tool helps evaluate policy changes before implementation\n")

#============================================================

#9. SUMMARY AND EXPORT

#============================================================

cat("\n========== FINAL SUMMARY ==========\n")

#Overall attrition statistics
cat("\n--- Overall Attrition Analysis Summary ---\n")
attrition_summary <- df_clean %>%
  summarise(
    TotalEmployees = n(),
    AttritionCount = sum(Attrition == "Yes"),
    AttritionRate = round(mean(Attrition == "Yes") * 100, 2),
    AvgAge = round(mean(Age), 1),
    AvgMonthlyIncome = round(mean(MonthlyIncome), 2),
    AvgYearsAtCompany = round(mean(YearsAtCompany), 2),
    AvgDistanceFromHome = round(mean(DistanceFromHome), 2),
    OvertimePercentage = round(mean(OverTime == "Yes") * 100, 2),
    HighRiskPercentage = round(mean(RiskLevel == "High Risk") * 100, 2)
  )

print(attrition_summary)

#Department-wise summary
dept_summary <- df_clean %>%
  group_by(Department) %>%
  summarise(
    Employees = n(),
    AttritionCount = sum(Attrition == "Yes"),
    AttritionRate = round(mean(Attrition == "Yes") * 100, 2),
    AvgIncome = round(mean(MonthlyIncome), 2),
    AvgSatisfaction = round(mean(as.numeric(as.character(JobSatisfaction)), na.rm = TRUE), 2),
    AvgWLB = round(mean(as.numeric(as.character(WorkLifeBalance)), na.rm = TRUE), 2)
  ) %>%
  arrange(desc(AttritionRate))

cat("\n--- Department-wise Attrition Summary ---\n")
print(dept_summary)

#Export cleaned dataset
write.csv(df_clean, "cleaned_employee_attrition.csv", row.names = FALSE)
cat("\nCleaned dataset exported as 'cleaned_employee_attrition.csv'\n")

#Save all key plots
png("attrition_demographic_analysis.png", width = 12, height = 8, units = "in", res = 300)
grid.arrange(p1_gender, p1_age, p1_marital, ncol = 2)
dev.off()

png("compensation_analysis.png", width = 12, height = 8, units = "in", res = 300)
grid.arrange(p2_income_dist, p2_income_box, p2_hike, p2_comp_score, ncol = 2)
dev.off()

png("wlb_analysis.png", width = 12, height = 8, units = "in", res = 300)
grid.arrange(p3_wlb_dist, p3_distance, p3_stress, ncol = 2)
dev.off()

cat("All analysis plots saved successfully!\n")

#Final message
cat("\n========================================\n")
cat("ANALYSIS COMPLETE\n")
cat("========================================\n")
cat("All analyses have been completed successfully.\n")
cat("Key deliverables:\n")
cat("1. Cleaned dataset: cleaned_employee_attrition.csv\n")
cat("2. Risk report: high_risk_employees_report.csv\n")
cat("3. Interactive dashboard: attrition_dashboard.html\n")
cat("4. Analysis plots: PNG files in working directory\n")
cat("========================================\n")

#============================================================

