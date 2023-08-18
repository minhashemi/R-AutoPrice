# Regression Analysis Project in R

This repository contains a project focused on regression analysis, covering various aspects of regression techniques, feature selection, and analysis. The project involves working with a dataset that includes information about different car models and their prices based on various features. The main goals of this project are to familiarize the programmer with regression issues, analysis methods, feature selection techniques, and practical implementation using the R programming language.

You may find a full script by script documentation in `output.pdf` as well as `main.Rmd` files in the repository.

## Project Objectives

The primary objectives of this project are as follows:

1. **Data Loading and Preprocessing**: Load the dataset and perform appropriate preprocessing steps on it. The following tasks are mandatory:

   - Choose three columns of the dataset and create box plots for them. Provide explanations for each plot.
   - Handle missing data without removing any data points. Describe your method and rationale.
   - Generate a correlation map for the dataset's features. Propose four hypotheses about feature correlations and test them using t-tests.
   - Create dummy variables for categorical variables in the dataset.

2. **Multiple Regression Analysis with Training Data**: Fit a multiple regression model to the training data. Perform the following tasks and analyses:

   - Fit the model to the training data and report the following metrics for both training and test datasets:
     - Residual Sum of Squares (RSS)
     - Total Sum of Squares (TSS)
     - Mean Squared Error (MSE)
     - R-Squared
     - Adjusted R-Squared
   - Explain the significance and applications of each mentioned metric.
   - Create a comparison map of coefficient magnitudes. Discuss the importance of high coefficients and the impact of standardized data scales.

3. **Model Performance and Interpretation**: Describe the model's performance on the test data. Investigate ways to enhance the model's performance, including interpretability and predictive capability.

4. **Feature Selection and Analysis**: In this section, perform feature selection based on the results from previous sections, statistical tests, and analyses. Follow these steps:

   - Use t-tests and p-values to reduce the number of features while enhancing model interpretability. Describe the changes in your model's performance and mentioned metrics.
   - Utilize ANOVA and f-statistics to perform feature selection and present the top 10 features.
   - Identify pairs of variables that exhibit synergy. Select and analyze 10 such pairs, and consider adding them to your feature set. Justify the need for further t-tests and feature selection.

## Communication and Collaboration Rules

1. The only means of communication with the project team is through the Telegram group or the Q&A section on Koofa. Direct answers to specific questions will not be provided.
2. Collaboration is allowed within the project, but refrain from sharing your code with others.
3. Regardless of the final results, the analysis and reasoning behind different approaches carry high importance and will be graded separately.

## Language and Libraries

- The project must be conducted using the R programming language.
- Include suitable comments for crucial code sections and lines to ensure clarity during submission.

## Project Organization

The project is divided into several sections, each addressing a specific aspect of regression analysis and feature selection. The sections are as follows:

1. Data Loading and Preprocessing
2. Multiple Regression Analysis with Training Data
3. Feature Selection and Analysis

(Optional):
4. Additional Models

Please follow the instructions in each section carefully and document your findings, analyses, and interpretations thoroughly.

For the dataset and further details, refer to the [data](data/) directory.

For implementation and analysis, refer to the corresponding sections in the provided code files.


Best regards,
Amin Hashemi
Summer 2023
