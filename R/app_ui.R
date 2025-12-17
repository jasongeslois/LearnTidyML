#' LearnTidyML UI
#'
#' @import shiny
#' @import shinydashboard
#' @export
app_ui <- function() {
 # Add resource path for logo
 logo_dir <- system.file("shiny-app/www", package = "LearnTidyML")
 if (logo_dir != "") {
   shiny::addResourcePath("www", logo_dir)
 }

 shinydashboard::dashboardPage(

   shinydashboard::dashboardHeader(
     title = tagList(
       tags$img(src = "www/logo.png", height = "40px", style = "margin-right: 10px;"),
       "LearnTidyML"
     ),
     tags$li(class = "dropdown",
             actionButton("show_about_btn", "About", class = "btn-flat",
                          style = "color: white; background-color: transparent; border-color: transparent; margin-top: 8px; margin-right: 5px;")
     )
   ),
   
   shinydashboard::dashboardSidebar(
     shinydashboard::sidebarMenu(
       id = "sidebar_menu",
       shinydashboard::menuItem("1. import Data", tabName = "import", icon = icon("import")),
       shinydashboard::menuItem("2. Data Profile", tabName = "profile", icon = icon("chart-bar")),
       shinydashboard::menuItem("3. Task & Methods", tabName = "methods", icon = icon("robot")),
       shinydashboard::menuItem("4. Data Cleaning", tabName = "cleaning", icon = icon("broom")),
       shinydashboard::menuItem("5. Train Model", tabName = "training", icon = icon("cogs")),
       shinydashboard::menuItem("6. Results", tabName = "results", icon = icon("chart-line")),
       shinydashboard::menuItem("7. Validate New Data", tabName = "validate", icon = icon("check-circle"))
     ),
     hr(),
     div(style = "padding: 10px;",
         h5("Workflow Progress"),
         uiOutput("workflow_progress")
     )
   ),
   
   shinydashboard::dashboardBody(
     shinyjs::useShinyjs(),
     tags$head(
       tags$style(HTML("
         .metric-card {
           background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
           color: white;
           padding: 20px;
           border-radius: 10px;
           text-align: center;
           margin-bottom: 15px;
         }
         .metric-card .metric-value {
           font-size: 36px;
           font-weight: bold;
         }
         .metric-card .metric-label {
           font-size: 14px;
           opacity: 0.9;
         }
         .metric-card .metric-explanation {
           font-size: 11px;
           opacity: 0.8;
           margin-top: 5px;
         }
         .metric-card.green { background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); }
         .metric-card.yellow { background: linear-gradient(135deg, #F2994A 0%, #F2C94C 100%); }
         .metric-card.orange { background: linear-gradient(135deg, #f093fb 0%, #f5576c 100%); }
         .metric-card.red { background: linear-gradient(135deg, #CB356B 0%, #BD3F32 100%); }
         .metric-card.blue { background: linear-gradient(135deg, #2193b0 0%, #6dd5ed 100%); }
         .metric-card.gray { background: linear-gradient(135deg, #606c88 0%, #3f4c6b 100%); }
         
         .explanation-box {
           background: #f8f9fa;
           border-left: 4px solid #3498db;
           padding: 15px;
           margin: 10px 0;
           border-radius: 0 5px 5px 0;
         }
         .explanation-box.warning {
           border-left-color: #f39c12;
           background: #fef9e7;
         }
         .explanation-box.success {
           border-left-color: #27ae60;
           background: #eafaf1;
         }
         .explanation-box.info {
           border-left-color: #9b59b6;
           background: #f5eef8;
         }
         
         .step-complete { color: #27ae60; }
         .step-pending { color: #bdc3c7; }
         .step-current { color: #3498db; font-weight: bold; }
         
         .cleaning-option {
           background: white;
           border: 1px solid #ddd;
           border-radius: 5px;
           padding: 15px;
           margin-bottom: 10px;
         }
         .cleaning-option:hover {
           border-color: #3498db;
           box-shadow: 0 2px 5px rgba(0,0,0,0.1);
         }
         
         .training-status {
           padding: 15px;
           border-radius: 5px;
           margin: 10px 0;
         }
         .training-status.running {
           background: #fff3cd;
           border: 1px solid #ffc107;
         }
         .training-status.complete {
           background: #d4edda;
           border: 1px solid #28a745;
         }
         .training-status.error {
           background: #f8d7da;
           border: 1px solid #dc3545;
         }
         
         .model-checkbox-group .checkbox {
           background: #f8f9fa;
           padding: 10px 15px;
           margin: 5px 0;
           border-radius: 5px;
           border: 1px solid #ddd;
         }
         .model-checkbox-group .checkbox:hover {
           background: #e9ecef;
           border-color: #3498db;
         }
         
         .edu-tooltip {
           color: #3498db;
           cursor: help;
           border-bottom: 1px dotted #3498db;
         }
         
         .comparison-winner {
           background: #d4edda;
           border: 2px solid #28a745;
           border-radius: 5px;
           padding: 5px;
         }
         
         .plot-explanation {
           background: #f0f0f0;
           padding: 10px;
           border-radius: 5px;
           margin-top: 10px;
           font-size: 12px;
         }

         .code-container {
           position: relative;
         }

         .copy-code-btn {
           position: absolute;
           right: 10px;
           top: 10px;
           z-index: 10;
         }
       ")),
       tags$script(HTML("
         // Copy code to clipboard function
         function copyCodeToClipboard() {
           var codeText = document.getElementById('method_code').innerText;
           navigator.clipboard.writeText(codeText).then(function() {
             // Show success message
             var btn = document.getElementById('copy_code_btn');
             var originalText = btn.innerHTML;
             btn.innerHTML = '<i class=\"fa fa-check\"></i> Copied!';
             btn.classList.add('btn-success');
             btn.classList.remove('btn-primary');
             setTimeout(function() {
               btn.innerHTML = originalText;
               btn.classList.remove('btn-success');
               btn.classList.add('btn-primary');
             }, 2000);
           }, function(err) {
             alert('Failed to copy code: ' + err);
           });
         }
       "))
     ),
     
     shinydashboard::tabItems(
       
       # =====================================================================
       # TAB 1: import DATA
       # =====================================================================
       shinydashboard::tabItem(
         tabName = "import",
         fluidRow(
           shinydashboard::box(
             title = "import Your Data",
             width = 12,
             fileInput("file", "Choose Data File",
                       accept = c("text/csv", "text/tab-separated-values", "text/plain",
                                "application/vnd.ms-excel", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                ".csv", ".tsv", ".txt", ".xlsx", ".xls", ".rds", ".RData", ".rda",
                                ".sas7bdat", ".xpt", ".dta", ".sav")),
             helpText(
               "import a data file to begin analysis.",
               tags$br(),
               tags$strong("Supported formats:"), " CSV, TSV, TXT, Excel (.xlsx/.xls), R data (.rds/.RData), SAS (.sas7bdat/.xpt), Stata (.dta), SPSS (.sav)",
               tags$br(),
               tags$strong("Requirements:"),
               tags$ul(
                 tags$li("Maximum file size: 50 MB"),
                 tags$li("Maximum rows: 1,000,000"),
                 tags$li("Maximum columns: 1,000"),
                 tags$li("Column names must start with a letter")
               )
             ),
             hr(),
             uiOutput("data_preview_ui")
           )
         )
       ),
       
       # =====================================================================
       # TAB 2: DATA PROFILE
       # =====================================================================
       shinydashboard::tabItem(
         tabName = "profile",
         fluidRow(
           shinydashboard::valueBoxOutput("n_rows_box"),
           shinydashboard::valueBoxOutput("n_cols_box"),
           shinydashboard::valueBoxOutput("missing_box")
         ),
         fluidRow(
           shinydashboard::box(
             title = "Data Quality Issues",
             width = 12,
             div(class = "explanation-box info",
                icon("info-circle"), " ",
                strong("Understanding Issue Types:"),
                tags$ul(
                  tags$li(strong("High Missing: "), "Columns with >50% missing values. Consider removing these columns or using specialized imputation methods."),
                  tags$li(strong("High Cardinality: "), "Categorical columns with many unique values (>10). May need special encoding like target encoding or grouping rare categories."),
                  tags$li(strong("Near Zero Variance: "), "Columns with very little variation. These provide minimal predictive value and can be removed."),
                  tags$li(strong("Encoding: "), "Converting categorical (text) variables into numbers that models can use. Common methods include dummy/one-hot encoding (creates binary columns) or label encoding (assigns numbers to categories).")
                )
            ),
            DT::DTOutput("issues_table")
           )
         ),
         fluidRow(
           shinydashboard::box(
             title = "Column Types",
             width = 6,
             DT::DTOutput("col_types_table")
           ),
           shinydashboard::box(
             title = "Numeric Summary",
             width = 6,
             DT::DTOutput("numeric_summary_table")
           )
         )
       ),
       
       # =====================================================================
       # TAB 3: TASK & METHODS
       # =====================================================================
       shinydashboard::tabItem(
         tabName = "methods",
         fluidRow(
           shinydashboard::box(
             title = "Select Target Variable",
             width = 12,
             selectInput("target_col", "Target Variable (what you want to predict):", choices = NULL),
             div(class = "explanation-box",
                 p(strong("What is a target variable?")),
                 p("The target is the column you want the model to predict. For example:"),
                 tags$ul(
                   tags$li("Predicting house prices -> target is 'price'"),
                   tags$li("Classifying emails -> target is 'spam' (yes/no)"),
                   tags$li("Predicting customer churn -> target is 'churned'")
                 )
             ),
             actionButton("detect_task", "Detect ML Task", 
                          class = "btn-primary btn-lg")
           )
         ),
         fluidRow(
           shinydashboard::valueBoxOutput("task_type_box"),
           shinydashboard::valueBoxOutput("task_subtype_box"),
           shinydashboard::valueBoxOutput("n_predictors_box")
         ),
         fluidRow(
           shinydashboard::box(
             title = "Recommended Methods",
             width = 12,
             div(class = "explanation-box",
                 p(strong("How to choose a method:")),
                 p("Priority 1 methods are usually best to start with. Select one to see sample code and proceed to training.")
             ),
             DT::DTOutput("methods_table"),
             hr(),
             div(class = "code-container",
              tags$button(
                id = "copy_code_btn",
                class = "btn btn-primary btn-sm copy-code-btn",
                onclick = "copyCodeToClipboard()",
                icon("copy"), " Copy Code"
              ),
              verbatimTextOutput("method_code")
            )
           )
         )
       ),
       
       # =====================================================================
       # TAB 4: DATA CLEANING
       # =====================================================================
       shinydashboard::tabItem(
         tabName = "cleaning",
         fluidRow(
           column(12,
             div(class = "explanation-box warning",
                 icon("exclamation-triangle"), " ",
                 strong("IMPORTANT - Data Leakage Prevention:"),
                 br(), br(),
                 "This cleaning tab is for ", strong("EXPLORATION ONLY"), ". ",
                 "When you train models, the actual preprocessing happens through a 'recipe' that is ",
                 strong("fitted ONLY on training data"), " to prevent data leakage.",
                 br(), br(),
                 "If you apply cleaning here, you'll see a preview of the cleaned data, but the model ",
                 "training will still use the correct, leakage-free preprocessing workflow. ",
                 "You can safely skip this tab and go directly to training.",
                 br(), br(),
                 tags$small(
                   icon("graduation-cap"), " ",
                   strong("Why this matters:"), " If preprocessing uses information from test data ",
                   "(e.g., medians computed from the full dataset), your performance metrics will be ",
                   "optimistically biased and won't reflect real-world performance."
                 )
             )
           )
         ),
         fluidRow(
           shinydashboard::box(
             title = "Data Cleaning Options",
             width = 8,
             status = "primary",
             solidHeader = TRUE,

             h4("Missing Data Handling"),
             div(class = "cleaning-option",
                 radioButtons("missing_strategy", "How to handle missing values:",
                              choices = c(
                                "Impute with median (numeric) / mode (categorical)" = "impute",
                                "Remove rows with any missing values" = "remove_rows",
                                "Remove columns with >50% missing" = "remove_cols",
                                "Keep as-is (some models handle missing data)" = "keep"
                              ),
                              selected = "impute"),
                 br(),
                 div(class = "explanation-box",
                     strong("When to use each option:"), br(),
                     strong("Impute"), " (Recommended): Replaces missing values with reasonable estimates. ",
                     "Best when you have <20% missing data randomly distributed.", br(),
                     strong("Remove rows"), ": Only use if very few rows have missing data (<5% of dataset). ",
                     "Wastes less data than removing columns.", br(),
                     strong("Remove columns"), ": Use when specific columns have excessive missing data (>50%). ",
                     "Better than losing many rows. Target column is always preserved.", br(),
                     strong("Keep as-is"), ": Only for tree-based models (Random Forest, XGBoost) that handle missing data natively. ",
                     "Most other models will fail."
                 )
             ),
             
             h4("Categorical Variable Encoding"),
             div(class = "cleaning-option",
                 radioButtons("encoding_strategy", "How to encode categorical variables:",
                              choices = c(
                                "Dummy variables (one-hot encoding)" = "dummy",
                                "Label encoding (convert to numbers)" = "label",
                                "Keep as-is (for tree-based models)" = "keep_cat"
                              ),
                              selected = "dummy"),
                 br(),
                 div(class = "explanation-box",
                     strong("Note:"), " These options show what the model recipe will apply during training.", br(),
                     strong("Dummy encoding"), " (Recommended): Creates binary (0/1) columns for each category. ",
                     "Required for: Linear/Logistic Regression, SVM, Neural Networks. Safe for all models.", br(),
                     strong("Label encoding"), ": Assigns numbers to categories (Cat=1, Dog=2, etc.). ",
                     "WARNING: Implies ordering that may not exist. Only use for ordinal data (Small < Medium < Large).", br(),
                     strong("Keep as-is"), ": No encoding applied. ",
                     "Only works with: Random Forest, XGBoost, Decision Trees. Other models will fail."
                 )
             ),
             
             h4("Numeric Scaling"),
             div(class = "cleaning-option",
                 radioButtons("scaling_strategy", "How to scale numeric variables:",
                              choices = c(
                                "Standardize (mean=0, sd=1)" = "normalize",
                                "Min-Max scaling (0 to 1)" = "minmax",
                                "No scaling" = "none"
                              ),
                              selected = "normalize"),
                 br(),
                 div(class = "explanation-box",
                     strong("Note:"), " These options show what the model recipe will apply.", br(),
                     strong("Standardize"), " (Recommended): Centers data at 0 and scales by standard deviation. ",
                     "Required for: SVM, KNN, Neural Networks, Lasso/Ridge. Handles outliers better than Min-Max.", br(),
                     strong("Min-Max"), ": Scales all values to 0-1 range. ",
                     "Good when you need bounded values, but sensitive to outliers (one extreme value affects everything).", br(),
                     strong("No scaling"), ": Keep original values. ",
                     "Only for: Tree-based models (Random Forest, XGBoost, Decision Tree) that don't care about scale. ",
                     "Will harm performance of distance-based models (SVM, KNN)."
                 )
             ),
             
             h4("Outlier Handling"),
             div(class = "cleaning-option",
                 radioButtons("outlier_strategy", "How to handle outliers:",
                              choices = c(
                                "Keep all data" = "keep_outliers",
                                "Cap at 1.5 x IQR (recommended for clean data)" = "cap_iqr"
                              ),
                              selected = "keep_outliers"),
                 br(),
                 div(class = "explanation-box",
                     strong("When to use each option:"), br(),
                     strong("Keep all data"), " (Default): Preserves all information. ",
                     "Best when: outliers are real/meaningful (e.g., CEO salary in salary data), ",
                     "using tree-based models (Random Forest, XGBoost) which handle outliers well, ",
                     "or you want to detect anomalies.", br(),
                     strong("Cap at 1.5 x IQR"), ": Limits extreme values to reasonable bounds. ",
                     "Best when: outliers are likely errors/noise, using linear models or SVM sensitive to extremes, ",
                     "or improving model generalization. IQR (Interquartile Range) method is robust - ",
                     "unlike mean/SD methods that can be skewed by the outliers themselves."
                 )
             ),

             h4("Class Balance (Classification Only)"),
             div(class = "cleaning-option",
                 radioButtons("balance_strategy", "Handle class imbalance:",
                              choices = c(
                                "No balancing (use original distribution)" = "none",
                                "Oversample minority classes (duplicate rare cases)" = "oversample",
                                "Undersample majority class (remove common cases)" = "undersample"
                              ),
                              selected = "none"),
                 br(),
                 div(class = "explanation-box",
                     strong("When to balance classes:"), br(),
                     strong("No balancing"), " (Default): Use when classes are reasonably balanced (30-70% split) or when class proportions reflect reality.", br(),
                     strong("Oversample"), ": Duplicates minority class examples. Best when you have limited data and can't afford to lose any. ",
                     "Warning: Can lead to overfitting on minority class patterns.", br(),
                     strong("Undersample"), ": Randomly removes majority class examples to match minority size. ",
                     "Best when you have abundant data. Warning: Wastes data - only use if you have >1000 observations per class.", br(),
                     strong("When to use:"), " If one class is <20% of data (e.g., fraud detection, rare disease diagnosis), balancing often improves model performance."
                 )
             ),

             hr(),
             actionButton("apply_cleaning", "Apply Cleaning & Preview",
                          class = "btn-success btn-lg")
           ),
           
           shinydashboard::box(
             title = "Cleaning Summary",
             width = 4,
             status = "info",
             solidHeader = TRUE,
             
             uiOutput("cleaning_summary"),
             hr(),
             h5("Generated Recipe Code:"),
             verbatimTextOutput("cleaning_recipe_code")
           )
         ),
         
         fluidRow(
           shinydashboard::box(
             title = "Data Preview: Before vs After Cleaning",
             width = 12,
             status = "success",
             solidHeader = TRUE,
             collapsible = TRUE,
             
             fluidRow(
               column(6,
                      h4("Original Data"),
                      DT::DTOutput("original_data_preview")
               ),
               column(6,
                      h4("Cleaned Data"),
                      DT::DTOutput("cleaned_data_preview")
               )
             ),
             hr(),
             downloadButton("download_cleaned", "Download Cleaned Data", class = "btn-primary")
           )
         )
       ),
       
       # =====================================================================
       # TAB 5: MODEL TRAINING (UPDATED)
       # =====================================================================
       shinydashboard::tabItem(
         tabName = "training",
         fluidRow(
           shinydashboard::box(
             title = "Select Models to Train",
             width = 6,
             status = "primary",
             solidHeader = TRUE,
             
             div(class = "explanation-box info",
                 icon("lightbulb"), " ",
                 strong("Tip: "), "Select multiple models to compare their performance. ",
                 "Training takes longer with more models, but helps you find the best approach."
             ),
             
             # Changed from selectInput to checkboxGroupInput
             div(class = "model-checkbox-group",
                 uiOutput("model_checkboxes")
             ),
             
             hr(),
             
             h4("Training Options"),
             
             fluidRow(
               column(6,
                 sliderInput("cv_folds", "Cross-Validation Folds:",
                             min = 3, max = 10, value = 5, step = 1),
                 div(class = "explanation-box",
                     tags$small(
                       strong("What is this?"), " CV splits your training data into parts, ",
                       "trains on some, tests on others. More folds = more reliable estimate, but slower."
                     )
                 )
               ),
               column(6,
                 sliderInput("tune_grid", "Hyperparameter Combinations:",
                             min = 5, max = 25, value = 10, step = 5),
                 div(class = "explanation-box",
                     tags$small(
                       strong("What is this?"), " Models have settings (hyperparameters) that affect performance. ",
                       "This controls how many combinations to try. More = better tuning, but slower."
                     )
                 )
               )
             ),
             
             hr(),
             
             fluidRow(
               column(6,
                 actionButton("train_selected_btn", "Train Selected Models", 
                              class = "btn-success btn-lg btn-block",
                              icon = icon("play"))
               ),
               column(6,
                 actionButton("train_all_btn", "Train All Recommended", 
                              class = "btn-info btn-lg btn-block",
                              icon = icon("layer-group"))
               )
             )
           ),
           
           shinydashboard::box(
             title = "Training Status & Info",
             width = 6,
             status = "info",
             solidHeader = TRUE,
             
             uiOutput("training_status"),
             
             hr(),
             
             uiOutput("selected_models_info"),
             
             hr(),
             
             h4("What Happens During Training"),
             div(class = "explanation-box",
               tags$ol(
                 tags$li(strong("Data Split: "), "Data is divided into training (75%) and test (25%) sets"),
                 tags$li(strong("Cross-Validation: "), "Training data is further split to estimate real-world performance"),
                 tags$li(strong("Hyperparameter Tuning: "), "Model settings are optimized for best performance"),
                 tags$li(strong("Final Training: "), "Best model is trained on all training data"),
                 tags$li(strong("Evaluation: "), "Model is tested on held-out test data for unbiased metrics")
               )
             ),
             
             h4("About Training Warnings"),
             div(class = "explanation-box warning",
               p(icon("exclamation-triangle"), " ", 
                 strong("'Issues with computations' warnings are normal!"), 
                 " They typically occur when:"),
               tags$ul(
                 tags$li("Some CV folds have too few samples of a class"),
                 tags$li("A hyperparameter combination doesn't work well"),
                 tags$li("The model can't calculate certain metrics for some folds")
               ),
               p("The training still produces valid results - these warnings just mean some ",
                 "intermediate calculations were skipped. Your final metrics are still reliable.")
             )
           )
         ),
         
         fluidRow(
           shinydashboard::box(
             title = "Training Progress",
             width = 12,
             collapsible = TRUE,
             
             uiOutput("training_progress_ui")
           )
         )
       ),
       
       # =====================================================================
       # TAB 6: RESULTS & EXPLANATION (ENHANCED)
       # =====================================================================
       shinydashboard::tabItem(
         tabName = "results",
         
         # Show comparison or single model based on what was trained
         uiOutput("results_mode_ui"),
         
         # Metrics Cards Row
         fluidRow(
           column(12,
                  uiOutput("results_metrics_cards")
           )
         ),
         
         # Comparison table (if multiple models)
         fluidRow(
           uiOutput("model_comparison_ui")
         ),
         
         # Main Results
         fluidRow(
           shinydashboard::box(
             title = "Understanding Your Results",
             width = 12,
             status = "success",
             solidHeader = TRUE,
             collapsible = TRUE,
             
             uiOutput("results_explanation")
           )
         ),
         
         # Visualizations with explanations
         fluidRow(
           shinydashboard::box(
             title = "Feature Importance",
             width = 6,
             status = "primary",
             solidHeader = TRUE,
             
             plotOutput("importance_plot", height = "350px"),
             div(class = "plot-explanation",
                 icon("info-circle"), " ",
                 strong("How to read this: "), 
                 "Bars show which variables most influence predictions. ",
                 "Longer bars = more important. Focus data quality efforts on top variables."
             )
           ),
           
           shinydashboard::box(
             title = "Prediction Analysis",
             width = 6,
             status = "info",
             solidHeader = TRUE,
             
             plotOutput("prediction_plot", height = "350px"),
             div(class = "plot-explanation",
                 uiOutput("prediction_plot_explanation")
             )
           )
         ),
         
         fluidRow(
           shinydashboard::box(
             title = "Model Diagnostics",
             width = 6,
             status = "warning",
             solidHeader = TRUE,
             
             plotOutput("diagnostic_plot", height = "350px"),
             div(class = "plot-explanation",
                 uiOutput("diagnostic_plot_explanation")
             )
           ),
           
           shinydashboard::box(
             title = "Metrics Glossary",
             width = 6,
             status = "info",
             solidHeader = TRUE,
             collapsible = TRUE,
             collapsed = TRUE,
             
             uiOutput("metrics_glossary")
           )
         ),
         
         # Detailed Results
         fluidRow(
           shinydashboard::box(
             title = "Detailed Predictions (First 500 rows)",
             width = 12,
             collapsible = TRUE,
             collapsed = TRUE,
             
             DT::DTOutput("predictions_table"),
             hr(),
             downloadButton("download_predictions", "Download All Predictions", class = "btn-primary")
           )
         ),
         
         # Export Options
         fluidRow(
           shinydashboard::box(
             title = "Export & Next Steps",
             width = 12,
             
             fluidRow(
               column(4,
                      h4("Download Results"),
                      downloadButton("download_report", "Download Full Report (HTML)", class = "btn-success btn-block"),
                      br(), br(),
                      downloadButton("download_model", "Download Model Object (.rds)", class = "btn-info btn-block"),
                      br(),
                      tags$small(class = "text-muted", 
                                 "The .rds file can be loaded in R with readRDS() to make new predictions")
               ),
               column(8,
                      h4("Recommendations & Next Steps"),
                      uiOutput("next_steps_recommendations")
               )
             )
           )
         )
       ),

       # Validation tab
       shinydashboard::tabItem(
         tabName = "validate",
         fluidRow(
           shinydashboard::box(
             width = 12,
             title = "Validate Model on New Data",
             status = "primary",
             solidHeader = TRUE,

             div(class = "explanation-box",
                 p(strong("Purpose:"), " Test your trained model on completely new data to verify it generalizes well."),
                 p(strong("Requirements:"), " import a data file with the same columns as your training data (including the target variable)."),
                 p(strong("Supported formats:"), " CSV, TSV, TXT, Excel, R data, SAS"),
                 p(strong("What you'll see:"), " Performance metrics on the new data compared to your original test set.")
             ),

             br(),

             fluidRow(
               column(4,
                      fileInput("validation_file", "import Validation Dataset",
                               accept = c("text/csv", "text/tab-separated-values", "text/plain",
                                         "application/vnd.ms-excel", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                         ".csv", ".tsv", ".txt", ".xlsx", ".xls", ".rds", ".RData", ".rda", ".sas7bdat", ".xpt")),
                      actionButton("run_validation", "Run Validation",
                                  class = "btn-primary btn-block",
                                  icon = icon("play"))
               ),
               column(8,
                      uiOutput("validation_status")
               )
             )
           )
         ),

         fluidRow(
           shinydashboard::box(
             width = 12,
             title = "Validation Results",
             status = "success",
             solidHeader = TRUE,
             uiOutput("validation_results")
           )
         )
       )
     )
   )
 )
}
