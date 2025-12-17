#' LearnTidyML Server
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @import shiny
#' @export
app_server <- function(input, output, session) {

 # =========================================================================
 # REACTIVE VALUES - Application State
 # =========================================================================
 
 rv <- reactiveValues(
   data = NULL,
   profile = NULL,
   issues = NULL,
   task_info = NULL,
   methods = NULL,
   cleaned_data = NULL,
   cleaning_recipe = NULL,
   prep_recipe = NULL,             # Prepared recipe for validation preprocessing
   model_results = NULL,           # Single model or best model from comparison
   all_model_results = NULL,       # All models when comparing
   comparison_results = NULL,      # Comparison summary
   validation_results = NULL,      # Validation dataset results
   last_detection = NULL,
   workflow_step = 1,
   training_log = character(),
   is_comparison_mode = FALSE
 )

 # Session cleanup handler
 session$onSessionEnded(function() {
   rv$data <- NULL
   rv$profile <- NULL
   rv$cleaned_data <- NULL
   rv$model_results <- NULL
   rv$all_model_results <- NULL
   gc()
   # Hash session token for security - never log raw tokens
   session_id <- hash_session_token(session$token)
   message(paste("Session ended:", session_id, "at", Sys.time()))

   # Stop the app when browser closes
   shiny::stopApp()
 })

 # Hash session token for security - never log raw tokens
 session_id <- hash_session_token(session$token)
 message(paste("Session started:", session_id, "at", Sys.time()))

 # =========================================================================
 # WORKFLOW PROGRESS INDICATOR
 # =========================================================================
 
 output$workflow_progress <- renderUI({
   steps <- c("Import", "Profile", "Methods", "Clean", "Train", "Results")
   current <- rv$workflow_step
   
   tagList(
     lapply(seq_along(steps), function(i) {
       icon_class <- if (i < current) "step-complete" 
                    else if (i == current) "step-current"
                    else "step-pending"
       icon_name <- if (i < current) "check-circle" 
                   else if (i == current) "arrow-right"
                   else "circle"
       
       div(
         class = icon_class,
         steps[i]
       )
     })
   )
 })

 # =========================================================================
 # TAB 1: Import DATA
 # =========================================================================

 observeEvent(input$file, {
   req(input$file)

   message(paste("File import attempt:", input$file$name))

   if(!is.null(rv$data)) {
     rv$data <- NULL
     rv$profile <- NULL
     rv$issues <- NULL
     rv$cleaned_data <- NULL
     rv$model_results <- NULL
     rv$all_model_results <- NULL
     rv$comparison_results <- NULL
     gc()
   }

   rv$data <- tryCatch({
     validate_imported_file(input$file, max_size_mb = 50)

     data <- with_timeout({
       read_data_file(input$file$datapath, input$file$name)
     }, timeout = 60)

     validate_data_dimensions(data, max_rows = 1000000, max_cols = 1000, max_cells = 10000000)
     validate_column_names(data)
     data
     
   }, error = function(e) {
     msg <- safe_error_message(e, "Unable to process file.")
     showNotification(msg, type = "error", duration = 10)
     NULL
   })

   if(is.null(rv$data)) return(NULL)

   rv$profile <- tryCatch({
     with_timeout({ analyze_data(rv$data) }, timeout = 60)
   }, error = function(e) {
     showNotification(safe_error_message(e, "Error analyzing data."), type = "error")
     NULL
   })

   rv$issues <- tryCatch({
     if(!is.null(rv$profile)) identify_issues(rv$data, rv$profile)
     else tibble::tibble()
   }, error = function(e) {
     tibble::tibble()
   })

   updateSelectInput(session, "target_col", choices = names(rv$data))
   rv$workflow_step <- 2
   
   showNotification(
     paste("Data loaded:", nrow(rv$data), "rows,", ncol(rv$data), "columns"),
     type = "message"
   )
 })

 output$data_preview_ui <- renderUI({
   req(rv$data)
   tagList(
     h4("Data Preview (first 100 rows)"),
     DT::DTOutput("data_preview")
   )
 })

 output$data_preview <- DT::renderDT({
   req(rv$data)
   DT::datatable(head(rv$data, 100), options = list(scrollX = TRUE, pageLength = 10), escape = TRUE)
 })

 # =========================================================================
 # TAB 2: DATA PROFILE
 # =========================================================================

 output$n_rows_box <- shinydashboard::renderValueBox({
   req(rv$profile)
   shinydashboard::valueBox(
     format(rv$profile$structure$n_rows, big.mark = ","), "Rows",
     icon = icon("table"), color = "blue"
   )
 })

 output$n_cols_box <- shinydashboard::renderValueBox({
   req(rv$profile)
   shinydashboard::valueBox(rv$profile$structure$n_cols, "Columns",
     icon = icon("columns"), color = "green"
   )
 })

 output$missing_box <- shinydashboard::renderValueBox({
   req(rv$profile)
   avg_missing <- mean(rv$profile$missing$pct_missing)
   shinydashboard::valueBox(
     paste0(round(avg_missing, 1), "%"), "Avg Missing",
     icon = icon("question-circle"),
     color = if(avg_missing > 20) "red" else "yellow"
   )
 })

 output$issues_table <- DT::renderDT({
   req(rv$issues)
   DT::datatable(rv$issues, options = list(pageLength = 10), rownames = FALSE, escape = TRUE)
 })

 output$col_types_table <- DT::renderDT({
   req(rv$profile)
   DT::datatable(rv$profile$col_types, options = list(pageLength = 10), rownames = FALSE, escape = TRUE)
 })

 output$numeric_summary_table <- DT::renderDT({
   req(rv$profile)
   if(nrow(rv$profile$numeric_summary) > 0) {
     DT::datatable(
       rv$profile$numeric_summary %>% dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 3))),
       options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE, escape = TRUE
     )
   }
 })

 # =========================================================================
 # TAB 3: TASK & METHODS
 # =========================================================================

 observeEvent(input$detect_task, {
   req(rv$data, input$target_col)

   current_time <- Sys.time()
   if(!is.null(rv$last_detection)) {
     if(as.numeric(difftime(current_time, rv$last_detection, units = "secs")) < 2) {
       showNotification("Please wait before detecting again", type = "warning")
       return(NULL)
     }
   }
   rv$last_detection <- current_time

   if(!input$target_col %in% names(rv$data)) {
     showNotification("Please select a valid target column", type = "error")
     return(NULL)
   }

   rv$task_info <- tryCatch({
     detect_ml_task(rv$data, input$target_col)
   }, error = function(e) {
     showNotification(safe_error_message(e, "Unable to detect ML task."), type = "error")
     NULL
   })

   if(is.null(rv$task_info)) return(NULL)

   rv$methods <- tryCatch({
     recommend_methods(rv$task_info, rv$profile)
   }, error = function(e) {
     showNotification(safe_error_message(e, "Unable to generate recommendations."), type = "error")
     NULL
   })
   
   rv$workflow_step <- 3
 })

 output$task_type_box <- shinydashboard::renderValueBox({
   req(rv$task_info)
   shinydashboard::valueBox(rv$task_info$task_type, "ML Task",
     icon = icon("bullseye"), color = "purple"
   )
 })

 output$task_subtype_box <- shinydashboard::renderValueBox({
   req(rv$task_info)
   shinydashboard::valueBox(rv$task_info$task_subtype, "Task Subtype",
     icon = icon("tag"), color = "orange"
   )
 })

 output$n_predictors_box <- shinydashboard::renderValueBox({
   req(rv$task_info)
   shinydashboard::valueBox(rv$task_info$n_predictors, "Predictors",
     icon = icon("layer-group"), color = "teal"
   )
 })

 output$methods_table <- DT::renderDT({
   req(rv$methods)
   DT::datatable(
     rv$methods %>% dplyr::select(-tidymodels_engine),
     options = list(pageLength = 5), rownames = FALSE, selection = "single", escape = TRUE
   )
 })

 output$method_code <- renderText({
   req(rv$methods, rv$task_info, rv$data)
   selected <- input$methods_table_rows_selected
   if(length(selected) > 0) {
     tryCatch({
       method <- rv$methods[selected, ]
       generate_tidymodels_code(method, rv$task_info, rv$data, rv$profile)
     }, error = function(e) {
       safe_error_message(e, "Unable to generate code.")
     })
   } else {
     "Select a method above to see example tidymodels code"
   }
 })

 # =========================================================================
 # TAB 4: DATA CLEANING
 # =========================================================================

 output$cleaning_summary <- renderUI({
   req(rv$data, input$target_col)
   
   n_missing <- sum(is.na(rv$data))
   n_numeric <- sum(sapply(rv$data, is.numeric))
   n_categorical <- ncol(rv$data) - n_numeric
   
   tagList(
     h5("Current Data:"),
     p(strong("Rows: "), nrow(rv$data)),
     p(strong("Columns: "), ncol(rv$data)),
     p(strong("Missing Values: "), format(n_missing, big.mark = ",")),
     p(strong("Numeric Columns: "), n_numeric),
     p(strong("Categorical Columns: "), n_categorical),
     hr(),
     h5("Selected Options:"),
     p(strong("Missing: "), input$missing_strategy),
     p(strong("Encoding: "), input$encoding_strategy),
     p(strong("Scaling: "), input$scaling_strategy),
     p(strong("Outliers: "), input$outlier_strategy)
   )
 })

 output$cleaning_recipe_code <- renderText({
   req(input$target_col)

   code <- paste0("recipe(", input$target_col, " ~ ., data = train_data)")

   # Show steps in CORRECT order to prevent warnings
   if (input$missing_strategy == "impute") {
     code <- paste0(code, " %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors())")
   } else if (input$missing_strategy == "remove_rows") {
     code <- paste0(code, " %>%
  step_naomit(everything())")
   } else if (input$missing_strategy == "remove_cols") {
     code <- paste0(code, " %>%
  # Remove columns with >50% missing
  step_filter_missing(all_predictors(), threshold = 0.5)")
   }
   # If "keep", no missing data handling step is added

   if (input$encoding_strategy == "dummy") {
     code <- paste0(code, " %>%
  step_dummy(all_nominal_predictors())")
   } else if (input$encoding_strategy == "label") {
     code <- paste0(code, " %>%
  step_integer(all_nominal_predictors())")
   }
   # If "keep_cat", no encoding step is added

   # Remove zero variance BEFORE scaling (correct order)
   code <- paste0(code, " %>%
  step_zv(all_predictors())")

   if (input$scaling_strategy == "normalize") {
     code <- paste0(code, " %>%
  step_normalize(all_numeric_predictors())")
   } else if (input$scaling_strategy == "minmax") {
     code <- paste0(code, " %>%
  step_range(all_numeric_predictors())")
   }
   # If "none", no scaling step is added

   # Note: Outlier capping is typically done outside recipe or with custom step
   if (input$outlier_strategy == "cap_iqr") {
     code <- paste0(code, "

# Note: Outlier capping (IQR method) is applied separately in data preview
# For production, consider step_mutate() with custom capping logic")
   }

   code
 })

 observeEvent(input$apply_cleaning, {
   req(rv$data, input$target_col)
   
   withProgress(message = "Applying data cleaning...", {
     
     cleaned <- rv$data
     
     incProgress(0.2, detail = "Handling missing values...")
     
     if (input$missing_strategy == "impute") {
       numeric_cols <- names(cleaned)[sapply(cleaned, is.numeric)]
       for (col in numeric_cols) {
         if (any(is.na(cleaned[[col]]))) {
           cleaned[[col]][is.na(cleaned[[col]])] <- median(cleaned[[col]], na.rm = TRUE)
         }
       }
       cat_cols <- names(cleaned)[sapply(cleaned, function(x) is.character(x) | is.factor(x))]
       for (col in cat_cols) {
         if (any(is.na(cleaned[[col]]))) {
           mode_val <- names(sort(table(cleaned[[col]]), decreasing = TRUE))[1]
           cleaned[[col]][is.na(cleaned[[col]])] <- mode_val
         }
       }
     } else if (input$missing_strategy == "remove_rows") {
       cleaned <- cleaned[complete.cases(cleaned), ]
     } else if (input$missing_strategy == "remove_cols") {
       missing_pct <- sapply(cleaned, function(x) sum(is.na(x)) / length(x))
       # Ensure target column is NOT removed
       cols_to_keep <- (missing_pct <= 0.5) | (names(cleaned) == input$target_col)
       cleaned <- cleaned[, cols_to_keep]
     }
     
     incProgress(0.2, detail = "Handling outliers...")

     if (input$outlier_strategy == "cap_iqr") {
       numeric_cols <- names(cleaned)[sapply(cleaned, is.numeric)]
       numeric_cols <- setdiff(numeric_cols, input$target_col)
       for (col in numeric_cols) {
         q1 <- quantile(cleaned[[col]], 0.25, na.rm = TRUE)
         q3 <- quantile(cleaned[[col]], 0.75, na.rm = TRUE)
         iqr <- q3 - q1
         lower <- q1 - 1.5 * iqr
         upper <- q3 + 1.5 * iqr
         cleaned[[col]] <- pmax(pmin(cleaned[[col]], upper), lower)
       }
     }
     
     incProgress(0.2, detail = "Handling class imbalance...")

     # Apply class balancing if classification task and requested
     if (rv$task_info$task_type == "classification" && input$balance_strategy != "none") {
       target_col <- input$target_col
       class_counts <- table(cleaned[[target_col]])

       if (input$balance_strategy == "oversample") {
         # Oversample minority classes to match largest class
         max_count <- max(class_counts)
         balanced_data <- list()

         for (class_name in names(class_counts)) {
           class_data <- cleaned[cleaned[[target_col]] == class_name, ]
           current_count <- nrow(class_data)

           if (current_count < max_count) {
             # Oversample by sampling with replacement
             extra_rows <- sample(1:current_count, max_count - current_count, replace = TRUE)
             class_data <- rbind(class_data, class_data[extra_rows, ])
           }

           balanced_data[[class_name]] <- class_data
         }

         cleaned <- do.call(rbind, balanced_data)

       } else if (input$balance_strategy == "undersample") {
         # Undersample majority classes to match smallest class
         min_count <- min(class_counts)
         balanced_data <- list()

         for (class_name in names(class_counts)) {
           class_data <- cleaned[cleaned[[target_col]] == class_name, ]

           if (nrow(class_data) > min_count) {
             # Randomly sample to match minority class size
             class_data <- class_data[sample(1:nrow(class_data), min_count), ]
           }

           balanced_data[[class_name]] <- class_data
         }

         cleaned <- do.call(rbind, balanced_data)
       }

       # Shuffle the balanced data
       cleaned <- cleaned[sample(1:nrow(cleaned)), ]
     }

     incProgress(0.2, detail = "Finalizing...")

     # Validate cleaned data before saving
     if (!input$target_col %in% names(cleaned)) {
       showNotification(
         paste("Error: Target column '", input$target_col, "' was removed during cleaning. Please adjust your cleaning options."),
         type = "error", duration = 10
       )
       return(NULL)
     }

     if (nrow(cleaned) < 10) {
       showNotification(
         paste("Error: Only", nrow(cleaned), "rows remain after cleaning. Need at least 10 rows. Please adjust your cleaning options."),
         type = "error", duration = 10
       )
       return(NULL)
     }

     if (ncol(cleaned) < 2) {
       showNotification(
         "Error: Not enough columns remain after cleaning. Need at least the target and one predictor.",
         type = "error", duration = 10
       )
       return(NULL)
     }

     rv$cleaned_data <- cleaned
     rv$workflow_step <- 4
   })
   
   showNotification(
     paste("Cleaning complete:", nrow(rv$cleaned_data), "rows remaining"),
     type = "message"
   )
 })

 output$original_data_preview <- DT::renderDT({
   req(rv$data)
   DT::datatable(head(rv$data, 50), options = list(scrollX = TRUE, pageLength = 5), escape = TRUE)
 })

 output$cleaned_data_preview <- DT::renderDT({
   req(rv$cleaned_data)
   DT::datatable(head(rv$cleaned_data, 50), options = list(scrollX = TRUE, pageLength = 5), escape = TRUE)
 })

 output$download_cleaned <- downloadHandler(
   filename = function() { paste0("cleaned_data_", Sys.Date(), ".csv") },
   content = function(file) {
     req(rv$cleaned_data)
     # Sanitize data to prevent CSV formula injection attacks
     safe_data <- sanitize_csv_data(rv$cleaned_data)
     readr::write_csv(safe_data, file)
   }
 )

 # =========================================================================
 # TAB 5: MODEL TRAINING (UPDATED)
 # =========================================================================

 # Dynamic checkbox UI for model selection
 output$model_checkboxes <- renderUI({
   req(rv$methods)
   
   # Create choices with descriptions
   choices <- setNames(
     rv$methods$tidymodels_engine,
     paste0(rv$methods$method, " (Priority: ", rv$methods$priority, ")")
   )
   
   # Method descriptions
   descriptions <- list(
     glmnet = "Best for: Many variables, interpretable coefficients. Adds penalty to prevent overfitting.",
     ranger = "Best for: Complex patterns, robust to outliers. Builds many trees and averages results.",
     xgboost = "Best for: Maximum accuracy, competition winning. Builds trees that correct each other's mistakes.",
     kernlab = "Best for: Complex boundaries between classes. Finds optimal separating surface.",
     kknn = "Best for: Local patterns, simple concept. Predicts based on similar observations.",
     rpart = "Best for: Interpretability, if-then rules. Creates decision flowchart.",
     naivebayes = "Best for: Text classification, fast training. Uses probability with independence assumption."
   )
   
   tagList(
     h4("Select Models to Compare:"),
     checkboxGroupInput(
       "selected_models",
       label = NULL,
       choices = choices,
       selected = choices[1:min(3, length(choices))]  # Default select top 3
     ),
     hr(),
     h5("Model Descriptions:"),
     lapply(rv$methods$tidymodels_engine, function(eng) {
       desc <- descriptions[[eng]]
       if (!is.null(desc)) {
         div(class = "explanation-box",
             strong(get_method_display_name(eng)), ": ", desc
         )
       }
     })
   )
 })

 # Show info about selected models
 output$selected_models_info <- renderUI({
   selected <- input$selected_models
   
   if (is.null(selected) || length(selected) == 0) {
     return(div(class = "explanation-box warning",
                "No models selected. Please select at least one model to train."))
   }
   
   n_models <- length(selected)
   est_time <- n_models * 30  # rough estimate in seconds
   
   tagList(
     h4(n_models, " model(s) selected"),
     p("Selected: ", paste(sapply(selected, get_method_display_name), collapse = ", ")),
     p("Estimated time: ", 
       if(est_time < 60) paste(est_time, "seconds") 
       else paste(round(est_time/60, 1), "minutes")),
     if (n_models > 1) {
       div(class = "explanation-box success",
           "Multiple models selected. ",
           "Results tab will show a comparison of all models.")
     }
   )
 })

 output$training_status <- renderUI({
   if (is.null(rv$model_results) && is.null(rv$all_model_results)) {
     div(class = "training-status",
         "Ready to train. Select model(s) and click a Train button."
     )
   } else if (rv$is_comparison_mode && !is.null(rv$comparison_results)) {
     div(class = "training-status complete",
         "Comparison complete! ", 
         strong(length(rv$all_model_results)), " models trained. ",
         "Best model: ", strong(get_method_display_name(rv$comparison_results$best_model)),
         br(), br(),
         actionButton("go_to_results", "View Results", class = "btn-success")
     )
   } else if (!is.null(rv$model_results)) {
     div(class = "training-status complete",
         "Training complete! ",
         "Model: ", strong(get_method_display_name(rv$model_results$method)),
         br(), br(),
         actionButton("go_to_results", "View Results", class = "btn-success")
     )
   }
 })

 output$training_progress_ui <- renderUI({
   if (length(rv$training_log) > 0) {
     tagList(
       h5("Training Log:"),
       tags$pre(paste(rv$training_log, collapse = "
"))
     )
   } else {
     p("Training log will appear here when you start training.")
   }
 })

 # Navigate to results
 observeEvent(input$go_to_results, {
   updateTabItems(session, "sidebar_menu", "results")
 })

 # Train selected models
 observeEvent(input$train_selected_btn, {
   req(rv$data, input$target_col, rv$task_info, input$selected_models)
   
   selected_engines <- input$selected_models
   
   if (length(selected_engines) == 0) {
     showNotification("Please select at least one model", type = "error")
     return()
   }
   
   train_data <- if (!is.null(rv$cleaned_data)) rv$cleaned_data else rv$data
   rv$training_log <- character()
   
   if (length(selected_engines) == 1) {
     # Single model training
     rv$is_comparison_mode <- FALSE
     
     withProgress(message = paste("Training", get_method_display_name(selected_engines[1]), "..."), {
       
       rv$training_log <- c(rv$training_log, paste(Sys.time(), "- Starting training..."))
       
       rv$model_results <- tryCatch({
         incProgress(0.2, detail = "Preparing data...")
         rv$training_log <- c(rv$training_log, paste(Sys.time(), "- Preparing recipe..."))
         
         result <- train_model(
           data = train_data,
           target_col = input$target_col,
           method_engine = selected_engines[1],
           task_type = rv$task_info$task_type,
           cv_folds = input$cv_folds,
           tune_grid = input$tune_grid
         )
         
         incProgress(0.6, detail = "Generating explanations...")
         rv$training_log <- c(rv$training_log, paste(Sys.time(), "- Training complete!"))
         
         if (length(result$warnings) > 0) {
           rv$training_log <- c(rv$training_log, 
                               paste(Sys.time(), "- Note:", length(result$warnings), "warnings during CV (this is normal)"))
         }
         
         result$explanation <- explain_model_results(result)
         result
         
       }, error = function(e) {
         rv$training_log <- c(rv$training_log, paste(Sys.time(), "- ERROR:", e$message))
         showNotification(paste("Training failed:", e$message), type = "error", duration = 10)
         NULL
       })
       
       incProgress(0.2, detail = "Done!")
     })
     
   } else {
     # Multiple model comparison
     rv$is_comparison_mode <- TRUE
     
     withProgress(message = "Comparing models...", value = 0, {
       
       rv$training_log <- c(rv$training_log, paste(Sys.time(), "- Starting model comparison..."))
       rv$training_log <- c(rv$training_log, paste(Sys.time(), "- Models to train:", paste(selected_engines, collapse = ", ")))
       
       rv$comparison_results <- tryCatch({
         compare_models(
           data = train_data,
           target_col = input$target_col,
           task_type = rv$task_info$task_type,
           engines = selected_engines,
           cv_folds = input$cv_folds,
           tune_grid = input$tune_grid,
           progress_callback = function(pct, msg) {
             incProgress(pct / length(selected_engines), detail = msg)
             rv$training_log <- c(rv$training_log, paste(Sys.time(), "-", msg))
           }
         )
       }, error = function(e) {
         rv$training_log <- c(rv$training_log, paste(Sys.time(), "- ERROR:", e$message))
         showNotification(paste("Comparison failed:", e$message), type = "error")
         NULL
       })
     })
     
     if (!is.null(rv$comparison_results)) {
       rv$all_model_results <- rv$comparison_results$models
       rv$model_results <- rv$comparison_results$models[[rv$comparison_results$best_model]]
       rv$model_results$explanation <- explain_model_results(rv$model_results)
       rv$training_log <- c(rv$training_log, 
                           paste(Sys.time(), "- Comparison complete! Best model:", 
                                 get_method_display_name(rv$comparison_results$best_model)))
     }
   }
   
   if (!is.null(rv$model_results)) {
     rv$workflow_step <- 6
     # Extract and store the prepared recipe from the fitted workflow for validation
     rv$prep_recipe <- tryCatch({
       workflows::extract_recipe(rv$model_results$model, estimated = TRUE)
     }, error = function(e) {
       message("Could not extract recipe for validation: ", e$message)
       NULL
     })
     showNotification("Training complete! Check the Results tab.", type = "message")
   }
 })

 # Train all recommended models
 observeEvent(input$train_all_btn, {
   req(rv$data, input$target_col, rv$task_info, rv$methods)
   
   all_engines <- rv$methods$tidymodels_engine
   train_data <- if (!is.null(rv$cleaned_data)) rv$cleaned_data else rv$data
   
   rv$training_log <- character()
   rv$is_comparison_mode <- TRUE
   
   withProgress(message = "Training all recommended models...", value = 0, {
     
     rv$training_log <- c(rv$training_log, paste(Sys.time(), "- Starting full comparison..."))
     rv$training_log <- c(rv$training_log, paste(Sys.time(), "- Models:", paste(all_engines, collapse = ", ")))
     
     rv$comparison_results <- tryCatch({
       compare_models(
         data = train_data,
         target_col = input$target_col,
         task_type = rv$task_info$task_type,
         engines = all_engines,
         cv_folds = input$cv_folds,
         tune_grid = input$tune_grid,
         progress_callback = function(pct, msg) {
           incProgress(pct / length(all_engines), detail = msg)
           rv$training_log <- c(rv$training_log, paste(Sys.time(), "-", msg))
         }
       )
     }, error = function(e) {
       rv$training_log <- c(rv$training_log, paste(Sys.time(), "- ERROR:", e$message))
       showNotification(paste("Comparison failed:", e$message), type = "error")
       NULL
     })
   })
   
   if (!is.null(rv$comparison_results)) {
     rv$all_model_results <- rv$comparison_results$models
     rv$model_results <- rv$comparison_results$models[[rv$comparison_results$best_model]]
     rv$model_results$explanation <- explain_model_results(rv$model_results)
     rv$workflow_step <- 6
     # Extract and store the prepared recipe from the fitted workflow for validation
     rv$prep_recipe <- tryCatch({
       workflows::extract_recipe(rv$model_results$model, estimated = TRUE)
     }, error = function(e) {
       message("Could not extract recipe for validation: ", e$message)
       NULL
     })
     rv$training_log <- c(rv$training_log,
                         paste(Sys.time(), "- All models trained! Best:",
                               get_method_display_name(rv$comparison_results$best_model)))
     showNotification("All models trained! Check the Results tab.", type = "message")
   }
 })

 # =========================================================================
 # TAB 6: RESULTS & EXPLANATION (ENHANCED)
 # =========================================================================

 # Results mode indicator
 output$results_mode_ui <- renderUI({
   if (rv$is_comparison_mode && !is.null(rv$comparison_results)) {
     fluidRow(
       column(12,
         div(class = "explanation-box success",
             strong("Model Comparison Mode: "), 
             length(rv$all_model_results), " models compared. ",
             "Best performing model: ", strong(get_method_display_name(rv$comparison_results$best_model)),
             " (showing results for best model below, comparison table follows)"
         )
       )
     )
   } else if (!is.null(rv$model_results)) {
     fluidRow(
       column(12,
         div(class = "explanation-box info",
             strong("Single Model Results: "), 
             get_method_display_name(rv$model_results$method)
         )
       )
     )
   }
 })

 # Model comparison table UI
 output$model_comparison_ui <- renderUI({
   if (rv$is_comparison_mode && !is.null(rv$comparison_results)) {
     shinydashboard::box(
       title = "Model Comparison",
       width = 12,
       status = "success",
       solidHeader = TRUE,
       
       div(class = "explanation-box",
           "This table compares all trained models. ",
           strong("Higher R-squared (regression) or Accuracy (classification) = better. "),
           "The best model is highlighted."
       ),
       
       DT::DTOutput("model_comparison_table"),
       
       hr(),
       
       uiOutput("comparison_explanation")
     )
   }
 })

 output$model_comparison_table <- DT::renderDT({
   req(rv$comparison_results)
   
   comparison <- rv$comparison_results$comparison %>%
     dplyr::mutate(
       model_name = sapply(model, get_method_display_name)
     ) %>%
     dplyr::select(model_name, dplyr::everything(), -model)
   
   # Round numeric columns
   numeric_cols <- names(comparison)[sapply(comparison, is.numeric)]
   comparison <- comparison %>%
     dplyr::mutate(dplyr::across(dplyr::all_of(numeric_cols), ~round(.x, 4)))
   
   # Rename for display
   names(comparison)[1] <- "Model"
   
   DT::datatable(
     comparison,
     options = list(pageLength = 10, dom = 't'),
     rownames = FALSE
   ) %>%
     DT::formatStyle(
       'Model',
       target = 'row',
       backgroundColor = DT::styleEqual(
         get_method_display_name(rv$comparison_results$best_model),
         '#d4edda'
       )
     )
 })

 output$comparison_explanation <- renderUI({
   req(rv$comparison_results)
   
   HTML(markdown::markdownToHTML(
     text = explain_model_comparison(rv$comparison_results),
     fragment.only = TRUE
   ))
 })

 # Metrics cards
 output$results_metrics_cards <- renderUI({
   req(rv$model_results)
   
   summary <- create_metrics_summary(rv$model_results)
   primary <- summary$primary
   secondary <- summary$secondary
   
   fluidRow(
     column(4,
       div(class = paste("metric-card", primary$color),
           div(class = "metric-value", primary$value),
           div(class = "metric-label", primary$name),
           p(class = "metric-explanation", primary$description),
           tags$small(style = "opacity: 0.7;", primary$explanation)
       )
     ),
     column(8,
       fluidRow(
         lapply(secondary, function(s) {
           column(4,
             div(class = "metric-card blue",
                 div(class = "metric-value", style = "font-size: 24px;", s$value),
                 div(class = "metric-label", s$name),
                 p(class = "metric-explanation", s$description),
                 tags$small(style = "opacity: 0.7; font-size: 10px;", s$explanation)
             )
           )
         })
       )
     )
   )
 })

 # Main explanation
 output$results_explanation <- renderUI({
   req(rv$model_results$explanation)
   
   HTML(markdown::markdownToHTML(
     text = rv$model_results$explanation,
     fragment.only = TRUE
   ))
 })

 # Plots
 output$importance_plot <- renderPlot({
   req(rv$model_results$importance)
   plot_importance(rv$model_results$importance, top_n = 10)
 })

 output$prediction_plot <- renderPlot({
   req(rv$model_results)
   
   if (rv$model_results$task_type == "regression") {
     plot_predictions_regression(rv$model_results$predictions, rv$model_results$target)
   } else {
     plot_confusion_matrix(rv$model_results$predictions, rv$model_results$target)
   }
 })

 output$prediction_plot_explanation <- renderUI({
   req(rv$model_results)
   
   target_name <- rv$model_results$target

   if (rv$model_results$task_type == "regression") {
     tagList(
       div(class = "explanation-box",
       strong("Predicted vs Actual Plot for ", target_name, ":"), br(),
       "This scatter plot compares your model's predictions against the actual observed values.", br(), br(),
       strong("How to read this plot:"), br(),
       "- Each point represents one observation from the test set", br(),
       "- X-axis: Actual value of ", target_name, br(),
       "- Y-axis: Model's predicted value", br(),
       "- Red dashed line: Perfect prediction (predicted = actual)", br(), br(),
       strong("What to look for:"), br(),
       "- ", strong("Good fit:"), " Points cluster tightly around the diagonal line", br(),
       "- ", strong("Systematic errors:"), " Curved patterns suggest non-linear relationships the model missed", br(),
       "- ", strong("Increasing variance:"), " Fan-shaped patterns indicate prediction quality varies across the range", br(),
       "- ", strong("Outliers:"), " Points far from the line may be unusual cases or data errors"
       )
     )
   } else {
     tagList(
       div(class = "explanation-box",
       strong("Confusion Matrix for ", target_name, ":"), br(),
       "This table shows how well your model distinguishes between different classes.", br(), br(),
       strong("How to read this matrix:"), br(),
       "- Rows: Actual true class labels", br(),
       "- Columns: Predicted class labels", br(),
       "- Diagonal cells (top-left to bottom-right): Correct predictions", br(),
       "- Off-diagonal cells: Classification errors", br(), br(),
       strong("What to look for:"), br(),
       "- ", strong("Good model:"), " Large numbers on the diagonal, small numbers elsewhere", br(),
       "- ", strong("Confused classes:"), " Large off-diagonal numbers show which classes the model confuses", br(),
       "- ", strong("Bias toward certain classes:"), " Check if some classes have lower correct prediction rates", br(),
       "- ", strong("Class imbalance effects:"), " Rare classes may have fewer correct predictions simply due to sample size"
       )
     )
   }
 })

 output$diagnostic_plot <- renderPlot({
   req(rv$model_results)
   
   if (rv$model_results$task_type == "regression") {
     plot_residuals(rv$model_results$predictions, rv$model_results$target)
   } else {
     ggplot2::ggplot(rv$model_results$predictions, 
                     ggplot2::aes(x = .pred_class, fill = .pred_class)) +
       ggplot2::geom_bar(alpha = 0.8) +
       ggplot2::labs(title = "Predicted Class Distribution",
                     x = "Predicted Class", y = "Count") +
       ggplot2::theme_minimal() +
       ggplot2::theme(legend.position = "none")
   }
 })

 output$diagnostic_plot_explanation <- renderUI({
   req(rv$model_results)

   target_name <- rv$model_results$target

   if (rv$model_results$task_type == "regression") {
     tagList(
       div(class = "explanation-box",
       strong("Residual Diagnostic Plot:"), br(),
       "Residuals are the prediction errors: Actual ", target_name, " minus Predicted ", target_name, ".", br(), br(),
       strong("How to read this plot:"), br(),
       "- X-axis: Predicted values", br(),
       "- Y-axis: Residuals (errors)", br(),
       "- Horizontal line at zero: Perfect prediction would have all points here", br(), br(),
       strong("What to look for:"), br(),
       "- ", strong("Good model:"), " Random scatter around zero with no clear patterns", br(),
       "- ", strong("Funnel shape:"), " Errors increase/decrease with predicted values (heteroscedasticity)", br(),
       "- ", strong("Curved pattern:"), " Model is missing non-linear relationships", br(),
       "- ", strong("Clusters or gaps:"), " May indicate distinct subgroups in your data", br(),
       "- ", strong("Outliers:"), " Points far from zero are observations the model predicts poorly"
       )
     )
   } else {
     # Get actual class distribution for comparison
     actual_counts <- table(rv$model_results$test_data[[target_name]])
     tagList(
       div(class = "explanation-box",
       strong("Predicted Class Distribution:"), br(),
       "This bar chart shows how many test observations the model assigned to each ", target_name, " class.", br(), br(),
       strong("Actual class distribution in test set:"), br(),
       paste(names(actual_counts), ": ", actual_counts, collapse = ", "), br(), br(),
       strong("What to look for:"), br(),
       "- ", strong("Similar to actual:"), " Model predictions reflect true class proportions", br(),
       "- ", strong("Skewed distribution:"), " Model may be biased toward predicting certain classes", br(),
       "- ", strong("Missing classes:"), " If model never predicts a class, it may not distinguish it well", br(),
       "- ", strong("Context matters:"), " In imbalanced data, always predicting the majority class gives high accuracy but low value"
       )
     )
   }
 })

 # Metrics glossary
 output$metrics_glossary <- renderUI({
   req(rv$model_results)
   
   if (rv$model_results$task_type == "regression") {
     tagList(
       h4("Regression Metrics Explained"),
       
       div(class = "explanation-box",
           h5("R-squared"),
           p(strong("Range: "), "0 to 1 (can be negative if model performs poorly)"),
           p(strong("Meaning: "), "Proportion of variance in the target explained by the model"),
           p(strong("Interpretation: "), 
             "0.8 means the model explains 80% of why target values differ. ",
             "Higher is better. Compare to baseline of 0 (just predicting the mean)."),
           p(strong("Important Considerations: "), 
             "Always increases with more variables (even those with no predictive value). ",
             "Can be misleading - check other metrics too.")
       ),
       
       div(class = "explanation-box",
           h5("RMSE (Root Mean Square Error)"),
           p(strong("Range: "), "0 to infinity (same units as target)"),
           p(strong("Meaning: "), "Typical size of prediction errors"),
           p(strong("Interpretation: "), 
             "If RMSE = 10 and target is price in dollars, predictions are typically off by ~$10"),
           p(strong("Important Considerations: "), 
             "Sensitive to outliers (big errors hurt more). ",
             "Must compare to scale of your target to judge whether acceptable.")
       ),
       
       div(class = "explanation-box",
           h5("MAE (Mean Absolute Error)"),
           p(strong("Range: "), "0 to infinity (same units as target)"),
           p(strong("Meaning: "), "Average size of errors (ignoring direction)"),
           p(strong("Interpretation: "), 
             "If MAE = 8, on average predictions miss by 8 units"),
           p(strong("vs RMSE: "), 
             "MAE treats all errors equally. RMSE penalizes big errors more. ",
             "If RMSE >> MAE, you have some very large errors.")
       )
     )
   } else {
     tagList(
       h4("Classification Metrics Explained"),
       
       div(class = "explanation-box",
           h5("Accuracy"),
           p(strong("Range: "), "0% to 100%"),
           p(strong("Meaning: "), "Percentage of all predictions that were correct"),
           p(strong("Interpretation: "), 
             "90% accuracy means 9 out of 10 predictions are right"),
           p(strong("Important Considerations: "), 
             "Misleading with imbalanced classes! If 95% of data is class A, ",
             "always predicting A gives 95% accuracy but would not be informative.")
       ),
       
       div(class = "explanation-box",
           h5("Precision"),
           p(strong("Range: "), "0% to 100%"),
           p(strong("Meaning: "), "Of positive predictions, how many were actually positive?"),
           p(strong("Interpretation: "), 
             "High precision = few false alarms. When model says 'positive', trust it."),
           p(strong("When it matters: "), 
             "Spam filters (don't want good emails marked as spam), ",
             "expensive interventions (don't want to treat healthy patients)")
       ),
       
       div(class = "explanation-box",
           h5("Recall (Sensitivity)"),
           p(strong("Range: "), "0% to 100%"),
           p(strong("Meaning: "), "Of actual positives, how many did we catch?"),
           p(strong("Interpretation: "), 
             "High recall = find most positives. We catch 'em all."),
           p(strong("When it matters: "), 
             "Disease screening (don't want to miss sick people), ",
             "fraud detection (missing fraud is costly)")
       ),
       
       div(class = "explanation-box",
           h5("F1 Score"),
           p(strong("Range: "), "0 to 1"),
           p(strong("Meaning: "), "Harmonic mean of precision and recall"),
           p(strong("Interpretation: "), 
             "Balances precision and recall. Only high if BOTH are good."),
           p(strong("When to use: "), 
             "When you care about both false positives AND false negatives")
       ),
       
       div(class = "explanation-box",
           h5("AUC (Area Under ROC Curve)"),
           p(strong("Range: "), "0 to 1 (0.5 = random guessing)"),
           p(strong("Meaning: "), "Model's ability to rank positives higher than negatives"),
           p(strong("Interpretation: "), 
             "0.8 means: pick a random positive and negative, 80% chance model ranks positive higher"),
           p(strong("Why useful: "), 
             "Not affected by class imbalance. Shows performance across all thresholds.")
       )
     )
   }
 })

 # Predictions table
 output$predictions_table <- DT::renderDT({
   req(rv$model_results$predictions)
   DT::datatable(
     rv$model_results$predictions %>% head(500),
     options = list(scrollX = TRUE, pageLength = 10),
     rownames = FALSE
   )
 })

 # Downloads
 output$download_predictions <- downloadHandler(
   filename = function() { paste0("predictions_", Sys.Date(), ".csv") },
   content = function(file) {
     req(rv$model_results$predictions)
     # Sanitize data to prevent CSV formula injection attacks
     safe_predictions <- sanitize_csv_data(rv$model_results$predictions)
     readr::write_csv(safe_predictions, file)
   }
 )

 output$download_model <- downloadHandler(
   filename = function() {
     paste0("model_", rv$model_results$method, "_", Sys.Date(), ".rds")
   },
   content = function(file) {
     req(rv$model_results$model)
     saveRDS(rv$model_results$model, file)
   }
 )

 output$next_steps_recommendations <- renderUI({
   req(rv$model_results)

   task_type <- rv$model_results$task_type
   metrics <- rv$model_results$test_metrics
   current_method <- rv$model_results$method

   get_metric <- function(name) {
     val <- metrics %>% dplyr::filter(.metric == name) %>% dplyr::pull(.estimate)
     if (length(val) == 0) NA else val
   }

   recommendations <- tagList()

   if (task_type == "regression") {
     rsq <- get_metric("rsq")
     if (!is.na(rsq) && rsq >= 0.7) {
       recommendations <- tagList(recommendations,
         tags$li("Model performs well - consider using for predictions"),
         tags$li("Validate on completely new data before production use"),
         tags$li("Document model assumptions and when it should/shouldn't be used")
       )
     } else {
       recommendations <- tagList(recommendations,
         tags$li("Consider adding more predictor variables"),
         tags$li(
           "Try different modeling approaches (XGBoost often helps)",
           if (current_method != "xgboost") {
             tagList(
               br(),
               actionButton("quick_train_xgboost", "Quick Train: XGBoost",
                           class = "btn-primary btn-sm",
                           style = "margin-top: 5px;")
             )
           }
         ),
         tags$li(
           "Check for non-linear relationships or interactions",
           if (current_method != "ranger") {
             tagList(
               br(),
               actionButton("quick_train_rf", "Quick Train: Random Forest",
                           class = "btn-primary btn-sm",
                           style = "margin-top: 5px;")
             )
           }
         )
       )
     }
   } else {
     acc <- get_metric("accuracy")
     if (!is.na(acc) && acc >= 0.85) {
       recommendations <- tagList(recommendations,
         tags$li("Model classifies well"),
         tags$li("Consider business impact of different error types"),
         tags$li("Test with stakeholders before deployment")
       )
     } else {
       recommendations <- tagList(recommendations,
         tags$li(
           "Try ensemble methods or feature engineering",
           if (current_method != "xgboost") {
             tagList(
               br(),
               actionButton("quick_train_xgboost_cls", "Quick Train: XGBoost",
                           class = "btn-primary btn-sm",
                           style = "margin-top: 5px;")
             )
           }
         ),
         tags$li(
           "Check for class imbalance - consider resampling",
           tagList(
             br(),
             actionButton("go_to_cleaning", "Go to Data Cleaning",
                         class = "btn-info btn-sm",
                         style = "margin-top: 5px;")
           )
         ),
         tags$li("Examine misclassified cases for patterns")
       )
     }
   }

   tags$ul(recommendations)
 })

 output$download_report <- downloadHandler(
   filename = function() { paste0("ml_report_", Sys.Date(), ".html") },
   content = function(file) {
     req(rv$model_results)

     # SECURITY FIX (HIGH-2): HTML encode all user-controlled values to prevent XSS
     safe_method <- html_encode(get_method_display_name(rv$model_results$method))
     safe_task_type <- html_encode(rv$model_results$task_type)
     safe_target <- html_encode(rv$model_results$target)
     safe_n_train <- html_encode(as.character(rv$model_results$n_train))
     safe_n_test <- html_encode(as.character(rv$model_results$n_test))

     comparison_html <- ""
     if (rv$is_comparison_mode && !is.null(rv$comparison_results)) {
       safe_best_model <- html_encode(get_method_display_name(rv$comparison_results$best_model))
       # Encode comparison table values
       safe_comparison <- rv$comparison_results$comparison
       for (col in names(safe_comparison)) {
         if (is.character(safe_comparison[[col]])) {
           safe_comparison[[col]] <- sapply(safe_comparison[[col]], html_encode)
         }
       }
       comparison_html <- paste0(
         "<h2>Model Comparison</h2>",
         "<p>Best model: <strong>", safe_best_model, "</strong></p>",
         knitr::kable(safe_comparison, format = "html", escape = TRUE)
       )
     }

     # Encode metric names (in case they contain user-influenced data)
     safe_metrics_html <- paste(sapply(1:nrow(rv$model_results$test_metrics), function(i) {
       safe_metric_name <- html_encode(rv$model_results$test_metrics$.metric[i])
       paste0("<p><strong>", safe_metric_name, ":</strong> ",
              round(rv$model_results$test_metrics$.estimate[i], 4), "</p>")
     }), collapse = "")

     html_content <- paste0(
       "<!DOCTYPE html><html><head>",
       "<meta charset='UTF-8'>",
       "<meta http-equiv='Content-Security-Policy' content=\"default-src 'self'; style-src 'unsafe-inline'\">",
       "<title>LearnTidyML Report</title>",
       "<style>",
       "body{font-family:Arial,sans-serif;max-width:900px;margin:0 auto;padding:20px;line-height:1.6;}",
       ".metric{background:#f0f0f0;padding:15px;margin:10px 0;border-radius:5px;}",
       "h1{color:#2c3e50;} h2{color:#34495e;border-bottom:2px solid #3498db;padding-bottom:10px;}",
       "table{border-collapse:collapse;width:100%;} th,td{border:1px solid #ddd;padding:8px;text-align:left;}",
       "th{background:#3498db;color:white;}",
       "</style></head><body>",
       "<h1>LearnTidyML Report</h1>",
       "<p><strong>Generated:</strong> ", html_encode(as.character(Sys.time())), "</p>",
       "<p><strong>Model:</strong> ", safe_method, "</p>",
       "<p><strong>Task:</strong> ", safe_task_type, "</p>",
       "<p><strong>Target:</strong> ", safe_target, "</p>",
       "<p><strong>Training samples:</strong> ", safe_n_train, "</p>",
       "<p><strong>Test samples:</strong> ", safe_n_test, "</p>",
       comparison_html,
       "<h2>Performance Metrics</h2>",
       "<div class='metric'>",
       safe_metrics_html,
       "</div>",
       "<h2>Detailed Explanation</h2>",
       markdown::markdownToHTML(text = rv$model_results$explanation, fragment.only = TRUE),
       "</body></html>"
     )

     writeLines(html_content, file)
   }
 )

 # Quick train action buttons
 observeEvent(input$quick_train_xgboost, {
   # Switch to training tab
   shinydashboard::updateTabItems(session, "sidebar_menu", selected = "training")

   # Pre-select XGBoost - FIX: use rv$methods instead of rv$recommended_methods
   updateCheckboxGroupInput(session, "selected_models", selected = "xgboost")

   # Trigger training automatically after a short delay
   shinyjs::delay(500, {
     shinyjs::click("train_selected_btn")
   })
 })

 observeEvent(input$quick_train_rf, {
   # Switch to training tab
   shinydashboard::updateTabItems(session, "sidebar_menu", selected = "training")

   # Pre-select Random Forest - FIX: use rv$methods instead of rv$recommended_methods
   updateCheckboxGroupInput(session, "selected_models", selected = "ranger")

   # Trigger training automatically after a short delay
   shinyjs::delay(500, {
     shinyjs::click("train_selected_btn")
   })
 })

 observeEvent(input$quick_train_xgboost_cls, {
   # Switch to training tab
   shinydashboard::updateTabItems(session, "sidebar_menu", selected = "training")

   # Pre-select XGBoost - FIX: use rv$methods instead of rv$recommended_methods
   updateCheckboxGroupInput(session, "selected_models", selected = "xgboost")

   # Trigger training automatically after a short delay
   shinyjs::delay(500, {
     shinyjs::click("train_selected_btn")
   })
 })

 observeEvent(input$go_to_cleaning, {
   # Switch to data cleaning tab
   shinydashboard::updateTabItems(session, "sidebar_menu", selected = "cleaning")
 })

 # Validation tab
 output$validation_status <- renderUI({
   if (is.null(rv$model_results)) {
     div(class = "alert alert-warning",
         icon("exclamation-triangle"),
         " You must train a model first before validating on new data.",
         br(), br(),
         actionButton("go_to_training", "Go to Training Tab", class = "btn-warning")
     )
   } else if (is.null(input$validation_file)) {
     div(class = "alert alert-info",
         icon("info-circle"),
         " import a validation dataset to begin."
     )
   } else {
     div(class = "alert alert-success",
         icon("check"),
         " Validation file imported. Click 'Run Validation' to test your model."
     )
   }
 })

 observeEvent(input$go_to_training, {
   shinydashboard::updateTabItems(session, "sidebar_menu", selected = "training")
 })

 observeEvent(input$run_validation, {
   req(rv$model_results, input$validation_file)

   tryCatch({
     # SECURITY FIX: Apply same validation as main file upload
     validate_imported_file(input$validation_file, max_size_mb = 50)

     # Read validation data
     validation_data <- read_data_file(input$validation_file$datapath,
                                      input$validation_file$name)

     # Validate dimensions and column names for security
     validate_data_dimensions(validation_data, max_rows = 1000000, max_cols = 1000, max_cells = 10000000)
     validate_column_names(validation_data)

     # Get target column
     target_col <- rv$task_info$target

     # Validate that target column exists
     if (!target_col %in% names(validation_data)) {
       showNotification(
         paste0("Error: Target column '", target_col, "' not found in validation data."),
         type = "error",
         duration = 10
       )
       return(NULL)
     }

     # Check for required predictor columns from training data
     training_cols <- names(rv$model_results$train_data)
     validation_cols <- names(validation_data)

     missing_cols <- setdiff(training_cols, validation_cols)
     if (length(missing_cols) > 0) {
       showNotification(
         paste0("Error: Validation data is missing columns required by the model: ",
                paste(head(missing_cols, 5), collapse = ", "),
                if (length(missing_cols) > 5) paste0(" ... and ", length(missing_cols) - 5, " more")),
         type = "error",
         duration = 15
       )
       return(NULL)
     }

     # Note about extra columns (informational only - they will be ignored)
     extra_cols <- setdiff(validation_cols, training_cols)
     if (length(extra_cols) > 0) {
       message("Note: Validation data has extra columns not used by model: ",
               paste(head(extra_cols, 5), collapse = ", "))
     }

     # Make predictions using the fitted workflow
     # Note: The workflow automatically applies preprocessing (recipe), so pass raw validation_data
     model_fit <- rv$model_results$model
     task_type <- rv$model_results$task_type

     validation_preds <- tryCatch({
       if (task_type == "regression") {
         # Workflow handles preprocessing internally
         predict(model_fit, validation_data) %>%
           dplyr::bind_cols(validation_data)
       } else {
         predict(model_fit, validation_data, type = "class") %>%
           dplyr::bind_cols(predict(model_fit, validation_data, type = "prob")) %>%
           dplyr::bind_cols(validation_data)
       }
     }, error = function(e) {
       showNotification(
         paste0("Error making predictions: ", e$message),
         type = "error",
         duration = 15
       )
       return(NULL)
     })
     if (is.null(validation_preds)) return(NULL)

     # Ensure target column has same type as in training for classification
     if (task_type == "classification") {
       # Get the levels from the training data target
       train_target_levels <- levels(rv$model_results$train_data[[target_col]])
       if (!is.null(train_target_levels)) {
         validation_preds[[target_col]] <- factor(
           validation_preds[[target_col]],
           levels = train_target_levels
         )
       }
     }

     # Calculate metrics
     validation_metrics <- tryCatch({
       if (task_type == "regression") {
         validation_preds %>%
           yardstick::metrics(truth = !!rlang::sym(target_col), estimate = .pred)
       } else {
         validation_preds %>%
           yardstick::metrics(truth = !!rlang::sym(target_col), estimate = .pred_class)
       }
     }, error = function(e) {
       showNotification(
         paste0("Error calculating metrics: ", e$message),
         type = "error",
         duration = 15
       )
       return(NULL)
     })
     if (is.null(validation_metrics)) return(NULL)

     # Store results
     rv$validation_results <- list(
       predictions = validation_preds,
       metrics = validation_metrics,
       data_size = nrow(validation_data)
     )

     showNotification("Validation completed successfully!", type = "message", duration = 5)

   }, error = function(e) {
     # Show actual error message for debugging
     showNotification(
       paste0("Validation error: ", e$message),
       type = "error",
       duration = 15
     )
     message("Full validation error: ", e$message)
     if (!is.null(e$call)) {
       message("Error call: ", paste(deparse(e$call), collapse = " "))
     }
   })
 })

 output$validation_results <- renderUI({
   req(rv$validation_results)

   val_metrics <- rv$validation_results$metrics
   test_metrics <- rv$model_results$test_metrics
   task_type <- rv$model_results$task_type

   # Create comparison
   if (task_type == "regression") {
     test_rmse <- test_metrics %>%
       dplyr::filter(.metric == "rmse") %>%
       dplyr::pull(.estimate)
     val_rmse <- val_metrics %>%
       dplyr::filter(.metric == "rmse") %>%
       dplyr::pull(.estimate)

     test_rsq <- test_metrics %>%
       dplyr::filter(.metric == "rsq") %>%
       dplyr::pull(.estimate)
     val_rsq <- val_metrics %>%
       dplyr::filter(.metric == "rsq") %>%
       dplyr::pull(.estimate)

     rmse_diff <- ((val_rmse - test_rmse) / test_rmse) * 100
     rsq_diff <- ((val_rsq - test_rsq) / test_rsq) * 100

     tagList(
       h4("Validation Dataset: ", rv$validation_results$data_size, " rows"),
       br(),

       fluidRow(
         column(6,
                div(class = "metric-card",
                    div(class = "metric-label", "Original Test RMSE"),
                    div(class = "metric-value", round(test_rmse, 4))
                )
         ),
         column(6,
                div(class = "metric-card",
                    div(class = "metric-label", "Validation RMSE"),
                    div(class = "metric-value", round(val_rmse, 4)),
                    div(class = "metric-explanation",
                        if (abs(rmse_diff) < 10) {
                          paste0("Similar performance (", round(rmse_diff, 1), "% difference)")
                        } else if (rmse_diff > 0) {
                          paste0("Worse on new data (+", round(rmse_diff, 1), "%)")
                        } else {
                          paste0("Better on new data (", round(rmse_diff, 1), "%)")
                        }
                    )
                )
         )
       ),

       br(),

       fluidRow(
         column(6,
                div(class = "metric-card",
                    div(class = "metric-label", "Original Test R-squared"),
                    div(class = "metric-value", round(test_rsq, 4))
                )
         ),
         column(6,
                div(class = "metric-card",
                    div(class = "metric-label", "Validation R-squared"),
                    div(class = "metric-value", round(val_rsq, 4)),
                    div(class = "metric-explanation",
                        if (abs(rsq_diff) < 10) {
                          "Model generalizes well"
                        } else if (rsq_diff < 0) {
                          "Performance degraded - possible overfitting"
                        } else {
                          "Better performance (unusual - verify data)"
                        }
                    )
                )
         )
       ),

       br(),

       div(class = "explanation-box",
           h5("Interpretation"),
           p(
             if (abs(rmse_diff) < 15 && abs(rsq_diff) < 15) {
               strong("Positive Result: ", style = "color: green;")
             } else {
               strong("Warning: ", style = "color: orange;")
             },
             if (abs(rmse_diff) < 15 && abs(rsq_diff) < 15) {
               "Your model shows consistent performance on the validation data. This indicates good generalization."
             } else if (rmse_diff > 15 || rsq_diff < -15) {
               "Your model performs noticeably worse on the validation data. This suggests overfitting - the model may have learned patterns specific to the training data."
             } else {
               "Your model performs better on the validation data, which is unusual. Verify that the validation data is from the same distribution as the training data."
             }
           )
       )
     )

   } else {
     # Classification
     test_acc <- test_metrics %>%
       dplyr::filter(.metric == "accuracy") %>%
       dplyr::pull(.estimate)
     val_acc <- val_metrics %>%
       dplyr::filter(.metric == "accuracy") %>%
       dplyr::pull(.estimate)

     acc_diff <- (val_acc - test_acc) * 100

     tagList(
       h4("Validation Dataset: ", rv$validation_results$data_size, " rows"),
       br(),

       fluidRow(
         column(6,
                div(class = "metric-card",
                    div(class = "metric-label", "Original Test Accuracy"),
                    div(class = "metric-value", paste0(round(test_acc * 100, 1), "%"))
                )
         ),
         column(6,
                div(class = "metric-card",
                    div(class = "metric-label", "Validation Accuracy"),
                    div(class = "metric-value", paste0(round(val_acc * 100, 1), "%")),
                    div(class = "metric-explanation",
                        if (abs(acc_diff) < 5) {
                          paste0("Similar performance (", round(acc_diff, 1), " pp difference)")
                        } else if (acc_diff < 0) {
                          paste0("Worse on new data (", round(acc_diff, 1), " pp)")
                        } else {
                          paste0("Better on new data (+", round(acc_diff, 1), " pp)")
                        }
                    )
                )
         )
       ),

       br(),

       div(class = "explanation-box",
           h5("Interpretation"),
           p(
             if (abs(acc_diff) < 5) {
               strong("Positive Result: ", style = "color: green;")
             } else {
               strong("Warning: ", style = "color: orange;")
             },
             if (abs(acc_diff) < 5) {
               "Your model shows consistent classification performance on the validation data. This indicates good generalization."
             } else if (acc_diff < -5) {
               "Your model's accuracy dropped on the validation data. This suggests overfitting or that the validation data has different characteristics."
             } else {
               "Your model performs better on the validation data, which is unusual. Verify that the validation data is representative."
             }
           )
       )
     )
   }
 })
 
  observeEvent(input$show_about_btn, {
    showModal(modalDialog(
      title = "About LearnTidyML",
      size = "l",
      div(style = "padding: 20px;",
          h3("LearnTidyML - Machine Learning Tool"),
          p("An educational tool to learn and apply machine learning workflows using the tidymodels framework."),
          hr(),
          
          h4("Key Features"),
          tags$ul(
            tags$li("Guided workflow from data import to model evaluation."),
            tags$li("Automatic ML task detection (Regression/Classification)."),
            tags$li("Recommends appropriate tidymodels methods."),
            tags$li("Generates reproducible R code for each step."),
            tags$li("Interactive data cleaning and preprocessing previews.")
          ),
          hr(),

          h4("Security Notes"),
          tags$ul(
            tags$li("All data processing occurs locally in your R session."),
            tags$li("The app does not connect to any external services or transmit your data."),
            tags$li("File imports are validated for size and basic structure to prevent abuse.")
          ),
          hr(),

          h4("Documentation"),
          p("For more information, see the package README file."),
          hr(),

          h4("Data Sources"),
          p("This application uses user-provided data. Sample datasets are included in the package for demonstration purposes."),
          hr(),
          
          h4("Limitations"),
          tags$ul(
            tags$li("For educational and demonstration purposes only."),
            tags$li("Handles datasets up to 1,000,000 rows and 1,000 columns."),
            tags$li("Advanced feature engineering and model tuning are simplified.")
          ),
          hr(),
          
          h4("Warranty"),
          p("This software is provided 'as is', without warranty of any kind. Use at your own risk."),
          hr(),
          
          h4("Version"),
          # Read the package version dynamically
          p(paste("Version:", utils::packageVersion("LearnTidyML"))),
          hr(),

            h4("Created By"),
            p(tags$a(href = "https://github.com/jasongeslois", target = "_blank", "Jason Geslois")),
            hr(),
            h5("Development Environment"),
            tags$ul(
              tags$li(tags$a(href = "https://www.r-project.org/", target = "_blank", "R"), " version 4.4.3 by The R Foundation"),
              tags$li(tags$a(href = "https://positron.posit.co/", target = "_blank", "Positron"), " 2025.11.0 by Posit")
            ),
            h5("AI Assistance"),
            tags$ul(
              tags$li(tags$a(href = "https://www.anthropic.com/claude", target = "_blank", "Claude"), " (Opus 4.5, Sonnet 4.5) by Anthropic"),
              tags$li(tags$a(href = "https://deepmind.google/technologies/gemini/", target = "_blank", "Gemini"), " 3.0 by Google")
            ),
            hr(),

          h4("Dependencies & Citations"),
          p("This application relies on the following packages from the R community:"),
          uiOutput("dependencies_info")
      ),
      footer = modalButton("Close")
    ))
  })

  # Render the list of dependencies and their citations
  output$dependencies_info <- renderUI({

    tryCatch({
      # Get dependencies from DESCRIPTION file
      deps <- utils::packageDescription("LearnTidyML")$Imports

      if (is.null(deps) || length(deps) == 0 || (length(deps) == 1 && is.na(deps))) {
        return(p("No dependencies listed or package not installed."))
      }

      # Clean up the dependency string - handle newlines and extra whitespace
      dep_list <- strsplit(deps, ",\\s*")[[1]]
      dep_names <- unique(sapply(dep_list, function(x) {
        # Remove version constraints and trim whitespace/newlines
        name <- gsub("\\s*\\(.*\\)", "", x)
        trimws(gsub("[\r
]", "", name))
      }))

      # Remove empty strings
      dep_names <- dep_names[dep_names != ""]

      # Generate citation for each package
      citations <- lapply(dep_names, function(pkg) {
        # Skip base R packages which don't need explicit citation here
        if (pkg %in% c("R", "stats", "graphics", "grDevices", "utils", "datasets", "methods", "base")) {
          return(NULL)
        }

        tryCatch({
          cit <- citation(pkg)
          # Format citation as a simple string - collapse multiple elements into one
          cit_text <- paste(format(cit, style = "text"), collapse = "

")

          # Create a collapsible UI element for each citation
          tags$details(
            style = "margin-bottom: 10px; border: 1px solid #ddd; padding: 5px; border-radius: 4px;",
            tags$summary(strong(pkg), style = "cursor: pointer;"),
            tags$pre(style = "white-space: pre-wrap; font-size: 12px;", cit_text)
          )
        }, error = function(e) {
          p(strong(pkg), ": Citation not available.")
        })
      })

      # Remove NULLs from the list (base R approach, no purrr dependency)
      citations <- Filter(Negate(is.null), citations)

      # Combine into a single UI
      tagList(
        citations
      )
    }, error = function(e) {
      p("Error loading dependencies: ", e$message)
    })
  })
}

#' Helper to generate tidymodels code
#' @keywords internal
generate_tidymodels_code <- function(method, task_info, data, profile) {
 engine <- method$tidymodels_engine
 target <- task_info$target
 safe_target <- safe_r_name(target)
 task_type <- task_info$task_type

 # SECURITY FIX (CRITICAL-1): Sanitize ALL column names to prevent code injection
 # Validate and escape all column names, not just the target
 all_cols <- names(data)
 predictor_cols <- setdiff(all_cols, target)
 safe_predictors <- sapply(predictor_cols, safe_r_name, USE.NAMES = FALSE)

 # Build explicit formula with sanitized names for safety
 # This prevents injection via malicious column names like `system('cmd')`
 if (length(safe_predictors) <= 15) {
   # List predictors explicitly for maximum security
   formula_str <- paste0(safe_target, " ~ ", paste(safe_predictors, collapse = " + "))
 } else {
   # For many predictors, use ~ . but data column names were already validated on import
   formula_str <- paste0(safe_target, " ~ .")
 }

 # Generate method-specific model specification
 model_spec_code <- switch(
   engine,

   "glmnet" = paste0(
     "# LASSO/Ridge regression with automatic tuning
",
     "model_spec <- ", if(task_type == "regression") "linear_reg" else "multinom_reg",
     "(penalty = tune(), mixture = 1) %>%
",
     "  set_engine('glmnet')"
   ),

   "ranger" = paste0(
     "# Random Forest
",
     "model_spec <- rand_forest(mtry = tune(), trees = 300, min_n = tune()) %>%
",
     "  set_engine('ranger', importance = 'impurity') %>%
",
     "  set_mode('", task_type, "')"
   ),

   "xgboost" = paste0(
     "# XGBoost (Gradient Boosting)
",
     "model_spec <- boost_tree(
",
     "  trees = tune(), learn_rate = tune(), tree_depth = tune()
",
     ") %>%
",
     "  set_engine('xgboost') %>%
",
     "  set_mode('", task_type, "')"
   ),

   "kernlab" = paste0(
     "# Support Vector Machine (Radial Basis Function)
",
     "model_spec <- svm_rbf(cost = tune()) %>%
",
     "  set_engine('kernlab') %>%
",
     "  set_mode('", task_type, "')"
   ),

   "kknn" = paste0(
     "# K-Nearest Neighbors
",
     "model_spec <- nearest_neighbor(neighbors = tune()) %>%
",
     "  set_engine('kknn') %>%
",
     "  set_mode('", task_type, "')"
   ),

   "rpart" = paste0(
     "# Decision Tree
",
     "model_spec <- decision_tree(
",
     "  cost_complexity = tune(), tree_depth = tune()
",
     ") %>%
",
     "  set_engine('rpart') %>%
",
     "  set_mode('", task_type, "')"
   ),

   "naivebayes" = paste0(
     "# Naive Bayes (for classification only)
",
     "model_spec <- naive_Bayes() %>%
",
     "  set_engine('naivebayes') %>%
",
     "  set_mode('classification')"
   ),

   # Default fallback
   paste0(
     "# Basic ", if(task_type == "regression") "Linear" else "Logistic", " Model
",
     "model_spec <- ", if(task_type == "regression") "linear_reg" else "multinom_reg",
     "() %>%
",
     "  set_engine('", engine, "')"
   )
 )

 paste0(
   "# ", method$method, " with tidymodels
",
   "library(tidymodels)

",
   "# 1. Create recipe (preprocessing steps)
",
   "# Note: Column names have been validated and escaped for security
",
   "my_recipe <- recipe(", formula_str, ", data = train_data) %>%
",
   "  step_impute_median(all_numeric_predictors()) %>%
",
   "  step_impute_mode(all_nominal_predictors()) %>%
",
   "  step_novel(all_nominal_predictors()) %>%
",
   "  step_other(all_nominal_predictors(), threshold = 0.05) %>%
",
   "  step_mutate_at(all_nominal_predictors(), fn = droplevels) %>%
",
   "  step_dummy(all_nominal_predictors()) %>%
",
   "  step_zv(all_predictors()) %>%
",
   "  step_normalize(all_numeric_predictors()) %>%
",
   "  step_corr(all_numeric_predictors(), threshold = 0.9)

",
   "# 2. ", model_spec_code, "

",
   "# 3. Create workflow
",
   "wf <- workflow() %>%
",
   "  add_recipe(my_recipe) %>%
",
   "  add_model(model_spec)

",
   "# 4. Train/test split
",
   "set.seed(123)
",
   "data_split <- initial_split(train_data, prop = 0.75",
   if(task_type == "classification") ", strata = " else "",
   if(task_type == "classification") safe_target else "",
   ")
",
   "train <- training(data_split)
",
   "test <- testing(data_split)

",
   "# 5. Cross-validation
",
   "cv_folds <- vfold_cv(train, v = 5",
   if(task_type == "classification") ", strata = " else "",
   if(task_type == "classification") safe_target else "",
   ")

",
   "# 6. Tune hyperparameters (if model has tune() parameters)
",
   "tune_results <- tune_grid(
",
   "  wf,
",
   "  resamples = cv_folds,
",
   "  grid = 10  # Try 10 combinations
",
   ")

",
   "# 7. Select best model and finalize
",
   "best_params <- select_best(tune_results, '",
   if(task_type == "regression") "rmse" else "accuracy",
   "')
",
   "final_wf <- finalize_workflow(wf, best_params)
",
   "final_fit <- fit(final_wf, train)

",
   "# 8. Evaluate on test set
",
   "predictions <- predict(final_fit, test) %>%
",
   "  bind_cols(test)

",
   "# View metrics
",
   if(task_type == "regression") {
     "metrics(predictions, truth = "
   } else {
     "metrics(predictions, truth = "
   },
   safe_target, ", estimate = .pred",
   if(task_type == "classification") "_class" else "",
   ")
"
 )
}

