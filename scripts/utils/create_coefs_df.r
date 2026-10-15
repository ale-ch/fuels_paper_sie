#part <- readRDS("/Volumes/T7 Shield/FRES/fuels_data/results/rolling_results/res_Emilia_Romagna_rad2500_w471_w522.RDS")

#file_paths <- list.files(parts_dir, pattern = "\\.RDS$", full.names = TRUE)
#if(length(file_paths) == 0) stop("No RDS files found in the specified directory.")

#mod_data <- readRDS("/Volumes/T7 Shield/FRES/fuels_data/rolling_results/res_Emilia_Romagna_rad2500_w471_w522.RDS")

create_coef_df <- function(mod_data, output_path) {
  c_df_part <- map_dfr(names(mod_data$models), function(m_name) {
    
    #print(paste("PARSING: ", m_name))
    # 1. Safely attempt to generate the summary
    #print("GET SUMMARY")
    summ <- tryCatch({
      summary(mod_data$models[[m_name]])
    }, error = function(e) {
      return(NULL) # Skip if the internal summary method crashes
    })
    
    if (is.null(summ)) return(NULL)
    
    #print("GET TABLE")
    # 2. Extract table safely
    if ("CoefTable" %in% names(summ)) {
      c_df <- as.data.frame(summ$CoefTable)
    } else if ("coefficients" %in% names(summ)) {
      c_df <- as.data.frame(summ$coefficients)
    } else {
      return(NULL)
    }
    
    # 3. Skip if the model failed and returned a degenerate table
    if (nrow(c_df) == 0 || ncol(c_df) < 4) {
      #print("SKIPPING")
      return(NULL)
    }
    
    # 4. Force standard dimensions and names
    #print("MAKING COLUMNS")
    c_df <- c_df[, 1:4]
    colnames(c_df) <- c("estimate", "std_error", "t_value", "p_value")
    
    c_df$term <- rownames(c_df)
    c_df$model <- m_name
    c_df$window <- mod_data$window
    c_df$radius_m <- mod_data$radius
    c_df$end_week <- mod_data$end_week
    c_df$region <- mod_data$region
    c_df$estimation_granularity <- mod_data$estimation_granularity
    
    # Add (week_index - 1) * 7 days to the base date
    c_df$date <- as.Date("2016-01-01") + (mod_data$start_week - 1) * 7
    c_df$end_date <- as.Date("2016-01-01") + (mod_data$end_week - 1) * 7
    
    return(c_df)
  })
  
  
  rownames(c_df_part) <- NULL
  
  # Map terms using the central dictionary
  c_df_part$term <- sapply(c_df_part$term, function(x) {
    if (x %in% names(config$term_labels)) {
      return(config$term_labels[[x]])
    } else if (grepl("W\\*", x)) { 
      base_var <- gsub("W\\*", "", x)
      clean_base <- ifelse(base_var %in% names(config$term_labels), config$term_labels[[base_var]], base_var)
      return(paste("Wx", clean_base))
    } else if (grepl("^Wx_", x)) {
      base_var <- gsub("^Wx_", "", x)
      clean_base <- ifelse(base_var %in% names(config$term_labels), config$term_labels[[base_var]], base_var)
      return(paste("Wx", clean_base))
    } else {
      return(x)
    }
  })
  
  # #print(paste("SAVE FILE:", file.path(output_dir, file_name)))
  saveRDS(c_df_part, output_path)
  
  return(c_df_part)
}


