# ------------------------------------------------------------------------------
# estimation.r (UPDATED FOR CENTRALIZED LOGGING)
# ------------------------------------------------------------------------------
library(foreach)
library(doParallel)
library(dplyr)
library(tidyverse)
library(sf)
library(spdep)
library(plm)
library(splm)

# ------------------------------------------------------------------------------
# 1. CENTRAL MODEL CONFIGURATION
# ------------------------------------------------------------------------------
config <- list(
  dependent_var = "mean_price_gasoline_self",
  base_features = c("population_density_km2", "brent_diff_pos", "brent_diff_neg"),
  term_labels = c(
    "(Intercept)"    = "Intercept",
    "lambda"         = "Spatial Lag (lambda)",
    "rho"            = "Spatial Error (rho)"
  ),
  spatial_models = list(
    SLX   = list(pkg = "plm",  lag = FALSE, spatial.error = FALSE, Durbin = TRUE),
    SEM   = list(pkg = "splm", lag = FALSE, spatial.error = TRUE,  Durbin = FALSE),
    SDEM  = list(pkg = "splm", lag = FALSE, spatial.error = TRUE,  Durbin = TRUE),
    SAR   = list(pkg = "splm", lag = TRUE,  spatial.error = FALSE, Durbin = FALSE),
    SDM   = list(pkg = "splm", lag = TRUE,  spatial.error = FALSE, Durbin = TRUE),
    SARAR = list(pkg = "splm", lag = TRUE,  spatial.error = TRUE,  Durbin = FALSE),
    GNS   = list(pkg = "splm", lag = TRUE,  spatial.error = TRUE,  Durbin = TRUE)
  )
)

# ------------------------------------------------------------------------------
# 2. MODULAR PIPELINE FUNCTIONS
# ------------------------------------------------------------------------------

create_coef_df <- function(mod_data, output_path) {
  c_df_part <- map_dfr(names(mod_data$models), function(m_name) {
    
    summ <- tryCatch({
      summary(mod_data$models[[m_name]])
    }, error = function(e) {
      return(NULL) 
    })
    
    if (is.null(summ)) return(NULL)
    
    if ("CoefTable" %in% names(summ)) {
      c_df <- as.data.frame(summ$CoefTable)
    } else if ("coefficients" %in% names(summ)) {
      c_df <- as.data.frame(summ$coefficients)
    } else {
      return(NULL)
    }
    
    if (nrow(c_df) == 0 || ncol(c_df) < 4) {
      return(NULL)
    }
    
    c_df <- c_df[, 1:4]
    colnames(c_df) <- c("estimate", "std_error", "t_value", "p_value")
    
    c_df$term <- rownames(c_df)
    c_df$model <- m_name
    c_df$window <- mod_data$window
    c_df$radius_m <- mod_data$radius
    c_df$end_week <- mod_data$end_week
    c_df$region <- mod_data$region
    c_df$estimation_granularity <- mod_data$estimation_granularity
    
    c_df$date <- as.Date("2016-01-01") + (mod_data$start_week - 1) * 7
    c_df$end_date <- as.Date("2016-01-01") + (mod_data$end_week - 1) * 7
    
    return(c_df)
  })
  
  rownames(c_df_part) <- NULL
  
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
  
  saveRDS(c_df_part, output_path)
  return(c_df_part)
}

apply_stratified_sampling <- function(df, granularity, sample_fraction, random_seed = 42) {
  
  if(granularity == "nuts0") {
    granularity <- "nuts1"
  }
  
  set.seed(random_seed)
  if (sample_fraction >= 1.0) return(df)
  
  log_msg("Applying stratified sampling (", sample_fraction * 100, "%) at ", granularity, " level.")
  
  sampled_pumps <- df %>%
    group_by(id_pump) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(!!sym(granularity)) %>%
    slice_sample(prop = sample_fraction) %>%
    pull(id_pump)
  
  result_df <- df %>% filter(id_pump %in% sampled_pumps)
  log_msg("Retained ", length(unique(result_df$id_pump)), " unique stations for estimation.")
  
  return(result_df)
}

prepare_window_data <- function(df, start_week, window_size) {
  end_week <- start_week + window_size - 1
  
  window_df <- df %>% filter(time_index >= start_week, time_index <= end_week)
  if (nrow(window_df) == 0) {
    log_msg("    -> SKIP: 0 rows found in time_index range [", start_week, " - ", end_week, "]")
    return(NULL)
  }
  
  complete_pumps <- window_df %>%
    filter(
      complete.cases(mean_price_gasoline_self, latitude, longitude),
      !is.nan(mean_price_gasoline_self)
    ) %>%
    count(id_pump) %>% 
    filter(n == window_size) %>% 
    pull(id_pump)
  
  if (length(complete_pumps) < 10) {
    log_msg("    -> SKIP: Only ", length(complete_pumps), " strictly balanced stations (need >=10).")
    return(NULL)
  }
  
  window_df <- window_df %>% 
    filter(id_pump %in% complete_pumps) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
    st_transform(32632)
  
  window_df_unique <- window_df %>% 
    group_by(id_pump) %>% 
    slice(1) %>% 
    ungroup()
  
  log_msg("    -> SUCCESS: Window balanced with ", length(complete_pumps), " active stations.")
  return(list(df = window_df, df_unique = window_df_unique))
}

compute_distance_matrix <- function(window_data, radius_m) {
  df <- window_data$df
  df_unique <- window_data$df_unique
  
  coords <- st_coordinates(df_unique)
  nb <- dnearneigh(coords, d1 = 0, d2 = radius_m, longlat = FALSE)
  
  isolated <- which(card(nb) == 0)
  if (length(isolated) > 0) {
    log_msg("    -> MATRIX: Dropping ", length(isolated), " isolated stations with 0 neighbors.")
    df_unique <- df_unique[-isolated, ]
  }
  
  if (nrow(df_unique) < 10) {
    log_msg("    -> SKIP: Matrix too small after dropping isolates (", nrow(df_unique), " remaining).")
    return(NULL)
  }
  
  df <- df %>% filter(id_pump %in% df_unique$id_pump) %>% arrange(id_pump, time_index)
  df_unique <- df_unique %>% arrange(id_pump)
  
  coords <- st_coordinates(df_unique)
  nb <- dnearneigh(coords, d1 = 0, d2 = radius_m, longlat = FALSE)
  
  W <- nb2listw(nb, glist = lapply(nbdists(nb, coords), function(x) ifelse(x>0, 1/x, 0)), 
                style = "W", zero.policy = TRUE)
  
  log_msg("    -> SUCCESS: Spatial matrix W created (N = ", nrow(df_unique), ").")
  return(list(df = df, W = W))
}

fit_spatial_models <- function(df, W, formula, config) {
  p_data <- pdata.frame(df, index = c("id_pump", "time_index"))
  fitted_models <- list()
  
  for (model_name in names(config$spatial_models)) {
    m_conf <- config$spatial_models[[model_name]]
    log_msg("    -> MODEL: Estimating ", model_name, "...")
    
    fit <- tryCatch({
      if (m_conf$pkg == "splm") {
        spgm(formula, data = p_data, listw = W, model = "within", method = "w2sls",
             lag = m_conf$lag, spatial.error = m_conf$spatial.error, Durbin = m_conf$Durbin)
      } else {
        plm(formula, data = p_data, model = "within", effect = "individual")
      }
    }, error = function(e) {
      log_msg("       [!] FAILED: ", model_name, " | Error: ", e$message)
      return(NULL)
    })
    
    if (!is.null(fit)) {
      fitted_models[[model_name]] <- fit
    }
  }
  
  return(fitted_models)
}

# ------------------------------------------------------------------------------
# UPDATED EXECUTION LOOP
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# UPDATED EXECUTION LOOP
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# UPDATED EXECUTION LOOP WITH OVERALL PROGRESS & ETA
# ------------------------------------------------------------------------------
run_rolling_estimation <- function(base_data_path, config, formula, radiuses, window_size, 
                                   estimation_granularity = c("nuts0", "nuts1", "nuts2", "nuts3"),
                                   output_dir,
                                   sample_fraction = 1.0, random_seed = 42,
                                   n_cores = NULL) {
  
  match.arg(estimation_granularity)
  set.seed(random_seed)
  
  log_msg("LOADING DATA")
  base_data <- readRDS(base_data_path)
  
  if (!is.numeric(base_data$df$time_index)) {
    log_msg("WARNING: time_index is not numeric. Forcing conversion to sequential integers.")
    base_data$df <- base_data$df %>%
      mutate(time_index = as.integer(factor(time_index, ordered = TRUE)))
  }
  
  working_df <- apply_stratified_sampling(base_data$df, estimation_granularity,
                                          sample_fraction, random_seed)
  
  run_config <- list(
    timestamp = Sys.time(),
    config_params = config,
    formula = deparse(formula),
    radiuses = radiuses,
    window_size = window_size,
    estimation_granularity = estimation_granularity,
    sample_fraction = sample_fraction,
    random_seed = random_seed,
    sampled_id_pumps = unique(working_df$id_pump)
  )
  
  config_file_name <- paste0("run_config_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".RDS")
  config_path <- file.path(output_dir, config_file_name)
  saveRDS(run_config, config_path)
  log_msg("Run configuration and sampled id_pump list saved to: ", config_path)
  
  log_msg("CLEARING MEMORY - remove base_data")
  rm(base_data); gc()
  
  if (estimation_granularity == "nuts0") {
    regions <- "Italy"
  } else {
    regions <- unique(working_df[[estimation_granularity]])
  }
  valid_regions <- regions[!is.na(regions)]
  
  min_week <- min(working_df$time_index, na.rm = TRUE)
  max_week <- max(working_df$time_index, na.rm = TRUE)
  log_msg("Global time_index range: ", min_week, " to ", max_week)
  
  # --------------------------------------------------------------------------
  # Build flat task list (resume-aware: skip tasks whose output already exists)
  # --------------------------------------------------------------------------
  tasks <- list()
  for (region_name in valid_regions) {
    for (rad in radiuses) {
      sw <- min_week
      while ((sw + window_size - 1) <= max_week) {
        ew <- sw + window_size - 1
        fname <- sprintf("res_%s_rad%d_w%03d_w%03d.RDS",
                         gsub("[^A-Za-z0-9]", "_", region_name), rad, sw, ew)
        target <- file.path(output_dir, fname)
        if (!file.exists(target)) {
          tasks[[length(tasks) + 1L]] <- list(
            region      = region_name,
            radius      = rad,
            start_week  = sw,
            end_week    = ew,
            target_file = target
          )
        }
        sw <- sw + 1
      }
    }
  }
  
  log_msg("Total tasks to run: ", length(tasks))
  if (length(tasks) == 0) {
    log_msg("Nothing to do (all outputs already exist).")
    return(invisible(NULL))
  }
  
  if (is.null(n_cores)) n_cores <- max(1L, parallel::detectCores() - 1L)
  n_cores <- min(n_cores, length(tasks))
  log_msg("Parallelizing with ", n_cores, " workers.")
  
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  on.exit({
    try(parallel::stopCluster(cl), silent = TRUE)
    try(foreach::registerDoSEQ(), silent = TRUE)
  }, add = TRUE)
  
  # --------------------------------------------------------------------------
  # Dispatch
  # --------------------------------------------------------------------------
  results <- foreach::foreach(
    task = tasks,
    .packages = c("dplyr", "sf", "spdep", "plm", "splm", "tidyverse"),
    .export   = c("config", "formula", "window_size", "estimation_granularity",
                  "working_df",
                  "prepare_window_data", "compute_distance_matrix",
                  "fit_spatial_models", "create_coef_df"),
    .errorhandling = "pass"
  ) %dopar% {
    
    # Silence logger on workers to avoid shared-file contention.
    assign("log_msg", function(...) invisible(NULL), envir = globalenv())
    
    tryCatch({
      region_df <- if (task$region == "Italy") {
        working_df
      } else {
        working_df[working_df[[estimation_granularity]] == task$region, , drop = FALSE]
      }
      
      window_data <- prepare_window_data(region_df, task$start_week, window_size)
      if (is.null(window_data)) return(list(ok = TRUE, file = NA_character_))
      
      aligned_data <- compute_distance_matrix(window_data, task$radius)
      if (is.null(aligned_data)) return(list(ok = TRUE, file = NA_character_))
      
      fitted_models <- fit_spatial_models(aligned_data$df, aligned_data$W, formula, config)
      
      if (length(fitted_models) > 0) {
        payload <- list(
          models                 = fitted_models,
          window                 = window_size,
          start_week             = task$start_week,
          end_week               = task$end_week,
          region                 = task$region,
          radius                 = task$radius,
          estimation_granularity = estimation_granularity
        )
        create_coef_df(payload, task$target_file)
        return(list(ok = TRUE, file = task$target_file))
      }
      list(ok = TRUE, file = NA_character_)
    },
    error = function(e) {
      list(ok = FALSE, file = task$target_file,
           msg = paste0(task$region, "|rad=", task$radius,
                        "|w=", task$start_week, "-", task$end_week,
                        "|", conditionMessage(e)))
    })
  }
  
  # --------------------------------------------------------------------------
  # Summary
  # --------------------------------------------------------------------------
  ok_flags <- vapply(results, function(x) isTRUE(x$ok), logical(1))
  wrote    <- vapply(results, function(x) isTRUE(x$ok) && !is.na(x$file), logical(1))
  
  log_msg("=== PARALLEL PIPELINE COMPLETE ===")
  log_msg(sprintf("Tasks: %d | OK: %d | Errors: %d | New files written: %d",
                  length(tasks), sum(ok_flags), sum(!ok_flags), sum(wrote)))
  
  if (any(!ok_flags)) {
    log_msg("Errors:")
    for (x in results[!ok_flags]) log_msg("  - ", x$msg)
  }
  
  invisible(results)
}

# ------------------------------------------------------------------------------
# UPDATED RESULT COLLECTION
# ------------------------------------------------------------------------------
collect_results <- function(directory_path, output_path, pattern = "\\.RDS$") {
  
  file_paths <- list.files(path = directory_path, pattern = pattern, full.names = TRUE)
  file_paths <- file_paths[!grepl("run_config", file_paths)]
  
  if (length(file_paths) == 0) {
    log_msg("WARNING: No result files matching pattern ", pattern, " found in ", directory_path)
    return(NULL)
  }
  
  combined_df <- map_dfr(file_paths, function(file) {
    tryCatch({
      readRDS(file)
    }, error = function(e) {
      log_msg("WARNING: Failed to read ", file, ": ", e$message)
      return(NULL)
    })
  })
  
  saveRDS(combined_df, output_path)
  return(combined_df)
}