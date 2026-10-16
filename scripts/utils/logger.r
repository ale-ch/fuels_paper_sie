log_file_path <- NULL

setup_logging <- function(output_dir, config = NULL) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  log_file_path <<- file.path(output_dir, paste0("execution_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
  file.create(log_file_path)
  
  log_msg("=== PIPELINE INITIALIZED ===")
  log_msg("Output directory: ", output_dir)
  
  if (!is.null(config)) {
    log_msg("Configuration Setup:")
    config_text <- paste(capture.output(print(config)), collapse = "\n")
    cat(config_text, "\n\n", file = log_file_path, append = TRUE)
  }
}

log_msg <- function(...) {
  msg <- sprintf("[%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste0(...))
  cat(msg)
  if (!is.null(log_file_path) && file.exists(log_file_path)) {
    cat(msg, file = log_file_path, append = TRUE)
  }
}
