#' Parallel Execution Function using foreach and doParallel
#'
#' This function applies a user-specified function (\code{FUN}) to each element of an iterable
#' (a vector or list) in parallel using the \code{foreach} and \code{doParallel} packages.
#'
#' @param iterable A vector or list containing the tasks.
#' @param FUN A function to be applied to each element of \code{iterable}.
#' @param cores Number of cores to use. Defaults to \code{parallel::detectCores() - 1}.
#' @param .packages A character vector of package names to be loaded on each worker.
#' @param .export A character vector of objects to export to each worker.
#' @param progress Logical. If \code{TRUE}, a progress bar is displayed.
#' @param ... Additional arguments passed to \code{FUN}.
#'
#' @return A list of results obtained by applying \code{FUN} to each element of \code{iterable}.
#'
#' @import foreach
#' @import doParallel
#' @import doSNOW
#' @examples
#' \dontrun{
#' # Example 1: Simple computation (square function)
#' square <- function(x) { x^2 }
#' tasks <- as.list(1:10)
#'
#' # Serial execution timing
#' serial_time <- system.time({
#'   serial_result <- lapply(tasks, square)
#' })
#' print(serial_time)
#'
#' # Parallel execution timing using png.parallel
#' parallel_time <- system.time({
#'   parallel_result <- png.parallel(tasks, FUN = square, cores = 2, progress = TRUE)
#' })
#' print(parallel_time)
#'
#' # Check that results are identical
#' all.equal(serial_result, parallel_result)
#'
#' # Example 2: Simulated heavy workload using Sys.sleep
#' heavy_task <- function(x) {
#'   Sys.sleep(0.5)  # 각 작업마다 0.5초 지연
#'   return(x^2)
#' }
#'
#' tasks2 <- as.list(1:10)
#'
#' # Serial execution timing for heavy_task
#' serial_time2 <- system.time({
#'   serial_result2 <- lapply(tasks2, heavy_task)
#' })
#' print(serial_time2)
#'
#' # Parallel execution timing for heavy_task using png.parallel
#' parallel_time2 <- system.time({
#'   parallel_result2 <- png.parallel(tasks2, FUN = heavy_task, cores = 2, progress = TRUE)
#' })
#' print(parallel_time2)
#'
#' # Check that results are identical
#' all.equal(serial_result2, parallel_result2)
#' }
#'
#'
#' @export
png.parallel <- function(iterable, FUN, 
                         cores = parallel::detectCores() - 1, 
                         .packages = NULL, 
                         .export = NULL, 
                         progress = TRUE,
                         ...) {
  if (!requireNamespace("foreach", quietly = TRUE)) {
    stop("Package 'foreach' is required but not installed.")
  }
  if (!requireNamespace("doParallel", quietly = TRUE)) {
    stop("Package 'doParallel' is required but not installed.")
  }
  
  # # Create and register a parallel cluster
  # cl <- parallel::makeCluster(cores)
  # doParallel::registerDoParallel(cl)
  # 
  # # Execute the tasks in parallel
  # results <- foreach::foreach(i = seq_along(iterable), 
  #                             .packages = .packages, 
  #                             .export = .export) %dopar% {
  #                               FUN(iterable[[i]], ...)
  #                             }
  # 
  # # Stop the cluster and return the results
  # parallel::stopCluster(cl)
  # closeAllConnections()
  
  
  
  # 진행률 표시를 위해 SOCK 클러스터 생성 (doSNOW 필요)
  cl <- parallel::makeCluster(cores, type = "SOCK")
  
  if (progress) {
    if (!requireNamespace("doSNOW", quietly = TRUE)) {
      stop("Package 'doSNOW' is required for progress bar support but is not installed.")
    }
    doSNOW::registerDoSNOW(cl)
    total <- length(iterable)
    pb <- txtProgressBar(max = total, style = 3)
    progress_fun <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress_fun)
  } else {
    # progress가 FALSE인 경우 doParallel로 등록
    if (!requireNamespace("doParallel", quietly = TRUE)) {
      stop("Package 'doParallel' is required but is not installed.")
    }
    doParallel::registerDoParallel(cl)
    opts <- list()
  }
  
  results <- foreach::foreach(i = seq_along(iterable),
                              .packages = .packages,
                              .export = .export,
                              .options.snow = opts) %dopar% {
                                FUN(iterable[[i]], ...)
                              }
  
  if (progress) close(pb)
  parallel::stopCluster(cl)
  
  
  return(results)
}
