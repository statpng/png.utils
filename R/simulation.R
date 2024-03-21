#' @export run_simulation_generic
run_simulation_generic <- function(GRID, sim_func, fit_func, result_func, n.results=1) {
  
  if(FALSE){
    GRID <- expand.grid(n=c(50, 500), sd=c(5, 10, 50))
    sim_func <- function(n,sd) rnorm(n=n, sd=sd)
    fit_func <- function(simdata) mean(simdata)
    result_func <- function(simdata, fit){
      err1 = sqrt( mean( (simdata - fit)^2 ) )
      err2 = sd(simdata)
      c(err1, err2)
    }
    run_simulation_generic(GRID=GRID, sim_func=sim_func, fit_func=fit_func, result_func=result_func, n.results=3)
  }
  
  # dimens <- as.vector(unlist(lapply(GRID, function(x) length(unique(x)) )))
  # out.array <- array(NA, dim=c(dimens, 2))  # 2는 결과 값의 수를 나타냄
  
  out.df <- GRID
  for (j in 1:n.results) {
    out.df[[paste("result", j, sep = "")]] <- NA
  }
  
  for (i in 1:nrow(GRID)) {
    if(i%%10==0) print(paste0(i, " / ", nrow(GRID)))
    params <- as.list(GRID[i,])
    
    indices <- lapply(names(GRID), function(col) which(unique(GRID[[col]]) == params[[col]])) %>% as.integer
    
    simdata <- do.call(sim_func, params)
    
    fit <- do.call(fit_func, list(simdata = simdata))
    
    results <- do.call(result_func, list(simdata = simdata, fit = fit))
    
    for (k in 1:length(results)) {
      out.df[i, paste("result", k, sep = "")] <- results[k]
    }
  }
  
  return(out.df)
}
