#' @export png.sim.filename
png.sim.filename <- function(GRID, title="[]", measure="[]"){
  if(FALSE){
    title <- "glm_vmf_simulation"
    measure <- "MSE"
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- paste0("glm_vmf_simulation_MSE_", timestamp, ".png")
  
  param_info <- sapply(names(GRID), function(param) {
    value_range <- range(GRID[[param]])
    paste(param, "=", paste(value_range, collapse="-"), sep="")
  })
  param_str <- paste(param_info, collapse="_")
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  timestamp <- format(Sys.time(), "%Y%m%d")
  
  filename <- paste0(title, "_", measure, "_", param_str, "_", timestamp, ".pdf")
  
  filename
}

#' @export png.sim.run
png.sim.run <- function(GRID, sim_func, fit_func, result_func, n.results=1, columns=NULL){
  
  if(FALSE){
    GRID <- expand.grid(n=c(50, 500), sd=c(5, 10, 50), iter=1:10)
    sim_func <- function(n,sd) rnorm(n=n, sd=sd)
    fit_func <- function(simdata) mean(simdata)
    result_func <- function(simdata, fit){
      err1 = sqrt( mean( (simdata - fit)^2 ) )
      err2 = sd(simdata)
      c(err1, err2)
    }
    out.df <- png.run_sim(GRID=GRID, sim_func=sim_func, fit_func=fit_func, result_func=result_func, n.results=2, columns=c("A", "B"))
    
    library(tidyr)
    out.df %>% group_by_at(vars(-iter, -value)) %>% 
      summarise(mean=mean(value))
    
    reshape2::dcast(data=out.df2, type ~ paste0("n=",n)+paste0("sd=",sd), mean, value.var="value")
    
  }
  
  
  
  
  if(FALSE){
    
    # library(png.utils)
    # detach("package:png.utils", unload=TRUE)
    library(tidyverse)
    library(png.vMF)
    
    sim_func <- function(n, sd, tau){
      if(FALSE){
        do.call(sim_func, list(n=10, sd=5, tau=10))
      }
      
      sim.glm_vmf(n=n, p=1, q=3, sd=sd, mu=c(0,0,tau), orthogonal=T)
    }
    
    fit_func <- function(simdata){
      glm_vmf_FixedMean_Offset2(X=simdata$X, Y=simdata$Y, lambda=1e-6, eps=1e-12)
    }
    
    result_func <- function(simdata, fit){
      err1 <- mean( (simdata$mu - fit$beta[1,])^2 )
      err2 <- mean( (simdata$B - fit$beta[2,])^2 )
      c(err1, err2)
    }
    
    
    # summarize the results: Figure
    out.df <- png.utils::png.run_sim(
      GRID=expand.grid(n=c(50, 100), sd=c(5, 10, 50), tau=c(10, 50, 100), iter=1:10), 
      sim_func=sim_func, fit_func=fit_func, result_func=result_func, n.results=2, columns=c("mu", "beta"))
    
    out.df %>% 
      ggplot() + 
      geom_boxplot(aes(n, value, group=n)) + 
      facet_grid(type~sd+tau, scales="free", labeller=png.labeller()) +
      ylab("Mean Squared Error")
    
    
    # summarize the results: Table
    out.df %>% reshape2::dcast(sd+tau~type+paste0("n=",n), function(x) mean(is.na(x)), value.var="value")
    
    out.df %>% reshape2::dcast(sd+tau~type+factor(paste0("n=",n),paste0("n=",c(3,10,50))), function(x) mean(x, na.rm=TRUE), value.var="value")
    
    
  }
  
  
  
  
  # dimens <- as.vector(unlist(lapply(GRID, function(x) length(unique(x)) )))
  # out.array <- array(NA, dim=c(dimens, 2))  # 2는 결과 값의 수를 나타냄
  
  out.df <- GRID
  
  if( any(c("iter","it") %in% names(GRID)) ) {
    iter_exists = TRUE
    GRID_without_iter = GRID[, !names(GRID) %in% c("iter", "it") ]
  } else {
    iter_exists = FALSE
    GRID_without_iter = GRID
  }
  
  
  if( is.null(columns) ){
    
    for (j in 1:n.results) {
      out.df[[paste("result", j, sep = "")]] <- NA
    }
    
  } else {
    if( length(columns)>1 ){
      for (j in 1:n.results) {
        out.df[[columns[j]]] <- NA
      }
    } else {
      for (j in 1:n.results) {
        out.df[[paste(columns, j, sep = "_")]] <- NA
      }
    }
  }
  
  result_col_names <- colnames(out.df)[-(1:(ncol(out.df)-n.results))]
  
  
  for (i in 1:nrow(GRID)) {
    if(i%%10==0) print(paste0(i, " / ", nrow(GRID)))
    params <- as.list(GRID_without_iter[i,])
    
    indices <- as.integer( lapply(names(GRID_without_iter), function(col) which(unique(GRID[[col]]) == params[[col]])) )
    
    simdata <- do.call(sim_func, params)
    
    fit <- try( do.call(fit_func, list(simdata = simdata)) )
    
    if(class(fit) == "try-error"){
      results <- NA
    } else {
      results <- do.call(result_func, list(simdata = simdata, fit = fit))
    }
    
    for (k in 1:length(results)) {
      out.df[i, result_col_names[k]] <- results[k]
    }
  }
  
  library(tidyr)
  out.df2 <- as.data.frame( gather(data=out.df, key = type, value = value, -colnames(GRID)) ) %>% mutate(across(-value, as.factor))
  
  # out.df.mean <- as.data.frame( out.df2 %>% group_by_at(vars(-iter, -value)) %>% summarise(mean=mean(value)) )
  
  
  return(out.df2)
  
}



#' @export png.getwd
png.getwd <- function(){
  dirname(rstudioapi::getSourceEditorContext()$path)
}


#' @export png.sim.ListFilesExtract
png.sim.ListFilesExtract <- function(ListFiles, ...){
  
  # pattern = "II=(\\d+)_ii=(\\d+)"
  # group = c(1,2)
  # ListFiles <- list.files(path %+% "/run", ".RData", full.names=TRUE) %>% gtools::mixedsort()
  
  # ListFiles_II <- stringr::str_extract(ListFiles, "II=(\\d+)_ii=(\\d+)", group=c(1,2))
  ListFiles2 <- stringr::str_extract(ListFiles, ...)
  FinalIndex <- ListFiles2 %>% { tapply(.[,2], .[,1], function(x) max(as.numeric(x))) }
  ListFiles[ sapply( FinalIndex, function(x) which( ListFiles2[,2] %in% x ) ) ]
}








#' @export png.grid2array
png.grid2array <- function(grid){
  dims <- sapply(grid, function(x) length(unique(x)))
  grid2array <- array(NA, dim = dims, dimnames = lapply(grid, unique))
  grid2array
}

#' @export png.array2df
png.array2df <- function(arr){
  as.data.frame.table(arr, responseName = "value")
}


#' @export png.abind
png.abind <- function(arr, dim.list){
  # dim.list = list( measure=c("wald", "pvalue") )
  
  for( i in 1:length(dim.list) ){
    dim <- dim.list[[i]]
    if( i == 1 ){
      new_arr <- abind::abind( lapply(dim, function(x) arr), along = length(dim(arr))+1 )
    } else {
      new_arr <- abind::abind( lapply(dim, function(x) new_arr), along = length(dim(new_arr))+1 )
    }
  }
  
  dimnames(new_arr) <- c(dimnames(arr), dim.list)
  
  new_arr
}



#' @export png.sim.grid2index
png.sim.grid2index <- function(grid, arr){
  target_cols <- colnames(grid)[ colnames(grid) %in% names(dimnames(arr)) ]
  
  colname <- target_cols[1]
  
  result.mat <- as.data.frame(matrix(NA, nrow(grid), ncol(grid)))
  colnames(result.mat) <- colnames(grid)
  for( i in 1:nrow(grid) ){
    for( colname in target_cols ){
      cur_value <- unique(grid[i,colname])
      total_values <- dimnames(arr)[[ which(names(dimnames(arr)) == colname) ]]
      
      result.mat[i,colname] <- which( total_values %in% cur_value )
    }
  }
  
  result.mat
  
}






#' @export png.array.combine
png.array.combine <- function(...) {
  if(FALSE){
    # 예시 array 생성
    arr1 <- arr2 <- arr3 <- array(NA, dim=c(3,3,3))
    
    # 각 array에 값 할당 (예시)
    arr1[1,1,1] <- 1
    arr2[2,2,2] <- 2
    arr3[3,3,3] <- 3
    
    png.array.combine(arr1, arr2, arr3)
  }
  
  arrays <- list(...)
  
  # 모든 array가 같은 차원을 가지는지 확인
  if (length(unique(lapply(arrays, dim))) != 1) {
    stop("All arrays must have the same dimensions")
  }
  
  # 결과 array 초기화
  result <- arrays[[1]]
  
  # 각 array의 non-NA 값을 결과 array에 할당
  for (arr in arrays) {
    non_na <- !is.na(arr)
    result[non_na] <- arr[non_na]
  }
  
  return(result)
}
