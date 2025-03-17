#' @export png.file.generate_filename
png.file.generate_filename <- function(prefix, ext = "pdf") {
  i <- 1
  while (TRUE) {
    new_filename <- paste0(prefix, "_", i, ".", ext)
    if (!file.exists(new_filename)) {
      break
    }
    i <- i + 1
  }
  return(new_filename)
}


#' @export png.str.remove_extension
png.str.remove_extension <- function(path){
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", path)
}

#' @export png.str.get_extension
png.str.get_extension <- function(path){
  pos <- regexpr("\\.([[:alnum:]]+)$", path)
  ifelse(pos > -1L, substring(path, pos + 1L), "")
}

#' @export png.str.extract_last_dir
png.str.extract_last_dir <- function(path){
  # path <- "/Users/png/Documents/6. Yonsei/1. Mesh3d/data/2826-cut2_poly"
  strsplit(path, "/")[[1]] %>% {.[length(.)]}
}


#' @importFrom gtools mixedorder
#' @export png.mat.order
png.mat.order <- function(mat, row=TRUE, col=TRUE){
  library(gtools)
  if( row ){
    row.order <- mixedorder(rownames(mat))
  } else {
    row.order <- TRUE
  }
  
  if( col ){
    col.order <- mixedorder(colnames(mat))
  } else {
    col.order <- TRUE
  }
  return( mat[row.order, col.order] )
}


#' @export png.mat.get_dim_from_sparse
png.mat.get_dim_from_sparse <- function(sparse_matrix) {
  rows <- sparse_matrix@i + 1
  cols <- findInterval(seq(sparse_matrix@x) - 1, sparse_matrix@p[-1]) + 1
  list(rows = rows, cols = cols)
}


#' @export png.progress.example
png.progress.example <- function(){
  txt <- " pb <- txtProgressBar(min=0, max=nrow(genotype.hapmap[-1,]), style=3) \n setTxtProgressBar(pb, i)"
  cat(txt)
}


#' @export png.parLapply
png.parLapply <- function(cl, X, FUN, ...) {
  library(doSNOW)
  registerDoSNOW(cl)
  pb <- txtProgressBar(max=length(X))
  on.exit(close(pb))
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress=progress)
  
  foreach(i=X, .combine='rbind', .options.snow=opts) %dopar% {
    FUN(i, ...)
  }
}


#' @export png.venn2
png.venn2 = function(x,y,duplicated=FALSE){
  inner = intersect(x,y)
  xnyc = x[!x%in%inner]
  ynxc = y[!y%in%inner]
  if(!duplicated){
    inner = unique(inner)
    xnyc = unique(xnyc)
    ynxc = unique(ynxc)
  }
  list( x = xnyc , y = ynxc , inner = inner )
}


#' @export png.venn3
png.venn3 = function(x,y,z){
  
  n1 = unique( x )
  n2 = unique( y )
  n3 = unique( z )
  
  n12 = intersect(n1, n2)
  n13 = intersect(n1, n3)
  n23 = intersect(n2, n3)
  
  n123 = intersect( intersect(n12,n13), n23 )
  
  n1 = n1[ ! n1 %in% union( n12, n13) ]
  n2 = n2[ ! n2 %in% union( n12, n23) ]
  n3 = n3[ ! n3 %in% union( n13, n23) ]
  
  n12 = n12[ ! n12 %in% n123 ]
  n13 = n13[ ! n13 %in% n123 ]
  n23 = n23[ ! n23 %in% n123 ]
  
  list( n1=n1, n2=n2, n3=n3, n12=n12, n13=n13, n23=n23, n123=n123 )  
  
}


#' @export png.vec.nonamed
png.vec.nonamed = function(x){
  out <- x
  names(out) = NULL
  out
}


#' @export png.str.find
png.str.find = function(x, pattern){
  res = x[ grepl(pattern, x) ]
  names(res) = NULL
  res
}


#' @export png.vec.replace
png.vec.replace = function(x, list, b, nonamed=FALSE){
  res = x
  res[list]=b
  if(nonamed) names(res)=NULL
  res
}


#' @importFrom gtools mixedorder
#' @export png.order
png.order <- function(x){
  mixedorder(x)
}

png.cut = function(x, n, nonamed=FALSE){
  if( is.data.frame(x) ) x <- unlist(x)
  res = x
  x = as.numeric(na.omit(x))
  # res[!is.na(res)] = cut( x, seq( floor(min(x)), ceiling(max(x)*1.000000001), length.out=n+1), right=FALSE)
  res[!is.na(res)] = dplyr::ntile(x, n)
  if(nonamed) names(res) = NULL
  res
}





#' @export png.sim.split
png.sim.split <- function(nrep, nsplit = 10){
  n <- floor( nrep / nsplit )
  n1 <- nrep %% nsplit
  n2 <- nsplit - n1
  
  n.vec <- rep( c(n+1, n), c(n1, n2) )
  
  start <- end <- 0
  AA <- NULL
  for( i in 1:length(n.vec) ){
    start <- start + c(1,n.vec)[i]
    end <- end + c(1,n.vec)[i+1]
    
    AA[[i]] <- seq(start, end, by=1)
  }
  
  cat( "The cardinal number of each set =", sapply(AA, length), "\n" )
  
  AA
}




#' @export png.str.sort
png.str.sort <- function(x, prefix){
  if(FALSE){
    x <- c("gppca","gppca_qp_0.1","gppca_qp_0.3","gppca_qp_0.5","multiplicative_1.0e-10","multiplicative_2.5e-09","multiplicative_6.2e-08","multiplicative_1.5e-06","multiplicative_3.9e-05","ppca","ppca_qp_0.1","ppca_qp_0.3","ppca_qp_0.5","simple_1.0e-10","simple_2.5e-09","simple_6.2e-08","simple_1.5e-06","simple_3.9e-05")
    prefix=c("^simple", "^multiplicative", "^ppca$", "^gppca$", "^ppca_qp_\\d", "^gppca_qp_\\d")
  }
  
  purrr::map(prefix, ~ gtools::mixedsort(x[grepl(.x, x)]) ) %>% unlist
}


#' @export png.function.GetArguments
png.function.GetArguments <- function(...){
  x <- substitute(...())
  if(class(x[[1]]) == "call")  sapply(x[[1]][-1] , deparse)
  else sapply(x , deparse)
}



#' @export png.list.replace
png.list.replace <- function(params, LIST){
  # list_to_be_replaced
  if(FALSE){
    LIST=list(kappa.seq=1e-2, gamma.seq=0.5)
  }
  
  for( i in 1:length(LIST) ){
    params[ names(LIST)[i] ] <- LIST[[i]]
  }
  params
}


#' @export png.list.flatten
png.list.flatten <- function(L){
  f <- function(x){
    if(is.atomic(x)){
      list(x)
    }else{
      x
    }
  }
  
  out <- unlist(lapply(L, f), recursive=FALSE)
  while(any(sapply(out, is.list))){
    out <- png.list.flatten(out)
  }
  out
}