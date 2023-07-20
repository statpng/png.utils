#' @export png.fit.auto
png.fit.auto <- function(df, vars.cont, vars.factor, vars.outcome, vars.cov=NULL){
  if(FALSE){
    df <- df_new
    
    y_cols <- vars.outcome
    z_cols <- vars.cov
    x_cols <- colnames(df) %>% {.[!. %in% c(y_cols, z_cols)]} # c("Catheter")
    
    # z_cols <- c("Age", "Sex", "BMI", "LA")
    # x_cols <- colnames(df) %>% {.[!.%in%z_cols]} # c("Catheter")
    # y_cols <- c("PFV_A0.3","PFV_V0.3")
  }
  
  y_cols <- vars.outcome
  z_cols <- vars.cov
  x_cols <- colnames(df) %>% {.[!. %in% c(y_cols, z_cols)]} # c("Catheter")
  
  
  criterion <- c("estimate", "p.value")
  out_mat <- array(NA, dim=c(length(x_cols), length(y_cols), 2) )
  dimnames(out_mat) <- list(x_cols, NULL, c("estimate", "p.value"))
  for( i in 1:length(x_cols) ){
    for( j in 1:length(y_cols) ){
      
      col_x <- x_cols[i]
      col_y <- y_cols[j]
      
      x <- df[,col_x] %>% unlist
      y <- df[,col_y] %>% unlist
      z <- df[,z_cols]
      
      df.xyz <- cbind.data.frame(x,y,z) %>% filter_all(all_vars(!is.na(.)))
      
      n_unique <- length(unique(x) %>% na.omit)
      
      if( max(prop.table(table(df.xyz$x))) > 0.95 ) next
      
      if( class(x) == "factor" ){
        
        x <- as.factor(x)
        if(n_unique < 100){
          library(grpreg)
          
          X <- df.xyz %>% dplyr::select(-y)
          X.dummy <- X %>% {model.matrix(~.,.)}
          Y <- df.xyz %>% dplyr::select(y) %>% unlist
          group <- c( rep("X", n_unique), colnames(X)[-1] )
          fit <- grpreg(X, Y, group, penalty="grLasso")
          
          out_mat[i,j,"estimate"] <- select(fit, "BIC")$beta[-1][group=="X"] %>% norm("2")
          
          
          fit.full <- (lm(y~x+., data=df.xyz))
          fit.reduced <- (lm(y~.-x, data=df.xyz))
          fit.partial <- anova(fit.reduced, fit.full)
          
          out_mat[i,j,"p.value"] <- fit.partial$`Pr(>F)`[2]
        }
        
      } else if( class(x) %in% c("numeric", "integer") ){
        
        fit <- summary(lm(y~scale(x)+., data=df.xyz)) %>% broom::tidy() %>% filter(term=="scale(x)")
        out_mat[i,j,"estimate"] <- fit %>% .[criterion[1]] %>% as.numeric
        out_mat[i,j,"p.value"] <- fit %>% .[criterion[2]] %>% as.numeric
        
      }
      
    }
  }
  
  out_mat
}


#' @export png.tmp.PFV.saveTable
png.tmp.PFV.saveTable <- function(out_mat, file="Table-autoReg"){
  
  out_mat[,,"estimate",drop=F] %>% 
    apply(., 1,norm,"2") %>% 
    order(decreasing=TRUE) %>% 
    {cbind( dimnames(out_mat)[[1]][.], apply(out_mat[,,"estimate",drop=F],1,norm,"2")[.] ) } %>% 
    write.csv(file=file%+%"-estimate.csv", quote=FALSE)
  
  out_mat[,,"p.value",drop=F] %>% apply(., 1,norm,"2") %>% order(decreasing=FALSE) %>% 
    {cbind( dimnames(out_mat)[[1]][.], apply(out_mat[,,"p.value",drop=F],1,norm,"2")[.] ) } %>% 
    write.csv(file=file%+%"-pvalue.csv", quote=FALSE)
  
}

