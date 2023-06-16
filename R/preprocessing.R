#' @export png.vec.remove_outlier
png.vec.remove_outlier <- function(x, const=1.5){
  if(FALSE){
    x <- df$Post1yr_MeanNN
    
    cbind.data.frame(x=x, ifelse( is.na(x) | x<MEAN-const*SD | x > MEAN+const*SD, NA, x )) %>%
      filter(x<MEAN-const*SD | x > MEAN+const*SD)
  }
  # x.NonNa <- na.omit(x)
  # MEAN <- mean(x, na.rm = TRUE)
  # SD <- sd(x, na.rm = TRUE)
  
  quartiles <- quantile(x, probs=c(.25, .75), na.rm = TRUE)
  
  IQR <- IQR(x, na.rm = TRUE)
  Lower <- quartiles[1] - const*IQR
  Upper <- quartiles[2] + const*IQR 
  
  ifelse( is.na(x) | x<Lower | x > Upper, NA, x )
}


#' @export png.vec.remove_text_from_cont
png.vec.remove_text_from_cont <- function(x, cutoff=0.95){
  if(FALSE){
    x <- df$LowSep
    x <- as.double(df$Post1yr_MeanNN)*0.1
    cutoff=0.95
    x <- c("0", "1", "focal", "df", "-0.1", "123.123123", "V1", "123.12", "FU_3yr.123")
  }
  
  tab <- table(x)
  wh.int <- grep( "^(\\-)?[0-9]+(\\.[0-9]+)?$", names(tab) )
  
  tab[wh.int]
  # print( names(tab)[-wh.int] )
  
  if( sum(tab[wh.int])/sum(tab) > cutoff ){
    return( ifelse( x %in% names(tab)[-wh.int], NA, x ) )
  } else {
    return( x )
  }
  
}
