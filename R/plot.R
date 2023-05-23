#' @export png.file.generate_filename
png.file.generate_filename <- function(prefix, ext = "png") {
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


#' @export png.plot.save_figure
png.plot.save_figure <- function(expr, file=png.file.generate_filename("Figure"), print=T, save=T, ...){
  if(FALSE){
    png.plot.save_figure("plot(1)")
  }
  
  # expr <- deparse(substitute(expr))
  # extension <- length( strsplit( file, "\\." )[[1]] )
  extension <- png.str.get_extension(file)
  
  if(save){
    switch(extension,
           png = {
             png(file, ...)
             eval(parse(text=expr))
             dev.off()
           },
           jpeg = {
             jpeg(file, ...)
             eval(parse(text=expr))
             dev.off()
           },
           pdf = {
             pdf(file, ...)
             eval(parse(text=expr))
             dev.off()
           },
           eps = {
             setEPS()
             postscript(file, ...)
             # postscript("./Figure/Manhattan.eps", width=10, height=5)
             eval(parse(text=expr))
             dev.off()
           },
           
           {stop("filetype not recognized")}
    )
  }
  
  #print?
  if (print) eval(parse(text=expr))
  invisible(NULL)
  
}	


#' @import ggplot2
capitalize <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}

#' @export png.labeller
png.labeller <- function(){
  if(FALSE){
    library(tidyverse)
    iris %>% ggplot() +
      geom_point(aes(Sepal.Length, Sepal.Width)) +
      facet_grid(~Species, labeller = png.labeller())
  }
  
  ggplot2::labeller(
    vore = capitalize,
    # conservation = conservation_status,
    conservation2 = ggplot2::label_wrap_gen(10),
    .default = ggplot2::label_both
  )
}
# usage: facet_grid( Group1~Group2, labeller = png.labeller )


# For library(patchwork)
#' @export png.plot.label
png.plot.label <- function(label, angle=0) {
  if(FALSE){
    library(patchwork)
    p1 <- iris %>% filter(Species == "setosa") %>% ggplot() +
      geom_point(aes(Sepal.Length, Sepal.Width))
    (png.plot.label("setosa") / p1) + plot_layout(heights = c(1, 10))

    ggsave(filename=png.file.generate_filename("png.plot.label.png"))
  }
  # usage: png.plot.label("LA", 0)
  ggplot() + 
    geom_text(aes(x = 0, y = 0, label = label), size = 6, fontface = 2, angle=angle) + 
    theme_void()
}


#' @export png.plot.pairs_panel_linear
png.plot.pairs_panel_linear =  function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
                          cex = 1, col.smooth = "black", span = 2/3, iter = 3, ...)  {
  reg <- function(x, y, col) abline(lm(y~x), col=col, cex=1.2) 
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) reg(x[ok], y[ok], col.smooth)
}


#' @export png.plot.pairs_panel_cor
png.plot.pairs_panel_cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  if(FALSE){
    pairs(cbind(iris[,1:3],iris[,1:3]), 
          panel = png.plot.pairs_panel_linear, 
          lower.panel = png.plot.pairs_panel_cor,
          cex = 1.5, pch = 19, col = iris$Species, # adjustcolor(4, .4),
          cex.labels = 2, font.labels = 2)
  }
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  text(0.5, 0.5, txt, cex = 1.7, font = 4)
}


#' @export png.exam.histogram
png.exam.histogram <- function(){
  # Histogram of Exam Scores
  maximum <- 30
  dat <- c(14,16,5,6,14,6,7,4,6,9,15,11,14,25,2,12,24,12,19,20,18,26,0,19,5,8)
  xaxis <- seq((min(dat)%/%5)*5, (maximum%/%5+1)*5, by=5)
  
  MAX <- max(xaxis)
  MIN <- min()
  
  table(dat)
  require(ggplot2)
  ggplot(data.frame(Score=dat),
         aes(x = Score)) + geom_dotplot(binwidth = 1.0) +
    geom_text(aes( label=paste0("Top Score: a perfect ", max(dat), " points"), x=MAX, y=0.99 ), hjust="inward", vjust="inward", size=6) +
    # scale_discrete_manual(0:21) +
    ylab(NULL) +
    labs( title="IntroStat - Homework01" ) +
    theme( axis.line.y = element_blank() ) +
    theme_light(base_size = 14) +
    scale_x_continuous(limits = c(MIN, MAX), breaks = xaxis) + 
    ggsave("IntroStat_Homework01.pdf", height=4, width=10)
  # scale_x_continuous(breaks = c(min(dat), xaxis, maximum))
  # coord_cartesian(xlim = c(20, 48) )
  # expand_limits(x=c(min(dat), maximum))
}





#' @export png.plot.multhist
png.plot.multhist <- function (x, beside = TRUE, freq = NULL, probability = !freq, 
                          nclass=NULL, breaks=NULL, log2=FALSE, plot.it = TRUE, ...){
  if(FALSE){
    png.plot.multhist( list(runif(1000), runif(1000), runif(1000), runif(1000)) )
    
    
    x = list(runif(100), runif(100))
    beside = TRUE
    freq = FALSE
    probability = !freq
    plot.it = TRUE
    col=c("red", "blue", paste0("gray", floor(seq(1, 90, length.out=5))))
    xlab = expression(paste("LD (",r^2,")"))
    ylab = "Density"
    nclass = NULL
    args = list(col, xlab, ylab)
    
    breaks <- c(0, 0.05, 0.1, 0.15, 0.2, 0.3, 0.5, 0.7, 1.0)
    breaks = c(0, 2^(seq(-5, 0)))
    
  }
  
  hist.args <- formals(hist.default)
  args <- list(...)
  hargs <- names(args)[names(args) %in% names(hist.args)]
  hist.args[hargs] <- args[hargs]
  hist.args$plot <- FALSE
  hist.args$nclass <- nclass
  if( !is.null(nclass) ){
    allhist <- hist(unlist(x), nclass=hist.args$nclass, plot = FALSE)
  } else if( !is.null(breaks) ) {
    hist.args$breaks <- breaks
    allhist <- hist(unlist(x), hist.args$breaks, plot = FALSE)
  } else {
    allhist <- hist(unlist(x), hist.args$breaks, plot = FALSE)
  }
  
  if (plot.it) {
    barplot.args <- formals(barplot.default)
    bargs <- names(args)[names(args) %in% names(barplot.args)]
    barplot.args[bargs] <- args[bargs]
    barplot.args$beside <- beside
    if ("ann" %in% names(barplot.args)) 
      barplot.args$ann <- eval(barplot.args$ann, envir = barplot.args)
    barplot.args$... <- barplot.args$inside <- NULL
    if (!"names.arg" %in% bargs) 
      barplot.args$names.arg <- signif(allhist$mids, 2)
    if (is.null(freq)) {
      freq <- if (!missing(probability)) 
        !as.logical(probability)
      else TRUE
    }
    comp <- if (freq) {
      "counts"
    } else comp <- "density"
    
    combhist <- t(sapply(x, function(z) hist(z, breaks = allhist$breaks, 
                                             plot = FALSE)[[comp]]))
    
    if( log2 ){
      barplot.args$names.arg <- paste0( "(", log2( allhist$breaks ) %>% {.[-length(.)]}, ", ", log2( allhist$breaks )[-1], "]" )
    }
    
    
    if (plot.it){
      # barplot.args$axisnames = FALSE
      bar <- do.call("barplot", c(list(combhist), barplot.args))
      # barplot.args
      # axis( 1, allhist$breaks )
      # if( !log ){
      #   axis( 1, get(paste0("log", log))(allhist$breaks) )
      # }
      
    }
    
    invisible(list(allhist, combhist))
  }
}



#' @import tidyr
png.plot.test_normality <- function(x, test=c("shapiro", "ks"), bestNormalize=FALSE){
  if(FALSE){
    hist_ordinary <- png.plot.test_normality(moonBook::acs$TG, test="shapiro", bestNormalize = FALSE)
    hist_best <- png.plot.test_normality(moonBook::acs$TG, test="shapiro", bestNormalize = TRUE)
    
    hist_ordinary %>% ggsave(filename=png.file.generate_filename("png.plot.test_normality-ordinary"), width=10, height=5)
    hist_best %>% ggsave(filename=png.file.generate_filename("png.plot.test_normality-best"), width=10, height=5)
    
    gridExtra::grid.arrange(
      hist_ordinary, hist_best, nrow=1
    ) %>% ggsave(filename=png.file.generate_filename("png.plot.test_normality-combined"), width=20, height=5)
  }
  
  
  library(dplyr)
  name.x <- deparse(substitute(x))
  test <- match.arg(test)
  
  if( bestNormalize ){
    library(bestNormalize)
    
    func_list <- c("arcsinh_x(x)",
                   "boxcox(x)",
                   "log_x(x)",
                   "x",
                   "orderNorm(x)",
                   "sqrt_x(x)",
                   "yeojohnson(x)")
    tranformed_x <- sapply( func_list, function(y) {
      res <- eval(parse(text=y))
      if( is.vector(eval(parse(text=y))) ){
        res
      } else {
        res$x.t
      }
    })
  } else {
    func_list <- c("1/x^2",
                   "1/x",
                   "1/sqrt(x)",
                   "log(x)",
                   "sqrt(x)",
                   "x",
                   "x^2",
                   "x^(-0.014)")
    tranformed_x <- sapply( func_list, function(y) eval(parse(text=y)) )
  }
  
  
  pvalue <- NULL
  for( j in 1:ncol(tranformed_x) ){
    
    xj <- tranformed_x[,j,drop=T]
    if( test == "shapiro" ){
      pvaluej <- shapiro.test(xj)$p.value
    } else if ( test == "ks" ){
      pvaluej <- ks.test(xj, "pnorm")$p.value
    }
    
    pvalue[j] <- max( pvaluej, 1e-16 )
    
  }
  
  library(ggplot2)
  p <- tidyr::gather(as.data.frame(tranformed_x), variable, value) %>% 
    mutate(variable=factor(variable, levels=func_list),
           pvalue=factor(variable, levels=func_list, labels=ifelse( pvalue<1e-3, "p < 0.001", paste0("p = ", round(pvalue,3)) ))) %>% 
    group_by(variable) %>% 
    mutate( density_y = dnorm( x=value, mean=mean(value, na.rm=TRUE), sd=sd(value, na.rm=TRUE))
            # range_x = max(value, na.rm = TRUE),
            # range_y = max(density_y, na.rm = TRUE)
    ) %>% 
    filter(!is.na(value)) %>% 
    ggplot(aes(x=value)) +
    geom_histogram(aes(y=..density..), fill = "lightyellow", color = "black") +
    geom_line(aes(value, density_y), color="red") +
    geom_text(aes( label=pvalue, x=Inf, y=Inf ), hjust="inward", vjust="inward" ) +
    facet_wrap(~variable, scales = "free") +
    theme_bw() +
    scale_x_continuous(name=NULL)+
    labs(title=paste0("Transformation of ", name.x), 
         subtitle = paste0("p values by ", test) )
  p
}


#' @import eulerr
#' @export png.venndiagram
png.venndiagram <- function(set.list, ...){
  if(FALSE){
    png.venndiagram(list(A=1:10, B=2:11, C=3:12, D=4:13)) %>% 
      {deparse(substitute(.))} %>% 
      png.plot.save_figure(file="./Figure-VennDiagram.png" )
  }
  
  # set.list :: a list of sets
  
  # library(venneuler, quietly = TRUE)
  library(eulerr)
  
  venn.mat <- VennDiagram::get.venn.partitions(set.list) %>% 
    select(1:length(set.list), count = ..count..) %>% 
    {apply(., 1, function(x) replicate( x[length(x)], x[-length(x)] ) )} %>% 
    do.call("cbind", .) %>% t
  
  out.venn <- venn(venn.mat, ...)
  
  
  ## Example for exporting a pdf file
  # pdf(file="./Venndiagram.pdf", height = 5, width = 10)
  out.venn %>% plot( labels = list(fontsize = 17), quantities = list(fontsize = 15) ) %>% print
  
  ## With a main title
  # gridExtra::grid.arrange(
  #   grid::grobTree( out.venn %>% plot(labels = list(fontsize = 17), quantities = list(fontsize = 15)) ),
  #   textGrob(expression(bold("Venndiagram")), gp = gpar(fontfamily="serif", fontsize=20, fontface="bold", lineheight=1) ),
  #   nrow=2, ncol=1, vp=grid::viewport(width=1, height=1), layout_matrix = rbind(1, 1), heights = c(4, 0.5)
  # )
  
  ## When you should displays multiple Venndiagrams
  # pdf(file="./Venndiagram.pdf", height = 5, width = 10)
  # gridExtra::grid.arrange(
  # grid::grobTree( out.venn1 %>% plot(labels = list(fontsize = 17), quantities = list(fontsize = 15)) ),
  # grid::grobTree( out.venn2 %>% plot(labels = list(fontsize = 17), quantities = list(fontsize = 15)) ),
  # textGrob(expression(bold("cluster1")),gp=gpar(fontfamily="serif",fontsize=20, fontface="bold",lineheight=1) ),
  # textGrob(expression(bold("cluster2")),gp=gpar(fontfamily="serif",fontsize=20, fontface="bold",lineheight=1) ),
  # ncol=2, vp=grid::viewport(width=1, height=1), heights = c(4, 0.5),
  # layout_matrix = rbind( c(1, 2), c(3, 4) )
  # )
  # dev.off()
  
  
  
  
  
  # venn.cluster1 = VennDiagram::venn.diagram(set.list,
  #                                           fill = 2:4, alpha = 0.0, filename = NULL, cex = 2.5, 
  #                                           cat.cex = 2.5,#, main = "cluster1", main.cex = 3, main.pos = c(0.5, 1.2),
  #                                           cat.dist = rep(0.08, 3))
  # 
  # venn.cluster2 = VennDiagram::venn.diagram(rs.list.cluster2,
  #                                           fill = 2:4, alpha = 0.0, filename = NULL, cex = 2.5, 
  #                                           cat.cex = 2.5,#, main = "cluster2", main.cex = 3, main.pos = c(0.5, 1.2),
  #                                           cat.dist = rep(0.05, 3))
  
  
  out.venn
}



#' @importFrom ComplexUpset upset
#' @export png.venndiagram
png.plot.upset <- function(List){
  if(FALSE){
    List <- list(A=c(1:10), B=c(2:11))
    List <- list(c(1:10), c(2:11), c(3:12), c(2:12))
    png.plot.upset(List)
  }
  
  if( is.null(names(List)) ){
    names(List) <- paste0("S.", 1:length(List))
  }
  
  df_total <- cbind.data.frame( ID=List[[1]], " "=TRUE ) %>% {.[!duplicated(.),]}
  for( i in 2:length(List) ){
    df <- cbind.data.frame( ID=List[[i]], " "=TRUE ) %>% {.[!duplicated(.),]}
    df_total <- suppressWarnings( merge(df_total, df, by="ID", all=TRUE) )
  }
  colnames(df_total) <- c("ID", names(List))
  
  for( j in 2:ncol(df_total) ){
    df_total[,j][is.na(df_total[,j]) | df_total[,j] == "NA"] <- FALSE
  }
  
  
  
  library(ggplot2)
  library(ComplexUpset)
  
  # pdf(file=paste0("./Figure - SampleOverlap.pdf"), width=9, height=6)
  print( ComplexUpset::upset(df_total, colnames(df_total)[-1], name="", min_size=1, width_ratio=0.15) )
  # + ggtitle("TITLE")
  # dev.off()
  
}


#' @export png.plot.par_example
png.plot.par_example <- function(){
  pdf(file=png.file.generate_filename("example_of_par"), width=10, height=10)
  png.par <- par(mfrow = c(2,2),
                 oma = c(5,4,0,0) + 0.1,
                 mar = c(0,0,1,1) + 0.1)
  
  for( i in 1:4 ){
    plot(1:10, col=1:10, type="b", pch=18, lwd=2, cex=5)
  }
  
  par(png.par)
  dev.off()
}


#' @export png.plot.hangul
png.plot.hangul <- function(){
  # In a histogram, highlight specific samples from the total samples.
  
  library(ggplot2)
  dat <-rnorm(80)
  dat <-data.frame(dat)
  p <- ggplot(dat, aes(x=dat))+geom_histogram()
  ## filtering...특정 데이터만 추출하기
  dat_filtered <- dat %>% filter(dat >= -.5 & dat <= .5)
  
  p_new <- p + geom_rug(data = dat_filtered, aes(x = dat), colour="blue",  inherit.aes = F) +
    xlab("한글테스트")
  
  # Hangul Font
  # https://r-graphics.org/recipe-output-fonts-pdf
  
  library(extrafont)
  ggsave(p_new, filename = "tmp.pdf")
  embed_fonts("tmp.pdf")
  
}


#' @export png.plot.margin_example
png.plot.margin_example <- function(){
  # Margins area
  par(oma=c(3,3,3,3)) # all sides have 3 lines of space
  par(mar=c(5,4,4,2) + 0.1)
  
  # Plot
  plot(0:10, 0:10, type="n", xlab="X", ylab="Y") # type="n" hides the points
  
  # Place text in the plot and color everything plot-related red
  text(5,5, "Plot", col="red", cex=2)
  box(col="red")
  
  # Place text in the margins and label the margins, all in forestgreen  
  mtext("Margins", side=3, line=2, cex=2, col="forestgreen")  
  mtext("par(mar=c(b,l,t,r))", side=3, line=1, cex=1, col="forestgreen")  
  mtext("Line 0", side=3, line=0, adj=1.0, cex=1, col="forestgreen")  
  mtext("Line 1", side=3, line=1, adj=1.0, cex=1, col="forestgreen")  
  mtext("Line 2", side=3, line=2, adj=1.0, cex=1, col="forestgreen")  
  mtext("Line 3", side=3, line=3, adj=1.0, cex=1, col="forestgreen")  
  box("figure", col="forestgreen")  
  
  # Label the outer margin area and color it blue  
  # Note the 'outer=TRUE' command moves us from the figure margins to the outer margins.  
  mtext("Outer Margin Area", side=1, line=1, cex=2, col="blue", outer=TRUE)  
  mtext("par(oma=c(b,l,t,r))", side=1, line=2, cex=1, col="blue", outer=TRUE)  
  mtext("Line 0", side=1, line=0, adj=0.0, cex=1, col="blue", outer=TRUE)  
  mtext("Line 1", side=1, line=1, adj=0.0, cex=1, col="blue", outer=TRUE)  
  mtext("Line 2", side=1, line=2, adj=0.0, cex=1, col="blue", outer=TRUE)  
  box("outer", col="blue") 
}