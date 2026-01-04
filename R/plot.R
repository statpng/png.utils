#' @importFrom extrafont embed_fonts
#' @export png.plot.save_figure
png.plot.save_figure <- function(expr, file=png.file.generate_filename("Figure", ext="pdf"), print=T, save=T, ...){
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
  
  if( extension == "pdf" ){
    extrafont::embed_fonts(file)
  }
    
  
  #print?
  # if(print) eval(parse(text=expr))
  
  
  return(file)
  
}



#' @export png.savePlotAsImage
png.savePlotAsImage <- function(file=png.file.generate_filename("Figure", ext="eps"), height=500, width=500, ...){
  
  extension <- png.str.get_extension(file)
  rstudioapi::savePlotAsImage(file, format=extension, height=height, width=width, ...)
  
  # if(extension == "eps") png.eps2pdf(file)
  
}




#' @export png.eps2pdf
png.eps2pdf <- function(epsfile, margin=3){
  # ref: https://github.com/arni-magnusson/arni/blob/main/R/eps2pdf.R
  
  if(!file.exists(epsfile))
    stop(epsfile, " not found. Please verify filename.")
  
  if(!is.null(margin)){
    stdout <- tempfile(); on.exit(unlink(stdout))            # stdout -> garbage
    stderr <- tempfile(); on.exit(unlink(stderr), add=TRUE)  # stderr -> bbox
    system(paste("gs -dBATCH -dEPSCrop -dNOPAUSE -sDEVICE=bbox", epsfile,
                 "1>", stdout, "2>", stderr))
    tight <- readLines(stderr, encoding="latin1")  # R postscripts are latin1
    numbers <- substring(tight[substring(tight,1,14)=="%%BoundingBox:"], 16)
    ## E.g. 8 8 70 70
    numbers <- paste(as.numeric(unlist(strsplit(numbers," ")))+
                       c(-margin,-margin,+margin,+margin),collapse=" ")
    ## E.g. 5 5 73 73
    master <- readLines(epsfile, encoding="latin1")
    master[substring(master,1,14)=="%%BoundingBox:"] <- paste("%%BoundingBox:",
                                                              numbers)
    write(master, epsfile)
  }
  system(paste("2pdf",epsfile))
  
  invisible(NULL)
}


#' @import ggplot2
#' @export capitalize
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
png.plot.label <- function(label, angle=0, ...) {
  if(FALSE){
    library(patchwork)
    p1 <- iris %>% filter(Species == "setosa") %>% ggplot() +
      geom_point(aes(Sepal.Length, Sepal.Width))
    (png.plot.label("setosa") / p1) + plot_layout(heights = c(1, 10))

    ggsave(filename=png.file.generate_filename("png.plot.label.png"))
  }
  # usage: png.plot.label("LA", 0)
  ggplot() + 
    geom_text(aes(x = 0, y = 0, label = label), size = 6, fontface = 2, angle=angle, ...) + 
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
  r <- abs(cor(x, y, use="complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  text(0.5, 0.5, txt, cex = 1.7, font = 4)
}




#' @title Scatter plot with Marginal Density Plots
#' @description Creates a scatter plot with marginal density plots on the top and right.
#'
#' @param data A data frame.
#' @param x_var String. The name of the variable for the x-axis.
#' @param y_var String. The name of the variable for the y-axis.
#' @param group_var String. The name of the variable for coloring and grouping.
#' @param palette Optional. The color palette (e.g., "jco", "npg", "aaas").
#' @param size Optional. Point size for the scatter plot.
#' @param alpha Optional. Point alpha transparency for the scatter plot.
#'
#' @return A combined plot object (from cowplot).
#'
#' @export png.plot.scatter.marginal
png.plot.scatter.marginal <- function(data, x_var, y_var, group_var, 
                                    palette = "jco", alpha = 0.7,
                                    ... ) {
  library(ggpubr)
  
  ggscatterhist(
    x = x_var, 
    y = y_var,
    color = group_var,    # 점 색상
    fill = group_var,     # 밀도 플롯 채우기 (color와 맞추기)
    palette = palette,
    margin.plot = "density", # 여백 플롯을 "density"로 지정
    margin.params = list(fill = group_var, color = group_var, alpha = alpha), # 밀도 플롯 설정
    ...
  )
  
  # library(ggpubr)
  # library(cowplot)
  # 
  # # 1. Scatter plot colored by groups
  # sp <- ggscatter(data, x = x_var, y = y_var,
  #                 color = group_var, palette = palette,
  #                 size = size, alpha = alpha) +
  #   border()
  # 
  # # 2. Marginal density plot of x (top panel)
  # xplot <- ggdensity(data, x_var, fill = group_var,
  #                    palette = palette)
  # 
  # # 3. Marginal density plot of y (right panel)
  # yplot <- ggdensity(data, y_var, fill = group_var, 
  #                    palette = palette) +
  #   rotate()
  # 
  # # 4. Cleaning the plots (as in the original code)
  # sp <- sp + rremove("legend")
  # yplot <- yplot + clean_theme() + rremove("legend")
  # xplot <- xplot + clean_theme() + rremove("legend")
  # 
  # 
  # # rel_widths = c(2,1)
  # # rel_heights = c(1,2) 
  # 
  # # 5. Arranging the plot using cowplot
  # plot_grid(xplot, NULL, sp, yplot, ncol = 2, align = "hv", 
  #           rel_widths = rel_widths, rel_heights = rel_heights)
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
#' @export png.plot.test_normality
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
    dplyr::select(1:length(set.list), count = ..count..) %>% 
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




lowerFn <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(alpha=0.4) +
    stat_smooth(method = method, ...) +
    theme_bw()
  p
}


#' @export png.scatter3d
png.scatter3d <- function(df, angle=120, cex=0.5, pch=18, range=TRUE, ...){
  library(scatterplot3d)
  x=df[,1]
  y=df[,2]
  z=df[,3]
  
  if(range){
    xlim=c(min(x),max(x))
    ylim=c(min(y),max(y))
    zlim=c(min(z),max(z))
  } else {
    xlim=c(0,1)
    ylim=c(0,1)
    zlim=c(0,1)
  }
  
  scatterplot3d(x,y,z, pch=pch, angle=angle, color="red", cex.symbols=cex,
                xlim=xlim,
                ylim=ylim,
                zlim=zlim, ...)
  
}



#' @export png.plotly.scatter3d
png.plotly.scatter3d <- function(df, size = 2, unit=FALSE, ...) {
  library(plotly)
  library(dplyr)
  
  if(FALSE){
    df <- matrix(runif(10*3,0,1),10,3)
    unit <- TRUE
    size=2
  }
  colnames(df) <- c("x", "y", "z")
  
  if(unit){
    p <- plot_ly(as.data.frame(df),
                 x = ~x, y = ~y, z = ~z, 
                 type = 'scatter3d', 
                 marker = list(symbol = 3, size = size, color = "grey10",
                               line = list(color="grey10", width=1), ... ) ) %>%
      layout( scene = list(xaxis = list( range=c(-1,1) ),
                           yaxis = list( range=c(-1,1) ),
                           zaxis = list( range=c(-1,1) ))
      )
  } else {
    p <- plot_ly(x = ~x, y = ~y, z = ~z, type = 'scatter3d', data = as.data.frame(df), 
                 marker = list(symbol = 3, size = size, color = "grey70",
                               line = list(color="grey10", width=1), ... ) )
    
  }
  
  
  return(p)
}






#' @export png.ggplot.scale_y_log10
png.ggplot.scale_y_log10 <- function(n){
  scale_y_continuous(trans='log10',
                   breaks=scales::trans_breaks('log10', function(x) 10^x, n=n),
                   labels=scales::trans_format('log10', scales::math_format(10^.x)))
}
  

#' @export png.ggplot.remove_xaxis
png.ggplot.remove_xaxis <- function(title=TRUE){
    if(title){
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    } else {
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    }
}


#' @export png.colors
png.colors <- function(n){
  library(RColorBrewer)
  
  Taxa_cols <- brewer.pal.info[brewer.pal.info$category == 'qual',] %>% 
    # {.[c("Set1","Pastel1","Dark2","Set3"),]} %>% 
    {.[c("Set1","Set3","Pastel1","Dark2","Pastel2", "Set2", "Accent"),]} %>%
    { unlist(mapply(brewer.pal, .$maxcolors, rownames(.))) }
  Taxa_cols[1:n]
}



#' @import plot3D
#' @import plotly
#' @export png.mat2bar3d
png.mat2bar3d <- function(counts_matrix){
  if(FALSE){ counts_matrix <- tab }
  
  # Define a function to add 3D bars
  add_3Dbar <- function(p, x,y,z, width=0.4) {
    w <- width
    add_trace(p, type="mesh3d",
              x = c(x-w, x-w, x+w, x+w, x-w, x-w, x+w, x+w),
              y = c(y-w, y+w, y+w, y-w, y-w, y+w, y+w, y-w),
              z = c(0, 0, 0, 0, z, z, z, z),
              i = c(7, 0, 0, 0, 4, 4, 2, 6, 4, 0, 3, 7),
              j = c(3, 4, 1, 2, 5, 6, 5, 5, 0, 1, 2, 2),
              k = c(0, 7, 2, 3, 6, 7, 1, 2, 5, 5, 7, 6),
              facecolor = rep(toRGB(viridisLite::inferno(6)), each = 2)) 
  }
  
  # Draw the 3D histogram
  fig <- plot_ly()
  for (k1 in 1:nrow(counts_matrix)) {
    for (k2 in 1:ncol(counts_matrix)) {
      fig <- fig %>% add_3Dbar(k1,k2,counts_matrix[k1,k2])
    }
  }
  
  
  fig <- fig %>% layout(#title = list(text = "W와 U의 결합 분포 (Interactive 3D Bar Chart)", y = 0.95), # 제목 위치 조정
    scene = list(
      xaxis = list(
        title = names(dimnames(counts_matrix))[1]#,
        # tickvals = 1:length(u_values), # x축 눈금을 w_values로 설정
        # ticktext = as.character(u_values),
        # tickfont = list(size = 12) # X축 눈금 글자 크기 조절
      ),
      yaxis = list(
        title = names(dimnames(counts_matrix))[2]#,
        # tickvals = 1:length(w_values), # y축 눈금을 u_values로 설정
        # ticktext = as.character(w_values),
        # tickfont = list(size = 12) # X축 눈금 글자 크기 조절
      )#,
      # zaxis = list(title = "경우의 수 (Frequency)"),
      # camera = list(eye = list(x=1.8, y=1.8, z=1.5))
    )#,
    # margin = list(l = 0, r = 0, b = 0, t = 50) # 상단 여백 조절
  )
  fig
}





































# Some Spaital Compositional plots ----
tmp <- function(){
  
  # plotting
  
  
  # 필요한 라이브러리 설치 및 로드
  # install.packages("plotly")
  # install.packages("tidyr")
  # install.packages("ggplot2")
  library(plotly)
  library(tidyr)
  library(ggplot2)
  
  # 1000개의 샘플(스팟)에 대한 가상 데이터 생성
  set.seed(42)
  n_samples <- 1000
  df <- data.frame(
    x = runif(n_samples, 0, 100),
    y = runif(n_samples, 0, 100)
  )
  
  # 공간적으로 패턴을 갖는 조성 데이터 생성
  # (x,y) 좌표에 따라 세 feature의 비율이 달라지도록 설정
  v1 <- exp(-((df$x - 20)^2 + (df$y - 80)^2) / 1000)
  v2 <- exp(-((df$x - 80)^2 + (df$y - 80)^2) / 1000)
  v3 <- exp(-((df$x - 50)^2 + (df$y - 20)^2) / 1000)
  
  total <- v1 + v2 + v3
  df$f1 <- v1 / total
  df$f2 <- v2 / total
  df$f3 <- v3 / total
  
  # 데이터 확인
  head(df)
  #          x        y         f1         f2        f3
  # 1 91.48060 83.54764 0.05739328 0.77112009 0.1714866
  # 2 93.70754 21.35327 0.01083411 0.01955138 0.9696145
  # 3 28.61395 65.55392 0.70889981 0.13881476 0.1522854
  # 4 83.04476 53.94932 0.07603310 0.50151121 0.4224557
  
  
  
  
  
  {
    # 1. 각 feature 비율(0-1)을 RGB 값(0-255)으로 변환하고, 16진수 색상 코드로 만듭니다.
    df$rgb_color <- rgb(df$f1, df$f2, df$f3)
    
    # 2. plotly를 이용한 인터랙티브 2D 산점도
    # 각 점의 색상은 방금 계산한 rgb_color를 사용합니다.
    p1 <- plot_ly(
      data = df,
      x = ~x, 
      y = ~y,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        color = ~rgb_color,  # 점의 색상을 rgb_color 컬럼으로 지정
        size = 10,
        opacity = 0.8
      ),
      # 마우스를 올렸을 때 보일 정보
      hoverinfo = 'text',
      text = ~paste("X:", round(x, 2), "<br>Y:", round(y, 2), 
                    "<br>F1 (Red):", round(f1, 3), 
                    "<br>F2 (Green):", round(f2, 3),
                    "<br>F3 (Blue):", round(f3, 3))
    ) %>% layout(
      title = "Spatial Composition Plot (RGB Color Blending)",
      xaxis = list(title = "X coordinate"),
      yaxis = list(title = "Y coordinate")
    )
    
    # 3. (선택) 색상 해석을 돕는 삼각 좌표계(Ternary Plot) 범례 추가
    # 이 범례를 통해 어떤 색이 어떤 비율 조합을 의미하는지 알 수 있습니다.
    p_legend <- plot_ly(
      type = 'scatterternary',
      mode = 'markers',
      a = ~f1, b = ~f2, c = ~f3,  # a,b,c 축에 각 feature 매핑
      data = df,
      marker = list(
        color = ~rgb_color,
        size = 5
      )
    ) %>% layout(
      title = "Color Legend",
      ternary = list(
        aaxis = list(title = 'F1 (Red)'),
        baxis = list(title = 'F2 (Green)'),
        caxis = list(title = 'F3 (Blue)')
      )
    )
    
    # 두 플롯을 나란히 표시
    subplot(p1, p_legend, nrows = 1, widths = c(0.7, 0.3))
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # function 1 ----
  #' 3D 공간 조성 데이터 시각화
  #'
  #' 공간 좌표(x, y)와 여러 feature 비율을 3D 산점도로 시각화합니다.
  #'
  #' @param data 데이터프레임
  #' @param x_col x 좌표에 해당하는 컬럼 이름 (문자열)
  #' @param y_col y 좌표에 해당하는 컬럼 이름 (문자열)
  #' @param feature_cols 조성 비율을 나타내는 컬럼 이름들의 벡터 (문자열)
  #' @param colors 각 feature에 할당할 색상 벡터. `feature_cols`와 길이가 같아야 합니다.
  #' @param title 플롯의 제목
  #' @param marker_size 점의 크기
  #' @param marker_opacity 점의 투명도
  #'
  #' @importFrom tidyr pivot_longer
  #' @importFrom plotly plot_ly layout
  #'
  plot_spatial_composition_3d <- function(data, 
                                          x_col = "x", 
                                          y_col = "y", 
                                          feature_cols, 
                                          colors = c("red", "green", "blue"),
                                          title = "3D Spatial Composition Plot",
                                          marker_size = 4,
                                          marker_opacity = 0.7) {
    
    if(FALSE){
      # 함수 호출
      plot_3d <- plot_spatial_composition_3d(
        data = df,
        x_col = "x",
        y_col = "y",
        feature_cols = c("f1", "f2", "f3"),
        colors = c("#FF5733", "#33FF57", "#3357FF"), # 색상 변경
        title = "My Custom 3D Composition Plot"
      )
      
      # 플롯 출력
      plot_3d
    }
    
    
    # 필수 패키지 확인
    if (!requireNamespace("tidyr", quietly = TRUE) || !requireNamespace("plotly", quietly = TRUE)) {
      stop("이 함수를 사용하려면 'tidyr'와 'plotly' 패키지가 필요합니다.")
    }
    
    # 데이터를 long 형태로 변환
    data_long <- tidyr::pivot_longer(
      data, 
      cols = all_of(feature_cols), 
      names_to = "feature", 
      values_to = "value"
    )
    
    # 3D 산점도 생성
    p <- plotly::plot_ly(
      data = data_long,
      x = ~get(x_col), 
      y = ~get(y_col),
      z = ~value,
      color = ~feature,
      colors = colors,
      type = 'scatter3d',
      mode = 'markers',
      marker = list(size = marker_size, opacity = marker_opacity)
    ) %>% plotly::layout(
      title = title,
      scene = list(
        xaxis = list(title = x_col),
        yaxis = list(title = y_col),
        zaxis = list(title = "Proportion")
      )
    )
    
    return(p)
  }
  
  
  
  
  
  
  
  # function 2 -----
  # Faceted 2D 공간 조성 데이터 시각화
  #
  # 각 feature의 공간적 분포를 별도의 2D 플롯으로 나누어 보여줍니다.
  #
  
  
  #' @param data 데이터프레임
  #' @param x_col x 좌표에 해당하는 컬럼 이름 (문자열)
  #' @param y_col y 좌표에 해당하는 컬럼 이름 (문자열)
  #' @param feature_cols 조성 비율을 나타내는 컬럼 이름들의 벡터 (문자열)
  #' @param color_scale ggplot2에서 사용할 연속형 색상 스케일 (예: "viridis", "plasma")
  #' @param title 플롯의 전체 제목
  #' @param interactive TRUE일 경우 ggplotly를 이용해 인터랙티브 플롯을 반환, FALSE일 경우 ggplot 객체 반환
  #'
  #' @return interactive=TRUE일 경우 plotly 객체, FALSE일 경우 ggplot 객체
  #' @importFrom tidyr pivot_longer
  #' @importFrom ggplot2 ggplot aes geom_point facet_wrap scale_color_viridis_c labs theme_minimal
  #' @importFrom plotly ggplotly
  #'
  plot_spatial_composition_facet <- function(data, 
                                             x_col = "x",
                                             y_col = "y",
                                             feature_cols,
                                             color_scale = "viridis",
                                             title = "Faceted Spatial Composition Plots",
                                             interactive = TRUE) {
    
    if(FALSE){
      
      # 함수 호출 (인터랙티브 버전)
      plot_facet_interactive <- plot_spatial_composition_facet(
        data = df,
        feature_cols = c("f1", "f2", "f3"),
        color_scale = "plasma", # 색상 스케일 변경
        interactive = TRUE
      )
      
      # 플롯 출력
      plot_facet_interactive
      
      
      # 함수 호출 (정적 ggplot 버전)
      plot_facet_static <- plot_spatial_composition_facet(
        data = df,
        feature_cols = c("f1", "f2", "f3"),
        interactive = FALSE
      )
      
      # 플롯 출력 (논문, 보고서 등에 사용하기 좋음)
      plot_facet_static
      
    }
    
    
    # 필수 패키지 확인
    if (!requireNamespace("tidyr", quietly = TRUE) || !requireNamespace("ggplot2", quietly = TRUE)) {
      stop("이 함수를 사용하려면 'tidyr'와 'ggplot2' 패키지가 필요합니다.")
    }
    
    # 데이터를 long 형태로 변환
    data_long <- tidyr::pivot_longer(
      data, 
      cols = all_of(feature_cols), 
      names_to = "feature", 
      values_to = "value"
    )
    
    # ggplot으로 기본 플롯 생성
    p <- ggplot2::ggplot(data_long, ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], color = .data[["value"]])) +
      ggplot2::geom_point(alpha = 0.8) +
      ggplot2::facet_wrap(~feature) +
      ggplot2::scale_color_viridis_c(option = color_scale) +
      ggplot2::labs(title = title, color = "Proportion", x = x_col, y = y_col) +
      ggplot2::theme_minimal()
    
    if (interactive) {
      if (!requireNamespace("plotly", quietly = TRUE)) {
        stop("인터랙티브 플롯을 위해서는 'plotly' 패키지가 필요합니다.")
      }
      return(plotly::ggplotly(p))
    } else {
      return(p)
    }
  }
  
  
}