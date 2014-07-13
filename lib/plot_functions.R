library(ggplot2)
library(lubridate)
require(xtable)
library(reshape)
library(reshape2)

make.file <- function(type=c("png","pdf", "none"), filename, width, height, res){
    ## Pass it file type and dimensions in inches. It creates file.
    type <- match.arg(type)
    ## If no extension given, add one
    if(length(grep(type, filename))==0)
        filename <- paste0(filename,".",type)
    if(type=="png") png(filename, width=width, height=height, units="in", res=res)
    else if(type=="pdf"){pdf(filename, width=width, height=height)}
    else if(dev.cur()==1) dev.new(width=width, height=height)
}
my.dev.off <- function(){
    xx <- names(dev.cur())
    if(length(grep("png", xx))==1 | length(grep("pdf",xx))==1)
        dev.off()
}


add.polygon <- function(data=ts,  y, xvals, alpha.level, alpha.min=0, alpha.max=1){
    ## Pass this a series of years (x) and a matrix of trajectories (df) and it
    ## adds a polygon to the current plot with whose area contains 1-alpha.level
    library(reshape2)
    data <- data[order(data$year),]
    df.wide <- cast(data=data, formula=replicate~year, value=y)[,-1]
    x <- as.numeric(names(df.wide))
    alpha.level <- sort(alpha.level)
    for(i in 1:length(alpha.level)){
        alpha <- alpha.level[i]
        alpha.col <- alpha.min+alpha*(alpha.max-alpha.min)
        col.poly <- rgb(1-alpha.col,1-alpha.col,1-alpha.col, alpha=1)
        quantiles.temp <-  as.matrix(t(apply(df.wide, 2, quantile,
                                             probs=c(alpha/2,1-alpha/2),name=F, na.rm=T)))
        polygon(x=c(x, rev(x)), y=c(quantiles.temp[,1], rev(quantiles.temp[,2])),
                col=col.poly, border=NA)
    }
    return(invisible(df.wide))
}

add.polygon2 <- function(x, y, z, alpha.level, alpha.min=0, alpha.max=1){
    ## Pass this a series of years (x) and a matrix of trajectories (df) and it
    ## adds a polygon to the current plot with whose area contains 1-alpha.level
    library(reshape2)
    alpha.level <- sort(alpha.level)
    for(i in 1:length(alpha.level)){
        alpha <- alpha.level[i]
        alpha.col <- alpha.min+alpha*(alpha.max-alpha.min)
        col.poly <- rgb(1-alpha.col,1-alpha.col,1-alpha.col, alpha=1)
        quantiles.temp <-  as.matrix(t(apply(z, 2, quantile,
                                             probs=c(alpha/2,1-alpha/2),name=F, na.rm=T)))
        polygon(x=c(x, rev(x)), y=c(quantiles.temp[,1], rev(quantiles.temp[,2])),
                col=col.poly, border=NA)

    }
}

plot.scalar.points <- function(data=scalars,x,y,horiz="species", vert=".",
                               vert2=NULL, color="log_max_grad",
                               relative.error=F, axes.free=TRUE, plot=TRUE){
    g <- ggplot(data=data)+ theme_bw()
    if(relative.error){
        g <- g+coord_cartesian(ylim=c(-1,1))+ylab(paste("relative error for:", y))
        g <- g+geom_hline(yintercept=0, col="red")
    }
    if(is.null(vert2) | vert == ".") {
        form <- as.formula(paste(horiz, "~",vert))
    } else {
        form <- as.formula(paste(horiz, "~",vert,"+",vert2))
    }
    g <- g+geom_jitter(aes_string(x=x, y=y, color=color), size=1) +
        scale_color_gradient(low="gray", high="red") +
            facet_grid(form, scales=ifelse(axes.free, "free", "fixed"))
    if(plot) print(g)
    return(invisible(g))
}
plot.scalar.boxplot <- function(data=scalars,x,y,horiz="species", vert=".",
                                vert2=NULL, relative.error=F, axes.free=TRUE,
                                plot=TRUE, rel.ylim=c(-1,1)){
    g <- ggplot(data=data) + theme_bw()
    if(relative.error){
        g <- g+coord_cartesian(ylim=rel.ylim)+ylab(paste("relative error for:", y))
        g <- g+geom_hline(yintercept=0, col="red")
    }
    if(is.null(vert2) | vert == ".") {
        form <- as.formula(paste(horiz, "~",vert))
    } else {
        form <- as.formula(paste(horiz, "~",vert,"+",vert2))
    }
    g <- g+geom_boxplot(aes_string(x=x,y=y), size=.2, outlier.size=1,
                        outlier.colour=rgb(0,0,0,.5)) +
        facet_grid(form, scales=ifelse(axes.free, "free", "fixed"))
    if(plot) print(g)
    return(invisible(g))
}
plot.ts.boxplot <- function(data=ts, y,horiz="species", vert=".", vert2=NULL,
                    relative.error=F, axes.free=TRUE, plot=T, rel.ylim=c(-1,1)){
    g <- ggplot(data=data, aes(x=year))+ xlab("Year")+ xlim(1913,2013)+ theme_bw()
    if(relative.error){
        g <- g+coord_cartesian(ylim=rel.ylim)+ylab(paste("relative error for:", y))
        g <- g+geom_hline(yintercept=0, col="red")
    }
    if(is.null(vert2) | vert == ".") {
        form <- as.formula(paste(horiz, "~",vert))
    } else {
        form <- as.formula(paste(horiz, "~",vert,"+",vert2))
    }
    g <- g+geom_boxplot(aes_string(y=y,group="year"),
                        outlier.colour=rgb(0,0,0,.3), fill=gray(.5), size=.5,
                        outlier.size=.8,lwd=.5) + facet_grid(form,
                        scales=ifelse(axes.free, "free", "fixed"))
    if(plot) print(g)
    return(invisible(g))
}
plot.ts.boxplot.tufte <- function(data=ts, y,horiz="species", vert=".", vert2=NULL,
                    relative.error=F, axes.free=TRUE, plot=T, rel.ylim=c(-1,1)){
    g <- ggplot(data=data, aes(x=year))+ xlab("Year")+
                              coord_cartesian(xlim=c(1910,2016))+ theme_bw()
    library(ggthemes)
    if(relative.error){
        g <- g+coord_cartesian(ylim=rel.ylim,
                               xlim=c(1910,2016))+ylab(paste("relative error for:", y))
        g <- g+geom_hline(yintercept=0, col="red")
    }
    if(is.null(vert2) | vert == ".") {
        form <- as.formula(paste(horiz, "~",vert))
    } else {
        form <- as.formula(paste(horiz, "~",vert,"+",vert2))
    }
    g <- g+geom_tufteboxplot(aes_string(y=y,group="year"),
                        outlier.colour=rgb(0,0,0,.5), fatten=3,
                        outlier.size=.3, lwd=.2) + facet_grid(form,
                        scales=ifelse(axes.free, "free", "fixed"))
    if(plot) print(g)
    return(invisible(g))
}
plot.ts.points <- function(data=ts, y,horiz="species", vert=".", vert2=NULL,
                           color="log_max_grad", relative.error=F,
                           axes.free=TRUE, plot=TRUE){
    g <- ggplot(data=data, aes(x=year))+ xlab("Year")+theme_bw()
    if(relative.error){
        g <- g+coord_cartesian(ylim=c(-1,1))+ylab(paste("relative error for:", y))
        g <- g+geom_hline(yintercept=0, col="red")
    }
    if(is.null(vert2) | vert == ".") {
        form <- as.formula(paste(horiz, "~",vert))
    } else {
        form <- as.formula(paste(horiz, "~",vert,"+",vert2))
    }
    g <- g+geom_jitter(aes_string(y=y,group="year", colour=color), alpha=.5, size=1)+
        facet_grid(form, scales=ifelse(axes.free, "free", "fixed"))+
            scale_color_gradient(low="gray", high="red")
    if(plot) print(g)
    return(invisible(g))
}

# a function Sean wrote for the package but it is not exported
get_args <- function(file) {
  x <- read.csv(file, stringsAsFactors = FALSE, col.names =
    c("arg", "val"), header = FALSE, strip.white = TRUE, sep = ";",
    comment.char = "#")
  y <- as.list(x$val)
  names(y) <- x$arg

# if all numeric then eval(parse(text =
# if has [a-zA-Z]( then eval(parse(text =
# if has : then eval(parse(text =
# else use as character
is_f <- function(x) {
  if(!is.character(x)) stop("x must be a character")
  fn <- grepl("[a-zA-Z0-9]\\(", x) # is a function
  nu <- !grepl("[a-zA-Z]", x) # is not character (is numeric)
  ifelse(fn | nu, TRUE, FALSE)
}

  lapply(y, function(z) {
    if(is_f(as.character(z))) {
      eval(parse(text = z)) # turn into correct class
    } else {
      as.character(z)
    }}
    )
}
