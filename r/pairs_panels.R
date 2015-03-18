## ------------------------------------------------------------------------
#' @title Customization of pairs function
#' @description Customization of pairs function. Diagonal displays histograms. Lower panel displays pearson's r, with values above 0.5 or other specified value flagged by coloring red. 
#' @param df \code{data.frame} data.frame for comparison. all columns will be used, so pass a subset data.frame if you want to limit which colums.
#' @param main \code{character} title for plot
#' @param flag \code{numeric} value between 0 and 1.  above this value, pearson's r will be colored red to flag
#' @return only prints plot
#' @keywords pairs, correlation
#' @export

pairs.custom<-function(df, main=NULL, flag=0.5) {

     panel.pearson <- function(x, y, ...) {
          horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
          vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
          col<-rep("black",length(cor(x,y)))
          col[abs(cor(x,y))>=flag]<-"red"
          text(horizontal, vertical, format(cor(x,y), digits=2),col=col)
     }

     panel.hist <- function(x, ...) {
          usr <- par("usr"); on.exit(par(usr))
          par(usr = c(usr[1:2], 0, 1.5) )
          h <- hist(x, plot = FALSE)
          breaks <- h$breaks; nB <- length(breaks)
          y <- h$counts; y <- y/max(y)
          rect(breaks[-nB], 0, breaks[-1], y, col="gray", ...)
     }
  
     {
     if (!is.null(main))
          pairs(df,
               lower.panel=panel.pearson,
               diag.panel = panel.hist,
               main=main) 
     else     
          pairs(df,
               lower.panel=panel.pearson,
          diag.panel = panel.hist)
     }
}



