library(rgl)

#' Plot a 3D scatter plot
#'
#' This function plots a 3D scatter plot using the \code{rgl} package.
#'
#' @param df A data frame with 3 columns representing the x, y, and z coordinates.
#' @param col The color of the points in the plot. Default is "cornflowerblue".
#'
#' @return None
#'
#' @examples
#' df <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9))
#' Plot3D(df)
#'
#' @import rgl
#'
#' @export
Plot3D <- function(df, col = "cornflowerblue"){
  # Check if ncol == 3
  if (ncol(df) != 3) {
    stop("df must have 3 columns")
  }
  labels = colnames(df)
  rgl::plot3d(df$A, df$B, df$C, type = "s", col = col, size = 1,
              xlab = labels[1], ylab = labels[2], zlab = labels[3])
}