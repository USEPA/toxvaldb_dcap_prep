#-------------------------------------------------------------------------------
#' @param to.file If TRUE, send the plot to a file
#' @param toxval.db Database version
#' @param sys.date The date of the database export
#' @export
#' @title bmdh.aurisano.check.plot
#' @description Plot the difference between the Aurisano and current BMDh values
#' @return None; writes plot to PDF
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[openxlsx]{read.xlsx}}
#'  \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{labs}}, \code{\link[ggplot2]{geom_point}}, \code{\link[ggplot2]{ggtheme}}, \code{\link[ggplot2]{scale_continuous}}, \code{\link[ggplot2]{ggsave}}
#'  \code{\link[grDevices]{dev}}
#' @rdname bmdh.aurisano.check.plot
#' @importFrom openxlsx read.xlsx
#' @importFrom ggplot2 ggplot aes ggtitle geom_point theme_bw xlab ylab scale_x_continuous scale_y_continuous ggsave
#' @importFrom grDevices dev.off
#-------------------------------------------------------------------------------
bmdh.aurisano.check.plot <- function(to.file=FALSE,toxval.db="res_toxval_v95",sys.date=Sys.Date()) {
  printCurrentFunction()
  dir = "data/"
  file = paste0(dir,"results/ToxValDB BMDh per study ",toxval.db," ",sys.date,".xlsx")
  print(file)
  res = openxlsx::read.xlsx(file)

  # Get bmdh and aurisano values where aurisano is not NA
  x = res$bmdh
  y = res$bmdh_aurisano
  x = x[!is.na(y)]
  y = y[!is.na(y)]

  # Generate plot
  p = ggplot2::ggplot(data=res,ggplot2::aes(x=bmdh,y=bmdh_aurisano))  +
    ggplot2::ggtitle(paste0("Check BMDh values")) +
    ggplot2::geom_point(size=0.1) +
    ggplot2::theme_bw() +
    #facet_grid(~col) +
    ggplot2::xlab("Current") +
    ggplot2::ylab("Aurisano") +
    ggplot2::scale_x_continuous(trans = "log10",limits=c(1E-4,1E4)) +
    ggplot2::scale_y_continuous(trans = "log10",limits=c(1E-4,1E4)) #+
    #xlim() +
    #ylim(1E-4,1E4)
    #geom_segment(aes(x=-3,xend=3,y=-3,yend=3))
  print(p)

  # Write plot to file if specified, or display in browser
  if(to.file) {
    fname = paste0(dir,"results/bmdh.aurisano.check.plot.pdf")
    ggplot2::ggsave(plot = p, width = 5, height = 5, dpi = 300, filename =fname)
    grDevices::dev.off()
  }
  else browser()
}
