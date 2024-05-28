#-------------------------------------------------------------------------------
#' @#' Plot the cumulative distribution of number of studies
#'
#' `bmdh.cumdist.plot` plots the cumulative distribution of number of studies
#'
#' @param to.file If TRUE, send the plot to a file
#' @param toxval.db Database version
#' @param sys.date The date of the database export
#' @return writes a plot to a file
#' @export 
#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[openxlsx]{read.xlsx}}
#'  \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{labs}}, \code{\link[ggplot2]{stat_ecdf}}, \code{\link[ggplot2]{ggtheme}}, \code{\link[ggplot2]{lims}}, \code{\link[ggplot2]{ggsave}}
#'  \code{\link[ggpubr]{ggarrange}}
#'  \code{\link[grDevices]{dev}}
#' @rdname bmdh.cumdist.plot
#' @importFrom openxlsx read.xlsx
#' @importFrom ggplot2 ggplot aes ggtitle stat_ecdf theme_bw xlim xlab ylab ggsave
#' @importFrom ggpubr ggarrange
#' @importFrom grDevices dev.off
#-------------------------------------------------------------------------------
bmdh.cumdist.plot <- function(to.file=F,toxval.db="res_toxval_v95",sys.date="2024-02-28") {
  printCurrentFunction()
  dir = "data/"
  file = paste0(dir,"results/ToxValDB BMDh per chemical ",toxval.db," ",sys.date,".xlsx")
  print(file)
  mat = openxlsx::read.xlsx(file)
  file = paste0(dir,"TSCA ACTIVE.xlsx")
  print(file)
  tsca = openxlsx::read.xlsx(file)

  p1 = ggplot2::ggplot(data=mat,ggplot2::aes(x=studies))  +
    ggplot2::ggtitle(paste0("Percentiles All Chemicals: ",nrow(mat))) +
    ggplot2::stat_ecdf(geom = "step") +
    ggplot2::theme_bw() +
    ggplot2::xlim(1,20) +
    ggplot2::xlab("Number of Studies") +
    ggplot2::ylab("Cumulative Fraction of Chemicals")

  mat = mat[is.element(mat$dtxsid,tsca$dtxsid),]
  p2 = ggplot2::ggplot(data=mat,ggplot2::aes(x=studies))  +
    ggplot2::ggtitle(paste0("Percentiles TSCA Chemicals: ",nrow(mat))) +
    ggplot2::stat_ecdf(geom = "step") +
    ggplot2::theme_bw() +
    ggplot2::xlim(1,20) +
    ggplot2::xlab("Number of Studies") +
    ggplot2::ylab("Cumulative Fraction of Chemicals")

  p3 =  ggpubr::ggarrange(p1, p2,
            labels = c("", ""),
            ncol = 2, nrow = 1)

  print(p3)

  if(to.file) {
    fname = paste0(dir,"results/Percentile x number of studies.pdf")
    ggplot2::ggsave(plot = p3, width = 8, height = 2.5, dpi = 300, filename =fname)
    grDevices::dev.off()
  }
  else browser()
}
