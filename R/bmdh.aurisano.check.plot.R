#-------------------------------------------------------------------------------
#' Plot the different between the Aurisano and current BMDh values
#'
#' `bmdh.per.study` plots the different between the Aurisano and current BMDh values
#' @param to.file If TRUE, send the plot to a file
#' @param toxval.db Database version
#' @param sys.date The date of the database export
#' @export
#-------------------------------------------------------------------------------
bmdh.aurisano.check.plot <- function(to.file=F,toxval.db="res_toxval_v95",sys.date="2024-02-28") {
  printCurrentFunction()
  dir = "data/"
  file = paste0(dir,"results/ToxValDB BMDh per study ",toxval.db," ",sys.date,".xlsx")
  print(file)
  res = openxlsx::read.xlsx(file)
  x = res$bmdh
  y = res$bmdh_aurisano
  x = x[!is.na(y)]
  y = y[!is.na(y)]
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
  if(to.file) {
    fname = paste0(dir,"results/bmdh.aurisano.check.plot.pdf")
    ggplot2::ggsave(plot = p, width = 5, height = 5, dpi = 300, filename =fname)
    grDevices::dev.off()
  }
  else browser()
}
