#-------------------------------------------------------------------------------
#' @param dir The directory where the lists are stored
#' @title toxvaldb.statplots
#' @description Plot statistics for ToxValDB for DCAP sources
#' @param to.file Whether to write plots to file or simply view them, Default: F
#' @param toxval.db Database version
#' @param sys.date The date of the database export
#' @return None; plots are generated
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[openxlsx]{read.xlsx}}
#'  \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{labs}}, \code{\link[ggplot2]{geom_boxplot}}, \code{\link[ggplot2]{scale_continuous}}, \code{\link[ggplot2]{scale_manual}}, \code{\link[ggplot2]{coord_flip}}, \code{\link[ggplot2]{ggtheme}}, \code{\link[ggplot2]{theme}}, \code{\link[ggplot2]{element}}, \code{\link[ggplot2]{geom_jitter}}, \code{\link[ggplot2]{geom_freqpoly}}, \code{\link[ggplot2]{facet_wrap}}, \code{\link[ggplot2]{lims}}, \code{\link[ggplot2]{ggsave}}
#'  \code{\link[forcats]{fct_rev}}
#'  \code{\link[stats]{reorder.default}}
#'  \code{\link[gridExtra]{arrangeGrob}}
#'  \code{\link[grDevices]{dev}}
#' @rdname toxvaldb.statplots
#' @export 
#' @importFrom openxlsx read.xlsx
#' @importFrom ggplot2 ggplot aes ggtitle geom_boxplot scale_y_continuous scale_fill_manual coord_flip theme_bw ylab xlab theme element_text margin geom_jitter geom_histogram facet_wrap scale_x_continuous ylim element_rect ggsave
#' @importFrom forcats fct_rev
#' @importFrom stats reorder
#' @importFrom gridExtra grid.arrange
#' @importFrom grDevices dev.off
#-------------------------------------------------------------------------------
toxvaldb.statplots <- function(to.file=F,toxval.db="res_toxval_v95",sys.date=Sys.Date()) {
  printCurrentFunction()
  dir = "data/"
  file = paste0(dir,"results/ToxValDB summary stats ",toxval.db," ",sys.date,".xlsx")
  print(file)
  res = openxlsx::read.xlsx(file)
  #-----------------------------------------------------------------------------
  res$sg2 = res$sg1
  res$pod2 = res$pod1

  x1 = res$pod1/res$sg1
  x2 = res$pod2/res$sg2
  x3 = res$pod3/res$sg3
  x4 = res$pod4/res$sg4
  n1 = x1
  n2 = x2
  n3 = x3
  n4 = x4
  n1[] = "Level 1"
  n2[] = "Level 2"
  n3[] = "Level 3"
  n4[] = "Level 4"
  z1 = cbind(n1,x1)
  z2 = cbind(n2,x2)
  z3 = cbind(n3,x3)
  z4 = cbind(n4,x4)
  z = as.data.frame(rbind(z1,z2,z3,z4))
  names(z) = c("Level","PODs")
  z$PODs = as.numeric(z$PODs)
  title = paste0("")
  p1 = ggplot2::ggplot(data=z,ggplot2::aes(x= forcats::fct_rev(stats::reorder(Level,PODs,FUN=min)),y=PODs))  +
    ggplot2::ggtitle(title) +
    ggplot2::geom_boxplot(outlier.colour="black",
                          outlier.shape=21,outlier.size=2,outlier.fill="white",
                          notch=FALSE) +
    ggplot2::scale_y_continuous(limits=c(1,10),breaks=c(1,2,3,4,5,6,7,8,9,10)) +
    ggplot2::scale_fill_manual(values=c("white","red")) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::ylab("PODS/Study Group") +
    ggplot2::xlab("") +
    ggplot2::theme(axis.text=ggplot2::element_text(size=12),
                   axis.title=ggplot2::element_text(size=16,face="bold"),
                   plot.title=ggplot2::element_text(size=16,face="bold",vjust=0.5,hjust=0.5),
                   strip.text.x = ggplot2::element_text(size = 10),
                   plot.margin = ggplot2::margin(t=5,r=20,b=5,l=20),
                   legend.text = ggplot2::element_text(size=12),
                   legend.title = ggplot2::element_text(size=12)) +
    ggplot2::geom_jitter(size=0.5,alpha = 0.9, width=0.15)

  #-----------------------------------------------------------------------------
  x1 = res$pod1
  x2 = res$pod2
  x3 = res$pod3
  x4 = res$pod4
  n1 = x1
  n2 = x2
  n3 = x3
  n4 = x4
  n1[] = "Level 1"
  n2[] = "Level 2"
  n3[] = "Level 3"
  n4[] = "Level 4"
  z1 = cbind(n1,x1)
  z2 = cbind(n2,x2)
  z3 = cbind(n3,x3)
  z4 = cbind(n4,x4)
  z = as.data.frame(rbind(z1,z2,z3,z4))
  names(z) = c("Level","PODs")
  z$PODs = as.numeric(z$PODs)

  title = paste0("")
  p2 = ggplot2::ggplot(data=z,ggplot2::aes(x=PODs))  +
    ggplot2::ggtitle(title) +
    ggplot2::geom_histogram(binwidth=1) +
    ggplot2::theme_bw() +
    ggplot2::ylab("Chemicals") +
    ggplot2::xlab("PODS/Chemical") +
    ggplot2::facet_wrap(~Level) +
    ggplot2::scale_x_continuous(limits=c(1,20),breaks=c(5,10,15,20)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::ylim(0,1200) +
    ggplot2::theme(axis.text=ggplot2::element_text(size=12),
                   axis.title=ggplot2::element_text(size=16,face="bold"),
                   plot.title=ggplot2::element_text(size=16,face="bold",vjust=0.5,hjust=0.5),
                   strip.text.x = ggplot2::element_text(size = 10),
                   plot.margin = ggplot2::margin(t=5,r=20,b=5,l=20),
                   legend.text = ggplot2::element_text(size=12),
                   legend.title = ggplot2::element_text(size=12),
                   strip.background = ggplot2::element_rect(color="black", fill="white", size=0.1, linetype="solid"),
                   strip.text = ggplot2::element_text( size = 6))

  p = gridExtra::grid.arrange(p1,p2,nrow=2)
  print(p)
  if(!to.file) browser()

  if(to.file) {
    fname = paste0("data/results/toxvaldb.statplots.pdf")
    ggplot2::ggsave(plot = p, width = 5, height = 6, dpi = 300, filename =fname)
    grDevices::dev.off()
  }
}
