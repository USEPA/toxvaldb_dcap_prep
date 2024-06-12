#-------------------------------------------------------------------------------
#' @param to.file If TRUE, send the plot to a file
#' @param toxval.db Database version
#' @param sys.date The date of the database export
#' @param minstudies - only chemicals with this minimum number of studies will be used in the calculation
#' @param cutoff.logsd Only chemicals with their log SD of BMDh values will be used in the calculation
#' @return Write a file with the results: toxval_PODs_for_BMDh chemical level {toxval.db} {sys.date}.xlsx
#' @export 
#' @title bmdh.percentile.plot
#' @description Plot the BMDs vs the regulatory values for different percentiles and determine the best fit
#' @details Helps determine the optimal percentile. The output file shows the
#' fit statistics for different percentiles, and one should select the one with the lowest RMSE and highest R2.
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[openxlsx]{read.xlsx}}, \code{\link[openxlsx]{write.xlsx}}
#'  \code{\link[stats]{lm}}
#'  \code{\link[RMySQL]{character(0)}}
#'  \code{\link[ggplot2]{ggplot}}, \code{\link[ggplot2]{aes}}, \code{\link[ggplot2]{labs}}, \code{\link[ggplot2]{geom_point}}, \code{\link[ggplot2]{ggtheme}}, \code{\link[ggplot2]{facet_grid}}, \code{\link[ggplot2]{lims}}, \code{\link[ggplot2]{geom_segment}}, \code{\link[ggplot2]{ggsave}}
#'  \code{\link[grDevices]{dev}}
#' @rdname bmdh.percentile.plot
#' @importFrom openxlsx read.xlsx write.xlsx
#' @importFrom stats lm
#' @importFrom RMySQL summary
#' @importFrom ggplot2 ggplot aes ggtitle geom_point theme_bw facet_grid xlim ylim xlab ylab geom_segment ggsave
#' @importFrom grDevices dev.off
#' @importFrom readxl read_xlsx
#' @importFrom dplyr filter mutate case_when
#' @importFrom tidyr drop_na
#' @importFrom writexl write_xlsx
#-------------------------------------------------------------------------------
bmdh.percentile.plot <- function(to.file=FALSE, toxval.db="res_toxval_v95", sys.date=Sys.Date(),
                                 minstudies=3, cutoff.logsd=2) {
  printCurrentFunction()
  dir = "data/"
  file = paste0(dir,"results/ToxValDB BMDh per chemical ",toxval.db," ",sys.date,".xlsx")

  mat = readxl::read_xlsx(file) %>%
    # Filter out entries with less than minstudies
    dplyr::filter(studies >= !!minstudies) %>%
    dplyr::mutate(
      # Note whether logsd values excede specified cutoff
      highsd = dplyr::case_when(
        log.sd > !!cutoff.logsd ~ "Y",
        TRUE ~ "N"
      )
    )

  plist = c(5,10,15,20,25,30,35)
  clist = c("pod_05","pod_10","pod_15","pod_20","pod_25","pod_30","pod_35")
  hlist = c(" 5th percentile","10th percentile","15th percentile","20th percentile","25th percentile","30th percentile","35th percentile")
  nlist = c("percentile","column","abserr","rmse","r2","slope","pval","chemicals")
  res = as.data.frame(matrix(nrow=length(plist),ncol=length(nlist)))
  names(res) = nlist

  pdata = NULL

  tmat = mat %>%
    dplyr::filter(studies >= !!minstudies) %>%
    tidyr::drop_na(pod_hra)

  # Accumulate data
  for(i in seq_len(length(plist))) {
    col = clist[i]
    res[i,"percentile"] = plist[i]
    res[i,"column"] = col

    x = log10(tmat[[col]])
    y = log10(tmat$pod_hra)

    ptemp = tmat[,c(col,"pod_hra","log.sd","name")]
    ptemp[,1] = log10(ptemp[,1])
    ptemp[,2] = log10(ptemp[,2])
    names(ptemp) = c("experiment","RA","log.sd","name")
    ptemp$col = hlist[i]
    ptemp$highsd = "N"
    ptemp[ptemp$log.sd>cutoff.logsd,"highsd"] = "Y"
    pdata = rbind(pdata,ptemp)
    temp = stats::lm(y~x+0)
    stemp = RMySQL::summary(temp)
    slope = stemp$coefficients[1,1]
    p = stemp$coefficients[1,4]
    r2 = stemp$adj.r.squared
    rmse = stemp$sigma

    res[i,"abserr"] = mean(x-y,na.rm=T)
    res[i,"rmse"] = rmse
    res[i,"r2"] = r2
    res[i,"slope"] = slope
    res[i,"pval"] = p
    res[i,"chemicals"] = length(y)
  }
  pdata = pdata[!is.na(pdata$RA),]
  print(res)

  final_model = stats::lm(RA~experiment, data=pdata)
  residuals = as.data.frame(final_model$residuals) %>%
    dplyr::rename(residuals=`final_model$residuals`)

  # Plot results
  p = ggplot2::ggplot(data=pdata,ggplot2::aes(x=experiment,y=RA))  +
    ggplot2::ggtitle(paste0("BMDh Percentiles")) +
    ggplot2::geom_point(size=0.1) +
    ggplot2::theme_bw() +
    ggplot2::facet_grid(~col) +
    ggplot2::xlim(-4,4) + ggplot2::ylim(-4,4) +
    ggplot2::xlab("Experimental") +
    ggplot2::ylab("Human RA") +
    ggplot2::geom_segment(ggplot2::aes(x=-4,xend=4,y=-4,yend=4))
  print(p)

  p_labeled = ggplot2::ggplot(data=pdata,ggplot2::aes(x=experiment,y=RA,label=name))  +
    ggplot2::ggtitle(paste0("BMDh Percentiles")) +
    ggplot2::geom_point(size=0.1) +
    ggplot2::theme_bw() +
    ggplot2::facet_grid(~col) +
    ggplot2::xlim(-4,4) + ggplot2::ylim(-4,4) +
    ggplot2::xlab("Experimental") +
    ggplot2::ylab("Human RA") +
    ggplot2::geom_segment(ggplot2::aes(x=-4,xend=4,y=-4,yend=4)) +
    ggplot2::geom_text(check_overlap=T)

  # Write results to file
  file = paste0(dir,"results/ToxValDB BMDh per chemical percentiles ",toxval.db," ",sys.date,".xlsx")
  writexl::write_xlsx(res,file)

  file = paste0(dir,"results/ToxValDB BMDh per chemical RESIDUALS ",toxval.db," ",sys.date,".xlsx")
  writexl::write_xlsx(residuals,file)

  # Save or view resulting plot
  if(to.file) {
    fname = paste0(dir,"results/ToxValDB BMDh per chemical ",toxval.db," ",sys.date,".pdf")
    #fname = paste0(dir,"bmdh.percentile.plot.pdf")
    ggplot2::ggsave(plot = p, width = 8, height = 2.5, dpi = 300, filename =fname)
    tryCatch({
      grDevices::dev.off()
    }, error = function(err) {
      cat("Failed to shut down device\n")
    })

    fname = paste0(dir,"results/ToxValDB BMDh per chemical LABELED ",toxval.db," ",sys.date,".pdf")
    #fname = paste0(dir,"bmdh.percentile.plot.pdf")
    ggplot2::ggsave(plot = p_labeled, width = 49, height = 20, dpi = 700, filename =fname)
    tryCatch({
      grDevices::dev.off()
    }, error = function(err) {
      cat("Failed to shut down device\n")
    })
  }
  else browser()
}
