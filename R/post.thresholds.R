#' Descritavas da amostra a posteriori
#' @keywords internal
post.thresholds = function(x, lims)
{
  c(L0=mean(x<lims[1]),
    L1=mean(x>=lims[1] & x< lims[2]),
    L2 = mean(x>=lims[2] & x< lims[3]),
    L3 = mean(x>=lims[3]) )
}
