#' Recoding the smallest categories to "other" value in case of too many or too small categories.
#'
#' The smallest categories are recoded to "other" or user specified string. The variables is converted to factor if not  already.
#'
#' @param x The variable to be recoded.
#' @param maxLevels The maximum number of levels after recoding
#' @param minFreq The minimal frequency after recoding.
#' @param otherValue The name give to the new category
#' @param convertNA Should the \code{NA} values be converted to ordinary values. If \code{TRUE}, they are converted to string \code{"NA"}. If \code{FALSE}, there are left as missing and ignored in the recording.
#' @param orderLevels How should the categories be ordered. Possible values are:
#' \itemize{
#' \item \code{FALSE} - do not change the ordering (default)
#' \item \code{alpha} - alphabetically; and
#' \item {freq} - based on frequencies (highest frequencies first).
#' }
#' @param otherLast Only used if category with \code{otherValue} was created. If \code{TRUE}, the \code{otherValue} is placed as last category regardless of the \code{orderLevels} argument. Defaults to \code{FALSE}.
#' @export

small2other<-function(x, maxLevels=12, minFreq=0, otherValue="other", convertNA=TRUE, orderLevels=FALSE, otherLast=FALSE){
  if(!is.factor(x)){
    warning("x will be converted to factor!")
    x<-as.factor(x)
  } else x<-factor(x)
  if(convertNA && (any(is.na(x)))){
    levels(x)<-c(levels(x),"NA")
    x[is.na(x)]<-"NA"
  }
  lev<-levels(x)
  tab<-table(x)
  if(length(tab)>maxLevels){
    tab<-sort(tab, decreasing = TRUE)[1:(maxLevels-1)]
  }

  tab<-tab[tab>minFreq]
  lev[!lev%in%names(tab)]<-otherValue
  levels(x)<-lev
  lev<-levels(x)
  if(orderLevels=="alpha"){
    lev<-sort(lev)
  } else if(orderLevels=="freq"){
    lev<-names(sort(table(x), decreasing = TRUE))
  }
  if(otherLast && otherValue%in%lev) {
    lev<-c(lev[lev!=otherValue],otherValue)
  }

  return(factor(x, levels=lev))
}
