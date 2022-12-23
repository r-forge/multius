report.ordinal <- function(ordinalna, dataset, dec = 1){
  # 
  #   if (is.factor(dataset[, which(names(dataset)==ordinalna)]) == FALSE){
  #     dataset[, which(names(dataset)==ordinalna)] <- as.factor(dataset[, which(names(dataset)==ordinalna)])
  #   }
  if (length(ordinalna) > 1) labele <- rownames(sapply(podatki[, ordinalna], FUN = function(x) attr(x, which = "value.labels"))) else labele <- attr(dataset[,ordinalna], which = "value.labels")
  nravni <- length(labele)
  res <- matrix(NA, nrow = length(ordinalna), ncol = nravni + 1)
  colnames(res) <- c(labele, "n") 
  rownames(res) <- ordinalna
  for (i in 1:length(ordinalna)){
    res[i, ] <- c(paste0(table(dataset[, ordinalna[i]]), " (", round(prop.table(table(dataset[,ordinalna[i]]))*100, dec), ")"), sum(!is.na(dataset[,ordinalna[i]])))
  }
  return(res)
}


report.ordinal(ordinalna = paste0("Q16", letters[1:3]), dataset = podatki)
