mergePartitions <- function(podatki_ena, podatki_dva){
  osip <- "incoming"
  novi <- "outgoing"

  rownames(podatki_ena) <- podatki_ena[,1]
  rownames(podatki_dva) <- podatki_dva[,1]

  skupni_podatki <- merge(podatki_ena, podatki_dva, by = "row.names", all = TRUE)[, c(1, 3, 5)]
  skupni_podatki <- data.frame(skupni_podatki)
  skupni_podatki[,2] <- as.character(skupni_podatki[,2])
  skupni_podatki[,c(2)][is.na(skupni_podatki[,c(2)])]<- osip
  skupni_podatki[,3] <- as.character(skupni_podatki[,3])
  skupni_podatki[,c(3)][is.na(skupni_podatki[,c(3)])]<- novi
  return(skupni_podatki)
}
