addEmpoyee <- function(net, n){
  cbind(rbind(net, matrix(0, ncol = ncol(net), nrow = n)), rbind(matrix(0, nrow = nrow(net), ncol = n), matrix(0, ncol = n, nrow = n)))
}
