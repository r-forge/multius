mySample<-function(x,prob,...){
  if(length(x)==1) return(x)
  if(all(prob==0)) prob<-NULL
  sample(x,size=1, prob=prob, ...)
}
