################# nekaj koristnih funkcij #######################
# zaokrozi p-vrednost
# printP <- function(p){
#   if (p<0.01) return("p < 0.01")
#   if (p==0.01) return("p = 0.01")
#   if (p<0.05 & p>0.01) return("p < 0.05")
#   if (p==0.05) return("p = 0.05")
#   if (p<0.10 & p>0.05) return("p < 0.10")
#   if (p==0.10) return("p = 0.10")
#   else return(paste("p = ", round(p, 2)))
# }

# #naredi faktor na podlagi imen vrednosti, ki o shanjeni v atributih - za podatke, ki so prebrani iz SPSS-ove datoteke
# makeFactorLabels<-function(x,reduce=TRUE,...){
#   lab<-attr(x,"value.labels")
#   if(!is.null(lab)){
#     lab<-sort(lab)
#     x<-factor(x,levels=lab,labels=names(lab),...)
#     if(reduce) x<-factor(x)
#     return(x)
#   }else{
#     warning("The suplied argument does not contain the attribute \"value.labels\".\nThe unchanged argument is returned!")
#     return(x)
#     #stop("The suplied argument does not contain the attribute \"value.labels\"!")
#   }
# }

# #izracuna frekvencno porazdelitev s % in kumulativami
# frekTab<-function(x,dec=NULL,...){
#   tbl<-table(x,...)
#   cumFreq<-cumsum(tbl)
#   perc<-tbl/sum(tbl)*100
#   cumPerc<-cumsum(perc)
#   if(!is.null(dec)){
#     perc<-round(perc,dec)
#     cumPerc<-round(cumPerc,dec)
#   }
#
#   #vse supaj v eno tabelo
#   frekTab<-as.data.frame(cbind(Frekvenca=tbl,"Kum. frek."=cumFreq, "%"=perc,"Kumulativni %" = cumPerc))
#   return(frekTab)
# }

#izracuna korelacijsko matriko skupaj s stevilom enot in stopnjo znacilnosti
# corTestDf<-function(X,...){
#   m<-dim(X)[2]
#   varNames<-colnames(X)
#   #if(is.null(varNames)) varNames<-1:m
#   corMat<-matrix(NA,ncol=m,nrow=m)
#   diag(corMat)<-1
#   dimnames(corMat)<-list(varNames,varNames)
#   pMat<-matrix(NA,ncol=m,nrow=m)
#   diag(pMat)<-1
#   dimnames(pMat)<-list(varNames,varNames)
#   nMat<-matrix(NA,ncol=m,nrow=m)
#   diag(nMat)<-apply(X,2,function(x)sum(!is.na(x)))
#   dimnames(nMat)<-list(varNames,varNames)
#   for(i in 1:(m-1)){
#     for(j in (i+1):m){
#       tmp<-cor.test(x=X[,i],y=X[,j],...)
#       corMat[i,j]<-corMat[j,i]<-tmp$estimate
#       pMat[i,j]<-pMat[j,i]<-tmp$p.value
#       if(tmp$method[1]=="P"){
#         nMat[i,j]<-pMat[j,i]<-tmp$parameter + 2
#       }else {
#         nMat[i,j]<-nMat[j,i]<-sum(apply(!is.na(X[,c(i,j)]),1,prod))
#       }
#     }
#   }
#   return(list(cor=corMat,p=pMat,n=nMat))
# }

#"lepo" izpise rezultat zgornje funkcije
# printCorTestDf<-function(l, digits=c(3,3), format=NULL){
#   d<-dim(l$cor)
#   dNames<-dimnames(l$cor)
#   if(is.null(format))format<-c(sprintf("%%.%df", digits[1]),sprintf("%%.%df", digits[2]))
#   l$cor<-sprintf(format[1],l$cor)	#as.character(round(l$cor,digits[1]))
#   l$p<-sprintf(format[2],l$p) #as.character(round(l$p,digits[2]))
#   res<-array(NA,dim=c(d,3),dimnames=c(dNames,list(c("cor","p","n"))))
#   res[,,"cor"]<-l$cor
#   diag(res[,,"cor"])<-""
#   res[,,"p"]<-l$p
#   diag(res[,,"p"])<-""
#   res[,,"n"]<-l$n
#   res<-as.table(res)
#   res<-aperm(res,perm=c(1,3,2))
#   print(ftable(res))
#   invisible(res)
# }


#vstavi manjkajoce vrednosti kjer so bile izlocene iz podatkov s funkcijo "na.omit" ali podobno ali na mesta, ki so podana v argumentu
insert<- function(x,ind=na.action(x),insert=NA){
  if(length(ind)==0){
    newx<-x
    warning("The length of ind is 0. The unchanged dataset is returned!")
  } else {
    if(length(dim(x))<2){
      newx<-rep(insert, length(x)+length(ind))
      newx[-ind]<-x
      if(class(x)=="factor") newx<-factor(newx,labels=levels(x))
    } else if (length(dim(x))==2){
      if(length(insert)==1){
        newx<-rep(rep(insert,dim(x)[2]), dim(x)[1]+length(ind))
      } else if(length(insert)==dim(x)[2]){
        newx<-rep(insert, dim(x)[1]+length(ind))
      } else {
        stop("insert must have a length of 1 or equal to the number of columns of x")
      }
      newx<-matrix(newx,ncol=dim(x)[2])
      #newx<-as(newx,class(x))
      newx[-ind,]<-x
    } else stop("Objects with more than 2 dimmensions not supported")
  }
  return(newx)
}


#izracuna vsoto kvadratnih odklonov od popvrecja
ss<-function(x){
  m<-mean(x)
  sum((m-x)^2)
}

#izracuna skupno vsoto kvadratnih odklonov od popvrecja za vse spremenljivke
ssAllVar<-function(x){
  sum(apply(x,2,ss))
}

# za neko razbitje in spremenljivke izracuna vrednost Wardove kriterijske funkcije
# wardKF<-function(x,clu){
#   sum(by(x,INDICES=clu,FUN=ssAllVar))
# }


razsiriPodatke<-function(X,id=rownames(X),
                         varNames=colnames(X)){
  # funkcija za pretvorbo podatkov v obliko,
  # kot jo razumeta funkciji aov ali lm
  n12<-dim(X)
  n<-n12[1]
  m<-n12[2]
  if(is.null(varNames)) varNames<-1:m
  if(is.null(id)) id<-1:n

  res<-NULL
  for(i in 1:m){
    res<-rbind(res,data.frame(id=id,x=X[,i],
                              var=varNames[i]))
  }
  return(res)
}



colinEigen<-function(fit,SPSS=TRUE){
  #funkcija za racunanje lastnih vrednosti in indeksov pogojnosti
  X<-fit["x"]
  if(is.null(X)){
    stop("Fit must include x. See ?lm for details\n")
  }else{
    X<-fit$x
    tXX<-t(X)%*%X
    if(SPSS){
      tXX<-cov2cor(tXX)
    }
    e <- eigen(tXX)
    return(list(eigen=e$values,condIndex=sqrt(e$val[1]/e$val)))
  }
}



# # funkcija za risanje povprecji skupin pri vecih spremenljivkah
# plotMeans<-function(x, by, plotCI=TRUE,alpha=0.05,ylab="povprecja",plotLegend=TRUE,inset = 0.01, xleg="topleft", legPar=list(),gap=0, labels = NULL, ...){
#   tmpM<-aggregate(x =x,by = by, FUN=mean, na.rm=TRUE)
#   matplot(t(tmpM[,-1]),type="o",xaxt="n",ylab=ylab,...)
#   if (is.null(labels)) labels <- colnames(tmpM)[-1]
#   axis(side=1,at=1:dim(tmpM[,-1])[2],las=2, labels, ...)
#   g<-dim(tmpM)[1]
#   res<-list(means=tmpM)
#   if(plotLegend)  {
#     legPar<-c(list(x=xleg,legend = tmpM[,1], pch=as.character(1:g), col=1:min(6,g),lty = 1:min(5,g),inset = inset), legPar)
#     do.call(legend, args = legPar)
#   }
#   if(plotCI&require(gplots)){
#     f<-function(x){
#       n<-sum(!is.na(x))
#       qt(1-alpha/2,df=n-1)*sd(x, na.rm=TRUE)/sqrt(n)
#     }
#     tmpCI<-aggregate(x = x,by = by, FUN=f)
#
#     delta<-as.vector(t(tmpCI[,-1]))
#     y<-as.vector(t(tmpM[,-1]))
#     m<-dim(tmpM)[2]-1
#     x<-rep(1:m, times=g)
#     cols<-rep(1:g, each=m)
#     plotCI(x=x,y=y,uiw = delta, add=TRUE, labels=TRUE, gap=gap, col=cols,type="n")
#     res<-c(res, list(CI=tmpCI))
#   }
#   invisible(res)
# }
