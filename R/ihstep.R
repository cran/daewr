ihstep <- function(y,des,m,c) {
  #get names of linear terms
  lin<-colnames(des)
#    print(lin)
  lg1=FALSE
#    cat("m=",m,"\n")
#    cat("c=",c,"\n")
  mc<-m+c
  for (i in 1:mc){
    lm<-nchar(lin[i])
#    cat("lm=",lm,"\n")
    if(lm > 1){lg1=TRUE } 
#        cat("lg1=",lg1,"\n")
  }
#    cat("lg1=",lg1,"\n")
  if(lg1) {stop("factor names must be of length 1")} 
  #make replacement table for use in getting quadratic names
  values<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")
  repl<-c("I(A^2)","I(B^2)","I(C^2)","I(D^2)","I(E^2)","I(F^2)","I(G^2)","I(H^2)","I(I^2)","I(J^2)","I(K^2)","I(L^2)","I(M^2)","I(N^2)","I(O^2)","I(P^2)","I(Q^2)","I(R^2)","I(S^2)","I(T^2)","I(U^2)","I(V^2)","I(W^2)","I(X^2)","I(Y^2)","I(Z^2)")
  repl.tab <- cbind(values, repl)
  #get quadratic variable names
  if(m==0) {
    quad<-character()
  } else {
    indx<-rep(0,m)
    for (i in 1:m) {
      indx[i]<-match(lin[i],repl.tab[, 1], nomatch=0)
    }
    quad<-rep('A',m)
    for (i in 1:m) {
      quad[i]<-lin[i]
    }
    quad[indx != 0] <- repl.tab[indx, 2]
    quad<-paste(quad,collapse='+')
  }
  
  # create the data frame for analysis
  dat<-data.frame(y=y,des)
  #gets the model matrix
  lm1<-lm(y~(.)^2,data=dat)
  mm<-model.matrix(lm1)
  fact<-colnames(mm)
  fact<-fact[-1]
  fact<-paste(fact,collapse='+')
  mod<-paste(c(fact,quad),collapse='+')
  #  lmtest<-lm(y~A+B+A:B+I(A^2)+I(B^2),data=dat)
  lm2<-lm(reformulate(termlabels=mod, response='y'),data=dat) # This works
  mm<-model.matrix(lm2)
  #deletes the constant column from the model matrix
  mm<-mm[,2:ncol(mm)]
  # finds the initial hierarchical model with the lowest regression p-value
  trm<-firstm(y,mm)
  d1<-data.frame(y=y,mm[,trm])
  # if only one term in the model get the correct name
  if(length(trm)==1){
    t1<-substr(trm,1,1)
    t2<-substr(trm,2,2)
    t3<-substr(trm,3,3)
    iquad=FALSE
    if(t2=="(") {iquad=TRUE} ### first problem here t2 has length> 1
    #  }
    
    hmt<-trm
    #### second problem here t2 has length> 1
    if(t2=="") {
      nms<-names(d1)
      nms[2]<-hmt   ## 3rd problem here number of items to replace is not a multiple of replacement length
      names(d1)<-nms
    }
  } ## test
  m1<-lm(y~(.),data=d1)
  result<-summary(m1)
  print(result)
  return(trm)
}

