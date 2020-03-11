fnextrm<-function(y,des,prvm) {
  # get the variable names in the expanded data frame
  nmsm<-colnames(des)
  # eliminate the names already in the prvm
  nmsm<-nmsm[!nmsm %in% prvm]
  # get the length of the remaining vector of names
  nmt<-length(nmsm)
  pval<-rep(0,nmt)
  # get the pvalue for the hierarchical model associated with each term in nmsm when
  # combined with the prvm
  for (i in 1:nmt) {
    term<-nmsm[i]
    #checks to see if term is an interaction or quadratic term
    #gets first letter in term
    t1<-substr(term,1,1)
    t2<-substr(term,2,2)
    t3<-substr(term,3,3)
    iquad=FALSE
    if(t1=="I"&& t2=="(") {iquad=TRUE}
    hmt<-term
    if(iquad){hmt<-c(term,t3)}
    cmp<-FALSE
    if(t2 !=  "" && iquad != TRUE){cmp=TRUE}
    if(cmp){hmt<-c(t1,t3,term)}
    hmt<-union(hmt,prvm)
    d2<-data.frame(y=y,des[,hmt])
    m2<-lm(y~(.),data=d2)
    sm2<-summary(m2)
    sm2f<-sm2$fstatistic
    pval[i]<-1-pf(sm2f[1],sm2f[2],sm2f[3])
  }
 
  idx<-which.min(pval)
  term<-nmsm[idx]

  # gets hierarchical model for term whose hierarchical model has the smallest pvalue
  #checks to see if term is an interaction or quadratic term
  #gets first letter in term
  t1<-substr(term,1,1)
  t2<-substr(term,2,2)
  t3<-substr(term,3,3)
  iquad=FALSE
  if(t1=="I"&& t2=="(") {iquad=TRUE}
  mt<-term
  if(iquad){mt<-c(term,t3)}
  cmp<-FALSE
  if(t2!= ""&&iquad!=TRUE){cmp=TRUE}
  if(cmp){mt<-c(t1,t3,term)}
  mt<-union(mt,prvm)
  return(mt)
}
