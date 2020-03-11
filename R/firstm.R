firstm<-function(y,des) {
nmsm<-colnames(des)
nmt<-length(nmsm)
pval<-rep(0,nmt)
# get the pvalue for the hierarchical model associated with each term in mmsm 
for (i in 1:nmt) {
  term<-nmsm[i]
  #checks to see if term is an interaction or quadratic term
  #gets first letter in term
  t1<-substr(term,1,1)
  t2<-substr(term,2,2)
  t3<-substr(term,3,3)
  iquad=FALSE
  if(t1=="I" && t2=="(") {iquad=TRUE}
  hmt<-term
  if(iquad){hmt<-c(term,t3)}
  cmp<-FALSE
  if(t2 != "" && iquad!= TRUE){cmp=TRUE}
  if(cmp){hmt<-c(t1,t3,term)}
#  print(i)   was used to check
#  print(hmt)  was used to check
  d2<-data.frame(y=y,des[,hmt])
  m2<-lm(y~(.),data=d2)
  sm2<-summary(m2)
  sm2f<-sm2$fstatistic
  pval[i]<-1-pf(sm2f[1],sm2f[2],sm2f[3])
#  print(pval[i])
}
# gets hierarchical model for term whose hierarchical model has the smallest pvalue
idx<-which.min(pval)
term<-nmsm[idx]
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
if(t2!= ""&& iquad!=TRUE){cmp=TRUE}
if(cmp){mt<-c(t1,t3,term)}
return(mt)
}
