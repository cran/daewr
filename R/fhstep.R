fhstep <- function(y,des,m,c,prvm) {

  #get names of linear terms
  lin<-colnames(des)
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
#gets the model matrix of the full model
#gets the model matrix
lm1<-lm(y~(.)^2,data=dat)
mm<-model.matrix(lm1)
fact<-colnames(mm)
fact<-fact[-1]
fact<-paste(fact,collapse='+')
mod<-paste(c(fact,quad),collapse='+')
lm2<-lm(reformulate(termlabels=mod, response='y'),data=dat) # This works
mm<-model.matrix(lm2)
#deletes the constant column from the model matrix
mm<-mm[,2:ncol(mm)]
#creates data frame with terms from previous model
d1<-data.frame(y=y,mm[,prvm])
#fits the previous model
m1<-lm(y~(.),data=d1)
#finds the term whose hierarchical model added to the previous model
# has the minimum pvalue
term<-fnextrm(y,mm,prvm) 
# fits model model in factors with maximum correlation
d2<-data.frame(y=y,mm[,term])
m2<-lm(y~(.),data=d2)
result<-summary(m2)
print(result)
return(term)
}

