FitDefSc<-function(y,design,alpha=.05)
{
  # get row sums of absolute design elements
  abdf<-as.matrix(abs(design))
  ncl<-ncol(abdf)
  # checks feasibility
  if (ncl < 4) {stop("In Definitive Screening Designs produced by DefScreen factors must be > 3") }
  if (ncl > 12) {stop("In Definitive Screening Designs produced by DefScreen factors must be < 13") }
  if (alpha < .05) {stop("alpha must be in the range (0.05 - 0.20") }
  if (alpha > .20) {stop("alpha must be in the range (0.05 - 0.20") }
  colv<-c(rep(1,ncl))
  vecrs<-abdf%*%colv
  # get index of rows with zero sums
  cntrpr<-which(vecrs==0)
  #get pure error ss & df
  pedf<-0
  if(length(cntrpr)>1) {
    ycpr<-y[cntrpr]
    pess<-(length(ycpr)-1)*var(ycpr)
    pedf<-length(ycpr)-1} 
  # get main effects
  modme<-lm(y~(.),data=design)
  Smry<-summary(modme)
  Aov<-anova(modme)
  # main effect p-values
  Smry$coefficients[,4]
  # main effect labels
  menames<-rownames(Smry$coefficients)[-1]
  #effects with p-values less than .2
  sige<-Smry$coefficients[Smry$coefficients[,4]<.2,4]
  nsigme<-names(sige)[-1]
  # get main effects with smallest p-values
  test<-Smry$coefficients[,4]
  test2<-test[-1]
  nime<-ceiling(.7*length(test2))
  ime<-sort(test2)
  sige2<-ime[c(1:nime)]
  nsigme2<-names(sige2)
  # get union of the effects with pvalues less than .2
  nsigme<-union(nsigme,nsigme2)
  nsme<-length(nsigme)
  # reduced design matrix containing union of effects 
  subdes<-design[,nsigme]
  # call fhstep iteratively to get add largest 2nd order effect
  pvalnt<-.0499999
  while (pvalnt<alpha) {
  result<-fhstepDS(y,subdes,m=nsme,c=0,prvm=nsigme) ## invalid model formula in extractvars at second step
  trm<-result[[1]]
  prvm<-nsigme
  # get the reduced design matrix with columns for terms in the new model
  ndes<-result[[2]]
  ndesign<-data.frame(ndes)
  names(ndesign)<-colnames(ndes)
  ndesign<-ndesign[,trm]
  # find the name of the new term entering the model
  trm2<-trm[!trm %in% prvm]
  # get summary and Anova from last model
  so<-Smry
  ao<-Aov
  #Fit the new model and replace Smry
  modn<-lm(y~(.),data= ndesign)
  Smry<-summary(modn)
  Aov<-anova(modn)
  p<-Smry$coefficients[,4]
# get the pvalue of the term just entered
  p<-p[-1]
  np<-names(p)
  for (i in 1:length(np)) {
    np[i]<-sub("`","",np[i])
    np[i]<-sub("`","",np[i])
    if(np[i]==trm2){pvalnt<-p[i]}
  }
  nsigme<-trm
  }
#get the coefficients for the final model
ncfs<-so$coefficients[,1]
nmncfs<-names(ncfs)
# eliminate the ` around names
for (i in 1:length(nmncfs)) {
  nmncfs[i]<-sub("`","",nmncfs[i])
  nmncfs[i]<-sub("`","",nmncfs[i])
}
# Replace the names
rownames(so$coefficients)<-nmncfs
# print summary of the final model
print(so)
# get residual ss and df 
if(pedf>1){
  ssq<-ao$'Sum Sq'
  Rssq<-ssq[length(ssq)]
  dfo<-ao$'Df'
  Rdf<-dfo[length(dfo)]
  # get Lof SS and df
  sslof<-Rssq-pess
  dflof<-Rdf-pedf  
  Flof<-(sslof/dflof)/(pess/pedf)
  plof<-1-pf(Flof,dflof,dflof)
  # combine in data frame
  ssv<-c(sslof,pess)
  dfv<-c(dflof,pedf)
  Fv<-c(as.character(round(Flof,digits = 5)),"")
  Pvv<-c(as.character(round(plof,digits = 5)),"")
  testlof<-data.frame(ssv,dfv,Fv,Pvv)
  colnames(testlof)<-c("Sums of Squares", " df", "F-value", "P-value")
  rownames(testlof)<-c("Lack of Fit", "Pure Error")
  print(testlof)
}
}
