OptPB<-function(nruns,nfactors){
  if (nruns != 12 & nruns !=20 ) {stop("This function only works for nruns=12 or nruns=20")}
library(FrF2)
if (nruns==12) {
  if (nfactors < 5) {stop("At least 5 factors required for 12-run PB design")}
  if (nfactors==5 | nfactors==6 | nfactors==7 | nfactors==8) { design<-pb(12,nfactors)
                                                             }
  if (nfactors>8) {stop("With 12 runs you must use between 5 and 8 factors")}
               }
if (nruns==20) {
  if (nfactors <8) {stop("At least 8 factors required for 20-run PB design")}
  if (nfactors == 8) { design<-pb(20,16)
                      design<-design[,c(1,2,3,4,5,13,15,16)]
                     }
  if (nfactors ==9)  { design<-pb(20,16)
                      design<-design[,c(1,2,3,4,5,8,13,15,16)]
                     }
  if (nfactors ==10) { design<-pb(20,17)
                      design<-design[,c(1,2,3,4,6,8,13,14,16,17)]
                      }
  if (nfactors ==11) { design<-pb(20,17)
                      design<-design[,c(1,2,3,4,5,6,8,13,14,16,17)]
                      }
  if (nfactors ==12) { design<-pb(20,17)
                      design<-design[,c(1,2,3,4,5,6,8,10,13,14,16,17)]
                      }
  if (nfactors ==13) { design<-pb(20,18)
                      design<-design[,c(1,2,3,4,5,6,7,8,9,10,14,17,18)]
                     }
  if (nfactors ==14) { design<-pb(20,19)
                      design<-design[,c(1,3,4,5,6,8,9,10,11,12,15,16,18,19)]
                     }
  if (nfactors ==15) { design<-pb(20,19)
                      design<-design[,c(1,2,4,5,6,7,8,9,10,11,12,13,16,17,19)]
                     }
  if (nfactors> 15)  {stop("With 20 runs you must use between 8 and 15 factors")}

               }
return(design)
  } 
