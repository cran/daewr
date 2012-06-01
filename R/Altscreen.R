Altscreen<-function(nfac) {
library(daewr)
 if (nfac<6) {stop("Alternate screening designs exist only for 6, 7 or 8 factors") }
 if (nfac>8) {stop("Alternate screening designs exist only for 6, 7 or 8 factors") }
 if (nfac==6) {data(D16F6)
               des<-D16F6      }
 if (nfac==7) {data(D16F7)
               des<-D16F7      }
 if (nfac==8) {data(D16F8)
               des<-D16F8      
}
des
}