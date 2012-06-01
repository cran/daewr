Altscreen<-function(nf) {
D16F6 <- D16F7 <- D16F8 <- NULL # sets variables to null value first 
 if (nf<6) {stop("Alternate screening designs exist only for 6, 7 or 8 factors") }
 if (nf>8) {stop("Alternate screening designs exist only for 6, 7 or 8 factors") }
 if (nf==6) {data(D16F6)
            design<-D16F6      }
 if (nf==7) {data(D16F7)
            design<-D16F7      }
 if (nf==8) {data(D16F8)
            design<-D16F8      }
return(design)
}
