################################################################
################################################################
## HierAFS
## RSM forward regression keeping model hierarchy
## y: Response vector
## x: Data frame of x-variables
## step: Number of forward steps requested
## require(daewr)
## if the data frame of x variables has column names of length 1 nm1 should be TRUE
## if the column namees of x are more than length 1 nm1 should be FALSE
HierAFS <- function(y,x,m,c,step) 
  {
  nm1=TRUE
  colx<-ncol(x)
  try(if(m+c!=colx) stop("m+c must equal the number of columns of x"))
  if(nm1==TRUE)
  {
  any.col <- colnames(x)
    xx <- x
    xx$y <- y
    suppressWarnings(invisible(capture.output(res<- ihstep(y,x,m,c))) )
    f <- paste("y~", paste(res,collapse="+"),sep="")
    collect <- data.frame(formula=f, R2= round(summary(lm(formula(f),data=xx) )$r.square,3) )
    
    for (i in (2:step)) {
      suppressWarnings( invisible(capture.output(res <- fhstep(y,x,m,c,res))) )
      f <- paste("y~", paste(res,collapse="+"),sep="")
      #f2 <- paste("y ~",paste(stringi::stri_replace_all_fixed(res,letter.col ,
      #                                                        any.col, vectorize_all = FALSE),collapse="+"))
      collect <- rbind(collect,data.frame(formula=f, R2= round(summary(lm(formula(f),data=xx) )$r.square,3)))
    }
  }  
  if(nm1==FALSE) 
  { 
  any.col <- colnames(x)
  letter.col <- LETTERS[1:ncol(x)]
  colnames(x) <- letter.col
  xx <- x
  xx$y <- y
  suppressWarnings(invisible(capture.output(res<- ihstep(y,x,m,c))) )
  f <- paste("y~", paste(res,collapse="+"),sep="")
  
  f2 <- paste("y ~",paste(stringi::stri_replace_all_fixed(res,letter.col ,
                                                          any.col, vectorize_all = FALSE),collapse="+"))
  collect <- data.frame(formula=f2, R2= round(summary(lm(formula(f),data=xx) )$r.square,3) )
  
  
  for (i in (2:step)) {
    suppressWarnings( invisible(capture.output(res <- fhstep(y,x,m,c,res))) )
    f <- paste("y~", paste(res,collapse="+"),sep="")
    f2 <- paste("y ~",paste(stringi::stri_replace_all_fixed(res,letter.col ,
                                                            any.col, vectorize_all = FALSE),collapse="+"))
    collect <- rbind(collect,data.frame(formula=f2, R2= round(summary(lm(formula(f),data=xx) )$r.square,3)))
  }
  }
  return(collect)
}