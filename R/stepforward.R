################################################################
################################################################
## step.forward
## RSM forward regression keeping model hierarchy
## y: Response vector
## x: Data frame of x-variables
## step: Number of forward steps requested
## require(daewr)
step.forward <- function(y,x,step) {
  require(daewr)
  any.col <- colnames(x)
  letter.col <- LETTERS[1:ncol(x)]
  colnames(x) <- letter.col
  xx <- x
  xx$y <- y
  suppressWarnings(invisible(capture.output(res<- ihstep(y,x))) )
  f <- paste("y~", paste(res,collapse="+"),sep="")
  f2 <- paste("y ~",paste(stringi::stri_replace_all_fixed(res,letter.col ,
                                                          any.col, vectorize_all = FALSE),collapse="+"))
  collect <- data.frame(formula=f2, R2= round(summary(lm(formula(f),data=xx) )$r.square,3) )
  for (i in (2:step)) {
    suppressWarnings( invisible(capture.output(res <- fhstep(y,x,res))) )
    f <- paste("y~", paste(res,collapse="+"),sep="")
    f2 <- paste("y ~",paste(stringi::stri_replace_all_fixed(res,letter.col ,
                                                            any.col, vectorize_all = FALSE),collapse="+"))
    collect <- rbind(collect,data.frame(formula=f2, R2= round(summary(lm(formula(f),data=xx) )$r.square,3)))
  }
  return(collect)
}