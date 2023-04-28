### stdord function
stdord <-function(m) {
  v1 <- c(1:m)
  v2 <- c((m+1):(2*m))
  interleave(v1,v2)
}