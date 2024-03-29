DefScreen <- function(m=0, c=0,center=0,randomize=FALSE) {
# Definitive Screening Designs for 3-level factors ##
if (m<4) {stop("Definitive Screening Designs only exist for 4-12 3-level factors") }
if (m>12) {stop("Definitive Screening Designs only exist for 4-12 3-level factors") }
if (c<0) {stop("Definitive Screening Designs only exist for 0-4 2-level categorical factors") }
if (c>4) {stop("Definitive Screening Designs only exist for 0-4 2-level categorical factors") }
if (c>0 && center>0) {stop("Cannnot add center points to design with 2-level categorical factors") }

  if (c==0)              {
# Design for 4 3-level factors 
if (m==4) {
  f1 <- matrix(c(0,1,-1,-1,-1,0,-1,1,-1,-1,0,-1,-1,1,1,0), ncol=m,byrow=TRUE)
  f2 <- (-1)*f1
  des <- rbind(f1,f2)
  ord<-stdord(m)
  des <- des[ord , ]
  des <- rbind(des,c(rep(0,m)))
  colnames(des) <- c("A","B","C","D")
  des <- data.frame(des)
              }
# Design for 5 3-level factors 
if (m==5)  {
  f1 <- matrix(c(0,1,1,-1,-1, 1,0,-1,-1,1 ,1,-1,0,1,-1, 1,-1,1,0,1, 1,1,1,1,0), ncol=m, byrow=TRUE)
  f2 <- (-1)*f1
  des <- rbind(f1,f2)
  ord<-stdord(m)
  des <- des[ord , ]
  des <- rbind(des,c(rep(0,m)))
  colnames(des) <- c("A","B","C","D","E")
  des <- data.frame(des)
}

if (m==6)     {
  f1 <- matrix(c(0,1,-1,-1,-1,-1,1,0,-1,1,1,-1,-1,-1,0,1,-1,-1,-1,1,1,0,1,-1,1,-1,1,-1,0,-1,1,1,1,1,-1,0), ncol=m, byrow=TRUE)
  f2 <- (-1)*f1
  des <- rbind(f1,f2)
  ord<-stdord(m)
  des <- des[ord , ]
  des <- rbind(des,c(rep(0,m)))
  colnames(des) <- c("A","B","C","D","E","F")
  des <- data.frame(des)
              }
# Design for 7 3-level factors 
if (m==7)     {
  f1 <- matrix(c(0,1,-1,1,-1,1,-1, -1,0,1,-1,1,1,-1, 1,-1,0,1,1,1,1, 1,-1, -1,0,1,-1,-1, 
  -1,-1,1,1,0,-1,-1, -1,1,-1,1,1,0,1, 1,1,1,1,1,-1,0), ncol=m, byrow=TRUE)
  f2 <- (-1)*f1
  des <- rbind(f1,f2)
  ord<-stdord(m)
  des <- des[ord , ]
  des <- rbind(des,c(rep(0,m)))
  colnames(des) <- c("A","B","C","D","E","F","G")
  des <- data.frame(des)
}
# Design for 8 3-level factors 
if (m==8)     {
  f1 <- matrix(c(0,-1,1,1,-1,1,1,1, -1,0,-1,1,1,1,1,-1, -1,-1,0,1,1,-1,-1,1,
  1,-1,1,0,1,1,-1,-1, -1,-1,1,-1,0,-1,1,-1, 1,-1,-1,-1,1,0,1,1, 
  -1,1,1,-1,1,1,0,1, 1,1,1,1,1,-1,1,0), ncol=m, byrow=TRUE)
  f2 <- (-1)*f1
  des <- rbind(f1,f2)
  ord<-stdord(m)
  des <- des[ord , ]
  des <- rbind(des,c(rep(0,m)))
  colnames(des) <- c("A","B","C","D","E","F","G","H")
  des <- data.frame(des)
}
# Design for 9 3-level factors
if (m==9)     {
  f1 <- matrix(c(0,1,1,1,1,1,1,1,1,  1,0,1,-1,1,-1,-1,1,-1,
                 -1,1,0,-1,1,-1,1,-1,-1,  -1,-1,1,0,1,-1,-1,-1,1,
                 1,-1,1,-1,0,1,1,-1,-1, -1,-1,-1,-1,1,0,1,1,1,
                 1,1,-1,-1,1,1,0,-1,1, -1,-1,-1,1,1,1,-1,0,-1,
                 -1,1,1,-1,-1,1,-1,1,0), ncol=m, byrow=TRUE)
  f2 <- (-1)*f1
  des <- rbind(f1,f2)
  ord<-stdord(m)
  des <- des[ord , ]
  des <- rbind(des,c(rep(0,m)))
  colnames(des) <- c("A","B","C","D","E","F","G","H","J")
  des <- data.frame(des)
}
# Design for 10 3-level factors
if (m==10)     {
  f1 <- matrix(c(0,1,1,-1,1,1,1,1,-1,1, 1,0,-1,1,1,-1,1,1,-1,-1,
                 -1,1,0,-1,-1,-1,1,-1,-1,-1,  -1,1,1,0,1,-1,-1,1,1,-1,
                 -1,-1,-1,-1,0,1,1,1,1,-1, -1,1,-1,1,1,0,1,-1,1,1,
                 1,1,-1,-1,-1,-1,0,1,1,1, 1,1,1,1,-1,1,1,0,1,-1,
                 1,1,-1,-1,1,1,-1,-1,0,-1, 1,-1,1,-1,1,-1,1,-1,1,0), ncol=m, byrow=TRUE)
  f2 <- (-1)*f1
  des <- rbind(f1,f2)
  ord<-stdord(m)
  des <- des[ord , ]
  des <- rbind(des,c(rep(0,m)))
  colnames(des) <- c("A","B","C","D","E","F","G","H","J","K")
  des <- data.frame(des)
}
# Design for 11 3-level factors
if (m==11)     {
  f1 <- matrix(c(0,-1,1,-1,-1,-1,-1,-1,1,-1,1, -1,0,-1,-1,1,-1,-1,-1,-1,1,1,
                 -1,-1,0,1,1,1,1,-1,-1,-1,1, -1,-1,-1,0,-1,1,1,-1,1,1,-1,
                 1,-1,-1,1,0,1,-1,1,1,1,1, -1,-1,1,1,-1,0,-1,1,-1,1,-1,
                 -1,-1,-1,1,1,-1,0,1,1,-1,-1, -1,1,1,1,-1,-1,1,0,1,1,1,
                 -1,1,-1,-1,-1,1,-1,1,0,-1,1, 1,-1,-1,-1,-1,-1,1,1,-1,0,1,
                 1,1,-1,1,-1,-1,-1,-1,-1,-1,0), ncol=m, byrow=TRUE)
  f2 <- (-1)*f1
  des <- rbind(f1,f2)
  ord<-stdord(m)
  des <- des[ord , ]
  des <- rbind(des,c(rep(0,m)))
  colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L")
  des <- data.frame(des)
}
# Design for 12 3-level factors
if (m==12)     {
  f1 <- matrix(c(0,-1,-1,1,-1,1,-1,1,1,1,-1,1,
                 -1,0,1,1,1,1,1,1,1,-1,-1,-1,
                 1,1,0,-1,1,1,-1,1,1,-1,1,1,
                 1,-1,-1,0,1,-1,1,-1,1,-1,-1,1,
                 1,1,1,1,0,-1,1,1,1,1,1,1,
                 1,-1,1,-1,1,0,1,1,-1,1,-1,1,
                 1,1,1,1,-1,1,0,-1,-1,-1,-1,1,
                 -1,-1,1,1,1,-1,-1,0,-1,-1,1,1,
                 1,-1,1,1,1,1,-1,-1,0,1,1,-1,
                 1,1,-1,1,1,-1,-1,1,-1,0,-1,-1,
                 -1,1,-1,1,1,1,1,-1,-1,1,0,1,
                 1,-1,-1,1,-1,1,1,1,-1,-1,1,0), ncol=m, byrow=TRUE)
  f2 <- (-1)*f1
  des <- rbind(f1,f2)
  ord<-stdord(m)
  des <- des[ord , ]
  des <- rbind(des,c(rep(0,m)))
  colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L","M")
  des <- data.frame(des)
}
                             }
# Definitive Screening Designs for 3-level factors  and one 2-level factor ##
if (c==1)                    {  
# Design for 4 3-level factors 
if (m==4)  {
  f1 <- matrix(c(0,1,1,1,1, 1,0,-1,1,1, 1,-1,0,-1,1, 1,1,-1,0,-1, 
                 1,1,1,-1,1, 1,-1,1,1,-1, 0,0,0,0,-1), ncol=(m+1), byrow=TRUE)
  f2 <- (-1)*f1
  des <- rbind(f1,f2)
  ord<-stdord(nrow(f1))
  des <- des[ord , ]
  #z <- rbind(c(rep(0,m),-1),c(rep(0,m),-1))
  #des <- rbind(des,z)
  colnames(des) <- c("A","B","C","D","E")
  des <- data.frame(des)
          }
if (m==5)  {
  f1 <- matrix(c(0,1,1,1,1,1, 1,0,-1,1,1,-1,
                 1,-1,0,-1,1,1, 1,1,-1,0,-1,1,
                 1,1,1,-1,0,-1, 1,-1,1,1,-1,1,
                 0,0,0,0,0,-1), ncol=(m+1), byrow=TRUE)
  f2 <- (-1)*f1
  des <- rbind(f1,f2)
  ord<-stdord(nrow(f1))
  des <- des[ord , ]
  #z <- rbind(c(rep(0,m),-1),c(rep(0,m),-1))
  #des <- rbind(des,z)
  colnames(des) <- c("A","B","C","D","E","F")
  des <- data.frame(des)
}
if (m==6)  {
  f1 <- matrix(c(0,1,1,1,1,1,1, 1,0,1,1,-1,1,-1,
                1,-1,0,1,1,-1,1, 1,-1,-1,0,1,1,-1,
                1,1,-1,-1,0,1,1, 1,-1,1,-1,-1,0,1,
                1,1,-1,1,-1,-1,1, 1,1,1,-1,1,-1,-1,
                0,0,0,0,0,0,-1), ncol=(m+1), byrow=TRUE)
  f2 <- (-1)*f1
  des <- rbind(f1,f2)
  ord<-stdord(nrow(f1))
  des <- des[ord , ]
  colnames(des) <- c("A","B","C","D","E","F","G")
  des <- data.frame(des)
}
if (m==7)  {
  f1 <- matrix(c(0,1,1,1,1,1,1,1,  1,0,1,1,-1,1,-1,-1,
                 1,-1,0,1,1,-1,1,-1, 1,1,-1,0,1,1,-1,1,
                 1,1,-1,-1,0,1,1,-1, 1,-1,1,-1,-1,0,1,1,
                 1,1,-1,1,-1,-1,0,1, 1,1,1,-1,1,-1,-1,1,
                 0,0,0,0,0,0,0,-1), ncol=(m+1), byrow=TRUE)
  f2 <- (-1)*f1
  des <- rbind(f1,f2)
  ord<-stdord(nrow(f1))
  des <- des[ord , ]
  colnames(des) <- c("A","B","C","D","E","F","G","H")
  des <- data.frame(des)
}
if (m==8)  {
  f1 <- matrix(c(0,1,1,1,1,1,1,1,1, 1,0,-1,-1,-1,-1,1,1,1,
                 1,-1,0,-1,1,1,-1,-1,1, 1,-1,-1,0,1,1,1,1,-1,
                 1,-1,1,1,0,-1,-1,1,-1, 1,-1,1,1,-1,0,1,-1,1,
                 1,1,-1,1,-1,1,0,-1,-1, 1,1,-1,1,1,-1,-1,0,1,
                 1,1,1,-1,-1,1,-1,1,1, 1,1,1,-1,1,-1,1,-1,-1,
                 0,0,0,0,0,0,0,0,-1), ncol=(m+1), byrow=TRUE)
  f2 <- (-1)*f1
  des <- rbind(f1,f2)
  ord<-stdord(nrow(f1))
  des <- des[ord , ]
  colnames(des) <- c("A","B","C","D","E","F","G","H","J")
  des <- data.frame(des)
}
if (m==9)  {
  f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1, 1,0,-1,-1,-1,-1,1,1,1,1,
               1,-1,0,-1,1,1,-1,-1,1,1, 1,-1,-1,0,1,1,1,1,-1,-1,
               1,-1,1,1,0,-1,-1,1,-1,1, 1,-1,1,1,-1,0,1,-1,1,-1,
               1,1,-1,1,-1,1,0,-1,-1,1, 1,1,-1,1,1,-1,-1,0,1,-1,
               1,1,1,-1,-1,1,-1,1,0,-1, 1,1,1,-1,1,-1,1,-1,-1,1,
               0,0,0,0,0,0,0,0,0,-1), ncol=(m+1), byrow=TRUE)
  f2 <- (-1)*f1
  des <- rbind(f1,f2)
  ord<-stdord(nrow(f1))
  des <- des[ord , ]
  colnames(des) <- c("A","B","C","D","E","F","G","H","J","K")
  des <- data.frame(des)
}
if (m==10)  {
  f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1,1, 1,0,1,-1,1,1,1,-1,-1,-1,1,
               1,-1,0,1,-1,1,1,1,-1,-1,-1, 1,1,-1,0,1,-1,1,1,1,-1,-1,
               1,-1,1,-1,0,1,-1,1,1,1,-1, 1,-1,-1,1,-1,0,1,-1,1,1,1,
               1,-1,-1,-1,1,-1,0,1,-1,1,1, 1,1,-1,-1,-1,1,-1,0,1,-1,1,
               1,1,1,-1,-1,-1,1,-1,0,1,-1, 1,1,1,1,-1,-1,-1,1,-1,0,1,
               1,-1,1,1,1,-1,-1,-1,1,-1,1, 1,1,-1,1,1,1,-1,-1,-1,1,-1,
               0,0,0,0,0,0,0,0,0,0,-1), ncol=(m+1), byrow=TRUE)
  f2 <- (-1)*f1
  des <- rbind(f1,f2)
  ord<-stdord(nrow(f1))
  des <- des[ord , ]
  colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L")
  des <- data.frame(des)
}
if (m==11)  {
  f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1,1,1, 1,0,1,-1,1,1,1,-1,-1,-1,1,-1,
               1,-1,0,1,-1,1,1,1,-1,-1,-1,1, 1,1,-1,0,1,-1,1,1,1,-1,-1,-1,
               1,-1,1,-1,0,1,-1,1,1,1,-1,-1, 1,-1,-1,1,-1,0,1,-1,1,1,1,-1,
               1,-1,-1,-1,1,-1,0,1,-1,1,1,1, 1,1,-1,-1,-1,1,-1,0,1,-1,1,1, 
               1,1,1,-1,-1,-1,1,-1,0,1,-1,1, 1,1,1,1,-1,-1,-1,1,-1,0,1,-1,
               1,-1,1,1,1,-1,-1,-1,1,-1,0,1, 1,1,-1,1,1,1,-1,-1,-1,1,-1,1,
               0,0,0,0,0,0,0,0,0,0,0,-1), ncol=(m+1), byrow=TRUE)
  f2 <- (-1)*f1
  des <- rbind(f1,f2)
  ord<-stdord(nrow(f1))
  des <- des[ord , ]
  colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L","M")
  des <- data.frame(des)
}
if (m==12)  {
  f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1,1,1,1, 1,0,-1,1,-1,-1,1,1,1,1,-1,-1,1,
               1,-1,0,-1,1,-1,-1,1,1,1,1,-1,-1, 1,1,-1,0,-1,1,-1,-1,1,1,1,1,-1,
               1,-1,1,-1,0,-1,1,-1,-1,1,1,1,1, 1,-1,-1,1,-1,0,-1,1,-1,-1,1,1,1,
               1,1,-1,-1,1,-1,0,-1,1,-1,-1,1,1, 1,1,1,-1,-1,1,-1,0,-1,1,-1,-1,1,
               1,1,1,1,-1,-1,1,-1,0,-1,1,-1,-1, 1,1,1,1,1,-1,-1,1,-1,0,-1,1,-1,
               1,-1,1,1,1,1,-1,-1,1,-1,0,-1,1, 1,-1,-1,1,1,1,1,-1,-1,1,-1,0,-1,
               1,1,-1,-1,1,1,1,1,-1,-1,1,-1,1, 1,-1,1,-1,-1,1,1,1,1,-1,-1,1,-1,
               0,0,0,0,0,0,0,0,0,0,0,0,-1), ncol=(m+1), byrow=TRUE)
  f2 <- (-1)*f1
  des <- rbind(f1,f2)
  ord<-stdord(nrow(f1))
  des <- des[ord , ]
  colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L","M","N")
  des <- data.frame(des)
}


                              }
if (c==2)                    {  
  # Design for 4 3-level factors 
  if (m==4)  {
    f1 <- matrix(c(0,1,1,1,1,1, 1,0,-1,1,1,-1, 1,-1,0,-1,1,1,
                   1,1,-1,0,-1,1, 1,1,1,-1,1,-1, 1,-1,1,1,-1,1,
                   0,0,0,0,-1,-1), ncol=(m+2), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F")
    des <- data.frame(des)
        }
  if (m==5)  {
    f1 <- matrix(c(0,1,1,1,1,1,1, 1,0,1,1,-1,1,-1,
                   1,-1,0,1,1,-1,1, 1,-1,-1,0,1,1,-1,
                   1,1,-1,-1,0,1,1, 1,-1,1,-1,-1,1,1,
                   1,1,-1,1,-1,-1,1, 1,1,1,-1,1,-1,-1,
                   0,0,0,0,0,-1,-1), ncol=(m+2), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G")
    des <- data.frame(des)
  }
  if (m==6)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1, 1,0,1,1,-1,1,-1,-1,
                 1,-1,0,1,1,-1,1,-1, 1,-1,-1,0,1,1,-1,1,
                 1,1,-1,-1,0,1,1,-1, 1,-1,1,-1,-1,0,1,1,
                 1,1,-1,1,-1,-1,1,1, 1,1,1,-1,1,-1,-1,1,
                 0,0,0,0,0,0,-1,-1), ncol=(m+2), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H")
    des <- data.frame(des)
  }
  if (m==7)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1, 1,0,-1,-1,-1,-1,1,1,1,
                   1,-1,0,-1,1,1,-1,-1,1, 1,-1,-1,0,1,1,1,1,-1,
                   1,-1,1,1,0,-1,-1,1,-1, 1,-1,1,1,-1,0,1,-1,1,
                   1,1,-1,1,-1,1,0,-1,-1, 1,1,-1,1,1,-1,-1,1,1,
                   1,1,1,-1,-1,1,-1,1,1, 1,1,1,-1,1,-1,1,-1,-1,
                   0,0,0,0,0,0,0,-1,-1), ncol=(m+2), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J")
    des <- data.frame(des)
  }
  if (m==8)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1, 1,0,-1,-1,-1,-1,1,1,1,1,
                 1,-1,0,-1,1,1,-1,-1,1,1, 1,-1,-1,0,1,1,1,1,-1,-1,
                 1,-1,1,1,0,-1,-1,1,-1,1, 1,-1,1,1,-1,0,1,-1,1,-1,
                 1,1,-1,1,-1,1,0,-1,-1,1, 1,1,-1,1,1,-1,-1,0,1,-1,
                 1,1,1,-1,-1,1,-1,1,1,-1, 1,1,1,-1,1,-1,1,-1,-1,1,
                 0,0,0,0,0,0,0,0,-1,-1), ncol=(m+2), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J","K")
    des <- data.frame(des)
  }
  if (m==9)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1,1, 1,0,1,-1,1,1,1,-1,-1,-1,1,
                 1,-1,0,1,-1,1,1,1,-1,-1,-1, 1,1,-1,0,1,-1,1,1,1,-1,-1,
                 1,-1,1,-1,0,1,-1,1,1,1,-1, 1,-1,-1,1,-1,0,1,-1,1,1,1, 
                 1,-1,-1,-1,1,-1,0,1,-1,1,1, 1,1,-1,-1,-1,1,-1,0,1,-1,1,
                 1,1,1,-1,-1,-1,1,-1,0,1,-1, 1,1,1,1,-1,-1,-1,1,-1,1,1,
                 1,-1,1,1,1,-1,-1,-1,1,-1,1, 1,1,-1,1,1,1,-1,-1,-1,1,-1,
                 0,0,0,0,0,0,0,0,0,-1,-1), ncol=(m+2), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L")
    des <- data.frame(des)
  }
  if (m==10)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1,1,1, 1,0,1,-1,1,1,1,-1,-1,-1,1,-1,
                   1,-1,0,1,-1,1,1,1,-1,-1,-1,1, 1,1,-1,0,1,-1,1,1,1,-1,-1,-1,
                   1,-1,1,-1,0,1,-1,1,1,1,-1,-1, 1,-1,-1,1,-1,0,1,-1,1,1,1,-1,
                   1,-1,-1,-1,1,-1,0,1,-1,1,1,1, 1,1,-1,-1,-1,1,-1,0,1,-1,1,1,
                   1,1,1,-1,-1,-1,1,-1,0,1,-1,1, 1,1,1,1,-1,-1,-1,1,-1,0,1,-1,
                   1,-1,1,1,1,-1,-1,-1,1,-1,1,1, 1,1,-1,1,1,1,-1,-1,-1,1,-1,1,
                   0,0,0,0,0,0,0,0,0,0,-1,-1), ncol=(m+2), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L","M")
    des <- data.frame(des)
  }
  if (m==11)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1,1,1,1, 1,0,-1,1,-1,-1,1,1,1,1,-1,-1,1,
                 1,-1,0,-1,1,-1,-1,1,1,1,1,-1,-1, 1,1,-1,0,-1,1,-1,-1,1,1,1,1,-1,
                 1,-1,1,-1,0,-1,1,-1,-1,1,1,1,1, 1,-1,-1,1,-1,0,-1,1,-1,-1,1,1,1,
                 1,1,-1,-1,1,-1,0,-1,1,-1,-1,1,1, 1,1,1,-1,-1,1,-1,0,-1,1,-1,-1,1,
                 1,1,1,1,-1,-1,1,-1,0,-1,1,-1,-1, 1,1,1,1,1,-1,-1,1,-1,0,-1,1,-1,
                 1,-1,1,1,1,1,-1,-1,1,-1,0,-1,1, 1,-1,-1,1,1,1,1,-1,-1,1,-1,1,-1,
                 1,1,-1,-1,1,1,1,1,-1,-1,1,-1,1, 1,-1,1,-1,-1,1,1,1,1,-1,-1,1,-1,
                 0,0,0,0,0,0,0,0,0,0,0,-1,-1), ncol=(m+2), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L","M","N")
    des <- data.frame(des)
  }
  if (m==12)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1,1,1,1,1, 
                   1,0,-1,1,-1,-1,1,1,1,1,-1,-1,1,-1,
                   1,-1,0,-1,1,-1,-1,1,1,1,1,-1,-1,1,
                   1,1,-1,0,-1,1,-1,-1,1,1,1,1,-1,-1,
                   1,-1,1,-1,0,-1,1,-1,-1,1,1,1,1,-1,
                   1,-1,-1,1,-1,0,-1,1,-1,-1,1,1,1,1,
                   1,1,-1,-1,1,-1,0,-1,1,-1,-1,1,1,1,
                   1,1,1,-1,-1,1,-1,0,-1,1,-1,-1,1,1,
                   1,1,1,1,-1,-1,1,-1,0,-1,1,-1,-1,1,
                   1,1,1,1,1,-1,-1,1,-1,0,-1,1,-1,-1,
                   1,-1,1,1,1,1,-1,-1,1,-1,0,-1,1,-1,
                   1,-1,-1,1,1,1,1,-1,-1,1,-1,0,-1,1,
                   1,1,-1,-1,1,1,1,1,-1,-1,1,-1,1,-1,
                   1,-1,1,-1,-1,1,1,1,1,-1,-1,1,1,1,
                   0,0,0,0,0,0,0,0,0,0,0,0,-1,-1), ncol=(m+2), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L","M","N","O")
    des <- data.frame(des)
  }


                               }


if (c==3)                     {  
  # Design for 4 3-level factors 
  if (m==4)  {
    f1 <- matrix(c(0,1,1,1,1,1,1, 1,0,1,1,-1,1,-1,
                   1,-1,0,1,1,-1,1, 1,-1,-1,0,1,1,-1,
                   1,1,-1,-1,1,1,  1,-1,1,-1,-1,1,1,1,
                   1,1,-1,1,-1,-1,1,  1,1,1,-1,1,-1,-1,
                   0,0,0,0,-1,-1,-1), ncol=(m+3), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G")
    des <- data.frame(des)
  }
  if (m==5)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1, 1,0,1,1,-1,1,-1,-1,
                 1,-1,0,1,1,-1,1,-1, 1,-1,-1,0,1,1,-1,1,
                 1,1,-1,-1,0,1,1,-1, 1,-1,1,-1,-1,1,1,1,
                 1,1,-1,1,-1,-1,1,1, 1,1,1,-1,1,-1,-1,1,
                 0,0,0,0,0,-1,-1,-1), ncol=(m+3), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H")
    des <- data.frame(des)
  }
  if (m==6)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1, 1,0,-1,-1,-1,-1,1,1,1,
                   1,-1,0,-1,1,1,-1,-1,1, 1,-1,-1,0,1,1,1,1,-1,
                   1,-1,1,1,0,-1,-1,1,-1, 1,-1,1,1,-1,0,1,-1,1,
                   1,1,-1,1,-1,1,1,-1,-1, 1,1,-1,1,1,-1,-1,1,1,
                   1,1,1,-1,-1,1,-1,1,1, 1,1,1,-1,1,-1,1,-1,-1,
                   0,0,0,0,0,0,-1,-1,-1), ncol=(m+3), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J")
    des <- data.frame(des)
  }
  if (m==7)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1, 1,0,-1,-1,-1,-1,1,1,1,1,
                   1,-1,0,-1,1,1,-1,-1,1,1, 1,-1,-1,0,1,1,1,1,-1,-1,
                   1,-1,1,1,0,-1,-1,1,-1,1, 1,-1,1,1,-1,0,1,-1,1,-1,
                   1,1,-1,1,-1,1,0,-1,-1,1, 1,1,-1,1,1,-1,-1,1,1,-1,
                   1,1,1,-1,-1,1,-1,1,1,-1, 1,1,1,-1,1,-1,1,-1,-1,1,
                   0,0,0,0,0,0,0,-1,-1,-1), ncol=(m+3), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J","K")
    des <- data.frame(des)
  }
  if (m==8)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1,1,  1,0,1,-1,1,1,1,-1,-1,-1,1,
                   1,-1,0,1,-1,1,1,1,-1,-1,-1, 1,1,-1,0,1,-1,1,1,1,-1,-1,
                   1,-1,1,-1,0,1,-1,1,1,1,-1, 1,-1,-1,1,-1,0,1,-1,1,1,1,
                   1,-1,-1,-1,1,-1,0,1,-1,1,1,  1,1,-1,-1,-1,1,-1,0,1,-1,1,
                   1,1,1,-1,-1,-1,1,-1,1,1,-1, 1,1,1,1,-1,-1,-1,1,-1,1,1,
                   1,-1,1,1,1,-1,-1,-1,1,-1,1, 1,1,-1,1,1,1,-1,-1,-1,1,-1,
                   0,0,0,0,0,0,0,0,-1,-1,-1), ncol=(m+3), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L")
    des <- data.frame(des)
  }
  if (m==9)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1,1,1, 1,0,1,-1,1,1,1,-1,-1,-1,1,-1,
                 1,-1,0,1,-1,1,1,1,-1,-1,-1,1, 1,1,1,0,1,-1,1,1,1,-1,-1,-1,
                 1,-1,1,-1,0,1,-1,1,1,1,-1,-1, 1,-1,-1,1,-1,0,1,-1,1,1,1,-1,
                 1,-1,-1,-1,1,-1,0,1,-1,1,1,1, 1,1,-1,-1,-1,1,-1,0,1,-1,1,1,
                 1,1,1,-1,-1,-1,1,-1,0,1,-1,1, 1,1,1,1,-1,-1,-1,1,-1,1,1,-1,
                 1,-1,1,1,1,-1,-1,-1,1,-1,1,1, 1,1,-1,1,1,1,-1,-1,-1,1,-1,1,
                 0,0,0,0,0,0,0,0,0,-1,-1,-1), ncol=(m+3), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L","M")
    des <- data.frame(des)
  }
  if (m==10)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1,1,1,1, 1,0,-1,1,-1,-1,1,1,1,1,-1,-1,1,
                 1,-1,0,-1,1,-1,-1,1,1,1,1,-1,-1, 1,1,-1,0,-1,1,-1,-1,1,1,1,1,-1,
                 1,-1,1,-1,0,-1,1,-1,-1,1,1,1,1, 1,-1,-1,1,-1,0,-1,1,-1,-1,1,1,1,
                 1,1,-1,-1,1,-1,0,-1,1,-1,-1,1,1, 1,1,1,-1,-1,1,-1,0,-1,1,-1,-1,1,
                 1,1,1,1,-1,-1,1,-1,0,-1,1,-1,-1, 1,1,1,1,1,-1,-1,1,-1,0,-1,1,-1,
                 1,-1,1,1,1,1,-1,-1,1,-1,1,-1,1, 1,-1,-1,1,1,1,1,-1,-1,1,-1,1,-1,
                 1,1,-1,-1,1,1,1,1,-1,-1,1,-1,1, 1,-1,1,-1,-1,1,1,1,1,-1,-1,1,-1,
                 0,0,0,0,0,0,0,0,0,0,-1,-1,-1), ncol=(m+3), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L","M","N")
    des <- data.frame(des)
  }
  if (m==11)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1,1,1,1,1,
                 1,0,-1,1,-1,-1,1,1,1,1,-1,-1,1,-1,
                 1,-1,0,-1,1,-1,-1,1,1,1,1,-1,-1,1,
                 1,1,-1,0,-1,1,-1,-1,1,1,1,1,-1,-1,
                 1,-1,1,-1,0,-1,1,-1,-1,1,1,1,1,-1,
                 1,-1,-1,1,-1,0,-1,1,-1,-1,1,1,1,1,
                 1,1,-1,-1,1,-1,0,-1,1,-1,-1,1,1,1,
                 1,1,1,-1,-1,1,-1,0,-1,1,-1,-1,1,1,
                 1,1,1,1,-1,-1,1,-1,0,-1,1,-1,-1,1,
                 1,1,1,1,1,-1,-1,1,-1,0,-1,1,-1,-1,
                 1,-1,1,1,1,1,-1,-1,1,-1,0,-1,1,-1,
                 1,-1,-1,1,1,1,1,-1,-1,1,-1,1,-1,1,
                 1,1,-1,-1,1,1,1,1,-1,-1,1,-1,1,-1,
                 1,-1,1,-1,-1,1,1,1,1,-1,-1,1,-1,1,
                 0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1), ncol=(m+3), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L","M","N","O")
    des <- data.frame(des)
  }
  if (m==12)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                   1,0,1,1,-1,1,-1,-1,1,-1,1,1,-1,1,-1,
                   1,-1,0,1,1,-1,1,-1,1,1,1,-1,1,-1,-1,
                   1,-1,-1,0,1,1,-1,1,1,1,-1,1,-1,-1,-1,
                   1,1,-1,-1,0,1,1,-1,1,-1,1,-1,-1,-1,1,
                   1,-1,1,-1,-1,0,1,1,1,1,-1,-1,-1,1,1,
                   1,1,-1,1,-1,-1,0,1,1,-1,-1,-1,1,1,-1,
                   1,1,1,-1,1,-1,-1,0,1,-1,-1,1,1,-1,1,
                   -1,1,1,1,1,1,1,1,0,-1,-1,-1,-1,-1,-1,
                   -1,-1,1,1,-1,1,-1,-1,1,0,-1,-1,1,-1,1,
                   -1,1,1,-1,1,-1,-1,-1,1,1,0,-1,-1,1,-1,
                   -1,1,-1,1,-1,-1,-1,1,1,1,1,0,-1,-1,1,
                   -1,-1,1,-1,-1,-1,1,1,1,-1,1,1,1,-1,-1,
                   -1,1,-1,-1,-1,1,1,-1,1,1,-1,1,1,1,-1,
                   -1,-1,-1,-1,1,1,-1,1,1,-1,1,-1,1,1,1,
                    -1,-1,-1,1,1,-1,1,-1,1,-1,-1,1,-1,1,1,
                    0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1), ncol=(m+3), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L","M","N","O","P")
    des <- data.frame(des)
  }


                               }


if (c==4)                     {  
  # Design for 4 3-level factors 
  if (m==4)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1, 1,0,1,1,-1,1,-1,-1,
                   1,-1,0,1,1,-1,1,-1, 1,-1,-1,0,1,1,-1,1,
                   1,1,-1,-1,1,1,1,-1, 1,-1,1,-1,-1,1,1,1,
                   1,1,-1,1,-1,-1,1,1, 1,1,1,-1,1,-1,-1,1,
                   0,0,0,0,-1,-1,-1,-1), ncol=(m+4), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H")
    des <- data.frame(des)
  }
  if (m==5)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1, 1,0,-1,-1,-1,-1,1,1,1,
                   1,-1,0,-1,1,1,-1,-1,1, 1,-1,-1,0,1,1,1,1,-1,
                   1,-1,1,1,0,-1,-1,1,-1, 1,-1,1,1,-1,1,1,-1,1,
                   1,1,-1,1,-1,1,1,-1,-1, 1,1,-1,1,1,-1,-1,1,1,
                   1,1,1,-1,-1,1,-1,1,1, 1,1,1,-1,1,-1,1,-1,-1,
                   0,0,0,0,0,-1,-1,-1,-1), ncol=(m+4), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J")
    des <- data.frame(des)
  }
  if (m==6)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1, 1,0,-1,-1,-1,-1,1,1,1,1,
                   1,-1,0,-1,1,1,-1,-1,1,1, 1,-1,-1,0,1,1,1,1,-1,-1,
                   1,-1,1,1,0,-1,-1,1,-1,1, 1,-1,1,1,-1,0,1,-1,1,-1,
                   1,1,-1,1,-1,1,1,-1,-1,1, 1,1,-1,1,1,-1,-1,1,1,-1,
                   1,1,1,-1,-1,1,-1,1,1,-1, 1,1,1,-1,1,-1,1,-1,-1,1,
                   0,0,0,0,0,0,-1,-1,-1,-1), ncol=(m+4), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J","K")
    des <- data.frame(des)
  }
  if (m==7)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1,1, 1,0,1,-1,1,1,1,-1,-1,-1,1,
                 1,-1,0,1,-1,1,1,1,-1,-1,-1, 1,1,-1,0,1,-1,1,1,1,-1,-1,
                 1,-1,1,-1,0,1,-1,1,1,1,-1, 1,-1,-1,1,-1,0,1,-1,1,1,1,
                 1,-1,-1,-1,1,-1,0,1,-1,1,1, 1,1,-1,-1,-1,1,-1,1,1,-1,1,
                 1,1,1,-1,-1,-1,1,-1,1,1,-1, 1,1,1,1,-1,-1,-1,1,-1,1,1,
                 1,-1,1,1,1,-1,-1,-1,1,-1,1, 1,1,-1,1,1,1,-1,-1,-1,1,-1,
                 0,0,0,0,0,0,0,-1,-1,-1,-1), ncol=(m+4), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L")
    des <- data.frame(des)
  }
  if (m==8)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1,1,1, 1,0,1,-1,1,1,1,-1,-1,-1,1,-1,
                 1,-1,0,1,-1,1,1,1,-1,-1,-1,1, 1,1,-1,0,1,-1,1,1,1,-1,-1,-1,
                 1,-1,1,-1,0,1,-1,1,1,1,-1,-1, 1,-1,-1,1,-1,0,1,-1,1,1,1,-1,
                 1,-1,-1,-1,1,-1,0,1,-1,1,1,1, 1,1,-1,-1,-1,1,-1,0,1,-1,1,1,
                 1,1,1,-1,-1,-1,1,-1,1,1,-1,1, 1,1,1,1,-1,-1,-1,1,-1,1,1,-1,
                 1,-1,1,1,1,-1,-1,-1,1,-1,1,1, 1,1,-1,1,1,1,-1,-1,-1,1,-1,1,
                 0,0,0,0,0,0,0,0,-1,-1,-1,-1), ncol=(m+4), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L","M")
    des <- data.frame(des)
  }
  if (m==9)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1,1,1,1, 1,0,-1,1,-1,-1,1,1,1,1,-1,-1,1,
                   1,-1,0,-1,1,-1,-1,1,1,1,1,-1,-1, 1,1,-1,0,-1,1,-1,-1,1,1,1,1,-1,
                   1,-1,1,-1,0,-1,1,-1,-1,1,1,1,1, 1,-1,-1,1,-1,0,-1,1,-1,-1,1,1,1,
                   1,1,-1,-1,1,-1,0,-1,1,-1,-1,1,1, 1,1,1,-1,-1,1,-1,0,-1,1,-1,-1,1,
                   1,1,1,1,-1,-1,1,-1,0,-1,1,-1,-1, 1,1,1,1,1,-1,-1,1,-1,1,-1,1,-1,
                   1,-1,1,1,1,1,-1,-1,1,-1,1,-1,1, 1,-1,-1,1,1,1,1,-1,-1,1,-1,1,-1,
                   1,1,-1,-1,1,1,1,1,-1,-1,1,-1,1, 1,-1,1,-1,-1,1,1,1,1,-1,-1,1,-1,
                   0,0,0,0,0,0,0,0,0,-1,-1,-1,-1), ncol=(m+4), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L","M","N")
    des <- data.frame(des)
  }
  if (m==10)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1,1,1,1,1,
                   1,0,-1,1,-1,-1,1,1,1,1,-1,-1,1,-1,
                   1,-1,0,-1,1,-1,-1,1,1,1,1,-1,-1,1,
                   1,1,-1,0,-1,1,-1,-1,1,1,1,1,-1,-1,
                   1,-1,1,-1,0,-1,1,-1,-1,1,1,1,1,-1,
                   1,-1,-1,1,-1,0,-1,1,-1,-1,1,1,1,1,
                   1,1,-1,-1,1,-1,0,-1,1,-1,-1,1,1,1,
                   1,1,1,-1,-1,1,-1,0,-1,1,-1,-1,1,1,
                   1,1,1,1,-1,-1,1,-1,0,-1,1,-1,-1,1,
                   1,1,1,1,1,-1,-1,1,-1,0,-1,1,-1,-1,
                   1,-1,1,1,1,1,-1,-1,1,-1,1,-1,1,-1,
                   1,-1,-1,1,1,1,1,-1,-1,1,-1,1,-1,1,
                   1,1,-1,-1,1,1,1,1,-1,-1,1,-1,1,-1,
                   1,-1,1,-1,-1,1,1,1,1,-1,-1,1,-1,1, 
                   0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1), ncol=(m+4), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L","M","N","O")
    des <- data.frame(des)
  }
  if (m==11)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                   1,0,1,1,-1,1,-1,-1,1,-1,1,1,-1,1,-1,
                   1,-1,0,1,1,-1,1,-1,1,1,1,-1,1,-1,-1,
                   1,-1,-1,0,1,1,-1,1,1,1,-1,1,-1,-1,-1,
                   1,1,-1,-1,0,1,1,-1,1,-1,1,-1,-1,-1,1,
                   1,-1,1,-1,-1,0,1,1,1,1,-1,-1,-1,1,1,
                   1,1,-1,1,-1,-1,0,1,1,-1,-1,-1,1,1,-1,
                   1,1,1,-1,1,-1,-1,0,1,-1,-1,1,1,-1,1,
                   -1,1,1,1,1,1,1,1,0,-1,-1,-1,-1,-1,-1,
                   -1,-1,1,1,-1,1,-1,-1,1,0,-1,-1,1,-1,1,
                   -1,1,1,-1,1,-1,-1,-1,1,1,0,-1,-1,1,-1,
                   -1,1,-1,1,-1,-1,-1,1,1,1,1,1,-1,-1,1,
                   -1,-1,1,-1,-1,-1,1,1,1,-1,1,1,1,-1,-1,
                   -1,1,-1,-1,-1,1,1,-1,1,1,-1,1,1,1,-1,
                    -1,-1,-1,-1,1,1,-1,1,1,-1,1,-1,1,1,1,
                    -1,-1,-1,1,1,-1,1,-1,1,-1,-1,1,-1,1,1,
                    0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1), ncol=(m+4), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L","M","N","O","P")
    des <- data.frame(des)
  }
  if (m==12)  {
    f1 <- matrix(c(0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                   1,0,1,1,-1,1,-1,-1,1,-1,1,1,-1,1,-1,-1,
                   1,-1,0,1,1,-1,1,-1,1,1,1,-1,1,-1,-1,-1,
                   1,-1,-1,0,1,1,-1,1,1,1,-1,1,-1,-1,-1,1,
                   1,1,-1,-1,0,1,1,-1,1,-1,1,-1,-1,-1,1,1,
                   1,-1,1,-1,-1,0,1,1,1,1,-1,-1,-1,1,1,-1,
                   1,1,-1,1,-1,-1,0,1,1,-1,-1,-1,1,1,-1,1,
                   1,1,1,-1,1,-1,-1,0,1,-1,-1,1,1,-1,1,-1,
                   -1,1,1,1,1,1,1,1,0,-1,-1,-1,-1,-1,-1,-1,
                   -1,-1,1,1,-1,1,-1,-1,1,0,-1,-1,1,-1,1,1,
                   -1,1,1,-1,1,-1,-1,-1,1,1,0,-1,-1,1,-1,1,
                   -1,1,-1,1,-1,-1,-1,1,1,1,1,0,-1,-1,1,-1,
                   -1,-1,1,-1,-1,-1,1,1,1,-1,1,1,1,-1,-1,1,
                   -1,1,-1,-1,-1,1,1,-1,1,1,-1,1,1,1,-1,-1,
                   -1,-1,-1,-1,1,1,-1,1,1,-1,1,-1,1,1,1,-1,
                   -1,-1,-1,1,1,-1,1,-1,1,-1,-1,1,-1,1,1,1,
                    0,0,0,0,0,0,0,0,0,0,0,0,-1,-1,-1,-1), ncol=(m+4), byrow=TRUE)
    f2 <- (-1)*f1
    des <- rbind(f1,f2)
    ord<-stdord(nrow(f1))
    des <- des[ord , ]
    colnames(des) <- c("A","B","C","D","E","F","G","H","J","K","L","M","N","O","P","Q")
    des <- data.frame(des)
  }

}
  
  nr <- nrow(des)
  ncd<-ncol(des)
  if(center>0){
    cpr<-c(rep(0,ncd))
    for (i in 1:center){
      des<-rbind(des,cpr)    
    }
   if(randomize==TRUE) {des <- des[sample(1:nr+center), ]}
  }
  if(center==0){
    if(randomize==TRUE) {des <- des[sample(1:nr), ]}
  }
    
return(des)
                                          }





