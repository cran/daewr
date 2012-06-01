ModelRobust <- function(des='') {
library(daewr)
#MR8m4g3<-MR8m5g2<-MR8m6g1<-MR12m5g5<-MR12m6g5<-MR12m7g4<-MR12m8g3<-MR12m9g2<-MR16m7g5<-MR16m8g5<-MR16m9g5<-MR16m10g3 <- NULL # sets variables to null value first 
  if (des=='')  {
cat(" ", "\n")
cat("       Model Robust Factorial Designs","\n")
cat(" ", "\n")
cat(" Li and Nachtsheim, JQT(2000) p. 345-352","\n")
cat(" ", "\n")
cat(format(" ",width=11),format(" ",width=7),format(" ",width=8),format("    g",width=13),"\n")
cat(format(" ",width=11),format(" ",width=7),format(" ",width=8),format("  Maximum",width=13),"\n")
cat(format(" ",width=11),format(" ",width=7),format("  m",width=8),format("  # of",width=13),"\n")
cat(format("Design Name",width=11),format("  runs",width=7),format("factors",width=8),format("interactions",width=13),"\n")
cat("----------------------------------------","\n")
cat(format("MR8m4g3", width=11),format("   8",width=7),format(" 4",width=8),format("    3",width=13),"\n")
cat(format("MR8m5g2", width=11),format("   8",width=7),format(" 5",width=8),format("    2",width=13),"\n")
cat(format("MR8m6g1", width=11),format("   8",width=7),format(" 6",width=8),format("    1",width=13),"\n")
cat(format("MR12m5g5", width=11),format("   12",width=7),format(" 5",width=8),format("    5",width=13),"\n")
cat(format("MR12m6g5", width=11),format("   12",width=7),format(" 6",width=8),format("    5",width=13),"\n")
cat(format("MR12m7g4", width=11),format("   12",width=7),format(" 7",width=8),format("    4",width=13),"\n")
cat(format("MR12m8g3", width=11),format("   12",width=7),format(" 8",width=8),format("    3",width=13),"\n")
cat(format("MR12m9g2", width=11),format("   12",width=7),format(" 9",width=8),format("    2",width=13),"\n")
cat(format("MR16m7g5", width=11),format("   16",width=7),format(" 7",width=8),format("    5",width=13),"\n")
cat(format("MR16m8g5", width=11),format("   16",width=7),format(" 8",width=8),format("    5",width=13),"\n")
cat(format("MR16m9g5", width=11),format("   16",width=7),format(" 9",width=8),format("    5",width=13),"\n")
cat(format("MR16m10g3", width=11),format("   16",width=7),format("10",width=8),format("    3",width=13),"\n")
cat(" ","\n")
cat("==> to retrieve a design type ModelRobust('MR8m4g3') etc.","\n")
         } else if (des=='MR8m4g3') {data(MR8m4g3)
                                    MR8m4g3
         } else if(des=='MR8m5g2') {data(MR8m5g2)
                                   MR8m5g2
         } else if(des=='MR8m6g1') {data(MR8m6g1)
                                   MR8m6g1
         } else if(des=='MR12m5g5') {data(MR12m5g5)
                                    MR12m5g5
         } else if(des=='MR12m6g5') {data(MR12m6g5)
                                    MR12m6g5
         } else if(des=='MR12m7g4') {data(MR12m7g4)
                                    MR12m7g4
         } else if(des=='MR12m8g3') {data(MR12m8g3)
                                    MR12m8g3
         } else if(des=='MR12m9g2') {data(MR12m9g2)
                                    MR12m9g2
         } else if(des=='MR16m7g5') {data(MR16m7g5)
                                    MR16m7g5
         } else if(des=='MR16m8g5') {data(MR16m8g5)
                                    MR16m8g5
         } else if(des=='MR16m9g5') {data(MR16m9g5)
                                    MR16m9g5
         } else if(des=='MR16m10g3') {data(MR16m10g3)
                                    MR16m10g3
         } else cat(" Design name misspelled","\n")
                 }
