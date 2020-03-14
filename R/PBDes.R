PBDes<-function(nruns,nfactors,randomize=FALSE)
   { 
# 12, 20, and 24 run Plackett-Burman Designs
  irun<-1
  if (nruns == 12) {irun<-0}
  if (nruns == 20) {irun<-0}
  if (nruns == 24) {irun<-0}
  if(irun>0) {stop("This function only works for nruns=12 or nruns=20, or nruns=24") }
  if (nruns==12){
    if (nfactors < 5) {stop("At least 5 factors required for 12-run PB design")}
    if (nfactors > 11){stop("No more than 11 factors can be used for 12 run PB design")}
                }
  if (nruns==20){
    if (nfactors < 8) {stop("At least 8 factors required for 20-run PB design")}
    if (nfactors > 19){stop("No more than 19 factors can be used for 20 run PB design")} 
                }
      
  if (nruns==24){
      if (nfactors < 15) {stop("At least 15 factors required for 24-run PB design")}
    if (nfactors > 23){stop("No more than 23 factors can be used for 20 run PB design")}  
                }
if(nruns==12){
  # Create 12-run PB design by cyclically rotating factor levels from first row
  pb12r1<-c( 1,  1, -1,  1,  1,  1, -1, -1, -1,   1,  -1)
  pb12r2<-c(pb12r1[11],pb12r1[1:10])
  pb12r3<-c(pb12r2[11],pb12r2[1:10])
  pb12r4<-c(pb12r3[11],pb12r3[1:10])
  pb12r5<-c(pb12r4[11],pb12r4[1:10])
  pb12r6<-c(pb12r5[11],pb12r5[1:10])
  pb12r7<-c(pb12r6[11],pb12r6[1:10])
  pb12r8<-c(pb12r7[11],pb12r7[1:10])
  pb12r9<-c(pb12r8[11],pb12r8[1:10])
  pb12r10<-c(pb12r9[11],pb12r9[1:10])
  pb12r11<-c(pb12r10[11],pb12r10[1:10])
  pb12r12<-rep(-1,11)
  des<-rbind(pb12r1,pb12r2,pb12r3,pb12r4,pb12r5,pb12r6,pb12r7,pb12r8,pb12r9,pb12r10,pb12r11,pb12r12)
  randord<-sample(1:12)
  if (randomize==TRUE){des <- des[randord, ] }
  colnames(des)<-c("A","B","C","D","E","F","G","H","J","K","L")
  row.names(des)<-c(1:12)
         }
if(nruns==20){  
# Create 20-run PB design by cyclically rotating factor levels from first row
pb20r1<-c(1,1,-1,-1,1,1,1,1,-1,1,-1,1,-1,-1,-1,-1,1,1,-1)
pb20r2<-c(pb20r1[19],pb20r1[1:18])
pb20r3<-c(pb20r2[19],pb20r2[1:18])
pb20r4<-c(pb20r3[19],pb20r3[1:18])
pb20r5<-c(pb20r4[19],pb20r4[1:18])
pb20r6<-c(pb20r5[19],pb20r5[1:18])
pb20r7<-c(pb20r6[19],pb20r6[1:18])
pb20r8<-c(pb20r7[19],pb20r7[1:18])
pb20r9<-c(pb20r8[19],pb20r8[1:18])
pb20r10<-c(pb20r9[19],pb20r9[1:18])
pb20r11<-c(pb20r10[19],pb20r10[1:18])
pb20r12<-c(pb20r11[19],pb20r11[1:18])
pb20r13<-c(pb20r12[19],pb20r12[1:18])
pb20r14<-c(pb20r13[19],pb20r13[1:18])
pb20r15<-c(pb20r14[19],pb20r14[1:18])
pb20r16<-c(pb20r15[19],pb20r15[1:18])
pb20r17<-c(pb20r16[19],pb20r16[1:18])
pb20r18<-c(pb20r17[19],pb20r17[1:18])
pb20r19<-c(pb20r18[19],pb20r18[1:18])
pb20r20<-c(rep(-1,19))
des<-rbind(pb20r1,pb20r2,pb20r3,pb20r4,pb20r5,pb20r6,
           pb20r7,pb20r8,pb20r9,pb20r10,pb20r11,pb20r12,
           pb20r13,pb20r14,pb20r15,pb20r16,pb20r17,pb20r18,
           pb20r19,pb20r20)
randord<-sample(1:20)
if (randomize==TRUE){des <- des[randord, ] }
colnames(des)<-c("A","B","C","D","E","F","G","H","J","K",
                 "L","M","N","O","P","Q","R","S","T")
row.names(des)<-c(1:20)
           }
if(nruns==24){   
# Create 24-run PB design by cyclically rotating factor levels from first row
pb24r1<-c(1,1,1,1,1,-1,1,-1,1,1,-1,-1,1,1,-1,-1,1,-1,1,-1,-1,-1,-1)
pb24r2<-c(pb24r1[23],pb24r1[1:22])
pb24r3<-c(pb24r2[23],pb24r2[1:22])
pb24r4<-c(pb24r3[23],pb24r3[1:22])
pb24r5<-c(pb24r4[23],pb24r4[1:22])
pb24r6<-c(pb24r5[23],pb24r5[1:22])
pb24r7<-c(pb24r6[23],pb24r6[1:22])
pb24r8<-c(pb24r7[23],pb24r7[1:22])
pb24r9<-c(pb24r8[23],pb24r8[1:22])
pb24r10<-c(pb24r9[23],pb24r9[1:22])
pb24r11<-c(pb24r10[23],pb24r10[1:22])
pb24r12<-c(pb24r11[23],pb24r11[1:22])
pb24r13<-c(pb24r12[23],pb24r12[1:22])
pb24r14<-c(pb24r13[23],pb24r13[1:22])
pb24r15<-c(pb24r14[23],pb24r14[1:22])
pb24r16<-c(pb24r15[23],pb24r15[1:22])
pb24r17<-c(pb24r16[23],pb24r16[1:22])
pb24r18<-c(pb24r17[23],pb24r17[1:22])
pb24r19<-c(pb24r18[23],pb24r18[1:22])
pb24r20<-c(pb24r19[23],pb24r19[1:22])
pb24r21<-c(pb24r20[23],pb24r20[1:22])
pb24r22<-c(pb24r21[23],pb24r21[1:22])
pb24r23<-c(pb24r22[23],pb24r22[1:22])
pb24r24<-c(rep(-1,23))
des<-rbind(pb24r1,pb24r2,pb24r3,pb24r4,pb24r5,pb24r6,
           pb24r7,pb24r8,pb24r9,pb24r10,pb24r11,pb24r12,
           pb24r13,pb24r14,pb24r15,pb24r16,pb24r17,pb24r18,
           pb24r19,pb24r20,pb24r21,pb24r22,pb24r23,pb24r24)
randord<-sample(1:24)
if (randomize==TRUE){des <- des[randord, ] }
colnames(des)<-c("A","B","C","D","E","F","G","H","J","K",
                 "L","M","N","O","P","Q","R","S","T","U",
                 "V","W","X")
row.names(des)<-c(1:24)
}
des<-as.data.frame(des[,1:nfactors] )
return(des)
  }

