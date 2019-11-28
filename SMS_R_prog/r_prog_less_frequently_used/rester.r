

          targetFs=NA   # combinations of F
){



if (F) {
  scenario<-"PLE-SOL-fixed-F"

  if (stochastic.recruitment) no.iter<-no.iter.stoch
  #          Cod     Whiting     Haddock      Saithe     Herring     Sandeel   Nor. pout       Sprat      Plaice        Sole
  HCR<- c(    1,          1,          1,          1,          1,          1,          1,          1,          1,          1)


   dim(targetFs)
}
# result from previous run
PLE.seq<-seq(0.30,0.30,0.05)
SOL.seq<-seq(0.40,0.40,0.05)


if (F) {
  scenario<-"saithe-HCR1"
  stochastic.recruitment<-T
  if (stochastic.recruitment) no.iter<-no.iter.stoch

  targetFs<-expand.grid(
       COD=seq(0.50,0.50,0.05),
       WHG=seq(0.30,0.30,0.05),
       HAD=seq(0.35,0.35,0.05),
       POK=seq(0.30,0.60,0.05),
       HER=seq(0.30,0.30,0.05),
       SAN=seq(0.25,0.25,0.05),
       NOR=seq(0.25,0.25,0.05),
       SPR=seq(0.30,0.30,0.05),
       PLE=PLE.seq,
       SOL=SOL.seq)
   dim(targetFs)
}
POK.seq<-seq(0.45,0.45,0.05)


if (T) {
  scenario<-"saithe-HCR2"
  stochastic.recruitment<-T
  if (stochastic.recruitment) no.iter<-no.iter.stoch
  #          Cod     Whiting     Haddock      Saithe     Herring     Sandeel   Nor. pout       Sprat      Plaice        Sole
  HCR<- c(    1,          1,          1,          2,          1,          1,          1,          1,          1,          1)

  targetFs<-expand.grid(
       COD=seq(0.50,0.50,0.05),
       WHG=seq(0.30,0.30,0.05),
       HAD=seq(0.35,0.35,0.05),
       POK=seq(0.30,0.60,0.05),
       HER=seq(0.30,0.30,0.05),
       SAN=seq(0.25,0.25,0.05),
       NOR=seq(0.25,0.25,0.05),
       SPR=seq(0.30,0.30,0.05),
       PLE=PLE.seq,
       SOL=SOL.seq)
}
POK.seq<-seq(0.55,0.60,0.05)

if (F) {
  scenario<-"HCR-2"
  stochastic.recruitment<-T

  targetFs<-expand.grid(
       COD=seq(0.40,0.60,0.05),     #COD=seq(0.40,0.60,0.05),
       WHG=seq(0.20,0.40,0.05),
       HAD=seq(0.20,0.45,0.05),
       POK=POK.seq,
       HER=seq(0.30,0.50,0.05),
       SAN=seq(0.40,0.60,0.05),
       NOR=seq(0.25,0.60,0.05),
       SPR=seq(0.5,0.65,0.05),
       PLE=PLE.seq,
       SOL=SOL.seq)
        dim(targetFs)
        dim(targetFs) /7000
}

if (F) {
  scenario<-"HCR-fin9"   # new run based on the FMSY stimated in previous runs (however with updated output on realized F)
  stochastic.recruitment<-T
  #          Cod     Whiting     Haddock      Saithe     Herring     Sandeel   Nor. pout       Sprat      Plaice        Sole
  HCR<- c(    2,          2,          2,          2,          2,         22,         22,         22,          2,          2)

  targetFs<-expand.grid(
       COD=seq(0.50,0.50,0.05),
       WHG=seq(0.30,0.60,0.05),
       HAD=seq(0.25,0.45,0.05),
       POK=POK.seq,
       HER=seq(0.50,0.60,0.05),
       SAN=seq(0.50,0.60,0.05),
       NOR=seq(0.50,0.60,0.05),
       SPR=seq(0.5,0.60,0.05),
       PLE=seq(0.35,0.35,0.05),
       SOL=seq(0.35,0.35,0.05))
              dim(targetFs)
        dim(targetFs) /7000
}


if (F) {
  scenario<-"HCR-fin99"   # new run based on the FMSY stimated in previous runs (however with updated output on realized F)
  stochastic.recruitment<-T
  #          Cod     Whiting     Haddock      Saithe     Herring     Sandeel   Nor. pout       Sprat      Plaice        Sole
  HCR<- c(    2,          2,          2,          2,          2,         22,         22,         22,          2,          2)

  targetFs<-expand.grid(
       COD=seq(0.50,0.50,0.05),
       WHG=seq(0.30,0.60,0.30),
       HAD=seq(0.35,0.35,0.05),
       POK=POK.seq,
       HER=seq(0.55,0.55,0.05),
       SAN=seq(0.55,0.55,0.05),
       NOR=seq(0.60,0.60,0.05),
       SPR=seq(0.55,0.55,0.05),
       PLE=seq(0.35,0.35,0.05),
       SOL=seq(0.35,0.35,0.05))
              dim(targetFs)
        dim(targetFs) /7000
}



if (F) {
  scenario<-"Baltic test"      #Target F for combinations of F
  stochastic.recruitment<-T
  if (stochastic.recruitment) no.iter<-no.iter.stoch

  adjustBaltic<-F
  #          Cod     Herring,     Sprat
  HCR<- c(    1,          1,          1)
  # wide  targetFs<-expand.grid(COD=seq(0.3,0.7,0.1),HER=seq(0.2,0.4,0.05),SPR=seq(0.2,0.4,0.05))
  # narrow  targetFs<-expand.grid(COD=seq(0.4,0.6,0.05),HER=seq(0.25,0.35,0.025),SPR=seq(0.25,0.35,0.025))

  #test
  targetFs<-expand.grid(
     COD=seq(0.4,0.5,0.05),
     HER=seq(0.25,0.35,0.05),
     SPR=seq(0.25,0.35,0.05))
}
