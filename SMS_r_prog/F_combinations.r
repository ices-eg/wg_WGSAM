
if (sce=='HCR_1_deter_adjust_single' || sce=='HCR_1_deter20_adjust_single' ||
    sce=='HCR_1_deter_single' || sce=='HCR_1_deter20_single') {
  cat("Scenario: ",sce,'\n')
  #my.OP.output<-15  # detailed output for test 
  
  ss<-0.01 
  rr<-matrix(c(
    0.0, 0.50, ss,     #  COD   
    0.00, 0.15, ss,      # WHG
    0.30, 0.60, ss,     #  HAD
    0.15, 0.60, ss,     # POK omkring 0.37 with hockey and 0.40 with Ricker
    0.10, 0.45, ss,     #  HER
    0.10, 0.60, ss,     # North SAN
    0.15, 0.50, ss,     # South SAN
    0.30, 0.60, ss,     #  NOP
    0.35, 0.70, ss,     # SPR
    0.2,  0.4,  ss,  #  PLE
    0.2,  0.3,  ss )  #  SOL
    ,ncol=3,byrow=T)
  dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
  
  
  rr<-matrix(c(
    0.10, 0.50, ss,     #  COD   
    0.00, 0.60, ss,      # WHG
    0.30, 0.90, ss,     #  HAD
    0.15, 0.60, ss,     # POK 
    0.20, 0.70, ss,     #  HER
    0.10, 1.0, ss,     # North SAN
    0.10, 0.80, ss,     # South SAN
    0.4, 1.5, ss,     #  NOP
    0.4, 1.5, ss,     # SPR
    0.2,  0.4,  ss,  #  PLE
    0.2,  0.3,  ss )  #  SOL
    ,ncol=3,byrow=T)
  dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
}



if (sce=='HCR_1_deter_had_adjust_wide' | sce=='HCR_1_deter_adjust_wide') {
  cat("Scenario: ",sce,'\n')
  #my.OP.output<-15  # detailed output for test 
  if (grepl('_had_',sce)) recruit.adjust.factor[3]<-0.75 # haddock adjust
  
  ss<-0.05 
  rr<-matrix(c(
    0.25, 0.50, ss,     #  COD   
    0.05, 0.15, ss,      # WHG
    0.30, 0.60, ss,     #  HAD
    0.35, 0.35, ss,     # POK omkring 0.37 with hockey and 0.40 with Ricker
    0.20, 0.40, ss,     #  HER
    0.30, 0.60, ss,     # North SAN
    0.25, 0.50, ss,     # South SAN
    0.30, 0.60, ss,     #  NOP
    0.35, 0.70, ss,     # SPR
    PLE.seq, PLE.seq, 0,  #  PLE
    SOL.seq, SOL.seq, 0)  #  SOL
    ,ncol=3,byrow=T)
  dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
}

if (sce=='HCR_2_deter_noadjust_wide' | sce=='HCR_2_deter_adjust_wide' | sce=='HCR_2_deter_had_adjust_wide' | 
    sce=='HCR_2_deter_whg_noadjust_wide' ) {
  cat("Scenario: ",sce,'\n')
  #my.OP.output<-15  # detailed output for test 
  if (sce=='HCR_2_deter_had_adjust_wide') recruit.adjust.factor[3]<-0.75
  if (sce=='HCR_2_deter_whg_noadjust_wide') recruit.adjust.factor[2]<-1.2
  
  ss<-0.1 
  #s<-0.05 
  rr<-matrix(c(
    0.30, 0.60, ss,     #  COD   
    0.0,  0.1, ss,      # WHG
    0.30, 0.60, ss,     #  HAD
    0.30, 0.40, ss,     # POK
    0.20, 0.40, ss,     #  HER
    0.20, 0.60, ss,     # North SAN
    0.20, 0.60, ss,     # South SAN
    0.20, 0.60, ss,     #  NOP
    0.20, 0.70, ss,     # SPR
    PLE.seq, PLE.seq, 0,  #  PLE
    SOL.seq, SOL.seq, 0)  #  SOL
    ,ncol=3,byrow=T)
  dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
}


if (sce=='HCR_2_deter_had_adjust_narrow' ) {
  cat("Scenario: ",sce,'\n')
  #my.OP.output<-15  # detailed output for test 
  recruit.adjust.factor[3]<-0.75 # haddock adjust
  
  ss<-0.05 
  rr<-matrix(c(
    0.30, 0.60, ss,     #  COD   
    0.0,  0.05, ss,      # WHG
    0.30, 0.60, ss,     #  HAD
    0.35, 0.40, ss,     # POK
    0.25, 0.40, ss,     #  HER
    0.40, 0.60, ss,     # North SAN
    0.30, 0.50, ss,     # South SAN
    0.30, 0.60, ss,     #  NOP
    0.40, 0.70, ss,     # SPR
    PLE.seq, PLE.seq, 0,  #  PLE
    SOL.seq, SOL.seq, 0)  #  SOL
    ,ncol=3,byrow=T)
  dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
}


if (sce=='HCR_2_deter_had_adjust_Athen' | sce=='HCR_2_deter_had_adjust_Athen_op1' | 
    sce=='HCR_2_deter_had_adjust_Athen_op2' | sce=='HCR_2_deter_had_adjust_Athen_op1') {
  cat("Scenario: ",sce,'\n')
  #my.OP.output<-15  # detailed output for test 
  recruit.adjust.factor[3]<-0.75 # haddock adjust
  
  ss<-0.05 
  rr<-matrix(c(
    0.30, 0.60, ss,     #  COD   
    0.05, 0.15, ss,     # WHG
    0.30, 0.60, ss,     #  HAD
    0.40, 0.40, ss,     # POK
    0.25, 0.40, ss,     #  HER
    0.40, 0.60, ss,     # North SAN
    0.30, 0.50, ss,     # South SAN
    0.30, 0.60, ss,     #  NOP
    0.40, 0.70, ss,     # SPR
    PLE.seq, PLE.seq, 0,  #  PLE
    SOL.seq, SOL.seq, 0)  #  SOL
    ,ncol=3,byrow=T)
  dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
}

if (sce=='HCR_2_deter_had_adjust_Athen_op4') {
  cat("Scenario: ",sce,'\n')
  #my.OP.output<-15  # detailed output for test 
  recruit.adjust.factor[3]<-0.75 # haddock adjust
  
  ss<-0.02 
  rr<-matrix(c(
    0.28, 0.32, ss,     #  COD   
    0.06, 0.10, ss,     # WHG
    0.52, 0.60, ss,     #  HAD
    0.40, 0.40, ss,     # POK
    0.32, 0.40, ss,     #  HER
    0.50, 0.58, ss,     # North SAN
    0.34, 0.46, ss,     # South SAN
    0.46, 0.54, ss,     #  NOP
    0.62, 0.70, ss,     # SPR
    PLE.seq, PLE.seq, 0,  #  PLE
    SOL.seq, SOL.seq, 0)  #  SOL
    ,ncol=3,byrow=T)
  dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
}

if (sce=='HCR_1_deter_had_adjust_narrow' ) {
  cat("Scenario: ",sce,'\n')
  #my.OP.output<-15  # detailed output for test 
  recruit.adjust.factor[3]<-0.75 # haddock adjust
  
  ss<-0.02 
  rr<-matrix(c(
    0.28, 0.32, ss,     #  COD   
    0.06, 0.10, ss,     # WHG
    0.50, 0.60, ss,     #  HAD
    0.40, 0.40, ss,     # POK
    0.30, 0.40, ss,     #  HER
    0.46, 0.56, ss,     # North SAN
    0.30, 0.44, ss,     # South SAN
    0.40, 0.50, ss,     #  NOP
    0.60, 0.70, ss,     # SPR
    PLE.seq, PLE.seq, 0,  #  PLE
    SOL.seq, SOL.seq, 0)  #  SOL
    ,ncol=3,byrow=T)
  dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
}


if (sce=='HCR_1_deter_adjust_narrow' ) {
  cat("Scenario: ",sce,'\n')
  #my.OP.output<-15  # detailed output for test 
  #recruit.adjust.factor[3]<-0.75 # haddock adjust
  
  ss<-0.02 
  rr<-matrix(c(
    0.28, 0.32, ss,     #  COD   
    0.04, 0.10, ss,     # WHG
    0.38, 0.44, ss,     #  HAD
    0.34, 0.38, ss,     # POK
    0.32, 0.40, ss,     #  HER
    0.36, 0.44, ss,     # North SAN
    0.46, 0.54, ss,     # South SAN
    0.46, 0.52, ss,     #  NOP
    0.32, 0.40, ss,     # SPR
    PLE.seq, PLE.seq, 0,  #  PLE
    SOL.seq, SOL.seq, 0)  #  SOL
    ,ncol=3,byrow=T)
  dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
}






if (sce=='HCR_2_deter_had_adjust_Athen2') {
  cat("Scenario: ",sce,'\n')
  #my.OP.output<-15  # detailed output for test 
  recruit.adjust.factor[3]<-0.75 # haddock adjust
  
  ss<-0.025 
  rr<-matrix(c(
    0.30, 0.35, ss,     #  COD   
    0.10, 0.15, ss,      # WHG
    0.525, 0.60, ss,     #  HAD
    0.40, 0.40, ss,     # POK
    0.325, 0.40, ss,     #  HER
    0.475, 0.55, ss,     # North SAN
    0.30, 0.475, ss,     # South SAN
    0.475, 0.55, ss,     #  NOP
    0.625, 0.70, ss,     # SPR
    PLE.seq, PLE.seq, 0,  #  PLE
    SOL.seq, SOL.seq, 0)  #  SOL
    ,ncol=3,byrow=T)
  dimnames(rr)[[1]]<-sp.names[first.VPA:nsp]
}


