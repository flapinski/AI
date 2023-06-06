

learn = function(hist){
  Pn0 = nrow(hist[hist$Pn==0, ]) + 1
  Pn1 = nrow(hist[hist$Pn==1, ]) + 1
  P_Pn = data.frame(Pn0,Pn1)
  P_Pn = P_Pn/rowSums(P_Pn)

  VTB0 = nrow(hist[hist$VTB==0, ]) + 1
  VTB1 = nrow(hist[hist$VTB==1, ]) + 1
  P_VTB = data.frame(VTB0, VTB1)
  P_VTB = P_VTB/rowSums(P_VTB)

  Sm0 = nrow(hist[hist$Sm==0,]) + 1
  Sm1 = nrow(hist[hist$Sm==1,]) + 1
  P_Sm = data.frame(Sm0, Sm1)
  P_Sm = P_Sm/rowSums(P_Sm)

  Br0_Sm0 = nrow(hist[hist$Br==0 & hist$Sm == 0,]) + 1
  Br0_Sm1 = nrow(hist[hist$Br==0 & hist$Sm == 1,]) + 1
  Br1_Sm0 = nrow(hist[hist$Br==1 & hist$Sm == 0,]) + 1
  Br1_Sm1 = nrow(hist[hist$Br==1 & hist$Sm == 1,]) + 1
  Br0 = c(Br0_Sm0,Br0_Sm1)
  Br1 = c(Br1_Sm0,Br1_Sm1)
  P_Br = data.frame(Br0, Br1)
  P_Br = P_Br/rowSums(P_Br)
  P_Br
  P_Br = cbind(Sm = c(0,1),P_Br)
  P_Br

  TB0_VTB0 = nrow(hist[hist$TB==0 & hist$VTB == 0,]) + 1
  TB0_VTB1 = nrow(hist[hist$TB==0 & hist$VTB == 1,]) + 1
  TB1_VTB0 = nrow(hist[hist$TB==1 & hist$VTB == 0,]) + 1
  TB1_VTB1 = nrow(hist[hist$TB==1 & hist$VTB == 1,]) + 1
  TB0 = c(TB0_VTB0, TB0_VTB1)
  TB1 = c(TB1_VTB0, TB1_VTB1)
  P_TB = data.frame(TB0,TB1)
  P_TB = P_TB/rowSums(P_TB)
  P_TB = cbind(VTB = c(0,1), P_TB)

  LC0_Sm0 = nrow(hist[hist$LC==0 & hist$Sm == 0,]) + 1
  LC0_Sm1 = nrow(hist[hist$LC==0 & hist$Sm == 1,]) + 1
  LC1_Sm0 = nrow(hist[hist$LC==1 & hist$Sm == 0,]) + 1
  LC1_Sm1 = nrow(hist[hist$LC==1 & hist$Sm == 1,]) + 1
  LC0 = c(LC0_Sm0, LC0_Sm1)
  LC1 = c(LC1_Sm0, LC1_Sm1)
  P_LC = data.frame(LC0, LC1)
  P_LC = P_LC/rowSums(P_LC)
  P_LC = cbind(Sm = c(0,1), P_LC)

  Te0 = hist[hist$Pn == 0, ]$Te
  Te0mean = mean(Te0)
  Te0sd = sqrt(var(Te0))
  Te1 = hist[hist$Pn == 1, ]$Te
  Te1mean = mean(Te1)
  Te1sd = sqrt(var(Te1))
  Te_mean = c(Te0mean, Te1mean)
  Te_sd = c(Te0sd, Te1sd)
  P_Te = data.frame(Te_mean, Te_sd)
  P_Te = cbind(Pn = c(0,1), P_Te)

  Dy0_LC0_Br0 = nrow(hist[hist$Dy==0 & hist$LC == 0 & hist$Br == 0,]) + 1
  Dy1_LC0_Br0 = nrow(hist[hist$Dy==1 & hist$LC == 0 & hist$Br == 0,]) + 1
  Dy0_LC1_Br0 = nrow(hist[hist$Dy==0 & hist$LC == 1 & hist$Br == 0,]) + 1
  Dy1_LC1_Br0 = nrow(hist[hist$Dy==1 & hist$LC == 1 & hist$Br == 0,]) + 1
  Dy0_LC0_Br1 = nrow(hist[hist$Dy==0 & hist$LC == 0 & hist$Br == 1,]) + 1
  Dy1_LC0_Br1 = nrow(hist[hist$Dy==1 & hist$LC == 0 & hist$Br == 1,]) + 1
  Dy0_LC1_Br1 = nrow(hist[hist$Dy==0 & hist$LC == 1 & hist$Br == 1,]) + 1
  Dy1_LC1_Br1 = nrow(hist[hist$Dy==1 & hist$LC == 1 & hist$Br == 1,]) + 1
  Dy0 = c(Dy0_LC0_Br0, Dy0_LC1_Br0, Dy0_LC0_Br1, Dy0_LC1_Br1)
  Dy1 = c(Dy1_LC0_Br0, Dy1_LC1_Br0, Dy1_LC0_Br1, Dy1_LC1_Br1)
  P_Dy = data.frame(Dy0, Dy1)
  P_Dy = P_Dy/rowSums(P_Dy)
  P_Dy = cbind(LC = c(0,1,0,1), Br = c(0,0,1,1), P_Dy)

  XR0_Pn0_TB0_LC0 = nrow(hist[hist$XR==0 & hist$Pn == 0 & hist$TB == 0 & hist$LC == 0,]) + 1
  XR1_Pn0_TB0_LC0 = nrow(hist[hist$XR==1 & hist$Pn == 0 & hist$TB == 0 & hist$LC == 0,]) + 1
  XR0_Pn1_TB0_LC0 = nrow(hist[hist$XR==0 & hist$Pn == 1 & hist$TB == 0 & hist$LC == 0,]) + 1
  XR1_Pn1_TB0_LC0 = nrow(hist[hist$XR==1 & hist$Pn == 1 & hist$TB == 0 & hist$LC == 0,]) + 1
  XR0_Pn0_TB1_LC0 = nrow(hist[hist$XR==0 & hist$Pn == 0 & hist$TB == 1 & hist$LC == 0,]) + 1
  XR1_Pn0_TB1_LC0 = nrow(hist[hist$XR==1 & hist$Pn == 0 & hist$TB == 1 & hist$LC == 0,]) + 1
  XR0_Pn0_TB0_LC1 = nrow(hist[hist$XR==0 & hist$Pn == 0 & hist$TB == 0 & hist$LC == 1,]) + 1
  XR1_Pn0_TB0_LC1 = nrow(hist[hist$XR==1 & hist$Pn == 0 & hist$TB == 0 & hist$LC == 1,]) + 1
  XR0_Pn1_TB1_LC0 = nrow(hist[hist$XR==0 & hist$Pn == 1 & hist$TB == 1 & hist$LC == 0,]) + 1
  XR1_Pn1_TB1_LC0 = nrow(hist[hist$XR==1 & hist$Pn == 1 & hist$TB == 1 & hist$LC == 0,]) + 1
  XR0_Pn1_TB0_LC1 = nrow(hist[hist$XR==0 & hist$Pn == 1 & hist$TB == 0 & hist$LC == 1,]) + 1
  XR1_Pn1_TB0_LC1 = nrow(hist[hist$XR==1 & hist$Pn == 1 & hist$TB == 0 & hist$LC == 1,]) + 1
  XR0_Pn0_TB1_LC1 = nrow(hist[hist$XR==0 & hist$Pn == 0 & hist$TB == 1 & hist$LC == 1,]) + 1
  XR1_Pn0_TB1_LC1 = nrow(hist[hist$XR==1 & hist$Pn == 0 & hist$TB == 1 & hist$LC == 1,]) + 1
  XR0_Pn1_TB1_LC1 = nrow(hist[hist$XR==0 & hist$Pn == 1 & hist$TB == 1 & hist$LC == 1,]) + 1
  XR1_Pn1_TB1_LC1 = nrow(hist[hist$XR==1 & hist$Pn == 1 & hist$TB == 1 & hist$LC == 1,]) + 1
  XR0 = c(XR0_Pn0_TB0_LC0, XR0_Pn1_TB0_LC0, XR0_Pn0_TB1_LC0, XR0_Pn0_TB0_LC1, XR0_Pn1_TB1_LC0, XR0_Pn1_TB0_LC1, XR0_Pn0_TB1_LC1, XR0_Pn1_TB1_LC1)
  XR1 = c(XR1_Pn0_TB0_LC0, XR1_Pn1_TB0_LC0, XR1_Pn0_TB1_LC0, XR1_Pn0_TB0_LC1, XR1_Pn1_TB1_LC0, XR1_Pn1_TB0_LC1, XR1_Pn0_TB1_LC1, XR1_Pn1_TB1_LC1)
  P_XR = data.frame(XR0, XR1)
  P_XR = P_XR/rowSums(P_XR)
  P_XR = cbind(Pn = c(0,1,0,0,1,1,0,1), TB = c(0,0,1,0,1,0,1,1), LC = c(0,0,0,1,0,1,1,1),  P_XR)

  results = list(P_Pn, P_VTB, P_Sm, P_TB, P_LC, P_Br, P_Te, P_XR, P_Dy)
  return(results)
  }

diagnose = function(bn, cases){
  runs = 2950
  rnumbers = runif(4*runs)
  # Initialize
  #bn = learn(hist)  # when running diagnose this line can be removed
  P_Pn = as.data.frame(bn[[1]])
  P_VTB = as.data.frame(bn[[2]])
  P_Sm = as.data.frame(bn[[3]])
  P_TB = as.data.frame(bn[[4]])
  P_LC = as.data.frame(bn[[5]])
  P_Br = as.data.frame(bn[[6]])
  P_Te = as.data.frame(bn[[7]])
  P_XR = as.data.frame(bn[[8]])
  P_Dy = as.data.frame(bn[[9]])
  
  output = matrix(ncol = 4, nrow = 10)

  for (i in 1:nrow(cases)){
  results = data.frame(matrix(ncol = 9, nrow = runs))
  colnames(results) = c('Pn', 'Te', 'VTB', 'TB', 'Sm', 'LC', 'Br', 'XR', 'Dy')
  x = as.list(t(cases[i,]))

  Te = cases[i,2]
  VTB = cases[i,3]
  Sm = cases[i,5]
  Dy = cases[i,9]
  XR = cases[i,8]

  initval = rbinom(4,1,0.5)
  Pn = initval[1]
  TB = initval[2]
  LC = initval[3]
  Br = initval[4]
  
  for (j in 1:runs){
  ## Update Pn:
  Pn_old = Pn
  P_old = P_Pn[1,1+Pn_old]*dnorm(Te,P_Te[Pn_old+1,2], P_Te[Pn_old+1,3])*P_XR[P_XR$Pn == Pn_old & P_XR$TB == TB & P_XR$LC == LC, ][1,4+XR]
  Pn_new = abs(Pn-1)
  P_new = P_Pn[1,1+Pn_new]*dnorm(Te,P_Te[Pn_new+1,2], P_Te[Pn_new+1,3])*P_XR[P_XR$Pn == Pn_new & P_XR$TB == TB & P_XR$LC == LC, ][1,4+XR]
  Pquotient = P_new/P_old
  if (rnumbers[4*j-3] < Pquotient){
    Pn = Pn_new
  }

  ## Update TB:
  TB_old = TB
  P_old = P_TB[P_TB$VTB == VTB, ][1,2+TB_old]*P_XR[P_XR$Pn == Pn & P_XR$TB == TB_old & P_XR$LC == LC, ][1,4+XR]
  TB_new = abs(TB-1)
  P_new = P_TB[P_TB$VTB == VTB, ][1,2+TB_new]*P_XR[P_XR$Pn == Pn & P_XR$TB == TB_new & P_XR$LC == LC, ][1,4+XR]
  Pquotient = P_new/P_old
  if (rnumbers[4*j-2] < Pquotient){
    TB = TB_new
  }

  ## Update LC:
  LC_old = LC
  P_old = P_LC[Sm+1,LC_old+2]*P_XR[P_XR$Pn == Pn & P_XR$TB == TB & P_XR$LC == LC_old, ][1,4+XR]*P_Dy[P_Dy$LC == LC_old & P_Dy$Br == Br, ][1,3+Dy]
  LC_new = abs(LC_old-1)
  P_new = P_LC[Sm+1,LC_new+2]*P_XR[P_XR$Pn == Pn & P_XR$TB == TB & P_XR$LC == LC_new, ][1,4+XR]*P_Dy[P_Dy$LC == LC_new & P_Dy$Br == Br, ][1,3+Dy]
  Pquotient = P_new/P_old
  if (rnumbers[4*j-1] < Pquotient){
    LC = LC_new
  }

  ## Update Br:
  P_old = P_Br[P_Br$Sm==Sm, ][1,2+Br]*P_Dy[P_Dy$LC==LC & P_Dy$Br==Br, ][1,3+Dy]
  Br_new = abs(Br-1)
  P_new = P_Br[P_Br$Sm==Sm, ][1,2+Br_new]*P_Dy[P_Dy$LC==LC & P_Dy$Br==Br_new, ][1,3+Dy]
  Pquotient = P_new/P_old
  if (rnumbers[4*j] < Pquotient){
    Br = Br_new
  }
  
  res = c(Pn, Te, VTB, TB, Sm, LC, Br, XR, Dy)
  results[j,] = res
  }
  burn = 9*floor(runs/10)
  add = 1 # Borde vi börja count på 1?
  Prob_Pn = (nrow(tail(results[results$Pn==1, ], burn)) + add)/(burn + 2*add) 
  Prob_Br = (nrow(tail(results[results$Br==1, ], burn)) + add)/(burn + 2*add)
  Prob_LC = (nrow(tail(results[results$LC==1, ], burn)) + add)/(burn + 2*add)
  Prob_TB = (nrow(tail(results[results$TB==1, ], burn)) + add)/(burn + 2*add)
  output[i,] = c(Prob_Pn, Prob_TB, Prob_LC, Prob_Br)
  }
  return(output)
}

bn = learn(hist)
diagnose(bn,cases)
runDiagnostics(learn, diagnose, 2)

