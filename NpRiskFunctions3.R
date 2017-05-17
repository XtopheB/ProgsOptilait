## Fonction de calcul ds l'AR , theta1, etc...
## 10/05/2015  : New version  : f(x, Z) , g(x)  x= factors,  z = weather factors...
##             :  Correction computing theta (denominator)
## 15/05/2015  : Adapted to parallel tratment
##             : Correction : computing g with abs(ei) on same regressors. 

## 03/06/2015  :Version 4 with computation of bandwidth also for g*
## 02/11/2015 : Updated set of regressors (U = region) 
## 05/11/2015 : Version with direct introduction of  regressors  
## 12/02/2016 : NpRiskAversion5 was computing G() qwith residuals, not residuals squared
## 12/02/2016 : New function NpRiskAversion6 wityh new computation of g based on squared residuals !
## 15/02/2016 : Correction of the theta2 coef (Milk price should be divided per 1000: implicitely done in theta1 (grain should also so it cancelels in theta1) )
## 17/02/2016 : Correction of the computation of sigma, estimated NP
## 02/05/2017 : Reprise programme depuis NpRiskFunctions2.R

#rm(list=ls())

library(np)
library(foreign)


# Loading the main functions

#############################
# computes  one year all the results (NEW computation on e2 for g !!(17/05/2017)
NpAversion8 <- function(year.prog, 
                        Xvar = x,
                        bwmethod = "CV",
                        bwcompute = "TRUE",
                        samplereduc = 1 ,
                        data = data.work,
                        ampli = 1,
                        Myroot = Myroot,
                        ...
) 
{
  options(np.messages=FALSE)
  # We take observations for the considered year 
  data.loc <- subset(data,  year == year.prog )
  
  N <- nrow(data.loc)
  print(N)
  
  # we need to restrict the dataframe to complete observations <---  NOT NECESSERARY ANYMORE
  #data.loc <- data.loc[complete.cases(data.loc),]
  
  ####       ############ For testing  : subset of the data 
  if(samplereduc < 1) {
    set.seed(0924)
    S <-  samplereduc * nrow(data.loc)      # Sample size of the training file
    ii <- sample(seq(1:nrow(data.loc)),replace=FALSE) #  random values of iobs. index
    data.loc <- data.loc[ii[1:S],]                # <<<<< ===== random sample from original file
    
  }    
  # #       #############
  attach(data.loc)
  print(paste(" --- Sample Size for", year.prog, " ---"))
  print(nrow(data.loc))
  print(" ---------------")
  # NEW : All the variable are in here 
  W <- as.data.frame(data.loc[,Xvar])           
  
  
  ##################   Estimation ###################  
  # New model without  "prixlaiteriemoyen" 
  ## Step 1 -  bandwidth for f 
  
  tic  <- proc.time()
  
  
  if(bwmethod == "CV"){
    if(bwcompute != "TRUE") {
      # 11/05/2015  we load the cross-validated bandwidths
      #load (paste("Results/CV/bw.f.",year.prog,".RData", sep=""))
      load(paste(Myroot,"Results/bw.f.",bwmethod,".",year.prog,"-",samplereduc,".RData", sep=""))
      
    }
    if(bwcompute == "TRUE") {
      bw.f <- npregbw(ydat = data.loc$laitproduit, 
                      xdat = W, 
                      regtype = "ll",
                      bwmethod = "cv.aic",
                      ckertype = "epanechnikov",
                      ukertype = "liracine",
                      okertype = "liracine",
                      #  Options to change below 
                      #                       nmulti = 5, 
                      #                       tol = 0.1,
                      #                       ftol = 0.1,
                      #                       itmax = 20, 
                      data = data.loc )
      
    }
  }
  
  if(bwmethod == "FIXED") {
    # A dï¿½terminer 
    bw.fixed.f <- c(3.766250e+06, 4.150987e+05, 2.359682e+06, 4.393298e+06,
                    1.204398e+06, 8.197868e+07, 1.394551e+09, 1.174233e+00,
                    6.407930e+04, 1.398660e+05)
    
    # Calcul de l'objet "bandwidth"
    
    bw.f <- npregbw(ydat = data.loc$laitproduit, 
                    xdat = W,                       
                    regtype = "ll",
                    bws = bw.fixed.f,
                    bandwidth.compute=FALSE,  
                    ckertype = "epanechnikov",
                    ukertype = "liracine",
                    okertype = "liracine",
                    data = data.loc )
    
  }
  
  if(bwmethod == "SILVERMAN") {
    bw.silver.f <-  apply(W, 2, silverman.bw)
    
    # A dï¿½terminer si on modifie la fenï¿½tre de Silverman ... (ici x 2) 
    bw.silver.f <- ampli*bw.silver.f  ### <<<<<<<------- amplificateur  #####
    
    # Calcul de l'objet "bandwidth"
    
    bw.f <- npregbw(ydat = data.loc$laitproduit, 
                    xdat = W,                       
                    regtype = "ll",
                    bws = bw.silver.f,
                    bandwidth.compute=FALSE,  
                    ckertype = "epanechnikov",
                    ukertype = "liracine",
                    okertype = "liracine",
                    data = data.loc )
    
  }
  
  # bandwidth information   
  print(" --- bandwith used for F ---")
  print(" ---------------")
  print(summary(bw.f))
  
  tac  <- proc.time()
  duree.f <- (tac-tic)/60
  
  print(" --- Duration for F ---")
  print(duree.f)
  print(" ---------------")
  
  
  save(bw.f, duree.f,  file = paste(Myroot,"Results/bw.f.",bwmethod,".",year.prog,"-",samplereduc,".RData", sep=""))
  
  # npplot(bw.f,  plot.errors.method="bootstrap")
  ##  Np estimation of f  (need to integrate to data.loc)  
  
  f.np <- npreg( bws = bw.f,
                 gradients = TRUE,
                 residuals = TRUE)
  
  
  ## Step 2 -- Computing g
  # Residuals Squared !! (12/02/16 - Discussion with M. Simioni)
  # Residuals in absolute value (16/05/2017 : discussion with stephane)
  
  e.2 <- (f.np$resid)^2
  
  
  # -- Computing bw for g  (same variable than for f )
  if(bwmethod == "CV") {
    if(bwcompute != "TRUE") {
      # 11/05/2015  we load the cross-validated bandwidths
      load (paste("Results/bw.g.",bwmethod,".",year.prog,"-",samplereduc,".RData", sep=""))
    }
    if(bwcompute == "TRUE") {
      
      bw.g <- npregbw(ydat = e.2, 
                      xdat = W,  
                      regtype = "ll",
                      bwmethod = "cv.aic",
                      ckertype = "epanechnikov",
                      ukertype = "liracine",
                      okertype = "liracine",
                      #                       nmulti = 5, 
                      #                       tol = 0.1,
                      #                       ftol = 0.1,
                      #                       itmax = 20, 
                      data = data.loc )
      
    }
  }
  
  if(bwmethod == "FIXED") {
    # A dï¿½terminer 
    bw.fixed.g <- c(1.522843e+07, 2.113754e+06 ,1.407951e+07, 5.513004e+06, 9.820912e+05 ,
                    1.740240e+08 , 2.020195e+08, 1.940852e-02 , 6.899923e+00, 7.041676e+00)
    
    # Calcul de l'objet "bandwidth"
    
    bw.g <- npregbw(ydat = e.1, 
                    xdat = W,    
                    regtype = "ll",
                    bw <- bw.fixed.g,
                    bandwidth.compute=FALSE,  
                    ckertype = "epanechnikov",
                    ukertype = "liracine",
                    okertype = "liracine",
                    data = data.loc )
    
  }
  
  if(bwmethod == "SILVERMAN") {
    
    bw.silver.g <-  apply(W, 2, silverman.bw)
    # A dï¿½terminer si on modifie la fenï¿½tre de Silverman .<<<---- ACHTUNG A MODIFIER ? #####
    bw.silver.g <- ampli*bw.silver.g 
    
    # Calcul de l'objet "bandwidth"
    
    bw.g <- npregbw(ydat = e.1, 
                    xdat = W,         
                    regtype = "ll",
                    bws = bw.silver.g,
                    bandwidth.compute=FALSE,  
                    ckertype = "epanechnikov",
                    ukertype = "liracine",
                    okertype = "liracine",
                    data = data.loc )
    
  }
  
  
  # bandwidth information   
  print(" --- bandwith used for G  ---")
  print(" ---------------")
  print(summary(bw.g))
  tac  <- proc.time()
  duree.g <- (tac-tic)/60
  save(bw.g, duree.g, file = paste(Myroot,"Results/bw.g.",bwmethod,".",year.prog,"-",samplereduc,".RData", sep=""))
  
  ##  Np estimation of g 
  g.np <- npreg( bws = bw.g,
                 data = data.loc,
                 gradients = TRUE,
                 residuals = TRUE )
  # summary(g.np)
  
  save(bw.f, bw.g, f.np, g.np,  file = paste(Myroot,"Results/np.",bwmethod,".",year.prog,"-",samplereduc,".RData", sep=""))
  
  ### New: computing real g
  e.0 <- f.np$resid
  
  bw.real.g <- npregbw(ydat = e.0, 
                  xdat = W,  
                  regtype = "ll",
                  bwmethod = "cv.aic",
                  ckertype = "epanechnikov",
                  ukertype = "liracine",
                  okertype = "liracine",
                  #                       nmulti = 5, 
                  #                       tol = 0.1,
                  #                       ftol = 0.1,
                  #                       itmax = 20, 
                  data = data.loc )
  
  ##  Np estimation of g 
  g.real <- npreg( bws = bw.real.g,
                 data = data.loc,
                 gradients = TRUE,
                 residuals = TRUE )
  
  #### computing  theta's  ###
  
  # step 4 --  derivative of f and  g 
  # first retrive the names of the variables
  nom <- f.np$xnames
  
  idx.grain <- match("concvlr", nom) # <<<- correspond to  concvlr
  idx.irri <- match("hasfpirri", nom) # <<<- correspond to  hasfirri
  
  # gradient for  f (grain and irri)
  f1.np <- f.np$grad[,  idx.grain]  
  f2.np <- f.np$grad[, idx.irri]  
  
  # since g.np estimates the ^2 of the function of interest --> h = g^2 ; h1.np = d/dx1(g.np^2) ,  h1.np = d/dx2(g.np^2)
  h1.np <- g.np$grad[,idx.grain]
  h2.np <- g.np$grad[, idx.irri]
  
  # Step 4 -- computing theta1 
  w1 <- prixconcvlr
  p <-  prixlaiteriemoyen
  
  #  12/02/2016: Def de Theta1  sans  signe NEGATIF 
  # gradient for  g (grain)  new computation  is required since regression was made on e.2, 
  # New computation of theta, using h computed on e2 residuals, an real.g computed on e (17/05/2017) 
 
  # NEW computing of denominator
  
  theta1 <-  - (f1.np - (w1/p)) / g1.np
  
  #       --- computing theta2
  # ordre de grandeur pour le coï¿½t marginal de l'irrigation : environ 15 euros par hectare 
  # c'est une info moyenne pour la campagne 2010 pour la rï¿½gion du sud-ouest.
  
  # 12/02/2016: Def de Theta2  sans  signe NEGATIF  
  # gradient for  g (irri)  new computation  is required s ince regression was made on e.2,
  # 05/05/2017 : Absolute value inside sqrt !!! 
  
  w2 <- 15    #  Le prix est en euros par litre doit ï¿½tre ramenï¿½ aux 1000 l comme pour grain ! (SC et CB  16/02/2016 )
  
  

  theta2 <-  -(f2.np - (w2/(p/1000)))/  g2.np
  
  # computing theta 
  theta <- (theta1  + theta2) /2
  
  # Computing PROFIT  and sigma  (CB 27/11/2015)
  # Profit exist already  !     
  
  # Computing sigma as in the paper (CB 17/02/2016)
  
  # Residuals squared  !!!
  
  Sigma2 <-  p^2 * (g.np$mean)      #  estimates of the regression function at the evaluation points
  SigmaProf <- sqrt(abs(Sigma2))     # 05/05/2017   absolute value...
  
  # Conforme au papier
  AR <- - theta/SigmaProf
  
  # Computing RP ( Checked with Stephane : 12/02/2016)
  
  RP <-  0.5 * AR * Sigma2 * mean(Profit) 
  RP.pc <- RP/Profit
  
  
  ####  RESULTS ############
  summary(theta)
  summary(AR)
  
  ###returned results ####
  
  #     risk.results <- list(sample.used =  data.loc, 
  #                        theta = theta, 
  #                        AR = AR )  
  #     
  risk.results <- as.data.frame(cbind(ident, 
                                      annee,
                                      # region,     Since already in data, would cause region.x pb
                                      f.np$mean, 
                                      g.np$mean, 
                                      f1.np,
                                      f2.np,
                                      theta1,
                                      theta2,
                                      theta,
                                      # Profit,    Since already in data, would cause Profit.x pb
                                      SigmaProf,
                                      AR,
                                      RP, 
                                      RP.pc)) 
  save( risk.results, bw.f, bw.g, f.np, g.np, duree.f, duree.g,  
        file = paste(Myroot,"Results/Risknp.",bwmethod,".",year.prog,"-",samplereduc,".RData", sep=""))
  
  return(risk.results)
  
}





#############################
# computes  one year all the results (with computation on |e| for g)
NpAversion7 <- function(year.prog, 
                        Xvar = x,
                        bwmethod = "CV",
                        bwcompute = "TRUE",
                        samplereduc = 1 ,
                        data = data.work,
                        ampli = 1,
                        Myroot = Myroot,
                        ...
) 
{
  options(np.messages=FALSE)
  # We take observations for the considered year 
  data.loc <- subset(data,  year == year.prog )
  
  N <- nrow(data.loc)
  print(N)
  
  
  # we need to restrict the dataframe to complete observations <---  NOT NECESSERARY ANYMORE
  
  #data.loc <- data.loc[complete.cases(data.loc),]
  
  ####       ############ For testing  : subset of the data 
  if(samplereduc < 1) {
    set.seed(0924)
    S <-  samplereduc * nrow(data.loc)      # Sample size of the training file
    ii <- sample(seq(1:nrow(data.loc)),replace=FALSE) #  random values of iobs. index
    data.loc <- data.loc[ii[1:S],]                # <<<<< ===== random sample from original file
    
  }    
  # #       #############
  attach(data.loc)
  print(paste(" --- Sample Size for", year.prog, " ---"))
  print(nrow(data.loc))
  print(" ---------------")
  # NEW : All the variable are in here 
  W <- as.data.frame(data.loc[,Xvar])           
  
  
  ##################   Estimation ###################  
  # New model without  "prixlaiteriemoyen" 
  ## Step 1 -  bandwidth for f 
  
  tic  <- proc.time()
  
  
  if(bwmethod == "CV"){
    if(bwcompute != "TRUE") {
      # 11/05/2015  we load the cross-validated bandwidths
      #load (paste("Results/CV/bw.f.",year.prog,".RData", sep=""))
      load(paste(Myroot,"Results/bw.f.",bwmethod,".",year.prog,"-",samplereduc,".RData", sep=""))
      
    }
    if(bwcompute == "TRUE") {
      bw.f <- npregbw(ydat = data.loc$laitproduit, 
                      xdat = W, 
                      regtype = "ll",
                      bwmethod = "cv.aic",
                      ckertype = "epanechnikov",
                      ukertype = "liracine",
                      okertype = "liracine",
                      #  Options to change below 
                      #                       nmulti = 5, 
                      #                       tol = 0.1,
                      #                       ftol = 0.1,
                      #                       itmax = 20, 
                      data = data.loc )
      
    }
  }
  
  if(bwmethod == "FIXED") {
    # A déterminer 
    bw.fixed.f <- c(3.766250e+06, 4.150987e+05, 2.359682e+06, 4.393298e+06,
                    1.204398e+06, 8.197868e+07, 1.394551e+09, 1.174233e+00,
                    6.407930e+04, 1.398660e+05)
    
    # Calcul de l'objet "bandwidth"
    
    bw.f <- npregbw(ydat = data.loc$laitproduit, 
                    xdat = W,                       
                    regtype = "ll",
                    bws = bw.fixed.f,
                    bandwidth.compute=FALSE,  
                    ckertype = "epanechnikov",
                    ukertype = "liracine",
                    okertype = "liracine",
                    data = data.loc )
    
  }
  
  if(bwmethod == "SILVERMAN") {
    bw.silver.f <-  apply(W, 2, silverman.bw)
    
    # A déterminer si on modifie la fenêtre de Silverman ... (ici x 2) 
    bw.silver.f <- ampli*bw.silver.f  ### <<<<<<<------- amplificateur  #####
    
    # Calcul de l'objet "bandwidth"
    
    bw.f <- npregbw(ydat = data.loc$laitproduit, 
                    xdat = W,                       
                    regtype = "ll",
                    bws = bw.silver.f,
                    bandwidth.compute=FALSE,  
                    ckertype = "epanechnikov",
                    ukertype = "liracine",
                    okertype = "liracine",
                    data = data.loc )
    
  }
  
  # bandwidth information   
  print(" --- bandwith used for F ---")
  print(" ---------------")
  print(summary(bw.f))
  
  tac  <- proc.time()
  duree.f <-tac-tic
  
  print(" --- Duration for F ---")
  print(duree.f)
  print(" ---------------")
  
  
  save(bw.f, duree.f,  file = paste(Myroot,"Results/bw.f.",bwmethod,".",year.prog,"-",samplereduc,".RData", sep=""))
  
  # npplot(bw.f,  plot.errors.method="bootstrap")
  ##  Np estimation of f  (need to integrate to data.loc)  
  
  f.np <- npreg( bws = bw.f,
                 gradients = TRUE,
                 residuals = TRUE)
  
  
  ## Step 2 -- Computing g
  # Residuals Squared !! (12/02/16 - Discussion with M. Simioni)
  # Residuals in absolute value (16/05/2017 : discussion with stephane)
  
  e.1 <-abs(f.np$resid)
  
  
  # -- Computing bw for g  (same variable than for f )
  if(bwmethod == "CV") {
    if(bwcompute != "TRUE") {
      # 11/05/2015  we load the cross-validated bandwidths
      load (paste("Results/bw.g.",bwmethod,".",year.prog,"-",samplereduc,".RData", sep=""))
    }
    if(bwcompute == "TRUE") {
      
      bw.g <- npregbw(ydat = e.1, 
                      xdat = W,  
                      regtype = "ll",
                      bwmethod = "cv.aic",
                      ckertype = "epanechnikov",
                      ukertype = "liracine",
                      okertype = "liracine",
                      #                       nmulti = 5, 
                      #                       tol = 0.1,
                      #                       ftol = 0.1,
                      #                       itmax = 20, 
                      data = data.loc )
      
    }
  }
  
  if(bwmethod == "FIXED") {
    # A déterminer 
    bw.fixed.g <- c(1.522843e+07, 2.113754e+06 ,1.407951e+07, 5.513004e+06, 9.820912e+05 ,
                    1.740240e+08 , 2.020195e+08, 1.940852e-02 , 6.899923e+00, 7.041676e+00)
    
    # Calcul de l'objet "bandwidth"
    
    bw.g <- npregbw(ydat = e.1, 
                    xdat = W,    
                    regtype = "ll",
                    bw <- bw.fixed.g,
                    bandwidth.compute=FALSE,  
                    ckertype = "epanechnikov",
                    ukertype = "liracine",
                    okertype = "liracine",
                    data = data.loc )
    
  }
  
  if(bwmethod == "SILVERMAN") {
    
    bw.silver.g <-  apply(W, 2, silverman.bw)
    # A déterminer si on modifie la fenêtre de Silverman .<<<---- ACHTUNG A MODIFIER ? #####
    bw.silver.g <- ampli*bw.silver.g 
    
    # Calcul de l'objet "bandwidth"
    
    bw.g <- npregbw(ydat = e.1, 
                    xdat = W,         
                    regtype = "ll",
                    bws = bw.silver.g,
                    bandwidth.compute=FALSE,  
                    ckertype = "epanechnikov",
                    ukertype = "liracine",
                    okertype = "liracine",
                    data = data.loc )
    
  }
  
  
  # bandwidth information   
  print(" --- bandwith used for G  ---")
  print(" ---------------")
  print(summary(bw.g))
  tac  <- proc.time()
  duree.g <-tac-tic
  save(bw.g, duree.g, file = paste(Myroot,"Results/bw.g.",bwmethod,".",year.prog,"-",samplereduc,".RData", sep=""))
  
  
  ##  Np estimation of g 
  g.np <- npreg( bws = bw.g,
                 data = data.loc,
                 gradients = TRUE,
                 residuals = TRUE )
  # summary(g.np)
  
  save(bw.f, bw.g, f.np, g.np,  file = paste(Myroot,"Results/np.",bwmethod,".",year.prog,"-",samplereduc,".RData", sep=""))
  
  
  #### computing  theta's  ###
  
  # step 4 --  derivative of f and  g 
  # first retrive the names of the variables
  nom <- f.np$xnames
  
  idx.grain <- match("concvlr", nom) # <<<- correspond to  concvlr
  idx.irri <- match("hasfpirri", nom) # <<<- correspond to  hasfirri
  
  # gradient for  f (grain and irri)
  f1.np <- f.np$grad[,  idx.grain]  
  f2.np <- f.np$grad[, idx.irri]  
  
  # Step 4 -- computing theta1 
  w1 <- prixconcvlr
  p <-  prixlaiteriemoyen
  
  #  12/02/2016: Def de Theta1  sans  signe NEGATIF 
  # gradient for  g (grain)  new computation  is required s ince regression was made on e.2, 
  # Residuals in absolute value (16/05/2017 : discussion with stephane)
  
  
   theta1 <-  - (f1.np - (w1/p)) /g.np$grad[,idx.grain]
  
  #       --- computing theta2
  # ordre de grandeur pour le coût marginal de l'irrigation : environ 15 euros par hectare 
  # c'est une info moyenne pour la campagne 2010 pour la région du sud-ouest.
  
  # 12/02/2016: Def de Theta2  sans  signe NEGATIF  
  # gradient for  g (irri)  new computation  is required s ince regression was made on e.2,
  # 05/05/2017 : Absolute value inside sqrt !!! 
  
  w2 <- 15    #  Le prix est en euros par litre doit être ramené aux 1000 l comme pour grain ! (SC et CB  16/02/2016 )
  
  theta2 <-  -(f2.np - (w2/(p/1000)))/ g.np$grad[, idx.irri]
  
  # computing theta 
  theta <- (theta1  + theta2) /2
  
  # Computing PROFIT  and sigma  (CB 27/11/2015)
  # Profit exist already  !     
  
  # Computing sigma as in the paper (CB 17/02/2016)
  
  # Residuals in absolute value (16/05/2017 : discussion with stephane)
  
  Sigma2 <-  p^2 * (g.np$mean)^2      #  estimates of the regression function at the evaluation points
  SigmaProf <- sqrt(abs(Sigma2))     # 05/05/2017   absolute value...
  # Conforme au papier
  AR <- - theta/SigmaProf
  
  # Computing RP ( Checked with Stephane : 12/02/2016)
  
  RP <-  0.5 * AR * Sigma2 * mean(Profit) 
  RP.pc <- RP/Profit
  
  
  ####  RESULTS ############
  summary(theta)
  summary(AR)
  
  ###returned results ####
  
  #     risk.results <- list(sample.used =  data.loc, 
  #                        theta = theta, 
  #                        AR = AR )  
  #     
  risk.results <- as.data.frame(cbind(ident, 
                                      annee,
                                      # region,     Since already in data, would cause region.x pb
                                      f.np$mean, 
                                      g.np$mean, 
                                      f1.np,
                                      f2.np,
                                      theta1,
                                      theta2,
                                      theta,
                                      # Profit,    Since already in data, would cause Profit.x pb
                                      SigmaProf,
                                      AR,
                                      RP, 
                                      RP.pc)) 
  save( risk.results, bw.f, bw.g, f.np, g.np, duree.f, duree.g,  
        file = paste(Myroot,"Results/Risknp.",bwmethod,".",year.prog,"-",samplereduc,".RData", sep=""))
  
  return(risk.results)
  
}






#################################
# computes for one year all the results (with computation on e2 for g)
NpAversion6 <- function(year.prog, 
                        Xvar = x,
                        bwmethod = "CV",
                        bwcompute = "TRUE",
                        samplereduc = 1 ,
                        data = data.work,
                        ampli = 1,
                        Myroot = Myroot,
                        ...
) 
{
  options(np.messages=FALSE)
  # We take observations for the considered year 
  data.loc <- subset(data,  year == year.prog )
  
  N <- nrow(data.loc)
  print(N)
 
  
  # we need to restrict the dataframe to complete observations <---  NOT NECESSERARY ANYMORE
  
  #data.loc <- data.loc[complete.cases(data.loc),]
  
  ####       ############ For testing  : subset of the data 
  if(samplereduc < 1) {
    set.seed(0924)
    S <-  samplereduc * nrow(data.loc)      # Sample size of the training file
    ii <- sample(seq(1:nrow(data.loc)),replace=FALSE) #  random values of iobs. index
    data.loc <- data.loc[ii[1:S],]                # <<<<< ===== random sample from original file
    
  }    
  # #       #############
  attach(data.loc)
  print(paste(" --- Sample Size for", year.prog, " ---"))
  print(nrow(data.loc))
  print(" ---------------")
  # NEW : All the variable are in here 
  W <- as.data.frame(data.loc[,Xvar])           
  
  
  ##################   Estimation ###################  
  # New model without  "prixlaiteriemoyen" 
  ## Step 1 -  bandwidth for f 
  
  tic  <- proc.time()
  
  
  if(bwmethod == "CV"){
    if(bwcompute != "TRUE") {
      # 11/05/2015  we load the cross-validated bandwidths
      #load (paste("Results/CV/bw.f.",year.prog,".RData", sep=""))
      load(paste(Myroot,"Results/bw.f.",bwmethod,".",year.prog,"-",samplereduc,".RData", sep=""))
      
    }
    if(bwcompute == "TRUE") {
      bw.f <- npregbw(ydat = data.loc$laitproduit, 
                      xdat = W, 
                      regtype = "ll",
                      bwmethod = "cv.aic",
                      ckertype = "epanechnikov",
                      ukertype = "liracine",
                      okertype = "liracine",
                      #  Options to change below 
                      #                       nmulti = 5, 
                      #                       tol = 0.1,
                      #                       ftol = 0.1,
                      #                       itmax = 20, 
                      data = data.loc )
      
    }
  }
  
  if(bwmethod == "FIXED") {
    # A déterminer 
    bw.fixed.f <- c(3.766250e+06, 4.150987e+05, 2.359682e+06, 4.393298e+06,
                    1.204398e+06, 8.197868e+07, 1.394551e+09, 1.174233e+00,
                    6.407930e+04, 1.398660e+05)
    
    # Calcul de l'objet "bandwidth"
    
    bw.f <- npregbw(ydat = data.loc$laitproduit, 
                    xdat = W,                       
                    regtype = "ll",
                    bws = bw.fixed.f,
                    bandwidth.compute=FALSE,  
                    ckertype = "epanechnikov",
                    ukertype = "liracine",
                    okertype = "liracine",
                    data = data.loc )
    
  }
  
  if(bwmethod == "SILVERMAN") {
    bw.silver.f <-  apply(W, 2, silverman.bw)
    
    # A déterminer si on modifie la fenêtre de Silverman ... (ici x 2) 
    bw.silver.f <- ampli*bw.silver.f  ### <<<<<<<------- amplificateur  #####
    
    # Calcul de l'objet "bandwidth"
    
    bw.f <- npregbw(ydat = data.loc$laitproduit, 
                    xdat = W,                       
                    regtype = "ll",
                    bws = bw.silver.f,
                    bandwidth.compute=FALSE,  
                    ckertype = "epanechnikov",
                    ukertype = "liracine",
                    okertype = "liracine",
                    data = data.loc )
    
  }
  
  # bandwidth information   
  print(" --- bandwith used for F ---")
  print(" ---------------")
  print(summary(bw.f))
  
  tac  <- proc.time()
  duree.f <-tac-tic
  
  print(" --- Duration for F ---")
  print(duree.f)
  print(" ---------------")
  
  
  save(bw.f, duree.f,  file = paste(Myroot,"Results/bw.f.",bwmethod,".",year.prog,"-",samplereduc,".RData", sep=""))
  
  # npplot(bw.f,  plot.errors.method="bootstrap")
  ##  Np estimation of f  (need to integrate to data.loc)  
  
  f.np <- npreg( bws = bw.f,
                 gradients = TRUE,
                 residuals = TRUE)
  
  
  ## Step 2 -- Computing g
  # Residuals Squared !! (12/02/16 - Discussion with M. Simioni)
  
  e.2 <-(f.np$resid)^2
  
  
  # -- Computing bw for g  (same variable than for f )
  if(bwmethod == "CV") {
    if(bwcompute != "TRUE") {
      # 11/05/2015  we load the cross-validated bandwidths
      load (paste("Results/bw.g.",bwmethod,".",year.prog,"-",samplereduc,".RData", sep=""))
    }
    if(bwcompute == "TRUE") {
      
      bw.g <- npregbw(ydat = e.2, 
                      xdat = W,  
                      regtype = "ll",
                      bwmethod = "cv.aic",
                      ckertype = "epanechnikov",
                      ukertype = "liracine",
                      okertype = "liracine",
                      #                       nmulti = 5, 
                      #                       tol = 0.1,
                      #                       ftol = 0.1,
                      #                       itmax = 20, 
                      data = data.loc )
      
    }
  }
  
  if(bwmethod == "FIXED") {
    # A déterminer 
    bw.fixed.g <- c(1.522843e+07, 2.113754e+06 ,1.407951e+07, 5.513004e+06, 9.820912e+05 ,
                    1.740240e+08 , 2.020195e+08, 1.940852e-02 , 6.899923e+00, 7.041676e+00)
    
    # Calcul de l'objet "bandwidth"
    
    bw.g <- npregbw(ydat = e.2, 
                    xdat = W,    
                    regtype = "ll",
                    bw <- bw.fixed.g,
                    bandwidth.compute=FALSE,  
                    ckertype = "epanechnikov",
                    ukertype = "liracine",
                    okertype = "liracine",
                    data = data.loc )
    
  }
  
  if(bwmethod == "SILVERMAN") {
    
    bw.silver.g <-  apply(W, 2, silverman.bw)
    # A déterminer si on modifie la fenêtre de Silverman .<<<---- ACHTUNG A MODIFIER ? #####
    bw.silver.g <- ampli*bw.silver.g 
    
    # Calcul de l'objet "bandwidth"
    
    bw.g <- npregbw(ydat = e.2, 
                    xdat = W,         
                    regtype = "ll",
                    bws = bw.silver.g,
                    bandwidth.compute=FALSE,  
                    ckertype = "epanechnikov",
                    ukertype = "liracine",
                    okertype = "liracine",
                    data = data.loc )
    
  }
  
  
  # bandwidth information   
  print(" --- bandwith used for G  ---")
  print(" ---------------")
  print(summary(bw.g))
  tac  <- proc.time()
  duree.g <-tac-tic
  save(bw.g, duree.g, file = paste(Myroot,"Results/bw.g.",bwmethod,".",year.prog,"-",samplereduc,".RData", sep=""))
  
  
  ##  Np estimation of g 
  g.np <- npreg( bws = bw.g,
                 data = data.loc,
                 gradients = TRUE,
                 residuals = TRUE )
  # summary(g.np)
  
  save(bw.f, bw.g, f.np, g.np,  file = paste(Myroot,"Results/np.",bwmethod,".",year.prog,"-",samplereduc,".RData", sep=""))
  
  
  #### computing  theta's  ###
  
  # step 4 --  derivative of f and  g 
  # first retrive the names of the variables
  nom <- f.np$xnames
  
  idx.grain <- match("concvlr", nom) # <<<- correspond to  concvlr
  idx.irri <- match("hasfpirri", nom) # <<<- correspond to  hasfirri
  
  # gradient for  f (grain and irri)
  f1.np <- f.np$grad[,  idx.grain]  
  f2.np <- f.np$grad[, idx.irri]  
  
  # Step 4 -- computing theta1 
  w1 <- prixconcvlr
  p <-  prixlaiteriemoyen
  
  #  12/02/2016: Def de Theta1  sans  signe NEGATIF 
  # gradient for  g (grain)  new computation  is required s ince regression was made on e.2, 
  
  theta1 <- (f1.np - (w1/p)) *  2 * sqrt(abs(g.np$grad[,idx.grain]))
  
  #       --- computing theta2
  # ordre de grandeur pour le coût marginal de l'irrigation : environ 15 euros par hectare 
  # c'est une info moyenne pour la campagne 2010 pour la région du sud-ouest.
  
  # 12/02/2016: Def de Theta2  sans  signe NEGATIF  
  # gradient for  g (irri)  new computation  is required s ince regression was made on e.2,
  # 05/05/2017 : Absolute value inside sqrt !!! 
  
  w2 <- 15    #  Le prix est en euros par litre doit être ramené aux 1000 l comme pour grain ! (SC et CB  16/02/2016 )
  theta2 <-  (f2.np - (w2/(p/1000))) * 2 * sqrt(abs(g.np$grad[, idx.irri]))
  
  # computing theta 
  theta <- (theta1  + theta2) /2
  
  # Computing PROFIT  and sigma  (CB 27/11/2015)
  # Profit exist already  !     
  
  # Computing sigma as in the paper (CB 17/02/2016)
  
  Sigma2 <-  p^2 * g.np$mean  #  estimates of the regression function at the evaluation points
  SigmaProf <- sqrt(abs(Sigma2))     # 05/05/2017   absolute value...
  # Conforme au papier
  AR <- - theta/SigmaProf
  
  # Computing RP ( Checked with Stephane : 12/02/2016)
  
  RP <-  0.5 * AR * Sigma2 * mean(Profit) 
  RP.pc <- RP/Profit
  
  
  ####  RESULTS ############
  summary(theta)
  summary(AR)
  
  ###returned results ####
  
  #     risk.results <- list(sample.used =  data.loc, 
  #                        theta = theta, 
  #                        AR = AR )  
  #     
  risk.results <- as.data.frame(cbind(ident, 
                                      annee,
                                      # region,     Since already in data, would cause region.x pb
                                      f.np$mean, 
                                      g.np$mean, 
                                      f1.np,
                                      f2.np,
                                      theta1,
                                      theta2,
                                      theta,
                                      # Profit,    Since already in data, would cause Profit.x pb
                                      SigmaProf,
                                      AR,
                                      RP, 
                                      RP.pc)) 
  save( risk.results, bw.f, bw.g, f.np, g.np, duree.f, duree.g,  
        file = paste(Myroot,"Results/Risknp.",bwmethod,".",year.prog,"-",samplereduc,".RData", sep=""))
  
  return(risk.results)
  
}




