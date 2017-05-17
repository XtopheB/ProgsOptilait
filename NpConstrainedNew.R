## Code to replicate the examples in Chapter 12 - Constrained Regression
## See the relevant source code in the file "chapter12.code.R"
## From http://www.the-smooth-operators.com/code
##  16/06/2015 :  Adapted by Chris 
##  15/05/2017: Updated version

rm(list=ls())

library(foreign)
## Load in necessary libraries, np and quadprog packages
## at the moment
library(np)
library(quadprog)
library(minqa)
library(dplyr)
options(np.tree=TRUE,np.messages=FALSE)
	
# --- My Files , path, etc.. here
rm(list=ls())
Myroot <- "D:/progs/Optilait/"
#Myroot  <- "C:/Chris/progs/Optilait/"   
setwd(Myroot)
## DATA 
load(paste(Myroot,"Progs/Shiny/dataobserver/data.work.RData", sep=""))

year.chosed <- 2002

data.year <- subset(data.work, year == year.chosed)


#####       ############ For testing  : subset of the data <<<<<<<<<<<<<<<<<<< to remove !!!!!
set.seed(1234)
S <-  100   # Sample size of the training file
ii <- sample(seq(1:nrow(data.year)),replace=FALSE) #  random values of iobs. index
data.year <- data.year[ii[1:S],]                # <<<<< ===== random sample from original file

#### ------------------------------------------

data.year <- data.year %>%ungroup()
data.year<-  dplyr::select(data.year, laitproduit, sau, eqvltotal, hasfpirri, 
                           concvlr,  Tyear, ETPyear, DELTAyear)


data.year <- data.year[complete.cases(data.year),]


## Load in lib_lp.R source file for constrained estimation
source(paste(Myroot,"progs/SourcecodeParmeter/cons_lib.R", sep=""))
# kernel need for chapter5
kernel <- "gaussian"    ### <<<---- change that   
source(paste(Myroot,"progs/SourcecodeParmeter/chapter5.code.R", sep=""))





	
## Load the local linear bandwidth object already computed !
#load(paste(Myroot, "Results/bw.f.CV.",year.chosed,"-1.RData", sep = ""))

### Or compute it here: 
bw.f <- npregbw(laitproduit ~ sau + eqvltotal + hasfpirri +concvlr
                      + Tyear + ETPyear + DELTAyear,
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
                      data = data.year )







## Compute the unrestricted model and gradients
model.unres <- npreg(bws = bw.f,data = data.year,gradients = TRUE)
	
model.gradient.unres <- gradients(model.unres)   # First derivatives 

## Start from uniform weights equal to 1/n. If constraints are
## non-binding these are optimal.
n <- length(data.year$laitproduit)
p <- rep(1/n,n)
Dmat <- diag(1,n,n)
dvec <- as.vector(p)

## Achtung : data.year must have only the variables of the formula in the regression !


#### PB de dim là !!!!! 

Aymat.unres.mean <- Aymat.lp(j.reg = 0,
                             mydata.train = data.year,
                             mydata.eval = data.year,
                             bws = bw.f,
                             p = 1, warning.immediate = TRUE)

# First derivative relative to X1
	Aymat.unres.fp1 <- Aymat.lp(j.reg = 1,mydata.train = data.year,
							mydata.eval = data.year,
							bws = bw.f, p = 1, warning.immediate = FALSE)
	
	# First derivative relative to X2
		Aymat.unres.fp2 <- Aymat.lp(j.reg = 2,mydata.train = data.year,
  							mydata.eval = data.year,
							bws = bw.f,p = 1,warning.immediate = FALSE)

	## Don't forget to divide by n as Aymat is scaled
	## Conditional mean  
	model.fit.unres <- rowSums(Aymat.unres.mean)/n
	# First partials
	model.fp.unres <- cbind(rowSums(Aymat.unres.fp1)/n,
						rowSums(Aymat.unres.fp2)/n)

	## As a check model.fp.unres should be identical
	## to model.gradient.unres
	all.equal(model.fp.unres,model.gradient.unres)

	## Now setup constraint matrix and vector
	## Here is Amat for 
	## First Derivative Restrictions  
	Amat <- t(rbind(rep(1,n),
                 Aymat.unres.fp1,
                 Aymat.unres.fp2))

	## Determine how many constraints are violated initially
	length(which(t(Amat)%*%p<0))
	
  ######  Constrains !!!!!!!!
	## Here is bvec
	## For first derivatives
	lower=-0.0005
	bvec <- c(1,rep(lower,n),rep(lower,n))

	## Solve the quadratic programming problem
  
	QP.output <- solve.QP(Dmat=Dmat,dvec=dvec,
                      Amat=Amat,bvec=bvec,meq=1)

	## Get the solution and update the uniform weights
  
	w.hat <- QP.output$solution

	## Check to make sure all constraints are satisfied
	check <- t(Amat)%*%w.hat-bvec
	all(check>=-0.0000000001)

	
	### From Racine Parmeter and Du (2009), we recompute the resticted model with np 
	p.updated <- p + QP.output$solution
	## Now estimate the restricted model using the np package and you are done.
	data.trans <- data.frame(y=p.updated*n*data.year$laitproduit,data.year[,2:ncol(data.year)])
	
	model.res <- npreg(bws=bw,data=data.trans,gradients=TRUE)
	
	
	
	plot(model.res)
	plot(model.unres)
	
	
	
	
	
	model.fp.res <- t(Amat)%*%w.hat

	## Note first restriction is sum to one so we can 
	## drop that
	model.fp.res <- model.fp.res[-1]
	model.fp.res1 <- matrix(model.fp.res,n,2)

	## Table 12.1 
	model.gradient.unres1 <- model.gradient.unres[,1]
	model.gradient.unres2 <- model.gradient.unres[,2]
	model.fp.res11 <- model.fp.res1[,1]
	model.fp.res12 <- model.fp.res1[,2]

	m <- matrix(ncol=4, nrow=5)
	colnames(m) <- c("UN(K)","RES(K)","UN(HL)","RES(HL)")
	rownames(m) <- c("D_10","Q_25","Q_50","Q_75","D_90")

	m[1,] <- c(quantile(model.gradient.unres1,0.10),quantile(model.fp.res11,0.10),
			quantile(model.gradient.unres2,0.10),quantile(model.fp.res12,0.10))
	m[2,] <- c(quantile(model.gradient.unres1,0.25),quantile(model.fp.res11,0.25),
			quantile(model.gradient.unres2,0.25),quantile(model.fp.res12,0.25))
	m[3,] <- c(quantile(model.gradient.unres1,0.5),quantile(model.fp.res11,0.5),
			quantile(model.gradient.unres2,0.5),quantile(model.fp.res12,0.5))
	m[4,] <- c(quantile(model.gradient.unres1,0.75),quantile(model.fp.res11,0.75),
			quantile(model.gradient.unres2,0.75),quantile(model.fp.res12,0.75))
	m[5,] <- c(quantile(model.gradient.unres1,0.90),quantile(model.fp.res11,0.90),
			quantile(model.gradient.unres2,0.90),quantile(model.fp.res12,0.90))

	print.default(m)

	## Figure 12.4
	plot(model.fp.unres[,1],model.fp.unres[,1],cex=0.9,pch=1,
		 xlab="Unconstrained Gradient Estimate",
		 ylab="Gradient Estimate")
	points(model.fp.unres[,1],model.fp.res1[,1],cex=0.9,pch=2)
	abline(h=0,v=0,lwd=1.5)
	legend(0.05,0.94,legend=c("Unconstrained","Constrained"),pch=c(1,2))
	dev.off()

	# ## Now for lh
	plot(model.fp.unres[,2],model.fp.unres[,2],cex=0.9,pch=1,
		 xlab="Unconstrained Gradient Estimate",
		 ylab="Gradient Estimate")
	points(model.fp.unres[,2],model.fp.res1[,2],cex=0.9,pch=2)
	abline(h=0,v=0,lwd=1.5)
	legend(0.05,3.7,legend=c("Unconstrained","Constrained"),pch=c(1,2))
	dev.off()

	par(mfrow=c(1,2))

	plot(model.fp.unres[,1],model.fp.unres[,1],cex=0.9,pch=1,
		xlab="Gradient Estimate",
		ylab="Gradient Estimate",
		sub="Physical Capital")
	points(model.fp.unres[,1],model.fp.res1[,1],cex=0.9,pch=2)
	abline(h=0,v=0,lwd=1.5)
	legend(0.05,-0.1,legend=c("Unconstrained","Constrained"),pch=c(1,2))

	plot(model.fp.unres[,2],model.fp.unres[,2],cex=0.9,pch=1,
		xlab="Gradient Estimate",
		ylab="Gradient Estimate",
		sub="Human Capital Augmented Labor")
	points(model.fp.unres[,2],model.fp.res1[,2],cex=0.9,pch=2)
	abline(h=0,v=0,lwd=1.5)
	legend(0.05,-0.8,legend=c("Unconstrained","Constrained"),pch=c(1,2))
	dev.new()

	
	
	
	
	
	
	
	## Here is Amat for 
	## Constant Returns to Scale Restriction 
	Aymat.unres.CRS <- Aymat.unres.fp1+Aymat.unres.fp2

	Amat <- t(rbind(rep(1,n),
                 Aymat.unres.CRS,
                 -Aymat.unres.CRS))

	## Determine how many constraints are violated initially
	length(which(t(Amat)%*%p!=1))

	## Here is bvec
	## For first derivatives
	lower <- 0.9995
	upper <- 1.0005
	bvec <- c(1,rep(lower,n),rep(-upper,n))

	## Solve the quadratic programming problem
  
	QP.output <- solve.QP(Dmat=Dmat,dvec=dvec,
                      Amat=Amat,bvec=bvec,meq=1)
  
	## Get the solution and update the uniform weights
  
	w.hat <- QP.output$solution

	## Check to make sure all constraints are satisfied
	check <- t(Amat)%*%w.hat-bvec
	all(check>=-0.0000000001)

	CRS.unres <- rowSums(Aymat.unres.CRS)/n
	CRS.res   <- t(Amat)%*%w.hat
	CRS.res   <- CRS.res[-1]
	CRS.res1 <- matrix(CRS.res,n,2)[,1]

	## Figure 12.5
	## Now plot out CRS
	plot(CRS.unres,cex=0.9,pch=1,
		xlab="Observation",
		ylab="RTS Estimate")
	abline(h=1,lwd=2,lty=2)
	dev.new()

	## Here is Amat for 
	## Decreasing Returns to Scale Restriction 
	Aymat.unres.RTS <- Aymat.unres.fp1+Aymat.unres.fp2

	Amat <- t(rbind(rep(1,n),
                 Aymat.unres.RTS,
                 -Aymat.unres.RTS))

	## Determine how many constraints are violated initially
	val <- -t(Amat)%*%p
	length(which(val>=1))

	## Here is bvec
	## For first derivatives
	upper <- 1
	lower <- 0
	bvec <- c(1,rep(lower,n),rep(-upper,n))

	## Solve the quadratic programming problem
  
	QP.output <- solve.QP(Dmat=Dmat,dvec=dvec,
                      Amat=Amat,bvec=bvec,meq=1)
  
	## Get the solution and update the uniform weights
  
	w.hat <- QP.output$solution

	## Check to make sure all constraints are satisfied
	check <- t(Amat)%*%w.hat-bvec
	all(check>=-0.0000000001)

	RTS.unres <- rowSums(Aymat.unres.CRS)/n
	RTS.res   <- t(Amat)%*%w.hat
	RTS.res   <- RTS.res[-1]
	RTS.res1 <- matrix(RTS.res,n,2)[,1]

	## Here is Amat for 
	## Constant Returns to Scale Restriction and Monotonicity
	Aymat.unres.RTS <- Aymat.unres.fp1+Aymat.unres.fp2

	Amat <- t(rbind(rep(1,n),
                 Aymat.unres.RTS,
                 -Aymat.unres.RTS,
                 Aymat.unres.fp1,
                 Aymat.unres.fp2))

	## Determine how many constraints are violated initially
	val <- -t(Amat)%*%p
	length(which(val>=1))

	## Here is bvec
	## For first derivatives
	upper <- 1.000005
	lower <- 0.999995
	lower1 <- -0.0005
	bvec <- c(1,rep(lower,n),rep(-upper,n),
			rep(lower1,n),rep(lower1,n))

	## Solve the quadratic programming problem 
	QP.output <- solve.QP(Dmat=Dmat,dvec=dvec,
                      Amat=Amat,bvec=bvec,meq=1)
  
	## Get the solution and update the uniform weights 
	w.hat <- QP.output$solution

	## Check to make sure all constraints are satisfied
	check <- t(Amat)%*%w.hat-bvec
	all(check>=-0.0000000001)

	## Plot CRS

	RTS.unres <- rowSums(Aymat.unres.RTS)/n
	RTS.res   <- t(Amat)%*%w.hat
	RTS.res   <- RTS.res[-1]
	RTS.res   <- matrix(RTS.res,n,4)
	RTS.res1  <- RTS.res[,1]

	## Figure 12.6
	par(mfrow=c(1,2))
	plot(model.fp.unres[,1],model.fp.unres[,1],cex=0.9,pch=1,
		xlab="Gradient Estimate",
		ylab="Gradient Estimate",
		sub="Phyical Capital")
	points(model.fp.unres[,1],RTS.res[,3],cex=0.9,pch=2)
	abline(h=0,v=0,lwd=1.5)
	legend(0.05,-0.05,legend=c("Unconstrained","Constrained"),pch=c(1,2))

	plot(model.fp.unres[,2],model.fp.unres[,2],cex=0.9,pch=1,
		xlab="Gradient Estimate",
		ylab="Gradient Estimate",
		sub="Human Capital Augmented Labor")
	points(model.fp.unres[,2],RTS.res[,4],cex=0.9,pch=2)
	abline(h=0,v=0,lwd=1.5)
	legend(0.15,-0.7,legend=c("Unconstrained","Constrained"),pch=c(1,2))