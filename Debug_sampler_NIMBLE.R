# Code to check sampler failures in some runs
# note: needs debug_workspace to run
library(nimble);library(extraDistr)

kk <- 2 # which iteration (3, 12 failed)
ww <- 2 # corresponds to strategy 7
y <- 5

# Check weird estimates
# all.N.est[,,kk,ww] 
# all.K.est[,,,kk,ww]

# Prepare data for learning sites at kk,ww
learning.sites <- which(colSums(all.N[1:y,,kk,ww]>0)>0)[-1]
N <- all.N[1:(y+1),learning.sites,kk,ww]
FR <- all.FR[1:(y+1),learning.sites,kk,ww]+1
Harvest <- all.Harvest[1:(y+1),learning.sites,kk,ww]
N.est <- all.N.est[1:(y+1),learning.sites,kk,ww]
n.counts <- all.n.counts[1:(y+1),learning.sites,kk,ww]
K.est <- all.K.est[1:y,,,kk,ww]

# Constants for NIMBLE model
hihiConst <- list(
  # Data
  n.sites=length(learning.sites),n.years=y+1, Feeding=FR,
  # Priors
  a.p.mu=a.p.mu[learning.sites],a.p.tau=a.p.tau[learning.sites],
  sd.p.mu=sd.p.mu[learning.sites],sd.p.tau=sd.p.tau[learning.sites],
  r.mu=rmax.mu.prior,r.tau=rmax.tau.prior,
  min.sd.r.process=min(rmax.sd.process),max.sd.r.process=max(rmax.sd.process),
  K.mn.prior=K.mn.prior[,learning.sites],K.mn.tau.prior=K.mn.tau.prior[,learning.sites],
  p.rel.alpha=cor.beta$alpha[learning.sites],p.rel.beta=cor.beta$beta[learning.sites])

# Data for NIMBLE model
hihiData <- list(n.counts=n.counts, # Observed counts at occupied sites
                 N.init=round(N.est[1,]/plogis(a.p.mu[learning.sites])), # Estimated initial population sizes
                 Harvest=Harvest,
                 releases=ifelse(Harvest>0,Harvest,0)) 
# Initialize nodes
hihiInits <- function(){list(
  N=sweep(round(n.counts/plogis(a.p.mu[learning.sites])), 2, as.numeric(N.est[1,]>0)*10, "+"),
  N.real=sweep(round(n.counts/plogis(a.p.mu[learning.sites])), 2, as.numeric(N.est[1,]>0)*10, "+") + Harvest,
  received=matrix(0,nrow=y+1,ncol=length(learning.sites)),#round(hihiData$releases*cor.mu[learning.sites]),
  mu.r=rmax.mu.prior,sd.r.process=runif(1,rmax.sd.process[1],rmax.sd.process[2]),
  r.max=matrix(rmax.mu.prior,nrow=y+1,ncol=length(learning.sites),byrow=T),
  post.rel=cor.mu[learning.sites],
  a.p=a.p.mu[learning.sites], re.p.sd=sd.p.mu[learning.sites], re.p=matrix(0,nrow=y+1,ncol=length(learning.sites)),
  p=matrix(plogis(a.p.mu[learning.sites]),nrow=y+1,ncol=length(learning.sites),byrow=T),
  K.mn=t(all.K.est[y,learning.sites,,kk,ww]) #K.mn.prior[,learning.sites]
)} 

# warning: problem initializing stochastic node N[7, 2]: logProb is -Inf.
# warning: problem initializing stochastic node N[9, 6]: logProb is -Inf.
# warning: problem initializing stochastic node N[10, 6]: logProb is -Inf.

# Set monitors
params=c("N","K.mn","mu.r")
# Nimble options (make as silent as possible)
nimbleOptions(verbose=FALSE, verboseErrors=FALSE,MCMCprogressBar=FALSE,
              MCMCuseConjugacy=FALSE,showCompilerOutput = FALSE,maxContractionsWarning=FALSE)
# Build model - NB model code sourced from All_strategies_source script
hihiModel <- nimbleModel(code = hihiCode, data = hihiData, constants = hihiConst, inits=hihiInits(), check=FALSE)
# hihiMCMC <- buildMCMC(hihiModel)
# hihiMCMC$run(1)
# hihiModel$N

# Compile model
hihiCompiled <- compileNimble(hihiModel)
# Sample model
hihiSamples <- nimbleMCMC(model=hihiCompiled, monitors=params,inits=hihiInits(),
                          summary=TRUE, niter = ni, nburnin = nb, thin = nt, nchains=nc)
options(scipen=999)
# round(hihiSamples$summary$all.chains,2)

temp.df <- data.frame(hihiSamples$summary$all.chains) 
temp.df %>% filter(Mean>10000|Mean < -10000)
matrix(unlist(floor(temp.df %>% select(Mean) %>% filter(grepl("K.mn", rownames(temp.df))))),
       nrow=2,ncol=length(learning.sites))
matrix(unlist(floor(temp.df %>% select(Mean) %>% filter(grepl("N", rownames(temp.df))))),
       nrow=y+1,ncol=length(learning.sites))

# 
# round(hihiSamples$summary$chain1,2)[,1:2]
# round(hihiSamples$summary$chain2,2)
# round(hihiSamples$summary$chain3,2)

# # Compare with and without extra step around received
# # nostep.2.df <- data.frame(hihiSamples$summary$all.chains)
# step.2.df <- data.frame(hihiSamples$summary$all.chains)
# 
# Harvest # Check actual harvest
# matrix(unlist(floor(nostep.2.df %>% select(Mean) %>% filter(grepl("received", rownames(nostep.2.df))))),
#        nrow=y,ncol=length(learning.sites))
# matrix(unlist(floor(step.2.df %>% select(Mean) %>% filter(grepl("received", rownames(step.2.df))))),
#        nrow=y,ncol=length(learning.sites))
# 
# 
# matrix(unlist(floor(nostep.2.df %>% select(Mean) %>% filter(grepl("K.mn", rownames(nostep.2.df))))),
#        nrow=2,ncol=length(learning.sites))
# matrix(unlist(floor(step.2.df %>% select(Mean) %>% filter(grepl("K.mn", rownames(step.2.df))))),
#        nrow=2,ncol=length(learning.sites))
# 
# round(t(nostep.2.df %>% select(Mean) %>% filter(grepl("post.rel", rownames(nostep.2.df)))),2)
# round(t(step.2.df %>% select(Mean) %>% filter(grepl("post.rel", rownames(step.2.df)))),2)
