
# Strategy 1: status quo ------------------------------
# CHECKED
# Harvest: 1) Harvest Tiri for Nj=40 (max.Hv juv females) if its total pop size is >=60
# Release: 1) Unoccupied site with highest K(fed). Commit to >=2 years of releases until failure or success
#          2) If there are no unoccupied sites, stop harvest
# Feeding: 1) ad libitum everywhere (any occupied site except Hauturu)

strategy.1.rules <<- function(yr=y,ns=n.sites,sn=site.names,st=site.type,sa=site.ages,sx=site.access,fl=feed.levels,
                              N.est=N.est,Hv=Harvest,FR.1=FR,fk=failed,Gx=Genetics,
                              Ke=K.est,Km=K.managed,Kt=K.true,
                              max.Hv=max.h,fk.cut=fail.cut,ku.cut.1=ku.cut,kf.cut.1=kf.cut,feed.cut.1=feed.cut,
                              minN.Hv=minN.h,N.top.1=N.top,drop.p.1=drop.p,drop.N.1=drop.N
){
  
  
  hh <- rr <- gg <- ff <- rep(0,n.sites) 
  
  # Sources
  hh[sn=="Tiri"&N.est[yr+1,]>=60] <- 1 # Tiri only - note the cutoff is different from other strategies
  
  # FIRST PRIORITY: sites with ongoing releases
  if(yr>2){
    cond.1 <- which(sn!="Hauturu"&fk[yr,]==0&colSums(Hv[max(1,yr-1):yr,]>0)<2&N.est[max(1,yr-1),]==0&N.est[yr+1,]>=fk.cut&N.est[yr+1,]+Hv[yr+1,]<min(kf.cut.1,Ke[yr+1,,fl]))
    # If there are site failures, add to to list
    fk[yr+1,which((sn!="Hauturu"&colSums(Hv[max(1,yr-1):yr,]>0)<2&N.est[max(1,yr-1),]==0&N.est[yr+1,]<fk.cut&Ke[yr+1,,1]<fk.cut)|fk[yr,]==1)] <- 1
  }else{cond.1 <- 0}
  # SECOND PRIORITY: Unoccupied sites
  cond.2 <- which(N.est[yr+1,]+Hv[yr+1,]<1&fk[yr+1,]==0)

  # PRIORITY: rank both by K.fed - Note: only one release possible in this strategy
  rr[which(Ke[yr+1,,fl]==sort(Ke[yr+1,cond.1,fl],d=T)[1])] <- 1
  rr[which(Ke[yr+1,,fl]==sort(Ke[yr+1,cond.2,fl],d=T)[1])] <- ifelse(sum(rr)==0,1,0)

  # If number of sources exceeds number of available destinations, discard sources based on population sizes
  if(sum(hh)>sum(rr>0)){hh[!is.na(match(sn,names(sort(N.est[yr+1,hh==1])[1:(sum(hh)-sum(rr>0))])))] <- 0}
  
  # Allocate actual birds
  Hv[yr+2,] <- 0
  if(sum(hh)==0|sum(rr)==0) {Hv[yr+2,] <- 0} else{   # If there are no suitable sources or destinations, no harvest
    Hv[yr+2,hh==1] <- -max.Hv # Otherwise Harvest max.Hv juveniles from each source site
    Hv[yr+2,rr==1] <- max.Hv # And release at the chosen site
  } # else 
  
  # If one or more of the release sites are on remote islands:
  if(sum(sx[Hv[yr+2,]>0]=="Remote")>0){
    Hv[yr+2,sn=="Hauturu"] <- sum(-Hv[yr+2,Hv[yr+2,]>0&sx=="Remote"])/2 # Harvest half of those birds from Hauturu
    # Which sites do we take less birds from? The smallest ones of the harvest candidates (matching number of remote releases)
    put.back <- which(N.est[yr+1,]==sort(N.est[yr+1,Hv[yr+2,]<0&sn!="Hauturu"],d=F)[1:length(sn[Hv[yr+2,]>0&sx=="Remote"])])
    Hv[yr+2,put.back] <- Hv[yr+2,put.back]/2 # Halve the harvest from the sources determined above (not the releases!)
  }
  
  # Genetic releases - every fifth year harvest Hauturu and allocate to the two occupied sites with the smallest N with no previous genetic imports
  if(((yr+2)%%5)==0){ # Plan two years before!
    gg[which(sa==sort(sa[sn!="Hauturu"&N.est[yr+1,]>1&colSums(Gx[1:(yr+1),])==0])[1])] <- 1 # Note: this works for n.years=20!
    gg[which(sa==sort(sa[sn!="Hauturu"&N.est[yr+1,]>1&colSums(Gx[1:(yr+1),])==0])[2])] <- 1 # If there is a second site
    Hv[yr+2,sn=="Hauturu"] <- Hv[yr+2,sn=="Hauturu"]-max(10,10*sum(gg)) # Harvest Hauturu - on top of birds above
    Hv[yr+2,sn!="Hauturu"] <- -Hv[yr+2,sn=="Hauturu"]/sum(gg)*gg[which(sn!="Hauturu")] # Release
  }
  Gx[yr+2,] <- gg # Track which sites have received genetic releases 

## FEED
ff[N.est[yr+1,]+Hv[yr+1,]>1] <- 1 # All occupied sites and those which will receive birds next year
ff[sx=="Remote"] <- 0  # Never Hauturu, never remote islands
# Convert into actual feeding decisions
FR.1[yr+1,] <- ff+1

# Function returns:
return(list(Harvest=Hv,FR=FR.1,failed=fk,Genetics=Gx))
}

################ Strategy 1 ENDS HERE 
####################################################################################################



# Strategy 2: sequential ------------------------------
# CHECKED
# Harvest: 1) Harvest Tiri for Nj=2*max.Hv (max.Hv female juvs) if N.est[t]>minN.Hv
#          2) Harvest any site for Nj=2*max.Hv (max.Hv female juvs) where N.est[t]>minN.Hv AND N.est[t]/K.managed >= 0.75
# Release: 1) Unoccupied (island) site with highest K(unfed). Commit to >=2 years of releases, 
#             unless the first season only produces Ntot<fk.cut in which case, go to the next best site.
#          2) Unoccupied (mainland) site with highest K(fed). Commit to >=2 years of releases, 
#             unless the first season only produces Ntot<fk.cut in which case, go to the next best site.
#          3) Top up existing sites where N.est[t]<N.top.1 and K.fed>kf.cut (original sites) OR K.unfed>ku.cut (later sites)
#          4) back to 1), then 2), then 3) until no more sites or birds available
# Feeding: 1) ad libitum at all initial occupied sites except Hauturu
#          2) Do not feed recipient sites at harvest 1) above
#          3) Feed new mainland sites for the first 4 years (or until N.est==0), then remove
#          3b) Restart if N.est drops by drop.p% or <drop.N (smallest)

strategy.2.rules <<- function(yr=y,ns=n.sites,sn=site.names,st=site.type,sa=site.ages,sx=site.access,fl=feed.levels,
                              N.est=N.est,Hv=Harvest,FR.1=FR,fk=failed,Gx=Genetics,
                              Ke=K.est,Km=K.managed,Kt=K.true,
                              max.Hv=max.h,fk.cut=fail.cut,ku.cut.1=ku.cut,kf.cut.1=kf.cut,feed.cut.1=feed.cut,
                              minN.Hv=minN.h,N.top.1=N.top,drop.p.1=drop.p,drop.N.1=drop.N
){

hh <- rr <- ff <- gg <- rep(0,ns) 

# HARVEST

# Sources
hh[sn=="Tiri"&N.est[yr+1,]>=minN.Hv] <- 1 # Tiri harvest based on population size
Km.est <- c();for(i in 1:ns){Km.est[i] <- Ke[yr,i,FR[yr,i]]} # Estimated K.managed (needed to choose harvest below)
hh[N.est[yr+1,]>=minN.Hv&N.est[yr+1,]/Km.est>0.75] <- 1 # Other harvests based on capacity
hh[sn=="Hauturu"] <- 0 # Never Hauturu

# FIRST PRIORITY: sites with ongoing releases NB: condition only valid for years >1
if(yr>2){
  cond.1 <- which(sn!="Hauturu"&fk[yr,]==0&colSums(Hv[max(1,yr-1):yr,]>0)==1&Hv[yr,]==1&N.est[yr+1,]>=fk.cut&Ke[yr+1,,1]>=ku.cut.1&N.est[yr+1,]+Hv[yr+1,]<Ke[yr+1,,1])
  # If there are site failures, add to to list
  fk[yr+1,which((sn!="Hauturu"&colSums(Hv[max(1,yr-1):yr,]>0)==1&Hv[yr,]==1&N.est[yr+1,]<fk.cut)|fk[yr,]==1)] <- 1
}
# SECOND PRIORITY: unoccupied island sites (with no previous failure and no ongoing releases)
cond.2 <- which(sn!="Hauturu"&st=="Island"&N.est[yr+1,]<1&Hv[yr+1,]==0&rr==0&fk[yr+1,]==0&Ke[yr+1,,1]>=fk.cut) # 2 + (4*(j-1)) # CHECK: fk.cut or ku.cut?
# THIRD PRIORITY: unoccupied mainland sites (with no previous failure and no ongoing releases)
cond.3 <- which(st=="Mainland"&N.est[yr+1,]<1&Hv[yr+1,]==0&rr==0&fk[yr+1,]==0) # 3 + (4*(j-1))
# FOURTH PRIORITY: initially occupied sites with long-term feeding: which occupied site has N<N.top and K.fed>kf.cut
cond.4 <- which(N.est[1,]>1&N.est[yr+1,]>1&Ke[yr+1,,fl]>kf.cut.1&N.est[yr+1,]<N.top.1&rr==0&fk[yr+1,]==0) # 4 + (4*(j-1))
# FIFTH PRIORITY: new occupied sites without long-term feeding: which occupied site has N<N.top and K.unfed>ku.cut
cond.5 <- which(N.est[1,]==0&N.est[yr+1,]>1&Ke[yr+1,,1]>ku.cut.1&N.est[yr+1,]<N.top.1&rr==0&fk[yr+1,]==0) # 5 + (4*(j-1))


# Identify other potential destination sites in order of priority
# Check how many more sites in total fulfil conditions for release (to set loop limits)
max.rr <- length(c(cond.2,cond.3,cond.4,cond.5))

# FIRST PRIORITY: rank by K.unfed
j=1
if(yr>2){
  while(sum(rr>0)<sum(hh)|j<max.rr){ # If number of release sites matches that of potential sources, break
    rr[which(Ke[yr+1,,1]==sort(Ke[yr+1,cond.1,1],d=T)[j])] <- j
    if(sum(rr>0)==length(cond.1)){break} # When all sites with this priority level have been used, break
    j=j+1 # If there are still sites available with this level of priority, another round
  } # While
} # if

# SECOND TO FIFTH PRIORITY:
j=max(rr)+1
while(sum(rr>0)<sum(hh)|j<max.rr){ # If number of release sites matches that of potential sources, break
  # SECOND PRIORITY: rank by K.unfed
  rr[which(Ke[yr+1,,1]==sort(Ke[yr+1,cond.2,1],d=T)[j])] <- max(rr)+1
  if(sum(rr>0)<sum(hh)|j<max.rr)break
  # THIRD PRIORITY: rank by K.fed
  rr[which(Ke[yr+1,,fl]==sort(Ke[yr+1,cond.3,fl],d=T)[j])] <- max(rr)+1
  if(sum(rr>0)<sum(hh)|j<max.rr)break
  # FOURTH PRIORITY: rank by N/K.fed
  rr[which(N.est[yr+1,]/Ke[yr+1,,fl]==sort(N.est[yr+1,cond.4]/Ke[yr+1,cond.4,fl],d=T)[j])] <- max(rr)+1
  if(sum(rr>0)<sum(hh)|j<max.rr)break
  # FIFTH PRIORITY: rank by N/K.unfed
  rr[which(N.est[yr+1,]/Ke[yr+1,,1]==sort(N.est[yr+1,cond.5]/Ke[yr+1,cond.5,1],d=T)[j])] <- max(rr)+1
  if(sum(rr>0)<sum(hh)|j<max.rr)break
  j=j+1 # If I haven't used all sites, another round
}

# If number of sources exceeds number of available destinations, discard sources based on population sizes
if(sum(hh)>sum(rr>0)){hh[!is.na(match(sn,names(sort(N.est[yr+1,hh==1])[1:(sum(hh)-sum(rr>0))])))] <- 0}

# Allocate actual birds
Hv[yr+2,] <- 0
if(sum(hh)==0|sum(rr)==0) {Hv[yr+2,] <- 0} else{   # If there are no suitable sources or destinations, no harvest
  Hv[yr+2,hh==1] <- -max.Hv # Otherwise Harvest max.Hv juveniles from each source site
  birds.available <- -sum(Hv[yr+2,])     # How many of these birds actually become available for release?
  for(k in 1:max(rr)){     # Then allocate birds in order of priority
    Hv[yr+2,rr==k] <- min(max.Hv,birds.available-sum(Hv[yr+2,Hv[yr+2,]>0]))
    if(birds.available-sum(Hv[yr+2,Hv[yr+2,]>0])==0){break} # When all birds have been used, break
  } # for
} # else 

# If one or more of the release sites are on remote islands:
if(sum(sx[Hv[yr+2,]>0]=="Remote")>0){
  Hv[yr+2,sn=="Hauturu"] <- sum(-Hv[yr+2,Hv[yr+2,]>0&sx=="Remote"])/2 # Harvest half of those birds from Hauturu
  # Which sites do we take less birds from? The smallest ones of the harvest candidates (matching number of remote releases)
  put.back <- which(N.est[yr+1,]==sort(N.est[yr+1,Hv[yr+2,]<0&sn!="Hauturu"],d=F)[1:length(sn[Hv[yr+2,]>0&sx=="Remote"])])
  Hv[yr+2,put.back] <- Hv[yr+2,put.back]/2 # Halve the harvest from the sources determined above (not the releases!)
}

# Genetic releases - every fifth year harvest Hauturu and allocate to the two occupied sites with the smallest N with no previous genetic imports
if(((yr+2)%%5)==0){ # Plan two years before!
  gg[which(sa==sort(sa[sn!="Hauturu"&N.est[yr+1,]>1&colSums(Gx[1:(yr+1),])==0])[1])] <- 1
  gg[which(sa==sort(sa[sn!="Hauturu"&N.est[yr+1,]>1&colSums(Gx[1:(yr+1),])==0])[2])] <- 1 # If there is a second site
  Hv[yr+2,sn=="Hauturu"] <- Hv[yr+2,sn=="Hauturu"]-max(10,10*sum(gg)) # Harvest Hauturu - on top of birds above
  Hv[yr+2,sn!="Hauturu"] <- -Hv[yr+2,sn=="Hauturu"]/sum(gg)*gg[which(sn!="Hauturu")] # Release
}
Gx[yr+2,] <- gg # Track which sites have received genetic releases 

## FEED
# All initially and currently occupied sites
ff[N.est[1,]>1&N.est[yr+1,]+Hv[yr+1,]>1] <- 1
# Feed occupied non-failed mainland sites with ongoing releases,unless they have exceeded their K.unfed or Ku.cut
ff[st=="Mainland"&fk[yr+1,]==0&Hv[yr+1,]>0&N.est[yr+1,]+Hv[yr+1,]<min(ku.cut.1,K.est[yr+1,,1])] <- 1
# Feed occupied, non-failed mainland sites that have dropped below N<drop.N or by drop.p%
ff[st=="Mainland"&fk[yr+1,]==0&N.est[yr+1,]>1&(N.est[yr+1,]+Hv[yr+1,]<min(drop.N.1,Ke[yr+1,,1])|(N.est[yr+1,]+Hv[yr+1,])/N.est[yr,]<drop.p.1)] <- 1
ff[st=="Island"&N.est[1,]==0] <- 0  # Never feed new island sites (includes the "remote" condition used for other strategies)
# Convert into actual feeding decisions
FR.1[yr+1,] <- ff+1

# Function returns:
return(list(Harvest=Hv,FR=FR.1,failed=fk,Genetics=Gx))
}

################ Strategy 2 ENDS HERE 
####################################################################################################



# Strategy 3: seasonal ------------------------------
# CHECKED
# Harvest: 1) Harvest any site for Nj=2*max.Hv (max.Hv juv females) where N.est[t]>=minN.Hv
# Release: 1) Any unoccupied site with K(unfed)>ku.cut.1. Commit to 4 years of releases or until K[unfed] is reached.
#             If population drops below N.est<fk.cut and estimated K.unfed<max.Hv, abandon and mark as failure
#             Where no feeding is possible, use 50% Hauturu birds
#          2) If no unoccupied sites exist, top up existing sites where feeding restarted, until N.est>40
# Feeding: 1) At libitum at new sites during release years, until N>40 or N>k.unfed
#          2) Sites with >7 years: reduce to visitor experience feeders when demand is low (= keep feeding)
#          3) Restart if N(f) drops >drop.p% or to N<40

strategy.3.rules <<- function(yr=y,ns=n.sites,sn=site.names,st=site.type,sa=site.ages,sx=site.access,fl=feed.levels,
                              N.est=N.est,Hv=Harvest,FR.1=FR,fk=failed,Gx=Genetics,
                              Ke=K.est,Km=K.managed,Kt=K.true,
                              max.Hv=max.h,fk.cut=fail.cut,ku.cut.1=ku.cut,kf.cut.1=kf.cut,feed.cut.1=feed.cut,
                              minN.Hv=minN.h,N.top.1=N.top,drop.p.1=drop.p,drop.N.1=drop.N
){
  
  hh <- rr <- ff <- gg <- rep(0,n.sites) 
  
  # Potential sources (never Hauturu)
  hh[sn!="Hauturu"&N.est[yr+1,]>minN.Hv] <- 1
  
  # FIRST PRIORITY: sites with ongoing releases NB: condition only valid for years >1
  if(yr>2){
    cond.1 <- which(sn!="Hauturu"&fk[yr,]==0&colSums(Hv[max(1,yr-3):yr,]>0)<4&N.est[max(1,yr-4),]==0&N.est[yr+1,]>=fk.cut&Ke[yr+1,,1]>=ku.cut.1&N.est[yr+1,]+Hv[yr+1,]<Ke[yr+1,,1])
    # If there are site failures, add to to list
    fk[yr+1,which(sn!="Hauturu"&(colSums(Hv[max(2,yr-3):yr,]>0)<4&colSums(Hv[max(2,yr-3):yr,]>0)>0&N.est[max(1,yr-4),]==0&N.est[yr+1,]<fk.cut)|fk[yr,]==1)] <- 1
  }
  # SECOND PRIORITY: Unoccupied sites with (1) K.unfed>ku.cut.1 (2) no previous failure (3) no ongoing releases to avoid double counting
  cond.2 <- which(N.est[yr+1,]+Hv[yr+1,]<1&fk[yr+1,]==0&Ke[yr+1,,1]>=ku.cut.1)
  # THIRD PRIORITY: Top-up occupied sites where N drops below drop.N or K.unfed, or by drop.p
  cond.3 <- which(N.est[yr+1,]+Hv[yr+1,]>1&fk[yr+1,]==0&(N.est[yr+1,]+Hv[yr+1,]<min(drop.N.1,Km[yr+1,]))|(N.est[yr+1,]+Hv[yr+1,])/N.est[yr,]<drop.p.1)
  
  # FIRST PRIORITY: rank by K.unfed
  if(yr>2){
    j=1
    while(sum(rr>0)<sum(hh)){ # If number of release sites matches that of potential sources, break
      rr[which(Ke[yr+1,,1]==sort(Ke[yr+1,cond.1,1],d=T)[j])] <- j
      if(sum(rr>0)==length(cond.1)){break} # When all sites with this priority level have been used, break
      j=j+1 # If there are still sites available with this level of priority, another round
    } # While
  } # if
  
  # SECOND PRIORITY: rank by K.unfed
  j=max(rr)+1
  while(sum(rr>0)<sum(hh)){ # If number of release sites matches that of potential sources, break
    rr[which(Ke[yr+1,,1]==sort(Ke[yr+1,cond.2,1],d=T)[j-max(rr)])] <- j
    if(sum(rr>0)<sum(hh)|sum(rr>0)==length(cond.2)){break} # When all sites with this priority level have been used, break
    j=j+1 # If there are still sites available with this level of priority, another round
  }
  
  # THIRD PRIORITY: rank by difference N/K.unfed
  j=max(rr)+1
  while(sum(rr>0)<sum(hh)){ # If number of release sites matches that of potential sources, break
    rr[which(N.est[yr+1,]/Ke[yr+1,,1]==sort(N.est[yr+1,cond.3]/Ke[yr+1,cond.3,1],d=T)[j-max(rr)])] <- j
    if(sum(rr>0)<sum(hh)|sum(rr>0)==length(cond.3)){break} # When all sites with this priority level have been used, break
    j=j+1 # If there are still sites available with this level of priority, another round
  }
  
  # If number of sources exceeds number of available destinations, discard sources based on population sizes
  if(sum(hh)>sum(rr>0)){hh[!is.na(match(sn,names(sort(N.est[yr+1,hh==1])[1:(sum(hh)-sum(rr>0))])))] <- 0}
  
  # Allocate actual birds
  Hv[yr+2,] <- 0
  if(sum(hh)==0|sum(rr)==0) {Hv[yr+2,] <- 0} else{   # If there are no suitable sources or destinations, no harvest
    Hv[yr+2,hh==1] <- -max.Hv # Otherwise Harvest max.Hv juveniles from each source site
    birds.available <- -sum(Hv[yr+2,])     # How many of these birds actually become available for release?
    for(k in 1:max(rr)){     # Then allocate birds in order of priority
      Hv[yr+2,rr==k] <- min(max.Hv,birds.available-sum(Hv[yr+2,Hv[yr+2,]>0]))
      if(birds.available-sum(Hv[yr+2,Hv[yr+2,]>0])==0){break} # When all birds have been used, break
    } # for
  } # else 
  
  # If one or more of the release sites are on remote islands:
  if(sum(sx[Hv[yr+2,]>0]=="Remote")>0){
    Hv[yr+2,sn=="Hauturu"] <- sum(-Hv[yr+2,Hv[yr+2,]>0&sx=="Remote"])/2 # Harvest half of those birds from Hauturu
    # Which sites do we take less birds from? The smallest ones of the harvest candidates (matching number of remote releases)
    put.back <- which(N.est[yr+1,]==sort(N.est[yr+1,Hv[yr+2,]<0&sn!="Hauturu"],d=F)[1:length(sn[Hv[yr+2,]>0&sx=="Remote"])])
    Hv[yr+2,put.back] <- Hv[yr+2,put.back]/2 # Halve the harvest from the sources determined above (not the releases!)
  }
  
  # Genetic releases - every fifth year harvest Hauturu and allocate to the two occupied sites with the smallest N with no previous genetic imports
  if(((yr+2)%%5)==0){ # Plan two years before!
    gg[which(sa==sort(sa[sn!="Hauturu"&N.est[yr+1,]>1&colSums(Gx[1:(yr+1),])==0])[1])] <- 1
    gg[which(sa==sort(sa[sn!="Hauturu"&N.est[yr+1,]>1&colSums(Gx[1:(yr+1),])==0])[2])] <- 1 # If there is a second site
    Hv[yr+2,sn=="Hauturu"] <- Hv[yr+2,sn=="Hauturu"]-max(10,10*sum(gg)) # Harvest Hauturu - on top of birds above
    Hv[yr+2,sn!="Hauturu"] <- -Hv[yr+2,sn=="Hauturu"]/sum(gg)*gg[which(sn!="Hauturu")] # Release
  }
  Gx[yr+2,] <- gg # Track which sites have received genetic releases 
  
  ## FEED
  
  # Feed during release years if N<40 or N<K[unfed]
  ff[Hv[yr+1,]>1&N.est[yr+1,]+Hv[yr+1,]<min(feed.cut.1,Ke[yr+1,,1])] <- 1
  # Feed initially unoccupied, non-failed mainland sites that have dropped below N<40 or by drop.p%
  ff[N.est[1,]==0&N.est[yr+1,]>1&fk[yr+1,]==0&(N.est[yr+1,]+Hv[yr+1,]<min(feed.cut.1,Ke[yr+1,,1])|((N.est[yr+1,]+Hv[yr+1,])/N.est[yr,])<drop.p.1)] <- 1
  ff[N.est[1,]>1] <- 1 # Always feed original sites
  ff[sx=="Remote"] <- 0  # Never Hauturu, never remote islands 
  # Convert into actual feeding decisions
  FR.1[yr+1,] <- ff+1
  
  # Function returns:
  return(list(Harvest=Hv,FR=FR.1,failed=fk,Genetics=Gx))
}

################ Strategy 3 ENDS HERE
####################################################################################################



# Strategy 4: new learning ------------------------------
# CHECKED
# Harvest: 1) Harvest any site for Nj=40 (max.Hv juv females) where N.est[t]>minN.Hv
# Release: 1) Any unoccupied site with K(unfed)>ku.cut.1. Commit to 4 years of releases
#             If population drops below N.est<fk.cut, abandon and mark as failure
#          2) For Taranga releases, 50% Hauturu birds
#          3) If no unoccupied sites exist, top up existing sites where N<N.top.1 and K.fed>kf.cut.1
# Feeding: 1) Status quo at initial sites
#          2) Never new sites

strategy.4.rules <<- function(yr=y,ns=n.sites,sn=site.names,st=site.type,sa=site.ages,sx=site.access,fl=feed.levels,
                              N.est=N.est,Hv=Harvest,FR.1=FR,fk=failed,Gx=Genetics,
                              Ke=K.est,Km=K.managed,Kt=K.true,
                              max.Hv=max.h,fk.cut=fail.cut,ku.cut.1=ku.cut,kf.cut.1=kf.cut,feed.cut.1=feed.cut,
                              minN.Hv=minN.h,N.top.1=N.top,drop.p.1=drop.p,drop.N.1=drop.N
){
  
  hh <- rr <- ff <- gg <- rep(0,n.sites) 
  
  # Potential sources (never Hauturu)
  hh[sn!="Hauturu"&N.est[yr+1,]>minN.Hv] <- 1
  
  # FIRST PRIORITY: sites with ongoing releases NB: condition only valid for years >1
  if(yr>2){
    cond.1 <- which(sn!="Hauturu"&fk[yr,]==0&colSums(Hv[max(1,yr-3):yr,]>0)<4&N.est[max(1,yr-4),]==0&N.est[yr+1,]>=fk.cut&Ke[yr+1,,1]>=ku.cut.1&N.est[yr+1,]+Hv[yr+1,]<Ke[yr+1,,1])
    # If there are site failures, add to to list
    fk[yr+1,which(sn!="Hauturu"&(colSums(Hv[max(2,yr-3):yr,]>0)<4&colSums(Hv[max(2,yr-3):yr,]>0)>0&N.est[max(1,yr-4),]==0&N.est[yr+1,]<fk.cut)|fk[yr,]==1)] <- 1
  }
  # SECOND PRIORITY: Unoccupied sites with (1) K.unfed>ku.cut (2) no previous failure (3) no ongoing releases to avoid double counting
  cond.2 <- which(N.est[yr+1,]+Hv[yr+1,]<1&fk[yr+1,]==0&Ke[yr+1,,1]>=ku.cut.1)
  # THIRD PRIORITY: original occupied sites with long-term feeding where N<N.top and K.fed>kf.cut
  cond.3 <- which(N.est[1,]>1&N.est[yr+1,]>1&Ke[yr+1,,fl]>kf.cut.1&N.est[yr+1,]<N.top.1&rr==0&fk[yr+1,]==0)
  # FOURTH PRIORITY: new occupied sites without long-term feeding where N<N.top and K.unfed>ku.cut
  cond.4 <- which(N.est[1,]==0&N.est[yr+1,]>1&Ke[yr+1,,1]>ku.cut.1&N.est[yr+1,]<N.top.1&rr==0&fk[yr+1,]==0)
  
  # FIRST PRIORITY: rank by K.unfed
  if(yr>2){
    j=1
    while(sum(rr>0)<sum(hh)){ # If number of release sites matches that of potential sources, break
      rr[which(Ke[yr+1,,1]==sort(Ke[yr+1,cond.1,1],d=T)[j])] <- j
      if(sum(rr>0)==length(cond.1)){break} # When all sites with this priority level have been used, break
      j=j+1 # If there are still sites available with this level of priority, another round
    } # While
  } # if
  
  # SECOND PRIORITY: rank by K.unfed
  j=max(rr)+1
  while(sum(rr>0)<sum(hh)){ # If number of release sites matches that of potential sources, break
    rr[which(Ke[yr+1,,1]==sort(Ke[yr+1,cond.2,1],d=T)[j-max(rr)])] <- j
    if(sum(rr>0)<sum(hh)|sum(rr>0)==length(cond.2)){break} # When all sites with this priority level have been used, break
    j=j+1 # If there are still sites available with this level of priority, another round
  }
  
  # THIRD PRIORITY: rank by difference N/K.unfed
  j=max(rr)+1
  while(sum(rr>0)<sum(hh)){ # If number of release sites matches that of potential sources, break
    rr[which(N.est[yr+1,]/Ke[yr+1,,fl]==sort(N.est[yr+1,cond.3]/Ke[yr+1,cond.3,fl],d=T)[j-max(rr)])] <- j
    if(sum(rr>0)<sum(hh)|sum(rr>0)==length(cond.3)){break} # When all sites with this priority level have been used, break
    j=j+1 # If there are still sites available with this level of priority, another round
  }
  
  # FOURTH PRIORITY: rank by difference N/K.unfed
  j=max(rr)+1
  while(sum(rr>0)<sum(hh)){ # If number of release sites matches that of potential sources, break
    rr[which(N.est[yr+1,]/Ke[yr+1,,1]==sort(N.est[yr+1,cond.4]/Ke[yr+1,cond.4,1],d=T)[j-max(rr)])] <- j
    if(sum(rr>0)<sum(hh)|sum(rr>0)==length(cond.4)){break} # When all sites with this priority level have been used, break
    j=j+1 # If there are still sites available with this level of priority, another round
  }
  
  
  # If number of sources exceeds number of available destinations, discard sources based on population sizes
  if(sum(hh)>sum(rr>0)){hh[!is.na(match(sn,names(sort(N.est[yr+1,hh==1])[1:(sum(hh)-sum(rr>0))])))] <- 0}
  
  # Allocate actual birds
  Hv[yr+2,] <- 0
  if(sum(hh)==0|sum(rr)==0) {Hv[yr+2,] <- 0} else{   # If there are no suitable sources or destinations, no harvest
    Hv[yr+2,hh==1] <- -max.Hv # Otherwise Harvest max.Hv juveniles from each source site
    birds.available <- -sum(Hv[yr+2,])     # How many of these birds actually become available for release?
    for(k in 1:max(rr)){     # Then allocate birds in order of priority
      Hv[yr+2,rr==k] <- min(max.Hv,birds.available-sum(Hv[yr+2,Hv[yr+2,]>0]))
      if(birds.available-sum(Hv[yr+2,Hv[yr+2,]>0])==0){break} # When all birds have been used, break
    } # for
  } # else 
  
  # If one or more of the release sites are on remote islands:
  if(sum(sx[Hv[yr+2,]>0]=="Remote")>0){
    Hv[yr+2,sn=="Hauturu"] <- sum(-Hv[yr+2,Hv[yr+2,]>0&sx=="Remote"])/2 # Harvest half of those birds from Hauturu
    # Which sites do we take less birds from? The smallest ones of the harvest candidates (matching number of remote releases)
    put.back <- which(N.est[yr+1,]==sort(N.est[yr+1,Hv[yr+2,]<0&sn!="Hauturu"],d=F)[1:length(sn[Hv[yr+2,]>0&sx=="Remote"])])
    Hv[yr+2,put.back] <- Hv[yr+2,put.back]/2 # Halve the harvest from the sources determined above (not the releases!)
  }
  
  # Genetic releases - every fifth year harvest Hauturu and allocate to the two occupied sites with the smallest N with no previous genetic imports
  if(((yr+2)%%5)==0){ # Plan two years before!
    gg[which(sa==sort(sa[sn!="Hauturu"&N.est[yr+1,]>1&colSums(Gx[1:(yr+1),])==0])[1])] <- 1
    gg[which(sa==sort(sa[sn!="Hauturu"&N.est[yr+1,]>1&colSums(Gx[1:(yr+1),])==0])[2])] <- 1 # If there is a second site
    Hv[yr+2,sn=="Hauturu"] <- Hv[yr+2,sn=="Hauturu"]-max(10,10*sum(gg)) # Harvest Hauturu - on top of birds above
    Hv[yr+2,sn!="Hauturu"] <- -Hv[yr+2,sn=="Hauturu"]/sum(gg)*gg[which(sn!="Hauturu")] # Release
  }
  Gx[yr+2,] <- gg # Track which sites have received genetic releases 
  
  ## FEED
  ff[N.est[1,]>1&N.est[yr+1,]+Hv[yr+1,]>1] <- 1   # All initially and currently occupied sites
  ff[which(sx=="Remote")] <- 0  # Never Hauturu, never remote islands
  # Convert into actual feeding decisions
  FR.1[yr+1,] <- ff+1
  
  # Function returns:
  return(list(Harvest=Hv,FR=FR.1,failed=fk,Genetics=Gx))
}

################ Strategy 4 ENDS HERE
####################################################################################################



# Strategy 5: visitor ------------------------------
# PROBLEMATIC STRATEGY - DOUBLE CHECK
# Harvest: 1) Harvest any site for Nj=40 (max.Hv juv females) where N.est[t]>minN.Hv
# Release: 1) Any unoccupied site with K(unfed)>ku.cut.1. Commit to 4 years of releases
#             If population drops below N.est<fk.cut, abandon and mark as failure
#          2) Where no feeding possible, 50% Hauturu birds
#          3) If no unoccupied sites exist, top up existing (initial?) sites where N<N.top.1 and K.fed>kf.cut.1
# Feeding: 1) New sites over release years (except remote islands) until N>40
#          2) Remove food from all sites N>40
#          3) Restart when N drops by drop.p% or <40 (smallest)

strategy.5.rules <<- function(yr=y,ns=n.sites,sn=site.names,st=site.type,sa=site.ages,sx=site.access,fl=feed.levels,
                              N.est=N.est,Hv=Harvest,FR.1=FR,fk=failed,Gx=Genetics,
                              Ke=K.est,Km=K.managed,Kt=K.true,
                              max.Hv=max.h,fk.cut=fail.cut,ku.cut.1=ku.cut,kf.cut.1=kf.cut,feed.cut.1=feed.cut,
                              minN.Hv=minN.h,N.top.1=N.top,drop.p.1=drop.p,drop.N.1=drop.N
){
  
  hh <- rr <- ff <- gg <- rep(0,n.sites) 
  
  # Potential sources (never Hauturu)
  hh[sn!="Hauturu"&N.est[yr+1,]>minN.Hv] <- 1
  
  # FIRST PRIORITY: sites with ongoing releases NB: condition only valid for years >1
  if(yr>2){
    cond.1 <- which(sn!="Hauturu"&fk[yr,]==0&colSums(Hv[max(1,yr-3):yr,]>0)<4&N.est[max(1,yr-4),]==0&N.est[yr+1,]>=fk.cut&Ke[yr+1,,1]>=ku.cut.1&N.est[yr+1,]+Hv[yr+1,]<Ke[yr+1,,1])
    # If there are site failures, add to to list
    fk[yr+1,which(sn!="Hauturu"&(colSums(Hv[max(2,yr-3):yr,]>0)<4&colSums(Hv[max(2,yr-3):yr,]>0)>0&N.est[max(1,yr-4),]==0&N.est[yr+1,]<fk.cut)|fk[yr,]==1)] <- 1
  }
  # SECOND PRIORITY: Unoccupied sites with (1) K.unfed>ku.cut.1 (2) no previous failure (3) no ongoing releases to avoid double counting
  cond.2 <- which(N.est[yr+1,]+Hv[yr+1,]<1&fk[yr+1,]==0&Ke[yr+1,,1]>=ku.cut.1)
  # THIRD PRIORITY: Top-up occupied sites where N drops below drop.N or K.unfed, or by drop.p
  cond.3 <- which(N.est[yr+1,]+Hv[yr+1,]>1&fk[yr+1,]==0&(N.est[yr+1,]+Hv[yr+1,]<min(drop.N.1,Km[yr+1,]))|(N.est[yr+1,]+Hv[yr+1,])/N.est[yr,]<drop.p.1)
  
  # FIRST PRIORITY: rank by K.unfed
  if(yr>2){
    j=1
    while(sum(rr>0)<sum(hh)){ # If number of release sites matches that of potential sources, break
      rr[which(Ke[yr+1,,1]==sort(Ke[yr+1,cond.1,1],d=T)[j])] <- j
      if(sum(rr>0)==length(cond.1)){break} # When all sites with this priority level have been used, break
      j=j+1 # If there are still sites available with this level of priority, another round
    } # While
  } # if
  
  # SECOND PRIORITY: rank by K.unfed
  j=max(rr)+1
  while(sum(rr>0)<sum(hh)){ # If number of release sites matches that of potential sources, break
    rr[which(Ke[yr+1,,1]==sort(Ke[yr+1,cond.2,1],d=T)[j-max(rr)])] <- j
    if(sum(rr>0)<sum(hh)|sum(rr>0)==length(cond.2)){break} # When all sites with this priority level have been used, break
    j=j+1 # If there are still sites available with this level of priority, another round
  }
  
  # THIRD PRIORITY: rank by difference N/K.unfed
  j=max(rr)+1
  while(sum(rr>0)<sum(hh)){ # If number of release sites matches that of potential sources, break
    rr[which(N.est[yr+1,]/Ke[yr+1,,1]==sort(N.est[yr+1,cond.3]/Ke[yr+1,cond.3,1],d=T)[j-max(rr)])] <- j
    if(sum(rr>0)<sum(hh)|sum(rr>0)==length(cond.3)){break} # When all sites with this priority level have been used, break
    j=j+1 # If there are still sites available with this level of priority, another round
  }
  
  # If number of sources exceeds number of available destinations, discard sources based on population sizes
  if(sum(hh)>sum(rr>0)){hh[!is.na(match(sn,names(sort(N.est[yr+1,hh==1])[1:(sum(hh)-sum(rr>0))])))] <- 0}
  
  # Allocate actual birds
  Hv[yr+2,] <- 0
  if(sum(hh)==0|sum(rr)==0) {Hv[yr+2,] <- 0} else{   # If there are no suitable sources or destinations, no harvest
    Hv[yr+2,hh==1] <- -max.Hv # Otherwise Harvest max.Hv juveniles from each source site
    birds.available <- -sum(Hv[yr+2,])     # How many of these birds actually become available for release?
    for(k in 1:max(rr)){     # Then allocate birds in order of priority
      Hv[yr+2,rr==k] <- min(max.Hv,birds.available-sum(Hv[yr+2,Hv[yr+2,]>0]))
      if(birds.available-sum(Hv[yr+2,Hv[yr+2,]>0])==0){break} # When all birds have been used, break
    } # for
  } # else 
  
  # If one or more of the release sites are on remote islands:
  if(sum(sx[Hv[yr+2,]>0]=="Remote")>0){
    Hv[yr+2,sn=="Hauturu"] <- sum(-Hv[yr+2,Hv[yr+2,]>0&sx=="Remote"])/2 # Harvest half of those birds from Hauturu
    # Which sites do we take less birds from? The smallest ones of the harvest candidates (matching number of remote releases)
    put.back <- which(N.est[yr+1,]==sort(N.est[yr+1,Hv[yr+2,]<0&sn!="Hauturu"],d=F)[1:length(sn[Hv[yr+2,]>0&sx=="Remote"])])
    Hv[yr+2,put.back] <- Hv[yr+2,put.back]/2 # Halve the harvest from the sources determined above (not the releases!)
  }
  
  # Genetic releases - every fifth year harvest Hauturu and allocate to the two occupied sites with the smallest N with no previous genetic imports
  if(((yr+2)%%5)==0){ # Plan two years before!
    gg[which(sa==sort(sa[sn!="Hauturu"&N.est[yr+1,]>1&colSums(Gx[1:(yr+1),])==0])[1])] <- 1
    gg[which(sa==sort(sa[sn!="Hauturu"&N.est[yr+1,]>1&colSums(Gx[1:(yr+1),])==0])[2])] <- 1 # If there is a second site
    Hv[yr+2,sn=="Hauturu"] <- Hv[yr+2,sn=="Hauturu"]-max(10,10*sum(gg)) # Harvest Hauturu - on top of birds above
    Hv[yr+2,sn!="Hauturu"] <- -Hv[yr+2,sn=="Hauturu"]/sum(gg)*gg[which(sn!="Hauturu")] # Release
  }
  Gx[yr+2,] <- gg # Track which sites have received genetic releases 
  
  ## FEED
  
  # Feed non-failed sites with ongoing releases where N<feed.cut or N<Ku.cut
  ff[Hv[yr+1,]>1&fk[yr+1,]==0&N.est[yr+1,]+Hv[yr+1,]<min(feed.cut.1,Ke[yr+1,,1])] <- 1
  # Feed occupied sites where N>1.1*K.unfed (avoid collapse)
  ff[N.est[yr+1,]+Hv[yr+1,]>1&N.est[yr+1,]+Hv[yr+1,]>(1.1*Ke[yr+1,,1])] <- 1
  # Feed occupied, non-failed sites that have dropped <drop.N or >drop.p%, but do not exceed their K.managed
  ff[N.est[yr+1,]+Hv[yr+1,]>1&fk[yr+1,]==0&(N.est[yr+1,]+Hv[yr+1,]<min(drop.N.1,Km[yr+1,]))|(N.est[yr+1,]+Hv[yr+1,])/N.est[yr,]<drop.p.1] <- 1 
  ff[sx=="Remote"] <- 0  # Never Hauturu, never remote islands
  # Convert into actual feeding decisions
  FR.1[yr+1,] <- ff+1

  # Function returns:
  return(list(Harvest=Hv,FR=FR.1,failed=fk,Genetics=Gx))
}
################ Strategy 5 ENDS HERE
####################################################################################################



# Strategy 6: responsive ------------------------------
# CHECKED
# Harvest: 1) Harvest any site for Nj=40 (max.Hv juv females) where N.est[t]>minN.Hv
# Release: 1) Any unoccupied site with K(unfed)>ku.cut.1. Commit to 4 years of releases
#             If population drops below N.est<fk.cut, abandon and mark as failure
#          2) Where no feeding possible, 50% Hauturu birds
#          3) If no unoccupied sites exist, top up existing (initial?) sites where feeding restarts
# Feeding: 1) Status quo for initially occupied sites
#          2) New sites over release years (except remote islands) until N>40 or N>K.unfed
#          3) Restart when N drops by drop.p% or <40 (smallest)


strategy.6.rules <<- function(yr=y,ns=n.sites,sn=site.names,st=site.type,sa=site.ages,sx=site.access,fl=feed.levels,
                              N.est=N.est,Hv=Harvest,FR.1=FR,fk=failed,Gx=Genetics,
                              Ke=K.est,Km=K.managed,Kt=K.true,
                              max.Hv=max.h,fk.cut=fail.cut,ku.cut.1=ku.cut,kf.cut.1=kf.cut,feed.cut.1=feed.cut,
                              minN.Hv=minN.h,N.top.1=N.top,drop.p.1=drop.p,drop.N.1=drop.N
){
  
  hh <- rr <- ff <- gg <- rep(0,n.sites) 
  
  # Potential sources (never Hauturu)
  hh[sn!="Hauturu"&N.est[yr+1,]>minN.Hv] <- 1
  
  # FIRST PRIORITY: sites with ongoing releases NB: condition only valid for years >1
  if(yr>2){
    cond.1 <- which(sn!="Hauturu"&fk[yr,]==0&colSums(Hv[max(1,yr-3):yr,]>0)<4&N.est[max(1,yr-4),]==0&N.est[yr+1,]>=fk.cut&Ke[yr+1,,1]>=ku.cut.1&N.est[yr+1,]+Hv[yr+1,]<Ke[yr+1,,1])
    # If there are site failures, add to to list
    fk[yr+1,which(sn!="Hauturu"&(colSums(Hv[max(2,yr-3):yr,]>0)<4&colSums(Hv[max(2,yr-3):yr,]>0)>0&N.est[max(1,yr-4),]==0&N.est[yr+1,]<fk.cut)|fk[yr,]==1)] <- 1
  }
  # SECOND PRIORITY: Unoccupied sites with (1) K.unfed>ku.cut.1 (2) no previous failure (3) no ongoing releases to avoid double counting
  cond.2 <- which(N.est[yr+1,]+Hv[yr+1,]<1&fk[yr+1,]==0&Ke[yr+1,,1]>=ku.cut.1)
  # THIRD PRIORITY: Top-up occupied sites where N drops below drop.N or K.unfed, or by drop.p
  cond.3 <- which(N.est[yr+1,]+Hv[yr+1,]>1&fk[yr+1,]==0&(N.est[yr+1,]+Hv[yr+1,]<min(drop.N.1,Km[yr+1,]))|(N.est[yr+1,]+Hv[yr+1,])/N.est[yr,]<drop.p.1)
  
  # FIRST PRIORITY: rank by K.unfed
  if(yr>2){
    j=1
    while(sum(rr>0)<sum(hh)){ # If number of release sites matches that of potential sources, break
      rr[which(Ke[yr+1,,1]==sort(Ke[yr+1,cond.1,1],d=T)[j])] <- j
      if(sum(rr>0)==length(cond.1)){break} # When all sites with this priority level have been used, break
      j=j+1 # If there are still sites available with this level of priority, another round
    } # While
  } # if
  
  # SECOND PRIORITY: rank by K.unfed
  j=max(rr)+1
  while(sum(rr>0)<sum(hh)){ # If number of release sites matches that of potential sources, break
    rr[which(Ke[yr+1,,1]==sort(Ke[yr+1,cond.2,1],d=T)[j-max(rr)])] <- j
    if(sum(rr>0)<sum(hh)|sum(rr>0)==length(cond.2)){break} # When all sites with this priority level have been used, break
    j=j+1 # If there are still sites available with this level of priority, another round
  }
  
  # THIRD PRIORITY: rank by difference N/K.unfed
  j=max(rr)+1
  while(sum(rr>0)<sum(hh)){ # If number of release sites matches that of potential sources, break
    rr[which(N.est[yr+1,]/Ke[yr+1,,1]==sort(N.est[yr+1,cond.3]/Ke[yr+1,cond.3,1],d=T)[j-max(rr)])] <- j
    if(sum(rr>0)<sum(hh)|sum(rr>0)==length(cond.3)){break} # When all sites with this priority level have been used, break
    j=j+1 # If there are still sites available with this level of priority, another round
  }
  
  # If number of sources exceeds number of available destinations, discard sources based on population sizes
  if(sum(hh)>sum(rr>0)){hh[!is.na(match(sn,names(sort(N.est[yr+1,hh==1])[1:(sum(hh)-sum(rr>0))])))] <- 0}
  
  # Allocate actual birds
  Hv[yr+2,] <- 0
  if(sum(hh)==0|sum(rr)==0) {Hv[yr+2,] <- 0} else{   # If there are no suitable sources or destinations, no harvest
    Hv[yr+2,hh==1] <- -max.Hv # Otherwise Harvest max.Hv juveniles from each source site
    birds.available <- -sum(Hv[yr+2,])     # How many of these birds actually become available for release?
    for(k in 1:max(rr)){     # Then allocate birds in order of priority
      Hv[yr+2,rr==k] <- min(max.Hv,birds.available-sum(Hv[yr+2,Hv[yr+2,]>0]))
      if(birds.available-sum(Hv[yr+2,Hv[yr+2,]>0])==0){break} # When all birds have been used, break
    } # for
  } # else 
  
  # If one or more of the release sites are on remote islands:
  if(sum(sx[Hv[yr+2,]>0]=="Remote")>0){
    Hv[yr+2,sn=="Hauturu"] <- sum(-Hv[yr+2,Hv[yr+2,]>0&sx=="Remote"])/2 # Harvest half of those birds from Hauturu
    # Which sites do we take less birds from? The smallest ones of the harvest candidates (matching number of remote releases)
    put.back <- which(N.est[yr+1,]==sort(N.est[yr+1,Hv[yr+2,]<0&sn!="Hauturu"],d=F)[1:length(sn[Hv[yr+2,]>0&sx=="Remote"])])
    Hv[yr+2,put.back] <- Hv[yr+2,put.back]/2 # Halve the harvest from the sources determined above (not the releases!)
  }
  
  # Genetic releases - every fifth year harvest Hauturu and allocate to the two occupied sites with the smallest N with no previous genetic imports
  if(((yr+2)%%5)==0){ # Plan two years before!
    gg[which(sa==sort(sa[sn!="Hauturu"&N.est[yr+1,]>1&colSums(Gx[1:(yr+1),])==0])[1])] <- 1
    gg[which(sa==sort(sa[sn!="Hauturu"&N.est[yr+1,]>1&colSums(Gx[1:(yr+1),])==0])[2])] <- 1 # If there is a second site
    Hv[yr+2,sn=="Hauturu"] <- Hv[yr+2,sn=="Hauturu"]-max(10,10*sum(gg)) # Harvest Hauturu - on top of birds above
    Hv[yr+2,sn!="Hauturu"] <- -Hv[yr+2,sn=="Hauturu"]/sum(gg)*gg[which(sn!="Hauturu")] # Release
  }
  Gx[yr+2,] <- gg # Track which sites have received genetic releases 
  
  ## FEED
  
  # All original occupied sites, new unoccupied sites with K.unfed<ku.cut, and sites that will receive birds for the first time
  ff[(N.est[1,]>1&N.est[yr+1,]+Hv[yr+1,]>1)|(N.est[1,]==0&N.est[yr+1,]>1&Ke[yr+1,,1]<ku.cut.1)] <- 1
  # Feed non-failed sites with ongoing releases that have not exceeded K.unfed or ku.cut
  ff[Hv[yr+1,]>1&fk[yr+1,]==0&N.est[yr+1,]+Hv[yr+1,]<min(Ke[yr+1,,1],ku.cut.1)] <- 1 
  # Feed non-failed sites currently occupied that (1) do not exceed K.managed or 40 birds, or (2) have dropped >drop.p%
  ff[N.est[yr+1,]+Hv[yr+1,]>1&fk[yr+1,]==0&(N.est[yr+1,]+Hv[yr+1,]<min(drop.N.1,Km[yr+1,]))|(N.est[yr+1,]+Hv[yr+1,])/N.est[yr,]<drop.p.1] <- 1 
  ff[sx=="Remote"] <- 0  # Never Hauturu, never remote islands
  # Convert into actual feeding decisions
  FR.1[yr+1,] <- ff+1
  
  # Function returns:
  return(list(Harvest=Hv,FR=FR.1,failed=fk,Genetics=Gx))
}




################ Strategy 6 ENDS HERE
####################################################################################################



# Strategy 7: reinforcement ------------------------------
# CHECKED
# Harvest: 1) Harvest any site for Nj=40 (max.Hv juv females) where N.est[t]>minN.Hv and N.est[t]/K[fed]>0.75
# Release: 1) Prioritise existing sites until N/K>0.75
#          2) Unoccupied sites where K.fed>45, ranked by K.unfed
# Feeding: 1) New sites over release years (except remote islands) until N>40
#          2) Sites with >7 years: reduce to visitor experience feeders when demand low
#          3) Restart when N drops by drop.p% or <40 (smallest)

strategy.7.rules <<- function(yr=y,ns=n.sites,sn=site.names,st=site.type,sa=site.ages,sx=site.access,fl=feed.levels,
                              N.est=N.est,Hv=Harvest,FR.1=FR,fk=failed,Gx=Genetics,
                              Ke=K.est,Km=K.managed,Kt=K.true,
                              max.Hv=max.h,fk.cut=fail.cut,ku.cut.1=ku.cut,kf.cut.1=kf.cut,feed.cut.1=feed.cut,
                              minN.Hv=minN.h,N.top.1=N.top,drop.p.1=drop.p,drop.N.1=drop.N
){
  
  hh <- rr <- ff <- gg <- rep(0,ns) 

  # Potential sources (never Hauturu)
  hh[sn!="Hauturu"&N.est[yr+1,]>minN.Hv&(N.est[yr+1,]+Hv[yr+1,])/Ke[yr+1,,fl]>0.75] <- 1
 
  # FIRST PRIORITY: original occupied sites less than 75% K.fed full
  cond.1 <- which(sn!="Hauturu"&N.est[1,]>1&(N.est[yr+1,]+Hv[yr+1,])>1&(N.est[yr+1,]+Hv[yr+1,])/Ke[yr+1,,fl]<0.75)
  # SECOND PRIORITY: new occupied sites less than 75% K.unfed full
  cond.2 <- which(sn!="Hauturu"&N.est[1,]==0&(N.est[yr+1,]+Hv[yr+1,])>1&(N.est[yr+1,]+Hv[yr+1,])/Ke[yr+1,,1]<0.75)
  # THIRD PRIORITY: Unoccupied sites with K.fed>40, ranked by K.unfed
  cond.3 <- which(sn!="Hauturu"&N.est[yr+1,]+Hv[yr+1,]<1&fk[yr+1,]==0&Ke[yr+1,,fl]>40)

  # FIRST PRIORITY: rank by K.fed
  j=1
  while(sum(rr>0)<sum(hh)){ # If number of release sites matches that of potential sources, break
    rr[which(Ke[yr+1,,fl]==sort(Ke[yr+1,cond.1,fl],d=T)[j])] <- j
    if(sum(rr>0)==length(cond.1)){break} # When all sites with this priority level have been used, break
    j=j+1 # If there are still sites available with this level of priority, another round
  }
    
  # SECOND PRIORITY: rank by K.unfed
  j=max(rr)+1
  while(sum(rr>0)<sum(hh)){ # If number of release sites matches that of potential sources, break
    rr[which(Ke[yr+1,,1]==sort(Ke[yr+1,cond.2,1],d=T)[j-max(rr)])] <- j
    if(sum(rr>0)<sum(hh)|sum(rr>0)==length(cond.2)){break} # When all sites with this priority level have been used, break
    j=j+1 # If there are still sites available with this level of priority, another round
  }
  
  # THIRD PRIORITY: rank by K.unfed
  j=max(rr)+1
  while(sum(rr>0)<sum(hh)){ # If number of release sites matches that of potential sources, break
    rr[which(Ke[yr+1,,1]==sort(Ke[yr+1,cond.3,1],d=T)[j-max(rr)])] <- j
    if(sum(rr>0)<sum(hh)|sum(rr>0)==length(cond.3)){break} # When all sites with this priority level have been used, break
    j=j+1 # If there are still sites available with this level of priority, another round
  }

  # If number of sources exceeds number of available destinations, discard sources based on population sizes
  if(sum(hh)>sum(rr>0)){hh[!is.na(match(sn,names(sort(N.est[yr+1,hh==1])[1:(sum(hh)-sum(rr>0))])))] <- 0}
  
  # Allocate actual birds
  Hv[yr+2,] <- 0
  if(sum(hh)==0|sum(rr)==0) {Hv[yr+2,] <- 0} else{   # If there are no suitable sources or destinations, no harvest
    Hv[yr+2,hh==1] <- -max.Hv # Otherwise Harvest max.Hv juveniles from each source site
    birds.available <- -sum(Hv[yr+2,])     # How many of these birds actually become available for release?
    for(k in 1:max(rr)){     # Then allocate birds in order of priority
      Hv[yr+2,rr==k] <- min(max.Hv,birds.available-sum(Hv[yr+2,Hv[yr+2,]>0]))
      if(birds.available-sum(Hv[yr+2,Hv[yr+2,]>0])==0){break} # When all birds have been used, break
    } # for
  } # else 

  # If one or more of the release sites are on remote islands:
  if(sum(sx[Hv[yr+2,]>0]=="Remote")>0){
    Hv[yr+2,sn=="Hauturu"] <- -Hv[yr+2,sn[Hv[yr+2,]>0&sx=="Remote"]]/2 # Harvest half of those birds from Hauturu
    # Which sites do we take less birds from? The smallest ones of the harvest candidates (matching number of remote releases)
    put.back <- which(sn%in%names(sort(N.est[yr+1,Hv[yr+2,]<0&sn!="Hauturu"],d=F)[1:length(sn[Hv[yr+2,]>0&sx=="Remote"])]))
    Hv[yr+2,put.back] <- Hv[yr+2,put.back]/2 # Halve the harvest from the sources determined above (not the releases!)
  }
  
  # Genetic releases - every fifth year harvest Hauturu and allocate to the two occupied sites with the smallest N with no previous genetic imports
  if(((yr+2)%%5)==0){ # Plan two years before!
    gg[which(sa==sort(sa[sn!="Hauturu"&N.est[yr+1,]>1&colSums(Gx[1:(yr+1),])==0])[1])] <- 1
    gg[which(sa==sort(sa[sn!="Hauturu"&N.est[yr+1,]>1&colSums(Gx[1:(yr+1),])==0])[2])] <- 1 # If there is a second site
    Hv[yr+2,sn=="Hauturu"] <- Hv[yr+2,sn=="Hauturu"]-max(10,10*sum(gg)) # Harvest Hauturu - on top of birds above
    Hv[yr+2,sn!="Hauturu"] <- -Hv[yr+2,sn=="Hauturu"]/sum(gg)*gg[which(sn!="Hauturu")] # Release
  }
  Gx[yr+2,] <- gg # Track which sites have received genetic releases 
  
  ## FEED
  
  # Always at occupied original sites
  ff[N.est[1,]>1&N.est[yr+1,]+Hv[yr+1,]>1] <- 1 
  # Feed during release years if N<40 or N<K[unfed]
  ff[Hv[yr+1,]>1&N.est[yr+1,]+Hv[yr+1,]<min(feed.cut.1,Ke[yr+1,,1])] <- 1
  # Feed non-failed new sites that have dropped below N<40 or K.unfed, or by drop.p%
  ff[N.est[1,]==0&N.est[yr+1,]>1&fk[yr+1,]==0&(N.est[yr+1,]+Hv[yr+1,]<min(feed.cut.1,Ke[yr+1,,1])|((N.est[yr+1,]+Hv[yr+1,])/N.est[yr,])<drop.p.1)] <- 1
  ff[sx=="Remote"] <- 0  # Never Hauturu, never remote islands 
  # Convert into actual feeding decisions
  FR.1[yr+1,] <- ff+1
  
  # Function returns:
  return(list(Harvest=Hv,FR=FR.1,failed=fk,Genetics=Gx))
}
################ Strategy 7 ENDS HERE
####################################################################################################


# NIMBLE model ------------------------------
hihiCode <- nimbleCode({
  
  # LIKELIHOOD
  for(i in 1:n.sites){
    
    # Model count observations 
    for(t in 1:n.years){n.counts[t,i] ~ dbin(p[t,i],N[t,i])}
    
    # Population size in year 1 
    N[1,i] ~ dpois(N.init[i])
    
    # Population sizes in subsequent years
    for(t in 1:(n.years-1)){
      
      # Actual numbers of birds   
      received[t,i] ~ dbin(post.rel[i],releases[t,i]) # Extra mortality releases[t,i]
      N.real[t,i] <- N[t,i] + received[t,i] + removed[t,i] # Extra step to avoid sampler issues
      
      # Growth and crash dynamics    
      growth.temp[t,i] <- N.real[t,i] * r.max[t,i] * (1-N.real[t,i]/K[t,i])
      overshoot[t,i] <- step(N.real[t,i]+growth.temp[t,i]) # If overshoot too much, collapse to extinction   
      lambda[t,i] <- (N.real[t,i] + growth.temp[t,i])*overshoot[t,i] 
      K[t,i] <- K.mn[Feeding[t,i],i]
      N[t+1,i] ~ dpois(lambda[t,i])
      # Annual variance on r.max
      r.max[t,i] ~ dnorm(mu.r,tau.r.process)
    }
  }
  
  # PRIORS
  # Informative priors on p(detection) based on IPM-derived posteriors
  for(i in 1:n.sites){
    for(t in 1:n.years){
      logit(p[t,i]) <- a.p[i] + re.p[t,i] # Mean p[site] plus annual variability
      re.p[t,i] ~ dnorm(0,re.p.tau[i])
    }
    a.p[i] ~ dnorm(a.p.mu[i], a.p.tau[i]) # Priors for mean p[site]
    re.p.tau[i] <- pow(re.p.sd[i],-2) # Annual variability
    re.p.sd[i] ~ dnorm(sd.p.mu[i],sd.p.tau[i]) # Prior for annual variability in p
    post.rel[i] ~ dbeta(p.rel.alpha[i],p.rel.beta[i]) # Prior for post-release effect (site-specific)
  }
  # Prior for mean rmax 
  mu.r ~ dnorm(r.mu,r.tau)
  # Prior for variance of rmax 
  tau.r.process <- pow(sd.r.process,-2)
  sd.r.process ~ dunif(min.sd.r.process,max.sd.r.process)
  
  # PREDICTIONS
  # Predicted K for fed and unfed sites
  for(f in 1:2){
    for(i in 1:n.sites){
      K.mn[f,i] ~ T(dnorm(K.mn.prior[f,i],K.mn.tau.prior[f,i]),0.001,) # f=feeding levels (here two: yes/no)
    }
  }
})
################ NIMBLE MODEL ENDS HERE
####################################################################################################

# Additional functions ------------------------------

# Convert mean and variance to beta shape parameters
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

# Send email on sim completion
completion.email <- function(strategyr=strategy,N.1=N,n.sims.1=n.sims,n.outcomes.1=n.outcomes,
                             time.taken.par.1=time.taken.par,good.sims.1=good.sims){
  library(blastula);library(keyring)
  email <- compose_email(body = md(c(
    "
    ## Hello!

    ",
    paste0("The simulation for Strategy ", strategyr, " ran ", ifelse(n.outcomes.1==n.sims.1,"successfully.",
                                                                      paste0("unsuccessfully until iteration ", n.outcomes.1, "."))),
    
    "
    The total time taken was:", paste0(format(time.taken.par.1),"."),
    "
    The proportion of failed simulations was:", paste0(1-length(good.sims.1)/n.sims.1,"."))),
    
    header=md(paste0("Simulation ",ifelse(n.outcomes.1==n.sims.1,"successful","unsuccessful"))),
    footer = md("This email message was generated by the blastula package.")
  )
  smtp_send(email = email,subject=ifelse(n.outcomes.1==n.sims.1,"Simulation successful", "Simulation failed"),
            from = "stefano.canessa@hotmail.com",to = "canessa.stefano@ugent.be",credentials = creds_key("hotmail"))
}
####################################################################################################



# Multiplot function  ------------------------------

# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols),byrow=T)
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' Allows to add another scale 
#' 
#' @param new_aes character with the aesthetic for which new scales will be 
#' created
#'
new_scale <- function(new_aes) {
  structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
}

#' Convenient functions
new_scale_fill <- function() {
  new_scale("fill")
}

new_scale_color <- function() {
  new_scale("colour")
}

new_scale_colour <- function() {
  new_scale("colour")
}

#' Special behaviour of the "+" for adding a `new_aes` object
#' It changes the name of the aesthethic for the previous layers, appending
#' "_new" to them. 
ggplot_add.new_aes <- function(object, plot, object_name) {
  plot$layers <- lapply(plot$layers, bump_aes, new_aes = object)
  plot$scales$scales <- lapply(plot$scales$scales, bump_aes, new_aes = object)
  plot$labels <- bump_aes(plot$labels, new_aes = object)
  plot
}


bump_aes <- function(layer, new_aes) {
  UseMethod("bump_aes")
}

bump_aes.Scale <- function(layer, new_aes) {
  old_aes <- layer$aesthetics[remove_new(layer$aesthetics) %in% new_aes]
  new_aes <- paste0(old_aes, "_new")
  
  layer$aesthetics[layer$aesthetics %in% old_aes] <- new_aes
  
  if (is.character(layer$guide)) {
    layer$guide <- match.fun(paste("guide_", layer$guide, sep = ""))()
  }
  layer$guide$available_aes[layer$guide$available_aes %in% old_aes] <- new_aes
  layer
}

bump_aes.Layer <- function(layer, new_aes) {
  original_aes <- new_aes
  
  old_aes <- names(layer$mapping)[remove_new(names(layer$mapping)) %in% new_aes]
  new_aes <- paste0(old_aes, "_new")
  
  old_geom <- layer$geom
  
  old_setup <- old_geom$handle_na
  new_setup <- function(self, data, params) {
    colnames(data)[colnames(data) %in% new_aes] <- original_aes
    old_setup(data, params)
  }
  
  new_geom <- ggplot2::ggproto(paste0("New", class(old_geom)[1]), old_geom,
                               handle_na = new_setup)
  
  new_geom$default_aes <- change_name(new_geom$default_aes, old_aes, new_aes)
  new_geom$non_missing_aes <- change_name(new_geom$non_missing_aes, old_aes, new_aes)
  new_geom$required_aes <- change_name(new_geom$required_aes, old_aes, new_aes)
  new_geom$optional_aes <- change_name(new_geom$optional_aes, old_aes, new_aes)
  
  layer$geom <- new_geom
  
  old_stat <- layer$stat
  
  old_setup2 <- old_stat$handle_na
  new_setup <- function(self, data, params) {
    colnames(data)[colnames(data) %in% new_aes] <- original_aes
    old_setup2(data, params)
  }
  
  new_stat <- ggplot2::ggproto(paste0("New", class(old_stat)[1]), old_stat,
                               handle_na = new_setup)
  
  new_stat$default_aes <- change_name(new_stat$default_aes, old_aes, new_aes)
  new_stat$non_missing_aes <- change_name(new_stat$non_missing_aes, old_aes, new_aes)
  new_stat$required_aes <- change_name(new_stat$required_aes, old_aes, new_aes)
  new_stat$optional_aes <- change_name(new_stat$optional_aes, old_aes, new_aes)
  
  layer$stat <- new_stat
  
  layer$mapping <- change_name(layer$mapping, old_aes, new_aes)
  layer
}

bump_aes.list <- function(layer, new_aes) {
  old_aes <-  names(layer)[remove_new(names(layer)) %in% new_aes]
  new_aes <- paste0(old_aes, "_new")
  
  names(layer)[names(layer) %in% old_aes] <- new_aes
  layer
}

change_name <- function(list, old, new) {
  UseMethod("change_name")
}

change_name.character <- function(list, old, new) {
  list[list %in% old] <- new
  list
}

change_name.default <- function(list, old, new) {
  nam <- names(list)
  nam[nam %in% old] <- new
  names(list) <- nam
  list
}

change_name.NULL <- function(list, old, new) {
  NULL
}

remove_new <- function(aes) {
  stringi::stri_replace_all(aes, "", regex = "(_new)*")
}
################ Multiplot function ENDS HERE
####################################################################################################