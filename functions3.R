
MCDA <- function(datamatrix, Nvector, weights, most, least, piece1, piece2, piece3, topvar, variance) {
  
  xi=datamatrix/Nvector
  
  pvf <- function(x, most, least, piece1, piece2 , piece3) {
    partialval = as.numeric(most > least) * (as.numeric(x <= piece1)*(((x-least)/(piece1-least)) * 0.25) + as.numeric(x > piece1)*as.numeric(x <= piece2)*(0.25+(((x-piece1)/(piece2-piece1)) * 0.25)) + as.numeric(x > piece2)*as.numeric(x <= piece3)*(0.5+(((x-piece2)/(piece3-piece2)) * 0.25)) + as.numeric(x > piece3)*(0.75 + ((x-piece3)/(most-piece3)) * 0.25)) + as.numeric(most < least) * (as.numeric(x >= piece3)*(((x-least)/(piece3-least)) * 0.25) + as.numeric(x >= piece2)*as.numeric(x < piece3)*(0.25+(((x-piece3)/(piece2-piece3)) * 0.25)) + as.numeric(x >= piece1)*as.numeric(x < piece2)*(0.5+(((x-piece2)/(piece1-piece2)) * 0.25)) + as.numeric(x < piece1)*(0.75 + ((x-piece1)/(most-piece1)) * 0.25))
    
    return(partialval)}
  values=pvf(t(xi), most, least, piece1, piece2, piece3)
  # Utility score
  us <- function (v, w) { return (v * w)}
  
  data1= us(values,weights)
  
  pos = which.max(topvar)
  
  weightvec = matrix(c(rep((1-seq(0.1,1,by=0.1))/(nrow(values)-1),pos-1),seq(0.1,1,by=0.1),rep((1-seq(0.1,1,by=0.1))/(nrow(values)-1),(nrow(values)-pos))),nrow(values),ncol=10,byrow = T)
  
  weightvecsq = weightvec^2
  
  utilvar = matrix(1, nrow = nrow(xi), ncol = ncol(xi))
  
  #utilvar = xi*(1-xi) / Nvector
  for (i in 1:nrow(xi)) {
    for (j in 1:ncol(xi)) {
      utilvar[i,j] = var(sapply(X = rnorm(1000, mean = xi[i,j], sd = sqrt(variance[i,j])), FUN = pvf, most=most[j], least=least[j], piece1=piece1[j], piece2=piece2[j], piece3=piece3[j]))
    }
  }
  
  data2_t = t(values)%*%weightvec
  data2 = t(data2_t)
  
  data3_t = sqrt(utilvar%*%weightvecsq)
  
  data3 = t(data3_t)
  
  return(list(data1=data1,data2=data2,data3=data3))

}

pMCDA <- function(datamatrix, Nvector, Ntrt, Nendpts, a_in, b_in, alpha_in, beta_in, mu0_in, tau_in, variance, weights, most, least, piece1, piece2, piece3, contvars, binvars) {
  
  nsim=100000 # nb of simulations to obtain the posterior distributions

  a=datamatrix+a_in
  b=Nvector-datamatrix+b_in-a_in
  
  alpha = datamatrix+alpha_in
  beta = Nvector + beta_in
  
  sigma = Nvector/variance + 1/tau_in 
  
  mu = ((datamatrix / variance) + (mu0_in / tau_in))/sigma
  
  # Distribution of the parameters
  xi = array(0, c(nsim, Ntrt, Nendpts))
  for (i in 1:Ntrt) {
    for(j in 1:Nendpts) {
      if (binvars[j] == 1) {
        xi[,i,j]=rbeta(nsim, a[i,j], b[i,j])
      } else if (contvars[j] == 1) {
        xi[,i,j]=rnorm(nsim, mean = mu[j], sd = sqrt(sigma[j]))
      } else {
        xi[,i,j]=rgamma(nsim, shape = alpha[i,j], rate = beta[i,j])
      }
      
    }
  }
  
  rm(a,b)
  
  
  pvf <- function(x, most, least, piece1, piece2 , piece3) {
    partialval = as.numeric(most > least) * (as.numeric(x <= piece1)*(((x-least)/(piece1-least)) * 0.25) + as.numeric(x > piece1)*as.numeric(x <= piece2)*(0.25+(((x-piece1)/(piece2-piece1)) * 0.25)) + as.numeric(x > piece2)*as.numeric(x <= piece3)*(0.5+(((x-piece2)/(piece3-piece2)) * 0.25)) + as.numeric(x > piece3)*(0.75 + ((x-piece3)/(most-piece3)) * 0.25)) + as.numeric(most < least) * (as.numeric(x >= piece3)*(((x-least)/(piece3-least)) * 0.25) + as.numeric(x >= piece2)*as.numeric(x < piece3)*(0.25+(((x-piece3)/(piece2-piece3)) * 0.25)) + as.numeric(x >= piece1)*as.numeric(x < piece2)*(0.5+(((x-piece2)/(piece1-piece2)) * 0.25)) + as.numeric(x < piece1)*(0.75 + ((x-piece1)/(most-piece1)) * 0.25))
    
    return(partialval)}
  values = array(0, c(nsim, Nendpts, Ntrt))
  for (i in 1:nsim) {
    values[i,,]=pvf(t(xi[i,,]), most, least, piece1, piece2, piece3)
  }
  
  rm(xi)
  
  # Utility score
  us <- function (v, w) { return (t(v) %*% w)}
  
  uss=matrix(NA,Ntrt,nsim)
  
  difference=vector(length=nsim)
  
  for (i in 1:nsim) {
    uss[,i] = us(values[i,,], weights)
  }
  
  return(t(uss))
  
}



SMAA <- function(datamatrix, Nvector, Ntrt, Nendpts, a_in, b_in, alpha_in, beta_in, mu0_in, tau_in, variance, weights, most, least, piece1, piece2, piece3, contvars, binvars) {
  
  nsim=100000 # nb of simulations to obtain the posterior distributions
  
  a=datamatrix+a_in
  b=Nvector-datamatrix+b_in-a_in
  
  alpha = datamatrix+alpha_in
  beta = Nvector + beta_in
  
  sigma = Nvector/variance + 1/tau_in 
  
  mu = ((datamatrix / variance) + (mu0_in / tau_in))/sigma
  
  
  # Distribution of the parameters
  xi = array(0, c(nsim, Ntrt, Nendpts))
  for (i in 1:Ntrt) {
    for(j in 1:Nendpts) {
      if (binvars[j] == 1) {
        xi[,i,j]=rbeta(nsim, a[i,j], b[i,j])
      } else if (contvars[j] == 1) {
        xi[,i,j]=rnorm(nsim, mean = mu[j], sd = sqrt(sigma[j]))
      } else {
        xi[,i,j]=rgamma(nsim, shape = alpha[i,j], rate = beta[i,j])
      }
    }
  }
  
  rm(a,b)
  
  
  pvf <- function(x, most, least, piece1, piece2 , piece3) {
    partialval = as.numeric(most > least) * (as.numeric(x <= piece1)*(((x-least)/(piece1-least)) * 0.25) + as.numeric(x > piece1)*as.numeric(x <= piece2)*(0.25+(((x-piece1)/(piece2-piece1)) * 0.25)) + as.numeric(x > piece2)*as.numeric(x <= piece3)*(0.5+(((x-piece2)/(piece3-piece2)) * 0.25)) + as.numeric(x > piece3)*(0.75 + ((x-piece3)/(most-piece3)) * 0.25)) + as.numeric(most < least) * (as.numeric(x >= piece3)*(((x-least)/(piece3-least)) * 0.25) + as.numeric(x >= piece2)*as.numeric(x < piece3)*(0.25+(((x-piece3)/(piece2-piece3)) * 0.25)) + as.numeric(x >= piece1)*as.numeric(x < piece2)*(0.5+(((x-piece2)/(piece1-piece2)) * 0.25)) + as.numeric(x < piece1)*(0.75 + ((x-piece1)/(most-piece1)) * 0.25))
    
    return(partialval)}
  values = array(0, c(nsim, Nendpts, Ntrt))
  for (i in 1:nsim) {
    values[i,,]=pvf(t(xi[i,,]), most, least, piece1, piece2, piece3)
  }
  
  rm(xi)
  
  c = mergeConstraints(lowerRatioConstraint(length(weights),1,2,100/(weights[[2]][2])),upperRatioConstraint(length(weights),1,2,100/(weights[[2]][1])))
  for(j in 3:length(weights)) { c = mergeConstraints(c,lowerRatioConstraint(length(weights),1,j,100/(weights[[j]][2])),upperRatioConstraint(length(weights),1,j,100/(weights[[j]][1])) )}
  c = mergeConstraints(c,simplexConstraints(length(weights)))
  w = hitandrun(c, n.samples = nsim)
  
  # Utility score
  us <- function (v, w) { return (t(v) %*% w)}
  
  uss=matrix(NA,Ntrt,nsim)
  
  difference=vector(length=nsim)
  
  for (i in 1:nsim) {
    uss[,i] = us(values[i,,], w[i,])
  }
  
  return(t(uss))
  
}
