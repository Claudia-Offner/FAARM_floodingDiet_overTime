install.packages('mice')
library(mice)
library(INLABMA)

# https://becarioprecario.bitbucket.io/inla-gitbook/ch-missing.html#sec:missimput

data(nhanes2)
summary(nhanes2)

m1 <- inla(chl ~ 1 + bmi + age, data = nhanes2)
summary(m1)

#Generic variables for model fitting
d.mis <- nhanes2
idx.mis <- which(is.na(d.mis$bmi))
n.mis <- length(idx.mis)

#Fit linear model with R-INLA with a fixed beta
#d.mis: Dataset
#x.mis: Imputed values
fit.inla <- function(data, x.mis) {
  
  data$bmi[idx.mis] <- x.mis
  
  res <- inla(chl ~ 1 + bmi + age, data = data)
  
  return(list(mlik = res$mlik[1,1], model = res))
}

#Proposal x -> y
#density
dq.beta <- function(x, y, sigma = sqrt(10), log =TRUE) {
  res <- dnorm(y, mean = x, sd = sigma, log = log)
  
  if(log) {
    return(sum(res))
  } else {
    return(prod(res))
  }
}
#random
rq.beta <- function(x, sigma = sqrt(10) ) {
  rnorm(length(x), mean = x, sd = sigma)
}


#Prior for beta
prior.beta <- function(x, mu = mean(d.mis$bmi, na.rm = TRUE), 
                       sigma = 2*sd(d.mis$bmi, na.rm = TRUE), log = TRUE) {
  res <- dnorm(x, mean = mu, sd= sigma, log = log)
  
  if(log) {
    return(sum(res))
  } else {
    return(prod(res))
  }
}


# Set initial values to mean of bmi
d.init <- rep(mean(d.mis$bmi, na.rm = TRUE), n.mis)
#Run MCMC simulations
inlamh.res <- INLAMH(d.mis, fit.inla, d.init,
                     rq.beta, dq.beta, prior.beta, 
                     n.sim = 100, n.burnin = 50, n.thin = 10)


#Show results
x.sim <- do.call(rbind, inlamh.res$b.sim)
summary(x.sim)


nhanes2.models <- lapply(inlamh.res$model.sim, function(X) { X$model })
nhanes2.imp <- inla.merge(nhanes2.models, rep(1, length(nhanes2.models)))
summary(nhanes2.imp)


####################################

kidiq <- haven::read_dta("http://www.stat.columbia.edu/~gelman/arm/examples/child.iq/kidiq.dta")

kidiq100 <- kidiq %>% 
  mutate(mom_iq = mom_iq / 100,  # divid mom_iq by 100
         kid_score = kid_score / 100,   # divide kid_score by 100
         mom_iq_c = mom_iq - 1, 
         mom_hs = factor(mom_hs, labels = c("no", "yes"))) %>% 
  select(- mom_iq)

set.seed(1955)
kidiq100_mar <- ampute(kidiq100, prop = 0.5, 
                       pattern = data.frame(kid_score = c(0, 0, 0), 
                                            mom_hs = c(1, 1, 0), 
                                            mom_work = c(1, 1, 1), 
                                            mom_age = c(1, 1, 1), 
                                            mom_iq_c = c(1, 0, 1)), 
                       freq = c(.2, .4, .4), 
                       mech = "MAR")
kidiq100_mar <- kidiq100_mar$amp

##############################


# kidiq100_imp <- mice(kidiq100_mar, m = 20, maxit = 35,
#                      printFlag = FALSE)  # set to false only for knitting to Rmd

kidiq100_imp <- mice(df, m = 20, maxit = 35,
                     printFlag = FALSE)  # set to false only for knitting to Rmd

kidiq100_imp1 <- complete(kidiq100_imp, 1)

head(kidiq100_imp1, 10)

