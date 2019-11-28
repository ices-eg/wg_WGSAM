# likelihood ratio statistics for testing the significanse of added model parameters
# and  Akaike's An Information Criterion

#  input

n.par.1<-91               # number of parameter in model 1
n.par.2<-87                # number of parameter in model 2
neg.like.1<- -175.477     # negative log likelihood, model 1 (taken from file sms.rep "objective function (negative log likelihood):"  or from sms.par file 
neg.like.2<- -167.324     # negative log likelihood, model 2

#################

# likelihood ratio statistics for testing the significanse of a change in  model parameters

n.free<-abs(n.par.1-n.par.2)

P.value<-1-pchisq(2*abs(neg.like.1-neg.like.2),n.free)
P.value
# a P.value <0.05 shows significant effect of changing the number of parameters

##############################################
#Akaike's An Information Criterion
#     Generic function calculating the Akaike information criterion for
#     one or several fitted model objects for which a log-likelihood
#     value can be obtained, according to the formula -2*log-likelihood
#     + k*npar, where npar represents the number of parameters in the
#     fitted model, and k = 2 for the usual AIC, or k = log(n) (n the
#     number of observations) for the so-called BIC or SBC (Schwarz's
#     Bayesian criterion).

k<-2
AIC1<-2*neg.like.1+k*n.par.1
AIC1

AIC2<-2*neg.like.2+k*n.par.2
AIC2
AIC1

