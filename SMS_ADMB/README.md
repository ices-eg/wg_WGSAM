# admb-SMS
ADMB code for the SMS and OP programs

## SMS 
SMS (Stochastic Multi Species model) is a fish stock assessment model in which includes estimation of predation mortalities from observation of catches, survey indices and stomach contents. Estimation of predation mortality is based on the theory for predation mortality as defined by Andersen and Ursin (1977) and Gislason and Helgason (1985). SMS is a “forward running” model that operates with a cho-sen number of time steps (e.g. quarters of the year).  The default SMS is a one-area model, but the model has options for spatial explicit predation mortality given a known stock distribution.

Model parameters are estimated using maximum likelihood (ML) technique. Uncertainties of the model parameters are estimated from the Hessian matrix and confidence limits of derived quantities like his-torical fishing mortalities and stock abundances are estimated from the parameter estimates and the delta-method. SMS can be used to for forecast scenarios and Management Strategy Evaluations, where fishing mortalities are estimated dynamically from Harvest Control Rules

## OP
OP (OPerating model) is a simpler version of SMS used for forecast scenarios and Management Strategy Evaluations, where fishing mortalities are estimated dynamically from Harvest Control Rules. OP can also be used as an operating model for MSE 
