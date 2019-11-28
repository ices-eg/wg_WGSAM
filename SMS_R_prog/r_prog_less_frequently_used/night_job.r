
# remember first to remove data.path and rm statement from init.R

###################################

# log-normal size, log-normal noise
data.path<-file.path(root,"NS_4_7_MAC_logn_logn")
source(file.path(prog.path,"Init.R"))
source(file.path(prog.path,"do_a_full_sms_run.R"))


# log-normal size, log-normal noise, nBar
data.path<-file.path(root,"NS_4_7_MAC_logn_logn_nBar")
source(file.path(prog.path,"Init.R"))
source(file.path(prog.path,"do_a_full_sms_run.R"))


# log-normal size, Dirichlet noise
data.path<-file.path(root,"NS_4_7_MAC_logn_diri")
source(file.path(prog.path,"Init.R"))
source(file.path(prog.path,"do_a_full_sms_run.R"))

####################################


# no size, log-normal noise
data.path<-file.path(root,"NS_4_7_MAC_noSize_logn")
source(file.path(prog.path,"Init.R"))
source(file.path(prog.path,"do_a_full_sms_run.R"))


# no size, log-normal noise, nBar
data.path<-file.path(root,"NS_4_7_MAC_noSize_logn_nbar")
source(file.path(prog.path,"Init.R"))
source(file.path(prog.path,"do_a_full_sms_run.R"))


# no size, Dirichlet noise
data.path<-file.path(root,"NS_4_7_MAC_noSize_diri")
source(file.path(prog.path,"Init.R"))
source(file.path(prog.path,"do_a_full_sms_run.R"))

####################################


# beta5 (unconstraint parameters) size, log-normal noise
data.path<-file.path(root,"NS_4_7_MAC_beta5_logn")
source(file.path(prog.path,"Init.R"))
source(file.path(prog.path,"do_a_full_sms_run.R"))

# beta6 (unimodal) size, log-normal noise
data.path<-file.path(root,"NS_4_7_MAC_beta6_logn")
source(file.path(prog.path,"Init.R"))
source(file.path(prog.path,"do_a_full_sms_run.R"))

# beta5 (unconstraint parameters) size, Dirichlet noise
data.path<-file.path(root,"NS_4_7_MAC_beta5_diri")
source(file.path(prog.path,"Init.R"))
source(file.path(prog.path,"do_a_full_sms_run.R"))

# beta6 (unimodal) size, Dirichlet noise
data.path<-file.path(root,"NS_4_7_MAC_beta6_diri")
source(file.path(prog.path,"Init.R"))
source(file.path(prog.path,"do_a_full_sms_run.R"))

###################################

# gamma size, log-normal noise
data.path<-file.path(root,"NS_4_7_MAC_gamma_logn")
source(file.path(prog.path,"Init.R"))
source(file.path(prog.path,"do_a_full_sms_run.R"))

###################################################

data.path<-file.path(root,"Baltic_logn_Diri_77-86")
source(file.path(prog.path,"Init.R"))
source(file.path(prog.path,"do_a_full_sms_run.R"))

data.path<-file.path(root,"Baltic_logn_Diri_87-94")
source(file.path(prog.path,"Init.R"))
source(file.path(prog.path,"do_a_full_sms_run.R"))

data.path<-file.path(root,"Baltic_logn_Diri_81-83")
source(file.path(prog.path,"Init.R"))
source(file.path(prog.path,"do_a_full_sms_run.R"))

data.path<-file.path(root,"Baltic_logn_Diri_91-93")
source(file.path(prog.path,"Init.R"))
source(file.path(prog.path,"do_a_full_sms_run.R"))


data.path<-file.path(root,"Baltic_logn_logn_77-86")
source(file.path(prog.path,"Init.R"))
source(file.path(prog.path,"do_a_full_sms_run.R"))

data.path<-file.path(root,"Baltic_logn_logn_87-94")
source(file.path(prog.path,"Init.R"))
source(file.path(prog.path,"do_a_full_sms_run.R"))
