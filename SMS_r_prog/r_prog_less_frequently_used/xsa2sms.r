### script for conversion of assessment files on 
#     the "Lowestoft format" to SMS formatted data
#     and running SMS

# First, you have to give a directory containing SMS for the specific species e.g. c:\SMS\Cod
# this is normally done in the init.R script setting the variables root and my.stock.dir
# submit the init.R script. You will get an error message - ignore it 
# Secondly, you can run this script in a stepwise manner

####################   step 1
# user options


################  sandeel #####
VPA.input.dir<-"XSA"            # directory name with VPA-input files. The directory must be a sub-directory in the SMS directory
XSA.path<-file.path(data.path,VPA.input.dir)  # path of the folder with VPA input data
index.name <- "index.dat"            # VPA index file
tuning.name<- "fleet.dat"          # Tuning index file
stock.name <- "sandeel"
plusgroup  <- 4                       # if no plusgroup set NA
Fbarmin    <- 1
Fbarmax    <- 2
N1         <- 10000000                   # guestimate of  mean stock numbers for the youngest age



###############  cod
VPA.input.dir<-"XSA-input"            # directory name with VPA-input files. The directory must be a sub-directory in the SMS directory
XSA.path<-file.path(data.path,VPA.input.dir)  # path of the folder with VPA input data
index.name <- "c2532A.ind"            # VPA index file
tuning.name<- "c2532Tun-standard.dat"          # Tuning index file
stock.name <- "Cod"
plusgroup  <- 8                       # if no plusgroup set NA
Fbarmin    <- 4
Fbarmax    <- 7
N1         <- 50000                   # guestimate of  mean stock numbers for the youngest age



# function call, do not change
XSA2SMS(SMS.path=data.path, XSA.path=file.path(data.path,VPA.input.dir),index.name=index.name,tuning.name=tuning.name,
                 stock.name=stock.name,plusgroup=plusgroup,Fbarmin=Fbarmin,Fbarmax=Fbarmax,N1=N1)



# help("FLSMS.control-class")

# SMS data (ASCII format) are now ready to run

FLSMS()

######################## step 2



# start to read SMS.dat into R
control<-read.FLSMS.control()

# change options as needed
control@test.output<-1
control@species.info[,"last-age-selec"]<-plusgroup-1

control@obj.func.weight[,"SSB/R"]<-0.1
control@min.catch.CV<-0.1
control@min.SR.CV<-0.1
control@catch.sep.year<-list(c(1966,1993))
control@catch.s2.group<-list(c(2,3,6))
control@SSB.R.year.first<-c(1987)
control@var.penalty.trunc<-c(1,2,1)

# write the control object on ASCII format for use by SMS
write.FLSMS.control(control,path=data.path,write.multi=FALSE ,nice=TRUE)

# read indices into FLR
index<-SMS2FLIndices(control)
for (i in index) {
 print(i@name)
 print(i@range)
 print(i@range.SMS)
}

#change for fleet 1
index[[1]]@range.SMS[["var.age.group"]]<-c(2,3)
index[[1]]@range.SMS[["q.age"]]<-3

# write the SMS ASCII fleet files, NOTE is is not the right output files
FLIndices2SMS(out.path=data.path,indices=index,control=control,fleet.inf="fleet_info2.dat",
                        fleet.index="fleet_catch2.in",fleet.name="fleet_names2.in")
                        
                        
# make SMS run using ASCII input files, this will produce ASCII output file summary.out
FLSMS(use.ASCII=TRUE)

# read input and output data from a SMS run into a FLSMSStock object
Stock<-SMS2FLStocks(sumfile="summary.out",control=control,read.input=T)

# Use FLR functionality
plot(Stock@catch)
plot(Stock@catch.n)
