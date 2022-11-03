# this script creates a "*.psv" file from the std file
#

# read SMS.par   
# but first you have to comment out (#) the line with 
# init_pref_size_ratio[1]:
#  1.00000000000

a<-scan(file.path(data.path,"sms.par"),comment.char = "#") 
head(a)
length(a)


b<-read.table(file.path(data.path,"par_exp.out"),comment.char = "#",header=T) 
head(b)
b<-b[1:length(a),]
b$parVal<-a
head(b)

bb<-subset(b,par=="F_q_ini")
minp<-min(bb$parNo)
maxp<-max(bb$parNo)

fq.inp<-scan(file.path(data.path,"F_q_ini.in"),comment.char = "#")
sum(fq.inp<0)

b[minp:maxp,'parval2']<-fq.inp

(subset(b,!is.na(parval2)))  

b<-subset(b,is.na(parval2) | parval2<0)
head(b)
b$parval2<-NULL

which(b$par=="init_season_overlap")

#b<-b[1:max(which(b$par=="init_season_overlap")),]
b<-b[1:max(which(b$par=="Stom_var")),]
tail(b,300)

parf<-as.vector(b$parVal)
length(parf)
# write the sms.psv file
zz <- file(file.path(data.path,"sms.psv"), "wb")
writeBin(as.integer(length(parf)),zz)
writeBin(as.double(parf),zz)
close(zz)
