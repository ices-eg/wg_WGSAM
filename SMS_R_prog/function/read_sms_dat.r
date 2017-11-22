#Read no. of species, years and seasons

Read.SMS.dat<-function(dir=data.path){
    info<<-scan(file.path(dir,"sms.dat"),comment.char = "#")
    i<-3
    years<<-info[i:(i+1)]
    i<-i+2
    nyr<<-years[2]-years[1]+1

    l.model.y<<-info[i]
    i<-i+1
    seasons<<-rep(1,2)
    seasons[2]<<-info[i]
    i<-i+1
    n.seasons<<-seasons[2]-seasons[1]+1
    last.season.last.year<<-info[i]
    i<-i+1
    nsp<<-info[i]    #no. of species
    i<-i+1
    fa<<-info[i]     #first age all species
    i<-i+1
    rec.season<<-info[i]
    i<-i+1
    maximum.age.all.species<<-info[i]
    i<-i+1           #counter in info file
    SP.settings<<-t(matrix(info[i:(i+nsp*8-1)],nrow=8,ncol=nsp))
    
    for (ii in (1:nsp))  if (SP.settings[ii,6]!=2) { first.VPA<<-ii; break;} #first VPA  species number
    for (ii in (1:nsp))  if (SP.settings[ii,6]==0) { npr<<-ii-1; break;}  #no. of predators
    
    i<-i+nsp*8
    beta.cor<<-info[i:(i+nsp-first.VPA)]

    i<-i+nsp-first.VPA+1
    SSB.R.first.year<<-info[i:(i+nsp-first.VPA)]
    SSB.R.first.year[SSB.R.first.year==-1]<<-years[1]

    i<-i+nsp-first.VPA+1
    SSB.R.last.year<<-info[i:(i+nsp-first.VPA)]
    SSB.R.last.year[SSB.R.last.year==-1]<<-years[2]

    SMS.control<<-read.FLSMS.control()

}
