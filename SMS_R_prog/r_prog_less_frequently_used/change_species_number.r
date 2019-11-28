file<-file.path(data.path,'summary_table_raw.out')
a<-read.table(file,header=TRUE)

        #  NEW                                                   COD WHG HAD POK HER NSA SSA NOP SPR PLE SOL
        #  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23 ,24, 25, 26, 27, 28,
        #  old                                               COD WHG HAD POK HER SAN NOP SPR PLE SOL
        #  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23 ,24, 25, 26, 27, 28,
old.new<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 99, 24 ,25, 26, 27)

a$Species.n<- old.new[a$Species.n]

write.table(a,file=file,row.names=F)
