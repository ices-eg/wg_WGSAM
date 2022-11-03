
###################################################
# stomach data  

if (is.null(stomMark))  f<-'stomcon_list.dat' else  f<-paste0('stomcon_list',stomMark,'.dat')
stopifnot(file.copy(from=file.path(stomDir,f),to=file.path(root,finalExchangeDir,f),overwrite=TRUE))

if (is.null(stomMark))  f<-'ALK_all_list.dat' else  f<-paste0('ALK_all_list',stomMark,'.dat')
stopifnot(file.copy(from=file.path(stomDir,f),to=file.path(root,finalExchangeDir,f),overwrite=TRUE))
file.exists(file.path(stomDir,f))

if (is.null(stomMark))  f<-'ALK_stom_list.dat' else  f<-paste0('ALK_stom_list',stomMark,'.dat')
stopifnot(file.copy(from=file.path(stomDir,f),to=file.path(root,finalExchangeDir,f),overwrite=TRUE))

