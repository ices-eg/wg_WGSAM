# batch job for SMS runs
# remember first  to make the run_do.bat files 


dirs<-  c("NS_paper_single","NS_paper_uniform","NS_paper_uniform_confined","NS_paper_size","NS_paper_size_fixed","NS_paper_size_mesh","NS_paper_test")       # directories with output to compare
 
for (dir in dirs) {
  if ( file.access(file.path(root,dir,"run_do.bat"), mode = 0)!=0)  stop(paste('Directory',dir, " or file run_do.bat",'does not exist'))
} 
 
for (dir in dirs) {
   my.stock.dir<-dir
    command<-file.path( root,my.stock.dir,'run_do.bat')
    
    system(command,show.output.on.console =T)
}

