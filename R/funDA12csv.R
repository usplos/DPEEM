funDA12csv <-
function()
{
  # convert the DA1 files to csv files
  # the DA1 files should be names as Sub*.DA1
  # the DA1 files should be in current working directory
  
  DA1filename = dir(pattern = 'Sub*')
  DA1filename = DA1filename[grep('*.DA1',DA1filename)]
  if(length(DA1filename) == 0)
  {cat('Error: there is no DA1 file\n')}else
  {
    for (ida1 in DA1filename) {
      icsv = paste(substr(ida1,1,nchar(ida1)-4),'.csv',sep = '')
      file.copy(ida1, icsv)
      cat(ida1,'is done\n')
    }
    cat('DA1 to csv convert is done!\n\n')
    
  }
}
