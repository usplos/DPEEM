funROITTFFD <-
function(outputdir)
{
  # calculate the first fixation duration and total time in ROI on each trial in each subject
  # based on FTtoalASR.csv
  # outputdir - a character string indicating where you want to put out the result file
  # FTtotalASR.csv should be in current working directory
  
  cat('Calculating the first fixation duration and total time...\n')
  
  FTtotalASR = read.csv(paste(outputdir, 'FTtotalASR.csv',sep = '/'), stringsAsFactors = F)
  sub0 = c()
  item0 = c()
  cond0 = c()
  totaltime0 = c()
  fft0 = c()
  subindex = unique(FTtotalASR$sub)
  for(i in subindex)
  {
    tempsub = FTtotalASR[FTtotalASR$sub0 %in% i,]
    itemindex = unique(tempsub$item0)
    for(j in itemindex)
    {
      tempitem = tempsub[tempsub$item0 %in% j,]
      sub0 = c(sub0, i)
      item0 = c(item0, j)
      cond0 = c(cond0, unique(tempitem$cond0)[[1]])
      totaltime0 = c(totaltime0, sum(tempitem$FFT))
      fft0 = c(fft0, ifelse(is.null(tempitem$FFT[[1]]),0,tempitem$FFT[[1]]))
    }
    cat(i,' has been done!!!!','\n')
  }
  totaltime_fft = data.frame(Sub = sub0, Item = item0, Cond = cond0, TotalTime = totaltime0, FFD = fft0, stringsAsFactors = F)
  write.csv(totaltime_fft, paste(outputdir,'ROItotaltime&fft.csv', sep = '/'), row.names = F, quote = F)
  cat('Totaltime and first fixation duration have been done','\n\n')
  
}
