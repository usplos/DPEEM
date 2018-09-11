funROIgazeduration <-
function(outputdir)
{
  # calcualte the gaze duration on ROI
  # based on FTtotalASRptReg.csv file
  # outputdir - a character string indicating where you want to put out the result files
  # FTtotalASRptReg.csv should be in current working directory
  
  cat('Calculating the first pass time...\n')
  
  FTtotalASRptReg = read.csv(paste(outputdir,'FTtotalASRptReg.csv', sep = '/'), stringsAsFactors = F)
  Sub = c()
  Item = c()
  Cond = c()
  GazeDuration = c()
  subindex = unique(FTtotalASRptReg$sub)
  for(i in subindex)
  {
    tempsub = FTtotalASRptReg[FTtotalASRptReg$sub0 %in% i,]
    itemindex = unique(tempsub$item0)
    for(j in itemindex)
    {
      tempitem = tempsub[tempsub$item0 %in% j,]
      Sub=c(Sub,i)
      Item = c(Item,j)
      Cond = c(Cond, unique(tempitem$cond0)[[1]])
      tempsecFT = tempitem[tempitem$passtimes %in% 1,]
      if(is.na(tempsecFT$FFT[1]))
      {
        GazeDuration = c(GazeDuration,0)
      }
      else
      {
        GazeDuration = c(GazeDuration, sum(tempsecFT$FFT))
      }
    }
    cat(i,' has been done!!!','\n')
  }
  ROIgazeduration = data.frame(Sub, Item, Cond, GazeDuration, stringsAsFactors = F)
  write.csv(ROIgazeduration, paste(outputdir,'ROIgazeduration.csv', sep = '/'), row.names = F, quote = F)
  cat('ROI gaze duration has been done','\n\n')
  
}
