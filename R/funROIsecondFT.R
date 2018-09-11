funROIsecondFT <-
function(outputdir)
{
  # calculate the duration of the second, third and four times passing the ROI
  # based on FTtotalASRepReg.csv
  # outputdir - a character string indicating where you want to put out the result file
  # FTtotalASRptReg.csv should be in current working directory
  
  cat('Calculating the second pass time...\n')
  
  FTtotalASRptReg = read.csv(paste(outputdir, 'FTtotalASRptReg.csv', sep = '/'), stringsAsFactors = F)
  Sub = c()
  Item = c()
  Cond = c()
  SecondFT2 = c()
  SecondFT3 = c()
  SecondFT4 = c()
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
      tempsecFT = tempitem[tempitem$passtimes %in% 2,]
      if(is.na(tempsecFT$FFT[1]))
      {
        SecondFT2 = c(SecondFT2,0)
      }
      else
      {
        SecondFT2 = c(SecondFT2, sum(tempsecFT$FFT))
      }
      
      tempsecFT = tempitem[tempitem$passtimes %in% 3,]
      if(is.na(tempsecFT$FFT[1]))
      {
        SecondFT3 = c(SecondFT3,0)
      }
      else
      {
        SecondFT3 = c(SecondFT3, sum(tempsecFT$FFT))
      }
      
      tempsecFT = tempitem[tempitem$passtimes %in% 4,]
      if(is.na(tempsecFT$FFT[1]))
      {
        SecondFT4 = c(SecondFT4,0)
      }
      else
      {
        SecondFT4 = c(SecondFT4, sum(tempsecFT$FFT))
      }
    }
    cat(i,' has been done!!!','\n')
  }
  ROIsecondFT = data.frame(Sub, Item, Cond, SecondFT2, SecondFT3, SecondFT4, stringsAsFactors = F)
  write.csv(ROIsecondFT, paste(outputdir,'ROIsecondFT.csv', sep = '/'), row.names = F, quote = F)
  cat('ROI second fixation duration has been done','\n\n')
  
}
