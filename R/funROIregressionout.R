funROIregressionout <-
function(outputdir)
{
  # calculate whether any regression was given out from ROI and whether any regression was given out from ROI first time passing
  # based on FTtotalASRptReg.csv file
  # outputdir - a character string indicating where you want to put out the result file
  # FTtotalASRptReg.csv should be in current working directory
  
  cat('Calculating the ROI regression out...\n')
  
  FTtotalASRptReg = read.csv(paste(outputdir, 'FTtotalASRptReg.csv', sep = '/'), stringsAsFactors = F)
  Sub = c()
  Item = c()
  Cond = c()
  Regressionout = c()
  FFregressionout = c()
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
      NAposition = which(tempitem$passtimes != 0)
      regout = 0
      if(length(NAposition)>0)
      {
        
        for(k in NAposition)
        {
          if(k < nrow(tempitem) & tempitem$passtimes[k+1] == 0 & tempitem$sacdir[k] %in% 'back')
          {
            regout = 1
          }
        }
      }
      Regressionout = c(Regressionout, regout)
      NAposition = which(tempitem$passtimes == 1)
      FTregout = 0
      if(length(NAposition) > 0)
      {
        for(k in NAposition)
        {
          if(k < nrow(tempitem) & tempitem$passtimes[k+1] == 0 & tempitem$sacdir[k] %in% 'back')
          {
            FTregout = 1
          }
        }
      }
      FFregressionout = c(FFregressionout, FTregout)
    }
    cat(i,'has been done!!!','\n')
  }
  ROIregressionout = data.frame(Sub, Item,Cond,Regressionout, FPregressionout = FFregressionout, stringsAsFactors = F)
  write.csv(ROIregressionout, paste(outputdir,'ROIregressionout.csv', sep = '/'), row.names = F, quote = F)
  cat('ROI regression out has been done','\n\n')
  
}
