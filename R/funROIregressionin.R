funROIregressionin <-
function(outputdir)
{
  # calculate whether the ROI received regression and whether the first time landing in ROI was regression
  # based on FTtotalASRepReg.csv
  # outputdir - a character string indicating where you want put out the result file
  # FTtotalASRptReg.csv should be in current working directory
  
  cat('Calculating the ROI regression in...\n')
  
  FTtotalASRptReg = read.csv(paste(outputdir, 'FTtotalASRptReg.csv', sep = '/'), stringsAsFactors = F)
  Sub = c()
  Item = c()
  Cond = c()
  ReginRight = c()
  ReginRightFF = c()
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
      if(sum(tempitem$regressionfrom[tempitem$passtimes != 0] %in% 'Right') != 0)
      {
        ReginRight = c(ReginRight, 1)
      }
      else
      {
        ReginRight = c(ReginRight, 0)
      }
      if(sum(tempitem$regressionfrom[tempitem$passtimes == 1] %in% 'Right') !=0)
      {
        ReginRightFF = c(ReginRightFF, 1)
      }
      else
      {
        ReginRightFF = c(ReginRightFF,0)
      }
    }
    cat(i,' has been done!!!','\n')
  }
  ROIrightregressionin = data.frame(Sub, Item, Cond, ReginRight, ReginRightFF, stringsAsFactors = F)
  write.csv(ROIrightregressionin, paste(outputdir, 'ROIrightregressionIn.csv', sep = '/'), row.names = F, quote = F)
  cat('ROI regression has been done','\n\n')
  
}
