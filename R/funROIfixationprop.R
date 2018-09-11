funROIfixationprop <-
function(outputdir)
{
  # calculate whether the ROI was focused when subject passed it first time
  # based on FTtotalASRptReg.csv file
  # outputdir - a character string where you want to put out the result file
  # FTtotalASRptReg.csv should be in current working directory
  
  cat('Calculating the fixation proportion...\n')
  
  FTtotalASRptReg = read.csv(paste(outputdir, 'FTtotalASRptReg.csv', sep = '/'), stringsAsFactors = F)
  Sub = c()
  Item = c()
  Cond = c()
  FixationProp = c()
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
      FFTposition = which(tempitem$passtimes == 1)[1]
      if(is.na(FFTposition))
      {
        FixationProp = c(FixationProp,0)
      }
      else
      {
        if(max(tempitem$finalcoor[1:(FFTposition-1)]) < tempitem$finalcoor[FFTposition])
        {
          FixationProp = c(FixationProp,1)
        }
        else
        {
          FixationProp = c(FixationProp,0)
        }
      }
    }
    cat(i,'has been done!!!','\n')
  }
  ROIfixationprop = data.frame(Sub,Item,Cond,FixationProp, stringsAsFactors = F)
  write.csv(ROIfixationprop, paste(outputdir,'ROIfixationprop.csv', sep = '/'), row.names = F, quote = F)
  cat('ROI fixation proportion has been done','\n\n')
  
}
