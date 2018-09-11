funROIsaccadelength <-
function(outputdir)
{
  # calculate the saccade length saccading in and out ROI when first time passing based on the direction of saccade from
  # based on FTtotalASRptReg.csv file
  # outputdir - a character string indicating where you want to put out the result file
  # FTtotalASRptReg.csv should be in current working directory
  
  cat('Calculating the saccade length...\n')
  
  FTtotalASRptReg = read.csv(paste(outputdir, 'FTtotalASRptReg.csv', sep = '/'), stringsAsFactors = F)
  Sub = c()
  Item = c()
  Cond = c()
  saccadelengthinL = c()
  saccadelengthinR = c()
  saccadelengthoutL = c()
  saccadelengthoutR = c()
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
      FFTposition0 = which(tempitem$passtimes == 1)
      if(length(FFTposition0)> 0)
      {
        FFTposition = FFTposition0[1]
        
        if(FFTposition > 1)
        {
          if(tempitem$sacdir[FFTposition-1] %in% 'fore')
          {saccadelengthinL = c(saccadelengthinL,sqrt((tempitem$xcoor0[FFTposition-1]-tempitem$xcoor0[FFTposition])^2+(tempitem$ycoor0[FFTposition-1]-tempitem$ycoor0[FFTposition])^2))}
          else
          {saccadelengthinL = c(saccadelengthinL,NA)}
        }else
        {saccadelengthinL = c(saccadelengthinL,NA)}
        
        if(FFTposition > 1)
        {
          if(tempitem$sacdir[FFTposition-1] %in% 'back')
          {saccadelengthinR = c(saccadelengthinR,sqrt((tempitem$xcoor0[FFTposition-1]-tempitem$xcoor0[FFTposition])^2+(tempitem$ycoor0[FFTposition-1]-tempitem$ycoor0[FFTposition])^2))}
          else
          {saccadelengthinR = c(saccadelengthinR,NA)}
        }else
        {saccadelengthinR = c(saccadelengthinR,NA)}
        
        FFTposition = FFTposition0[length(FFTposition0)]
        
        if(FFTposition < nrow(tempitem))
        {
          if(tempitem$sacdir[FFTposition] %in% 'fore')
          {saccadelengthoutR = c(saccadelengthoutR,sqrt((tempitem$xcoor0[FFTposition+1]-tempitem$xcoor0[FFTposition])^2+(tempitem$ycoor0[FFTposition+1]-tempitem$ycoor0[FFTposition])^2))}
          else
          {saccadelengthoutR = c(saccadelengthoutR,NA)}
        }else
        {saccadelengthoutR = c(saccadelengthoutR,NA)}
        
        if(FFTposition < nrow(tempitem))
        {
          if(tempitem$sacdir[FFTposition] %in% 'back')
          {saccadelengthoutL = c(saccadelengthoutL,sqrt((tempitem$xcoor0[FFTposition+1]-tempitem$xcoor0[FFTposition])^2+(tempitem$ycoor0[FFTposition+1]-tempitem$ycoor0[FFTposition])^2))}
          else
          {saccadelengthoutL = c(saccadelengthoutL,NA)}
        }else
        {saccadelengthoutL = c(saccadelengthoutL,NA)}
        
      }else
      {
        saccadelengthinL = c(saccadelengthinL,NA)
        saccadelengthinR = c(saccadelengthinR,NA)
        saccadelengthoutL = c(saccadelengthoutL, NA)
        saccadelengthoutR = c(saccadelengthoutR, NA)
      }
      
    }
    cat(i,'has been done!!!','\n')
  }
  ROIsaccadelength = data.frame(Sub, Item, Cond,saccadelengthinL,saccadelengthinR,saccadelengthoutL,saccadelengthoutR, stringsAsFactors = F)
  write.csv(ROIsaccadelength, paste(outputdir,'ROIsaccadelength.csv', sep = '/'), row.names = F, quote = F,na = '')
  cat('ROI saccade length is done\n\n')
  
}
