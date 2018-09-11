funFTROInum <-
function(outputdir)
{
  # calculate the fixation numbers and its proportion in ROI for each trial on each prticipant
  # based on FTtotalAS.csv
  # outputdir - a character string indicating where you want to put out the result file
  # FTtotalAS.csv should be in current working directory
  
  cat('Calculating the number of fixation potint in ROI...\n')
  
  FTtotalAS = read.csv(paste(outputdir, 'FTtotalAS.csv', sep = '/'), stringsAsFactors = F)
  Sub = c()
  Item = c()
  Cond = c()
  ROIFixationnum = c()
  ROIFixationprop = c()
  subindex = unique(FTtotalAS$sub)
  for(i in subindex)
  {
    tempsub = FTtotalAS[FTtotalAS$sub0 %in% i,]
    itemindex = unique(tempsub$item0)
    for(j in itemindex)
    {
      tempitem = tempsub[tempsub$item0 %in% j,]
      Sub=c(Sub,i)
      Item = c(Item,j)
      Cond = c(Cond, unique(tempitem$cond0)[[1]])
      tempitem2 = tempitem[tempitem$ROI0 == T,]
      ROIFixationnum = c(ROIFixationnum, nrow(tempitem2))
      ROIFixationprop = c(ROIFixationprop, nrow(tempitem2)/nrow(tempitem))
    }
    cat(i,' has been done','\n')
  }
  FTROInum = data.frame(Sub, Item, Cond, ROIFixationnum, ROIFixationprop)
  write.csv(FTROInum,paste(outputdir,'ROIFTnum.csv',sep = '/'),row.names = F,quote = F)
  cat('FTROInum.csv has been produced','\n\n')
  
}
