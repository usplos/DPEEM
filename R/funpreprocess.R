funpreprocess <-
function(workdir, FDMax = 1000, FDMin = 80, ROIfilename, outputdir)
{
  # This function is aimed to tide the csv files of each subjects.
  # Files of all subjects will be tidied in to FTtotal.csv in which one line contains the basic information about one fixation point.
  # FTtotalA.csv will then be produced based on FTtotal.csv in which several variables is appended inclusing the final coordinate (xcoor + ycoor*160), whether it is the first-pass fixation, fixation duration, whether it is in the ROI, saccade direction from current fixation point.
  # FTtotalAS.csv will then be produced based on FTtotalA.csv in which fixations whose duration located within the limitation will be remained.
  # FTtotalASR.csv will then be produced based on FTtotalAS.csv in which only fixations locating in ROI are remained.
  # FTtotalASRpt.csv will then be produced based on FTtoalAS.csv in which another variable is appended which indicats how many times the current fixation point has passed the ROI if it is located in ROI.
  # FTtotalASRptReg.csv will finally be produced based on FTtotalASRptReg.csv in which another variable is appended which indicates whether the current saccade to ROIs is from right or left to ROIs.
  # workdir - a character string indicating where the csv files are
  # FDMax - numeric indicating the longest duration of fixation point you want to remain
  # FDMin - numeric indicating the shortest duration of fixation point you want to remain
  # ROIfilename - a character string indicating the name ROI location file
  # outputdir - a character string indicating where you want to put out the FTtotal*.csv files
  
  setwd(workdir)
  cat('Now preprocessing is done...\n')
  
  csvfilename = dir(pattern = "Sub*")
  csvfilename = csvfilename[grep('*.csv',csvfilename)]
  sub0 = c()
  item0 = c()
  cond0 = c()
  xcoor0 = c()
  ycoor0 = c()
  Tstart0 = c()
  Tend0 = c()
  
  for(i in csvfilename)
  {
    tempsubfile = read.csv(i, sep = ' ', header = F)
    
    for(j in 1:nrow(tempsubfile))
    {
      templine = tempsubfile[j,]
      templine = templine[,!is.na(templine)]
      if(ncol(templine) > 8 | templine[[3]] > 99)
      {
        if(templine[[3]] < 99)
        {
          numberfixation = (ncol(templine)-8)/4
          
          for(k in 1:numberfixation)
          {
            sub0 = c(sub0, substr(i,1,nchar(i)-4))
            cond0 = c(cond0, templine[[2]])
            item0 = c(item0, templine[[3]])
            xcoor0 = c(xcoor0, templine[[4*k+5]])
            ycoor0 = c(ycoor0, templine[[4*k+6]])
            Tstart0 = c(Tstart0, templine[[4*k+7]])
            Tend0 = c(Tend0, templine[[4*k+8]])
          }
        }
        else
        {
          numberfixation = ncol(templine)/4
          for(k in 1:numberfixation)
          {
            sub0 = c(sub0, substr(i,1,nchar(i)-4))
            cond0 = c(cond0, templine[j-1,][[2]])
            item0 = c(item0, tempsubfile[j-1,][[3]])
            xcoor0 = c(xcoor0, templine[[4*k-3]])
            ycoor0 = c(ycoor0, templine[[4*k-2]])
            Tstart0 = c(Tstart0, templine[[4*k-1]])
            Tend0 = c(Tend0, templine[[4*k]])
          }
        }
      } 
    }
    cat("\015")
    cat(i,' has been done!!',"\n")
  }
  
  FTtotal = data.frame(sub0,cond0,item0,xcoor0,ycoor0,Tstart0,Tend0, stringsAsFactors = F)
  naposition = which(is.na(FTtotal$cond0))
  for(i in naposition)
  {
    FTtotal$cond0[i] = FTtotal$cond0[i-1]
  }
  largeitemp = which(FTtotal$item0 > 99)
  for(i in largeitemp)
  {
    FTtotal$item0[i] = FTtotal$item0[i-1]
  }
  
  write.csv(FTtotal,paste(outputdir,'FTtotal.csv',sep = '/'),row.names = F, quote = F)
  FTtotal = read.csv(paste(outputdir,'FTtotal.csv',sep = '/'), stringsAsFactors = F)
  cat('FTtotal.csv has been produced','\n')
  
  
  FTtotal = within(FTtotal,{
    finalcoor = NA
    finalcoor = xcoor0+ycoor0*160
    finalcoor = as.integer(finalcoor)
  })
  
  subindex = unique(FTtotal$sub0)
  ffd0 = c()
  FFT = c()
  ROI0 = c()
  sacdir = c()
  checkffd = c()
  library(rio)
  ROI = import(ROIfilename)
  ROI[is.na(ROI),is.na(ROI)]=-1
  for(i in subindex)
  {
    ffd1 = length(ffd0)
    #cat(i,' is doing...............','\n')
    tempsub = FTtotal[FTtotal$sub0 %in% i,]
    itemindex = unique(tempsub$item0)
    for(j in itemindex)
    {
      ffd1 = length(ffd0)
      # n == 1
      tempitem = tempsub[tempsub$item0 %in% j,]
      
      xmax = 0
      if(nrow(tempitem) > 1)
      {
        templine1 = tempitem[1,]
        templine2 = tempitem[2,]
        xmax = templine1$finalcoor[[1]]
        #ffd0
        ffd0 = c(ffd0, T)
        #FFT
        FFT = c(FFT, templine1$Tend0[[1]] - templine1$Tstart0[[1]])
        #sacdir
        x1 = templine1$finalcoor[[1]]
        x2 = templine2$finalcoor[[1]]
        sacdir = c(sacdir, ifelse(x2 > x1, 'fore','back'))
        #ROI0
        temproi = ROI[ROI$item == templine1$item0 & ROI$condition == templine1$cond0,][3:ncol(ROI)]
        ROI0 = c(ROI0, ifelse(x1 %in% temproi, T, F))
        #2 - n
        fnumber = nrow(tempitem)
        if(fnumber>1)
        {
          for(k in 2:fnumber)
          {
            templinen = tempitem[k,]
            xn = templinen$finalcoor[[1]]
            #ffd0
            if(xn > xmax)
            {
              ffd0 = c(ffd0,T)
              xmax = xn
            }
            else
            {
              ffd0 = c(ffd0,F)
            }
            #FFT
            FFT = c(FFT, templinen$Tend0[[1]] - templinen$Tstart0[[1]])
            #sacdir
            if(k < fnumber)
            {
              templinenp1 = tempitem[k+1,]
              xp1 = templinenp1$finalcoor[[1]]
              sacdir = c(sacdir, ifelse(xp1 > xn, 'fore', 'back'))
            }
            else
            {
              sacdir = c(sacdir, "NO")
            }
            #ROI
            temproi = ROI[ROI$item == templinen$item0 & ROI$condition == templinen$cond0,][3:ncol(ROI)]
            ROI0 = c(ROI0, ifelse(xn %in% temproi, T, F))
          }
        }
      }
      checkffd = c(checkffd, length(ffd0) - ffd1)
    }
    
    cat(i,' has been done!!!!','\n')
  }
  
  FTtotalA = cbind(FTtotal, ffd0, FFT, ROI0, sacdir)
  write.csv(FTtotalA, paste(outputdir, 'FTtotalA.csv',sep = '/'), row.names = F, quote = F)
  cat('FTtotalA.csv has been produced','\n')
  
  
  FTtotalAS = FTtotalA[FTtotalA$FFT %in% FDMax:FDMin,]
  write.csv(FTtotalAS, paste(outputdir, 'FTtotalAS.csv', sep = '/'),row.names = F, quote = F)
  cat('FTtotalAS.csv has been produced','\n')
  
  
  FTtotalASR = FTtotalAS[FTtotalAS$ROI0 == T,]
  write.csv(FTtotalASR, paste(outputdir,'FTtotalASR.csv',sep = '/'), row.names = F, quote = F)
  cat('FTtotalASR.csv has been produced','\n')
  cat('Preprocessing has been done','\n\n')
  
  
  cat('Calculating the ROI passing times...\n')
  
  FTtotalAS = read.csv(paste(outputdir, 'FTtotalAS.csv', sep = '/'), stringsAsFactors = F)
  FTtotalASRpt = within(FTtotalAS,{
    passtimes = NA
    passtimes[ROI0 == F] = 0
  })
  subindex = unique(FTtotalAS$sub)
  passtimes = c()
  for(i in subindex)
  {
    tempsub = FTtotalASRpt[FTtotalASRpt$sub0 %in% i,]
    itemindex = unique(tempsub$item0)
    for(j in itemindex)
    {
      tempitem = tempsub[tempsub$item0 %in% j,]
      NAposition = which(is.na(tempitem$passtimes))
      if(length(NAposition) >0 )
      {
        passtimes = c(passtimes,1)
        count = 1
        if(length(NAposition) >1)
        {
          for(k in 2:length(NAposition))
          {
            if(NAposition[k]-NAposition[k-1] == 1)
            {
              passtimes = c(passtimes,count)
            }
            else
            {
              count = count+1
              passtimes = c(passtimes, count)
            }
          }
        }
      }
    }
    cat(i,' has been done!!!','\n')
  }
  FTtotalASRpt$passtimes[which(is.na(FTtotalASRpt$passtimes))] = passtimes
  write.csv(FTtotalASRpt, paste(outputdir, 'FTtotalASRpt.csv', sep = '/'), quote = F, row.names = F)
  cat('FTtotalASRpt.csv has been done','\n\n')
  
  cat('Calculating whether regression was existed...\n')
  
  FTtotalASRpt = read.csv(paste(outputdir,'FTtotalASRpt.csv', sep = '/'), stringsAsFactors = F)
  FTtotalASRptReg = within(FTtotalASRpt, {
    regressionfrom = NA
    regressionfrom[passtimes %in% 0] = 'None'
  })
  regression = c()
  subindex = unique(FTtotalASRpt$sub)
  for(i in subindex)
  {
    tempsub = FTtotalASRptReg[FTtotalASRptReg$sub0 %in% i,]
    itemindex = unique(tempsub$item0)
    for(j in itemindex)
    {
      tempitem = tempsub[tempsub$item0 %in% j,]
      NAposition = which(is.na(tempitem$regre))
      if(length(NAposition) > 0)
      {
        for(k in NAposition)
        {
          if(k==1)
          {
            regression = c(regression,'None')
          }
          else
          {
            if(tempitem$sacdir[[k-1]] %in% 'fore' & tempitem$ROI0[[k-1]] == F)
            {
              regression = c(regression,'Left')
            }
            else if(tempitem$sacdir[[k-1]] %in% 'back' & tempitem$ROI0[[k-1]] == F)
            {
              regression = c(regression,'Right')
            }
            else
            {
              regression = c(regression,regression[length(regression)])
            }
          }
        }
      }
    }
    cat(i,' has been done!!!','\n')
  }
  FTtotalASRptReg$regressionfrom[is.na(FTtotalASRptReg$regressionfrom)] = regression
  write.csv(FTtotalASRptReg, paste(outputdir, 'FTtotalASRptReg.csv', sep = '/'), row.names = F, quote = F)
  cat('FTtotalASRptReg.csv has been done','\n\n')
  
}
