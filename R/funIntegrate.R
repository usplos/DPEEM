funIntegrate <-
function(workdir = getwd(),
                        outputdir = workdir,
                        FDMax, FDMin,
                        ROIfilename1,
                        DA1_to_csv = T,
                        preprocess = T,
                        TTFFD = T,
                        FTnum = T,
                        GazeDuration = T,
                        Regression = T,
                        SaccadeLength = T,
                        SecondPassTime = T,
                        FixationProportion = T,
                        DataIntegrate = T)
{
  # integrate the DA1 converting to csv, preprocessing and measures extracting
  # rio package is need, but dont worry about it because this function will check whether this package has been downloaded and will download it if not.
  # workdir - a character string indicating where the files you wang to use first are, defaultly the current working directory
  # outputdir - a character string indicating where you want to put out all the preprocessing and result files, can be a vector if you have more than one ROI
  # FDMax - numeric indicating the longest duration of fixation point you want to remain
  # FDMin - numeric indicating the shortest duration of fixation point you want to remain
  # ROIfilename1 - a character string indicating the name of the ROI location file (can be a vector if you have more than one ROI)
  # DA1_to_csv - logical indicating whether to convert the DA1 files
  # preprocess - logical indicating whether to preprocess
  # TTFFD - logical indicating whether to calculating total time and first fixation duration in ROI
  # FTnum - logical indicating whether to calculating the fixation numbers and its proportion in ROI
  # GazeDuration - logical indicating whether to calculating the gaze duration through ROI
  # Regression - logical indicating whether to calculating data regarding regression in ROI
  # SaccadeLength - logical indicating whether to calculating the saccade length to and from ROI
  # SecondPassTime - logical indicating whether to calculating the second pass time through ROI
  # FixationProportion - logical indicating whether to calculating the fixation proportion when passing ROI first time
  # DataIntegrate - logical whether to integrate all the result files into one result file
  
  # check whether the rio package has been downloaded
  if(sum(unique(installed.packages()[,c('Package')] %in% 'rio')) == 0)
  {install.packages('rio')}
  library(rio)
  if(DA1_to_csv == T)
  {
    funDA12csv()
  }
  if(preprocess == T)
  {
    for(i in outputdir)
    {
      funpreprocess(workdir, outputdir = i, FDMax = FDMax, FDMin = FDMin, ROIfilename = ROIfilename1[which(outputdir %in% i)])
      funROIRpt(i)
      funROIRptReg(i)
    }
  }
  checkpreprocess = dir(pattern = 'FTtotalASRptReg.csv')
  if(length(checkpreprocess) == 1)
  {
    for(i in outputdir)
    {
      if(TTFFD == T)
      {
        funROITTFFD(i)
        cat('Total time and First fixation duration is done!\n\n')
        #print(i)
        
      }
      if(FTnum == T)
      {
        funFTROInum(i)
        cat('Number of fixation point in ROI is done!\n\n')
        #print(i)
      }
      if(GazeDuration == T)
      {
        funROIgazeduration(i)
        cat('Gaze duration is done\n\n')
        #print(i)
      }
      if(Regression == T)
      {
        funROIregressionin(i)
        funROIregressionout(i)
        cat('Regression on ROI is done\n\n')
        #print(i)
      }
      if(SaccadeLength == T)
      {
        funROIsaccadelength(i)
        cat('Saccade Length is done\n\n')
        #print(i)
      }
      if(SecondPassTime == T)
      {
        funROIsecondFT(i)
        cat('Second Pass Time is done\n\n')
        #print(i)
      }
      if(FixationProportion == T)
      {
        funROIfixationprop(i)
        cat('Fixation Proportion is done!\n\n')
        #print(i)
      }
      if(DataIntegrate == T)
      {
        
        setwd(i)
        datafilename = dir(pattern = 'ROI[a-zA-Z]')
        Totalposition = grep('ROI[Tt]otal.csv',datafilename)
        if(length(Totalposition) > 0)
        {datafilename = datafilename[-1*Totalposition]}
        if(length(datafilename) == 0)
        {cat('Error: there is no data file!')}
        if(length(datafilename) == 1)
        {cat('There is only one file, so there is no need to integrate')}
        if(length(datafilename) > 1)
        {
          ROItotal = import(datafilename[1])
          for (ii in 2:length(datafilename)) {
            ROItotal = merge(ROItotal,import(datafilename[ii]), by = c('Sub','Item','Cond'))
          }
          export(ROItotal, paste(i, 'ROITotal.csv', sep = '/'))
          cat('Data Integrate is done!\n\n')
          
        }
      }
      cat('Congratulations!!!\n\n')
    }
  }else
  {cat('\nError:You have not done the preprocess analysis, please be sure that you have done that!\n\n')}
}
