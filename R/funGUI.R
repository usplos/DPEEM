funGUI <-
function()
{
  if(sum(unique(installed.packages()[,c('Package')] %in% 'tools')) == 0)
  {install.packages('tools')}
  if(sum(unique(installed.packages()[,c('Package')] %in% 'fgui')) == 0)
  {install.packages('fgui')}
  library(fgui)
  res = gui(funIntegrate,
            argOption = list(DA1_to_csv = c('T','F'), preprocess = c('T','F'), TTFFD = c('T','F'),
                             FTnum = c('T','F'), GazeDuration = c('T','F'),
                             Regression = c('T','F'), SaccadeLength = c('T','F'),
                             SecondPassTime = c('T','F'), FixationProportion = c('T','F'),
                             DataIntegrate = c('T','F')),
            #argEdit = list(FDMax = NULL, FDMin = NULL),
            title = 'DPEEM')
}
