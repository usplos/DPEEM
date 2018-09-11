\name{funIntegrate}
\alias{funIntegrate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{integrate the DA1 converting to csv, preprocessing and measures extracting}
\description{
integrate the DA1 converting to csv, preprocessing and measures extracting
}
\usage{
funIntegrate(workdir = getwd(), outputdir = workdir, FDMax, FDMin, ROIfilename1, DA1_to_csv = T, preprocess = T, TTFFD = T, FTnum = T, GazeDuration = T, Regression = T, SaccadeLength = T, SecondPassTime = T, FixationProportion = T, DataIntegrate = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{workdir}{
a character string indicating where the files you wang to use first are, defaultly the current working directory
}
  \item{outputdir}{
a character string indicating where you want to put out all the preprocessing and result files, can be a vector if you have more than one ROI
}
  \item{FDMax}{
numeric indicating the longest duration of fixation point you want to remain
}
  \item{FDMin}{
numeric indicating the shortest duration of fixation point you want to remain
}
  \item{ROIfilename1}{
a character string indicating the name of the ROI location file (can be a vector if you have more than one ROI)
}
  \item{DA1_to_csv}{
logical indicating whether to convert the DA1 files
}
  \item{preprocess}{
logical indicating whether to preprocess
}
  \item{TTFFD}{
logical indicating whether to calculating total time and first fixation duration in ROI
}
  \item{FTnum}{
logical indicating whether to calculating the fixation numbers and its proportion in ROI
}
  \item{GazeDuration}{
logical indicating whether to calculating the gaze duration through ROI
}
  \item{Regression}{
logical indicating whether to calculating data regarding regression in ROI
}
  \item{SaccadeLength}{
logical indicating whether to calculating the saccade length to and from ROI
}
  \item{SecondPassTime}{
logical indicating whether to calculating the second pass time through ROI
}
  \item{FixationProportion}{
logical indicating whether to calculating the fixation proportion when passing ROI first time
}
  \item{DataIntegrate}{
logical whether to integrate all the result files into one result file
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Preprocess files and results files will be generated according to your setting
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Zhang Guangyao
}

\keyword{funIntegrate}% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
