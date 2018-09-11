\name{funpreprocess}
\alias{funpreprocess}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{tide the csv files of each subjects.
}
\description{
see title}
\usage{
funpreprocess(workdir, FDMax = 1000, FDMin = 80, ROIfilename, outputdir)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{workdir}{
a character string indicating where the csv files are
}
  \item{FDMax}{
numeric indicating the longest duration of fixation point you want to remain
}
  \item{FDMin}{
numeric indicating the shortest duration of fixation point you want to remain
}
  \item{ROIfilename}{
a character string indicating the name ROI location file
}
  \item{outputdir}{
a character string indicating where you want to put out the FTtotal*.csv files
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
FTtotalA.csv will then be produced based on FTtotal.csv in which several variables is appended inclusing the final coordinate (xcoor + ycoor*160), whether it is the first-pass fixation, fixation duration, whether it is in the ROI, saccade direction from current fixation point.
FTtotalAS.csv will then be produced based on FTtotalA.csv in which fixations whose duration located within the limitation will be remained.
FTtotalASR.csv will then be produced based on FTtotalAS.csv in which only fixations locating in ROI are remained.
FTtotalASRpt.csv will then be produced based on FTtoalAS.csv in which another variable is appended which indicats how many times the current fixation point has passed the ROI if it is located in ROI.
FTtotalASRptReg.csv will finally be produced based on FTtotalASRptReg.csv in which another variable is appended which indicates whether the current saccade to ROIs is from right or left to ROIs.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Zhang Guangyao
}
\keyword{funpreprocess}% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
