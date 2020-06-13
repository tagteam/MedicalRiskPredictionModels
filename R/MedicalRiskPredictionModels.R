### MedicalRiskPredictionModels.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Jun 13 2020 (11:02) 
## Version: 
## Last-Updated: Jun 13 2020 (12:26) 
##           By: Thomas Alexander Gerds
##     Update #: 12
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
#' ivf 
#'
#' This is a computer modified synthetic version of the data of the in
#' vitro fertilization study by la Cour Freiesleben et al.
#' 
#' 
#' @name ivf
#' @docType data
#' @format A data frame with 276 observations on the following 11 variables.
#' \describe{
#' \item{\code{cyclelen}}{Cycle length} 
#' \item{\code{bmi}}{BMI}
#' \item{\code{weight}}{Body weight}
#' \item{\code{age}}{Age}
#' \item{\code{ant.foll}}{Number of antral follicles}
#' \item{\code{fsh}}{Hormon fsh}
#' \item{\code{smoking}}{Smoking status}
#' \item{\code{no.cig.d}}{Number of cigarettes per day}
#' \item{\code{ovolume}}{Ovarian volume}
#' \item{\code{ohss}}{Ovarian hyper stimulation syndrome: as integer}
#' \item{\code{ohss}}{Ovarian hyper stimulation syndrome: as character}
#' }
#' @references
#'
#' N. la Cour Freiesleben, T.A. Gerds, J.L. Forman, J.D. Silver, A. Nyboe
#' Andersen, and B. Popovic-Todorovic. Risk charts to identify low and
#' excessive responders among first-cycle ivf/icsi standard patients.
#' Reproductive Biomedicine Online, 22(1):50--58, 2011.
#'
#' @source
#' The data are computer modified synthetic data
#' @keywords datasets
#' @examples
#' data(ivf)
NULL

#' oc 
#'
#' This is a computer modified synthetic version of the data of the oral 
#' cancer study by 
#' 
#' 
#' @name oc
#' @docType data
#' @format A data frame with 1284 observations on the following 14 variables.
#' \describe{
#' \item{\code{survtime}}{survival time in months} 
#' \item{\code{survstatus}}{survival status}
#' \item{\code{grade}}{Tumor grade}
#' \item{\code{age}}{Age}
#' \item{\code{tumorthickness}}{Tumor thickness}
#' \item{gender}{gender}
#' \item{tobacco}{tobacco}
#' \item{deep}{deep}
#' \item{site}{site}
#' \item{race}{race}
#' \item{x.posnodes}{number of positive lymphnodes}
#' \item{tumormaxdimension}{tumormaxdimension}
#' \item{vascular}{vascular}
#' \item{Grade}{Grade}
#' }
#' @references
#'
#' 
#' @source
#' The data are computer modified synthetic data
#' @keywords datasets
#' @examples
#' data(oc)
NULL

#' as 
#'
#' This is a computer modified synthetic version of the data of the active 
#' surveillance study by Berg et al. 
#' 
#' 
#' @name as
#' @docType data
#' @format A data frame with 217 observations on the following 10 variables.
#' \describe{
#' \item{age5}{Age in years divided by 5}
#' \item{psa}{psa}
#' \item{ct1}{ct1}
#' \item{diaggs}{diaggs}
#' \item{ppb5}{ppb5}
#' \item{lmax}{lmax}
#' \item{asprog}{Progression status: 1= progression, 2= death without progression, 0 = censored}
#' \item{asprogtime}{Time on active surveillance}
#' \item{age}{Age in years}
#' \item{erg}{Biomarker erg status}
#' }
#' @references
#'
#' Kasper Drimer Berg, Ben Vainer, Frederik Birkebaek Thomsen, M Andreas
#' Roeder, Thomas Alexander Gerds, Birgitte Groenkaer Toft, Klaus Brasso, and
#' Peter Iversen. Erg protein expression in diagnostic specimens is associated
#' with increased risk of progression during active surveillance for prostate
#' cancer. European urology, 66(5):851--860, 2014.
#' 
#' @source
#' The data are computer modified synthetic data
#' @keywords datasets
#' @examples
#' data(as)
NULL

#' long 
#'
#' This is a computer generated data set for illustration purposes
#' 
#' @name long
#' @docType data
#' @format A data frame with 18 observations on the following 4 variables.
#' \describe{
#' \item{subject}{subject}
#' \item{psadate}{psadate}
#' \item{psa}{psa}
#' \item{psa.doublingstime}{psa doubling-time}
#' }
#'
#' 
#' @source
#' The data are computer modified synthetic data
#' @keywords datasets
#' @examples
#' data(long)
NULL

#' ttedata 
#'
#' This is a computer generated data set for illustration purposes
#' 
#' @name ttedata
#' @docType data
#' @format A data frame with 5 observations on the following 5 variables.
#' \describe{
#' \item{id}{id}
#' \item{af.date}{af date}
#' \item{death.date}{death date}
#' \item{stroke.date}{stroke date}
#' \item{lost.date}{end of followup date}
#' }
#'
#' 
#' @source
#' The data are computer modified synthetic data
#' @keywords datasets
#' @examples
#' data(ttedata)
NULL


######################################################################
### MedicalRiskPredictionModels.R ends here
