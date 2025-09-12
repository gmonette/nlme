search()
library(spida2)
dd <- MathAchieve
warnings()
#
dpi <- pdInd(diag(4) + .2, cov = covind(4))
dpi
coef(dpi)

#
#
fit <- lme(MathAch ~ SES + Sex, dd, random = ~ 1 + SES + Sex |School)
summary(fit)
fit2 <- lme(MathAch ~ SES + Sex, dd, random = list(School = pdSymm(~ 1 + SES + Sex )))
summary(fit2)
AIC(fit, fit2)

fit3 <- lme(MathAch ~ SES + Sex, dd, random = list(School = pdSymm2(~ 1 + SES + Sex )))
summary(fit3)
AIC(fit, fit2, fit3)

cov <- covind(3, 8)

fit4 <- lme(MathAch ~ SES + Sex, dd, random = list(School = pdInd(~ 1 + SES + Sex, cov = cov )))
summary(fit4)
AIC(fit, fit2, fit3, fit4)

covi <- diag(3)


fit5<- lme(MathAch ~ SES + Sex, dd, random = list(School = pdInd(~ 1 + SES + Sex, cov = covi )))
summary(fit5)

fit6<- lme(MathAch ~ SES + Sex, dd, random = list(School = pdDiag(~ 1 + SES + Sex)))
summary(fit6)
AIC(fit5, fit6)

AIC(fit, fit2, fit3, fit4)

cov3 <- covind(3)
pdi <- pdInd(~1 + SES + Sex, cov=cov3, data = dd)

pdii <- initialize(list(School = pdi), data = dd)
class(pdii)









#' ---
#' title: "default.R"
#' # This is ~/.config/rstudio/templates/default.R
#' # pdf output  ~/.config/rstudio/templates/default.pdf
#' author: ""
#' date: "2025-03-24"
#' # minimalist xournalpp screen: 13.18 in x 7.375 in
#' geometry: top=.4in,left=.4in,right=.4in,bottom=.45in,paperwidth=6.59in,paperheight=3.682in
#' output: pdf_document
#' header-includes:
#'   - \usepackage{pdfpages}
#'   - \pagenumbering{arabic}
#' ---
#' \pagenumbering{gobble}
#' \pagenumbering{arabic}
#' \raggedright
#' \tableofcontents
#'
#' # Reading a Stata file
#'
#' rio doesn't do a good job here.
#'
library(haven)
dd <- read_dta('~/Courses_taken/2025_Discrimination_Research/session1.dta')


# Conversion of Stat (.dta) objects as read by 'haven::read_sta()'
conv <- function(object, ...) UseMethod("conv")
conv.haven_labelled <- function(object,...) haven::as_factor(object)
conv.default <- function(object,...) object
conv.data.frame <- function(object,...){
  object[] <- lapply(object, conv)
  as.data.frame(object)
}
dd <- conv(dd)
head(dd)
lapply(dd, class)
#'
#' # Raster pdf for fast plotting of very large vector graphics
#'
# install.packages('rasterpdf')
library(spida2)
library(rasterpdf)
# help(p=rasterpdf)
ls(2)
#'
system.time({
  file <- paste0("/tmp/tmp",paste0(sample(letters,16,T), collapse = ''), '.pdf')
  raster_pdf(file, width = 12, height = 6, res = 600)
  print(xqplot(dd, mfrow = c(2,3)))
  dev.off()
}
)
#' \includepdf[pages=-,pagecommand={}]{`r file`}
#'
#' # BUGS
#'
#' - 2025-07-18: R version 4.5.1 (2025-06-13),
#'   - `as.data.frame.matrix` has methods in different packages
#'     some of which produce weird results. Can use:
#'     `base:::as.data.frame.matrix` explicitly.
#'
#' # lme classes
#'
#' random:
#' list( id = pdBlocked(list(pdIdent(~control-1),pdSymm(~patient+years_post-1))
#' list( id = pdBlocked(list(pdIdent(~ 0 + control),pdSymm(~ 0 + patient + years_post))
#' 
#' 
#' weights:
#'  weights=varIdent(form = ~1 | gender)
#'  
#' correlation:
#'    correlation = corCompSymm(form = ~ 1 | id) 
#'    
#' SOMETHING ABOUT HAVING INTEGERS FROM 1 to ...
#'  
#' 
#' 
#' 
#'

ch <- function(A) {
  sv <- svd(A)
  object <- sv$v %*% (log(sv$d) *t(sv$v))
  value <- object[row(object) <= col(object)]
  value
}
chinv <- function(val) {
  N <- (sqrt(1+8*length(val)) -1)/2
  ret <- matrix(0,N,N)
  ret[row(ret) <= col(ret)] <- val
  ret + t(ret) - diag(ret)
}
ch(diag(4) + .3) %>% chinv



library(nlme)

dd <- spida2::hs
g <- spida2::ga
`%>%` <- magrittr::`%>%`

g(pdConstruct.pdInd)[1]

pdi <- pdInd(diag(4) + .2, cov = covind(4))
unclass(pdi)
coef(pdi)
solve(pdi) %*% pdi
as.matrix(pdi)
solve(pdi) %>% as.matrix -> A
A %*% (diag(4) + .2)

pdMatrix(pdi)
pdFactor(pdi) %>% matrix(4,4) -> R
R %*% t(R)
