dd <- MathAchieve

fit <- lme(MathAch ~ SES + Sex + Minority, dd, random = ~ 1 + SES + Sex | School ,
           control = list(msVerbose = T))
summary(fit)

fit2 <- lme(MathAch ~ SES + Sex + Minority, dd, random = list( School = pdInd(~ 1 + SES + Sex, cov = covind(3))),
            control = list(msVerbose = T))
summary(fit2)
AIC(fit, fit2)
anova(fit, fit2)

pdi <- pdInd(~ 1 + SES + Sex + Minority, cov = covind(4, 13), data = dd)
class(pdi)
attr(pdi, 'cov')

fit3 <- lme(MathAch ~ SES + Sex + Minority, dd, 
            # random = list( School = pdInd(~ 1 + SES + Sex + Minority, cov = covind(4))),
            random = list(School = pdi),
            control = list(msVerbose = T))
summary(fit3)
AIC(fit, fit2, fit3)
anova(fit, fit2, fit3)

cov4 <- covind(4)
cov4[3,4] <- 1
cov4
tcrossprod(cov4)

cov4 <- covind(4)
cov4[1,4] <-0
cov4[col(cov4)>row(cov4)] <- 1
cov4
cov4[3,4] <- 0
cov4
dd <- MathAchieve

fit4 <- lme(MathAch ~ SES + Sex + Minority, dd, random = list( School = pdInd(~ 1 + SES + Sex + Minority, cov = cov4)),
            control = list(msVerbose = T))

pdi <- pdInd(~1 + SES + Sex + Minority, data = dd, cov = cov4, value = diag(4))
attr(pdi, 'cov')
summary(fit4)
AIC(fit, fit2, fit3)
anova(fit, fit2, fit3)

fit5 <- lme(MathAch ~ SES + Sex + Minority, dd, random = list( School = pdInd(~ 1 + SES + Sex + Minority, cov = covind(4))),
            control = list(msVerbose = T))
summary(fit5)
 
cov6 <- covind(4)
cov6[1,4] <- 0

cov7 <- diag(4)
cov7[col(cov7) > row(cov7)] <- 1
cov7

system.time(
fit6 <- lme(MathAch ~ SES + Sex + Minority, dd, random = list( School = pdInd(~ 1 + SES + Sex + Minority, cov = cov7)),
            control = list(msVerbose = T))
)
summary(fit6)

system.time(
  fit7 <- lme(MathAch ~ SES + Sex + Minority, dd, random = list( School = pdSymm(~ 1 + SES + Sex + Minority)),
              control = list(msVerbose = T))
)

system.time(
  fit8 <- lme(MathAch ~ SES + Sex + Minority, dd, random = ~ 1 + SES + Sex + Minority| School,
              control = list(msVerbose = T))
)

AIC(fit6, fit7,fit8)
library(car)
compareCoefs(fit6, fit7, fit8)


debug(nlme:::pdConstruct.pdInd)

search()
library(spida2)
library(nlme)
dd <- MathAchieve
warnings()
#
covindL <- function(n, ...) {
  ret <- covind(n,...)
  diag(ret) <- 0
  ret > 0
}
{
  V <- diag(4)
  V[1,] <- V[1,] + .1
  V[,1] <- V[,1] + .1
  V
}
dpi <- pdInd(V, cov = covind(4, 13))
dpi

V <- diag(4)
V[c(2,3)] <- 1
V <- V + t(V)
V

dpm <- pdMat(V)
dpm %>% unclass
coef(dpm)
copdFactor(dpm) %>% matrix(4,4) %>% crossprod %>% zapsmall
pdFactor(dpm) %>% matrix(4,4) 
coef(dpm) %>% matrix(4,4)

debug(nlme:::pdConstruct.pdInd)

debug(nlme:::pdFactor.pdInd)
dpi <- pdInd(V, cov = covind(4))
class(dpi)
unclass(dpi)
class(dpi) <- c('pdInd','pdMat')
%>% matrix(4,4) %>% crossprod %>% zapsmall
pdFactor(dpi)
coef(dpi)
pdMatrix(dpi,factor=TRUE)


zz <- dpi %>% unclass
Rmat <- diag(4)
diag(Rmat) <- exp(zz[1:4])
debug(nlme:::pdConstruct.pdInd)
Rmat^2
ddi <- pdDiag(diag(4) + .2)
Rmat[covindL(4)] <- zz[5:7]
crossprod(t(Rmat))

pdFactor(ddi)
pdMatrix(ddi)
coef(ddi) %>% exp %>% {.^2}


dpi
chol(dpi)
attr(dpi, 'cov')
zc <- coef(dpi)
A <- diag(4)

diag(A) <- exp(zc[1:4])
A[attr(dpi,'cov')] <- zc[5:7]
A
xx <- pdFactor(dpi) %>% matrix(4,4)
crossprod(A)
A[co]
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
