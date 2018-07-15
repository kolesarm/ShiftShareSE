#' Dataset from Autor, Dorn and Hanson (2013)
#'
#' Subset of ADH data.
#' @format A list with data frames. The first data frame,\code{r} has 1,444
#'     rows and 16 variables. The rows correspond to 722 commuting zones (CZ)
#'     over 2 time periods (1990-1999 and 2000-2007)
#'
#' \describe{
#' \item{d_sh_empl}{}
#' \item{d_sh_empl_mfg}{}
#' \item{d_sh_empl_nmfg}{}
#' \item{shock}{}
#' \item{IV}{}
#' \item{weights}{}
#' \item{statefip}{State FIPS code}
#' \item{czone}{Commuting zone number}
#' \item{t2}{Indicator for 2000-2007}
#' \item{l_shind_manuf_cbp}{}
#' \item{l_sh_popedu_c}{}
#' \item{l_sh_popfborn}{}
#' \item{l_sh_empl_f}{}
#' \item{l_sh_routine33}{}
#' \item{l_task_outsource}{}
#' \item{division}{US Census division of CZ}
#' }
#'
#' The second data frame, \code{s} has 775 rows and two columns, the first
#' corresponding to sectoral shocks and the second to SIC code. The third
#' object, \code{W}, is a weight matrix.
#'
#' @source David Dorn's website \url{http://ddorn.net/data.htm}
#' @references{
#'
#' \cite{Autor, David H., David Dorn, and Gordon H. Hanson, "The China syndrome:
#' Local labor market effects of import competition in the United States,"
#' American Economic Review, 2013, 103 (6), 2121–68.}
#'
#' }
"ADH"
