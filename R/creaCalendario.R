#' Tibble con colonne anno mese giorno 
#' 
#' Crea un tibble con le colonne yy (anno), mm (mese), dd (giorno) con yy che va dal primo gennaio di annoI al 31 dicembre di annoF
#' 
#' @param annoI anno di inizio del calendario
#' @param annoF anno di fine del calendario
#' 
#' @return a [tibble][tibble::tibble-package]
#' 
#' @examples 
#' creaCalendario(1961,1990)
#' 
#' @importFrom rlang .data 
#' 
#' @export
creaCalendario<-function(annoI,annoF){
  
  if(missing(annoI)) stop("Anno inizio mancante")
  if(missing(annoF)) stop("Anno fine mancante")
  
  as.integer(annoI)->annoI
  as.integer(annoF)->annoF
  
  stopifnot(annoI<=annoF)
  
  seq.Date(from=as.Date(glue::glue("{annoI}-01-01")),to=as.Date(glue::glue("{annoF}-12-31")),by="day")->yymmdd
  
  tibble::tibble(yymmdd=yymmdd) %>%
    tidyr::separate(.data$yymmdd,into=c("yy","mm","dd"),sep="-") %>%
    dplyr::mutate(yy=as.integer(.data$yy),mm=as.integer(.data$mm),dd=as.integer(.data$dd))
  
}