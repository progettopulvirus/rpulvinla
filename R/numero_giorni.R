#' Numero di giorni nel periodo di analisi
#' 
#' @description 
#' Il numero di giorni tra la data di inizio e di fine nel campo date del tibble di input
#' 
#' @param .x Un tibble con campo date che descrive la data delle osservazioni
#' 
#' @return Il numero di giorni del periodo di analisi
#' @export  
numero_giorni<-function(.x){ 
  
  if(missing(.x)) stop("Fornire il tibble .x")
  if(!nrow(.x))   stop("Il tibble .x e' vuoto") 
  
  grep("date",names(.x))->colonna
  if(length(colonna)!=1) stop("Verificare la colonna 'date'")
  
  seq.Date(from = as.Date(min(.x$date)),to=as.Date(max(.x$date)),by="day")->.y
  
  length(.y)
  
  
}