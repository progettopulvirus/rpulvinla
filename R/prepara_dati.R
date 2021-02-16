#' Funzione per preparare i dati all'analisi del modello
#' 
#' @description 
#' La funzione restituisce il tibble .x con in piu' la variabile "banda", un intero che va da 1 al numero di osservazioni per il periodo
#' oggetto di analisi. La variabile "banda" e' necessaria per costruire il campo SPDE. Mettendo il parametro logaritmo=TRUE il tibble .x presenta in output la variabile lvalue (logaritmo di value). Il parametro
#' lockdown=TRUE restitusce nel tibble di output una colonna di nome "lockdown" pari a 0 per il periodo che va dall'inizio delle osservazioni alla data
#' fissata dal parametro "inizio_lockdown" e pari a 1 altrimenti.
#'    
#' @param .x Un tibble con i dati di input     
#' @param logaritmo Logical
#' @param lockdown Logical
#' @param inizio_lockdown Character o Logical nel formato %Y-%m-%d
#' 
#' @return a [tibble][tibble::tibble-package]
#' 
#' @importFrom rlang .data 
#' 
#' @export                                          
prepara_dati<-function(.x,logaritmo=TRUE,lockdown=TRUE,inizio_lockdown="2020-03-07"){
  
  purrr::walk(c("value","station_eu_code","yy","mm","dd","date"),.f=function(nomeColonna){
    grep(nomeColonna,names(.x))->colonna
    if(length(colonna)!=1) stop(glue::glue("Non trovo la colonna '{nomeColonna}'"))
  })
  
  if(missing(.x)) stop(".x mancante")
  if(!nrow(.x)) stop(".x e' vuoto!")
  
  if(!is.logical(logaritmo)) stop("logaritmo deve essere TRUE o FALSE")
  if(!is.logical(lockdown)) stop("lockdown deve essere TRUE o FALSE")
  
  
  #questo il periodo effettivo coperto dalle osservazioni in .x  
  min(.x$date)->firstDate
  max(.x$date)->lastDate  

  unique(.x$station_eu_code)->codici
  stopifnot(length(codici)>0)
  

  #Il calendario deve essere completo: calendario copre tutto il periodo da annoI ad annoF  
  creaCalendario(annoI=min(.x$yy),annoF=max(.x$yy))->calendario
  dplyr::left_join(calendario,.x,by=c("yy"="yy","mm"="mm","dd"="dd"))->.x
  
  #adesso .x non ha buchi tra firstDate e lastDate, pero' contiene anche date per il periodo che va da lastDate fino al 31 dicembre di annoF
  #Devo togliere questo periodo in eccesso  
  .x %>% dplyr::filter(date<=lastDate)->.x
  
  #Questo codice serve ad assicurarci che tutte le stazioni abbiano gli stessi valori nella vriabile "banda", indipendentemente
  #dal fatto che vi possano essre dei dati mancanti  
  .x[,c("yy","mm","dd")]->.y
  
  .y[!duplicated(.y),] %>%
    dplyr::arrange(.data$yy,.data$mm,.data$dd)->.y
  
  #numeroRighe rappresenta il valore finale che avrà ciascuna stazione nella variabile "banda"  
  nrow(.y)->numeroRighe
  
  #finale e' un data.frame in cui tutte le stazioni (tutte le colonne) riportano  un intero che varia da 1:numeroRighe  
  purrr::map_dfc(codici,.f=function(.codice){
    
    tibble::tibble(nome=1:numeroRighe)->df
    names(df)<-.codice
    
    df
    
  })->finale
  
  dplyr::bind_cols(.y,finale)->finale
  
  finale %>%
    tidyr::gather(key="station_eu_code",value="banda",-.data$yy,-.data$mm,-.data$dd)->gfinale
  
  #associo a .x il campo "banda"  
  dplyr::left_join(.x,gfinale,by=c("yy"="yy","mm"="mm","dd"="dd","station_eu_code"="station_eu_code"))->gfinale
  
  
  if(logaritmo) gfinale$lvalue<-log(gfinale$value+0.1)
  
  if(lockdown){ 
    as.Date(inizio_lockdown)->inizio_lockdown
    gfinale$lockdown<-0

    if(firstDate>=inizio_lockdown){
      warning(glue::glue("Attenzione, il dataset copre un periodo un periodo oltre la data del {inizio_lockdown}, la variabile lockdown sara' tutta uguale a 1"))
    }else{ 
      which(gfinale$date>=inizio_lockdown)->righe
      gfinale[righe,]$lockdown<-1 
    }

  }
    
  gfinale
  
}#fine prepara dati