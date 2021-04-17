#' Funzione per preparare i dati all'analisi del modello
#' 
#' @description 
#' La funzione restituisce il tibble .x con in piu': 1) la variabile "banda", un intero che va da 1 al numero di osservazioni per il periodo
#' oggetto di analisi; la variabile Intercept. La variabile "banda" e' necessaria per costruire il campo SPDE. 
#' Settando il parametro previous a TRUE viene restituito inoutput un tibble con la colonna "pvalue" ovvero il valore dell'inquinante nel giorno precedente.
#' Mettendo il parametro logaritmo=TRUE il tibble .x presenta in output la variabile lvalue (logaritmo di value). Se previous e' uguale a TRUE viene restituita anche la colonna 
#' lpvalue ovvero il logaritmo del valore dell'inquinante nel giorno precedente. 
#' Il parametro lockdown=TRUE restitusce nel tibble di output una colonna di nome "lockdown" pari a 1 per il periodo compreso tra "inizio_lockdown" e "fine_lockdown" 
#' pari a 0 altrimenti. Impostando day a TRUE viene creato un duplicato della variabile banda (banda e' una variabile pensata per l'spde mentre
#' day puo' essere usata per uno smoother al di fuori dell'spde). Impostando wday a TRUE viene creata una variabile che assume valori da 1 a 7 a seconda del giorno della settimana.
#' Impostando week a TRUE viene creata una variabile che assume valori da 1 a numero delle settimane del periodo in esame.   
#' Impostando weekend a TRUE viene creata una variabile 0/1 per l'effetto fine settimana (solo domenica).   
#' @param .x Un tibble con i dati di input   
#' @param previous Logical  
#' @param logaritmo Logical
#' @param lockdown Logical
#' @param inizio_lockdown Character o Logical nel formato %Y-%m-%d
#' @param fine_lockdown Character o Logical nel formato %Y-%m-%d
#' @param day Logical 
#' @param wday Logical  
#' @param week Logical 
#' @param weekend Logical
#' 
#' @return a [tibble][tibble::tibble-package]
#' 
#' @importFrom rlang .data 
#' 
#' @export                                          
prepara_dati<-function(.x,previous=FALSE,
                       logaritmo=TRUE,
                       lockdown=TRUE,
                       inizio_lockdown="2020-03-09",
                       fine_lockdown="2020-05-03",
                       day=FALSE,
                       wday=FALSE,
                       week=FALSE,
                       weekend=FALSE){
  
  purrr::walk(c("value","station_eu_code","yy","mm","dd","date"),.f=function(nomeColonna){
    which(names(.x)==nomeColonna)->colonna
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
  
  #se voglio un trend sul giorno creo un duplicato di banda. banda mi serve per l'spde, mentre giorno la posso usare per uno smoother   
  if(day) gfinale$day<-gfinale$banda
  
  if(logaritmo) gfinale$lvalue<-log(gfinale$value+0.1)
  
  if(lockdown){ 
    as.Date(inizio_lockdown)->inizio_lockdown
    gfinale$lockdown<-0

    if(firstDate>=inizio_lockdown){
      warning(glue::glue("Attenzione, il dataset copre un periodo un periodo oltre la data del {inizio_lockdown}, la variabile lockdown sara' tutta uguale a 1"))
    }else{ 
      
      lastDate<-as.Date("2020-05-31")
      
      
      if(is.null(fine_lockdown)){
        fine_lockdown<-lastDate
      }else{
        
        as.Date(fine_lockdown)->fine_lockdown
        
      }

      if(inizio_lockdown > fine_lockdown){
        warning(paste0("Attenzione la data di inizio lockdown e' posteriore alla data di fine lockdown!"))
      }
      
      which(gfinale$date>=inizio_lockdown & gfinale$date<=fine_lockdown)->righe
      gfinale[righe,]$lockdown<-1 
    }

  }
  
  #creo la variabile pvalue con il valore dell'inquinante nel giorno precedente
  if(previous){
    
    
    gfinale[,c("station_eu_code","date","value")]->copia
    copia$date<-copia$date+1
    names(copia)[grep("value",names(copia))]<-"pvalue"

    dplyr::left_join(gfinale,copia,by=c("station_eu_code","date"))->gfinale
    rm(copia)
    if(logaritmo) gfinale$lpvalue<-log(gfinale$pvalue+0.1)
    
  }#fine previous
  
    
  #Intercetta  
  gfinale$Intercept<-1
  
  #wday
  if(wday) gfinale$wday<-lubridate::wday(gfinale$date,week_start = 1)
  
  #week  
  if(week) gfinale$week<-lubridate::isoweek(gfinale$date)
  
  #weekend
  if(weekend) gfinale$weekend<-as.numeric(lubridate::wday(gfinale$date,week_start = 1) %in% c(7))
  
  gfinale
  
}#fine prepara dati