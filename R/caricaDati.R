#' Carica i dati di un inquinante 
#' 
#' @description 
#' Questa funzione permette di caricare i dati di un determinato inquinante in un determinato pacchetto (regione), senza dover caricare il pacchetto. La funzione richiede
#' come parametri il nome della regione e dell'inquinante. Questa funzione e' pensata per facilitare il riutilizzo di uno stesso script su piu' regioni e su piu' inquinanti
#' 
#' @param pacchetto Nome del pacchetto, stringa di caratteri  
#' @param inquinante Nome dell'inquinante, stringa di caratteri
#' @param verbose Logical, info aggiuntive  
#' 
#' @return 
#' tibble con i dati richiesti
#' 
#' @example 
#' caricaDati(pacchetto="lazio",inquinante="pm10")->dati
#' 
#' @export
caricaDati<-function(pacchetto,inquinante,verbose=FALSE){ 
  
  if(missing(pacchetto)) stop("Nome del pacchetto dati mancante")
  if(missing(inquinante)) stop("Nome dell'inquinante mancante")
  
  if(!pacchetto %in% c("piemonte","valleaosta","lombardia","patrento","veneto","friuliveneziagiulia","liguria","emiliaromagna","toscana","umbria","marche","lazio","abruzzo","molise","campania","puglia","basilicata","calabria","sicilia","sardegna"))  stop(glue::glue("{pacchetto} non e' un nome pacchetto valido"))

  
  if(!inquinante %in% c("pm10", "pm2.5", "no2", "nox", "c6h6", "co", "o3_max_h_d", "o3_max_mm8h_d")) stop(glue::glue("{inquinante} non e' un nome inquinante valido"))

  #assegna i dati (non toccare)
  if(verbose) message(glue::glue("\n\nEseguo: {pacchetto}::{inquinante}\n\n"))
  eval(substitute(`::`(x,y),list(x=pacchetto,y=inquinante)))->dati
  
  if(!nrow(dati)) stop(glue::glue("Il data.frame {INQUINANTE} non e' disponibile nel pacchetto {REGIONE}"))
    
  dati
    
}#fine carica dati                                                                                            @                                                                                          