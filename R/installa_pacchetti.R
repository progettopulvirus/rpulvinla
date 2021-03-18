#' Funzione per installare i pacchetti degli inquinante
#' 
#' @description
#' La funzione installa il pacchetto dal repository github progettopulvirus
#' 
#' @param .nomePacchetto nome del pacche da installare
#' @param ... Altri parametri per la funzione install_github
#' 
#' @export
installa_pacchetto_pulvirus<-function(.nomePacchetto,...){
  if(missing(.nomePacchetto) || !is.character(.nomePacchetto)) stop("Passare un nome valido del pacchetto da installare")
  devtools::install_github(glue::glue("progettopulvirus/{.nomePacchetto}"),...)
  
  utils::packageVersion(.nomePacchetto)->qualeVersione
  message(glue::glue("Pacchetto {.nomePacchetto} installato con successo, versione {qualeVersione}"))
  
}