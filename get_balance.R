library(readr)

#' Calcular variables para los balances
#' 
#' Esta función utiliza variables registradas en la base de datos para calcular
#' otras variables utilizadas en los balances.
#'
#' @param db Data set con la base de datos
#' @param paciente Character vector con el nombre del paciente (como aparece
#'   en la base de datos)
#' @param fecha_hoy Date of today (the function uses the information from
#'   yesterday)
#' @return
#' @export
#'
#' @examples
#' dat <- readr::read_csv("data.csv")
#' get_balance(dat, "daniel")
get_balance <- function(db, paciente, fecha_hoy = Sys.Date()) {
  i <- db$paciente == paciente & db$fecha == fecha_hoy - 1
  fila_ayer <- db[i, ]
  
  # if (fila_ayer$consistencia_deposiciones == "grumosas") {
  #   orina_extra <- fila_ayer$deposiciones * 2/3
  # } else if (fila_ayer$consistencia_deposiciones == "acuosas") {
  #   orina_extra <- fila_ayer$deposiciones * 1/3
  # } else if (fila_ayer$consistencia_deposiciones == "semiacuosas") {
  #   orina_extra <- fila_ayer$deposiciones * 1/2 
  # } else {
  #   orina_extra <- 0
  # }
  orina_extra <- 0
  
  if (fila_ayer$peso <= 10) {
    superficie_corporal <- (fila_ayer$peso * 4 + 9) / 100
    agua_oxidacion <- fila_ayer$peso * 10
    flujo_urinario <- (fila_ayer$diuresis + orina_extra) / fila_ayer$peso / fila_ayer$horas
    flujo_urinario <- round(flujo_urinario, digits = 2)
    flujo_urinario <- paste0(flujo_urinario, " ml/kg/h")
    if (fila_ayer$fiebre) {
      perdidas_insensibles <- fila_ayer$peso * 40 * fila_ayer$horas / 24 
    } else {
      perdidas_insensibles <- fila_ayer$peso * 33 * fila_ayer$horas / 24
    }
  } else {
    superficie_corporal <- (fila_ayer$peso * 4 + 7) / (fila_ayer$peso + 90)
    agua_oxidacion <- superficie_corporal * 250
    flujo_urinario <- (fila_ayer$diuresis + orina_extra) / superficie_corporal / fila_ayer$horas
    flujo_urinario <- round(flujo_urinario, digits = 2)
    flujo_urinario <- paste0(flujo_urinario, " ml/m2/h")
    if (fila_ayer$fiebre) {
      perdidas_insensibles <- superficie_corporal * 600 * fila_ayer$horas / 24
    } else {
      perdidas_insensibles <- superficie_corporal * 400 * fila_ayer$horas / 24
    }
  }
  perdidas_insensibles <- round(perdidas_insensibles)
  superficie_corporal <- round(superficie_corporal, digits = 2)
  
  flujo_diarreico <- fila_ayer$deposiciones / fila_ayer$peso / fila_ayer$horas
  flujo_diarreico <- round(flujo_diarreico, digits = 2)
  
  ingresos <- fila_ayer$via_oral + fila_ayer$endovenoso + fila_ayer$tratamiento
  egresos <- fila_ayer$diuresis + fila_ayer$deposiciones + fila_ayer$otros_egresos + perdidas_insensibles
  balance <- ingresos - egresos
  if (balance >= 0) {
    balance <- paste0("+", balance)
  }
  
  list(
    paciente = paciente,
    peso = fila_ayer$peso,
    horas = fila_ayer$horas,
    superficie_corporal = superficie_corporal,
    diuresis = fila_ayer$diuresis,
    orina_deposiciones = fila_ayer$deposiciones,
    perdidas_insensibles = perdidas_insensibles,
    otros_egresos = fila_ayer$otros_egresos,
    via_oral = fila_ayer$via_oral,
    endovenoso = fila_ayer$endovenoso,
    tratamiento = fila_ayer$tratamiento,
    balance = balance,
    ingresos = ingresos,
    egresos = egresos,
    flujo_urinario = flujo_urinario,
    flujo_diarreico = flujo_diarreico
  )
}
