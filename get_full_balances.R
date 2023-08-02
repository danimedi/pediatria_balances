get_full_balances <- function(db, fecha_hoy = Sys.Date()) {
  pacientes_hoy <- db[db$fecha == fecha_hoy - 1 & !is.na(db$peso), "paciente"]
  pacientes_hoy <- unlist(unique(pacientes_hoy), use.names = FALSE)
  res <- lapply(
    pacientes_hoy,
    function(paciente) {
      tryCatch(
        get_text(db, paciente, fecha_hoy),
        error = function(e) "ERROR"
      )
    }
  )
  res <- paste0(res, collapse = "\n\n\n\n")
  res
}
