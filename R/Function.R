#' Fahrenheit to Kelvin conversion
#'
#' This function takes a temperature expressed in Fahrenheit and convert it to Kelvin.
#'
#' @param temp A numeric variable. A temperature expressed in Fahrenheit.
#'
#' @examples
#' fahr_to_kelvin(100)
#'
#' @export
fahr_to_kelvin <- function(temp) {
  stopifnot(is.numeric(temp))
  kelvin <- ((temp - 32) * (5 / 9)) + 273.15
  return(kelvin)
}

#' Kelvin to Celsius conversion
#'
#' This function takes a temperature expressed in Kelvin and convert it to Celsius.
#'
#' @param temp A numeric variable. A temperature expressed in Kelvin
#'
#' @examples
#' kelvin_to_celsius(273.15)
#'
#' @export
kelvin_to_celsius <- function(temp) {
  celsius <- temp - 273.15
  return(celsius)
}

#' Celsius to Kelvin conversion
#'
#' This function takes a temperature expressed in Celsius and convert it to Kelvin.
#'
#' @param temp A numeric variable. A temperature expressed in Celsius
#'
#' @examples
#' celsius_to_kelvin(0)
#'
#' @export
celsius_to_kelvin <- function(temp) {
  kelvin <- temp + 273.15
  return(kelvin)
}


#' Convert wide to long temperature datasets
#'
#' Convert wide to long temperature datasets
#'
#' @param dat x
#' @param id.vars x
#' @param variable.name x
#' @param value.name x
#'
#' @examples
#' data(tuggeranong)
#' Wide_To_Long_Temperature(tuggeranong)
#'
#' @export
Wide_To_Long_Temperature <- function(dat, id.vars="Year",
                                     variable.name = "Month",
                                     value.name = "Temperature"){
  stopifnot(expr={is.data.frame(dat)
    id.vars %in% colnames(dat)
  })
  melttemp <- reshape2::melt(dat, id.vars = id.vars,
                   variable.name = variable.name,
                   value.name = value.name)
  return(melttemp)
}
