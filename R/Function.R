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


#' Plot trend
#'
#' Plot trend
#'
#' @param dat x
#'
#' @examples
#' data(tuggeranong)
#' plot_trend(tuggeranong)
#'
#' @export
plot_trend <- function(dat){
  melttemp <- Wide_To_Long_Temperature(dat)

  model <- summary(stats::lm(Temperature ~ Year + Month, data = melttemp))
  year_effect <- round(model$coefficients["Year",],3)

  ggplot2::ggplot(data=melttemp,
                  ggplot2::aes(x=melttemp$Year,y=melttemp$Temperature, color=melttemp$Month))+
    ggplot2::geom_point() + ggplot2::geom_smooth(alpha=0.2) +
    ggplot2::annotate("label", x = mean(melttemp$Year),
             y = max(melttemp$Temperature),
             label = paste0(ifelse(year_effect[1]>=0, "+", "-"),
                            year_effect[1], "\u2103 /year, p=",year_effect[4]))
}
