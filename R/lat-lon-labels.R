# Functions to convert DD to DMS for ggmap axis plotting
# https://stackoverflow.com/questions/45698588/revisiting-the-format-latitude-and-longitude-axis-labels-in-ggplot

scale_x_longitude <- function(xmin = -180, xmax = 180, step = 0.002, ...) {
  xbreaks <- seq(xmin, xmax, step)
  xlabels <- unlist(
    lapply(xbreaks, function(x) {
      ifelse(x < 0, parse(text = paste0(
        paste0(abs(dms(x)$d), expression("*{degree}*")),
        paste0(abs(dms(x)$m), expression("*{minute}*")),
        paste0(abs(dms(x)$s)), expression("*{second}*W")
      )),
      ifelse(x > 0, parse(text = paste0(
        paste0(abs(dms(x)$d), expression("*{degree}*")),
        paste0(abs(dms(x)$m), expression("*{minute}*")),
        paste0(abs(dms(x)$s)), expression("*{second}*E")
      )),
      abs(dms(x))
      )
      )
    })
  )
  return(scale_x_continuous("Longitude", breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
}

scale_y_latitude <- function(ymin = -90, ymax = 90, step = 0.002, ...) {
  ybreaks <- seq(ymin, ymax, step)
  ylabels <- unlist(
    lapply(ybreaks, function(x) {
      ifelse(x < 0, parse(text = paste0(
        paste0(abs(dms(x)$d), expression("*{degree}*")),
        paste0(abs(dms(x)$m), expression("*{minute}*")),
        paste0(abs(dms(x)$s)), expression("*{second}*S")
      )),
      ifelse(x > 0, parse(text = paste0(
        paste0(abs(dms(x)$d), expression("*{degree}*")),
        paste0(abs(dms(x)$m), expression("*{minute}*")),
        paste0(abs(dms(x)$s)), expression("*{second}*N")
      )),
      abs(dms(x))
      )
      )
    })
  )
  return(scale_y_continuous("Latitude", breaks = ybreaks, labels = ylabels, expand = c(0, 0), ...))
}
