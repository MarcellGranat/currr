format_eta <- function(seconds) {
  if (seconds > 360000) {
    paste(round(seconds / 86400, digits = 1), "days")
  }
  if (seconds > 3600) {
    hours <- floor(seconds / 3600)
    minutes <- floor((seconds - (hours * 3600)) / 60)
    remaining_seconds <- ceiling(seconds - (hours * 3600) - (minutes * 60))
    time <- sprintf("%02d:%02d:%02d", hours, minutes, remaining_seconds)
  } else if (seconds > 300) {
    paste(floor(seconds %/% 60), "min")
  } else if (seconds > 60) {
    paste(floor(seconds %/% 60), "min", ceiling(seconds %% 60), "sec")
  } else {
    paste(ceiling(seconds), "sec")
  }
}
