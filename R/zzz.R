#' @importFrom utils packageVersion
StartupMessage <- function()
{
  # Startup message obtained as
  msg <- c(paste0("
  ____  __    __
 /___|  | \\  / |  Credit
 ||     |  \\/  |  Model
 ||___  |  ||  |
  \\___| |__||__|  version ", packageVersion("creditmodel")))
  return(msg)
}

.onAttach <- function(lib, pkg)
{
  # startup message
  msg <- StartupMessage()
  if(!interactive())
    msg[1] <- paste("Package 'creditmodel' version", packageVersion("creditmodel"))
  packageStartupMessage(msg)
  invisible()
}

