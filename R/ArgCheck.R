#' Argcheck
#'
#' @keywords internal
#' @export
newArgCheck <- function () {
  argcheck <- new.env()
  assign("n_warn", 0, envir = argcheck)
  assign("warn_msg", NULL, envir = argcheck)
  assign("n_error", 0, envir = argcheck)
  assign("error_msg", NULL, envir = argcheck)
  assign("n_message", 0, envir = argcheck)
  assign("message_msg", NULL, envir = argcheck)
  class(argcheck) <- c("ArgCheck", "environment")
  return(argcheck)
}

#' Argcheck
#'
#' @keywords internal
#' @export
finishArgCheck <- function (argcheck) {
  fn_call <- sys.call(-1)
  fn_call <- utils::capture.output(fn_call)
  if (!"ArgCheck" %in% class(argcheck))
    stop("'argcheck' must be an object of class 'ArgCheck'")
  argcheck <- mget(ls(envir = argcheck), envir = argcheck)
  if (argcheck$n_warn > 0)
    warning(paste0(c("", paste0(1:argcheck$n_warn,
                                ": ", argcheck$warn_msg)),
                   collapse = "\n"),
            call. = FALSE)
  if (argcheck$n_message > 0)
    message(paste0(c("", paste0(1:argcheck$n_message,
                                ": ", argcheck$message_msg)),
                   collapse = "\n"))
  if (argcheck$n_error > 0)
    stop(paste0(c("", paste0(1:argcheck$n_error,
                             ": ", argcheck$error_msg)),
                collapse = "\n"),
         call. = FALSE)
}

#' Argcheck
#'
#' @keywords internal
#' @export
addError <- function (msg, argcheck) {
  if (!"ArgCheck" %in% class(argcheck))
    stop("'argcheck' must be an object of class 'ArgCheck'")
  assign("n_error", get("n_error", envir = argcheck) + 1, envir = argcheck)
  assign("error_msg", c(get("error_msg", envir = argcheck),
                        msg), envir = argcheck)
}

#' Argcheck
#'
#' @keywords internal
#' @export
addMessage <- function (msg, argcheck) {
  if (!"ArgCheck" %in% class(argcheck))
    stop("'argcheck' must be an object of class 'ArgCheck'")
  assign("n_message", get("n_message", envir = argcheck) +
           1, envir = argcheck)
  assign("message_msg", c(get("message_msg", envir = argcheck),
                          msg), envir = argcheck)
}

#' Argcheck
#'
#' @keywords internal
#' @export
addWarning <- function (msg, argcheck) {
  if (!"ArgCheck" %in% class(argcheck))
    stop("'argcheck' must be an object of class 'ArgCheck'")
  assign("n_warn", get("n_warn", envir = argcheck) + 1, envir = argcheck)
  assign("warn_msg", c(get("warn_msg", envir = argcheck), msg),
         envir = argcheck)
}
