#' Set Canvas Domain
#'
#' Set the Canvas domain to be used in the current session.
#'
#' @param domain Canvas Domain. Defaults to <https://canvas.instructure.com>.
#' @export
#'
#' @examples
#' \dontrun{
#' set_canvas_domain(domain = "https://canvas.instructure.com")
#' }
set_canvas_domain = function(domain = "https://canvas.instructure.com") {
    canvas_set_api_key_r_environ(domain)
}

#' Set Canvas API Key
#'
#' Set the Canvas API Key to be used in the current session.
#'
#' @param api_key Canvas Web API Key
#' @param ask     Try to set the API key if not found. Requires session to be
#'                interactive.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_canvas_token(api_key = "my-secret-1823510823uadf-api-key")
#' }
set_canvas_token <- function(api_key = NULL, ask = is_interactive()) {
    if (is.null(api_key)) {
        if (ask && is_interactive()) {
            # Todo: clean up
            cli::cli_alert_info(
                "The Canvas API Key has not yet been set.\\
                 Please visit your Canvas institution webpage's profile page to generate one.")
            user_supplied_api_key = readline("Please enter your API Key: ")
            canvas_set_api_key_r_environ(user_supplied_api_key)
        } else {
            cli::cli_abort(
                "Set RCANVAS_API_KEY by providing a value for argument {.arg api_key}\\
                or by setting {.envvar RCANVAS_API_KEY} inside of {.file ~/.Renviron} file."
            )
        }
    } else {
        if (canvas_api_key_is_set()) {
            cli::cli_alert_info(
                "RCANVAS_API_KEY token is already set.\\
                Modifying the API being used for this session.\\
                To use this token permanently, please update \\
                {.envvar RCANVAS_API_KEY} inside of your {.file ~/.Renviron} file.")
            canvas_set_api_key_r_environ(api_key)
        }
    }
}


canvas_set_domain_r_environ = function(domain_url) {
    Sys.setenv(RCANVAS_CANVAS_DOMAIN = domain_url)
}
canvas_set_api_key_r_environ = function(api_key) {
    Sys.setenv(RCANVAS_API_KEY = api_key)
}

is_canvas_domain_set = function() {
    check_if_environment_variable_set("RCANVAS_CANVAS_DOMAIN")
}

is_canvas_api_key_set = function() {
    check_if_environment_variable_set("RCANVAS_API_KEY")
}

check_if_environment_variable_set = function(env_var) {
    !is.na(Sys.getenv(env_var, unset = NA))
}
