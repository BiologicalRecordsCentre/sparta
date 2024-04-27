# Detect whether the user is on a mac or Windows OS (compatable with Frescalo) or a different OS. This function is needed to work with Mockery functions in tests
detect_os_compat <- function() {
    if (Sys.info()["sysname"] %in% c("Darwin", "Windows")) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
