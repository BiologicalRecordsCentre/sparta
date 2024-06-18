# Internal function to detect JAGS installation.
#'
detect_jags <- function(jags_path){
  
  # Get the operating system type
  os_type <- Sys.info()["sysname"]
  
  if(!is.null(jags_path)){
    
    message("A non-default jags installation path has been specified. Setting jags path as an environment variable")
    
    # Set JAGS_HOME variable
    Sys.setenv(JAGS_HOME = jags_path)
    
    return(TRUE)
    
  } else {
    
    if (os_type == "Windows") {
      
      # Search the default installation path for Windows
      user_home <- Sys.getenv("USERPROFILE")
      jags_dir <- file.path(user_home, "AppData", "Local", "Programs", "JAGS")
      
      jags_path <- list.files(jags_dir, full.name = TRUE, recursive = TRUE, pattern = "*jags-terminal.exe")
      
    } else if (os_type == "Darwin") {
      
      # Search the default installation path for macOS
      jags_path <- list.files("/usr/local/bin/JAGS", full.name = TRUE, recursive = TRUE, pattern = "*jags-terminal.exe")
      
    } else {
      
      message("Unsupported OS type, returning false for JAGS installation. To bypass, please specify the path to the jags executable in jags_path")
      return(FALSE)
      
    }
    
    if(length(jags_path) > 0){
      
      return(TRUE)
      
    } else {
      
      return(FALSE)
      
    }
  }
}
