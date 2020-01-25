
#' Create Dockerfile from renv.lock file
#
#' @param path path to the renv.lock file to use as an input.
#'
#' @param FROM The FROM of the Dockerfile. Default is FROM rocker/r-ver:
#'     with `R.Version()$major` and `R.Version()$minor`.
#' @param AS The AS of the Dockerfile. Default it NULL.
#' @param sysreqs boolean to check the system requirements    
#' @param repos character vector, the base URL of the repositories  
#' @param expand boolean, if `TRUE` each system requirement will be known his own RUN line
#' @param update_tar_gz boolean, if `TRUE` and build_golem_from_source is also `TRUE` an updated tar.gz Package is created
#' @param build_golem_from_source  boolean, if `TRUE` no tar.gz Package is created and the Dockerfile directly mount the source folder to build it
#' @importFrom utils installed.packages packageVersion
#' @importFrom remotes dev_package_deps
#' @importFrom desc desc_get_deps
#' @importFrom magrittr %>% 
#' @importFrom renv snapshot
#' @importFrom stats setNames
#' @noRd
dock_from_renv <- function(
  path = "renv.lock",
  # FROM = paste0(
  #   "rocker/r-ver:", 
  #   R.Version()$major,".", 
  #   R.Version()$minor
  # ),
  AS = NULL,
  sysreqs = TRUE,
  # repos = "https://cran.rstudio.com/",
  expand = FALSE,
  update_tar_gz = TRUE,
  build_golem_from_source = TRUE
){
  
  
  if ( ! file.exists(path)){
    lock <- renv::snapshot(lockfile = NULL, confirm = FALSE)
    renv_lockfile_write(lock, file = path)
    }
  
  
  
  
    lock <-  renv:::renv_lockfile_read(path)
  
  
  
  FROM <- paste0("rocker/r-ver:", lock$R$Version )
  repos <- lock$R$Repositories[[1]] # faudra pas faire comme ca , et permettre de mettre autre chose.
  
  
  
  
  
  if (sysreqs){
    # please wait during system requirement calculation
    cat_bullet(
      "Please wait while we compute system requirements...", 
      bullet = "info",
      bullet_col = "green"
    ) # TODO animated version ?
    system_requirement <- unique(
      get_sysreqs(packages = names(lock$Packages))
    )
    cat_green_tick("Done") # TODO animated version ?
    
  } else{
    system_requirement <- NULL
  }
  
  
  dock <- dockerfiler::Dockerfile$new(FROM = FROM, AS=AS)
  
  if (length(system_requirement)>0){
    if ( !expand){
      dock$RUN(
        paste(
          "apt-get update && apt-get install -y ",
          paste(system_requirement, collapse = " ") 
        )
      )
    } else {
      dock$RUN("apt-get update")
      for ( sr in system_requirement ){
        dock$RUN( paste("apt-get install -y ", sr) )
      }
    }
  }
  
  dock$RUN(
    sprintf(
      "echo \"options(repos = c(CRAN = '%s'), download.file.method = 'libcurl')\" >> /usr/local/lib/R/etc/Rprofile.site",
      repos
    )
  )
  
  
  dock$RUN("mkdir /renv_restore")
  dock$WORKDIR("/renv_restore")
  dock$COPY(from=path ,"/renv_restore/renv.lock")
  dock$RUN("R -e 'install.packages(\"renv\")'")
  dock$RUN("R -e 'renv::restore()'")
  
  
  
  
  if ( !build_golem_from_source){
    
    if ( update_tar_gz ){
      old_version <- list.files(
        pattern = glue::glue("{read.dcf(desc.path)[1]}_.+.tar.gz"),
        full.names = TRUE
      )
      
      if ( length(old_version) > 0){
        lapply(old_version, file.remove)
        lapply(old_version, unlink, force = TRUE)
        cat_red_bullet(glue::glue("{paste(old_version,collapse = ', ')} were removed from folder"))
      }
      
      cat_green_tick(glue::glue(" {read.dcf(desc.path)[1]}_{read.dcf(desc.path)[1,][['Version']]}.tar.gz created."))
      devtools::build(path = ".")
    }
    # we use an already builded tar.gz file
    dock$COPY(
      from = paste0(read.dcf(desc.path)[1], "_*.tar.gz"),
      to = "/app.tar.gz"
    )
    dock$RUN("R -e 'remotes::install_local(\"/app.tar.gz\",upgrade=\"never\")'")
  } else {
    dock$RUN("mkdir /build_zone")
    dock$ADD(from = ".",to =  "/build_zone")
    dock$WORKDIR("/build_zone")
    dock$RUN("R -e 'remotes::install_local(upgrade=\"never\")'")
  }
  
  dock
}