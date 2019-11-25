#' Save check into RDS file
#' 
#' @export 
#' @param check check to be exported
#' @param file_name destination filename
save_check <- function(check, file_name) {
    saveRDS(check, file_name)
}

#' Load check from RDS file
#' 
#' @export 
#' @param file_name source filename that contains check
load_check <- function(file_name) {
    readRDS(file_name)
}

#' Save group of checks into RDS file
#' 
#' @export 
#' @param group group of checks to be exported
#' @param file_name destination filename
save_group <- function(group, file_name) {
    saveRDS(group, file_name)
}

#' Load group of checks from RDS file
#' 
#' @export 
#' @param file_name source filename that contains group of checks
load_group <- function(file_name) {
    readRDS(group, file_name)
}