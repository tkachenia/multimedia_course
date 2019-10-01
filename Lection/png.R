# install.packages("png")
library("png")
writePNG.open <- function(file_name, func, c_xy = c(1920, 1080), arg_list = list()) {
     subdir_exec("pic", impl.writePNG.open, file_name, func = func, c_xy = c_xy, arg_list = arg_list)
     NULL
}

impl.writePNG.open <- function(file_name, func, c_xy = c(1920, 1080), arg_list = list()) {
     png(file_name, c_xy[1], c_xy[2])
     if (!is.null(func))
          do.call(func, arg_list)
}

writePNG.add <- function(file_name, func, arg_list = list()) {
     subdir_exec("pic", impl.writePNG.add, file_name, func = func, arg_list = arg_list)
     NULL
}

impl.writePNG.add <- function(file_name, func, arg_list = list()) {
     if (!is.null(func))
          do.call(func, arg_list)
}

writePNG.close <- function(file_name, func, arg_list = list()) {
     subdir_exec("pic", impl.writePNG.close, file_name, func = func, arg_list = arg_list)
     NULL
}

impl.writePNG.close <- function(file_name, func, arg_list = list()) {
     if (!is.null(func))
          do.call(func, arg_list)
     dev.off()
}

writePNG <- function(file_name, func, c_xy = c(1920, 1080), arg_list = list()) {
     subdir_exec("pic", impl.writePNG, file_name, func = func, c_xy = c_xy, arg_list = arg_list)
     NULL
}

impl.writePNG <- function(file_name, func, c_xy = c(1920, 1080), arg_list = list()) {
     png(file_name, c_xy[1], c_xy[2])
     if (!is.null(func))
          do.call(func, arg_list)
     dev.off()
}

subdir_exec <- function(sd, exec_func, ...) {
     wd <- getwd()
     
     dir.create(file.path(wd, sd), showWarnings = FALSE)
     setwd(file.path(wd, sd))
     
     ret <- do.call(exec_func, list(...))
     
     setwd(wd)
     
     return(ret)
}
