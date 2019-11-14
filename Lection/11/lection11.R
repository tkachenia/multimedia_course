# install.packages("caTools") #
library("caTools") # for write.gif

# install.packages("grid") #
library(grid) # for grid.raster to plot RGB

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

Sys.setlocale("LC_CTYPE", "russian")
source("../png.R")

g_bin.path <- "C:/Program Files"
g_cex.main <- 4
g_mar = c(0, 0, 5, 0)
g_font.size <- 14

add.quote <- function(str) {
     str <- paste0('"', str, '"')
     
     return(str)
}

plot.image <- function(cex.main = g_cex.main, mar = g_mar, ...) {
     par(cex.main = cex.main, mar = mar)
     do.call(image, list(...))
}

plot.image.proportional.subdir <- function(subdir, ...) {
     ret <- subdir_exec(subdir, plot.image.proportional, ...)
     
     return(ret)
}

plot.image.proportional <- function(png_name, pic, col, max_xy = 500, main = NA, cex.main = g_cex.main) {
     coef <- 1
     c_xy_orig <- rev(dim(pic))
     c_xy <- coef * c_xy_orig
     while (max(c_xy) < max_xy) {
          coef <- coef + 1
          c_xy <- coef * c_xy_orig
     }
     c_xy <- c_xy + 1 # axes
     
     mar <- rep(0, 4)
     if (!is.na(main)) {
        line_count = length(strsplit(main, "\n")[[1]])
        mar[3] <-  cex.main * line_count + 1
        c_xy[2] <- c_xy[2] + mar[3] * g_font.size
     }
      
     splice <- c(0, 0)
     if (c_xy[1] %% 2 == 1) {
        splice[1] <- 1
     }
     if (c_xy[2] %% 2 == 1) {
        splice[2] <- 1
     }
          
     impl.writePNG(png_name, func = plot.image, c_xy = c_xy, arg_list = list(x = rotate(pic), col = col, axes = FALSE, main = main, cex.main = cex.main, mar = mar))
     if (any(splice != 0)) {
          png_orig_name <- paste(strsplit(png_name, ".", fixed = TRUE)[[1]], collapse = "_orig.")
          file.rename(png_name, png_orig_name)
          
          cmnd <- paste(add.quote(paste0(g_bin.path, "/ImageMagick/convert.exe")), add.quote(png_orig_name), "-background white", sprintf("-splice %dx%d", splice[1], splice[2]), add.quote(png_name))
          system(cmnd)
     }
     
     return(NULL)
}

rotate <- function(x) {
     t(apply(x, 2, rev))
}

add_frame <- function(m, width = 1, coef = NULL, pen = 0) {
     if (!is.null(coef)) {
          width <- ((coef - 1) * ncol(m)) / 2
     }
     col_ins <- matrix(pen, nrow = nrow(m), ncol = width)
     m <- cbind(col_ins, m, col_ins)
     
     if (!is.null(coef)) {
        width <- ((coef - 1) * nrow(m)) / 2
     }
     row_ins <- matrix(pen, nrow = width, ncol = ncol(m))
     m <- rbind(row_ins, m, row_ins)
     
     return(m)
}

resize <- function(m, coef = 1, downscale = FALSE, func = max) {
     if (downscale) {
          if (nrow(m) %% coef != 0 || ncol(m) %% coef != 0) {
               warning(paste0("Matrix ", nrow(m), "x", ncol(m), " can not downscale by ", coef))
               return(m)
          }
     }

     tmp <- 0
     if (downscale) {
          tmp <- matrix(data = 0, nrow = nrow(m)/coef, ncol = ncol(m)/coef)
          for (i in 1:nrow(tmp))
               for (j in 1:ncol(tmp))
                    tmp[i, j] <- func(m[((i-1)*coef + 1):(i*coef), ((j-1)*coef + 1):(j*coef)])
     } else {
          tmp <- matrix(data = 0, nrow = nrow(m)*coef, ncol = ncol(m)*coef)
          for (i in 1:nrow(m))
               for (j in 1:ncol(m))
                    tmp[((i-1)*coef + 1):(i*coef), ((j-1)*coef + 1):(j*coef)] <- m[i,j]
     }
     
     return(tmp)
}

get_rect <- function(row, col, k_resize, shift = 1, coef = 1) {
     rect <- list()
     rect$x <- (col - shift)*coef*k_resize + ifelse(col == 1, 1, 0)
     rect$y <- (row - shift)*coef*k_resize + ifelse(row == 1, 1, 0)
     rect$width  <- 1*coef*k_resize + ifelse(col != 1, 1, 0)
     rect$height <- 1*coef*k_resize + ifelse(row != 1, 1, 0)
     
     return(rect)
}

draw_rect <- function(mtrx, up_left = c(1, 1), width, height, sub.width = width, sub.height = height, border = 1, sub.border = 1, pen = NA, sub.pen = pen, fill = FALSE, fill.pen = pen, notdrawside = c(0)) {
     if (up_left[1] < 1 || up_left[1]+width-1 > ncol(mtrx)) {
          warning(paste0("Incorrect rectangle x coordinates ", up_left[1], ":", up_left[1]+width-1, ", when picture size is ", ncol(mtrx), "x", nrow(mtrx)))
          return(mtrx)
     }
     if (up_left[2] < 1 || up_left[2]+height-1 > nrow(mtrx)) {
          warning(paste0("Incorrect rectangle y coordinates ", up_left[2], ":", up_left[2]+height-1, ", when picture size is ", ncol(mtrx), "x", nrow(mtrx)))
          return(mtrx)
     }
     if (width %% sub.width != 0 || height %% sub.height != 0) {
          warning(paste0("Can not equaly devide rectangle ", width, "x", height, " by sub-rectangle ", sub.width, "x", sub.height))
          return(mtrx)
     }
     down_right <- c(up_left[1]+width-1, up_left[2]+height-1)
     
     border <- border - 1
     sub.border <- sub.border - 1
     
     if (is.na(pen)) {
          pen <- max(unique(as.vector(mtrx)))+1
          sub.pen <- pen + 1
          fill.pen <- pen + 2
     }
     
     if (fill)
          mtrx[up_left[2]:down_right[2], up_left[1]:down_right[1]] <- fill.pen

     if (all(notdrawside != 3))
          mtrx[up_left[2]:(up_left[2]+border), up_left[1]:down_right[1]] <- pen
     if (all(notdrawside != 1))
          mtrx[(down_right[2]-border):down_right[2], up_left[1]:down_right[1]] <- pen
     if (all(notdrawside != 2))
          mtrx[up_left[2]:down_right[2], up_left[1]:(up_left[1]+border)] <- pen
     if (all(notdrawside != 4))
          mtrx[up_left[2]:down_right[2], (down_right[1]-border):down_right[1]] <- pen

     if (width/sub.width > 1)
          for (i in 1:(width/sub.width - 1))
               mtrx[(up_left[2]+border+1):(down_right[2]-border-1), (up_left[1]+sub.width*i-1-sub.border):(up_left[1]+sub.width*i-1)] <- sub.pen
     if (height/sub.height > 1)
         for (j in 1:(height/sub.height - 1))
              mtrx[(up_left[2]+sub.height*j-1-sub.border):(up_left[2]+sub.height*j-1), (up_left[1]+border+1):(down_right[1]-border-1)] <- sub.pen

     return(mtrx)
}

convolution <- function(ltr, krnl, subDir, append_title_single = " для\nнеизвестной буквы", append_title_joined = append_title_single) {
     if (any(sort(unique(as.vector(ltr))) != c(0, 1))) {
          warning("Letter matrix must consists of only 0 and 1")
          return(NULL)
     }
     if (any(sort(unique(as.vector(krnl))) != c(0, 1))) {
          warning("Kernel matrix must consists of only 0 and 1")
          return(NULL)
     }
     
     mainDir <- getwd()
     subDir <- subDir
     
     if (!file.exists(subDir))
          dir.create(file.path(mainDir, subDir))
     setwd(file.path(mainDir, subDir))
     
     k_resize <- 10
     
     data <- matrix(data = 0, nrow = nrow(ltr), ncol = ncol(ltr))
     mtrx <- data
     pen <- c(0:sum(krnl), 1:2 + sum(krnl))
     col <- c(gray(sum(krnl):0 / sum(krnl)), "#0000FF", "#FF0000")
     pic <- 0

     ltrf <- add_frame(ltr)
     
     ltrfr <- ltrf
     ltrfr[ltrfr == 1] <- sum(krnl)
     ltrfr <- resize(ltrfr, k_resize)
     
     gif.pic <- matrix(0, nrow = nrow(ltrfr), ncol = 2*ncol(ltrfr)+10)
     gif.all <- array(0, c(3*nrow(gif.pic), 3*ncol(gif.pic), nrow(ltr)*ncol(ltr)))
     gif.ind <- 1
     
     idx <- 0
     for (i in 1:(nrow(ltrf)-2)) {
          for (j in 1:(ncol(ltrf)-2)) {
               idx <- idx + 1
             
               kernel_1 <- draw_rect(ltrfr, c((j-1)*k_resize+1, (i-1)*k_resize+1), 3*k_resize, 3*k_resize, 1*k_resize, 1*k_resize, pen = pen[length(pen)-1])
               kernel_2 <- draw_rect(kernel_1, c((j-1)*k_resize+1, i*k_resize), 3*k_resize, 1*k_resize+1, 1*k_resize, 1*k_resize+1, pen = pen[length(pen)])
             
               png_name_left <- paste0(subDir, "-kl_", sprintf("%03d", idx), ".png")
               plot.image.proportional(png_name_left, kernel_2, col, main = paste0("Свертка с ядром", append_title_single), cex.main = 3)
               
               #  ----------
               
               data[i, j] <- sum(krnl*ltrf[i:(i+nrow(krnl)-1), j:(j+ncol(krnl)-1)])/sum(krnl)
               
               mtrx[i, j] <- sum(krnl*ltrf[i:(i+nrow(krnl)-1), j:(j+ncol(krnl)-1)])
               feature_1 <- resize(mtrx, k_resize)
               feature_1 <- draw_rect(feature_1, c(1, 1), ncol(mtrx)*k_resize, nrow(mtrx)*k_resize, 1*k_resize, 1*k_resize, pen = pen[length(pen)-1])
               pic <- feature_1
               feature_1 <- add_frame(feature_1, width = k_resize)
               rect <- get_rect(i, j, k_resize, shift = 0)
               feature_2 <- draw_rect(feature_1, c(rect$x, rect$y), rect$width, rect$height, rect$width, rect$height, pen = pen[length(pen)])
               
               png_name_right <- paste0(subDir, "-fm_", sprintf("%03d", idx), ".png")
               plot.image.proportional(png_name_right, feature_2, col, main = paste0("Карта признаков", append_title_single), cex.main = 3)

               #  ----------
               
               # 1. Join 2 matrix (single main title)
               gif.pic <- cbind(kernel_2, matrix(0, nrow = nrow(gif.pic), ncol = k_resize), feature_2)
               gif.all[,,gif.ind] <- resize(gif.pic, 3)
               gif.ind <- gif.ind + 1
               
               png_name_1st_approach <- paste0(subDir, "_1_", sprintf("%03d", idx), ".png")
               plot.image.proportional(png_name_1st_approach, gif.pic, col, max_xy = 900, main = paste0("Свертка ", append_title_joined), cex.main = 4)

               # 2. Join 2 PNG (separate main titles)
               png_name_2nd_approach <- paste0(subDir, "_2_", sprintf("%03d", idx), ".png")
               cmnd <- paste(add.quote(paste0(g_bin.path, "/ImageMagick/montage.exe")), "-geometry +0+0", add.quote(png_name_left), add.quote(png_name_right), add.quote(png_name_2nd_approach))
               system(cmnd)

               # Read and hstack raw RGB data #
               
               # png_left <- readPNG(png_name_left)
               # png_right <- readPNG(png_name_right)
               # if (!all(dim(png_left) == dim(png_right))) {
               #    warning("Can not concat pictures with different dimentions")
               #    return(NULL)
               # }
               # 
               # row_fixed <- dim(png_left)[1]
               # col_single <- dim(png_left)[2]
               # col_joined <- 2 * col_single
               # 
               # c_xy_single <- c(col_single, row_fixed)
               # c_xy_joined <- c(col_joined, row_fixed)
               # 
               # # 3. Join 2 PNG and plot RGB
               # png_joined_dim <- dim(png_left)
               # png_joined_dim[2] <- 2 * png_joined_dim[2]
               # png_joined <- array(data = NA, png_joined_dim)
               # png_joined[,1:col_single,] <- png_left
               # png_joined[,(col_single+1):col_joined,] <- png_right
               # png_name_3rd_approach <- paste0(subDir, "_3_", sprintf("%03d", idx), ".png")
               # impl.writePNG(png_name_3rd_approach, func = grid.raster, c_xy = c_xy_joined, arg_list = list(png_joined))
               
               # Raster approach #
               # Add additional margin to resulted png
               # https://gis.stackexchange.com/questions/207477/r-export-raster-to-png-without-margin
               # https://stackoverflow.com/questions/27800307/adding-a-picture-to-plot-in-r

               # 4. Raster 1 
               # png_name_4th_approach <- paste0(subDir, "_4_", sprintf("%03d", idx), ".png")
               # png(png_name_4th_approach, col_joined, row_fixed)
               # par(mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0)) # no margins
               # 
               # plot(c(0, col_joined), c(0, row_fixed), type = "n", xlab = "", ylab = "")
               # rasterImage(png_left, 0, 0, col_single, row_fixed, mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
               # rasterImage(png_right, col_single, 0, col_joined, row_fixed, mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
               # 
               # dev.off()

               # 5. Raster 2
               # png_name_5th_approach <- paste0(subDir, "_5_", sprintf("%03d", idx), ".png")
               # png(png_name_5th_approach, col_joined, row_fixed)
               # par(mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0)) # no margins
               # 
               # layout(matrix(1:2, ncol=2, byrow=TRUE)) # layout the plots into a matrix 2 columns, by row
               # 
               # plot.new() # same as plot(NA, xlim=0:1, ylim=0:1, main = NA, sub = NA, xlab = NA, ylab = NA, xaxt="n", yaxt="n", bty="n")
               # rasterImage(png_left, 0, 0, 1, 1)
               # 
               # plot.new() # same as plot(NA, xlim=0:1, ylim=0:1, main = NA, sub = NA, xlab = NA, ylab = NA, xaxt="n", yaxt="n", bty="n")
               # rasterImage(png_right, 0, 0, 1, 1)
               # 
               # dev.off()
          }
     }
     
     # Make GIF from raw matrix data #
     
     # gif.col <- c(col, rep("#FFFFFF", times = 256-length(col)))
     # write.gif(gif.all, paste0(subDir, "-0.gif"), col = gif.col, delay = 30)
     
     # Make GIF from png files #
     
     # cmnd <- paste('"C:/Program Files/ImageMagick/convert.exe\"', "-delay 30", paste0(subDir, "_1*.png"), paste0(subDir, "-1.gif"))
     # system(cmnd)
     # 
     # cmnd <- paste('"C:/Program Files/ImageMagick/convert.exe\"', "-delay 30", paste0(subDir, "_2*.png"), paste0(subDir, "-2.gif"))
     # system(cmnd)
     
     # Make MP4 Video #
     cmnd <- paste(add.quote(paste0(g_bin.path, "/ffmpeg/bin/ffmpeg.exe")), "-y", "-r 6", "-start_number 0", "-i", add.quote(paste0(subDir, "_1_%03d.png")), "-c:v libx264", '-vf "fps=25,format=yuv420p"', add.quote(paste0(subDir, "-1.mp4")))
     system(cmnd)
     
     setwd(mainDir)
     
     return(list(data = data, mtrx = mtrx, pic = pic, col = head(col, -1)))
}

subsampling <- function(lty, str_ltr, coef = 1, downscale = TRUE) {
     if (class(coef)[1] != "numeric") {
          warning("Subsampling coeficient must be Number")
          return(lty)
     }
     
     ret <- 0
     
     if (downscale)
          ret <- do.call(subsampling.downscale, list(lty, str_ltr, coef = coef))
     else
          ret <- do.call(subsampling.upscale, list(lty, str_ltr, coef = coef))
     
     return(ret)
}

subsampling.downscale <- function(ltr, str_ltr, coef = 1) {
     if (nrow(ltr) %% coef != 0 || ncol(ltr) %% coef != 0) {
          warning(paste0("Matrix ", nrow(ltr), "x", ncol(ltr), " can not downscale by ", coef))
          return(ltr)
     }
     
     mainDir <- getwd()
     subDir <- str_ltr
     
     if (!file.exists(subDir))
          dir.create(file.path(mainDir, subDir))
     setwd(file.path(mainDir, subDir))
     
     k_resize <- 10
     
     mtrx <- matrix(data = 0, nrow = nrow(ltr)/coef, ncol = ncol(ltr)/coef)
     pen <- sort(unique(c(0, as.vector(ltr))))
     col <- c(gray(max(pen):0 / max(pen)), "#0000FF", "#FF0000")
     pen <- c(0:max(pen), 1:2 + max(pen))
     pic <- 0
     
     ltr_r <- resize(ltr, k_resize)
     
     gif.pic <- matrix(0, nrow = nrow(ltr_r)+2*k_resize, ncol = 2*ncol(ltr_r)+5*k_resize)
     gif.all <- array(0, c(3*nrow(gif.pic), 3*ncol(gif.pic), nrow(mtrx)*ncol(mtrx)))
     gif.ind <- 1
     
     for (i in 1:nrow(mtrx)) {
          for (j in 1:ncol(mtrx)) {
               in_1 <- draw_rect(ltr_r, c(1, 1), ncol(ltr)*k_resize, nrow(ltr)*k_resize, 1*k_resize, 1*k_resize, pen = pen[length(pen)-1])
               rect <- get_rect(i, j, k_resize, coef = coef)
               in_2 <- draw_rect(in_1, c(rect$x, rect$y), rect$width, rect$height, rect$width, rect$height, pen = pen[length(pen)])
               in_3 <- add_frame(in_2, width = k_resize, pen = 0)
               
               png_name_left <- paste0(str_ltr, "-down_in_i", sprintf("%02d", i), "_j", sprintf("%02d", j), ".png")
               impl.writePNG(png_name_left, func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(in_3), col = col, axes = FALSE, main = paste("Входная карта\nпризнаков для ", str_ltr)))
               
               #  ----------
               
               mtrx[i, j] <- max(ltr[((i-1)*coef+1):(i*coef), ((j-1)*coef+1):(j*coef)])
               mtrx_r <- resize(mtrx, k_resize)
               
               out_1 <- draw_rect(mtrx_r, c(1, 1), ncol(mtrx)*k_resize, nrow(mtrx)*k_resize, 1*k_resize, 1*k_resize, pen = pen[length(pen)-1])
               pic <- out_1
               rect <- get_rect(i, j, k_resize)
               out_2 <- draw_rect(out_1, c(rect$x, rect$y), rect$width, rect$height, rect$width, rect$height, pen = pen[length(pen)])
               out_3 <- add_frame(out_2, coef = 2, pen = 0)
               out_4 <- add_frame(out_3, width = k_resize, pen = 0)
               
               png_name_right <- paste0(str_ltr, "-down_out_i", sprintf("%02d", i), "_j", sprintf("%02d", j), ".png")
               impl.writePNG(png_name_right, func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(out_4), col = col, axes = FALSE, main = paste("Выходная карта\nпризнаков для", str_ltr)))
               
               #  ----------
               
               # Join 2 matrix
               gif.pic <- cbind(in_3, matrix(0, nrow = nrow(gif.pic), ncol = k_resize), out_4)
               gif.all[,,gif.ind] <- resize(gif.pic, 3)
               gif.ind <- gif.ind + 1
               
               png_name_1st_approach <- paste0(str_ltr, "_down_sub1(i=", sprintf("%02d", i), ",j=", sprintf("%02d", j), ").png")
               impl.writePNG(png_name_1st_approach, func = plot.image, c_xy = c(960, 640), arg_list = list(cex.main = 4, mar = c(1, 1, 4, 1), x = rotate(gif.pic), col = col, axes = FALSE, main = paste("Подвыборка для буквы", str_ltr)))
               
               # Join 2 PNG and plot RGB
               png_name_2nd_approach <- paste0(str_ltr, "_down_sub2(i=", sprintf("%02d", i), ",j=", sprintf("%02d", j), ").png")
               png_joined <- array(data = NA, c(640, 960 ,3))
               png_joined[,1:480,] <- readPNG(png_name_left)
               png_joined[,481:960,] <- readPNG(png_name_right)
               impl.writePNG(png_name_2nd_approach, func = grid.raster, c_xy = c(960, 640), arg_list = list(png_joined))
          }
     }
     # Make GIF from raw matrix data
     gif.col <- c(col, rep("#FFFFFF", times = 256-length(col)))
     write.gif(gif.all, paste0(str_ltr, "-0_down_sub.gif"), col = gif.col, delay = 30)
     
     # Make GIF from png files
     cmnd <- paste('"C:/Program Files/ImageMagick/convert.exe\"', "-delay 30", paste0(str_ltr, "_down_sub1*.png"), paste0(str_ltr, "-1_down_sub.gif"))
     system(cmnd)
     
     cmnd <- paste('"C:/Program Files/ImageMagick/convert.exe\"', "-delay 30", paste0(str_ltr, "_down_sub2*.png"), paste0(str_ltr, "-2_down_sub.gif"))
     system(cmnd)
     
     setwd(mainDir)
     
     return(list(data = mtrx, mtrx = mtrx, pic = pic, col = head(col, -1)))
}

subsampling.upscale <- function(ltr, str_ltr, coef = 1) {
     mainDir <- getwd()
     subDir <- str_ltr
     
     if (!file.exists(subDir))
          dir.create(file.path(mainDir, subDir))
     setwd(file.path(mainDir, subDir))
     
     k_resize <- 10
     
     mtrx <- matrix(data = 0, nrow = nrow(ltr)*coef, ncol = ncol(ltr)*coef)
     pen <- sort(unique(c(0, as.vector(ltr))))
     col <- c(gray(max(pen):0 / max(pen)), "#0000FF", "#FF0000")
     pen <- c(0:max(pen), 1:2 + max(pen))
     pic <- 0
     
     ltr_r <- resize(ltr, k_resize)
     
     gif.pic <- matrix(0, nrow = k_resize*(nrow(mtrx)+2), ncol = k_resize*(2*ncol(mtrx)+5))
     gif.all <- array(0, c(3*nrow(gif.pic), 3*ncol(gif.pic), nrow(ltr)*ncol(ltr)))
     gif.ind <- 1
     
     for (i in 1:nrow(ltr)) {
          for (j in 1:ncol(ltr)) {
               in_1 <- draw_rect(ltr_r, c(1, 1), ncol(ltr)*k_resize, nrow(ltr)*k_resize, 1*k_resize, 1*k_resize, pen = pen[length(pen)-1])
               rect <- get_rect(i, j, k_resize)
               in_2 <- draw_rect(in_1, c(rect$x, rect$y), rect$width, rect$height, rect$width, rect$height, pen = pen[length(pen)])
               in_3 <- add_frame(in_2, coef = 2, pen = 0)
               in_4 <- add_frame(in_3, width = k_resize, pen = 0)
               
               png_name_left <- paste0(str_ltr, "-up_in_i", sprintf("%02d", i), "_j", sprintf("%02d", j), ".png")
               impl.writePNG(png_name_left, func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(in_4), col = col, axes = FALSE, main = paste("Входная карта\nпризнаков для ", str_ltr)))
               
               #  ----------
               
               mtrx[((i-1)*coef+1):(i*coef), ((j-1)*coef+1):(j*coef)] <- ltr[i, j]
               mtrx_r <- resize(mtrx, k_resize)
               
               out_1 <- draw_rect(mtrx_r, c(1, 1), ncol(mtrx)*k_resize, nrow(mtrx)*k_resize, 1*k_resize, 1*k_resize, pen = pen[length(pen)-1])
               pic <- out_1
               rect <- get_rect(i, j, k_resize, coef = coef)
               out_2 <- draw_rect(out_1, c(rect$x, rect$y), rect$width, rect$height, rect$width, rect$height, pen = pen[length(pen)])
               out_3 <- add_frame(out_2, width = k_resize, pen = 0)
               
               png_name_right <- paste0(str_ltr, "-up_out_i", sprintf("%02d", i), "_j", sprintf("%02d", j), ".png")
               impl.writePNG(png_name_right, func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(out_3), col = col, axes = FALSE, main = paste("Выходная карта\nпризнаков для", str_ltr)))
               
               #  ----------
               
               # Join 2 matrix
               gif.pic <- cbind(in_4, matrix(0, nrow = nrow(gif.pic), ncol = k_resize), out_3)
               gif.all[,,gif.ind] <- resize(gif.pic, 3)
               gif.ind <- gif.ind + 1
               
               png_name_1st_approach <- paste0(str_ltr, "_up_sub1(i=", sprintf("%02d", i), ",j=", sprintf("%02d", j), ").png")
               impl.writePNG(png_name_1st_approach, func = plot.image, c_xy = c(960, 640), arg_list = list(cex.main = 4, mar = c(1, 1, 4, 1), x = rotate(gif.pic), col = col, axes = FALSE, main = paste("Подвыборка для буквы", str_ltr)))

               # Join 2 PNG and plot RGB
               png_name_2nd_approach <- paste0(str_ltr, "_up_sub2(i=", sprintf("%02d", i), ",j=", sprintf("%02d", j), ").png")
               png_joined <- array(data = NA, c(640, 960 ,3))
               png_joined[,1:480,] <- readPNG(png_name_left)
               png_joined[,481:960,] <- readPNG(png_name_right)
               impl.writePNG(png_name_2nd_approach, func = grid.raster, c_xy = c(960, 640), arg_list = list(png_joined))
          }
     }
     # Make GIF from raw matrix data
     gif.col <- c(col, rep("#FFFFFF", times = 256-length(col)))
     write.gif(gif.all, paste0(str_ltr, "-0_up_sub.gif"), col = gif.col, delay = 30)
     
     # Make GIF from png files
     cmnd <- paste('"C:/Program Files/ImageMagick/convert.exe\"', "-delay 30", paste0(str_ltr, "_up_sub1*.png"), paste0(str_ltr, "-1_up_sub.gif"))
     system(cmnd)
     
     cmnd <- paste('"C:/Program Files/ImageMagick/convert.exe\"', "-delay 30", paste0(str_ltr, "_up_sub2*.png"), paste0(str_ltr, "-2_up_sub.gif"))
     system(cmnd)
     
     setwd(mainDir)
     
     return(list(data = mtrx, mtrx = mtrx, pic = pic, col = head(col, -1)))
}

draw.DFS <- function(mtrx, map, p_i, p_j, i, j, name, idx) {
     k_resize <- 10
     p_rect <- get_rect(p_i, p_j, k_resize)
     rect <- get_rect(i, j, k_resize)
     
     mtrx_ <- resize(mtrx, k_resize)
     mtrx_ <- draw_rect(mtrx_, c(1, 1), ncol(mtrx)*k_resize, nrow(mtrx)*k_resize, 1*k_resize, 1*k_resize, pen = 1)
     mtrx_ <- draw_rect(mtrx_, c(p_rect$x, p_rect$y), p_rect$width, p_rect$height, p_rect$width, p_rect$height, pen = 5)
     mtrx_ <- draw_rect(mtrx_, c(rect$x, rect$y), rect$width, rect$height, rect$width, rect$height, pen = 6)
     mtrx_ <- add_frame(mtrx_, width = k_resize)
     
     map_ <- resize(map, k_resize)
     map_ <- draw_rect(map_, c(1, 1), ncol(map)*k_resize, nrow(map)*k_resize, 1*k_resize, 1*k_resize, pen = 1)
     map_ <- draw_rect(map_, c(p_rect$x, p_rect$y), p_rect$width, p_rect$height, p_rect$width, p_rect$height, pen = 5)
     map_ <- draw_rect(map_, c(rect$x, rect$y), rect$width, rect$height, rect$width, rect$height, pen = 6)
     map_ <- add_frame(map_, width = k_resize)
     
     pic <- cbind(mtrx_, matrix(0, nrow = nrow(mtrx_), ncol = k_resize), map_)
     c_xy <- 3 * rev(dim(pic))
     c_xy_titled <- c_xy; c_xy_titled[2] <- c_xy_titled[2] + 60
     
     # Save title separately
     png_name_titled <- "text.png"
     if (!file.exists(png_name_titled)) {
          impl.writePNG(png_name_titled, func = plot, c_xy = c_xy_titled, arg_list = list(cex.main = 4, mai = rep(0, 4), mar = rep(0, 4), x = 0, y = 0, type =  "n", axes = FALSE, main = "Поиск границы объекта в глубину"))
     }
     png_titled <- array(data = NA, c(rev(c_xy_titled) ,3))
     # Load image with title
     png_titled[,,] <- readPNG(png_name_titled)
     
     # map color: 0 - default (empty = white), 1 - stop traverse (border = black), 2 - continue traverse (mask = green), 3 and 4 - (empty = white), 5 - parent (rect = red), 6 - position (rect = blue)
     col <- c("#FFFFFF", "#000000", "#00FF00", "#FFFFFF", "#FFFFFF", "#FF0000", "#0000FF")
     png_name <- sprintf("%s_%03d.png", name, idx); idx <- idx + 1
     # Save image with proportional size
     impl.writePNG(png_name, func = plot.image, c_xy = c_xy, arg_list = list(mar = c(0,0,0,0), x = rotate(pic), col = col, axes = FALSE, main = NA))
     # Load saved image and join it with title
     png_titled[(c_xy_titled[2] - c_xy[2] + 1):c_xy_titled[2],,] <- readPNG(png_name)
     # Save resulted image
     impl.writePNG(png_name, func = grid.raster, c_xy = c_xy_titled, arg_list = list(png_titled))

     # map color: 0 - default (empty = white), 1 and 3 - stop traverse (border = black), 2 and 4 - continue traverse (mask = green), 5 - parent (rect = red), 6 - position (rect = blue)
     col <- c("#FFFFFF", "#000000", "#00FF00", "#000000", "#00FF00", "#FF0000", "#0000FF")
     png_name <- sprintf("%s_%03d.png", name, idx); idx <- idx + 1
     impl.writePNG(png_name, func = plot.image, c_xy = c_xy, arg_list = list(mar = c(0,0,0,0), x = rotate(pic), col = col, axes = FALSE, main = NA))
     png_titled[(c_xy_titled[2] - c_xy[2] + 1):c_xy_titled[2],,] <- readPNG(png_name)
     impl.writePNG(png_name, func = grid.raster, c_xy = c_xy_titled, arg_list = list(png_titled))
     
     return(list(map = map_, col = col, idx = idx))
}

traverse.DFS <- function(mtrx, i, j, delta = 1, subdir = "test", name = "picture") {
     row <- nrow(mtrx)
     col <- ncol(mtrx)
   
     stack <- list(position = array(data = 0, dim = c(2, row*col)),
                   parent = array(data = 0, dim = c(2, row*col)),
                   tail = 1)
     stack$position[,stack$tail] <- c(i, j)
     stack$parent[,stack$tail] <- c(i, j)
     stack$tail <- stack$tail + 1 # push
     
     idx <- 1
     value <- mtrx[i, j]
     map <- array(data = 0, dim = dim(mtrx))
     visited <- array(data = 0, dim = dim(mtrx))
     
     while (stack$tail != 1) {
          stack$tail <- stack$tail - 1; # pop
          i <- stack$position[1, stack$tail]; j <- stack$position[2, stack$tail]
          p_i <- stack$parent[1, stack$tail]; p_j <- stack$parent[2, stack$tail]
          if (visited[i, j] != 1) {
               visited[i, j] <- 1
               
               # 0 - default (empty = white), 1 - stop traverse (border = black), 2 - continue traverse (mask = green)
               map[i, j] <- ifelse(abs(mtrx[i, j] - value) >= delta, 1, 2)
               
               map[i, j] <- map[i, j] + 2 # shift to 3 or 4
               ret <- subdir_exec(subdir, draw.DFS, mtrx, map, p_i, p_j, i, j, name, idx)
               map[i, j] <- map[i, j] - 2
               idx <- ret$idx
               
               if (map[i, j] == 2) {
                    if (i - 1 >= 1) { # move up
                         stack$position[,stack$tail] <- c(i - 1, j)
                         stack$parent[,stack$tail] <- c(i, j)
                         stack$tail <- stack$tail + 1 # push
                    }
                    if (i + 1 <= row) { # move down
                         stack$position[,stack$tail] <- c(i + 1, j)
                         stack$parent[,stack$tail] <- c(i, j)
                         stack$tail <- stack$tail + 1 # push
                    }
                    if (j - 1 >= 1) { # move left
                       stack$position[,stack$tail] <- c(i, j - 1)
                       stack$parent[,stack$tail] <- c(i, j)
                       stack$tail <- stack$tail + 1 # push
                    }
                    if (j + 1 <= col) { # move right
                       stack$position[,stack$tail] <- c(i, j + 1)
                       stack$parent[,stack$tail] <- c(i, j)
                       stack$tail <- stack$tail + 1 # push
                    }
               }
          }
     }
     
     cmnd <- paste('"C:/Program Files/ImageMagick/convert.exe\"', "-delay 30", paste0(subdir, "/", name, "_*.png"), paste0(subdir, "/", name, "_slow.gif"))
     system(cmnd)

     cmnd <- paste('"C:/Program Files/ImageMagick/convert.exe\"', "-delay 5", paste0(subdir, "/", name, "_*.png"), paste0(subdir, "/", name, "_fast.gif"))
     system(cmnd)

     border <- array(data = 0, dim = dim(mtrx))
     border[map == 1] <- 1
     
     mask <- array(data = 0, dim = dim(mtrx))
     mask[map == 2] <- 1
   
     return(list(border = border, mask = mask))
}

super_magic_wang <- function() {
   crcl <- matrix(data = 0, nrow = 12, ncol = 12)
   r1 <- 6
   r2 <- 5
   for (i in 1:ncol(crcl)) {
      for (j in 1:nrow(crcl)) {
         if (abs(sqrt((i-6.5)^2+(j-6.5)^2) - r1) < 1)
            crcl[i, j] <- 1
         if (abs(sqrt((i-6.5)^2+(j-6.5)^2) - r2) < 1)
            crcl[i, j] <- 1
      }
   }
   crcl[6:7,1:2] <- 0
   crcl_dfs <- add_frame(crcl, width = 2)
   
   subdir <- "test"
   name <- "DFS"
   ret <- subdir_exec("pic", traverse.DFS, crcl_dfs, 6, 6, subdir = subdir, name = name)
   
   k_resize <- 10
   area <- ret$mask
   area <- resize(area, k_resize)
   area <- draw_rect(area, c(1, 1), ncol(area), nrow(area), k_resize, k_resize, pen = 0)
   area <- add_frame(area, width = k_resize, pen = 2)
   writePNG("05_circle_dfs.png", func = plot.image, c_xy = c(555, 600), arg_list = list(cex.main = 3, x = rotate(area), col = c("#000000", "#00FF00", "#FFFFFF"), axes = FALSE, main = "Граница разорванного круга"))

   k_resize <- 10
   crcl_ <- resize(crcl, coef = 10)
   writePNG("06_circle.png", func = plot.image, c_xy = c(480, 640), arg_list = list(x = rotate(add_frame(crcl_, width = k_resize)), col = c("#FFFFFF", "#000000"), axes = FALSE, main = "Разорванный круг"))
   
   crcl_grid <- draw_rect(crcl_, c(1, 1), ncol(crcl)*k_resize, nrow(crcl)*k_resize, 1*k_resize, 1*k_resize, pen = 2)
   writePNG("07_circle_grid.png", func = plot.image, c_xy = c(480, 640), arg_list = list(x = rotate(add_frame(crcl_grid, width = k_resize)), col = c("#FFFFFF", "#000000", "#0000FF"), axes = FALSE, main = "Разорванный круг"))
   
   crcl_down <- subdir_exec("pic", subsampling, crcl, "Circle", 2)
   writePNG("08_circle_down.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1,1,7,1), x = rotate(add_frame(add_frame(crcl_down$pic, coef = 2), width = k_resize)), col = crcl_down$col, axes = FALSE, main = "Разорванный круг\nуменьшенный в 2 раза"))

   crcl_down_pic <- crcl_down$pic
   crcl_down_pic[(2*k_resize):(4*k_resize), 1*k_resize] <- 3
   crcl_down_pic[(2*k_resize):(4*k_resize), 5*k_resize] <- 3
   crcl_down_pic[(1*k_resize):(2*k_resize), 2*k_resize] <- 3
   crcl_down_pic[(4*k_resize):(5*k_resize), 2*k_resize] <- 3
   crcl_down_pic[(1*k_resize):(2*k_resize), 4*k_resize] <- 3
   crcl_down_pic[(4*k_resize):(5*k_resize), 4*k_resize] <- 3
   crcl_down_pic[1*k_resize, (2*k_resize):(4*k_resize)] <- 3
   crcl_down_pic[5*k_resize, (2*k_resize):(4*k_resize)] <- 3
   crcl_down_pic[2*k_resize, (1*k_resize):(2*k_resize)] <- 3
   crcl_down_pic[2*k_resize, (4*k_resize):(5*k_resize)] <- 3
   crcl_down_pic[4*k_resize, (1*k_resize):(2*k_resize)] <- 3
   crcl_down_pic[4*k_resize, (4*k_resize):(5*k_resize)] <- 3
   writePNG("09_circle_down_area.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1,1,7,1), x = rotate(add_frame(add_frame(crcl_down_pic, coef = 2), width = k_resize)), col = c(crcl_down$col, "#FF0000"), axes = FALSE, main = "Разорванный круг\nуменьшенный в 2 раза"))
   
   crcl_up <- subdir_exec("pic", subsampling, crcl_down$mtrx, "Circle", 2, downscale = FALSE)
   writePNG("10_circle_up.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1,1,7,1), x = rotate(add_frame(crcl_up$pic, width = k_resize)), col = crcl_up$col, axes = FALSE, main = "Восстановленный\nразорванный круг"))
   
   crcl_up_pic <- crcl_up$pic
   crcl_up_pic[(4*k_resize):(8*k_resize), 2*k_resize] <- 3
   crcl_up_pic[(4*k_resize):(8*k_resize), 10*k_resize] <- 3
   crcl_up_pic[(2*k_resize):(4*k_resize), 4*k_resize] <- 3
   crcl_up_pic[(8*k_resize):(10*k_resize), 4*k_resize] <- 3
   crcl_up_pic[(2*k_resize):(4*k_resize), 8*k_resize] <- 3
   crcl_up_pic[(8*k_resize):(10*k_resize), 8*k_resize] <- 3
   crcl_up_pic[2*k_resize, (4*k_resize):(8*k_resize)] <- 3
   crcl_up_pic[10*k_resize, (4*k_resize):(8*k_resize)] <- 3
   crcl_up_pic[4*k_resize, (2*k_resize):(4*k_resize)] <- 3
   crcl_up_pic[4*k_resize, (8*k_resize):(10*k_resize)] <- 3
   crcl_up_pic[8*k_resize, (2*k_resize):(4*k_resize)] <- 3
   crcl_up_pic[8*k_resize, (8*k_resize):(10*k_resize)] <- 3
   writePNG("11_circle_up_area.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1,1,7,1), x = rotate(add_frame(crcl_up_pic, width = k_resize)), col = c(crcl_up$col, "#FF0000"), axes = FALSE, main = "Восстановленный\nразорванный круг"))
   
   crcl_grid_sel <- crcl_grid
   crcl_grid_sel[(4*k_resize):(8*k_resize), 2*k_resize] <- 3
   crcl_grid_sel[(4*k_resize):(8*k_resize), 10*k_resize] <- 3
   crcl_grid_sel[(2*k_resize):(4*k_resize), 4*k_resize] <- 3
   crcl_grid_sel[(8*k_resize):(10*k_resize), 4*k_resize] <- 3
   crcl_grid_sel[(2*k_resize):(4*k_resize), 8*k_resize] <- 3
   crcl_grid_sel[(8*k_resize):(10*k_resize), 8*k_resize] <- 3
   crcl_grid_sel[2*k_resize, (4*k_resize):(8*k_resize)] <- 3
   crcl_grid_sel[10*k_resize, (4*k_resize):(8*k_resize)] <- 3
   crcl_grid_sel[4*k_resize, (2*k_resize):(4*k_resize)] <- 3
   crcl_grid_sel[4*k_resize, (8*k_resize):(10*k_resize)] <- 3
   crcl_grid_sel[8*k_resize, (2*k_resize):(4*k_resize)] <- 3
   crcl_grid_sel[8*k_resize, (8*k_resize):(10*k_resize)] <- 3
   writePNG("12_circle_up_area_sel.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1,1,7,1), x = rotate(add_frame(crcl_grid_sel, width = k_resize)), col = c(crcl_up$col, "#FF0000"), axes = FALSE, main = "Разорванный круг\nс полученной границей"))
   
   crcl_grid_sel_fill <- crcl_grid_sel
   x <- crcl_grid_sel_fill[(4*k_resize):(8*k_resize), (2*k_resize):(10*k_resize)]
   x[x==0] <- 4
   crcl_grid_sel_fill[(4*k_resize):(8*k_resize), (2*k_resize):(10*k_resize)] <- x
   x <- crcl_grid_sel_fill[(2*k_resize):(10*k_resize),(4*k_resize):(8*k_resize)]
   x[x==0] <- 4
   crcl_grid_sel_fill[(2*k_resize):(10*k_resize),(4*k_resize):(8*k_resize)] <- x
   writePNG("13_circle_up_area_sel_fill.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1,1,7,1), x = rotate(add_frame(crcl_grid_sel_fill, width = k_resize)), col = c(crcl_up$col, "#FF0000", "#00FF00"), axes = FALSE, main = "Разорванный круг\nс полученной границей"))

   crcl_grid_sel_fill_full <- crcl_grid
   crcl_grid_sel_fill_full[(4*k_resize):(8*k_resize), 2*k_resize] <- 3
   crcl_grid_sel_fill_full[(4*k_resize):(8*k_resize), 10*k_resize] <- 3
   crcl_grid_sel_fill_full[(2*k_resize):(3*k_resize), 4*k_resize] <- 3
   crcl_grid_sel_fill_full[(3*k_resize):(4*k_resize), 3*k_resize] <- 3
   crcl_grid_sel_fill_full[(8*k_resize):(9*k_resize), 3*k_resize] <- 3
   crcl_grid_sel_fill_full[(9*k_resize):(10*k_resize), 4*k_resize] <- 3
   crcl_grid_sel_fill_full[(2*k_resize):(3*k_resize), 8*k_resize] <- 3
   crcl_grid_sel_fill_full[(3*k_resize):(4*k_resize), 9*k_resize] <- 3
   crcl_grid_sel_fill_full[(8*k_resize):(9*k_resize), 9*k_resize] <- 3
   crcl_grid_sel_fill_full[(9*k_resize):(10*k_resize), 8*k_resize] <- 3
   crcl_grid_sel_fill_full[2*k_resize, (4*k_resize):(8*k_resize)] <- 3
   crcl_grid_sel_fill_full[10*k_resize, (4*k_resize):(8*k_resize)] <- 3
   crcl_grid_sel_fill_full[4*k_resize, (2*k_resize):(3*k_resize)] <- 3
   crcl_grid_sel_fill_full[3*k_resize, (3*k_resize):(4*k_resize)] <- 3
   crcl_grid_sel_fill_full[3*k_resize, (8*k_resize):(9*k_resize)] <- 3
   crcl_grid_sel_fill_full[4*k_resize, (9*k_resize):(10*k_resize)] <- 3
   crcl_grid_sel_fill_full[8*k_resize, (2*k_resize):(3*k_resize)] <- 3
   crcl_grid_sel_fill_full[9*k_resize, (3*k_resize):(4*k_resize)] <- 3
   crcl_grid_sel_fill_full[9*k_resize, (8*k_resize):(9*k_resize)] <- 3
   crcl_grid_sel_fill_full[8*k_resize, (9*k_resize):(10*k_resize)] <- 3
   x <- crcl_grid_sel_fill_full[(4*k_resize):(8*k_resize), (2*k_resize):(10*k_resize)]
   x[x==0] <- 4
   crcl_grid_sel_fill_full[(4*k_resize):(8*k_resize), (2*k_resize):(10*k_resize)] <- x
   x <- crcl_grid_sel_fill_full[(2*k_resize):(10*k_resize),(4*k_resize):(8*k_resize)]
   x[x==0] <- 4
   crcl_grid_sel_fill_full[(2*k_resize):(10*k_resize),(4*k_resize):(8*k_resize)] <- x
   writePNG("14_circle_up_area_sel_fill_full.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1,1,7,1), x = rotate(add_frame(crcl_grid_sel_fill_full, width = k_resize)), col = c(crcl_up$col, "#FF0000", "#00FF00"), axes = FALSE, main = "Внутрянняя область\nразорванного круга"))
   
   return(NULL)
}

lection11.make <- function() {
     k_resize <- 10
   
     # Empty fild
     m <- matrix(data = 0, nrow = 12, ncol = 8)

     # Letter A
     unk <- m
     unk[,1] <- 1
     unk[,2] <- 1
     unk[,ncol(unk)-1] <- 1
     unk[,ncol(unk)] <- 1
     unk[1,] <- 1
     unk[2,] <- 1
     unk[9,] <- 1
     unk[10,] <- 1
     unk[11,] <- 1
     
     unk_f <- add_frame(unk)
     writePNG("01_unk.png", func = plot.image, c_xy = c(480, 640), arg_list = list(x = rotate(unk_f), col = c("#FFFFFF", "#000000"), axes = FALSE, main = "Буква ?"))
     plot.image.proportional.subdir("pic", "01_unk_alt.png", unk_f, c("#FFFFFF", "#000000"), main = "Буква ?", cex.main = 4)
     
     unk_grid <- resize(unk, k_resize)
     unk_grid <- draw_rect(unk_grid, c(1, 1), ncol(unk)*k_resize, nrow(unk)*k_resize, 1*k_resize, 1*k_resize, pen = 2)
     unk_grid <- add_frame(unk_grid, width = k_resize)
     writePNG("01_unk_grid.png", func = plot.image, c_xy = c(480, 640), arg_list = list(x = rotate(unk_grid), col = c("#FFFFFF", "#000000", "#0000FF"), axes = FALSE, main = "Буква ?"))
     plot.image.proportional.subdir("pic", "01_unk_grid_alt.png", unk_grid, c("#FFFFFF", "#000000", "#0000FF"), main = "Буква ?", cex.main = 4)
     
     # Letter A
     a <- m
     a[(nrow(a)-4):nrow(a), 1] <- 1
     a[(nrow(a)-7):(nrow(a)-3), 2] <- 1
     a[(nrow(a)-10):(nrow(a)-6), 3] <- 1
     a[1:3, 4] <- 1
     a[1:3, 5] <- 1
     a[(nrow(a)-10):(nrow(a)-6), 6] <- 1
     a[(nrow(a)-7):(nrow(a)-3), 7] <- 1
     a[(nrow(a)-4):nrow(a), 8] <- 1
     a[9,] <- 1
     a[10,] <- 1
     a_f <- add_frame(a)
     writePNG("01_A.png", func = plot.image, c_xy = c(480, 640), arg_list = list(x = rotate(a_f), col = c("#FFFFFF", "#000000"), axes = FALSE, main = "Буква А"))

     a_grid <- resize(a, k_resize)
     a_grid <- draw_rect(a_grid, c(1, 1), ncol(a)*k_resize, nrow(a)*k_resize, 1*k_resize, 1*k_resize, pen = 2)
     a_grid <- add_frame(a_grid, width = k_resize)
     writePNG("01_A_grid.png", func = plot.image, c_xy = c(480, 640), arg_list = list(x = rotate(a_grid), col = c("#FFFFFF", "#000000", "#0000FF"), axes = FALSE, main = "Буква А"))
     
     # Letter E
     e <- m
     e[,1] <- 1
     e[,2] <- 1
     e[1,] <- 1
     e[2,] <- 1
     e[nrow(e)-1,] <- 1
     e[nrow(e),] <- 1
     e[6, 1:6] <- 1
     e[7, 1:6] <- 1
     e_f <- add_frame(e)
     writePNG("01_E.png", func = plot.image, c_xy = c(480, 640), arg_list = list(x = rotate(e_f), col = c("#FFFFFF", "#000000"), axes = FALSE, main = "Буква Е"))

     e_grid <- resize(e, k_resize)
     e_grid <- draw_rect(e_grid, c(1, 1), ncol(e)*k_resize, nrow(e)*k_resize, 1*k_resize, 1*k_resize, pen = 2)
     e_grid <- add_frame(e_grid, width = k_resize)
     writePNG("01_E_grid.png", func = plot.image, c_xy = c(480, 640), arg_list = list(x = rotate(e_grid), col = c("#FFFFFF", "#000000", "#0000FF"), axes = FALSE, main = "Буква Е"))
     
     # Letter O
     o <- m
     o[,1] <- 1
     o[,2] <- 1
     o[1,] <- 1
     o[2,] <- 1
     o[nrow(o)-1,] <- 1
     o[nrow(o),] <- 1
     o[, ncol(o)-1] <- 1
     o[, ncol(o)] <- 1
     o_f <- add_frame(o)
     writePNG("01_O.png", func = plot.image, c_xy = c(480, 640), arg_list = list(x = rotate(o_f), col = c("#FFFFFF", "#000000"), axes = FALSE, main = "Буква О"))

     o_grid <- resize(o, k_resize)
     o_grid <- draw_rect(o_grid, c(1, 1), ncol(o)*k_resize, nrow(o)*k_resize, 1*k_resize, 1*k_resize, pen = 2)
     o_grid <- add_frame(o_grid, width = k_resize)
     writePNG("01_O_grid.png", func = plot.image, c_xy = c(480, 640), arg_list = list(x = rotate(o_grid), col = c("#FFFFFF", "#000000", "#0000FF"), axes = FALSE, main = "Буква О"))
     
     # Result
     f <- file("pic/01_res.txt", "w")
     unk_a <- sum(ifelse(unk == a, 1, 0))
     cat("? -> A = ", unk_a, " (", 100*unk_a/length(unk),"%)\n", file = f)
     unk_e <- sum(ifelse(unk == e, 1, 0))
     cat("? -> E = ", unk_e, " (", 100*unk_e/length(unk),"%)\n", file = f)
     unk_o <- sum(ifelse(unk == o, 1, 0))
     cat("? -> O = ", unk_o, " (", 100*unk_o/length(unk),"%)\n", file = f)
     close(f)
     
     # Kernel
     kernel <- matrix(data = 0, nrow = 3, ncol = 3)
     kernel[2,] <- 1
     kernel_r10 <- resize(kernel, 10)
     kernel_r10_grid <- draw_rect(kernel_r10, c(1, 1), 30, 30, 10, 10, pen = 1)
     kernel_r10_grid_red <- draw_rect(kernel_r10_grid, c(1, 10), 30, 11, 10, 11, pen = 2)
     kernel_r10_grid_red_f5 <- add_frame(kernel_r10_grid_red, width = 5)
     writePNG("02_kernel.png", func = plot.image, c_xy = c(500, 500), arg_list = list(x = rotate(kernel_r10_grid_red_f5), col = c("#FFFFFF", "#000000", "#FF0000"), axes = FALSE, main = "Ядро свертки"))
     plot.image.proportional.subdir("pic", "02_kernel_alt.png", kernel_r10_grid_red_f5, c("#FFFFFF", "#000000", "#FF0000"), main = "Ядро свертки", cex.main = 4)
     
     # Convolution
     unk_fm <- subdir_exec("pic", convolution, unk, kernel, "Unknown", append_title_single = " для\nнеизвестной буквы", append_title_joined = " для неизвестной буквы")
     writePNG("03_Unknown-fm_final.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(add_frame(unk_fm$pic, width = 10)), col = unk_fm$col, axes = FALSE, main = "Карта признаков\nдля Unknown"))
     plot.image.proportional.subdir("pic", "03_Unknown-fm_final_alt.png", add_frame(unk_fm$pic, width = 10), unk_fm$col, main = "Карта признаков для\nнеизвестной буквы", cex.main = 3)
     
     a_fm <- subdir_exec("pic", convolution, a, kernel, "A")
     writePNG("03_A-fm_final.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(add_frame(a_fm$pic, width = 10)), col = a_fm$col, axes = FALSE, main = "Карта признаков\nдля буквы А"))
     
     e_fm <- subdir_exec("pic", convolution, e, kernel, "E")
     writePNG("03_E-fm_final.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(add_frame(e_fm$pic, width = 10)), col = e_fm$col, axes = FALSE, main = "Карта признаков\nдля буквы Е"))
     
     o_fm <- subdir_exec("pic", convolution, o, kernel, "O")
     writePNG("03_O-fm_final.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(add_frame(o_fm$pic, width = 10)), col = o_fm$col, axes = FALSE, main = "Карта признаков\nдля буквы О"))
     
     # Result
     f_fm <- file("pic/03_res.txt", "w")
     unk_a <- sum(ifelse(unk_fm$data == a_fm$data, 1, 0))
     cat("? -> A = ", unk_a, " (", 100*unk_a/length(unk_fm$data),"%)\n", file = f_fm)
     unk_e <- sum(ifelse(unk_fm$data == e_fm$data, 1, 0))
     cat("? -> E = ", unk_e, " (", 100*unk_e/length(unk_fm$data),"%)\n", file = f_fm)
     unk_o <- sum(ifelse(unk_fm$data == o_fm$data, 1, 0))
     cat("? -> O = ", unk_o, " (", 100*unk_o/length(unk_fm$data),"%)\n", file = f_fm)
     close(f_fm)
     
     # Downsamling
     unk_down <- subdir_exec("pic", subsampling, unk_fm$mtrx, "Unknown", 2)
     writePNG("04_Unknown-down_final.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(add_frame(add_frame(unk_down$pic, coef = 2), width = 10)), col = unk_down$col, axes = FALSE, main = "Выходная карта\nпризнаков для Unknown"))
     
     a_down <- subdir_exec("pic", subsampling, a_fm$mtrx, "A", 2)
     writePNG("04_A-down_final.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(add_frame(add_frame(a_down$pic, coef = 2), width = 10)), col = a_down$col, axes = FALSE, main = "Выходная карта\nпризнаков для буквы А"))
     
     e_down <- subdir_exec("pic", subsampling, e_fm$mtrx, "E", 2)
     writePNG("04_E-down_final.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(add_frame(add_frame(e_down$pic, coef = 2), width = 10)), col = e_down$col, axes = FALSE, main = "Выходная карта\nпризнаков для буквы Е"))
     
     o_down <- subdir_exec("pic", subsampling, o_fm$mtrx, "O", 2)
     writePNG("04_O-down_final.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(add_frame(add_frame(o_down$pic, coef = 2), width = 10)), col = o_down$col, axes = FALSE, main = "Выходная карта\nпризнаков для буквы О"))
     
     # Result
     f_sub <- file("pic/04_res.txt", "w")
     unk_a <- sum(ifelse(unk_down$data == a_down$data, 1, 0))
     cat("? -> A = ", unk_a, " (", 100*unk_a/length(unk_down$data),"%)\n", file = f_fm)
     unk_e <- sum(ifelse(unk_down$data == e_down$data, 1, 0))
     cat("? -> E = ", unk_e, " (", 100*unk_e/length(unk_down$data),"%)\n", file = f_fm)
     unk_o <- sum(ifelse(unk_down$data == o_down$data, 1, 0))
     cat("? -> O = ", unk_o, " (", 100*unk_o/length(unk_down$data),"%)\n", file = f_fm)
     close(f_sub)
     
     # unk_up <- subdir_exec("pic", subsampling, unk_down$mtrx, "Unknown", 2, downscale = FALSE)
     # writePNG("Unknown-up_final.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(add_frame(unk_up$pic, width = 10)), col = unk_up$col, axes = FALSE, main = "Выходная карта\nпризнаков для Unknown"))
     
     super_magic_wang()
     
     return(NULL)
}

lection11.make()
