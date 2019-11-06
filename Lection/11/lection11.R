# install.packages("caTools") #
library("caTools") # for write.gif

# install.packages("grid") #
library(grid) # for grid.raster to plot RGB

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

Sys.setlocale("LC_CTYPE", "russian")
source("../png.R")

g_mar <- c(0, 0, 5, 0)
g_cex.main <- 4

plot.set.par <- function(cex.main = g_cex.main, mar = g_mar) {
     par(cex.main = cex.main, mar = mar)
}

plot.image <- function(cex.main = g_cex.main, mar = g_mar, ...) {
     plot.set.par(cex.main = cex.main, mar = mar)
     do.call(image, list(...))
}

rotate <- function(x) {
     t(apply(x, 2, rev))
}

add_frame <- function(m, elem = 0, width = 1) {
     col_ins <- matrix(elem, nrow = nrow(m), ncol = width)
     m <- cbind(col_ins, m, col_ins)
     
     row_ins <- matrix(elem, nrow = width, ncol = ncol(m))
     rbind(row_ins, m, row_ins)
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

convolution <- function(ltr, krnl, str_ltr) {
     if (any(sort(unique(as.vector(ltr))) != c(0, 1))) {
          warning("Letter matrix must consists of only 0 and 1")
          return(NULL)
     }
     if (any(sort(unique(as.vector(krnl))) != c(0, 1))) {
          warning("Kernel matrix must consists of only 0 and 1")
          return(NULL)
     }
     
     mainDir <- getwd()
     subDir <- str_ltr
     
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
     
     for (i in 1:(nrow(ltrf)-2)) {
          for (j in 1:(ncol(ltrf)-2)) {
               kernel_1 <- draw_rect(ltrfr, c((j-1)*k_resize+1, (i-1)*k_resize+1), 3*k_resize, 3*k_resize, 1*k_resize, 1*k_resize, pen = pen[length(pen)-1])
               kernel_2 <- draw_rect(kernel_1, c((j-1)*k_resize+1, i*k_resize), 3*k_resize, 1*k_resize+1, 1*k_resize, 1*k_resize+1, pen = pen[length(pen)])

               png_name_left <- paste0(str_ltr, "-kl_i", sprintf("%02d", i), "_j", sprintf("%02d", j), ".png")
               impl.writePNG(png_name_left, func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(kernel_2), col = col, axes = FALSE, main = paste("Свертка с ядром\nдля ", str_ltr)))
               
               #  ----------
               
               data[i, j] <- sum(krnl*ltrf[i:(i+nrow(krnl)-1), j:(j+ncol(krnl)-1)])/sum(krnl)
               
               mtrx[i, j] <- sum(krnl*ltrf[i:(i+nrow(krnl)-1), j:(j+ncol(krnl)-1)])
               mtrx_f <- add_frame(mtrx)
               mtrx_fr <- resize(mtrx_f, k_resize)
               
               feature_1 <- draw_rect(mtrx_fr, c(k_resize+1, k_resize+1), ncol(mtrx)*k_resize, nrow(mtrx)*k_resize, 1*k_resize, 1*k_resize, pen = pen[length(pen)-1])
               pic <- feature_1
               x <- j*k_resize + (if (j == 1) 1 else 0)
               y <- i*k_resize + (if (i == 1) 1 else 0)
               width  <- 1*k_resize + (if (j != 1) 1 else 0)
               height <- 1*k_resize + (if (i != 1) 1 else 0)
               feature_2 <- draw_rect(feature_1, c(x, y), width, height, width, height, pen = pen[length(pen)])
               
               png_name_right <- paste0(str_ltr, "-fm_i", sprintf("%02d", i), "_j", sprintf("%02d", j), ".png")
               impl.writePNG(png_name_right, func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(feature_2), col = col, axes = FALSE, main = paste("Карта признаков\nдля", str_ltr)))

               #  ----------
               
               # Join 2 matrix
               gif.pic <- cbind(kernel_2, matrix(0, nrow = nrow(gif.pic), ncol = k_resize), feature_2)
               gif.all[,,gif.ind] <- resize(gif.pic, 3)
               gif.ind <- gif.ind + 1
               
               png_name_1st_approach <- paste0(str_ltr, "_1(i=", sprintf("%02d", i), ",j=", sprintf("%02d", j), ").png")
               impl.writePNG(png_name_1st_approach, func = plot.image, c_xy = c(960, 640), arg_list = list(cex.main = 4, mar = c(1, 1, 4, 1), x = rotate(gif.pic), col = col, axes = FALSE, main = paste("Свертка для буквы", str_ltr)))

               # Join 2 PNG and plot RGB
               png_name_2nd_approach <- paste0(str_ltr, "_2(i=", sprintf("%02d", i), ",j=", sprintf("%02d", j), ").png")
               png_joined <- array(data = NA, c(640, 960 ,3))
               png_joined[,1:480,] <- readPNG(png_name_left)
               png_joined[,481:960,] <- readPNG(png_name_right)
               impl.writePNG(png_name_2nd_approach, func = grid.raster, c_xy = c(960, 640), arg_list = list(png_joined))
               
               # Raster approach
               # Add additional margin to resulted png
               # https://gis.stackexchange.com/questions/207477/r-export-raster-to-png-without-margin
               # https://stackoverflow.com/questions/27800307/adding-a-picture-to-plot-in-r

               # Raster 3
               # png_name_3nd_approach <- paste0(str_ltr, "_3(i=", sprintf("%02d", i), ",j=", sprintf("%02d", j), ").png")
               # png(png_name_3nd_approach, 960, 640)
               # par(mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0)) # no margins
               # 
               # plot(c(0, 960), c(0, 640), type = "n", xlab = "", ylab = "")
               # rasterImage(readPNG(png_name_left), 0, 0, 480, 640, mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
               # rasterImage(readPNG(png_name_right), 480, 0, 960, 640, mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
               # 
               # dev.off()

               # Raster 4
               # png_name_4nd_approach <- paste0(str_ltr, "_4(i=", sprintf("%02d", i), ",j=", sprintf("%02d", j), ").png")
               # png(png_name_4nd_approach, 960, 640)
               # par(mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0)) # no margins
               # 
               # layout(matrix(1:2, ncol=2, byrow=TRUE)) # layout the plots into a matrix 2 columns, by row
               # 
               # plot.new() # same as plot(NA, xlim=0:1, ylim=0:1, main = NA, sub = NA, xlab = NA, ylab = NA, xaxt="n", yaxt="n", bty="n")
               # rasterImage(readPNG(png_name_left), 0, 0, 1, 1)
               # 
               # plot.new() # same as plot(NA, xlim=0:1, ylim=0:1, main = NA, sub = NA, xlab = NA, ylab = NA, xaxt="n", yaxt="n", bty="n")
               # rasterImage(readPNG(png_name_right), 0, 0, 1, 1)
               # 
               # dev.off()
          }
     }
     
     # Make GIF from raw matrix data
     gif.col <- c(col, rep("#FFFFFF", times = 256-length(col)))
     write.gif(gif.all, paste0(str_ltr, "-0.gif"), col = gif.col, delay = 30)
     
     # Make GIF from png files
     cmnd <- paste('"C:/Program Files/ImageMagick/convert.exe\"', "-delay 30", paste0(str_ltr, "_1*.png"), paste0(str_ltr, "-1.gif"))
     system(cmnd)
     
     cmnd <- paste('"C:/Program Files/ImageMagick/convert.exe\"', "-delay 30", paste0(str_ltr, "_2*.png"), paste0(str_ltr, "-2.gif"))
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
               x <- (j-1)*coef*k_resize + (if (j == 1) 1 else 0)
               y <- (i-1)*coef*k_resize + (if (i == 1) 1 else 0)
               width  <- 1*coef*k_resize + (if (j != 1) 1 else 0)
               height <- 1*coef*k_resize + (if (i != 1) 1 else 0)
               in_2 <- draw_rect(in_1, c(x, y), width, height, width, height, pen = pen[length(pen)])
               in_3 <- add_frame(in_2, 0, k_resize)
               
               png_name_left <- paste0(str_ltr, "-down_in_i", sprintf("%02d", i), "_j", sprintf("%02d", j), ".png")
               impl.writePNG(png_name_left, func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(in_3), col = col, axes = FALSE, main = paste("Входная карта\nпризнаков для ", str_ltr)))
               
               #  ----------
               
               mtrx[i, j] <- max(ltr[((i-1)*coef+1):(i*coef), ((j-1)*coef+1):(j*coef)])
               mtrx_r <- resize(mtrx, k_resize)
               
               out_1 <- draw_rect(mtrx_r, c(1, 1), ncol(mtrx)*k_resize, nrow(mtrx)*k_resize, 1*k_resize, 1*k_resize, pen = pen[length(pen)-1])
               pic <- out_1
               x <- (j-1)*k_resize + (if (j == 1) 1 else 0)
               y <- (i-1)*k_resize + (if (i == 1) 1 else 0)
               width  <- 1*k_resize + (if (j != 1) 1 else 0)
               height <- 1*k_resize + (if (i != 1) 1 else 0)
               out_2 <- draw_rect(out_1, c(x, y), width, height, width, height, pen = pen[length(pen)])
               
               col_ins <- matrix(0, nrow = nrow(out_2), ncol = (ncol(in_2) - ncol(out_2))/2)
               out_3 <- cbind(col_ins, out_2, col_ins)
               row_ins <- matrix(0, nrow = (nrow(in_2) - nrow(out_3))/2, ncol = ncol(out_3))
               out_3 <- rbind(row_ins, out_3, row_ins)
               out_4 <- add_frame(out_3, 0, k_resize)
               
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
     
     return(list(mtrx = mtrx, pic = pic, col = head(col, -1)))
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
               x <- (j-1)*k_resize + (if (j == 1) 1 else 0)
               y <- (i-1)*k_resize + (if (i == 1) 1 else 0)
               width  <- 1*k_resize + (if (j != 1) 1 else 0)
               height <- 1*k_resize + (if (i != 1) 1 else 0)
               in_2 <- draw_rect(in_1, c(x, y), width, height, width, height, pen = pen[length(pen)])
               
               col_ins <- matrix(0, nrow = nrow(in_2), ncol = (k_resize*ncol(mtrx) - ncol(in_2))/2)
               in_3 <- cbind(col_ins, in_2, col_ins)
               row_ins <- matrix(0, nrow = (k_resize*nrow(mtrx) - nrow(in_3))/2, ncol = ncol(in_3))
               in_3 <- rbind(row_ins, in_3, row_ins)
               in_4 <- add_frame(in_3, 0, k_resize)
               
               png_name_left <- paste0(str_ltr, "-up_in_i", sprintf("%02d", i), "_j", sprintf("%02d", j), ".png")
               impl.writePNG(png_name_left, func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(in_4), col = col, axes = FALSE, main = paste("Входная карта\nпризнаков для ", str_ltr)))
               
               #  ----------
               
               mtrx[((i-1)*coef+1):(i*coef), ((j-1)*coef+1):(j*coef)] <- ltr[i, j]
               mtrx_r <- resize(mtrx, k_resize)
               
               out_1 <- draw_rect(mtrx_r, c(1, 1), ncol(mtrx)*k_resize, nrow(mtrx)*k_resize, 1*k_resize, 1*k_resize, pen = pen[length(pen)-1])
               pic <- out_1
               x <- (j-1)*coef*k_resize + (if (j == 1) 1 else 0)
               y <- (i-1)*coef*k_resize + (if (i == 1) 1 else 0)
               width  <- 1*coef*k_resize + (if (j != 1) 1 else 0)
               height <- 1*coef*k_resize + (if (i != 1) 1 else 0)
               out_2 <- draw_rect(out_1, c(x, y), width, height, width, height, pen = pen[length(pen)])
               out_3 <- add_frame(out_2, 0, k_resize)
               
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
     
     return(list(mtrx = mtrx, pic = pic, col = head(col, -1)))
}

magic_wang_DFS <- function(mtrx, x, y, delta = 1) {
   
   
}

magic_wang <- function(mtrx, x, y, delta = 1) {
   map <- array(data = 0, dim = dim(mtrx))
   
   map <- matrix(0, nrow = nrow(mtrx), ncol = ncol(mtrx))
   
   magic_wang_search(map, mtrx[y, x], mtrx, y, x, rep(T, 4), 1)
   
   return(map)
}

mw <- NA

magic_wang_draw <- function(row, col, direction = 0) {
   k_resize <- 10
   color <- c("#FFFFFF", "#000000", "#00FF00", "#0000FF", "#FF0000")
   mtrx_r <- resize(mw$mtrx, k_resize)
   mtrx_ <- draw_rect(mtrx_r, c(1, 1), ncol(mw$mtrx)*k_resize, nrow(mw$mtrx)*k_resize, 1*k_resize, 1*k_resize, pen = 3)
   x <- (col-1)*k_resize + (if (col == 1) 1 else 0)
   y <- (row-1)*k_resize + (if (row == 1) 1 else 0)
   width  <- 1*k_resize + (if (col != 1) 1 else 0)
   height <- 1*k_resize + (if (row != 1) 1 else 0)
   mtrx_ <- draw_rect(mtrx_, c(x, y), width, height, width, height, pen = 4, notdrawside = direction)
   mtrx_ <- add_frame(mtrx_, width = k_resize)
   map_r <- resize(mw$map, k_resize)
   map_ <- draw_rect(map_r, c(1, 1), ncol(mw$map)*k_resize, nrow(mw$map)*k_resize, 1*k_resize, 1*k_resize, pen = 3)
   x <- (col-1)*k_resize + (if (col == 1) 1 else 0)
   y <- (row-1)*k_resize + (if (row == 1) 1 else 0)
   width  <- 1*k_resize + (if (col != 1) 1 else 0)
   height <- 1*k_resize + (if (row != 1) 1 else 0)
   map_ <- draw_rect(map_, c(x, y), width, height, width, height, pen = 4, notdrawside = direction)
   map_ <- add_frame(map_, width = k_resize)
   pic <- cbind(mtrx_, matrix(0, nrow = nrow(mtrx_), ncol = k_resize), map_)
   png(sprintf("%s_%03d.png", mw$name, mw$ind), 960, 640)
   mw$ind <<- mw$ind + 1
   par(cex.main = 4, mar = c(0, 0, 5, 0))
   image(x = rotate(pic), col = color, axes = FALSE, main = "Поиск границы объекта")
   dev.off()
}

magic_wang_search <- function(row, col) {
   if (abs(mw$mtrx[row, col]-mw$value) >= mw$delta) {
      mw$map[row, col] <<- 1
      magic_wang_draw(row, col)
      return(NULL)
   } else {
      mw$map[row, col] <<- 2
      magic_wang_draw(row, col, 1)
   }
   if (row < nrow(mw$mtrx)) {
      if (mw$map[row+1, col] == 0)
         magic_wang_search(row+1, col)
   } else
      mw$map[row, col] <<- 1
   magic_wang_draw(row, col, 4)
   if (col < ncol(mw$mtrx)) {
      if (mw$map[row, col+1] == 0)
         magic_wang_search(row, col+1)
   } else
      mw$map[row, col] <<- 1
   magic_wang_draw(row, col, 3)
   if (row > 1) {
      if (mw$map[row-1, col] == 0)
         magic_wang_search(row-1, col)
   } else
      mw$map[row, col] <<- 1
   magic_wang_draw(row, col, 2)
   if (col > 1) {
      if (mw$map[row, col-1] == 0)
         magic_wang_search(row, col-1)
   } else
      mw$map[row, col] <<- 1
   magic_wang_draw(row, col)
   return(NULL)
}

magic_wang <- function(mtrx, x, y, delta = 1, subdir = "test") {
   mw <<- new.env()
   mw$mtrx <<- mtrx
   mw$delta <<- delta
   mw$value <<- mtrx[y, x]
   mw$name <<- "circle"
   mw$map <<- matrix(0, nrow = nrow(mtrx), ncol = ncol(mtrx))
   mw$ind <<- 1
   subdir_exec(subdir, magic_wang_search, y, x)
   cmnd <- paste('"C:/Program Files/ImageMagick/convert.exe\"', "-delay 30", paste0(subdir, "/", mw$name, "_*.png"), paste0(mw$name, ".gif"))
   system(cmnd)
   border <- matrix(0, nrow = nrow(mtrx), ncol = ncol(mtrx))
   border[mw$map == 1] <- 1
   mask <- matrix(0, nrow = nrow(mtrx), ncol = ncol(mtrx))
   mask[mw$map == 2] <- 1
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
   crcl <- add_frame(crcl, width = 2)

   subdir <- "test"
   ret <- subdir_exec("pic", magic_wang, crcl, 6, 6, subdir = subdir)
   cmnd <- paste('"C:/Program Files/ImageMagick/convert.exe\"', "-delay 30", paste0(subdir, "/", mw$name, "_*.png"), paste0(mw$name, ".gif"))
   system(cmnd)
   
   # old code
   
   # Letter Circle
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
   crcl_f <- add_frame(crcl)
   writePNG("3_cicrle.png", func = plot.image, c_xy = c(480, 640), arg_list = list(x = rotate(crcl_f), col = c("#FFFFFF", "#000000"), axes = FALSE, main = "Разорванный круг"))

   x <- magic_wang_DFS(crcl, 6, 6)
   
   k_resize <- 10
   
   crcl_r <- resize(crcl, k_resize)
   crcl_ <- draw_rect(crcl_r, c(1, 1), ncol(crcl)*k_resize, nrow(crcl)*k_resize, 1*k_resize, 1*k_resize, pen = 2)
   png("2_cicrle_.png", 480, 640)
   par(cex.main = 4, mar = c(0, 0, 5, 0))
   image(x = rotate(add_frame(crcl_, width = k_resize)), col = crcl_down$col, axes = FALSE, main = "Разорванный круг")
   dev.off()
   
   crcl_down <- subsampling(crcl, "Circle", 2)
   crcl_down_ <- crcl_down$pic
   col_ins <- matrix(0, nrow = nrow(crcl_down_), ncol = (k_resize*ncol(crcl) - ncol(crcl_down_))/2)
   crcl_down_ <- cbind(col_ins, crcl_down_, col_ins)
   row_ins <- matrix(0, nrow = (k_resize*nrow(crcl) - nrow(crcl_down_))/2, ncol = ncol(crcl_down_))
   crcl_down_ <- rbind(row_ins, crcl_down_, row_ins)
   png("3_cicrle_down.png", 480, 640)
   par(cex.main = 4, mar = c(0, 0, 5, 0))
   image(x = rotate(add_frame(crcl_down_, width = k_resize)), col = crcl_down$col, axes = FALSE, main = "Уменьшили в 2 раза")
   dev.off()
   
   crcl_down <- subsampling(crcl, "Circle", 2)
   crcl_down_ <- crcl_down$pic
   crcl_down_[(2*k_resize):(4*k_resize), 1*k_resize] <- 3
   crcl_down_[(2*k_resize):(4*k_resize), 5*k_resize] <- 3
   crcl_down_[(1*k_resize):(2*k_resize), 2*k_resize] <- 3
   crcl_down_[(4*k_resize):(5*k_resize), 2*k_resize] <- 3
   crcl_down_[(1*k_resize):(2*k_resize), 4*k_resize] <- 3
   crcl_down_[(4*k_resize):(5*k_resize), 4*k_resize] <- 3
   crcl_down_[1*k_resize, (2*k_resize):(4*k_resize)] <- 3
   crcl_down_[5*k_resize, (2*k_resize):(4*k_resize)] <- 3
   crcl_down_[2*k_resize, (1*k_resize):(2*k_resize)] <- 3
   crcl_down_[2*k_resize, (4*k_resize):(5*k_resize)] <- 3
   crcl_down_[4*k_resize, (1*k_resize):(2*k_resize)] <- 3
   crcl_down_[4*k_resize, (4*k_resize):(5*k_resize)] <- 3
   col_ins <- matrix(0, nrow = nrow(crcl_down_), ncol = (k_resize*ncol(crcl) - ncol(crcl_down_))/2)
   crcl_down_ <- cbind(col_ins, crcl_down_, col_ins)
   row_ins <- matrix(0, nrow = (k_resize*nrow(crcl) - nrow(crcl_down_))/2, ncol = ncol(crcl_down_))
   crcl_down_ <- rbind(row_ins, crcl_down_, row_ins)
   png("4_cicrle_down_sel.png", 480, 640)
   par(cex.main = 4, mar = c(0, 0, 5, 0))
   image(x = rotate(add_frame(crcl_down_, width = k_resize)), col = c(crcl_down$col, "#FF0000"), axes = FALSE, main = "Уменьшили в 2 раза")
   dev.off()
   
   crcl_up <- subsampling(crcl_down$mtrx, "Circle", 2, downscale = FALSE)
   crcl_up_ <- crcl_up$pic
   crcl_up_[(4*k_resize):(8*k_resize), 2*k_resize] <- 3
   crcl_up_[(4*k_resize):(8*k_resize), 10*k_resize] <- 3
   crcl_up_[(2*k_resize):(4*k_resize), 4*k_resize] <- 3
   crcl_up_[(8*k_resize):(10*k_resize), 4*k_resize] <- 3
   crcl_up_[(2*k_resize):(4*k_resize), 8*k_resize] <- 3
   crcl_up_[(8*k_resize):(10*k_resize), 8*k_resize] <- 3
   crcl_up_[2*k_resize, (4*k_resize):(8*k_resize)] <- 3
   crcl_up_[10*k_resize, (4*k_resize):(8*k_resize)] <- 3
   crcl_up_[4*k_resize, (2*k_resize):(4*k_resize)] <- 3
   crcl_up_[4*k_resize, (8*k_resize):(10*k_resize)] <- 3
   crcl_up_[8*k_resize, (2*k_resize):(4*k_resize)] <- 3
   crcl_up_[8*k_resize, (8*k_resize):(10*k_resize)] <- 3
   png("5_cicrle_up_sel.png", 480, 640)
   par(cex.main = 4, mar = c(0, 0, 5, 0))
   image(x = rotate(add_frame(crcl_up_, width = k_resize)), col = c(crcl_up$col, "#FF0000") , axes = FALSE, main = "Увеличили в 2 раза")
   dev.off()
   
   crcl_sel <- crcl_
   crcl_sel[(4*k_resize):(8*k_resize), 2*k_resize] <- 3
   crcl_sel[(4*k_resize):(8*k_resize), 10*k_resize] <- 3
   crcl_sel[(2*k_resize):(4*k_resize), 4*k_resize] <- 3
   crcl_sel[(8*k_resize):(10*k_resize), 4*k_resize] <- 3
   crcl_sel[(2*k_resize):(4*k_resize), 8*k_resize] <- 3
   crcl_sel[(8*k_resize):(10*k_resize), 8*k_resize] <- 3
   crcl_sel[2*k_resize, (4*k_resize):(8*k_resize)] <- 3
   crcl_sel[10*k_resize, (4*k_resize):(8*k_resize)] <- 3
   crcl_sel[4*k_resize, (2*k_resize):(4*k_resize)] <- 3
   crcl_sel[4*k_resize, (8*k_resize):(10*k_resize)] <- 3
   crcl_sel[8*k_resize, (2*k_resize):(4*k_resize)] <- 3
   crcl_sel[8*k_resize, (8*k_resize):(10*k_resize)] <- 3
   png("6_cicrle_sel.png", 480, 640)
   par(cex.main = 4, mar = c(0, 0, 5, 0))
   image(x = rotate(add_frame(crcl_sel, width = k_resize)), col = c(crcl_up$col, "#FF0000") , axes = FALSE, main = "Разорванный круг")
   dev.off()
   
   crcl_fill <- crcl_sel
   x <- crcl_fill[(4*k_resize):(8*k_resize), (2*k_resize):(10*k_resize)]
   x[x==0] <- 4
   crcl_fill[(4*k_resize):(8*k_resize), (2*k_resize):(10*k_resize)] <- x
   x <- crcl_fill[(2*k_resize):(10*k_resize),(4*k_resize):(8*k_resize)]
   x[x==0] <- 4
   crcl_fill[(2*k_resize):(10*k_resize),(4*k_resize):(8*k_resize)] <- x
   png("7_cicrle_sel_1.png", 480, 640)
   par(cex.main = 4, mar = c(0, 0, 5, 0))
   image(x = rotate(add_frame(crcl_fill, width = k_resize)), col = c(crcl_up$col, "#FF0000", "#00FF00") , axes = FALSE, main = "Разорванный круг")
   dev.off()
   
   crcl_fill_ <- crcl_
   crcl_fill_[(4*k_resize):(8*k_resize), 2*k_resize] <- 3
   crcl_fill_[(4*k_resize):(8*k_resize), 10*k_resize] <- 3
   crcl_fill_[(2*k_resize):(3*k_resize), 4*k_resize] <- 3
   crcl_fill_[(3*k_resize):(4*k_resize), 3*k_resize] <- 3
   crcl_fill_[(8*k_resize):(9*k_resize), 3*k_resize] <- 3
   crcl_fill_[(9*k_resize):(10*k_resize), 4*k_resize] <- 3
   crcl_fill_[(2*k_resize):(3*k_resize), 8*k_resize] <- 3
   crcl_fill_[(3*k_resize):(4*k_resize), 9*k_resize] <- 3
   crcl_fill_[(8*k_resize):(9*k_resize), 9*k_resize] <- 3
   crcl_fill_[(9*k_resize):(10*k_resize), 8*k_resize] <- 3
   crcl_fill_[2*k_resize, (4*k_resize):(8*k_resize)] <- 3
   crcl_fill_[10*k_resize, (4*k_resize):(8*k_resize)] <- 3
   crcl_fill_[4*k_resize, (2*k_resize):(3*k_resize)] <- 3
   crcl_fill_[3*k_resize, (3*k_resize):(4*k_resize)] <- 3
   crcl_fill_[3*k_resize, (8*k_resize):(9*k_resize)] <- 3
   crcl_fill_[4*k_resize, (9*k_resize):(10*k_resize)] <- 3
   crcl_fill_[8*k_resize, (2*k_resize):(3*k_resize)] <- 3
   crcl_fill_[9*k_resize, (3*k_resize):(4*k_resize)] <- 3
   crcl_fill_[9*k_resize, (8*k_resize):(9*k_resize)] <- 3
   crcl_fill_[8*k_resize, (9*k_resize):(10*k_resize)] <- 3
   x <- crcl_fill_[(4*k_resize):(8*k_resize), (2*k_resize):(10*k_resize)]
   x[x==0] <- 4
   crcl_fill_[(4*k_resize):(8*k_resize), (2*k_resize):(10*k_resize)] <- x
   x <- crcl_fill_[(2*k_resize):(10*k_resize),(4*k_resize):(8*k_resize)]
   x[x==0] <- 4
   crcl_fill_[(2*k_resize):(10*k_resize),(4*k_resize):(8*k_resize)] <- x
   png("8_cicrle_sel_2.png", 480, 640)
   par(cex.main = 4, mar = c(0, 0, 5, 0))
   image(x = rotate(add_frame(crcl_fill_, width = k_resize)), col = c(crcl_up$col, "#FF0000", "#00FF00") , axes = FALSE, main = "Разорванный круг")
   dev.off()
   
   return(NULL)
}

lection11.make <- function() {
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
     writePNG("1_unk.png", func = plot.image, c_xy = c(480, 640), arg_list = list(x = rotate(unk_f), col = c("#FFFFFF", "#000000"), axes = FALSE, main = "Буква ?"))
     
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
     writePNG("1_A.png", func = plot.image, c_xy = c(480, 640), arg_list = list(x = rotate(a_f), col = c("#FFFFFF", "#000000"), axes = FALSE, main = "Буква А"))

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
     writePNG("1_E.png", func = plot.image, c_xy = c(480, 640), arg_list = list(x = rotate(e_f), col = c("#FFFFFF", "#000000"), axes = FALSE, main = "Буква Е"))

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
     writePNG("1_O.png", func = plot.image, c_xy = c(480, 640), arg_list = list(x = rotate(o_f), col = c("#FFFFFF", "#000000"), axes = FALSE, main = "Буква О"))

     f <- file("pic/1_res.txt", "w")
     unk_a <- sum(ifelse(unk == a, 1, 0))
     cat("? -> A = ", unk_a, " (", 100*unk_a/length(unk),"%)\n", file = f)
     unk_e <- sum(ifelse(unk == e, 1, 0))
     cat("? -> E = ", unk_e, " (", 100*unk_e/length(unk),"%)\n", file = f)
     unk_o <- sum(ifelse(unk == o, 1, 0))
     cat("? -> O = ", unk_o, " (", 100*unk_o/length(unk),"%)\n", file = f)
     close(f)
     
     kernel <- matrix(data = 0, nrow = 3, ncol = 3)
     kernel[2,] <- 1
     kernel_r10 <- resize(kernel, 10)
     kernel_r10_grid <- draw_rect(kernel_r10, c(1, 1), 30, 30, 10, 10, pen = 1)
     kernel_r10_grid_red <- draw_rect(kernel_r10_grid, c(1, 10), 30, 11, 10, 11, pen = 2)
     kernel_r10_grid_red_f5 <- add_frame(kernel_r10_grid_red, width = 5)
     writePNG("2_kernel.png", func = plot.image, c_xy = c(500, 500), arg_list = list(x = rotate(kernel_r10_grid_red_f5), col = c("#FFFFFF", "#000000", "#FF0000"), axes = FALSE, main = "Ядро свертки"))

     unk_fm <- subdir_exec("pic", convolution, unk, kernel, "Unknown")
     writePNG("Unknown-fm_final.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(unk_fm$pic), col = unk_fm$col, axes = FALSE, main = "Карта признаков\nдля Unknown"))
     
     # a_fm <- convolution(a, t, "a")
     # e_fm <- convolution(e, t, "e")
     # o_fm <- convolution(o, t, "o")
     # 
     # f_fm <- file("pic/2_res.txt", "w")
     # unk_a <- sum(ifelse(unk_fm$data == a_fm$data, 1, 0))
     # cat("? -> A = ", unk_a, " (", 100*unk_a/length(unk_fm$data),"%)\n", file = f_fm)
     # unk_e <- sum(ifelse(unk_fm$data == e_fm$data, 1, 0))
     # cat("? -> E = ", unk_e, " (", 100*unk_e/length(unk_fm$data),"%)\n", file = f_fm)
     # unk_o <- sum(ifelse(unk_fm$data == o_fm$data, 1, 0))
     # cat("? -> O = ", unk_o, " (", 100*unk_o/length(unk_fm$data),"%)\n", file = f_fm)
     # close(f_fm)
     
     unk_sub <- subdir_exec("pic", subsampling, unk_fm$mtrx, "Unknown", 2)
     writePNG("Unknown-down_final.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(add_frame(unk_sub$pic, width = 40)), col = unk_sub$col, axes = FALSE, main = "Выходная карта\nпризнаков для Unknown"))
     
     # a_sub <- subsampling(a_fm$mtrx, "a", 2)
     # e_sub <- subsampling(e_fm$mtrx, "e", 2)
     # o_sub <- subsampling(o_fm$mtrx, "o", 2)
     # 
     # f_sub <- file("res_3.txt", "w")
     # unk_a <- sum(ifelse(unk_sub$data == a_sub$data, 1, 0))
     # cat("? -> A = ", unk_a, " (", 100*unk_a/length(unk_sub$data),"%)\n", file = f_fm)
     # unk_e <- sum(ifelse(unk_sub$data == e_sub$data, 1, 0))
     # cat("? -> E = ", unk_e, " (", 100*unk_e/length(unk_sub$data),"%)\n", file = f_fm)
     # unk_o <- sum(ifelse(unk_sub$data == o_sub$data, 1, 0))
     # cat("? -> O = ", unk_o, " (", 100*unk_o/length(unk_sub$data),"%)\n", file = f_fm)
     # close(f_sub)
     
     unk_up <- subdir_exec("pic", subsampling, unk_sub$mtrx, "Unknown", 2, downscale = FALSE)
     writePNG("Unknown-up_final.png", func = plot.image, c_xy = c(480, 640), arg_list = list(cex.main = 3, mar = c(1, 1, 7, 1), x = rotate(unk_up$pic), col = unk_up$col, axes = FALSE, main = "Выходная карта\nпризнаков для Unknown"))
     
     super_magic_wang()
     
     return(NULL)
}

lection8.make()
