# install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# local_path <- "D:/Ubuntu/Share/Курс_Мультимедийные системы и среды"
# setwd(paste0(local_path, "/multimedia_course/Lection/07"))

Sys.setlocale("LC_CTYPE", "russian")
source("../generate.R", encoding="utf-8")
source("../png.R")

# Experiment
lection7.test <- function() {
   oscl <- data.frame.multi_sin(fs = 100, stop = 0.03, dt = 0.0001, amp = 0.5, phi = 0.5)
   writePNG("test_01.png", func = plot.gen.multi_sin, arg_list = list(oscl, title = "Осцилограмма", draw_period = TRUE))
   
   oscl_alt <- data.frame.multi_sin(c( 100,  500,  300), dt = 0.0001, lwd = 2)
   oscl_alt$start <- c(0.00, 0.02, 0.04)
   oscl_alt$stop  <- c(0.02, 0.04, 0.06)
   oscl_alt$amp   <- c( 0.9,  0.3,  0.6)
   oscl_alt$shift <- c(0.00,  0.0005,  0.00)
   oscl_alt$phi   <- c( 0.5,  0.0,  -0.5)
   oscl_alt$lty   <- c( "dashed",  "solid",  "dotted")
   writePNG("test_02_1.png", func = plot.gen.multi_sin, arg_list = list(oscl_alt, title = "Осцилограмма"))
   
   oscl_alt$dt    <- rep(0.001, times = 3)
   writePNG("test_02_2.png", func = plot.gen.multi_sin, arg_list = list(oscl_alt, title = "Осцилограмма", type = "p"))
   
   spm <- data.frame.prg(100, amp = 0.9)
   writePNG("test_03.png", func = plot.gen.prg, arg_list = list(spm, title = "Спектр"))
   
   spm_alt_1 <- data.frame.prg(100, damp = 0.1, amp = 0.9)
   writePNG("test_04_01.png", func = plot.gen.prg, arg_list = list(spm_alt_1, title = "Спектр"))
   
   spm_alt_2 <- data.frame.prg(c(50, 100), damp = 0.1, amp = 0.9)
   writePNG("test_04_02.png", func = plot.gen.prg, arg_list = list(spm_alt_2, title = "Спектр"))
   
   spm_alt_3 <- data.frame.prg(c(75, 100), damp = 0.1, amp = 0.9)
   writePNG("test_04_03.png", func = plot.gen.prg, arg_list = list(spm_alt_3, title = "Спектр"))
   
   spm_alt_4 <- data.frame.prg(c(75, 87), damp = 0.1, duty = 1, amp = 0.9)
   writePNG("test_04_04.png", func = plot.gen.prg, arg_list = list(spm_alt_4, title = "Спектр"))
   
   spm_alt_5 <- data.frame.prg(c(60, 90), damp = 0.05, amp = 0.9)
   writePNG("test_04_05.png", func = plot.gen.prg, arg_list = list(spm_alt_5, title = "Спектр"))
   
   spm_alt_6 <- data.frame.prg(c(75, 100), damp = 0.25, amp = 0.9)
   writePNG("test_04_06.png", func = plot.gen.prg, arg_list = list(spm_alt_6, title = "Спектр"))
   
   spm_alt_7 <- data.frame.prg(60, damp = 0.05, amp = 0.9, pow = 1/4)
   writePNG("test_04_07.png", func = plot.gen.prg, arg_list = list(spm_alt_7, title = "Спектр"))
   
   spm_alt_8 <- data.frame.prg(90, damp = 0.05, amp = 0.9, pow = 1/2)
   writePNG("test_04_08.png", func = plot.gen.prg, arg_list = list(spm_alt_8, title = "Спектр"))
   
   spm_alt_9 <- data.frame.prg(c(60, 90), damp = 0.05, amp = 0.9)
   spm_alt_9$pow <- c(1/4, 1/2)
   writePNG("test_04_09.png", func = plot.gen.prg, arg_list = list(spm_alt_9, title = "Спектр"))
   
   spm_alt_10 <- data.frame.prg(50, damp = 0.05, amp = 0.9, pow = 2)
   writePNG("test_04_10.png", func = plot.gen.prg, arg_list = list(spm_alt_10, title = "Спектр"))
   
   spm_alt_11 <- data.frame.prg(100, damp = 0.05, amp = 0.9, pow = 1.5)
   writePNG("test_04_11.png", func = plot.gen.prg, arg_list = list(spm_alt_11, title = "Спектр"))
   
   spm_alt_12 <- data.frame.prg(c(50, 100), damp = 0.05, amp = 0.9)
   spm_alt_12$pow <- c(2, 1.5)
   writePNG("test_04_12.png", func = plot.gen.prg, arg_list = list(spm_alt_12, title = "Спектр"))
   
   wspm <- data.frame.pws(c( 100,  500,  300))
   wspm$start <- c(0.00, 0.02, 0.05)
   wspm$stop <-  c(0.02, 0.05, 0.07)
   wspm$amp <-   c( 0.9,  0.3,  0.6)
   writePNG("test_05.png", func = plot.gen.pws, arg_list = list(wspm))
   
   wspm_alt_1 <- data.frame.pws(c( 100,  500,  300))
   wspm_alt_1$start <- c(0.00, 0.02, 0.05)
   wspm_alt_1$stop <-  c(0.02, 0.05, 0.07)
   wspm_alt_1$amp <-   c( 0.9,  0.6,  0.9)
   wspm_alt_1$damp <-   c( 0.5,  0.25,  0.5)
   wspm_alt_1$pow <-   c( 1/2,  1,  2)
   wspm_alt_1$duty <-   c( 25,  50,  25)
   writePNG("test_06_01.png", func = plot.gen.pws, arg_list = list(wspm_alt_1))
   
   wspm_alt_2 <- data.frame.pws(c( 100,  500,  300))
   wspm_alt_2$start <- c(0.00, 0.021, 0.051)
   wspm_alt_2$stop <-  c(0.019, 0.049, 0.069)
   wspm_alt_2$amp <-   c( 0.9,  0.9,  0.9)
   wspm_alt_2$damp <-   c( 0.25,  0.15,  0.25)
   wspm_alt_2$pow <-   c( 1/2,  1,  2)
   writePNG("test_06_02.png", func = plot.gen.pws, arg_list = list(wspm_alt_2))
   
   NULL
}

lection7.make <- function() {
   # lection7.test()
   
   c_xy <- c(1920, 750)

   oscl <- data.frame.multi_sin(100, start = -1, stop = 1, amp = 0.9)
   writePNG("01.png", func = plot.gen.multi_sin, arg_list = list(oscl, title = "Осцилограмма", draw_period = TRUE))
   
   spm <- data.frame.prg(100, amp = 0.9)
   writePNG("02.png", func = plot.gen.prg, arg_list = list(spm, title = "Спектр"))
   
   oscl <- data.frame.multi_sin(c(100, 150, 200), start = -1, stop = 1, amp = 0.3)
   writePNG("03.png", func = plot.gen.multi_sin, arg_list = list(oscl, draw_combined = TRUE, title = "Осцилограмма"))
   
   oscl <- data.frame.multi_sin(c(100, 150, 200), start = -1, stop = 1, amp = 0.3)
   writePNG("03_alt.png", func = plot.gen.multi_sin, c_xy = c(1920, 500), arg_list = list(oscl, draw_combined = TRUE, mai = c(1.25, 2.75, 1.25, 1.5)))
   
   oscl <- data.frame.multi_sin(100, start = -1, stop = 1, amp = 0.3)
   writePNG("04.png", func = plot.gen.multi_sin, c_xy = c(1920, 500), arg_list = list(oscl, ylim = c(-0.4, 0.4), draw_period = TRUE, mai = c(1.25, 2.75, 1.25, 1.5)))
   
   oscl <- data.frame.multi_sin(150, start = -1, stop = 1, amp = 0.3)
   writePNG("05.png", func = plot.gen.multi_sin, c_xy = c(1920, 500), arg_list = list(oscl, ylim = c(-0.4, 0.4), draw_period = TRUE, mai = c(1.25, 2.75, 1.25, 1.5)))
   
   oscl <- data.frame.multi_sin(200, start = -1, stop = 1, amp = 0.3)
   writePNG("06.png", func = plot.gen.multi_sin, c_xy = c(1920, 500), arg_list = list(oscl, ylim = c(-0.4, 0.4),draw_period = TRUE, mai = c(1.25, 2.75, 1.25, 1.5)))
   
   for (i in 0:2) {
        oscl <- data.frame.multi_sin(100 + 50 * i, start = -1, stop = 1)
        name <- paste0("0", 7 + 2*i)
        writePNG(paste0(substr(name, nchar(name) - 1, nchar(name)), ".png"), func = plot.gen.multi_sin, c_xy = c_xy, arg_list = list(oscl, title = "Осцилограмма", draw_period = TRUE))
        
        spm <- data.frame.prg(100 + 50 * i)
        name <- paste0("0", 7 + 2*i + 1)
        writePNG(paste0(substr(name, nchar(name) - 1, nchar(name)), ".png"), func = plot.gen.prg, c_xy = c_xy, arg_list = list(spm, xmax = 250, title = "Спектр"))
   }
   
   oscl <- data.frame.multi_sin(100, start = -1, stop = 1, amp = 0.3)
   writePNG("13.png", func = plot.gen.multi_sin, arg_list = list(oscl, draw_period = TRUE))
   
   oscl <- data.frame.multi_sin(c(100, 200), start = -1, stop = 1)
   oscl$amp <- c( 0.3,  0.9)
   oscl$phi <- c( 0.0,  pi)
   oscl$lty <- c( "solid",  "dotted")
   oscl$lwd <- c( 3,  2)
   writePNG("14.png", func = plot.gen.multi_sin, arg_list = list(oscl, title = "Осцилограмма"))
   
   oscl$phi <- c( 0.0,  0.0)
   writePNG("15.png", func = plot.gen.multi_sin, arg_list = list(oscl, title = "Осцилограмма"))
   
   oscl$fs <- c( 100,  100)
   writePNG("16.png", func = plot.gen.multi_sin, arg_list = list(oscl, title = "Осцилограмма"))
   
   oscl$amp <- c( 0.3,  0.5)
   writePNG("17.png", func = plot.gen.multi_sin, arg_list = list(oscl, title = "Осцилограмма"))
   
   spm <- data.frame.prg(100, amp = 0.3)
   writePNG("18.png", func = plot.gen.prg, arg_list = list(spm, title = "Спектр"))
   
   oscl <- data.frame.multi_sin(c(100, 150, 200), start = -1, stop = 1, amp = 0.3)
   writePNG("19.png", func = plot.gen.multi_sin, c_xy = c_xy, arg_list = list(oscl, draw_combined = TRUE, title = "Осцилограмма"))
   
   spm <- data.frame.prg(c(100, 150, 200), amp = 0.3)
   writePNG("20.png", func = plot.gen.prg, c_xy = c_xy, arg_list = list(spm, title = "Спектр"))
   
   oscl <- data.frame.multi_sin(c( 100,  150,  200))
   oscl$start <- c(0.00, 0.02, 0.04)
   oscl$stop  <- c(0.02, 0.04, 0.06)
   oscl$amp   <- c( 0.3,  0.9,  0.6)
   writePNG("21.png", func = plot.gen.multi_sin, arg_list = list(oscl, title = "Осцилограмма"))
   
   writePNG.open("22.png", func = plot.gen.multi_sin, arg_list = list(oscl, title = "Осцилограмма"))
   for (i in 0:3)
      writePNG.add("22.png", func = plot.lines, arg_list = list(0.02 * i *c(1, 1), c(-2, 2)))
   writePNG.close("22.png", func = NULL)
   
   writePNG.open("22_alt.png", func = plot.gen.multi_sin, c_xy = c_xy, arg_list = list(oscl, title = "Осцилограмма"))
   for (i in 0:3)
      writePNG.add("22_alt.png", func = plot.lines, arg_list = list(0.02 * i *c(1, 1), c(-2, 2)))
   writePNG.close("22_alt.png", func = NULL)
   
   writePNG.open("23.png", func = plot.gen.sin, arg_list = list(0, title ="Оконная функция", type = "n"))
   x <- c(-1, 0, 0, 0.02, 0.02, 1)
   y <- c( 0, 0, 1,    1,    0, 0)
   writePNG.add("23.png", func = plot.lines, arg_list = list(x, y, lty = "solid", lwd = 3))
   writePNG.add("23.png", func = plot.lines, arg_list = list(c(-1, 1), c(0, 0), lty = "solid", lwd = 1))
   writePNG.close("23.png", func = NULL)
   
   fs    <- c( 100,  150,  200)
   start <- c(0.00, 0.02, 0.04)
   stop  <- c(0.02, 0.04, 0.06)
   amp   <- c( 0.3,  0.9,  0.6)
   for (i in 1:3) {
      oscl <- data.frame.multi_sin(fs[i], start = start[i], stop = stop[i], amp = amp[i])
      writePNG(paste0(24 + 2*(i - 1), ".png"), func = plot.gen.multi_sin, c_xy = c_xy, arg_list = list(oscl, title = "Осцилограмма"))
      
      spm <- data.frame.prg(fs[i], amp = amp[i])
      writePNG(paste0(24 + 2*(i - 1) + 1, ".png"), func = plot.gen.prg, arg_list = list(spm, xmax = 250, title = "Спектр"))
      writePNG(paste0(24 + 2*(i - 1) + 1, "_alt.png"), func = plot.gen.prg, c_xy = c_xy, arg_list = list(spm, xmax = 250, title = "Спектр"))
   }
   
   wspm <- data.frame.pws(c( 100,  150,  200))
   wspm$start <- c(0.00, 0.02, 0.04)
   wspm$stop  <- c(0.02, 0.04, 0.06)
   wspm$amp   <- c( 0.3,  0.9,  0.6)
   writePNG("30.png", func = plot.gen.pws, c_xy = c_xy, arg_list = list(wspm))
   
   oscl <- data.frame.multi_sin(c( 25,  150,  200))
   oscl$start <- c(0.00, 0.04, 0.06)
   oscl$stop  <- c(0.04, 0.06, 0.08)
   oscl$amp   <- c( 0.3,  0.9,  0.6)
   writePNG("31.png", func = plot.gen.multi_sin, arg_list = list(oscl, xlim = c(0, 0.08), title = "Осцилограмма"))
   
   writePNG.open("32_1.png", func = plot.gen.multi_sin, c_xy = c_xy, arg_list = list(oscl, xlim = c(0, 0.08), title = "Осцилограмма"))
   for (i in 0:2)
      writePNG.add("32_1.png", func = plot.lines, arg_list = list(0.04 * i *c(1, 1), c(-2, 2)))
   writePNG.close("32_1.png", func = NULL)
   
   writePNG.open("32_2.png", func = plot.gen.multi_sin, c_xy = c_xy, arg_list = list(oscl, xlim = c(0, 0.08), title = "Осцилограмма"))
   for (i in 0:4)
      writePNG.add("32_2.png", func = plot.lines, arg_list = list(0.02 * i *c(1, 1), c(-2, 2)))
   writePNG.close("32_2.png", func = NULL)
   
   wspm <- data.frame.pws(c( 25,  175))
   wspm$start <- c(0.00, 0.04)
   wspm$stop  <- c(0.04, 0.08)
   wspm$amp   <- c( 0.3,  0.75)
   wspm$damp  <- c(   0,  0.35)
   wspm$pow   <- c(   1,  2)
   wspm$duty  <- c(   1,  15)
   writePNG("33_1.png", func = plot.gen.pws, c_xy = c_xy, arg_list = list(wspm, xlim = c(0, 0.08)))
   
   wspm <- data.frame.pws(c( 25, 150, 200))
   wspm$start <- c(0.00, 0.04, 0.06)
   wspm$stop  <- c(0.04, 0.06, 0.08)
   wspm$amp   <- c( 0.3,  0.9, 0.6)
   wspm$damp  <- c( 0.35,    0,   0)
   wspm$pow   <- c(   3,    1,   1)
   wspm$duty  <- c(  15,    1,   1)
   writePNG("33_2.png", func = plot.gen.pws, c_xy = c_xy, arg_list = list(wspm, xlim = c(0, 0.08)))
   
   t  <- seq(-1, 1, 0.0001)
   fs <- c(25, 100, 200)
   for (i in 1:3) {
      y1 <- 2 * sin(2 * 2*pi*t * fs[i])/(2 * 2*pi*t * fs[i])
      y2 <- sin(2*pi*t * fs[i])/(2*pi*t * fs[i])
      y <- y1 - y2
      
      title <- paste0("Вейвлет Майера ", fs[i], "Гц")
      oscl_png <- toString(34 + 2*(i - 1))
      prg_png  <- toString(34 + 2*(i - 1) + 1)
      
      oscl_png_name <- paste0(oscl_png, ".png")
      prg_png_name <- paste0(prg_png, ".png")
      resolution <- g_png_resolution
      
      for (j in 0:1) {
           writePNG.open(oscl_png_name, func = plot.gen.sin, c_xy = resolution, arg_list = list(0, xlim = c(-0.03, 0.03), title = paste0("Осцилограмма\n", title), type = "n"))
           writePNG.add(oscl_png_name, func = plot.lines, arg_list = list(t, y, lty = "solid", lwd = 3))
           writePNG.add(oscl_png_name, func = plot.lines, arg_list = list(c(-1, 1), c(0, 0), lty = "solid", lwd = 1))
           writePNG.close(oscl_png_name, func = NULL)
           
           
           spm <- data.frame.prg(fs[i], damp = 0.25)
           writePNG(prg_png_name, func = plot.gen.prg, c_xy = resolution, arg_list = list(spm, xmax = 250, title = paste0("Спектр\n", title)))
           
           oscl_png_name <- paste0(oscl_png, "_alt.png")
           prg_png_name <- paste0(prg_png, "_alt.png")
           resolution <- c_xy
      }
   }
   
   oscl <- data.frame.multi_sin(c( 25,  150,  200))
   oscl$start <- c(0.00, 0.04, 0.06)
   oscl$stop  <- c(0.04, 0.06, 0.08)
   oscl$amp   <- c( 0.3,  0.9,  0.6)
   writePNG.open("40.png", func = plot.gen.multi_sin, c_xy = c_xy, arg_list = list(oscl, xlim = c(0, 0.08), title = "Осцилограмма"))
   writePNG.add("40", func = plot.lines, arg_list = list(c(0, 0), c(-2, 2)))
   writePNG.add("40", func = plot.lines, arg_list = list(c(0.04, 0.04), c(-2, 2)))
   writePNG.add("40", func = plot.lines, arg_list = list(c(0.06, 0.06), c(-2, 2)))
   writePNG.add("40", func = plot.lines, arg_list = list(c(0.08, 0.08), c(-2, 2)))
   writePNG.close("40", func = NULL)

   wspm <- data.frame.pws(c( 25, 150, 200), damp = 0.5, duty = 7, lwd = 5)
   wspm$start <- c(0.00, 0.04, 0.06)
   wspm$stop  <- c(0.04, 0.06, 0.08)
   wspm$amp   <- c( 0.3,  0.9, 0.6)
   wspm$pow   <- c(   3,  0.5, 1.5)
   writePNG("41.png", func = plot.gen.pws, c_xy = c_xy, arg_list = list(wspm, xlim = c(0, 0.08)))
   
   spm <- data.frame.prg(c(51, 100), damp = 0.0275, amp = 0.9, lwd = 2)
   spm$pow <- c(1/4, 1/2)
   writePNG("42.png", func = plot.gen.prg, arg_list = list(spm, title = "Спектр"))

   spm <- data.frame.prg(c(50, 100), damp = 0.0275, amp = 0.9, lwd = 2)
   spm$pow <- c(1/4, 1/2)
   writePNG("43.png", func = plot.gen.prg, arg_list = list(spm, title = "Спектр"))
   
   spm <- data.frame.prg(c(51, 100), damp = 0.05, amp = 0.9, lwd = 2)
   spm$pow <- c(2, 1.5)
   writePNG("44.png", func = plot.gen.prg, arg_list = list(spm, title = "Спектр"))
   
   spm <- data.frame.prg(c(50, 100), damp = 0.05, amp = 0.9, lwd = 2)
   spm$pow <- c(2, 1.5)
   writePNG("45.png", func = plot.gen.prg, arg_list = list(spm, title = "Спектр"))
}

lection7.make()
