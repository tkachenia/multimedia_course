local_path <- "D:/Ubuntu/Share/Курс_Мультимедийные системы и среды"

setwd(paste0(local_path, "/multimedia_course/Lection/06"))
Sys.setlocale("LC_CTYPE", "russian")
source("../generate.R", encoding="utf-8")
source("../png.R")

# Experiment
lection6.make <- function() {
     writePNG("01.png", func = plot.gen.sin, arg_list = list(100, amp = 0.9, draw_ox = TRUE, lwd = 3))
     writePNG("02.png", func = plot.gen.const, arg_list = list(0.75, draw_ox = TRUE, lwd = 3))
     writePNG("03.png", func = plot.gen.square, arg_list = list(100, amp = 0.9, draw_ox = TRUE, lwd = 3))
     writePNG("04.png", func = plot.gen.square, arg_list = list(100, amp = 0.9, draw_ox = TRUE, draw_pattern_sin = TRUE, lwd = 3))
     
     writePNG("05.png", func = plot.gen.sin, arg_list = list(100, dt = 0.001, draw_ox = TRUE, draw_pattern_sin = TRUE, draw_period = TRUE, type = "p", lwd = 2))
     writePNG("06.png", func = plot.gen.sin, arg_list = list(100, dt = 0.0025, draw_ox = TRUE, draw_pattern_sin = TRUE, draw_period = TRUE, type = "p", lwd = 2))
     writePNG("07.png", func = plot.gen.sin, arg_list = list(100, dt = 0.005, shift = 0.005/2, draw_ox = TRUE, draw_pattern_sin = TRUE, draw_period = TRUE, type = "p", lwd = 2))
     writePNG("08.png", func = plot.gen.sin, arg_list = list(100, dt = 0.01*(3/2), shift = 0.005/2, draw_ox = TRUE, draw_pattern_sin = TRUE, draw_period = TRUE, type = "p", lwd = 2))
     
     writePNG.open("09.png", func = plot.gen.sin, arg_list = list(100, dt = 0.01*(3/2), shift= 0.005/2, fs_print = round(100/3, 1), draw_ox = TRUE, type = "p", lwd = 2))
     t0 <- seq(-1, 2, 0.0001)
     y0 <- sin(2*pi*100/3*(t0 + 0.015))
     writePNG.add("09.png", func = plot.lines, arg_list = list(t0, y0, lwd = 2))
     for (i in 0:2)
          writePNG.add("09.png", func = plot.lines, arg_list = list(0.03 * i *c(1, 1), c(-2, 2)))
     writePNG.close("09.png", func = NULL)
     
     # Дескретизация
     writePNG.open("10_1.png", func = plot.gen.sin, arg_list = list(100, title ="Дескретизация", lwd = 2))
     for (i in 0:30)
          writePNG.add("10_1.png", func = plot.lines, arg_list = list(c(i/(5*100), i/(5*100)), c(-2, 2)))
     writePNG.close("10_1.png", func = NULL)
     
     writePNG.open("11.png", func = plot.gen.sin, arg_list = list(100, dt = 0.002, title ="Дескретизация", type = "p", lwd = 2))
     for (i in 0:30)
          writePNG.add("11.png", func = plot.lines, arg_list = list(c(i/(5*100), i/(5*100)), c(-2, 2)))
     writePNG.close("11.png", func = NULL)
     
     # Квантование
     writePNG.open("10_2.png", func = plot.gen.sin, arg_list = list(100, title ="Квантование", lwd = 2))
     for (i in 0:15)
          writePNG.add("10_2.png", func = plot.lines, arg_list = list(c(-1, 1), c(-1 + (2/15)*i, -1 + (2/15)*i)))
     writePNG.close("10_2.png", func = NULL)
     
     writePNG.open("12.png", func = plot.gen.sin, arg_list = list(100, dt = 0.002, title ="Квантование", type = "p", lwd = 2))
     for (i in 0:30) {
          writePNG.add("12.png", func = plot.lines, arg_list = list(c(i/(5*100), i/(5*100)), c(-2, 2)))
          writePNG.add("12.png", func = plot.lines, arg_list = list(c(-1, 1), c(-1 + (2/15)*i, -1 + (2/15)*i)))
     }
     writePNG.close("12.png", func = NULL)
     
     # АЦП
     writePNG.open("13.png", func = plot.gen.sin, arg_list = list(100, dt = 0.002, title ="АЦП", type = "p", lwd = 2))
     for (i in 0:30) {
          writePNG.add("13.png", func = plot.lines, arg_list = list(c(i/(5*100), i/(5*100)), c(-2, 2)))
          writePNG.add("13.png", func = plot.lines, arg_list = list(c(-1, 1), c(-1 + (2/15)*i, -1 + (2/15)*i)))
     }
     writePNG.add("13.png", func = plot.points, arg_list = list(0.000, -1 + (2/15)* 7))
     writePNG.add("13.png", func = plot.points, arg_list = list(0.002, -1 + (2/15)*15))
     writePNG.add("13.png", func = plot.points, arg_list = list(0.004, -1 + (2/15)*12))
     writePNG.add("13.png", func = plot.points, arg_list = list(0.006, -1 + (2/15)* 3))
     writePNG.add("13.png", func = plot.points, arg_list = list(0.008, -1 + (2/15)* 0))
     writePNG.close("13.png", func = NULL)
     
     px <- seq(0, 0.06, 0.002)
     py <- rep(-1 + c((2/15)* 7, (2/15)*15, (2/15)*12, (2/15)* 3, (2/15)* 0), length.out = 31)
     
     writePNG.open("14.png", func = plot.gen.sin, arg_list = list(100, title ="АЦП", type = "n", lwd = 3))
     for (i in 0:30) {
          writePNG.add("14.png", func = plot.lines, arg_list = list(c(i/(5*100), i/(5*100)), c(-2, 2)))
          writePNG.add("14.png", func = plot.lines, arg_list = list(c(-1, 1), c(-1 + (2/15)*i, -1 + (2/15)*i)))
     }
     writePNG.add("14.png", func = plot.points, arg_list = list(px, py))
     writePNG.close("14.png", func = NULL)
     
     writePNG.open("15.png", func = plot.gen.sin, arg_list = list(100, draw_pattern_sin = TRUE, title ="АЦП", type = "n", lwd = 3))
     for (i in 0:30) {
          writePNG.add("15.png", func = plot.lines, arg_list = list(c(i/(5*100), i/(5*100)), c(-2, 2)))
          writePNG.add("15.png", func = plot.lines, arg_list = list(c(-1, 1), c(-1 + (2/15)*i, -1 + (2/15)*i)))
     }
     writePNG.add("15.png", func = plot.points, arg_list = list(px, py))
     writePNG.close("15.png", func = NULL)
     
     # ЦАП
     writePNG.open("16.png", func = plot.gen.sin, arg_list = list(100, title ="ЦАП", draw_ox = TRUE, type = "n", lwd = 3))
     writePNG.add("16.png", func = plot.points, arg_list = list(px, py, type = "s"))
     writePNG.add("16.png", func = plot.points, arg_list = list(px, py))
     writePNG.close("16.png", func = NULL)
     
     writePNG.open("17.png", func = plot.gen.sin, arg_list = list(100, draw_pattern_sin = TRUE, title ="ЦАП", draw_ox = TRUE, type = "n", lwd = 3))
     writePNG.add("17.png", func = plot.points, arg_list = list(px, py, type = "s"))
     writePNG.add("17.png", func = plot.points, arg_list = list(px, py))
     writePNG.close("17.png", func = NULL)
     
     px_2 <- seq(0, 0.06, 0.001)
     py_2 <- rep(-1 + c((2/15)*7, (2/15)*11, (2/15)*15, (2/15)*14, (2/15)*12, (2/15)*7, (2/15)*3, (2/15)*1, (2/15)*0, (2/15)*4), length.out = 61)
     
     writePNG.open("18.png", func = plot.gen.sin, arg_list = list(100, draw_pattern_sin = TRUE, title ="ЦАП", draw_ox = TRUE, type = "n", lwd = 3))
     writePNG.add("18.png", func = plot.points, arg_list = list(px_2, py_2, type = "s"))
     #writePNG.add("18.png", func = plot.points, arg_list = list(px_2, py_2))
     writePNG.close("18.png", func = NULL)
}

lection6.make()
