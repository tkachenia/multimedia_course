plot.gen.const <- function(amp = 1, draw_ox = FALSE, type = "l", lty = "solid", lwd = 5) {
     # amp - amplitude of sin [0, 1]
     par(cex = 3.0, cex.axis = 1.0, cex.lab = 1.25, cex.main = 1.75, cex.sub = 0.75, mai = c(2.75, 2.75, 2.5, 1.5))
     
     plot(c(-1, 1), c(amp, amp), xlim = c(0, 0.06), ylim = c(-1, 1), type = type, lty = lty, main = "Посто\u44fнное значение", xlab = "Врем\u44f, с", ylab = "Амплитуда", col = "black", lwd = lwd, las = 1)
     if (draw_ox)
          lines(c(-1, 1), c(0, 0), type = "l", col = "black", lwd = max(1, lwd - 3))
}

plot.gen.square <- function(fs, amp = 1, dt = 1/(100*fs), shift = 0, draw_ox = FALSE, draw_pattern_sin = FALSE, draw_period = FALSE, fs_print = fs, type = "l", lty = "solid", lwd = 5) {
     # fs - frequency sample rate (Hz)  (the ordinary frequency, the number of oscillations (cycles) that occur each second of time)
     # amp - amplitude of sin [0, 1]
     # dt - discrete time period (sec)
     # type - plot.default (type)
     # shift - shift horizontally plot (sec)
     # ...
     # fs_print - printed frequency sample rate (Hz)
     par(cex = 3.0, cex.axis = 1.0, cex.lab = 1.25, cex.main = 1.75, cex.sub = 0.75, mai = c(2.75, 2.75, 2.5, 1.5))
     
     t <- seq(-1 + shift, 1, dt)
     y <- amp*sign(sin(2*pi*fs*t))
     plot(t, y, xlim = c(0, 0.06), ylim = c(-1, 1), type = type, lty = lty, main = paste0("Меандр (", fs_print, " Гц)"), xlab = "Врем\u44f, с", ylab = "Амплитуда", col = "black", lwd = lwd, las = 1)
     if (draw_ox)
          lines(c(-1, 1), c(0, 0), type = "l", lty = "solid", col = "black", lwd = max(1, lwd - 3))
     if (draw_pattern_sin) {
          t0 <- seq(-1, 2, 0.0001)
          y0 <- amp*sin(2*pi*fs*t0)
          lines(t0, y0, xlim = c(0, 0.06), ylim = c(-1, 1), type = "l", lty = "dotted", col = "black", lwd=max(2, lwd - 2))
     }
     if (draw_period)
          for (i in 0:6)
               lines(c(i/fs, i/fs), c(-2, 2), type = "l", lty = "dotted", col = "black", lwd=max(1, lwd - 3))
}

plot.gen.sin <- function(fs, amp = 1, dt = 1/(1000*fs), shift = 0, draw_ox = FALSE, draw_pattern_sin = FALSE, draw_period = FALSE, title = "", fs_print = fs, type = "l", lty = "solid", lwd = 5) {
     # fs - frequency sample rate (Hz)  (the ordinary frequency, the number of oscillations (cycles) that occur each second of time)
     # amp - amplitude of sin [0, 1]
     # dt - discrete time period (sec)
     # type - plot.default (type)
     # shift - shift horizontally plot (sec)
     # ...
     # fs_print - printed frequency sample rate (Hz)
     par(cex = 3.0, cex.axis = 1.0, cex.lab = 1.25, cex.main = 1.75, cex.sub = 0.75, mai = c(2.75, 2.75, 2.5, 1.5))
     
     t <- seq(-1 + shift, 1, dt)
     y <- amp*sin(2*pi*fs*t)
     plot(t, y, xlim = c(0, 0.06), ylim = c(-1, 1), type = type, lty = lty, main = (if (title == "") paste0("sin(2pi*", fs_print, "Гц*t)") else title), xlab = "Врем\u44f, с", ylab = "Амплитуда", col = "black", lwd = lwd, las = 1)
     if (draw_ox)
          lines(c(-1, 1), c(0, 0), type = "l", lty = "solid", col = "black", lwd = max(1, lwd - 3))
     if (draw_pattern_sin) {
          t0 <- seq(-1, 2, 0.0001)
          y0 <- amp*sin(2*pi*fs*t0)
          lines(t0, y0, xlim = c(0, 0.06), ylim = c(-1, 1), type = "l", lty = "dotted", col = "black", lwd=max(2, lwd - 2))
     }
     if (draw_period)
          for (i in 0:6)
               lines(c(i/fs, i/fs), c(-2, 2), type = "l", lty = "dotted", col = "black", lwd=max(1, lwd - 3))
}

plot.gen.multi_sin <- function(df, xlim = c(0, 0.06), title = "", type = "l", draw_period = FALSE) {
     # df.fs - frequency sample rate (Hz)   (the ordinary frequency, the number of oscillations (cycles) that occur each second of time)
     # df.start - start plotting sin from time point (sec)
     # df.stop - stop plotting sin to time point (sec)
     # df.dt - discrete time period (sec)
     # df.amp - amplitude of sin [0, 1]
     # df.shift - shift horizontally plot (sec)
     # df.phi - phase (specifies (in radians) where in its cycle the oscillation is at t = 0)
     # df.lty - plot.default (lty)
     # df.lwd - plot.default (lwd)
     par(cex = 3.0, cex.axis = 1.0, cex.lab = 1.25, cex.main = 1.75, cex.sub = 0.75, mai = c(2.75, 2.75, 2.5, 1.5))
     
     df$amp[df$amp < -1] <- -1
     df$amp[df$amp >  1] <-  1
     
     title <- title
     if (nrow(df) == 1) {
          if (append_title != "")
               append_title <- paste0(append_title, "\n")
          if (df$amp[1] != 1)
               append_title <- paste0(append_title, df$amp[1], "*")
          title <- paste0(append_title, "sin(2pi*", df$fs[1], "Гц*t")
          if (df$phi[1] != 0)
               title <- paste0(title, "+ ", df$phi[1])
          title <- paste0(title, ")")
     }
     
     plot(0, 0, type = "n", xlim = xlim, ylim = c(-1, 1), main = title, xlab = "Врем\u44f, с", ylab = "Амплитуда")
     
     delta = abs(xlim[2] - xlim[1])/10
     x_lim <- c(xlim[1] - delta, xlim[2] + delta)
     lines(x_lim, c(0, 0), type = "l", lty = "solid", col = "black", lwd = 2)
     
     if (nrow(df) == 1) {
          fs <- df$fs[1]
          if (draw_period)
               for (i in (df$start[1]):(df$stop[1] * fs))
                    lines(c(i / fs, i / fs), c(-1, 1), type = "l", lty = "dotted", col = "black", lwd = 2)
     }
     
     if (nrow(df) >= 1)
          for (i in 1:nrow(df)) {
               t <- seq(df$start[i] + df$shift[i], df$stop[i], df$dt[i])
               y <- df$amp[i]*sin(2*pi*df$fs[i]*t + df$phi[i])
               lines(t, y, type = type, lty = df$lty[i], col = "black", lwd = df$lwd[i])
          }
}

plot.gen.prg <- function(df, xmax = max(df$fs) + 50, append_title = "", type = "l") {
     # df.fs - frequency sample rate (Hz)   (the ordinary frequency, the number of oscillations (cycles) that occur each second of time)
     # df.amp - amplitude of sin [0, 1]
     # df.lwd - plot.default (lwd)
     
     df$amp[df$amp < 0 | df$amp > 1] <- 1
     
     title <- append_title
     if (nrow(df) == 1) {
          if (append_title != "")
               append_title <- paste0(append_title, "\n")
          if (df$amp[1] != 1)
               append_title <- paste0(append_title, df$amp[1], "*")
          title <- paste0(append_title, "sin(2pi*", df$fs[1], "Гц*t)")
     }
     
     par(cex = 3.0, cex.axis = 1.0, cex.lab = 1.25, cex.main = 1.75, cex.sub = 0.75, mai = c(2.75, 2.75, 2.5, 1.5))
     
     plot(0, 0, type = "n", xlim = c(0, xmax), ylim = c(0, 1), main = title, xlab = "Частота, Гц", ylab = "Амплитуда")
     
     delta = xmax/10
     x_lim <- c(0 - delta, xmax + delta)
     lines(x_lim, c(0, 0), type = "l", lty = "solid", col = "black", lwd = 2)
     
     lines(c(0, 0), c(0, 1), type = "l", lty = "dotted", col = "black", lwd = 2)
     lines(c(xmax, xmax), c(0, 1), type = "l", lty = "dotted", col = "black", lwd = 2)
     
     if (nrow(df) >= 1)
          for (i in 1:nrow(df))
               lines(c(df$fs[i], df$fs[i]), c(0, df$amp[i]), type = type, lty = "solid", col = "black", lwd = df$lwd[i])
}

plot.gen.pws <- function(df, ymax = max(df$fs) + 50, xlim = c(0, 0.06), type = "l") {
     # df.fs - frequency sample rate (Hz)   (the ordinary frequency, the number of oscillations (cycles) that occur each second of time)
     # df.start - start plotting sin from time point (sec)
     # df.stop - stop plotting sin to time point (sec)
     # df.amp - amplitude of sin [0, 1]
     
     df$amp[df$amp < 0 | df$amp > 1] <- 1
     df$amp <- grey(1 - df$amp)
     
     par(cex = 3.0, cex.axis = 1.0, cex.lab = 1.25, cex.main = 1.75, cex.sub = 0.75, mai = c(2.75, 2.75, 2.5, 1.5))
     
     plot(0, 0, type = "n", xlim = xlim, ylim = c(0, ymax), main = "Оконный спектр", xlab = "Врем\u44f, с", ylab = "Частота, Гц")
     
     delta = abs(xlim[2] - xlim[1])/10
     x_lim <- c(xlim[1] - delta, xlim[2] + delta)
     lines(x_lim, c(0, 0), type = "l", lty = "solid", col = "black", lwd = 2)
     
     if (nrow(df) >= 1)
          for (i in 1:nrow(df))
               lines(c(df$start[i], df$stop[i]), c(df$fs[i], df$fs[i]), type = type, lty = "solid", col = df$amp[i], lwd = 10)
}