g_lwd_scale <- 1.5
g_mai <- c(2.75, 2.75, 2.5, 1.5)

plot.set.par <- function(cex = 3.0, cex.axis = 1.0, cex.lab = 1.25, cex.main = 1.75, cex.sub = 0.75, mai = g_mai) {
     par(cex = cex, cex.axis = cex.axis, cex.lab = cex.lab, cex.main = cex.main, cex.sub = cex.sub, mai = mai)
}

plot.points <- function(x, y = NULL, type = "p", pch = 0, lwd = 2, mai = g_mai) {
   #wrapper for built in function points
   plot.set.par(mai = mai)
   
   points(x = x , y = y, type = type, pch = pch, lwd = lwd * g_lwd_scale)
}

plot.lines <- function(x, y = NULL, type = "l", lty = "dotted", col = "black", lwd = 1, mai = g_mai) {
   #wrapper for built in function lines
   plot.set.par(mai = mai)
   
   lines(x = x , y = y, type = type, lty = lty, col = col, lwd = lwd * g_lwd_scale)
}

plot.gen.const <- function(amp = 1, xlim = c(0, 0.06), ylim = c(-1, 1), draw_ox = FALSE, type = "l", lty = "solid", lwd = 3, mai = g_mai) {
     # amp - amplitude of sin [0, 1]
     plot.set.par(mai = mai)
     
     plot(c(-1, 1), c(amp, amp), xlim = xlim, ylim = ylim, type = type, lty = lty, main = "Посто\u44fнное значение", xlab = "Врем\u44f, с", ylab = "Амплитуда", col = "black", lwd = lwd * g_lwd_scale, las = 1)
     if (draw_ox)
          lines(c(-1, 1), c(0, 0), type = "l", col = "black", lwd = max(1, lwd - 3) * g_lwd_scale)
}

plot.gen.square <- function(fs, amp = 1, dt = 1/(100*fs), shift = 0, xlim = c(0, 0.06), ylim = c(-1, 1), draw_ox = FALSE, draw_pattern_sin = FALSE, draw_period = FALSE, fs_print = fs, type = "l", lty = "solid", lwd = 3, mai = g_mai) {
     # fs - frequency sample rate (Hz)  (the ordinary frequency, the number of oscillations (cycles) that occur each second of time)
     # amp - amplitude of sin [0, 1]
     # dt - discrete time period (sec)
     # type - plot.default (type)
     # shift - shift horizontally plot (sec)
     # ...
     # fs_print - printed frequency sample rate (Hz)
     plot.set.par(mai = mai)
     
     t <- seq(-1 + shift, 1, dt)
     y <- amp*sign(sin(2*pi*fs*t))
     plot(t, y, xlim = xlim, ylim = ylim, type = type, lty = lty, main = paste0("Меандр (", fs_print, " Гц)"), xlab = "Врем\u44f, с", ylab = "Амплитуда", col = "black", lwd = lwd * g_lwd_scale, las = 1)
     if (draw_ox)
          lines(c(-1, 1), c(0, 0), type = "l", lty = "solid", col = "black", lwd = max(1, lwd - 3) * g_lwd_scale)
     if (draw_pattern_sin) {
          t0 <- seq(-1, 2, 0.0001)
          y0 <- amp*sin(2*pi*fs*t0)
          lines(t0, y0, xlim = xlim, ylim = ylim, type = "l", lty = "dotted", col = "black", lwd = max(2, lwd - 2) * g_lwd_scale)
     }
     if (draw_period)
          for (i in 0:6)
               lines(c(i/fs, i/fs), c(-2, 2), type = "l", lty = "dotted", col = "black", lwd = max(1, lwd - 3) * g_lwd_scale)
}

plot.gen.sin <- function(fs, amp = 1, dt = 1/(1000*fs), shift = 0, xlim = c(0, 0.06), ylim = c(-1, 1), draw_ox = FALSE, draw_pattern_sin = FALSE, draw_period = FALSE, title = "", fs_print = fs, type = "l", lty = "solid", lwd = 3, mai = g_mai) {
     # fs - frequency sample rate (Hz)  (the ordinary frequency, the number of oscillations (cycles) that occur each second of time)
     # amp - amplitude of sin [0, 1]
     # dt - discrete time period (sec)
     # type - plot.default (type)
     # shift - shift horizontally plot (sec)
     # ...
     # fs_print - printed frequency sample rate (Hz)
     plot.set.par(mai = mai)
     
     t <- seq(-1 + shift, 1, dt)
     y <- amp*sin(2*pi*fs*t)
     plot(t, y, xlim = xlim, ylim = ylim, type = type, lty = lty, main = (if (title == "") paste0("sin(2pi*", fs_print, "Гц*t)") else title), xlab = "Врем\u44f, с", ylab = "Амплитуда", col = "black", lwd = lwd * g_lwd_scale, las = 1)
     if (draw_ox)
          lines(c(-1, 1), c(0, 0), type = "l", lty = "solid", col = "black", lwd = max(1, lwd - 3) * g_lwd_scale)
     if (draw_pattern_sin) {
          t0 <- seq(-1, 2, 0.0001)
          y0 <- amp*sin(2*pi*fs*t0)
          lines(t0, y0, xlim = xlim, ylim = ylim, type = "l", lty = "dotted", col = "black", lwd = max(2, lwd - 2) * g_lwd_scale)
     }
     if (draw_period)
          for (i in 0:6)
               lines(c(i/fs, i/fs), c(-2, 2), type = "l", lty = "dotted", col = "black", lwd = max(1, lwd - 3) * g_lwd_scale)
}

data.frame.multi_sin <- function(fs, start = 0.0, stop = 0.06, dt = 1/(1000*fs), amp = 1.0, shift = 0.0, phi = 0.0, lty = "solid", lwd = 3) {
     # df.fs - frequency sample rate (Hz)   (the ordinary frequency, the number of oscillations (cycles) that occur each second of time)
     # df.start - start plotting sin from time point (sec)
     # df.stop - stop plotting sin to time point (sec)
     # df.dt - discrete time period (sec)
     # df.amp - amplitude of sin [0, 1]
     # df.shift - shift horizontally plot (sec)
     # df.phi - phase (specifies (in radians) where in its cycle the oscillation is at t = 0)
     # df.lty - plot.default (lty)
     # df.lwd - plot.default (lwd)
     
     count <- length(fs)
     
     df <- data.frame(fs = fs, stringsAsFactors = FALSE)
     
     df$start <- rep(start, count)
     df$stop  <- rep(stop, count)
     if (length(fs) == length(dt)) { df$dt <- dt } else {
     df$dt   <- rep(dt, count) }
     df$amp   <- rep(amp, count)
     df$shift <- rep(shift, count)
     df$phi   <- rep(phi, count)
     df$lty   <- rep(lty, count)
     df$lwd   <- rep(lwd, count)
     
     df
}

plot.gen.multi_sin <- function(df, draw_combined = FALSE, xlim = c(0, 0.06), ylim = c(-1, 1), title = "", type = "l", draw_period = FALSE, mai = g_mai) {
     plot.set.par(mai = mai)
     
     df$amp[df$amp < -1] <- -1
     df$amp[df$amp >  1] <-  1
     
     if (nrow(df) == 1) {
          if (title != "")
             title <- paste0(title, "\n")
          if (df$amp[1] != 1)
             title <- paste0(title, df$amp[1], "*")
          title <- paste0(title, "sin(2pi*", df$fs[1], "Гц*t")
          if (df$phi[1] != 0)
               title <- paste0(title, "+ ", df$phi[1])
          title <- paste0(title, ")")
     }
     
     plot(0, 0, type = "n", xlim = xlim, ylim = ylim, main = title, xlab = "Врем\u44f, с", ylab = "Амплитуда")
     
     delta = abs(xlim[2] - xlim[1])/10
     x_lim <- c(xlim[1] - delta, xlim[2] + delta)
     lines(x_lim, c(0, 0), type = "l", lty = "solid", col = "black", lwd = max(1, max(df$lwd) - 3) * g_lwd_scale)
     
     if (nrow(df) == 1) {
          fs <- df$fs[1]
          if (draw_period)
               for (i in (df$start[1]):(df$stop[1] * fs))
                    lines(c(i / fs, i / fs), c(-2, 2), type = "l", lty = "dotted", col = "black", lwd = max(1, max(df$lwd) - 3) * g_lwd_scale)
     }
     
     if (nrow(df) >= 1) {
          if (draw_combined) {
               start <- min(df$start)
               stop <- max(df$stop)
               dt <- max(df$dt)
               t <- seq(start, stop, dt)
               y <- rep(0, length(t))
               for (i in 1:nrow(df)) {
                    y <- y + df$amp[i]*sin(2*pi*df$fs[i]*t + df$phi[i])
               }
               lines(t, y, type = type, lty = df$lty[i], col = "black", lwd = df$lwd[i] * g_lwd_scale)
          } else {
               for (i in 1:nrow(df)) {
                    t <- seq(df$start[i] + df$shift[i], df$stop[i], df$dt[i])
                    y <- df$amp[i]*sin(2*pi*df$fs[i]*t + df$phi[i])
                    lines(t, y, type = type, lty = df$lty[i], col = "black", lwd = df$lwd[i] * g_lwd_scale)
               }
          }
     }

}

data.frame.prg <- function(fs, amp = 1.0, damp = 0.0, pow = 1/2, duty = 2, lwd = 5) {
     # df.fs - frequency sample rate (Hz)   (the ordinary frequency, the number of oscillations (cycles) that occur each second of time)
     # df.amp - amplitude of sin [0, 1]
     # df.damp - damping ratio of the signal's spectrum (0, 1]
     # df.pow - power of damping ratio (0, 1]
     # df.duty - duty cycle of spectum's stem [1, ]
     # df.lwd - plot.default (lwd)
     
     count <- length(fs)

     df <- data.frame(fs = fs, stringsAsFactors = FALSE)
     
     df$amp  <- rep(amp, count)
     df$damp <- rep(damp, count)
     df$pow  <- rep(pow, count)
     df$duty <- rep(duty, count)
     df$lwd  <- rep(lwd, count)

     df
}

data.frame.prg.preproc <- function(df) {
     df_ret <- df[, c("fs", "amp", "lwd")]
   
     df_bind <- data.frame(stringsAsFactors = FALSE)
     for (i in 1:nrow(df)) {
        if (df$damp[i] > 0) {
           for (k in 1:(df$amp[i]^(1/df$pow[i]) / df$damp[i])) {
              shift_x <- k * df$duty[i]
              shift_y <- (k * df$damp[i])^df$pow[i]
              df_bind <- rbind(df_bind, data.frame(fs = df$fs[i] + shift_x, amp = df$amp[i] - shift_y, lwd = df$lwd[i]))
              df_bind <- rbind(df_bind, data.frame(fs = df$fs[i] - shift_x, amp = df$amp[i] - shift_y, lwd = df$lwd[i]))
           }
        }
     }
     
     for (i in 1:nrow(df_bind)) {
          idx <- df_ret$fs == df_bind$fs[i]
          if (any(idx)) {
               df_ret$amp[idx] <- df_ret$amp[idx] + df_bind$amp[i]
          } else {
               df_ret <- rbind(df_ret, df_bind[i,])
          }
     }
     
     df_ret
}

plot.gen.prg <- function(df, xmax = max(df$fs) + 50, title = "", type = "l", mai = g_mai) {
     df <- data.frame.prg.preproc(df)

     df$amp[df$amp < 0 | df$amp > 1] <- 1
     
     if (nrow(df) == 1) {
          if (title != "")
             title <- paste0(title, "\n")
          if (df$amp[1] != 1)
             title <- paste0(title, df$amp[1], "*")
          title <- paste0(title, "sin(2pi*", df$fs[1], "Гц*t)")
     }
     
     plot.set.par(mai = mai)
     
     plot(0, 0, type = "n", xlim = c(0, xmax), ylim = c(0, 1), main = title, xlab = "Частота, Гц", ylab = "Амплитуда")
     
     delta = xmax/10
     x_lim <- c(0 - delta, xmax + delta)
     lines(x_lim, c(0, 0), type = "l", lty = "solid", col = "black", lwd = max(1, max(df$lwd) - 5) * g_lwd_scale)
     
     lines(c(0, 0), c(0, 1), type = "l", lty = "dotted", col = "black", lwd = max(1, max(df$lwd) - 5) * g_lwd_scale)
     lines(c(xmax, xmax), c(0, 1), type = "l", lty = "dotted", col = "black", lwd = max(1, max(df$lwd) - 5) * g_lwd_scale)
     
     if (nrow(df) >= 1)
          for (i in 1:nrow(df))
               lines(c(df$fs[i], df$fs[i]), c(0, df$amp[i]), type = type, lty = "solid", col = "black", lwd = df$lwd[i] * g_lwd_scale)
}

data.frame.pws <- function(fs, start = 0.0, stop = 0.06, amp = 1.0, damp = 0.0, pow = 1/2, duty = 25, lwd = 10) {
     # df.fs - frequency sample rate (Hz)   (the ordinary frequency, the number of oscillations (cycles) that occur each second of time)
     # df.start - start plotting sin from time point (sec)
     # df.stop - stop plotting sin to time point (sec)
     # df.amp - amplitude of sin [0, 1]
     # df.damp - damping ratio of the signal's spectrum (0, 1]
     # df.pow - power of damping ratio (0, 1]
     # df.duty - duty cycle of spectum's stem [1, ]
     # df.lwd - plot.default (lwd)
     
     count <- length(fs)
     
     df <- data.frame(fs = fs, stringsAsFactors = FALSE)
     
     df$start <- rep(start, count)
     df$stop  <- rep(stop, count)
     df$amp   <- rep(amp, count)
     df$damp  <- rep(damp, count)
     df$pow   <- rep(pow, count)
     df$duty  <- rep(duty, count)
     df$lwd   <- rep(lwd, count)

     df
}

data.frame.pws.preproc <- function(df) {
     df_ret <- df[, c("fs", "start", "stop", "amp", "lwd")]
     
     df_bind <- data.frame(stringsAsFactors = FALSE)
     for (i in 1:nrow(df)) {
          if (df$damp[i] > 0) {
               bound <- df$amp[i]^(1/df$pow[i]) / df$damp[i]
               if (bound > 1) {
                    for (k in 1:bound) {
                         shift_x <- k * df$duty[i]
                         shift_y <- (k * df$damp[i])^df$pow[i]
                         df_bind <- rbind(df_bind, data.frame(fs = df$fs[i] + shift_x, start = df$start[i], stop = df$stop[i], amp = df$amp[i] - shift_y, lwd = df$lwd[i]))
                         df_bind <- rbind(df_bind, data.frame(fs = df$fs[i] - shift_x, start = df$start[i], stop = df$stop[i], amp = df$amp[i] - shift_y, lwd = df$lwd[i]))
                    }
               }
          }
     }
     
     df_ret <- rbind(df_ret, df_bind)

     df_ret
}

plot.gen.pws <- function(df, ymax = max(df$fs) + 50, xlim = c(0, 0.06), type = "l", mai = g_mai) {
     df <- data.frame.pws.preproc(df)

     df$amp[df$amp < 0 | df$amp > 1] <- 1
     df$amp <- grey(1 - df$amp)
     
     plot.set.par(mai = mai)
     
     plot(0, 0, type = "n", xlim = xlim, ylim = c(0, ymax), main = "Спектрограмма", xlab = "Врем\u44f, с", ylab = "Частота, Гц")
     
     delta = abs(xlim[2] - xlim[1])/10
     x_lim <- c(xlim[1] - delta, xlim[2] + delta)
     lines(x_lim, c(0, 0), type = "l", lty = "solid", col = "black", lwd =  max(1, max(df$lwd) - 10) * g_lwd_scale)
     
     if (nrow(df) >= 1)
          for (i in 1:nrow(df))
               lines(c(df$start[i], df$stop[i]), c(df$fs[i], df$fs[i]), type = type, lty = "solid", col = df$amp[i], lwd = df$lwd[i] * g_lwd_scale)
}