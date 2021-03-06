# install.packages("tuneR")
library(tuneR)
library(tools)

g_mai <- c(2.75, 2.75, 2.5, 1.5)
g_lwd_scale <- 1.5
g_cex <- 3.0
g_cex.axis <- 1.0
g_cex.lab <- 1.25
g_cex.main <- 1.75
g_cex.sub <- 0.75

plot.set.par <- function(cex = g_cex, cex.axis = g_cex.axis, cex.lab = g_cex.lab, cex.main = g_cex.main, cex.sub = g_cex.sub, mai = g_mai) {
   par(cex = cex, cex.axis = cex.axis, cex.lab = cex.lab, cex.main = cex.main, cex.sub = cex.sub, mai = mai)
}

# Test (not use)
get <- function(x, ...) {
     message(123)
     print(class(x)[1])
     switch (class(x)[1],
             character = do.call(get.osg, c(x, list(...))),
             Wave = do.call(get.prg, c(x, list(...)))
     )
}

get_unit_coef <- function(data, units = c("samples", "seconds", "minutes", "hours")) {
     coef <- switch (units[1],
                     samples = 1,
                     seconds = data@samp.rate,
                     minutes = 60*data@samp.rate,
                     hours = 3600*data@samp.rate
     )
     
     if (is.null(coef))
          warning(paste("Wrong units argument -", units[1]))
     
     coef
}

get.osg <- function(filename, from = 1, to = Inf, units = c("samples", "seconds", "minutes", "hours")) {
     osg <- switch (file_ext(filename),
                    mp3 = readMP3(filename),
                    wav = readWave(filename)
     )
     
     if (is.null(osg)) {
          warning(paste("Unable to read file -", filename))
          return(NULL)
     }
     
     coef <- get_unit_coef(osg, units)
     
     if (!is.null(coef)) {
          from <- from * coef
          to <- to * coef
          len <- length(osg@left)
          
          if (from == 0 || from > len)
               from = 1
          if (to > len)
               to = len
          
          osg@left <- osg@left[from:to]
          if (osg@stereo[1])
               osg@right <- osg@right[from:to]
     }
     
     osg
}

data.osg.to_mono <- function(data) {
     mono_data <- data@left
     if (data@stereo) {
        mono_data <- mono_data + data@right
     }
     
     mono_data
}

data.osg.norm <- function(data) {
     maximum <- max(abs(range(data)))
     norm_data <- data / maximum

     norm_data
}

data.osg.ranged <- function(data, x_resolution = NULL) {
     if (is.null(x_resolution)) {
          return(data)
     }
     
     ranged_data <- rep(0, times = 2*x_resolution)
     dt <- length(data)/x_resolution
     for (i in 1:x_resolution) {
          min_max <- range(data[(((i - 1)*dt) + 1) : (i*dt)])
          ranged_data[2*i - 1] <- min_max[1]
          ranged_data[2*i] <- min_max[2]
     }  
     
     ranged_data
}

data.osg <- function(osg, norm = FALSE, x_resolution = NULL) {
     data <- data.osg.to_mono(osg)
     data <- data.osg.ranged(data, x_resolution)
     if (norm) {
        data <- data.osg.norm(data)
     }
     
     data
}

time.osg <- function(osg, xunit = c("time", "samples"), x_resolution = 2000) {
     dt <- 1
     t_length <- length(osg@left)
     if (!is.null(x_resolution)) {
          dt <- length(osg@left)/x_resolution
          t_length <- x_resolution
     }
     
     if (xunit[1] == "samples") {
          t <- seq(0, by = dt, length.out = t_length)
     } else {
          t <- seq(0, by = (1/osg@samp.rate)*dt, length.out = t_length)
     }
     
     if (!is.null(x_resolution)) {
          t <- rep(t, each = 2)
     }
     
     t
}

plot.osg <- function(osg, ylim = c(-1, 1), norm = FALSE, x_resolution = 2000, xunit = c("time", "samples"), title = "", xlab = "Врем\u44f", ylab = "Амплитуда", type = "h", draw_ox = TRUE, lwd = 1, mai = g_mai, ...) {
     plot.set.par(mai = mai)
     
     if (length(osg@left) / osg@samp.rate < 2.5) {
          x_resolution <- NULL
          type = "l"
     }
     y <- data.osg(osg, norm, x_resolution)
     t <- time.osg(osg, xunit, x_resolution)
     
     if (xunit[1] == "samples") {
          xlab <- paste0(xlab, ", отсчеты")
     } else {
          xlab <- paste0(xlab, ", секунды")
     }
     
     if (!osg@pcm) {
          y <- data.osg.norm(y)
     } else {
          border <- (2^osg@bit) / 2
          y <- y / border
     }
     
     plot(t, y, ylim = ylim, main = title, xlab = xlab, ylab = ylab, type = type, lty = "solid", col = "black", lwd = (if (lwd == 1) 1 else lwd * g_lwd_scale), las = (if (ylim[2] == 1) 1 else 3), ...)
     
     if (draw_ox) {
          t_head <- head(t, n = 1)
          t_tail <- tail(t, n = 1)
          space <- 0.1*(t_tail - t_head)
          
          lines(c(t_head - space, t_tail + space), c(0, 0), type = "l", lty = "solid", col = "black", lwd = max(1, lwd - 3) * g_lwd_scale)
     }
}

data.prg.log <- function(spec) {
   spec <- 20*log10(spec)
   spec
}

data.prg.norm <- function(spec, max = 1) {
   spec <- spec/max # normalize magnitude so that max is 0 dB.
   spec
}

data.prg.clip_dB <- function(spec, min_db = -40, max_db = -3) {
   spec[spec < min_db] <- min_db # clip below -40 dB.
   spec[spec > max_db] <- max_db # clip above -3 dB.
   spec
}

get.prg <- function(osg, width = length(osg), overlap = 0, postproc = list(log = TRUE, norm = FALSE, clip = FALSE, min_db = -40, max_db = -3)) {
     prg <- periodogram(osg, width = width, overlap = overlap)
     
     if (postproc$log) {
          prg@spec <- lapply(prg@spec, sapply, data.prg.log)
          
          if (postproc$norm) {
               max <- sapply(prg@spec, max)
               max <- max(max)
               if (max != 0) {
                    prg@spec <- lapply(prg@spec, sapply, data.prg.norm, max)
               }
          }
          
          if (postproc$clip) {
             prg@spec <- lapply(prg@spec, sapply, data.prg.clip, postproc$min_db, postproc$max_db)
          }
     }
     
     prg
}

plot.prg <- function(prg, which = 1, ylim = c(min(prg@spec[[which]]), 0), fill = TRUE, title = "", xlab = "Частота, Гц", ylab = "Амплитуда, дБ", mai = c(2.75, 2.75, 2.5, 1.5), ...) {
     plot.set.par(mai = mai)
     
     if (which <= 0) {
          warning(paste("Negative which index -", which))
          which <- 1
     }
     
     if (which > length(prg@spec)) {
          warning(paste("Going beyond range of which [1", length(prg@spec), "]  -", which))
          which <- 1
     }
     
     plot(x = prg@freq, y = prg@spec[[which]], ylim = ylim, type = "l", lty = "solid", col = "black", main = title, xlab = xlab, ylab = ylab, ...)
     
     if (fill) {
          min <- min(prg@spec[[which]])
          polygon(c(head(prg@freq, n = 1), prg@freq, tail(prg@freq, n = 1)), c(1.1*ylim[1], prg@spec[[which]], 1.1*ylim[1]), col = "black")
     }
}

get.pws <- function(osg, wintime = 0.025, steptime = 0.01, fs_band = osg@samp.rate/2,  postproc = list(log = TRUE, norm = FALSE, clip = FALSE, min_db = -40, max_db = -3)) {
     data <- data.osg.to_mono(osg)

     pws <- powspec(data, osg@samp.rate, wintime = wintime, steptime = steptime)
     pws <- abs(pws[2:(nrow(pws)*fs_band/(osg@samp.rate/2)),]) # magnitude in range (0; fs_band] Hz.
     pws <- pws / max(pws)
     
     if (postproc$log) {
          pws <- data.prg.log(pws)
          
          if (postproc$norm && max(pws) != 0) {
               pws <- data.prg.norm(pws, max(pws))
          }
          
          if (postproc$clip) {
               pws <- data.prg.clip_dB(pws, postproc$min_db, postproc$max_db)
          }
     }

     list(pws = pws, fs_band = fs_band, steptime = steptime)
}

plot.pws <- function(pws, title = "", xlab = "Врем\u44f, секунды", ylab = "Частота, Гц", log = FALSE, mai = c(2.75, 2.75, 2.5, 1.5), ...) {
     plot.set.par(mai = mai)
   
     duration <- ncol(pws$pws) * pws$steptime
     if (duration < 10) {
          delta_t <- duration/10
          digits <- 1
          for (i in 1:10) {
             if (delta_t*10^i > 1) {
                digits <- i
                break
             }
          }
          at_1 <- c(seq(0, 1, delta_t/duration))
          labels_1 <- c(round(seq(0, duration, delta_t), digits = digits))
     } else {
          delta_t <- floor(duration/10)
          at_1 <- c(seq(0, 1, delta_t/duration), 1)
          labels_1 <- c(round(seq(0, duration, delta_t), digits = 0), "")
     }
     
     at_2 <- seq(0, 1, 0.1)
     labels_2 <- seq(0, pws$fs_band, pws$fs_band/10)
     if (log) {
        delta_fs <- pws$fs_band / nrow(pws$pws)
        mtx <- matrix(-200.0, nrow = 100 * log10(pws$fs_band), ncol = ncol(pws$pws))
        fs_start <- 1
        fs_end <- NA
        for(i in 1:nrow(pws$pws)) {
           fs <- delta_fs * i
           fs_end <- floor(100*log10(fs))
           fs_start <- min(fs_start, fs_end)
           mtx[fs_start:fs_end,] <- apply(matrix(mtx[fs_start:fs_end,], ncol = ncol(pws$pws), byrow=TRUE), 1, pmax, pws$pws[i,])
           fs_start <- fs_end + 1
        }
        pws$pws <- mtx
        log_scale <- log10(pws$fs_band)
        labels_2 <- seq(0, log_scale, log_scale/10)
        labels_2 <- round(10^labels_2, digits = 0)
        labels_2[1] <- 0
     }
     
     image(x = t(pws$pws), col = gray(0:255 / 255), axes = FALSE, main = title, xlab = xlab, ylab = NA, ...)
     mtext(ylab, side = 4, line = 1, cex = g_cex * g_cex.lab)

     axis(side = 1, at = at_1, labels = labels_1) # time
     axis(side = 2, at = at_2, labels = labels_2, las = 1) # frequency
}