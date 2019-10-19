local_path <- "D:/Ubuntu/Share/Курс_Мультимедийные системы и среды"

setwd(paste0(local_path, "/multimedia_course/Lection/08"))
Sys.setlocale("LC_CTYPE", "russian")
source("../wave.R", encoding="utf-8")
source("../png.R")

lection8.test <- function() {
   # Oscilogram
   data_wav <- get.osg("DOOM.wav")
   writePNG("test_01.png", func = plot.osg, arg_list = list(data_wav, mai = c(2.75, 2.75, 1.25, 1.5)))
   
   data <- get.osg("DOOM.wav", from = 29, to = 30, units = "seconds")
   writePNG("test_02_1.png", func = plot.osg, arg_list = list(data, mai = c(2.75, 2.75, 1.25, 1.5)))
   writePNG("test_02_2.png", func = plot.osg, arg_list = list(data, type = "l", mai = c(2.75, 2.75, 1.25, 1.5)))
   writePNG("test_02_3.png", func = plot.osg, arg_list = list(data, xunit = "samples", type = "l", mai = c(2.75, 2.75, 1.25, 1.5)))
   
   data <- get.osg("DOOM.wav", from = 10, to = 30, units = "seconds")
   writePNG("test_03.png", func = plot.osg, arg_list = list(data, mai = c(2.75, 2.75, 1.25, 1.5)))
   
   data_mp3 <- get.osg("DOOM.mp3")
   writePNG("test_04.png", func = plot.osg, arg_list = list(data_mp3, mai = c(2.75, 2.75, 1.25, 1.5)))
   
   data <- get.osg("01_16.wav")
   writePNG("test_05.png", func = plot.osg, arg_list = list(data, mai = c(2.75, 2.75, 1.25, 1.5)))
   
   data <- get.osg("01_16.wav", from = 0, to = 2, units = "seconds")
   writePNG("test_05_1.png", func = plot.osg, arg_list = list(data, type = "l", mai = c(2.75, 2.75, 1.25, 1.5)))
   
   #Periodogram
   data <- get.osg("01_16.wav")
   prg <- get.prg(data, width = 32768, overlap = -(data@samp.rate - 32768))
   for (i in 1:length(prg@spec)) {
      name <- paste0("0", i)
      writePNG(paste0("test_06_", substr(name, nchar(name) - 1, nchar(name)), ".png"), func = plot.prg, arg_list = list(prg, which = i, ylim = c(-120, 0), log = "x"))
   }
   writePNG(paste0("test_07_1.png"), func = plot.prg, arg_list = list(prg, which = 1, ylim = c(-120, 0)))
   writePNG(paste0("test_07_2.png"), func = plot.prg, arg_list = list(prg, which = 1, ylim = c(-120, 0), log = "x"))
   
   wav <- get.osg("DOOM.wav")
   prg <- get.prg(wav, width = 4096, overlap = 1024)
   writePNG("test_08_1.png", func = plot.prg, arg_list = list(prg, which = 45))
   writePNG("test_08_2.png", func = plot.prg, arg_list = list(prg, which = 45, log = "x"))

   #Spectre
   wav <- get.osg("DOOM.wav")
   
   pws <- get.pws(wav, fs_band = 8000)
   writePNG("test_09_1.png", func = plot.pws, arg_list = list(pws))

   pws <- get.pws(wav, fs_band = 8000)
   writePNG("test_09_2.png", func = plot.pws, arg_list = list(pws, log = TRUE))
   
   pws <- get.pws(wav, postproc = list(log = TRUE, norm = TRUE, clip = TRUE, min_db = -150, max_db = -65))
   writePNG("test_09_3.png", func = plot.pws, arg_list = list(pws))
   
   wav <- get.osg("DOOM.wav", from = 29.5, to = 30, units = "seconds")
   pws <- get.pws(wav, fs_band = 8000)
   writePNG("test_10.png", func = plot.pws, arg_list = list(pws, log = TRUE))

   NULL
}

# Experiment
lection8.make <- function() {
     lection8.test()
   
     # Pentogram spectre
     # full
     data1 <- get.osg("DOOM.wav")
     writePNG("01.png", func = plot.osg, arg_list = list(data1, title = "Осцилограма"))
     pws1 <- get.pws(data1, 0.025, 0.005, fs_band = 17500, postproc = list(log = TRUE, norm = TRUE, clip = TRUE, min_db = -150, max_db = -65))
     writePNG("02.png", func = plot.pws, arg_list = list(pws1, title = "Спектрограмма"))
     # part
     data2 <- get.osg("DOOM.wav", from = 15.5, to = 17.5, units = "seconds")
     writePNG("03.png", func = plot.osg, arg_list = list(data2, title = "Осцилограма"))
     pws2 <- get.pws(data2, 0.025, 0.005, fs_band = 17500, postproc = list(log = TRUE, norm = TRUE, clip = TRUE, min_db = -150, max_db = -65))
     writePNG("04.png", func = plot.pws, arg_list = list(pws2, title = "Спектрограмма"))
     # Pentogram spectre
     
     # Referenced wav
     # full oscilogram
     wav <- get.osg("01_16.wav")
     writePNG("05.png", func = plot.osg, arg_list = list(wav, title = "Осцилограма"))
     
     for (i in 0:5) {
          # part oscilogram
          data <- get.osg("01_16.wav", from = 2*i, to = 2*i + 0.02, units = "seconds")
          writePNG(paste0(sprintf("%02d", 6 + i), "_osg.png"), func = plot.osg, arg_list = list(data, title = "Осцилограма", lwd = (if (i < 3) 2 else 1), draw_ox = FALSE))
          # part spectre
          data <- get.osg("01_16.wav", from = 2*i, to = 2*i + 1, units = "seconds")
          prg <- get.prg(data, width = 32768, overlap = -(data@samp.rate - 32768))
          writePNG(paste0(sprintf("%02d", 6 + i), "_prg.png"), func = plot.prg, arg_list = list(prg, which = 1, ylim = c(-120, 0), log = "x", title = "Спектр"))
     }
     # Referenced wav
     
     # Compare mp3
     # mp3 <- get.osg("03_constant_128.mp3", from = 2258) # constant 128 kbps delta: 10.83% (compression = 5.48 times)
     # mp3 <- get.osg("04_constant_320.mp3", from = 2258) # constant 320 kbps delta: 5.98% (compression = 2.19 times)
     # mp3 <- get.osg("05_variable_128.mp3", from = 2258) # variable 128 kbps delta: 14.45% (compression = 9.48 times)

     file_name_list = c("03_constant_128.mp3", "04_constant_320.mp3", "05_variable_128.mp3")
     description_list = c("constant 128 kbps", "constant 320 kbps", "variable 128 kbps")
     for (i in 1:length(file_name_list)) {
          mp3 <- get.osg(file_name_list[i], from = 2258)
          writePNG(paste0(sprintf("%02d", 11 + i), "_osg.png"), func = plot.osg, arg_list = list(mp3, title = paste0("Осцилограма", sprintf(" (%s)", description_list[i]))))
          
          data <- wav
          data@left[1:length(mp3@left)] <- wav@left[1:length(mp3@left)] - mp3@left
          data@left <- pmax( -32768, pmin(data@left, 32767))
          writePNG(paste0(sprintf("%02d", 11 + i), "_osg_delta.png"), func = plot.osg, arg_list = list(data, title = paste0("Осцилограма искажения", sprintf(" (%s)", description_list[i]))))
          
          pws <- get.pws(data, postproc = list(log = TRUE, norm = TRUE, clip = TRUE, min_db = -150, max_db = -65))
          writePNG(paste0(sprintf("%02d", 11 + i), "_pws.png"), func = plot.pws, arg_list = list(pws, title = paste0("Спектр искажения", sprintf(" (%s)", description_list[i]))))
          
          print(paste0(description_list[i], " delta: ", sprintf("%.2f%%", 100.0*sum(abs(as.numeric(data@left)))/sum(abs(as.numeric(wav@left)))), sprintf(" (compression = %.2f times)", file.size("01_16.wav")/file.size(file_name_list[i]))))
     }
     # Compare mp3
     
     # Compare flac
     # flac <- get.osg("07_faster_24.wav") # flac faster 24 bit delta: 0.0313% (compression = 1.41 times)
     # flac <- get.osg("08_best_24.wav")   # flac best 24 bit delta: 0.0314% (compression = 1.75 times)

     file_name_list = c("07_faster_24.wav", "08_best_24.wav")
     flac_name_list = c("07_faster_24.flac", "08_best_24.flac")
     description_list = c("flac faster 24 bit", "flac best 24 bit")
     for (i in 1:length(file_name_list)) {
          flac <- get.osg(file_name_list[i])
          writePNG(paste0(sprintf("%02d", 14 + i), "_osg.png"), func = plot.osg, arg_list = list(flac, title = paste0("Осцилограма", sprintf(" (%s)", description_list[i]))))
          
          data <- wav
          data@left[1:length(flac@left)] <- wav@left[1:length(flac@left)] - flac@left
          data@left <- pmax( -32768, pmin(data@left, 32767))
          writePNG(paste0(sprintf("%02d", 14 + i), "_osg_delta.png"), func = plot.osg, arg_list = list(data, title = paste0("Осцилограма искажения", sprintf(" (%s)", description_list[i]))))
          
          pws <- get.pws(data, postproc = list(log = TRUE, norm = TRUE, clip = TRUE, min_db = -150, max_db = -65))
          writePNG(paste0(sprintf("%02d", 14 + i), "_pws.png"), func = plot.pws, arg_list = list(pws, title = paste0("Спектр искажения", sprintf(" (%s)", description_list[i]))))
          
          print(paste0(description_list[i], " delta: ", sprintf("%.4f%%", 100.0*sum(abs(as.numeric(data@left)))/sum(abs(as.numeric(wav@left)))), sprintf(" (compression = %.2f times)", file.size("01_16.wav")/file.size(flac_name_list[i]))))
     }
     # Compare flac
     
     # Steganography
     wav_fn_in <- "01_16.wav"
     wav_fn_out <- "!01_16.wav"
     steg_fn_in <- "01_16.7z"
     steg_fn_out <- "!01_16.7z"
     
     data_steg <- get.osg(wav_fn_in)
     fsize <- file.info(steg_fn_in)$size
     if (fsize > length(data_steg@left))
          warning("Wav file is too small")
     file <- file(steg_fn_in, "rb")
     
     pos <- 1
     for (i in 1:fsize) {
          raw <- readBin(file, "raw", n = 1)
          bits <- rawToBits(raw)
          for (j in 1:length(bits)) {
               data_steg@left[pos] <- bitwAnd(data_steg@left[pos], bitwNot(1)) # make little bit equal 0
               if (as.integer(bits[j]) == 1)
                    data_steg@left[pos] <- data_steg@left[pos] + 1
               pos <- pos + 1
          }
     }
     close(file)
     writeWave(data_steg, wav_fn_out)
     
     #
     dataIn <- get.osg(wav_fn_in, units = "samples", from = 1, to = fsize)
     dataOut <- get.osg(wav_fn_out, units = "samples", from = 1, to = fsize)
     dataOut@left <- dataIn@left - dataOut@left
     writePNG("17.png", func = plot.osg, arg_list = list(dataOut, title = "Осцилограма искажения (variable 128 kbps)", xunit = "samples", lwd = 2))
     print(paste0(wav_fn_out, " delta: ", sprintf("%.4f%%", 100.0*sum(abs(as.numeric(dataOut@left)))/sum(abs(as.numeric(dataIn@left)))), sprintf(" (compression = %.2f times)", file.size(wav_fn_in)/file.size(wav_fn_out))))
     # !01_16.wav delta: 0.0025% (compression = 1.00 times)
     
     data_steg <- get.osg(wav_fn_out)
     file <- file(steg_fn_out, "wb")
     pos <- 1
     for (i in 1:fsize) {
          bits <- raw(len = 8)
          for (j in 1:8) {
               bits[j] <- intToBits(data_steg@left[pos])[1]
               pos <- pos + 1
          }
          writeBin(packBits(bits, "raw"), file)
     }
     close(file)
}

lection8.make()