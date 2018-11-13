setwd("c:/TkacheniaAV/MyDisc/Git/multimedia_course/MIW/miw02")
setwd("D:/MyDisk/Git/multimedia_course/MIW/miw02")
source("../base.R")

getValue <- function(field, idx = 1) {
   item <- field[idx]
   name <- names(item)
   as.integer(name)
}

getSampleIdx <- function(NumChannels, SampleRate, BitsPerSample) {
   idxNumChannels   <- sample(1:length(NumChannels), 1)
   idxSampleRate    <- sample(1:length(SampleRate), 1)
   idxBitsPerSample <- sample(1:length(BitsPerSample), 1)
   # 1 - ChunkSize
   # 2 - ByteRate
   # 3 - BlockAlign
   # 4 - Subchunk2Size
   idxError         <- sample(1:4, 1)
   if (getValue(BitsPerSample, idxBitsPerSample) == 8 && idxError == 4) {
      idxNumChannels <- 2
   }
   
   list(NumChannels = idxNumChannels, SampleRate = idxSampleRate, BitsPerSample = idxBitsPerSample, Error = idxError)
}

getFields <- function(NumChannels, SampleRate, BitsPerSample, idx_list, Subchunk1Size) {
   BlockAlign    <- getValue(NumChannels, idx_list$NumChannels)*getValue(BitsPerSample, idx_list$BitsPerSample)/8
   ByteRate      <- getValue(SampleRate, idx_list$SampleRate)*BlockAlign
   Subchunk2Size <- sample(500:1500, 1)*BlockAlign
   ChunkSize     <- 4 + (8 + getValue(Subchunk1Size)) + (8 + Subchunk2Size)
   
   error <- ( if (sample(0:1, 1) == 0) -1 else 1 )
   if (idx_list$Error == 1) {
      ChunkSize <- ChunkSize + error
   } else if (idxError == 2) {
      ByteRate <- ByteRate + error
   } else if (idxError == 3) {
      BlockAlign <- BlockAlign + error
   } else {
      Subchunk2Size <- Subchunk2Size + error
   }
   ChunkSize     <- make.Field(c(ChunkSize))
   ByteRate      <- make.Field(c(ByteRate))
   BlockAlign    <- make.Field(c(BlockAlign), bytesCount = 2)
   Subchunk2Size <- make.Field(c(Subchunk2Size))
   
   list(ChunkSize = ChunkSize, ByteRate = ByteRate, BlockAlign = BlockAlign, Subchunk2Size = Subchunk2Size)
}

make.Field <- function(values = c(""), ...) {
   if ( is.character(values[1]) ) {
      field <- sapply(values, hexStr)
   } else {
      field <- sapply(values, hexInt, ...)
   }
   
   names(field) <- values
   field
}

ksr1.make <- function(count) {
   ChunkID        <- make.Field(c("RIFF"))
   #ChunkSize     <- make.Field(c(0))
   Format         <- make.Field(c("WAVE"))
   Subchunk1ID    <- make.Field(c("fmt "))
   Subchunk1Size  <- make.Field(c(16))
   AudioFormat    <- make.Field(c(1), bytesCount = 2)
   NumChannels    <- make.Field(c(1, 2), bytesCount = 2)
   SampleRate     <- make.Field(c(8000, 16000, 22050, 44100, 48000, 96000, 192000))
   #ByteRate      <- make.Field(c(0))
   #BlockAlign    <- make.Field(c(0), bytesCount = 2)
   BitsPerSample  <- make.Field(c(8, 16, 24, 32), bytesCount = 2)
   Subchunk2ID    <- make.Field(c("data"))
   #Subchunk2Size <- make.Field(c(0))
   
   text <- ""
   year <- format(Sys.Date(), "%y")
   for ( i in 1:( (count%/%2) + count %% 2) ) {
      idx_1 <- getSampleIdx(NumChannels, SampleRate, BitsPerSample)
      fields_1 <- getFields(NumChannels, SampleRate, BitsPerSample, idx_1, Subchunk1Size)
      data_1 <- paste(ChunkID, fields_1$ChunkSize, Format,
                      Subchunk1ID, Subchunk1Size, AudioFormat, NumChannels[idx_1$NumChannels], SampleRate[idx_1$SampleRate], fields_1$ByteRate, fields_1$BlockAlign, BitsPerSample[idx_1$BitsPerSample],
                      Subchunk2ID, fields_1$Subchunk2Size)
      data_1 <- substring(data_1, seq(1, nchar(data_1), 16*3), c(seq(16*3 - 1, nchar(data_1), 16*3), nchar(data_1)))
      
      
      paste(year, 2*i - 1, sep="_")
      paste(year, 2*i, sep="_")
      
      text <- paste0(text,
                     "ФИО\t", "\t", "\t",
                     "ФИО\t", "\t", "\t", "\n")
      text <- paste0(text,
                     "Группа\t", "\t", "\t",
                     "Группа\t", "\t", "\t", "\n")
      text <- paste0(text,
                     "Данные\t", data[1], "\t", "\t",
                     "Данные\t", paste0(rep("\t", 6), collapse = ""), "\n")
      text <- paste0(text,
                     paste(year, 2*i - 1, sep="_"), "\t", getHexString(hex_eng, eng_idx_1, hex_rus, rus_idx_1, idx_1), "\t", getLetter(hex_eng, eng_idx_1, hex_rus, rus_idx_1, idx_1[2]), "\t\t",
                     paste(year, 2*i, sep="_"), "\t", getHexString(hex_eng, eng_idx_2, hex_rus, rus_idx_2, idx_2), "\t", getLetter(hex_eng, eng_idx_2, hex_rus, rus_idx_2, idx_2[2]), "\n")
      text <- paste0(text,
                     "Ответ\t", paste0(rep("\t", 5), collapse = ""), getLetter(hex_eng, eng_idx_1, hex_rus, rus_idx_1, idx_1[3]), "\t\t",
                     "Ответ\t", paste0(rep("\t", 5), collapse = ""), getLetter(hex_eng, eng_idx_2, hex_rus, rus_idx_2, idx_2[3]), "\n")

   }
}



list_ChunkID <- c("RIFF")
ChunkID <- sapply(list_ChunkID, hexStr)
names(ChunkID) <- list_ChunkID

list_ChunkSize <- c(0)
ChunkSize <- sapply(list_ChunkSize, hexStr)
names(ChunkSize) <- list_CChunkSize

ChunkSize <- hexInt(12345, bytesCount = 4)
names(ChunkSize) <- 12345


hexStr(listChunkID)

getLetter <- function(hex_eng, eng_idx, hex_rus, rus_idx, i) {
     letter <- ""
     if (i == 1) { # english
          letter <- names(hex_eng[eng_idx])
     } else {# russion
          letter <- names(hex_rus[rus_idx[i - 1]])
     }
     letter
}

getHexString <- function(hex_eng, eng_idx, hex_rus, rus_idx, idx) {
     hexString <- ""
     for (i in 1:length(idx)) {
          if (idx[i] == 1) { # english
               hexString <- paste(hexString, hex_eng[eng_idx], sep = "\t")
          } else { # russion
               hexString <- paste(hexString, hex_rus[rus_idx[idx[i] - 1]], sep = "\t")
          }
     }
     substring(hexString, 2)
}

ksr1.make <- function(count) {
     eng <- "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
     rus <- "абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
     
     hex_eng <- hexAbc(eng, prefix = "")
     hex_rus <- hexAbc(rus, prefix = "")
     
     text <- ""
     year <- format(Sys.Date(), "%y")
     for ( i in 1:( (count%/%2) + count %% 2) ) {
          eng_idx_1 <- sample(1:length(hex_eng), 1, replace=FALSE)
          rus_idx_1 <- sample(1:length(hex_rus), 2, replace=FALSE)
          idx_1 <- sample(1:3, 3, replace = FALSE) # 1 - english, 2 and 3 - russian

          eng_idx_2 <- sample(1:length(hex_eng), 1, replace=FALSE)
          rus_idx_2 <- sample(1:length(hex_rus), 2, replace=FALSE)
          idx_2 <- sample(1:3, 3, replace = FALSE) # 1 - english, 2 and 3 - russian

          text <- paste0(text,
                         "ФИО\t", paste0(rep("\t", 5), collapse = ""), paste(year, 2*i - 1, sep="_"), "\t\t",
                         "ФИО\t", paste0(rep("\t", 5), collapse = ""), paste(year, 2*i, sep="_"), "\n")
          text <- paste0(text,
                         "Группа\t", paste0(rep("\t", 5), collapse = ""), getLetter(hex_eng, eng_idx_1, hex_rus, rus_idx_1, idx_1[1]), "\t\t",
                         "Группа\t", paste0(rep("\t", 5), collapse = ""), getLetter(hex_eng, eng_idx_2, hex_rus, rus_idx_2, idx_2[1]), "\n")
          text <- paste0(text,
                         paste(year, 2*i - 1, sep="_"), "\t", getHexString(hex_eng, eng_idx_1, hex_rus, rus_idx_1, idx_1), "\t", getLetter(hex_eng, eng_idx_1, hex_rus, rus_idx_1, idx_1[2]), "\t\t",
                         paste(year, 2*i, sep="_"), "\t", getHexString(hex_eng, eng_idx_2, hex_rus, rus_idx_2, idx_2), "\t", getLetter(hex_eng, eng_idx_2, hex_rus, rus_idx_2, idx_2[2]), "\n")
          text <- paste0(text,
                         "Ответ\t", paste0(rep("\t", 5), collapse = ""), getLetter(hex_eng, eng_idx_1, hex_rus, rus_idx_1, idx_1[3]), "\t\t",
                         "Ответ\t", paste0(rep("\t", 5), collapse = ""), getLetter(hex_eng, eng_idx_2, hex_rus, rus_idx_2, idx_2[3]), "\n")
     }
     
     file <- file("miw1.txt", "w", encoding = "UTF-8")
     write(text, file)
     close(file)
     
     NULL
}

ksr1.make(36)


fs <- 8000
bits <- intToBits(fs)
packBits(bits, type = "raw")
