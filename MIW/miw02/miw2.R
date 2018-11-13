setwd("c:/TkacheniaAV/MyDisc/Git/multimedia_course/MIW/miw02")
#setwd("D:/MyDisk/Git/multimedia_course/MIW/miw02")
Sys.setlocale("LC_CTYPE", "russian")
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
   # 5, 6 - No error
   idxError         <- sample(1:6, 1)
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
   } else if (idx_list$Error == 2) {
      ByteRate <- ByteRate + error
   } else if (idx_list$Error == 3) {
      BlockAlign <- BlockAlign + error
   } else if (idx_list$Error == 4){
      Subchunk2Size <- Subchunk2Size + error
   }
   ChunkSize     <- make.Field(c(ChunkSize))
   ByteRate      <- make.Field(c(ByteRate))
   BlockAlign    <- make.Field(c(BlockAlign), bytesCount = 2)
   Subchunk2Size <- make.Field(c(Subchunk2Size))
   
   list(ChunkSize = ChunkSize, ByteRate = ByteRate, BlockAlign = BlockAlign, Subchunk2Size = Subchunk2Size)
}

getData <- function(ChunkID, Format, Subchunk1ID, Subchunk1Size, AudioFormat, NumChannels, SampleRate, BitsPerSample, Subchunk2ID,
                    fields_list, idx_list, value_count_in_line = 16, value_period = 3) {
   data <- paste(ChunkID, fields_list$ChunkSize, Format,
                 Subchunk1ID, Subchunk1Size, AudioFormat, NumChannels[idx_list$NumChannels], SampleRate[idx_list$SampleRate],
                 fields_list$ByteRate, fields_list$BlockAlign, BitsPerSample[idx_list$BitsPerSample],
                 Subchunk2ID, fields_list$Subchunk2Size)
   data <- substring(data, seq(1, nchar(data), value_count_in_line*value_period),
                           c(seq(value_count_in_line*value_period - 1, nchar(data), value_count_in_line*value_period), nchar(data)))
   data
}

getError <- function(idxError) {
   error <- ""
   if (idxError == 1) {
      error <- "ChunkSize"
   } else if (idxError == 2) {
      error <- "ByteRate"
   } else if (idxError == 3) {
      error <- "BlockAlign"
   } else if (idxError == 4){
      error <- "Subchunk2Size"
   }
   
   error
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

miw2.make <- function(count) {
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
      data_1 <- getData(ChunkID, Format, Subchunk1ID, Subchunk1Size, AudioFormat, NumChannels, SampleRate, BitsPerSample, Subchunk2ID, fields_1, idx_1)
      
      idx_2 <- getSampleIdx(NumChannels, SampleRate, BitsPerSample)
      fields_2 <- getFields(NumChannels, SampleRate, BitsPerSample, idx_2, Subchunk1Size)
      data_2 <- getData(ChunkID, Format, Subchunk1ID, Subchunk1Size, AudioFormat, NumChannels, SampleRate, BitsPerSample, Subchunk2ID, fields_2, idx_2)
      
      text <- paste0(text,
                     "ФИО\t", "\t", "\t",
                     "ФИО\t", "\t", "\t", "\n")
      text <- paste0(text,
                     paste(year, 2*i - 1, sep="_"), "\t", "Группа\t", paste(year, 2*i - 1, sep="_"), "\t",
                     paste(year, 2*i, sep="_"), "\t", "Группа\t", paste(year, 2*i, sep="_"), "\t", "\n")
      text <- paste0(text,
                     "Данные\t", data_1[1], "\t", "\t",
                     "Данные\t", data_2[1], "\t", "\t", "\n")
      text <- paste0(text,
                     "\t", data_1[2], "\t", "\t",
                     "\t", data_2[2], "\t", "\t", "\n")
      text <- paste0(text,
                     "\t", data_1[3], "\t", "\t",
                     "\t", data_2[3], "\t", "\t", "\n")
      text <- paste0(text,
                     "SampleRate:\t", "\t", getValue(SampleRate, idx_1$SampleRate), "\t",
                     "SampleRate:\t", "\t", getValue(SampleRate, idx_2$SampleRate), "\t", "\n")
      text <- paste0(text,
                     "BitsPerSample:\t", "\t", getValue(BitsPerSample, idx_1$BitsPerSample), "\t",
                     "BitsPerSample:\t", "\t", getValue(BitsPerSample, idx_2$BitsPerSample), "\n")
      text <- paste0(text,
                     "Методан. верны\t", "\t", (if (idx_1$Error < 5) "Нет," else "Да"), "\t",
                     "Методан. верны\t", "\t", (if (idx_2$Error < 5) "Нет," else "Да"), "\t", "\n")
      text <- paste0(text,
                     "да/нет (почему)?\t", "\t", getError(idx_1$Error), "\t",
                     "да/нет (почему)?\t", "\t", getError(idx_2$Error), "\t", "\n")

   }
   
   file <- file("miw2.txt", "w", encoding = "UTF-8")
   write(text, file)
   close(file)
   
   NULL
}

miw2.make(40)