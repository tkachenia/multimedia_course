# install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# local_path <- "D:/Ubuntu/Share/Курс_Мультимедийные системы и среды"
# setwd(paste0(local_path, "/multimedia_course/MIW/miw02"))

Sys.setlocale("LC_CTYPE", "russian")
source("../base.R")

getFields <- function() {
   fields <- list()
   fields["ChunkID"] <- makeField("RIFF")
   fields["ChunkSize"] <- NA
   fields["Format"] <- makeField("WAVE")
   fields["Subchunk1ID"] <- makeField("fmt ")
   fields["Subchunk1Size"] <- makeField(16)
   fields["AudioFormat"] <- makeField(1, bytesCount = 2)
   fields["NumChannels"] <- makeField(c(1, 2), bytesCount = 2)
   fields["SampleRate"] <- makeField(c(8000, 16000, 22050, 44100, 48000, 96000, 192000))
   fields["ByteRate"] <- NA
   fields["BlockAlign"] <- NA
   fields["BitsPerSample"] <- makeField(c(8, 16, 24, 32), bytesCount = 2)
   fields["Subchunk2ID"] <- makeField("data")
   fields["Subchunk2Size"] <- NA
   
   tags <- as.vector(c("ChunkID", "ChunkSize", "Format",
                       "Subchunk1ID", "Subchunk1Size", "AudioFormat", "NumChannels", "SampleRate", "ByteRate", "BlockAlign", "BitsPerSample",
                       "Subchunk2ID", "Subchunk2Size"))
   
   list(field = fields, tag = tags)
}

getErrorIdx <- function(fields, train = F) {
   idxError <- sample(1:length(fields$tag), 1)
   if (!train) {
      # 0  - No error
      idxError <- sample(c(0, 0, which(fields$tag %in% c("ChunkSize", "ByteRate", "BlockAlign", "Subchunk2Size"))), 1)
   }
   
   idxError
}

getValue <- function(field, idx = 1) {
   item <- field[[1]][idx]
   name <- names(item)
   
   as.integer(name)
}

getSampleIdx <- function(fields, idxError) {
   idxNumChannels   <- sample(1:length(fields$field["NumChannels"][[1]]), 1)
   idxSampleRate    <- sample(1:length(fields$field["SampleRate"][[1]]), 1)
   idxBitsPerSample <- sample(1:length(fields$field["BitsPerSample"][[1]]), 1)
   
   if (getValue(fields$field["BitsPerSample"], idxBitsPerSample) == 8 && idxError == which(fields$tag %in% "Subchunk2Size")) {
      idxBitsPerSample <- 2
   }

   list(NumChannels = idxNumChannels, SampleRate = idxSampleRate, BitsPerSample = idxBitsPerSample)
}

getFieldsUpdate <- function(fields, idx) {
   fields$field["NumChannels"] <- list(fields$field["NumChannels"][[1]][idx$NumChannels])
   fields$field["SampleRate"]  <- list(fields$field["SampleRate"][[1]][idx$SampleRate])
   fields$field["BitsPerSample"] <- list(fields$field["BitsPerSample"][[1]][idx$BitsPerSample])
   
   BlockAlign    <- getValue(fields$field["NumChannels"])*getValue(fields$field["BitsPerSample"])/8
   ByteRate      <- getValue(fields$field["SampleRate"])*BlockAlign
   Subchunk2Size <- sample(500:1500, 1)*BlockAlign
   ChunkSize     <- 4 + (8 + getValue(fields$field["Subchunk1Size"])) + (8 + Subchunk2Size)

   fields$field["ChunkSize"]     <- makeField(c(ChunkSize))
   fields$field["ByteRate"]      <- makeField(c(ByteRate))
   fields$field["BlockAlign"]    <- makeField(c(BlockAlign), bytesCount = 2)
   fields$field["Subchunk2Size"] <- makeField(c(Subchunk2Size))

   fields
}

getFieldsError <- function (fields, idxError, prefix = "", train = F) {
   if (idxError == 0) return(fields)
   
   error <- sample(1:254, 1)
   if (train) error <- ( if (sample(0:1, 1) == 0) -1 else 1 )
   
   strHex <- fields$field[fields$tag[idxError]][[1]][[1]]
   firstHexValue <- substring(strHex, nchar(prefix) + 1, nchar(prefix) + 2)
   firstValue <- (strtoi(firstHexValue, base = 16) + error) %% 255
   newFirstHexValue <- hexInt(firstValue, bytesCount = 1, prefix = prefix)
   substring(fields$field[fields$tag[idxError]][[1]][[1]], nchar(prefix) + 1, nchar(prefix) + 2) <- newFirstHexValue
   
   fields
}

getData <- function(fields, value_count_in_line = 16, prefix = "") {
   data <- ""
   for (i in 1:length(fields$tag)) {
      data <- paste(data, fields$field[fields$tag[i]])
   }
   data <- substring(data, 2)
   data <- substring(data, seq(1, nchar(data), value_count_in_line*(nchar(prefix) + 2 + 1)),
                           c(seq(value_count_in_line*(nchar(prefix) + 2 + 1) - 1, nchar(data), value_count_in_line*(nchar(prefix) + 2 + 1)), nchar(data)))
   data
}

miw2.make <- function(count, train = F, exam = F) {
   fields <- getFields()
   text <- ""
   year <- format(Sys.Date(), "%y")
   if (train) year <- "trn"
   for ( i in 1:( (count%/%2) + count %% 2) ) {
      error_1  <- getErrorIdx(fields, train)
      idx_1    <- getSampleIdx(fields, error_1)
      fields_1 <- getFieldsUpdate(fields, idx_1)
      fields_1 <- getFieldsError(fields_1, error_1, train = train)
      data_1   <- getData(fields_1)
      
      error_2  <- getErrorIdx(fields, train)
      idx_2    <- getSampleIdx(fields, error_2)
      fields_2 <- getFieldsUpdate(fields, idx_2)
      fields_2 <- getFieldsError(fields_2, error_2, train = train)
      data_2   <- getData(fields_2)

      if (exam) {
         text <- paste0(text, "Вариант:\t", paste(year, 2*i - 1, sep="_"), "\n")
         text <- paste0(text, "Данные:\n")
         for (j in (1:length(data_1))) {
            text <- paste0(text, data_1[j], "\n")
         }
         text <- paste0(text, "SampleRate:\nBitsPerSample:\nМетодан. верны: да/нет (почему)?\n")
         text <- paste0(text, "Ответ:\t", getValue(fields_1$field["SampleRate"]), ", ",
                                          getValue(fields_1$field["BitsPerSample"]), ", ",
                                          (if (error_1 != 0) "Нет" else "Да"), " (", fields_1$tag[error_1], ")\n")
         
         text <- paste0(text, "Вариант:\t", paste(year, 2*i, sep="_"), "\n")
         text <- paste0(text, "Данные:\n")
         for (j in (1:length(data_2))) {
            text <- paste0(text, data_2[j], "\n")
         }
         text <- paste0(text, "SampleRate:\nBitsPerSample:\nМетодан. верны: да/нет (почему)?\n")
         text <- paste0(text, "Ответ:\t", getValue(fields_2$field["SampleRate"]), ", ",
                                          getValue(fields_2$field["BitsPerSample"]), ", ",
                                          (if (error_2 != 0) "Нет" else "Да"), " (", fields_2$tag[error_2], ")\n")
      } else {
         text <- paste0(text,
                        "ФИО\t", "\t", "\t",
                        "ФИО\t", "\t", "\t", "\n")
         text <- paste0(text,
                        paste(year, 2*i - 1, sep="_"), "\t", "Группа\t", paste(year, 2*i - 1, sep="_"), "\t",
                        paste(year, 2*i, sep="_"), "\t", "Группа\t", paste(year, 2*i, sep="_"), "\t", "\n")
         text <- paste0(text,
                        "Данные\t", data_1[1], "\t", "\t",
                        "Данные\t", data_2[1], "\t", "\t", "\n")
         for (j in (2:length(data_1))) {
            text <- paste0(text,
                           "\t", data_1[j], "\t", "\t",
                           "\t", data_2[j], "\t", "\t", "\n")
         }
         text <- paste0(text,
                        "SampleRate:\t", "\t", getValue(fields_1$field["SampleRate"]), "\t",
                        "SampleRate:\t", "\t", getValue(fields_2$field["SampleRate"]), "\t", "\n")
         text <- paste0(text,
                        "BitsPerSample:\t", "\t", getValue(fields_1$field["BitsPerSample"]), "\t",
                        "BitsPerSample:\t", "\t", getValue(fields_2$field["BitsPerSample"]), "\n")
         text <- paste0(text,
                        "Методан. верны\t", "\t", (if (error_1 != 0) "Нет," else "Да"), "\t",
                        "Методан. верны\t", "\t", (if (error_2 != 0) "Нет," else "Да"), "\t", "\n")
         text <- paste0(text,
                        "да/нет (почему)?\t", "\t", fields_1$tag[error_1], "\t",
                        "да/нет (почему)?\t", "\t", fields_2$tag[error_2], "\t", "\n")
      }

   }
   
   file <- file("miw2.txt", "w", encoding = "UTF-8")
   write(text, file)
   close(file)
   
   NULL
}

set.seed(20211020)
miw2.make(60, train=F, exam=F)
