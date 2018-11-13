hexAbc <- function(str_abc, prefix = "") {
     
  abc <- strsplit(str_abc, "")[[1]]
  abc_utf8 <- enc2utf8(abc)
  
  hex <- sapply(abc_utf8, charToRaw, simplify = TRUE)
  hex_str <- paste0(prefix, toupper(hex))
  
  utf8 <- sapply(hex_str, as.character)
  dim(utf8) <- dim(hex)
  
  if (!is.null(dim(utf8)))
    utf8 <- apply(utf8, 2, paste, collapse = "\t")
  names(utf8) <- abc
  
  utf8
}

hexStr <- function(str, prefix = "") {
   str_utf8 <- enc2utf8(str)
   hex <- charToRaw(str_utf8)
   utf8 <- paste0(prefix, toupper(hex))
   paste(utf8, collapse = " ")
}

hexInt <- function(int, bytesCount = 4, endian = c("little", "big"), prefix = "") {
   bits <- intToBits(int)
   bytes <- packBits(bits, type = "raw")
   bytes <- as.character(bytes)
   if (length(bytes) > bytesCount) {
      bytes <- bytes[1:bytesCount]
   } else {
      bytes <- c(bytes, rep("00", bytesCount - length(bytes)))
   }
   bytes <- paste0(prefix, toupper(bytes))
   if (endian[1] == "big") {
      bytes <- rev(bytes)
   }
   paste(bytes, collapse = " ")
}
