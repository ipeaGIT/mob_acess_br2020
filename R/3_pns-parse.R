parse.sasci_mod <- function (sas_ri, beginline = 1, lrecl = NULL) 
{
  SASinput <- readLines(sas_ri,warn = FALSE,encoding = "latin1")
  SASinput <- gsub("\t", " ", SASinput)
  SASinput <- SASinput[beginline:length(SASinput)]
  SASinput <- toupper(SASinput)
  SASinput <- SAS.uncomment(SASinput, "/*", "*/")
  SASinput <- SAS.uncomment(SASinput, "*", ";")
  firstline <- grep("INPUT", SASinput)[1]
  a <- grep(";", toupper(SASinput))
  lastline <- min(a[a > firstline])
  FWFlines <- SASinput[firstline:lastline]
  input_word <- unlist(gregexpr("INPUT", FWFlines[1], fixed = T))
  FWFlines[1] <- substr(FWFlines[1], input_word + 5, nchar(FWFlines[1]))
  semicolon <- unlist(gregexpr(";", FWFlines[length(FWFlines)], 
                               fixed = T))
  FWFlines[length(FWFlines)] <- substr(FWFlines[length(FWFlines)], 
                                       1, semicolon - 1)
  for (i in 1:length(FWFlines)) FWFlines[i] <- gsub("$", " $ ", 
                                                    FWFlines[i], fixed = T)
  for (i in 1:length(FWFlines)) FWFlines[i] <- gsub("-", " - ", 
                                                    FWFlines[i], fixed = T)
  FWFlines <- FWFlines[which(gsub(" ", "", FWFlines) != "")]
  z <- strsplit(FWFlines, " ", perl = T)
  SAS.input.lines <- NULL
  for (i in 1:length(z)) {
    z[[i]] <- gsub("-", " ", z[[i]])
    z[[i]] <- z[[i]][which(gsub(" ", "", z[[i]]) != "")]
    SAS.input.lines <- c(SAS.input.lines, z[[i]])
  }
  x <- data.frame(NULL)
  i <- j <- 1
  elements_2_4 <- SAS.input.lines[2:4]
  elements_2_4 <- elements_2_4[elements_2_4 != "$"]
  widths_not_places <- (length(elements_2_4) == 2 & is.na(as.numeric(as.character(elements_2_4[2]))))
  if (sum(grepl("@", SAS.input.lines)) > 0) {
    while (i < length(SAS.input.lines)) {
      start.point <- as.numeric(gsub("@", "", SAS.input.lines[i], 
                                     fixed = T))
      if (i > 1) {
        if (x[j - 1, "start"] + x[j - 1, "width"] < 
            start.point) {
          x[j, "width"] <- (x[j - 1, "start"] + x[j - 
                                                    1, "width"]) - start.point
          j <- j + 1
        }
      }
      x[j, "start"] <- start.point
      x[j, "varname"] <- SAS.input.lines[i + 1]
      if (SAS.input.lines[i + 2] == "$") {
        x[j, "char"] <- T
        i <- i + 1
      }
      else x[j, "char"] <- F
      for (k in c("F", "CHAR")) {
        SAS.input.lines[i + 2] <- gsub(k, "", SAS.input.lines[i + 
                                                                2], fixed = T)
      }
      if (grepl(".", SAS.input.lines[i + 2], fixed = T)) {
        period <- unlist(gregexpr(".", SAS.input.lines[i + 
                                                         2], fixed = T))
        x[j, "width"] <- as.numeric(substr(SAS.input.lines[i + 
                                                             2], 1, period - 1))
        divisor <- substr(SAS.input.lines[i + 2], period + 
                            1, nchar(SAS.input.lines[i + 2]))
      }
      else {
        x[j, "width"] <- as.numeric(SAS.input.lines[i + 
                                                      2])
        divisor <- ""
      }
      if (divisor != "") {
        x[j, "divisor"] <- 1/10^as.numeric(divisor)
      }
      else x[j, "divisor"] <- 1
      i <- i + 3
      j <- j + 1
    }
  }
  else if (widths_not_places) {
    while (i < length(SAS.input.lines)) {
      x[j, "varname"] <- SAS.input.lines[i]
      if (SAS.input.lines[i + 1] == "$") {
        x[j, "width"] <- as.numeric(SAS.input.lines[i + 
                                                      2])
        x[j, "char"] <- T
        i <- i + 3
      }
      else {
        x[j, "width"] <- as.numeric(SAS.input.lines[i + 
                                                      1])
        x[j, "char"] <- F
        i <- i + 2
      }
      if (grepl(".", SAS.input.lines[i], fixed = T)) {
        period <- unlist(gregexpr(".", SAS.input.lines[i], 
                                  fixed = T))
        divisor <- substr(SAS.input.lines[i], period + 
                            1, nchar(SAS.input.lines[i]))
        x[j, "divisor"] <- 1/10^as.numeric(divisor)
        i <- i + 1
      }
      else x[j, "divisor"] <- 1
      j <- j + 1
    }
  }
  else {
    while (i < length(SAS.input.lines)) {
      x[j, "varname"] <- SAS.input.lines[i]
      if (SAS.input.lines[i + 1] == "$") {
        x[j, "start"] <- SAS.input.lines[i + 2]
        if (is.na(as.numeric(SAS.input.lines[i + 3])) | 
            grepl(".", SAS.input.lines[i + 3], fixed = T)) {
          x[j, "end"] <- x[j, "start"]
          i <- i - 1
        }
        else {
          x[j, "end"] <- SAS.input.lines[i + 3]
        }
        x[j, "char"] <- T
        i <- i + 4
      }
      else {
        x[j, "start"] <- SAS.input.lines[i + 1]
        if (is.na(as.numeric(SAS.input.lines[i + 2])) | 
            grepl(".", SAS.input.lines[i + 2], fixed = T)) {
          x[j, "end"] <- x[j, "start"]
          i <- i - 1
        }
        else {
          x[j, "end"] <- SAS.input.lines[i + 2]
        }
        x[j, "char"] <- F
        i <- i + 3
      }
      if (grepl(".", SAS.input.lines[i], fixed = T)) {
        period <- unlist(gregexpr(".", SAS.input.lines[i], 
                                  fixed = T))
        divisor <- substr(SAS.input.lines[i], period + 
                            1, nchar(SAS.input.lines[i]))
        x[j, "divisor"] <- 1/10^as.numeric(divisor)
        i <- i + 1
      }
      else x[j, "divisor"] <- 1
      if (j > 1) {
        if (as.numeric(x[j, "start"]) > as.numeric(x[j - 
                                                     1, "end"]) + 1) {
          x <- rbind(x[1:(j - 1), ], NA, x[j, ])
          j <- j + 1
          x[j - 1, "start"] <- as.numeric(x[j - 2, "end"]) + 
            1
          x[j - 1, "end"] <- as.numeric(x[j, "start"]) - 
            1
        }
      }
      j <- j + 1
    }
    x <- transform(x, width = as.numeric(end) - as.numeric(start) + 
                     1)
    x[is.na(x[, "varname"]), "width"] <- (-1 * x[is.na(x[, 
                                                         "varname"]), "width"])
  }
  x <- x[, c("varname", "width", "char", "divisor")]
  if (!is.null(lrecl)) {
    if (lrecl < sum(abs(x$width))) 
      stop("specified logical record length (lrecl) parameter is shorter than the SAS columns constructed")
    if (lrecl > sum(abs(x$width))) {
      length.of.blank.record.to.add.to.end <- (lrecl - 
                                                 sum(abs(x$width)))
      x[nrow(x) + 1, "width"] <- -length.of.blank.record.to.add.to.end
    }
  }
  x
}
