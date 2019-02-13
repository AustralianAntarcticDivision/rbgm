update_vertmix <- function(bfile, outfile, values) {
  if (bfile == outfile) stop("outfile should be a new file")
  if (file.exists(outfile)) stop(sprintf("outfile already exists: %s", outfile))
  tx <- readLines(bfile)
  idx <- grep("box.*\\.vertmix", tx)
  if (!length(idx) == length(values)) {
    if (!length(values) == 1) stop(sprintf("length of values should be 1 or the number of boxes: %i", length(idx)))
  }
  l <- do.call(rbind, strsplit(tx[idx], "\\s"))
  l[,2] <- as.character(values)
  tx[idx] <- paste(l[,1], l[,2], sep = " ")
  writeLines(tx, outfile)
}

update_horizmix <- function(bfile, outfile, values) {
  if (bfile == outfile) stop("outfile should be a new file")
  if (file.exists(outfile)) stop(sprintf("outfile already exists: %s", outfile))
  tx <- readLines(bfile)
  idx <- grep("box.*\\.horizmix", tx)
  if (!length(idx) == length(values)) {
    if (!length(values) == 1) stop(sprintf("length of values should be 1 or the number of boxes: %i", length(idx)))
  }
  l <- do.call(rbind, strsplit(tx[idx], "\\s"))
  l[,2] <- as.character(values)
  tx[idx] <- paste(l[,1], l[,2], sep = " ")
  writeLines(tx, outfile)
}
