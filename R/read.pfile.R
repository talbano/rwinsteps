read.pfile <- function(filename, skip = 1, col.names,
    sep = ",", header = TRUE, ...) {

    out <- read.table(filename, skip = skip, sep = sep,
        header = header, ...)

    if(!missing(col.names))
        colnames(out) <- col.names
    else {
        colnames(out) <- gsub("X.", "", colnames(out))
        colnames(out) <- tolower(colnames(out))
    }

    out <- as.pfile(out)

    return(out)
}
