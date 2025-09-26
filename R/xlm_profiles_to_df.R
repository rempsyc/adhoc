#' Read legacy profile XML into a data frame
#'
#' @description
#' Parses a legacy XML dump where records are stored under `<ROW>` nodes. The
#' function reads raw bytes, drops NULs, interprets as Latin-1/Windows-1252,
#' converts to UTF-8, fixes/sets the XML declaration, and extracts each `<ROW>`
#' as a row. Columns are the union of child element names across rows; missing
#' fields are filled with `NA`.
#'
#' @param path Character scalar. Path to the XML file.
#'
#' @return
#' A base `data.frame` with one row per `<ROW>` and character columns for each
#' child element name found in the file.
#'
#' @details
#' This helper is designed for messy, non-UTF-8 dumps (e.g., Windows-1252) that
#' may contain NUL bytes. It performs minimal normalization and does **not**
#' coerce types; parse dates or numerics downstream as needed.
#'
#' @examples
#' \donttest{
#' xml_txt <- paste0(
#'   '<?xml version="1.0"?>',
#'   '<ROWSET>',
#'   '  <ROW><ID>1</ID><PROFILE_TYPE>buyer</PROFILE_TYPE></ROW>',
#'   '  <ROW><ID>2</ID><PROFILE_TYPE>seller</PROFILE_TYPE><CITY>Panamá</CITY></ROW>',
#'   '</ROWSET>'
#' )
#' f <- tempfile(fileext = ".xml"); writeLines(xml_txt, f)
#' xlm_profiles_to_df(f)
#' unlink(f)
#' }
#'
#' @seealso
#' [xml2::read_xml()], [xml2::xml_find_all()]
#'
#' @export
xlm_profiles_to_df <- function(path) {
  insight::check_if_installed("xml2")
  raw <- readBin(path, "raw", file.info(path)$size)
  if (!length(raw)) {
    stop("File appears empty: ", path)
  }
  raw <- raw[raw != as.raw(0x00)]
  i <- which(raw == as.raw(0x3C))[1]
  if (is.na(i)) {
    stop("No '<' byte found.")
  }
  raw <- raw[i:length(raw)]
  txt0 <- rawToChar(raw)
  txt <- iconv(txt0, from = "UTF-8", to = "UTF-8", sub = NA)
  if (is.na(txt)) {
    txt <- iconv(txt0, from = "latin1", to = "UTF-8", sub = "byte")
  }
  if (grepl("^\\s*<\\?xml", txt)) {
    txt <- sub(
      "^\\s*<\\?xml[^>]*\\?>",
      '<?xml version="1.0" encoding="UTF-8"?>',
      txt
    )
  } else {
    txt <- paste0('<?xml version="1.0" encoding="UTF-8"?>\n', txt)
  }
  doc <- xml2::read_xml(txt, encoding = "UTF-8")
  rows <- xml2::xml_find_all(doc, ".//ROW")
  lst <- lapply(rows, function(r) {
    k <- xml2::xml_children(r)
    stats::setNames(xml2::xml_text(k), xml2::xml_name(k))
  })
  nms <- unique(unlist(lapply(lst, names)))
  as.data.frame(
    do.call(
      rbind,
      lapply(lst, function(x) {
        y <- stats::setNames(rep(NA_character_, length(nms)), nms)
        y[names(x)] <- x
        y
      })
    ),
    stringsAsFactors = FALSE
  )
}
