skip_if_not_installed("xml2")

test_that("parses a simple well-formed XML file", {
  xml_txt <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<ROWSET>',
    '  <ROW>',
    '    <ID>1</ID>',
    '    <USERID>100</USERID>',
    '    <PROFILE_TYPE>buyer</PROFILE_TYPE>',
    '    <TIMESTAMP>07-JAN-13</TIMESTAMP>',
    '  </ROW>',
    '  <ROW>',
    '    <ID>2</ID>',
    '    <USERID>200</USERID>',
    '    <PROFILE_TYPE>seller</PROFILE_TYPE>',
    '    <CITY>Panamá</CITY>',
    '    <UPDATE_TIMESTAMP>07-JAN-13</UPDATE_TIMESTAMP>',
    '  </ROW>',
    '</ROWSET>'
  )

  f <- tempfile(fileext = ".xml")
  writeLines(xml_txt, f, useBytes = TRUE)

  df <- xlm_profiles_to_df(f)

  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 2L)
  expect_setequal(
    colnames(df),
    c("ID", "USERID", "PROFILE_TYPE", "TIMESTAMP", "UPDATE_TIMESTAMP", "CITY")
  )
  expect_equal(df$ID, c("1", "2"))
  expect_equal(df$PROFILE_TYPE, c("buyer", "seller"))
  expect_true(is.na(df$CITY[1]))
  expect_equal(df$CITY[2], "Panamá")
})

test_that("handles Latin-1 bytes, NULs, and missing XML declaration", {
  xml_txt <- paste0(
    '<ROWSET>',
    '  <ROW>',
    '    <ID>10</ID>',
    '    <PROFILE_TYPE>seller</PROFILE_TYPE>',
    '    <CITY>Isla Colón</CITY>',
    '  </ROW>',
    '  <ROW>',
    '    <ID>11</ID>',
    '    <PROFILE_TYPE>buyer</PROFILE_TYPE>',
    '    <HQ_COUNTRY>PA</HQ_COUNTRY>',
    '  </ROW>',
    '</ROWSET>'
  )

  latin1_raw <- iconv(xml_txt, from = "UTF-8", to = "latin1", toRaw = TRUE)[[1]]
  insert_nul <- function(x, pos) {
    append(x, as.raw(0x00), after = pos)
  }
  latin1_raw <- insert_nul(latin1_raw, max(1, floor(length(latin1_raw) / 4)))
  latin1_raw <- insert_nul(latin1_raw, max(2, floor(length(latin1_raw) / 2)))

  f <- tempfile(fileext = ".xml")
  con <- file(f, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(latin1_raw, con)

  df <- xlm_profiles_to_df(f)

  expect_true(is.data.frame(df))
  expect_equal(nrow(df), 2L)
  expect_setequal(colnames(df), c("ID", "PROFILE_TYPE", "CITY", "HQ_COUNTRY"))
  expect_equal(df$ID, c("10", "11"))
  expect_equal(df$PROFILE_TYPE, c("seller", "buyer"))
  expect_equal(df$CITY[1], "Isla Colón")
  expect_true(is.na(df$CITY[2]))
  expect_equal(df$HQ_COUNTRY[2], "PA")

  all_chr <- vapply(df, is.character, logical(1))
  expect_true(all(all_chr))
})
