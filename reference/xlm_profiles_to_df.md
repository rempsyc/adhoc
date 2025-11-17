# Read legacy profile XML into a data frame

Parses a legacy XML dump where records are stored under `<ROW>` nodes.
The function reads raw bytes, drops NULs, interprets as
Latin-1/Windows-1252, converts to UTF-8, fixes/sets the XML declaration,
and extracts each `<ROW>` as a row. Columns are the union of child
element names across rows; missing fields are filled with `NA`.

## Usage

``` r
xlm_profiles_to_df(path)
```

## Arguments

- path:

  Character scalar. Path to the XML file.

## Value

A base `data.frame` with one row per `<ROW>` and character columns for
each child element name found in the file.

## Details

This helper is designed for messy, non-UTF-8 dumps (e.g., Windows-1252)
that may contain NUL bytes. It performs minimal normalization and does
**not** coerce types; parse dates or numerics downstream as needed.

## See also

[`xml2::read_xml()`](http://xml2.r-lib.org/reference/read_xml.md),
[`xml2::xml_find_all()`](http://xml2.r-lib.org/reference/xml_find_all.md)

## Examples

``` r
# \donttest{
xml_txt <- paste0(
  '<?xml version="1.0"?>',
  '<ROWSET>',
  '  <ROW><ID>1</ID><PROFILE_TYPE>buyer</PROFILE_TYPE></ROW>',
  '  <ROW><ID>2</ID><PROFILE_TYPE>seller</PROFILE_TYPE><CITY>Panamá</CITY></ROW>',
  '</ROWSET>'
)
f <- tempfile(fileext = ".xml"); writeLines(xml_txt, f)
xlm_profiles_to_df(f)
#>   ID PROFILE_TYPE   CITY
#> 1  1        buyer   <NA>
#> 2  2       seller Panamá
unlink(f)
# }
```
