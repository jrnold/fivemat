library("httr")
library("purrr")

locale_paths <-
  GET("https://unpkg.com/d3-format@latest/locale/?json") %>%
  content() %>%
  `[[`("files") %>%
  map("path")

fmt_locales <- list()
for (path in locale_paths) {
  x <- content(GET(file.path("https://unpkg.com/d3-format@latest", path)))
  x$grouping <- flatten_int(x$grouping)
  x$currency <- flatten_chr(x$currency)
  if (!is.null(x$numerals)) x$numerals <- flatten_chr(x$numerals)
  x$grouping_mark <- x$thousands
  x$thousands <- NULL
  x$decimal_mark <- x$decimal
  x$decimal <- NULL
  structure(x, class = "fmt_locale")
  nm <- tools::file_path_sans_ext(basename(path))
  fmt_locales[[nm]] <- structure(x, class = "fmt_locale")
}

save(fmt_locales, file = "data/fmt_locales.rda")
