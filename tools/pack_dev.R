## Default repo
local({
r <- getOption("repos")
r["CRAN"] <- "https://cloud.r-project.org"
options(repos=r)
})

# install.packages("devtools")
install.packages("renv")
# install.packages("tidyverse")

renv::install("usethis")
usethis::create_package("/Users/peterfuleky/Documents/UHERO/UHERO_work/forecast/fcutils")

renv::init()

usethis::use_blank_slate()

usethis::use_mit_license()

usethis::use_r("fcst_utils.R")

usethis::use_package_doc()
usethis::use_pipe()
usethis::use_tibble()

usethis::use_package("dplyr")
usethis::use_package("purrr") # map, map2, reduce, flatten, discard, pluck
usethis::use_package("stringr")
usethis::use_package("readr") # write_csv, write_file
usethis::use_package("slider") # slide_dbl
usethis::use_package("tidyr") # drop_na
usethis::use_package("tibble") # tibble, as_tibble, enframe
usethis::use_package("magrittr")
usethis::use_package("lubridate") # ymd
usethis::use_package("tempdisagg") # td
usethis::use_package("dygraphs")
usethis::use_package("xts") # xts, nquarters
usethis::use_package("httr") # GET, content, add_headers
usethis::use_package("jsonlite") # fromJSON
usethis::use_package("htmltools") # tagList, browsable
usethis::use_package("htmlwidgets") # tagList, browsable
usethis::use_package("tsbox")
usethis::use_package("reactable")
usethis::use_package("rmarkdown")
usethis::use_package("bimets", "Suggests")
usethis::use_package("gets", "Suggests")
usethis::use_package("dynlm", "Suggests")

usethis::use_import_from("magrittr", "%$%")
usethis::use_import_from("rlang", ":=")
usethis::use_import_from("rlang", "!!")
usethis::use_import_from("rlang", "sym")
usethis::use_import_from("rlang", ".data")
usethis::use_import_from("tsbox", "%ts+%")
usethis::use_import_from("tsbox", "%ts-%")
usethis::use_import_from("tsbox", "%ts*%")
usethis::use_import_from("tsbox", "%ts/%")

renv::status()
renv::snapshot()

devtools::document()
devtools::load_all()
devtools::check()

# pak::pkg_deps_tree("tsbox")

# utils::globalVariables(".")

usethis::use_readme_md()

# usethis::use_git_config(user.name = "Peter Fuleky", user.email = "fuleky@hawaii.edu")
usethis::use_git()
usethis::use_github(organisation = "UHERO", private = FALSE)

usethis::use_github_action()

usethis::git_sitrep()

usethis::use_citation()

# usethis::use_vignette("fcutils-vignette")

usethis::use_pkgdown()
pkgdown::build_site()

usethis::use_pkgdown_github_pages()

usethis::proj_get()
usethis::proj_sitrep()

usethis::use_version()


