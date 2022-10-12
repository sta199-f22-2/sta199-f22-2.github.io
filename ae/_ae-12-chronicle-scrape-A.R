# load packages ----------------------------------------------------------------

library(tidyverse)
library(rvest)
library(lubridate)
library(robotstxt)

# check that we can scrape data from the chronicle -----------------------------

paths_allowed("https://www.dukechronicle.com")

# read page --------------------------------------------------------------------

page <- read_html("https://www.dukechronicle.com/section/opinion?page=1&per_page=100")

# parse components -------------------------------------------------------------

titles <- page |>
  html_elements(".headline a") |>
  html_text()

authors_dates <- page |>
  html_elements(".col-md-8 .dateline") |>
  html_text2() |>
  str_remove("By ")

abstracts <- page |>
  html_elements(".article-abstract") |>
  html_text2()

article_types <- page |>
  html_elements(".kicker a:nth-child(1)") |>
  html_text()

columns <- page |>
  html_elements(".kicker a+ a") |>
  html_text()

urls <- page |>
  html_elements(".headline a") |>
  html_attr(name = "href")

# create a data frame ----------------------------------------------------------

chronicle_raw <- tibble(
  title = titles,
  author_date = authors_dates,
  abstract = abstracts,
  article_type = article_types,
  column = columns,
  url = urls
)

# clean up data ----------------------------------------------------------------

chronicle <- chronicle_raw |>
  separate(author_date, into = c("author", "date"), sep = "\\| ", fill = "left") |>
  mutate(
    date = case_when(
      str_detect(date, "hours ago") ~ "October 12, 2022",
      str_detect(date, "5 days ago") ~ "October 7, 2022",
      str_detect(date, "6 days ago") ~ "October 6, 2022",
      TRUE ~ date
    ),
    article_type = str_to_title(article_type),
    column = str_to_title(column),
    date = mdy(date)
  )

# write data -------------------------------------------------------------------

write_csv(chronicle, file = "data/chronicle.csv")
