#' Tweet bets
#'
#' @return
#' @export
#'
#' @examples
tweet_bets <- function() {

  twitter_token <- twitter_api_auth()

  bets <- make_bet_table()

  tweet <- make_tweet(bets)

  rtweet::post_tweet(tweet, token = twitter_token)

}

twitter_api_auth <- function() {
  rtweet::create_token(
    app = "basket-bets",
    consumer_key    = Sys.getenv("TWITTER_CONSUMER_KEY"),
    consumer_secret = Sys.getenv("TWITTER_CONSUMER_SECRET"),
    access_token    = Sys.getenv("TWITTER_ACCESS_TOKEN"),
    access_secret   = Sys.getenv("TWITTER_ACCESS_SECRET")
  )
}

make_tweet <- function(bet_table) {

  live_bets <- bet_table %>%

    dplyr::filter(
      bet_kelly_40 > 0
    )

  if(nrow(live_bets) > 0) {
    bets_text <-
      glue::glue_data(
        live_bets,
        "{team} ML at {odds_dec}: {bet_pct} / {scales::dollar(bet_kelly_40)} ($40 BR) "
      ) %>%
      stringr::str_c(
        collapse = "\n"
      ) %>%
      stringr::str_c(
        "\n #NBA"
      )

  } else {
    bets_text <- "No bets right now"
  }

  bets_text

}


#' Make a table of bet recommendations
#'
#' @return
#' @export
#'
#' @examples
make_bet_table <- function() {
  dat_538 <- scrape_538()
  dat_pinnacle <- scrape_pinnacle()
  dat <- dat_pinnacle %>%
    dplyr::inner_join(dat_538) %>%
    dplyr::mutate(
      kelly = ((odds_dec - 1) * win_chance - (1 - win_chance)) / (odds_dec - 1),
      edge = odds_dec / (1 / win_chance) - 1,
      bet_kelly_40 = dplyr::case_when(
        kelly * 40 < 1.40 ~ 0,
        kelly > 0 & edge > 0.15 ~ kelly * 40,
        TRUE ~ 0
      ),
      bet_pct = scales::percent(
        dplyr::case_when(
          bet_kelly_40 > 0 ~ kelly,
          TRUE ~ 0
        ),
        accuracy = 1
      ),
      dplyr::across(
        c(odds_dec, kelly, edge, bet_kelly_40),
        ~ round(.x, 2)
      )
    ) %>%
    dplyr::select(
      date,
      time_stamp_538,
      time_stamp_pinnacle,
      team,
      everything()
    )

  res <- dat

  res
}

scrape_538 <- function() {

  url <- "https://projects.fivethirtyeight.com/2021-nba-predictions/games/"

  req <- httr::GET(url)

  raw <- httr::content(req)

  time_stamp <- raw %>%
    rvest::html_node(".timestamp") %>%
    rvest::html_text() %>%
    lubridate::mdy_hm()

  games_sections <- raw %>% rvest::html_nodes(".day")

  dat <-

    dplyr::tibble(
      time_stamp_538 = time_stamp,
      date_raw = games_sections %>% purrr::map(rvest::html_node, ".h3") %>% purrr::map_chr(rvest::html_text),
      date = date_raw %>% stringr::str_remove("^.+, ") %>% stringr::str_c(" 2021") %>% lubridate::mdy(),
      team = games_sections %>% purrr::map(rvest::html_nodes, ".tr.team") %>% purrr::map(rvest::html_attr, "data-team"),
      win_chance = games_sections %>% purrr::map(rvest::html_nodes, ".td.number.chance") %>% purrr::map(rvest::html_text) %>% purrr::map(readr::parse_number) %>% purrr::map(~ .x / 100)
    ) %>%
    dplyr::select(
      -date_raw
    ) %>%
    tidyr::unnest(cols = c(team, win_chance))

  res <- dat

  res

}

scrape_pinnacle <- function() {

  url <- "https://classic.sportsbookreview.com/betting-odds/nba-basketball/money-line/"

  req <- httr::GET(url)
  raw <- httr::content(req)

  all_nodes <- raw %>%
    rvest::html_nodes(".dateGroup") %>%
    purrr::map(
      ~.x %>%
        rvest::html_nodes(".eventLine.status-scheduled")
    )

  empty_nodes <- all_nodes %>%
    purrr::map_lgl(
      ~ length(.x) == 0
    )

  data_nodes <- all_nodes[!empty_nodes]

  dates <- data_nodes %>% purrr::map_chr(~ .x[1] %>% rvest::html_attr("rel") %>% stringr::str_sub(1, 10)) %>% as.Date()
  nodes <- data_nodes[dates == Sys.Date()][[1]]

  dat <-
    dplyr::tibble(
      date = nodes[1] %>% rvest::html_attr("rel") %>% stringr::str_sub(1, 10) %>% as.Date(),
      team = nodes %>% purrr::map(~ .x %>% rvest::html_nodes(".team-name a") %>% rvest::html_text()),
      odds_moneyline = nodes %>%
        purrr::map(~ .x %>% rvest::html_nodes(".eventLine-book")) %>%
        purrr::map(~ .x[2]) %>%
        purrr::map(~ .x %>% rvest::html_nodes(".eventLine-book-value") %>% rvest::html_text() %>% as.numeric()),

    ) %>%
    tidyr::unnest(cols = c(team, odds_moneyline)) %>%
    dplyr::mutate(
      time_stamp_pinnacle = as.character(Sys.time()),
      odds_dec = dplyr::case_when(
        odds_moneyline > 0 ~ (odds_moneyline + 100) / 100,
        TRUE ~ 1 + 100 / abs(odds_moneyline)
      ),
      team = dplyr::case_when(
        team %>% stringr::str_detect("Clippers") ~ "LAC",
        team %>% stringr::str_detect("Oklahoma") ~ "OKC",
        team %>% stringr::str_detect("Orleans") ~ "NOP",
        team %>% stringr::str_detect("Lakers") ~ "LAL",
        team %>% stringr::str_detect("Brooklyn") ~ "BKN",
        TRUE ~ stringr::str_sub(team, 1, 3) %>% stringr::str_to_upper()
      )
    ) %>%

    dplyr::select(
      -odds_moneyline
    )

  res <- dat

  res

}
