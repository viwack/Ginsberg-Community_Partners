# ── find_websites.R ──────────────────────────────────────────────────────
# Fills in missing website URLs in websites.csv by searching DuckDuckGo's
# public HTML results page for each organization name and taking the first
# result that isn't a social media / directory / review / nonprofit-listing
# site.
#
# READ THIS BEFORE RUNNING:
#   - No API key needed - this scrapes DuckDuckGo's HTML search page
#     (https://html.duckduckgo.com/html/), which doesn't require one.
#   - DuckDuckGo can rate-limit or temporarily block you if you hit it too
#     fast or too often. DELAY_SECS below is there on purpose - don't drop
#     it much below 2-3 seconds, especially over ~370 rows.
#   - Their page markup can change at any time, which would break the CSS
#     selectors below. If results suddenly all come back "not found", that
#     markup change is the first thing to check.
#   - This is a heuristic best guess, NOT a verified match. A national
#     chain, a same-named business in another state, or a well-SEO'd
#     unrelated page can outrank a small nonprofit's real site. Always
#     spot-check the output - don't treat it as final without a look.
#   - Only rows where Website is currently blank get touched. Anything
#     you've already filled in by hand is left alone.
#
# Packages needed: httr, rvest, dplyr, stringr, readr
#   install.packages(c("httr", "rvest", "dplyr", "stringr", "readr"))

library(httr)
library(rvest)
library(dplyr)
library(stringr)
library(readr)

# ── Config ──────────────────────────────────────────────────────────────
INPUT_FILE    <- "websites.csv"
OUTPUT_FILE   <- "websites_filled.csv"
DELAY_SECS    <- 3           # be polite - raise this if you start getting blocked
SEARCH_SUFFIX <- " Michigan" # appended to each query to help disambiguate;
# set to "" to search the bare org name

# Domains that are almost never an org's own homepage - skip these as hits
JUNK_DOMAINS <- c(
  "facebook.com", "instagram.com", "twitter.com", "x.com", "linkedin.com",
  "youtube.com", "yelp.com", "wikipedia.org", "guidestar.org", "give.org",
  "charitynavigator.org", "propublica.org", "causeiq.com", "bbb.org",
  "indeed.com", "ziprecruiter.com", "glassdoor.com", "mapquest.com",
  "google.com", "bing.com", "duckduckgo.com", "amazon.com",
  "greatnonprofits.org", "idealist.org", "volunteermatch.org", "tiktok.com",
  "pinterest.com", "crunchbase.com", "zoominfo.com", "manta.com",
  "yellowpages.com", "chamberofcommerce.com"
)

is_junk_domain <- function(url) {
  host <- tryCatch(httr::parse_url(url)$hostname, error = function(e) NA)
  if (is.na(host) || is.null(host)) return(TRUE)
  host <- str_remove(host, "^www\\.")
  any(str_detect(host, fixed(JUNK_DOMAINS)))
}

# ── Search one organization name, return best-guess URL or NA ────────────
find_website <- function(org_name) {
  query <- paste0(org_name, SEARCH_SUFFIX)
  
  res <- tryCatch(
    GET(
      "https://html.duckduckgo.com/html/",
      query      = list(q = query),
      user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"),
      timeout(10)
    ),
    error = function(e) NULL
  )
  
  if (is.null(res) || status_code(res) != 200) return(NA_character_)
  
  page  <- read_html(res)
  links <- page |> html_elements(".result__a") |> html_attr("href")
  
  if (length(links) == 0) {
    # fallback selector in case DDG's markup has shifted
    links <- page |> html_elements("a.result__url") |> html_attr("href")
  }
  
  links <- links[!is.na(links) & links != ""]
  if (length(links) == 0) return(NA_character_)
  
  for (link in links) {
    # DDG sometimes wraps the real destination in a redirect param
    real_url <- if (str_detect(link, "uddg=")) {
      URLdecode(str_extract(link, "(?<=uddg=)[^&]+"))
    } else {
      link
    }
    if (!is_junk_domain(real_url)) return(real_url)
  }
  NA_character_
}

# ── Run over the file ──────────────────────────────────────────────────────
websites <- read_csv(INPUT_FILE, show_col_types = FALSE)

if (!"Website" %in% names(websites)) {
  stop("Expected a 'Website' column in ", INPUT_FILE, " - check the header.")
}

n_to_do <- sum(is.na(websites$Website) | str_trim(coalesce(websites$Website, "")) == "")
cat(sprintf("Starting: %d of %d rows still need a website.\n\n", n_to_do, nrow(websites)))

for (i in seq_len(nrow(websites))) {
  # skip rows that already have a website (e.g. ones filled in by hand)
  if (!is.na(websites$Website[i]) && str_trim(websites$Website[i]) != "") next
  
  name <- websites$Account.Name[i]
  cat(sprintf("[%d/%d] %s ... ", i, nrow(websites), name))
  
  url <- tryCatch(find_website(name), error = function(e) NA_character_)
  websites$Website[i] <- url
  cat(if (is.na(url)) "not found\n" else paste0(url, "\n"))
  
  # Save progress after every row so a crash partway through doesn't lose work
  write_csv(websites, OUTPUT_FILE)
  
  Sys.sleep(DELAY_SECS)
}

cat("\nDone. Results written to", OUTPUT_FILE, "\n")
cat(sprintf("Filled: %d / %d\n", sum(!is.na(websites$Website) & str_trim(websites$Website) != ""), nrow(websites)))
