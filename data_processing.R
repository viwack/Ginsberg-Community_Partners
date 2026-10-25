# Data Processing/Manip


library(leaflet) #mapping package
library(shiny) #builds web application
library(bslib) #interactive widgets
library(readr) #helps read the files
library(stringr) #supports with cleaning the files
library(sf) #dataframe objects
library(dplyr) #data manip
library(ggplot2) #data viz
library(scales) #supports data viz
library(shinydashboard) #supports web UI
library(htmltools) #when building the website makes it easier
library(tigris) #contains geographical information
library(readxl)
library(tidyr)
library(tidygeocoder)

# ── SUPERSEDED ────────────────────────────────────────────────────────────
# This block built accounts_with_categories from "Mapping CP Network.xlsx"
# (old 8-group Category taxonomy, 372 orgs, no Total Matches/Created Date).
# A newer export (the "SF_Accounts_Review... NEW Category Framework" file)
# replaces it - see the new block below - so this is left here for
# reference only and isn't run.
#
# accounts_with_categories <- read_xlsx("Mapping CP Network.xlsx", sheet = "Accounts with categories")
# accounts_with_categories$`Billing Zip/Postal Code` <- as.integer(accounts_with_categories$`Billing Zip/Postal Code`)
#
# accounts_cat_select <- accounts_with_categories %>%
#   select(-(`Category: Record ID`),-(`Account Category: Account Categories Name`))
#
# acc_cat_wide <- accounts_cat_select %>%
#   filter(!is.na(`Category: Category Name`)) %>%
#   group_by(`Account Name`) %>%
#   mutate(cat_num = paste0("Category ", row_number())) %>%
#   ungroup() %>%
#   pivot_wider(
#     names_from  = cat_num,
#     values_from = `Category: Category Name`
#   )
#
# write.csv(acc_cat_wide, "Accounts with Categories Wide.csv")
#
# acc_cat_wide <- acc_cat_wide %>%
#   mutate(Full_Address = paste0(`Billing Address Line 1`, ", ",
#                               `Billing Address Line 2`, ", ",
#                               `Billing City`, ", ",
#                               `Billing State/Province`, ", ",
#                               `Billing Zip/Postal Code`))
#
# acc_cat_geo <- acc_cat_wide %>%
#   geocode(
#     address = Full_Address,
#     method = "arcgis",
#     lat = latitude,
#     long = longitude
#   )
#
# write.csv(acc_cat_geo, "Accounts_wideCategories_Geocoded.csv")


# ── Accounts: new taxonomy export ────────────────────────────────────────
# The new file has 404 orgs (vs. the old 372), uses a different Community
# Priority taxonomy shared with the new Matches export (`community
# priority 1`-`7` instead of `Category 1`-`9`), and already has the
# "Total # of Matches" / "Created Date" columns app.R was built to expect
# once they showed up. But it drops lat/long, Account ID, and the old
# Billing Address Line 1/2 + Zip breakdown entirely - so this reshapes it
# to the column names app.R already reads, carries over coordinates for
# every org that already existed in the old geocoded file (so we're not
# re-geocoding ~340 addresses that haven't changed), and geocodes only the
# orgs that are new.
new_accounts_raw <- read_csv("SF Accounts Review (NEW Category Framework) - Orgs Addresses and Categories - SF Accounts Review (NEW Category Framework) - Orgs Addresses and Categories.csv")

# "community priority N" -> "Category N" so app.R's existing
# cat_cols <- grep("^Category\\.[0-9]+$", ...) regex keeps working as-is -
# it doesn't care whether there are 7 of them instead of 9.
names(new_accounts_raw) <- names(new_accounts_raw) %>%
  str_replace("^community priority (\\d+)$", "Category \\1")

new_accounts <- new_accounts_raw %>%
  rename(
    `Billing Address Line 1` = `Billing Street`,
    # "Total # of Matches" -> "Total Matches" so it reads into R as
    # Total.Matches, matching the name app.R's placeholder code expects.
    `Total Matches`          = `Total # of Matches`
  ) %>%
  mutate(
    `Billing Address Line 2`  = NA_character_,
    # This export has no zip code column at all - left blank rather than
    # guessing. app.R's address display (city/state/zip line) will just
    # show a trailing blank for every org until a zip is added back in.
    `Billing Zip/Postal Code` = NA_character_,
    Full_Address = paste0(`Billing Address Line 1`, ", ", `Billing City`,
                          ", ", `Billing State/Province`)
  )

old_geo <- read_csv("Accounts_wideCategories_Geocoded.csv") %>%
  select(`Account Name`, latitude, longitude)

new_accounts <- new_accounts %>%
  left_join(old_geo, by = "Account Name")

# Geocode only the orgs that didn't already have coordinates (orgs new to
# this export, or ones the old file never resolved). Comment this part out
# after the first run, same as the old pipeline did - geocoding is slow
# and only needs to happen once per new address.
to_geocode <- new_accounts %>% filter(is.na(latitude))

# geocode() errors on a 0-row input (tidygeocoder's internal bind_cols()
# can't recycle an empty result against what it expects back from the
# API) - skip the call entirely when there's nothing new to geocode.
if (nrow(to_geocode) > 0) {
  geocoded <- to_geocode %>%
    select(-latitude, -longitude) %>%
    geocode(address = Full_Address, method = "arcgis", lat = latitude, long = longitude)
} else {
  geocoded <- to_geocode
}

new_accounts_geo <- new_accounts %>%
  filter(!is.na(latitude)) %>%
  bind_rows(geocoded)

message(sum(is.na(new_accounts_geo$latitude)), " of ", nrow(new_accounts_geo),
        " orgs still have no coordinates after geocoding - check their addresses by hand.")

write.csv(new_accounts_geo, "Accounts_wideCategories_Geocoded.csv", row.names = FALSE)


new_matches <- read_csv("Matches (for GC Map) - FY25-26.csv")
head(new_matches)

str(new_matches)

# ── Account Name resolution ──────────────────────────────────────────────
# The new matches export has no clean org-ID column - only combined
# "OrgAbbrev: Project Title" text in `Initiative: Initiative Name`, and the
# abbreviation before the colon often isn't the org's real Account Name
# (e.g. "AAPS" is really "Ann Arbor Public Schools"; "HV-WORC" is really
# "Huron Valley Worker Organizing and Research Center"). Splitting on the
# colon alone only resolves ~38 of 144 distinct orgs against the accounts
# file.
#
# The *old* tracking workbook's FY25/FY26 sheets already solved this once:
# they carry a separate `Initiative Account` column with the real name,
# keyed by the same `Match: Match Name` ID the new export still uses. So
# borrow that first, keyed by match ID (not by abbreviation - the same
# abbreviation can map to more than one real org, e.g. "AAPS" alone covers
# both "Ann Arbor Public Schools" and "AAPS: Community Division", and only
# a per-match join tells them apart correctly). Only fall back to the
# colon-split abbreviation for matches that are too new to be in that
# workbook at all.
read_old_match_ids <- function(sheet) {
  read_xlsx("Mapping CP Network.xlsx", sheet = sheet) %>%
    select(`Match: Match Name`, `Initiative Account`)
}

old_org_lookup <- bind_rows(
  read_old_match_ids("FY25"),
  read_old_match_ids("FY26")
) %>%
  distinct(`Match: Match Name`, .keep_all = TRUE)

# A few matches aren't in the old workbook at all, so there's no
# Initiative Account to borrow for them. Add to this table by hand as new
# unresolved abbreviations turn up - this is the only place that still
# needs a manual lookup once this script is wired up.
org_overrides <- c(
  "HRWC" = "Huron River Watershed Council",
  "RGI"  = "Refugee Garden Initiatives"
)

matches_final <- new_matches %>%
  left_join(old_org_lookup, by = "Match: Match Name") %>%
  mutate(
    .abbrev        = trimws(sub(":.*$", "", `Initiative: Initiative Name`)),
    `Account Name` = coalesce(`Initiative Account`, unname(org_overrides[.abbrev]), .abbrev),
    .after         = `Match: Match Name`
  ) %>%
  select(-`Initiative Account`, -.abbrev)

# Some resolved names aren't missing an account at all - they're just a
# differently-worded variant of a real one, carried over from how the old
# workbook spelled it out. "Detroit Public School Community District-
# Partnerships Department" (singular "School", extra department suffix)
# is really "Detroit Public Schools Community District" by exact name.
# Add more exact-string fixes here as they turn up - this is for
# spelling/wording mismatches specifically, not missing sub-accounts
# (that's what district_fallback below is for).
name_corrections <- c(
  "Detroit Public School Community District- Partnerships Department" = "Detroit Public Schools Community District"
)

matches_final <- matches_final %>%
  mutate(`Account Name` = coalesce(unname(name_corrections[`Account Name`]), `Account Name`))

# District schools get their own pin when a school-specific account
# exists (e.g. "AAPS: Pattengill Elementary" is a real account, so matches
# there stay there - no rolling up to the district). But a few schools
# showing up in Matches (AAPS's Thurston, Wines; YCS's Perry) don't have
# their own account at all - not even as a Parent Account child - so
# route only those to the district's general account instead of leaving
# them unresolved. Add another prefix/district pair here if a third
# district turns up the same way.
district_fallback <- c(
  "AAPS" = "Ann Arbor Public Schools",
  "YCS"  = "Ypsilanti Community Schools"
)

matches_final <- matches_final %>%
  mutate(
    .district_prefix = trimws(sub(":.*$", "", `Account Name`)),
    `Account Name` = if_else(
      .district_prefix %in% names(district_fallback) & !(`Account Name` %in% new_accounts_geo$`Account Name`),
      unname(district_fallback[.district_prefix]),
      `Account Name`
    )
  ) %>%
  select(-.district_prefix)

# Sanity check before writing out - how many distinct orgs still don't
# resolve to a real account name, against the *new* accounts file (it
# picks up several orgs the old one was missing, e.g. "Math Corps" and
# "Lori's Hands" - but not all of them; a handful are missing from both).
unresolved <- setdiff(unique(matches_final$`Account Name`), new_accounts_geo$`Account Name`)
message(length(unresolved), " org name(s) still don't match an account: ",
        paste(unresolved, collapse = ", "))

write.csv(matches_final, "Matches_Final.csv", row.names = FALSE)