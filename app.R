library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(readxl)
library(tidyr)
library(htmltools)
library(DT)
library(plotly)

# ── Data loading ──────────────────────────────────────────────────────────────

orgs     <- read.csv("Accounts_wideCategories_Geocoded.csv", stringsAsFactors = FALSE)
websites <- read.csv("websites.csv", stringsAsFactors = FALSE)
orgs     <- orgs |> left_join(websites, by = "Account.Name")

# ── Manual geocode corrections ─────────────────────────────────────────────
# These two rows had missing/malformed source addresses that the geocoder
# mapped to bogus fallback points (Myanmar / Italy). Patch in verified
# addresses + coordinates until the source data itself is fixed upstream.
fix_org <- function(data, name, addr1, city, state, zip, lat, lon) {
  idx <- which(data$Account.Name == name)
  if (length(idx) == 0) return(data)
  data$Billing.Address.Line.1[idx]  <- addr1
  data$Billing.City[idx]            <- city
  data$Billing.State.Province[idx]  <- state
  data$Billing.Zip.Postal.Code[idx] <- zip
  data$latitude[idx]                <- lat
  data$longitude[idx]               <- lon
  data
}

orgs <- orgs |>
  fix_org("Detroit People's Platform",
          addr1 = "7700 Second Ave. #509", city = "Detroit", state = "MI", zip = "48202",
          lat = 42.3760, lon = -83.0780) |>
  fix_org("Next Chapter Bookclub of Saline",
          addr1 = "6711 Robison Ln", city = "Saline", state = "MI", zip = "48176",
          lat = 42.1934, lon = -83.7075)

# Remaining accounts that still have no usable address on file. Their lat/lon
# in the source CSV are geocoder fallback junk (not real locations), so they
# are pulled off the map and listed separately instead of being plotted.
no_geo_names <- c(
  "Middle Ground", "Resource Generation", "Justice InDeed",
  "Positive Impact for Life", "The Equitable Ann Arbor Land Trust",
  "The McKinney Foundation", "Our Village", "Jotno Foundation",
  "National Wild Turkey Federation - Michigan State Chapter",
  "Refugee Garden Initiatives", "Detroit Brownie", "Breyko",
  "Detroit Parent Network", "Ele's Place", "The Delian Club",
  "Reproductive Freedom for All - Michigan", "Concert Music Outreach Collective",
  "Trinity Health Community Health and Wellbeing", "Superhero Training Academy",
  "Voce Velata", "Greater Health Institute", "Laotian American Community of Michigan",
  "FutureRoot", "Clubhouse Michigan", "The SunBundle Nonprofit"
)

# ── Upcoming fields (not in the data yet) ──────────────────────────────────
# Two fields are coming later: a whole-number "total matches" count per org,
# and a Salesforce "Created Date" we'll use to show partnership length. They
# aren't in the CSV yet, so these placeholder columns keep the rest of the
# app (popups + detail panel below) working today and requiring NO further
# code changes once the real data shows up - as long as the incoming CSV
# columns end up named "Total Matches" and "Created Date" (R will read those
# in as Total.Matches / Created.Date). If they come in under different
# names, just update the two column names below to match.
if (!"Total.Matches" %in% names(orgs)) {
  orgs$Total.Matches <- NA_integer_
}
if (!"Created.Date" %in% names(orgs)) {
  orgs$Created.Date <- as.Date(NA)
} else {
  # Adjust the format string once we see what Salesforce actually exports
  # (commonly "%Y-%m-%d" or "%m/%d/%Y").
  orgs$Created.Date <- as.Date(orgs$Created.Date, format = "%Y-%m-%d")
}
# School/College/Unit is also coming later (used as a filter only - it
# doesn't need to display anywhere, per request). Expected column name is
# "School College Unit" -> read in as School.College.Unit.
if (!"School.College.Unit" %in% names(orgs)) {
  orgs$School.College.Unit <- NA_character_
}

# Turns a Created Date into e.g. "March 2019 — 6 years". Returns NA (which
# the display code below turns into a "coming soon" placeholder) until real
# dates exist.
format_partner_since <- function(created_date) {
  if (is.null(created_date) || length(created_date) == 0 || is.na(created_date)) {
    return(NA_character_)
  }
  yrs <- floor(as.numeric(difftime(Sys.Date(), created_date, units = "days")) / 365.25)
  paste0(format(created_date, "%B %Y"), " \u2014 ", yrs, if (yrs == 1) " year" else " years")
}

cat_cols <- grep("^Category\\.", names(orgs), value = TRUE)
orgs <- orgs |>
  mutate(
    Categories = apply(orgs[, cat_cols], 1, function(x) {
      vals <- x[!is.na(x) & x != ""]
      if (length(vals) == 0) NA_character_ else paste(vals, collapse = ", ")
    })
  )

# ── Focus area groups ─────────────────────────────────────────────────────────

group_map <- list(
  "Education & Youth Development"               = c("Education/Schooling","Early Childhood","Middle Childhood",
                                                    "Adolescence","Literacy","Tutoring","Mentoring",
                                                    "Adult Education","Postsecondary Education",
                                                    "STEM Education and Outreach","Special Education"),
  "Health & Wellbeing"                          = c("Health & Wellbeing","Mental Health & Substance Abuse",
                                                    "Nutrition Accessibility","Disability Safety and Rights","Families"),
  "Housing, Economic Opportunity & Basic Needs" = c("Poverty & Economic Opportunity","Housing Access and Affordability",
                                                    "Food Justice","Community Gardens and Farming"),
  "Community Engagement & Civic Life"           = c("Community Organizing & Advocacy","Civic Engagement",
                                                    "Community Building","Infrastructure"),
  "Equity, Justice & Inclusion"                 = c("Social Justice & Equity","Racial Justice",
                                                    "LGBTQIA2S+ Safety and Rights","Women and Girls",
                                                    "Refugee and Immigrant Support","Specific Cultural Focus",
                                                    "Religious Affiliation","Criminal Legal System",
                                                    "Military Veterans and Service Members"),
  "Environment & Sustainability"                = c("Environment & Sustainability"),
  "Arts, Culture & Creative Expression"         = c("Arts"),
  "Other / Specialized Areas"                   = c("Not Otherwise Classified")
)

cat_to_group <- setNames(
  rep(names(group_map), lengths(group_map)),
  unlist(group_map, use.names = FALSE)
)

# ── Skill area groups ──────────────────────────────────────────────────────
# Raw values come from the matches data's Match_Category__r.Name field
# (loaded below as projects$Category). Grouped into buckets the same way
# Community Priority groups the raw account-level categories above.

skill_group_map <- list(
  "Communications, Marketing & Design"             = c("Art and Design", "Communication", "Marketing", "Social Media"),
  "Data, Assessment & Evaluation"                  = c("Assessment/Evaluation", "Data Science"),
  "Technology & Engineering"                       = c("Computer Science & Information Technology", "Engineering"),
  "Organizational Development & Capacity Building" = c("Human Resources & Organizational Development", "Philanthropy/Development"),
  "Program Development, Policy & Strategy"         = c("Program Development", "Policy", "UM Expertise"),
  "Education & Community Learning"                 = c("Tutoring", "Guest Speaking"),
  "Community Engagement & Leadership"               = c("Volunteer Recruitment Opportunity", "UM Board Participation"),
  "Other / Specialized Skills"                     = c("Not Otherwise Classified")
)

skill_to_group <- setNames(
  rep(names(skill_group_map), lengths(skill_group_map)),
  unlist(skill_group_map, use.names = FALSE)
)

# ── Projects ──────────────────────────────────────────────────────────────────

read_fy <- function(sheet) {
  read_xlsx("Mapping CP Network.xlsx", sheet = sheet) |>
    select(
      Org       = `Initiative Account`,
      Project   = `Initiative`,
      Offering  = `Resource Offering`,
      Completed = `Match Completed Date`,
      Category  = `Match_Category__r.Name`
    ) |>
    mutate(
      Completed = as.Date(as.numeric(Completed), origin = "1899-12-30"),
      FY        = sheet
    )
}

projects <- bind_rows(read_fy("FY25"), read_fy("FY26"))

# ── Map data ──────────────────────────────────────────────────────────────────

orgs <- orgs |>
  mutate(
    primary_category = apply(pick(all_of(cat_cols)), 1, function(x) {
      vals <- x[!is.na(x) & x != ""]
      if (length(vals) == 0) NA_character_ else vals[[1]]
    }),
    primary_group = cat_to_group[primary_category]
  )

map_orgs <- orgs |>
  filter(!is.na(latitude), !is.na(longitude), !(Account.Name %in% no_geo_names))

# Accounts with no usable geographic data - shown in their own tab instead
problem_orgs <- orgs |>
  filter(Account.Name %in% no_geo_names) |>
  arrange(Account.Name)

# ── Filter choices ──────────────────────────────────────────────────────────

# "Last 2 fiscal years" - computed from whatever FY values exist in the
# matches data, so this stays correct as new FY sheets get added later
# (e.g. once FY27 shows up, this becomes FY26/FY27 automatically).
recent_fys <- tail(sort(unique(projects$FY)), 2)

# Form of Engagement / Skill Area come from the matches data (Resource
# Offering -> Offering, Match_Category__r.Name -> Category), not the
# accounts CSV, since they're properties of a match rather than the org.
engagement_choices <- sort(unique(projects$Offering[!is.na(projects$Offering) & projects$Offering != ""]))
skill_choices       <- names(skill_group_map)

# School / College / Unit isn't in the data yet (expected later). Choices
# stay empty until the real column has real values; once it does, this
# filter activates on its own with no code changes.
scu_choices <- sort(unique(orgs$School.College.Unit[!is.na(orgs$School.College.Unit) & orgs$School.College.Unit != ""]))

# ── Color palette ─────────────────────────────────────────────────────────────

group_colors <- c(
  "Education & Youth Development"               = "#4e79a7",
  "Health & Wellbeing"                          = "#59a14f",
  "Housing, Economic Opportunity & Basic Needs" = "#f28e2b",
  "Community Engagement & Civic Life"           = "#76b7b2",
  "Equity, Justice & Inclusion"                 = "#b07aa1",
  "Environment & Sustainability"                = "#499894",
  "Arts, Culture & Creative Expression"         = "#e15759",
  "Other / Specialized Areas"                   = "#9c755f"
)

group_pal <- colorFactor(
  palette  = group_colors,
  domain   = names(group_map),
  na.color = "#aaaaaa"
)

# Single uniform color for map markers (no longer tied to focus-area group)
marker_color <- "#00274C"

# ── Infographics data ───────────────────────────────────────────────────────
# Everything here is computed once from data we already have (no "coming
# soon" placeholders needed) - the accounts table + the FY25/FY26 matches
# data loaded above.

# -- Geographic classification -----------------------------------------
# No explicit country column in the source data, so this is inferred:
#   - "Unknown"            : accounts with no usable address at all (see
#                            no_geo_names above)
#   - "Southeast Michigan" : Billing State = MI AND falls inside a rough
#                            SE Michigan bounding box (Wayne/Oakland/Macomb/
#                            Washtenaw/Livingston/Monroe/St. Clair area).
#                            This is a bounding-box approximation, not real
#                            county boundaries - good enough for an at-a-
#                            glance stat, but worth swapping for a real
#                            county lookup if precision matters later.
#   - "Rest of Michigan"   : Billing State = MI, outside that box
#   - "USA (Other States)" : Billing State is a US state/territory, not MI
#   - "International"      : anything else (foreign address, or a Billing
#                            State that isn't a recognized US abbreviation)
us_state_abbrevs <- c(state.abb, "DC")
se_mi_box <- list(lat_min = 41.7, lat_max = 43.1, lon_min = -84.3, lon_max = -82.3)

geo_bucket_vec <- local({
  state <- orgs$Billing.State.Province
  lat   <- orgs$latitude
  lon   <- orgs$longitude
  
  in_se_mi <- !is.na(lat) & !is.na(lon) &
    lat >= se_mi_box$lat_min & lat <= se_mi_box$lat_max &
    lon >= se_mi_box$lon_min & lon <= se_mi_box$lon_max
  
  bucket <- case_when(
    orgs$Account.Name %in% no_geo_names        ~ "Unknown",
    !is.na(state) & state == "MI" & in_se_mi    ~ "Southeast Michigan",
    !is.na(state) & state == "MI"                ~ "Rest of Michigan",
    !is.na(state) & state %in% us_state_abbrevs ~ "USA (Other States)",
    TRUE                                          ~ "International"
  )
  bucket
})
orgs$Geo.Bucket <- geo_bucket_vec

geo_counts <- orgs |>
  count(Geo.Bucket, name = "n") |>
  arrange(match(Geo.Bucket, c("Southeast Michigan", "Rest of Michigan",
                              "USA (Other States)", "International", "Unknown")))

n_total        <- nrow(orgs)
n_se_mi        <- sum(orgs$Geo.Bucket == "Southeast Michigan")
n_mi_total     <- sum(orgs$Geo.Bucket %in% c("Southeast Michigan", "Rest of Michigan"))
n_usa_other    <- sum(orgs$Geo.Bucket == "USA (Other States)")
n_international <- sum(orgs$Geo.Bucket == "International")

# -- Top community priority areas (by number of distinct orgs involved) --
# An org can carry multiple categories; each org is counted once per
# priority group it touches (not once per raw category), so an org tagged
# with both "Literacy" and "Mentoring" only counts once toward "Education &
# Youth Development".
org_priority_groups <- apply(orgs[, cat_cols], 1, function(x) {
  vals   <- x[!is.na(x) & x != ""]
  groups <- unique(cat_to_group[vals])
  groups[!is.na(groups)]
})
priority_counts <- sort(table(unlist(org_priority_groups)), decreasing = TRUE)
top3_priorities <- head(priority_counts, 3)

# -- Matches leaderboard (all-time, from the FY25+FY26 matches data) -----
matches_leaderboard <- projects |>
  count(Org, name = "Matches") |>
  arrange(desc(Matches)) |>
  slice_head(n = 10)

# -- Last-2-FY summary -----------------------------------------------------
recent_matches   <- projects |> filter(FY %in% recent_fys)
n_recent_matches <- nrow(recent_matches)

recent_by_fy <- recent_matches |> count(FY, name = "n")

recent_matches_grouped <- recent_matches |>
  left_join(orgs |> select(Account.Name, primary_group), by = c("Org" = "Account.Name")) |>
  mutate(skill_group = skill_to_group[Category])

top_priorities_recent  <- recent_matches_grouped |>
  filter(!is.na(primary_group)) |> count(primary_group, name = "n") |> arrange(desc(n))
top_skills_recent       <- recent_matches_grouped |>
  filter(!is.na(skill_group)) |> count(skill_group, name = "n") |> arrange(desc(n))
top_engagement_recent   <- recent_matches_grouped |>
  filter(!is.na(Offering) & Offering != "") |> count(Offering, name = "n") |> arrange(desc(n))

# A distinct accent color per geographic bucket, reusing the style guide's
# secondary palette so this ties visually to the rest of the U-M brand.
geo_colors <- c(
  "Southeast Michigan"  = "#00274C",  # Blue
  "Rest of Michigan"    = "#407EC9",  # Arboretum Blue
  "USA (Other States)"  = "#D86018",  # Ross School Orange
  "International"       = "#702082",  # Ann Arbor Amethyst
  "Unknown"              = "#bbbbbb"
)

geo_counts <- geo_counts |>
  mutate(color = unname(geo_colors[Geo.Bucket]))

top3_priorities_df <- data.frame(
  Priority = names(top3_priorities),
  n        = as.integer(top3_priorities),
  stringsAsFactors = FALSE
)

# General-purpose qualitative palette (brand + style-guide secondary colors)
# for charts with categories that don't already have a fixed named palette
# (Form of Engagement values are whatever's in the spreadsheet, so they
# can't be pre-assigned specific colors the way Community Priority can).
brand_qualitative <- c("#00274C", "#FFCB05", "#9A3324", "#00B2A9", "#D86018",
                       "#702082", "#407EC9", "#59a14f", "#e15759", "#9c755f")

bar_colors_for <- function(n) {
  rep(brand_qualitative, length.out = n)
}

# ── Chart helpers ────────────────────────────────────────────────────────
# `colors` can be a single hex string (uniform bars) or a named vector keyed
# by the values in cat_col (e.g. group_colors) for a fixed per-category
# palette that stays consistent with the map/legend elsewhere in the app.
make_horiz_bar <- function(df, cat_col, val_col, colors = "#00274C", unit_label = "match") {
  df <- df[order(df[[val_col]]), , drop = FALSE]  # ascending so the biggest bar plots on top
  cats <- df[[cat_col]]
  vals <- df[[val_col]]
  
  bar_colors <- if (length(colors) > 1) {
    if (!is.null(names(colors))) unname(colors[cats]) else rep(colors, length.out = length(cats))
  } else {
    colors
  }
  bar_colors[is.na(bar_colors)] <- "#cccccc"
  
  plot_ly(
    x = vals,
    y = factor(cats, levels = cats),
    type = "bar",
    orientation = "h",
    marker = list(color = bar_colors),
    hovertemplate = paste0("%{y}<br>%{x} ", unit_label, "s<extra></extra>")
  ) |>
    layout(
      xaxis  = list(title = "", zeroline = FALSE, showgrid = TRUE, gridcolor = "#f0f2f5"),
      yaxis  = list(title = "", automargin = TRUE),
      margin = list(l = 10, r = 16, t = 10, b = 10),
      font   = list(family = "Roboto Condensed, sans-serif", size = 12, color = "#333")
    ) |>
    config(displayModeBar = FALSE)
}

make_donut <- function(df, label_col, val_col, colors) {
  labels <- df[[label_col]]
  plot_ly(
    labels = labels, values = df[[val_col]],
    type = "pie", hole = 0.58,
    marker = list(colors = unname(colors[labels]), line = list(color = "#ffffff", width = 2)),
    textinfo = "label+value",
    textposition = "outside",
    hovertemplate = "%{label}: %{value} orgs<extra></extra>"
  ) |>
    layout(
      showlegend = FALSE,
      margin = list(l = 10, r = 10, t = 10, b = 10),
      font   = list(family = "Roboto Condensed, sans-serif", size = 12, color = "#333")
    ) |>
    config(displayModeBar = FALSE)
}

stat_tile <- function(number, label, color = "#00274C") {
  tags$div(
    class = "stat-tile",
    tags$div(class = "stat-number", style = paste0("color:", color, ";"), number),
    tags$div(class = "stat-label", label)
  )
}

# Simple two-column Field / Description table for the Data Dictionary tab.
# `rows` is a list of c(field, description) pairs.
field_table <- function(rows) {
  th_style <- "text-align:left;padding:6px 10px;border-bottom:2px solid #e8edf2;color:#00274C;font-size:0.68rem;text-transform:uppercase;letter-spacing:0.05em;"
  td1_style <- "padding:7px 10px;border-bottom:1px solid #f0f2f5;font-weight:600;color:#333;white-space:nowrap;vertical-align:top;"
  td2_style <- "padding:7px 10px;border-bottom:1px solid #f0f2f5;color:#555;line-height:1.5;"
  tags$table(
    style = "width:100%; border-collapse:collapse; font-size:0.82rem;",
    tags$thead(
      tags$tr(tags$th(style = th_style, "Field"), tags$th(style = th_style, "Description"))
    ),
    tags$tbody(
      lapply(rows, function(r) {
        tags$tr(tags$td(style = td1_style, r[[1]]), tags$td(style = td2_style, r[[2]]))
      })
    )
  )
}

# ── Tooltip / popup builders ──────────────────────────────────────────────────

make_label <- function(data) {
  lapply(seq_len(nrow(data)), function(i) {
    o       <- data[i, ]
    grp     <- o$primary_group
    grp_col <- if (!is.na(grp)) group_pal(grp) else "#888888"
    
    status_line <- if (!is.na(o$Ginsberg.Partner.Status) && o$Ginsberg.Partner.Status != "")
      paste0("<div style='font-size:0.75rem;color:#666;margin-top:2px;'>",
             o$Ginsberg.Partner.Status, " Partner</div>")
    else ""
    
    group_line <- if (!is.na(grp))
      paste0("<div style='margin-top:5px;'><span style='background:", grp_col,
             ";color:#fff;padding:2px 9px;border-radius:20px;font-size:0.69rem;font-weight:600;'>",
             grp, "</span></div>")
    else ""
    
    city_line <- if (!is.na(o$Billing.City) && o$Billing.City != "")
      paste0("<div style='font-size:0.73rem;color:#888;margin-top:4px;'>",
             o$Billing.City, ", ", o$Billing.State.Province, "</div>")
    else ""
    
    HTML(paste0(
      "<div style='font-family:system-ui,-apple-system,sans-serif;padding:5px 7px;min-width:170px;'>",
      "<strong style='font-size:0.88rem;color:#1a1a1a;'>", htmlEscape(o$Account.Name), "</strong>",
      status_line, group_line, city_line,
      "</div>"
    ))
  })
}

make_popup <- function(o) {
  grp     <- o$primary_group
  grp_col <- if (!is.na(grp)) group_pal(grp) else "#888888"
  
  status_badge <- if (!is.na(o$Ginsberg.Partner.Status) && o$Ginsberg.Partner.Status != "") {
    bg  <- if (o$Ginsberg.Partner.Status == "Active") "#198754" else "#e6a817"
    txt <- if (o$Ginsberg.Partner.Status == "Active") "#fff" else "#1a1a1a"
    paste0("<span style='background:", bg, ";color:", txt,
           ";padding:3px 10px;border-radius:20px;font-size:0.71rem;font-weight:700;margin-right:5px;'>",
           o$Ginsberg.Partner.Status, "</span>")
  } else ""
  
  group_badge <- if (!is.na(grp))
    paste0("<span style='background:", grp_col,
           ";color:#fff;padding:3px 10px;border-radius:20px;font-size:0.71rem;font-weight:700;'>",
           grp, "</span>")
  else ""
  
  addr_line <- if (!is.na(o$Billing.Address.Line.1) && o$Billing.Address.Line.1 != "")
    paste0("<div style='font-size:0.8rem;color:#555;margin-top:8px;line-height:1.45;'>",
           htmlEscape(o$Billing.Address.Line.1), "<br>",
           htmlEscape(o$Billing.City), ", ", o$Billing.State.Province,
           " ", o$Billing.Zip.Postal.Code, "</div>")
  else ""
  
  website_line <- if (!is.na(o$Website) && o$Website != "")
    paste0("<div style='margin-top:7px;'><a href='", o$Website,
           "' target='_blank' rel='noopener' style='font-size:0.79rem;color:#4e79a7;text-decoration:none;'>",
           "\U0001F517 ", htmlEscape(o$Website), "</a></div>")
  else ""
  
  # ── Stats: total matches + partnership length ────────────────────────
  # Placeholder styling until "Total Matches" and "Created Date" arrive.
  matches_text <- if (!is.na(o$Total.Matches))
    paste0("<strong style='color:#1a1a1a;'>", o$Total.Matches, "</strong> match",
           if (o$Total.Matches != 1) "es" else "")
  else
    "<span style='color:#bbb;font-style:italic;'>Match count coming soon</span>"
  
  since_text <- if (!is.na(o$Created.Date))
    paste0("<strong style='color:#1a1a1a;'>", format_partner_since(o$Created.Date), "</strong>")
  else
    "<span style='color:#bbb;font-style:italic;'>Partnership length coming soon</span>"
  
  stats_line <- paste0(
    "<div style='font-size:0.78rem;color:#555;margin-top:8px;line-height:1.6;'>",
    matches_text, " &nbsp;\u2022&nbsp; ", since_text,
    "</div>"
  )
  
  HTML(paste0(
    "<div style='font-family:system-ui,-apple-system,sans-serif;min-width:250px;max-width:320px;padding:6px 2px;'>",
    "<div style='font-size:1rem;font-weight:700;color:#00274C;line-height:1.25;margin-bottom:9px;'>",
    htmlEscape(o$Account.Name), "</div>",
    "<div style='margin-bottom:4px;'>", status_badge, group_badge, "</div>",
    addr_line, website_line, stats_line,
    "<div style='font-size:0.69rem;color:#bbb;margin-top:10px;border-top:1px solid #f0f0f0;padding-top:7px;'>",
    "See sidebar for full details &amp; matches &rarr;</div>",
    "</div>"
  ))
}

# ── Theme ─────────────────────────────────────────────────────────────────────

app_theme <- bs_theme(
  version      = 5,
  primary      = "#00274C",
  secondary    = "#FFCB05",
  success      = "#59a14f",
  info         = "#4e79a7",
  bg           = "#f0f2f5",
  fg           = "#1a1a1a",
  "navbar-bg"               = "#00274C",
  "navbar-dark-color"       = "rgba(255,255,255,0.85)",
  "navbar-dark-hover-color" = "#FFCB05",
  "navbar-dark-active-color"= "#FFCB05",
  "navbar-dark-brand-color" = "#ffffff",
  "font-size-base"          = "0.9rem",
  "border-radius"           = "0.5rem",
  "border-radius-sm"        = "0.3rem",
  "card-border-width"       = "0",
  "card-box-shadow"         = "0 2px 14px rgba(0,0,0,0.07)",
  base_font    = font_google("Roboto Condensed"),
  heading_font = font_google("Fjalla One")
)

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  title    = tags$span(
    style = "display:flex; align-items:center; gap:16px;",
    tags$span(
      style = "font-weight:700; letter-spacing:-0.2px; color:rgba(255,255,255,0.85);",
      "Community Partners"
    )
  ),
  theme    = app_theme,
  fillable = c("Map", "Accounts with No Geographic Data"),
  
  # ── Head: global CSS + listbox JS ─────────────────────────────────────────
  tags$head(
    tags$style(HTML("

      /* ── Global text size ─────────────────────────────────────────────
         Almost every font-size in this app is set in rem, which is always
         relative to this root <html> size. Bumping it here scales all of
         them (sidebar labels, badges, popups, match cards, tables, etc.)
         proportionally in one place, instead of hunting down every
         hardcoded rem value individually. */
      html { font-size: 17px; }

      /* ── Navbar ────────────────────────────────────────────────────── */
      .navbar { border-bottom: 3px solid #FFCB05; box-shadow: 0 2px 10px rgba(0,0,0,0.18); }
      .navbar-nav .nav-link { font-weight: 500; font-size: 0.87rem; padding: 0.5rem 1rem; }

      /* ── Cards ──────────────────────────────────────────────────────── */
      .card { border: none !important; box-shadow: 0 2px 14px rgba(0,0,0,0.07); }

      /* ── Sidebar ────────────────────────────────────────────────────── */
      .bslib-sidebar-layout > .sidebar { background: #ffffff !important; border-right: 1px solid #e4e8ee; }
      .sidebar-section-label {
        font-size: 0.67rem; font-weight: 700; text-transform: uppercase;
        letter-spacing: 0.09em; color: #00274C; margin: 0 0 6px 0;
      }

      /* ── Community Priority / Engagement / Skill / SCU listboxes ──────── */
      #priority_filter, #engagement_filter, #skill_filter, #scu_filter {
        border-radius: 8px !important; border: 1px solid #dde2e8 !important;
        font-size: 0.82rem; overflow: hidden;
      }
      #priority_filter option, #engagement_filter option,
      #skill_filter option, #scu_filter option { padding: 7px 10px; line-height: 1.5; }
      #priority_filter option:checked, #engagement_filter option:checked,
      #skill_filter option:checked, #scu_filter option:checked { filter: brightness(0.78) !important; }

      /* ── Radio buttons ──────────────────────────────────────────────── */
      .form-check-input:checked { background-color: #00274C !important; border-color: #00274C !important; }

      /* ── Map popup ──────────────────────────────────────────────────── */
      .leaflet-popup-content-wrapper {
        border-radius: 12px !important;
        box-shadow: 0 6px 28px rgba(0,0,0,0.15) !important;
        padding: 0 !important;
        overflow: hidden;
      }
      .leaflet-popup-content { margin: 15px 18px !important; }
      .leaflet-popup-tip-container { display: none !important; }
      .leaflet-popup-close-button {
        top: 8px !important; right: 10px !important;
        font-size: 1.1rem !important; color: #999 !important;
      }

      /* ── Map legend ─────────────────────────────────────────────────── */
      .info.legend {
        background: rgba(255,255,255,0.95) !important;
        border-radius: 10px !important;
        box-shadow: 0 2px 12px rgba(0,0,0,0.12) !important;
        font-size: 0.75rem !important;
        padding: 10px 14px !important;
        line-height: 1.6 !important;
      }

      /* ── About box ──────────────────────────────────────────────────── */
      .about-box {
        background: #f0f4f8;
        border-radius: 8px;
        padding: 13px 15px;
        border-left: 4px solid #00274C;
        margin-bottom: 12px;
      }
      .about-title {
        font-weight: 700; font-size: 0.82rem; color: #00274C; margin-bottom: 9px;
      }
      .about-box p {
        font-size: 0.79rem; color: #444; line-height: 1.55; margin-bottom: 7px;
      }
      .about-box p:last-child { margin-bottom: 0; }

      /* ── Instruction box ────────────────────────────────────────────── */
      .instruction-box {
        background: #e8f0fe; border-radius: 8px;
        padding: 10px 13px; margin-top: 4px;
        font-size: 0.79rem; color: #1a3a6b; line-height: 1.45;
      }

      /* ── Org panel ──────────────────────────────────────────────────── */
      .org-name {
        font-size: 1rem; font-weight: 700; color: #00274C;
        line-height: 1.3; margin-bottom: 3px;
      }
      .org-website {
        font-size: 0.77rem; color: #4e79a7 !important;
        text-decoration: none !important; display: block; margin-bottom: 5px;
      }
      .org-website:hover { text-decoration: underline !important; }

      /* ── Match cards ────────────────────────────────────────────────── */
      .match-card {
        background: #f7f9fc; border-radius: 8px;
        padding: 11px 13px; margin-bottom: 9px;
        border-left: 4px solid #dee2e6;
      }
      .match-card.fy25 { border-left-color: #4e79a7; }
      .match-card.fy26 { border-left-color: #b8940a; }

      /* ── Map markers: make them read as clickable buttons ────────────── */
      .leaflet-interactive {
        cursor: pointer !important;
        transition: filter 0.12s ease-out, transform 0.12s ease-out;
        transform-box: fill-box;
        transform-origin: center;
      }
      .leaflet-interactive:hover {
        filter: brightness(1.12) drop-shadow(0 2px 5px rgba(0,0,0,0.45));
        transform: scale(1.15);
      }
      .leaflet-interactive:active {
        transform: scale(0.95);
      }
      .leaflet-marker-cluster { cursor: pointer !important; }

      /* ── Close button under the org detail panel ──────────────────────── */
      .close-panel-btn {
        display: inline-flex;
        align-items: center;
        gap: 5px;
        font-size: 0.79rem;
        font-weight: 600;
        color: #666 !important;
        text-decoration: none !important;
        padding: 5px 12px;
        border-radius: 20px;
        border: 1px solid #dde2e8;
        transition: background 0.12s ease-out, color 0.12s ease-out;
      }
      .close-panel-btn:hover {
        background: #f0f4f8;
        color: #00274C !important;
      }

      /* ── Nav tabs in sidebar ────────────────────────────────────────── */
      .nav-tabs { border-bottom: 2px solid #e8edf2 !important; }
      .nav-tabs .nav-link {
        font-size: 0.81rem; font-weight: 500; color: #666;
        border: none !important; padding: 6px 12px;
      }
      .nav-tabs .nav-link.active {
        font-weight: 700; color: #00274C !important;
        border-bottom: 2px solid #00274C !important;
        background: transparent !important;
      }
      .nav-tabs .nav-link:hover { color: #00274C; background: transparent; }

      /* ── Infographics ──────────────────────────────────────────────── */
      .infographic-section-title {
        font-size: 0.95rem; font-weight: 700; color: #00274C;
        margin-bottom: 14px; padding-bottom: 8px;
        border-bottom: 2px solid #FFCB05;
      }
      .stat-tile-row {
        display: flex; gap: 12px; flex-wrap: wrap; margin-bottom: 22px;
      }
      .stat-tile {
        flex: 1 1 140px;
        background: #f7f9fc;
        border-radius: 10px;
        padding: 14px 12px;
        text-align: center;
        border: 1px solid #eef1f5;
      }
      .stat-number { font-size: 1.7rem; font-weight: 800; line-height: 1.1; }
      .stat-label {
        font-size: 0.68rem; font-weight: 600; color: #888;
        text-transform: uppercase; letter-spacing: 0.06em; margin-top: 5px;
      }
      .chart-card-title {
        font-size: 0.76rem; font-weight: 700; color: #00274C;
        text-transform: uppercase; letter-spacing: 0.06em;
        margin-bottom: 8px;
      }
      .chart-panel {
        background: #ffffff; border: 1px solid #eef1f5; border-radius: 10px;
        padding: 14px 16px; height: 100%;
      }

    ")),
    tags$script(HTML("
      $(document).ready(function() {
        var groupColors = {
          'Education & Youth Development':               '#4e79a7',
          'Health & Wellbeing':                          '#59a14f',
          'Housing, Economic Opportunity & Basic Needs': '#f28e2b',
          'Community Engagement & Civic Life':           '#76b7b2',
          'Equity, Justice & Inclusion':                 '#b07aa1',
          'Environment & Sustainability':                '#499894',
          'Arts, Culture & Creative Expression':         '#e15759',
          'Other / Specialized Areas':                   '#9c755f'
        };
        $('#priority_filter option').each(function() {
          var col = groupColors[$(this).val()];
          if (col) {
            $(this).css({ 'background-color': col, 'color': '#fff', 'font-weight': '600' });
          }
        });
      });
    "))
  ),
  
  # ── Map tab ──────────────────────────────────────────────────────────────
  nav_panel(
    "Map",
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        bg      = "white",
        width   = 390,
        open    = "open",
        padding = "16px",
        uiOutput("org_panel"),
        tags$hr(style = "margin: 14px 0; border-color: #e8edf2;"),
        tags$p(class = "sidebar-section-label", "Partner Status"),
        radioButtons(
          "status_filter", label = NULL,
          choices = c("All", "Active", "Lead"), selected = "All", inline = TRUE
        ),
        tags$hr(style = "margin: 14px 0; border-color: #e8edf2;"),
        checkboxInput(
          "recent_match_filter",
          label = paste0("Only show orgs with a match in the last 2 fiscal years (",
                         paste(recent_fys, collapse = " or "), ")"),
          value = FALSE
        ),
        tags$hr(style = "margin: 14px 0; border-color: #e8edf2;"),
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:5px;",
          tags$p(class = "sidebar-section-label", style = "margin:0;", "Community Priority"),
          actionLink("clear_filter", "Clear all",
                     style = "font-size:0.73rem; color:#aaa; text-decoration:none;")
        ),
        tags$p(
          style = "font-size:0.72rem; color:#bbb; margin-bottom:8px; line-height:1.4;",
          "Hold Ctrl (Windows) or \u2318 Cmd (Mac) to select multiple."
        ),
        selectInput(
          "priority_filter", label = NULL,
          choices   = names(group_map), selected = NULL,
          multiple  = TRUE, selectize = FALSE, size = 8, width = "100%"
        ),
        tags$hr(style = "margin: 14px 0; border-color: #e8edf2;"),
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:5px;",
          tags$p(class = "sidebar-section-label", style = "margin:0;", "Form of Engagement"),
          if (length(engagement_choices) > 0)
            actionLink("clear_engagement_filter", "Clear all",
                       style = "font-size:0.73rem; color:#aaa; text-decoration:none;")
        ),
        if (length(engagement_choices) > 0) {
          tagList(
            tags$p(
              style = "font-size:0.72rem; color:#bbb; margin-bottom:8px; line-height:1.4;",
              "Hold Ctrl (Windows) or \u2318 Cmd (Mac) to select multiple."
            ),
            selectInput(
              "engagement_filter", label = NULL,
              choices = engagement_choices, selected = NULL,
              multiple = TRUE, selectize = FALSE, size = 5, width = "100%"
            )
          )
        } else
          tags$p(style = "font-size:0.78rem;color:#bbb;font-style:italic;", "No match data available yet."),
        tags$hr(style = "margin: 14px 0; border-color: #e8edf2;"),
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:5px;",
          tags$p(class = "sidebar-section-label", style = "margin:0;", "Skill Area"),
          if (length(skill_choices) > 0)
            actionLink("clear_skill_filter", "Clear all",
                       style = "font-size:0.73rem; color:#aaa; text-decoration:none;")
        ),
        if (length(skill_choices) > 0) {
          tagList(
            tags$p(
              style = "font-size:0.72rem; color:#bbb; margin-bottom:8px; line-height:1.4;",
              "Hold Ctrl (Windows) or \u2318 Cmd (Mac) to select multiple."
            ),
            selectInput(
              "skill_filter", label = NULL,
              choices = skill_choices, selected = NULL,
              multiple = TRUE, selectize = FALSE, size = 5, width = "100%"
            )
          )
        } else
          tags$p(style = "font-size:0.78rem;color:#bbb;font-style:italic;", "No match data available yet."),
        tags$hr(style = "margin: 14px 0; border-color: #e8edf2;"),
        tags$p(class = "sidebar-section-label", "School / College / Unit"),
        if (length(scu_choices) > 0)
          selectInput(
            "scu_filter", label = NULL,
            choices = scu_choices, selected = NULL,
            multiple = TRUE, selectize = FALSE, size = 5, width = "100%"
          )
        else
          tags$p(
            style = "font-size:0.78rem;color:#bbb;font-style:italic;",
            "Coming soon \u2014 this filter will activate automatically once that data is added."
          )
      ),
      card(
        full_screen = TRUE,
        leafletOutput("map", height = "100%")
      )
    )
  ),
  
  # ── No-geo-data tab ────────────────────────────────────────────────────────
  nav_panel(
    "Accounts with No Geographic Data",
    layout_sidebar(
      fillable = TRUE,
      sidebar = sidebar(
        bg      = "white",
        width   = 390,
        open    = "open",
        padding = "16px",
        uiOutput("problem_org_panel")
      ),
      card(
        full_screen = TRUE,
        card_header("Accounts with No Geographic Data"),
        card_body(
          tags$p(
            style = "font-size:0.79rem; color:#888; margin-bottom:12px; line-height:1.45;",
            "These accounts don't have a usable address on file, so they can't be placed on the map yet. ",
            "Click a row to see their details and matched projects."
          ),
          DTOutput("problem_table")
        )
      )
    )
  ),
  
  # ── Data Dictionary tab ───────────────────────────────────────────────────
  nav_panel(
    "Data Dictionary",
    div(
      style = "max-width:900px; margin:0 auto; padding:8px 4px 24px;",
      div(
        class = "instruction-box", style = "margin-bottom:18px;",
        tags$strong("Draft content."), "This needs significant cleaning."
      ),
      card(
        card_body(
          accordion(
            open = "Accounts Data",
            
            accordion_panel(
              "Accounts Data",
              tags$p(style = "font-size:0.78rem;color:#888;margin-bottom:10px;",
                     "From ", tags$code("Accounts_wideCategories_Geocoded.csv"), ", one row per community partner."),
              field_table(list(
                c("Account Name", "The organization's official name."),
                c("Description", "Free-text summary of the org's mission/programs, shown in the org detail panel."),
                c("Billing Address Line 1 / 2, City, State/Province, Zip/Postal Code",
                  "The org's primary mailing address on file."),
                c("Ginsberg Partner Status", "Where the relationship currently stands: Active or Lead."),
                c("Category 1\u20139", "Up to nine focus-area tags per org, from Salesforce's category picklist. Rolled up into the 8 Community Priority groups below for map coloring/filtering."),
                c("Category: Type / Category: Subtype", "A separate, higher-level categorization field from Salesforce. Not currently used for grouping in this app \u2014 worth checking whether it should replace or supplement Category 1\u20139."),
                c("Website", "Joined in from a separate file, websites.csv, by account name."),
                c("latitude / longitude", "Geocoded coordinates. A few rows had missing/bad source addresses that geocoded to bogus points; those are either manually corrected or, if no address exists at all, excluded from the map and listed on the \u201cAccounts with No Geographic Data\u201d tab instead."),
                c("Total Matches", "Coming soon. Whole-number count of matches per org, to be added from Salesforce."),
                c("Created Date (\u201cPartner Since\u201d)", "Coming soon. Date the org was added to Salesforce; will be used to show partnership length."),
                c("School / College / Unit", "Coming soon. Which U-M school, college, or unit the partnership sits under. Filter-only \u2014 won't be shown on the org card itself.")
              ))
            ),
            
            accordion_panel(
              "Matches Data",
              tags$p(style = "font-size:0.78rem;color:#888;margin-bottom:10px;",
                     "From ", tags$code("Mapping CP Network.xlsx"), ", one row per match/project, across the FY25 and FY26 sheets."),
              field_table(list(
                c("Initiative Account \u2192 Org", "Which community partner the match belongs to."),
                c("Initiative \u2192 Project", "Name/title of the specific match or project."),
                c("Resource Offering \u2192 Offering", "What kind of engagement the match was (e.g. a course-based project, a volunteer opportunity). Powers the \u201cForm of Engagement\u201d filter, currently shown as the raw values from the sheet."),
                c("Match_Category__r.Name \u2192 Category", "The skill/expertise area the match called for. Grouped into the 8 Skill Area buckets below for the \u201cSkill Area\u201d filter."),
                c("Match Completed Date \u2192 Completed", "When the match wrapped up."),
                c("FY", "Which sheet the match came from \u2014 FY25 or FY26.")
              ))
            ),
            
            accordion_panel(
              "Community Priority Groups",
              tags$p(style = "font-size:0.78rem;color:#888;margin-bottom:10px;",
                     "The 8 buckets used for map coloring and the Community Priority filter, and which raw Category 1\u20139 values roll up into each."),
              field_table(lapply(names(group_map), function(g) c(g, paste(group_map[[g]], collapse = "; "))))
            ),
            
            accordion_panel(
              "Skill Area Groups",
              tags$p(style = "font-size:0.78rem;color:#888;margin-bottom:10px;",
                     "The 8 buckets used for the Skill Area filter, and which raw match-category values roll up into each."),
              field_table(lapply(names(skill_group_map), function(g) c(g, paste(skill_group_map[[g]], collapse = "; "))))
            ),
            
            accordion_panel(
              "Filters on the Map Tab",
              field_table(list(
                c("Partner Status", "Active / Lead / All."),
                c("Recent match (last 2 FYs)", "Toggle \u2014 shows only orgs with at least one match in the two most recent fiscal years found in the matches data."),
                c("Community Priority", "Org-level. An org qualifies if any of its Category 1\u20139 tags falls in a selected group."),
                c("Form of Engagement", "Match-level. An org qualifies if any of its matches has a selected engagement type."),
                c("Skill Area", "Match-level, grouped. An org qualifies if any of its matches' category rolls up into a selected group."),
                c("School / College / Unit", "Coming soon \u2014 will activate automatically once that data is added.")
              ))
            ),
            
            accordion_panel(
              "Known Data Quality Notes",
              tags$ul(
                style = "font-size:0.82rem;color:#555;line-height:1.7;padding-left:20px;",
                tags$li("25 accounts have no address on file at all and can't be geocoded. They're excluded from the map and listed on the \u201cAccounts with No Geographic Data\u201d tab."),
                tags$li("Two accounts (Detroit People's Platform; Next Chapter Bookclub of Saline) had bad source addresses that geocoded to another country. Both were manually corrected with verified addresses/coordinates."),
                tags$li("A handful of accounts are legitimately located outside Michigan \u2014 real out-of-state or international partners, not an error."),
                tags$li("The Infographics tab's Southeast Michigan / Rest of Michigan split is a rough latitude/longitude bounding-box approximation, not actual county boundaries \u2014 worth spot-checking.")
              )
            )
          )
        )
      )
    )
  ),
  
  # ── Infographics tab ──────────────────────────────────────────────────────
  nav_panel(
    "Infographics",
    div(
      style = "max-width:1100px; margin:0 auto; padding:8px 4px 24px;",
      
      # ── Section 1: Community Partners at a Glance ──────────────────────
      card(
        card_body(
          tags$div(class = "infographic-section-title", "Community Partners at a Glance"),
          
          tags$div(
            class = "stat-tile-row",
            stat_tile(n_total, "Total Community Partners"),
            stat_tile(n_se_mi, "Southeast Michigan", geo_colors[["Southeast Michigan"]]),
            stat_tile(n_mi_total, "Total State of Michigan", geo_colors[["Rest of Michigan"]]),
            stat_tile(n_usa_other, "USA (Other States)", geo_colors[["USA (Other States)"]]),
            stat_tile(n_international, "International", geo_colors[["International"]])
          ),
          
          layout_columns(
            col_widths = c(6, 6),
            div(
              class = "chart-panel",
              tags$div(class = "chart-card-title", "Where Our Partners Are Located"),
              plotlyOutput("geo_donut", height = "260px")
            ),
            div(
              class = "chart-panel",
              tags$div(class = "chart-card-title", "Top 3 Community Priority Areas"),
              tags$p(
                style = "font-size:0.72rem;color:#999;margin-top:-4px;margin-bottom:10px;",
                "By number of partners working in that area"
              ),
              plotlyOutput("top3_priority_bar", height = "220px")
            )
          )
        )
      ),
      
      tags$div(style = "height:20px;"),
      
      # ── Section 2: Matches Over Time ────────────────────────────────────
      card(
        card_body(
          tags$div(class = "infographic-section-title", "Matches Over Time"),
          
          div(
            class = "chart-panel",
            style = "margin-bottom:18px;",
            tags$div(class = "chart-card-title", "Partners With the Most Matches (All-Time)"),
            plotlyOutput("leaderboard_bar", height = "320px")
          ),
          
          tags$div(
            class = "stat-tile-row",
            stat_tile(n_recent_matches,
                      paste0("Matches in the Last 2 FYs (", paste(recent_fys, collapse = " + "), ")")),
            div(
              class = "chart-panel", style = "flex:2 1 260px;",
              tags$div(class = "chart-card-title", "By Fiscal Year"),
              plotlyOutput("fy_comparison_bar", height = "110px")
            )
          ),
          
          tags$p(
            style = "font-size:0.78rem;color:#888;margin-bottom:10px;",
            "Most common skill areas, forms of engagement, and community priorities among matches in the last 2 fiscal years:"
          ),
          layout_columns(
            col_widths = c(4, 4, 4),
            div(
              class = "chart-panel",
              tags$div(class = "chart-card-title", "Skill Areas"),
              plotlyOutput("skill_recent_bar", height = "230px")
            ),
            div(
              class = "chart-panel",
              tags$div(class = "chart-card-title", "Forms of Engagement"),
              plotlyOutput("engagement_recent_bar", height = "230px")
            ),
            div(
              class = "chart-panel",
              tags$div(class = "chart-card-title", "Community Priorities"),
              plotlyOutput("priority_recent_bar", height = "230px")
            )
          )
        )
      )
    )
  ),
  
  # ── About tab ─────────────────────────────────────────────────────────────
  nav_panel(
    "About",
    div(
      style = "max-width:800px; margin:0 auto; padding:8px 4px 24px;",
      div(
        class = "instruction-box", style = "margin-bottom:18px;",
        tags$strong("Draft content."), "Skeleton that needs fleshing out.",
        " Anything in brackets is a placeholder \u2014 real details needed before final publishing."
      ),
      card(
        card_body(
          tags$div(class = "infographic-section-title", "About This Map"),
          tags$p(
            style = "font-size:0.88rem;color:#444;line-height:1.65;margin-bottom:14px;",
            "This site visualizes the Edward Ginsberg Center's community partnerships \u2014 where our ",
            "partners are located, what they focus on, and the matches we've facilitated between them ",
            "and University of Michigan students, faculty, and staff."
          ),
          div(class = "about-box",
              div(class = "about-title", "Where the Data Comes From"),
              tags$p("The Ginsberg Center began tracking community partner relationships in Salesforce in 2017.
                    The account data on this map comes from those Salesforce records, and the matches shown
                    come from the FY25 and FY26 match-tracking spreadsheet."),
              tags$p("This map is not a complete history of the Center's work \u2014 relationships that predate
                    Salesforce, or that aren't fully reflected in how matches are recorded, may be under-
                    represented here. Treat it as a current, evolving snapshot rather than a definitive record.")
          ),
          
          tags$div(class = "infographic-section-title", style = "margin-top:24px;", "Credits"),
          field_table(list(
            c("Built by", "[Placeholder \u2014 who owns this tool, e.g. \u201cGinsberg Center Data & Evaluation Team\u201d]"),
            c("Data sources", "Salesforce account records; the FY25\u2013FY26 match-tracking spreadsheet (\u201cMapping CP Network.xlsx\u201d)"),
            c("Design", "Built to the Ginsberg Center / University of Michigan brand style guide"),
            c("Built with", "R, Shiny, leaflet, plotly, bslib, and DT"),
            c("Questions or corrections", "[Placeholder]")
          ))
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  
  # Does an org match any selected focus area group?
  org_matches <- function(org_row, selected_groups) {
    selected_cats <- unlist(group_map[selected_groups], use.names = FALSE)
    org_cats      <- unlist(org_row[, cat_cols], use.names = FALSE)
    org_cats      <- org_cats[!is.na(org_cats) & org_cats != ""]
    any(org_cats %in% selected_cats)
  }
  
  # ── Initial map render ────────────────────────────────────────────────────
  output$map <- renderLeaflet({
    leaflet(map_orgs) |>
      addProviderTiles("OpenStreetMap") |>
      setView(lng = -83.5, lat = 42.4, zoom = 9) |>
      addCircleMarkers(
        lng          = ~longitude,
        lat          = ~latitude,
        layerId      = ~Account.Name,
        label        = make_label(map_orgs),
        labelOptions = labelOptions(textsize = "13px"),
        group        = "all",
        radius       = 9,
        color        = "#ffffff",
        fillColor    = marker_color,
        fillOpacity  = 0.95,
        weight       = 2,
        opacity      = 1,
        clusterOptions = markerClusterOptions()
      ) |>
      addEasyButton(easyButton(
        icon    = "fa-crosshairs",
        title   = "Reset view",
        onClick = JS("function(btn, map){ map.setView([42.4, -83.5], 9); }")
      ))
  })
  
  # ── Re-render markers when filters change ─────────────────────────────────
  observe({
    selected   <- input$priority_filter
    status     <- input$status_filter
    recent_only <- input$recent_match_filter
    engagement <- input$engagement_filter
    skill      <- input$skill_filter
    scu        <- input$scu_filter
    
    base <- if (is.null(status) || status == "All") map_orgs
    else map_orgs |> filter(Ginsberg.Partner.Status == status)
    
    if (isTRUE(recent_only)) {
      recent_orgs <- projects |> filter(FY %in% recent_fys) |> pull(Org) |> unique()
      base        <- base |> filter(Account.Name %in% recent_orgs)
    }
    
    if (!is.null(engagement) && length(engagement) > 0) {
      eng_orgs <- projects |> filter(Offering %in% engagement) |> pull(Org) |> unique()
      base     <- base |> filter(Account.Name %in% eng_orgs)
    }
    
    if (!is.null(skill) && length(skill) > 0) {
      skill_orgs <- projects |> filter(skill_to_group[Category] %in% skill) |> pull(Org) |> unique()
      base       <- base |> filter(Account.Name %in% skill_orgs)
    }
    
    if (!is.null(scu) && length(scu) > 0) {
      base <- base |> filter(School.College.Unit %in% scu)
    }
    
    leafletProxy("map") |>
      clearMarkers() |>
      clearMarkerClusters() |>
      clearGroup("highlight") |>
      clearPopups()
    
    if (length(selected) == 0) {
      leafletProxy("map") |>
        addCircleMarkers(
          data         = base,
          lng          = ~longitude,
          lat          = ~latitude,
          layerId      = ~Account.Name,
          label        = make_label(base),
          labelOptions = labelOptions(textsize = "13px"),
          group        = "all",
          radius       = 9,
          color        = "#ffffff",
          fillColor    = marker_color,
          fillOpacity  = 0.95,
          weight       = 2,
          opacity      = 1,
          clusterOptions = markerClusterOptions()
        )
    } else {
      matched   <- base[sapply(seq_len(nrow(base)), function(i) org_matches(base[i, ], selected)), ]
      unmatched <- base[sapply(seq_len(nrow(base)), function(i) !org_matches(base[i, ], selected)), ]
      
      if (nrow(unmatched) > 0) {
        leafletProxy("map") |>
          addCircleMarkers(
            data         = unmatched,
            lng          = ~longitude,
            lat          = ~latitude,
            layerId      = ~Account.Name,
            label        = make_label(unmatched),
            labelOptions = labelOptions(textsize = "13px"),
            radius       = 5,
            color        = "#bbbbbb",
            fillColor    = "#cccccc",
            fillOpacity  = 0.3,
            weight       = 1,
            opacity      = 0.4,
            group        = "unmatched"
          )
      }
      if (nrow(matched) > 0) {
        leafletProxy("map") |>
          addCircleMarkers(
            data         = matched,
            lng          = ~longitude,
            lat          = ~latitude,
            layerId      = ~Account.Name,
            label        = make_label(matched),
            labelOptions = labelOptions(textsize = "13px"),
            radius       = 10,
            color        = "#ffffff",
            fillColor    = marker_color,
            fillOpacity  = 0.95,
            weight       = 2.5,
            opacity      = 1,
            group        = "matched"
          )
      }
    }
  })
  
  # ── Clear community priority filter ───────────────────────────────────────
  observeEvent(input$clear_filter, {
    updateSelectInput(session, "priority_filter", selected = character(0))
  })
  
  # ── Clear form of engagement filter ───────────────────────────────────────
  observeEvent(input$clear_engagement_filter, {
    updateSelectInput(session, "engagement_filter", selected = character(0))
  })
  
  # ── Clear skill area filter ───────────────────────────────────────────────
  observeEvent(input$clear_skill_filter, {
    updateSelectInput(session, "skill_filter", selected = character(0))
  })
  
  # ── Selected org state ────────────────────────────────────────────────────
  selected_org <- reactiveVal(NULL)
  
  observeEvent(input$deselect_org, {
    selected_org(NULL)
    leafletProxy("map") |> clearGroup("highlight") |> clearPopups()
  })
  
  # ── Marker click: highlight + popup + sidebar ─────────────────────────────
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click$id)) {
      selected_org(click$id)
      org <- map_orgs |> filter(Account.Name == click$id)
      if (nrow(org) > 0) {
        leafletProxy("map") |>
          clearGroup("highlight") |>
          clearPopups() |>
          addCircleMarkers(
            lng         = click$lng,
            lat         = click$lat,
            radius      = 16,
            color       = "#FFD700",
            fillOpacity = 0,
            weight      = 3,
            opacity     = 1,
            group       = "highlight",
            options     = pathOptions(interactive = FALSE)
          ) |>
          addPopups(
            lng   = click$lng,
            lat   = click$lat,
            popup = make_popup(org)
          )
      }
    }
  })
  
  # Clicking blank map clears highlight and popup only
  # (do NOT reset selected_org — marker clicks propagate to map_click too)
  observeEvent(input$map_click, {
    leafletProxy("map") |> clearGroup("highlight") |> clearPopups()
  })
  
  # ── Shared org detail builder (Details/Matches tabs) ─────────────────────
  # Used by both the Map tab's sidebar and the "No Geographic Data" tab's
  # sidebar, so the two stay visually and behaviorally consistent.
  build_org_detail_ui <- function(org_name, data, deselect_id) {
    org <- data |> filter(Account.Name == org_name)
    if (nrow(org) == 0) return(NULL)
    
    grp     <- org$primary_group
    grp_col <- if (!is.na(grp)) group_pal(grp) else "#cccccc"
    
    org_projects <- projects |> filter(Org == org_name) |> arrange(FY, Project)
    
    addr_parts <- c(
      org$Billing.Address.Line.1,
      if (!is.na(org$Billing.Address.Line.2) && org$Billing.Address.Line.2 != "NA")
        org$Billing.Address.Line.2,
      paste0(org$Billing.City, ", ", org$Billing.State.Province, " ", org$Billing.Zip.Postal.Code)
    )
    addr <- paste(addr_parts[addr_parts != "" & !is.na(addr_parts)], collapse = "\n")
    
    # Build match cards
    if (nrow(org_projects) == 0) {
      proj_html <- tags$p(
        style = "color:#aaa; font-style:italic; font-size:0.82rem;",
        "No matched projects on record."
      )
    } else {
      rows <- lapply(seq_len(nrow(org_projects)), function(i) {
        p      <- org_projects[i, ]
        fy_bg  <- if (p$FY == "FY25") "#4e79a7" else "#FFCB05"
        fy_txt <- if (p$FY == "FY25") "#fff"    else "#1a1a1a"
        tags$div(
          class = paste("match-card", tolower(p$FY)),
          # Top row: FY badge + category + date
          tags$div(
            style = "display:flex; justify-content:space-between; align-items:flex-start; margin-bottom:6px;",
            tags$div(
              tags$span(
                style = paste0("background:", fy_bg, ";color:", fy_txt,
                               ";padding:2px 9px;border-radius:20px;font-size:0.68rem;font-weight:700;"),
                p$FY
              ),
              if (!is.na(p$Category))
                tags$span(style = "font-size:0.69rem; color:#888; margin-left:6px;", p$Category)
            ),
            if (!is.na(p$Completed))
              tags$span(
                style = "font-size:0.69rem; color:#bbb; white-space:nowrap; margin-left:4px;",
                format(p$Completed, "%b %d, %Y")
              )
          ),
          # Project title
          tags$p(
            style = "font-size:0.85rem; font-weight:600; color:#1a1a1a; margin:0 0 3px 0;",
            p$Project
          ),
          # Offering
          if (!is.na(p$Offering))
            tags$p(style = "font-size:0.78rem; color:#666; margin:0;", p$Offering)
        )
      })
      proj_html <- tagList(rows)
    }
    
    tagList(
      # ── Org header: group color accent stripe ────────────────────────────
      tags$div(
        style = paste0(
          "border-left:4px solid ", grp_col, "; padding-left:10px; margin-bottom:10px;"
        ),
        tags$div(class = "org-name", org_name),
        if (!is.na(org$Website) && org$Website != "")
          tags$a(class = "org-website", href = org$Website,
                 target = "_blank", rel = "noopener noreferrer", org$Website),
        if (!is.na(org$Ginsberg.Partner.Status) && org$Ginsberg.Partner.Status != "")
          tags$span(
            style = paste0(
              "display:inline-block;padding:2px 10px;border-radius:20px;",
              "font-size:0.7rem;font-weight:700;",
              if (org$Ginsberg.Partner.Status == "Active")
                "background:#198754;color:#fff;"
              else
                "background:#FFCB05;color:#1a1a1a;"
            ),
            org$Ginsberg.Partner.Status
          )
      ),
      
      # ── Stats: total matches + partnership length ────────────────────────
      # Placeholder text until "Total Matches" and "Created Date" land in the
      # data (expected Thursday) - see the "Upcoming fields" note near the
      # top of the file.
      tags$div(
        style = "display:flex; gap:22px; margin-bottom:14px;",
        tags$div(
          tags$p(
            style = "font-size:0.65rem;font-weight:700;text-transform:uppercase;letter-spacing:0.09em;color:#00274C;margin-bottom:3px;",
            "Total Matches"
          ),
          if (!is.na(org$Total.Matches))
            tags$p(style = "font-size:1.05rem;font-weight:700;color:#1a1a1a;margin:0;", org$Total.Matches)
          else
            tags$p(style = "font-size:0.78rem;font-weight:500;color:#bbb;font-style:italic;margin:0;", "Coming soon")
        ),
        tags$div(
          tags$p(
            style = "font-size:0.65rem;font-weight:700;text-transform:uppercase;letter-spacing:0.09em;color:#00274C;margin-bottom:3px;",
            "Partner Since"
          ),
          if (!is.na(org$Created.Date))
            tags$p(style = "font-size:0.9rem;font-weight:700;color:#1a1a1a;margin:0;", format_partner_since(org$Created.Date))
          else
            tags$p(style = "font-size:0.78rem;font-weight:500;color:#bbb;font-style:italic;margin:0;", "Coming soon")
        )
      ),
      
      # ── Details / Matches tabs ───────────────────────────────────────────
      navset_tab(
        nav_panel(
          "Details",
          tags$div(
            style = "padding-top:10px;",
            # Address
            if (!is.na(org$Billing.Address.Line.1) && org$Billing.Address.Line.1 != "")
              tags$p(
                style = "font-size:0.82rem;color:#666;white-space:pre-line;margin-bottom:12px;",
                addr
              ),
            # Focus area badges
            if (!is.na(org$Categories)) {
              cat_vals <- unlist(org[, cat_cols], use.names = FALSE)
              cat_vals <- cat_vals[!is.na(cat_vals) & cat_vals != ""]
              tags$div(
                style = "margin-bottom:14px;",
                tags$p(
                  style = "font-size:0.67rem;font-weight:700;text-transform:uppercase;letter-spacing:0.09em;color:#00274C;margin-bottom:7px;",
                  "Focus Areas"
                ),
                tagList(lapply(cat_vals, function(cat) {
                  bg <- group_pal(cat_to_group[cat])
                  tags$span(
                    style = paste0(
                      "background:", bg, ";color:#fff;",
                      "padding:3px 10px;border-radius:20px;",
                      "font-size:0.73rem;font-weight:500;",
                      "margin:2px;display:inline-block;"
                    ),
                    cat
                  )
                }))
              )
            },
            # Description
            if (!is.na(org$Description) && org$Description != "")
              tags$div(
                tags$p(
                  style = "font-size:0.67rem;font-weight:700;text-transform:uppercase;letter-spacing:0.09em;color:#00274C;margin-bottom:5px;",
                  "About"
                ),
                tags$p(
                  style = "font-size:0.8rem;color:#555;line-height:1.55;margin:0;",
                  org$Description
                )
              )
          )
        ),
        nav_panel(
          paste("Matches", if (nrow(org_projects) > 0) paste0("(", nrow(org_projects), ")")),
          tags$div(style = "padding-top:10px;", proj_html)
        )
      ),
      
      # ── Close button, sits under the tabs content (not floating) ────────
      tags$div(
        style = "margin-top:16px; padding-top:14px; border-top:1px solid #eef1f5;",
        actionLink(deselect_id, "\u00D7 Close", class = "close-panel-btn")
      )
    )
  }
  
  # ── Map tab: org panel renderUI ──────────────────────────────────────────
  output$org_panel <- renderUI({
    org_name <- selected_org()
    
    # ── Default / about state ──────────────────────────────────────────────
    if (is.null(org_name)) {
      return(div(
        div(class = "about-box",
            div(class = "about-title", "About the Map"),
            tags$p("The Ginsberg Center began tracking community partner relationships in Salesforce in 2017.
                  The data on this map comes from those Salesforce records, so the earliest relationship
                  that may appear is January 2017."),
            tags$p("This map represents relationships documented in our Salesforce system and is not a
                  complete history of the Ginsberg Center\u2019s work with community partners. Ginsberg
                  Center has worked with communities and organizations for many years prior to adopting
                  Salesforce, and some of our current relationships began before 2017. Likewise, some
                  former community partners may no longer be active or may not appear because of how
                  relationships are recorded in Salesforce."),
            tags$p("As a result, the number of years shown for a relationship may not reflect the full
                  length of our relationship with a community partner. A relationship that began before
                  2017, for example, may appear as beginning in 2017 because that is the earliest point
                  represented in this dataset."),
            tags$p("We share this map as a way to visualize the community partnerships documented in our
                  current data, not to define the full history, depth, or significance of Ginsberg
                  Center\u2019s relationships with communities.")
        ),
        div(class = "instruction-box",
            tags$strong("Click a marker"), " on the map to view organization details and matched projects."
        )
      ))
    }
    
    build_org_detail_ui(org_name, map_orgs, "deselect_org")
  })
  
  # ── No-geo-data tab: table + detail panel ────────────────────────────────
  selected_problem_org <- reactiveVal(NULL)
  
  output$problem_table <- renderDT({
    datatable(
      problem_orgs |>
        transmute(
          `Account Name`   = Account.Name,
          `Partner Status` = ifelse(is.na(Ginsberg.Partner.Status) | Ginsberg.Partner.Status == "",
                                    "\u2014", Ginsberg.Partner.Status),
          `Focus Areas`    = ifelse(is.na(Categories), "\u2014", Categories)
        ),
      selection = "single",
      rownames  = FALSE,
      options   = list(pageLength = 25, dom = "ftip"),
      class     = "display"
    )
  })
  
  observeEvent(input$problem_table_rows_selected, {
    sel <- input$problem_table_rows_selected
    if (length(sel) == 0) {
      selected_problem_org(NULL)
    } else {
      selected_problem_org(problem_orgs$Account.Name[sel])
    }
  })
  
  observeEvent(input$deselect_problem_org, {
    selected_problem_org(NULL)
    dataTableProxy("problem_table") |> selectRows(NULL)
  })
  
  output$problem_org_panel <- renderUI({
    org_name <- selected_problem_org()
    
    if (is.null(org_name)) {
      return(div(class = "instruction-box",
                 tags$strong("Click an account"), " in the table to view its details and matched projects."
      ))
    }
    
    build_org_detail_ui(org_name, problem_orgs, "deselect_problem_org")
  })
  
  # ── Infographics charts ──────────────────────────────────────────────────
  # All static/non-reactive - the underlying data doesn't depend on any
  # filter or selection, so these just render once per session.
  
  output$geo_donut <- renderPlotly({
    make_donut(
      geo_counts |> filter(Geo.Bucket != "Unknown"),
      label_col = "Geo.Bucket", val_col = "n", colors = geo_colors
    )
  })
  
  output$top3_priority_bar <- renderPlotly({
    make_horiz_bar(top3_priorities_df, "Priority", "n", colors = group_colors, unit_label = "partner")
  })
  
  output$leaderboard_bar <- renderPlotly({
    make_horiz_bar(matches_leaderboard, "Org", "Matches", colors = "#00274C", unit_label = "match")
  })
  
  output$fy_comparison_bar <- renderPlotly({
    make_horiz_bar(
      recent_by_fy, "FY", "n",
      colors = bar_colors_for(nrow(recent_by_fy)), unit_label = "match"
    )
  })
  
  output$skill_recent_bar <- renderPlotly({
    make_horiz_bar(top_skills_recent, "skill_group", "n",
                   colors = bar_colors_for(nrow(top_skills_recent)), unit_label = "match")
  })
  
  output$engagement_recent_bar <- renderPlotly({
    make_horiz_bar(top_engagement_recent, "Offering", "n",
                   colors = bar_colors_for(nrow(top_engagement_recent)), unit_label = "match")
  })
  
  output$priority_recent_bar <- renderPlotly({
    make_horiz_bar(top_priorities_recent, "primary_group", "n",
                   colors = group_colors, unit_label = "match")
  })
}

shinyApp(ui = ui, server = server)