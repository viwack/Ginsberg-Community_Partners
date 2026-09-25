library(shiny)
library(bslib)
library(leaflet)
library(dplyr)
library(readxl)
library(tidyr)
library(htmltools)

# ── Data loading ──────────────────────────────────────────────────────────────

orgs     <- read.csv("Accounts_wideCategories_Geocoded.csv", stringsAsFactors = FALSE)
websites <- read.csv("websites.csv", stringsAsFactors = FALSE)
orgs     <- orgs |> left_join(websites, by = "Account.Name")

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

map_orgs <- orgs |>
  filter(!is.na(latitude), !is.na(longitude)) |>
  mutate(
    primary_category = apply(pick(all_of(cat_cols)), 1, function(x) {
      vals <- x[!is.na(x) & x != ""]
      if (length(vals) == 0) NA_character_ else vals[[1]]
    }),
    primary_group = cat_to_group[primary_category]
  )

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
  
  HTML(paste0(
    "<div style='font-family:system-ui,-apple-system,sans-serif;min-width:250px;max-width:320px;padding:6px 2px;'>",
    "<div style='font-size:1rem;font-weight:700;color:#00274C;line-height:1.25;margin-bottom:9px;'>",
    htmlEscape(o$Account.Name), "</div>",
    "<div style='margin-bottom:4px;'>", status_badge, group_badge, "</div>",
    addr_line, website_line,
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
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter")
)

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  title    = tags$span(style = "font-weight:700; letter-spacing:-0.2px;",
                       "Ginsberg Center | Community Partners"),
  theme    = app_theme,
  fillable = "Map",
  
  # ── Head: global CSS + listbox JS ─────────────────────────────────────────
  tags$head(
    tags$style(HTML("

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

      /* ── Focus area listbox ─────────────────────────────────────────── */
      #focus_filter {
        border-radius: 8px !important; border: 1px solid #dde2e8 !important;
        font-size: 0.82rem; overflow: hidden;
      }
      #focus_filter option { padding: 7px 10px; line-height: 1.5; }
      #focus_filter option:checked { filter: brightness(0.78) !important; }

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
        $('#focus_filter option').each(function() {
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
        tags$div(
          style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:5px;",
          tags$p(class = "sidebar-section-label", style = "margin:0;", "Focus Area"),
          actionLink("clear_filter", "Clear all",
                     style = "font-size:0.73rem; color:#aaa; text-decoration:none;")
        ),
        tags$p(
          style = "font-size:0.72rem; color:#bbb; margin-bottom:8px; line-height:1.4;",
          "Hold Ctrl (Windows) or \u2318 Cmd (Mac) to select multiple."
        ),
        selectInput(
          "focus_filter", label = NULL,
          choices   = names(group_map), selected = NULL,
          multiple  = TRUE, selectize = FALSE, size = 8, width = "100%"
        )
      ),
      card(
        full_screen = TRUE,
        leafletOutput("map", height = "100%")
      )
    )
  ),
  
  # ── Data Dictionary tab ───────────────────────────────────────────────────
  nav_panel(
    "Data Dictionary",
    card(
      card_header("Data Dictionary"),
      card_body(tags$p(style = "color:#aaa; font-style:italic;", "Content coming soon."))
    )
  ),
  
  # ── Infographics tab ──────────────────────────────────────────────────────
  nav_panel(
    "Infographics",
    card(
      card_header("Infographics"),
      card_body(tags$p(style = "color:#aaa; font-style:italic;", "Content coming soon."))
    )
  ),
  
  # ── About tab ─────────────────────────────────────────────────────────────
  nav_panel(
    "About",
    card(
      card_header("About & Credits"),
      card_body(tags$p(style = "color:#aaa; font-style:italic;", "Content coming soon."))
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
        radius       = 8,
        color        = ~group_pal(primary_group),
        fillColor    = ~group_pal(primary_group),
        fillOpacity  = 0.85,
        weight       = 1.5,
        opacity      = 1,
        clusterOptions = markerClusterOptions()
      ) |>
      addLegend(
        position  = "bottomright",
        colors    = unname(group_colors),
        labels    = names(group_map),
        title     = "Focus Area",
        opacity   = 0.9,
        className = "info legend"
      ) |>
      addEasyButton(easyButton(
        icon    = "fa-crosshairs",
        title   = "Reset view",
        onClick = JS("function(btn, map){ map.setView([42.4, -83.5], 9); }")
      ))
  })
  
  # ── Re-render markers when filters change ─────────────────────────────────
  observe({
    selected <- input$focus_filter
    status   <- input$status_filter
    
    base <- if (is.null(status) || status == "All") map_orgs
    else map_orgs |> filter(Ginsberg.Partner.Status == status)
    
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
          radius       = 8,
          color        = ~group_pal(primary_group),
          fillColor    = ~group_pal(primary_group),
          fillOpacity  = 0.85,
          weight       = 1.5,
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
            color        = ~group_pal(primary_group),
            fillColor    = ~group_pal(primary_group),
            fillOpacity  = 0.9,
            weight       = 2.5,
            opacity      = 1,
            group        = "matched"
          )
      }
    }
  })
  
  # ── Clear focus filter ────────────────────────────────────────────────────
  observeEvent(input$clear_filter, {
    updateSelectInput(session, "focus_filter", selected = character(0))
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
  
  # ── Org panel renderUI ────────────────────────────────────────────────────
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
    
    # ── Org detail state ───────────────────────────────────────────────────
    org <- map_orgs |> filter(Account.Name == org_name)
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
      # ── Org header: group color accent stripe + × deselect ───────────────
      tags$div(
        style = paste0(
          "border-left:4px solid ", grp_col, "; padding-left:10px; margin-bottom:10px;",
          " display:flex; justify-content:space-between; align-items:flex-start;"
        ),
        tags$div(
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
        actionLink("deselect_org", "\u00D7",
                   style = "color:#ccc;font-size:1.4rem;line-height:1;text-decoration:none;")
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
      )
    )
  })
}

shinyApp(ui = ui, server = server)
