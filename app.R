
# Suicide Prevention Overview-of-Reviews Dashboard


# Packages ----

library(shiny)
library(reactable)
library(rio)
library(here)
library(tidyverse)
library(htmltools)
library(glue)


# Configuration ----

# Columns (besides Outcome) that together define one unique table row
ROW_GROUP_VARS <- c("RefID", "Intervention", "Time", "Moderator")

# Single switch for testing/development (TRUE) vs. a live/public dashboard (FALSE)
DEV_MODE <- FALSE

# CELL_SEP separates the fields of a single estimate's payload; 
CELL_SEP <- "\u241F"
MULTI_SEP <- "\u241E"

## Label overrides 
INTERVENTION_LABELS <- c(
  "School-based prevention interventions"         = "Prevention interventions",
  "School-based education interventions"          = "Educational interventions",
  "School-based suicide prevention interventions" = "High school interventions"
)

TIME_LABELS <- c(
  "at postintervention"                 = "Post-test",
  "Post-test (0-3 months)"              = "0-3 months post-test",
  "Short-term follow-up (3-12 months)"  = "3-12 months follow-up",
  "Follow-up (\u22643 to 20 months)"    = "\u22643- to 20-month follow-up",
  "Follow-up (12 months)"               = "12-month follow-up"
  # "3-month follow-up" and "12-month follow-up" (RefID 20) and "Post-test" (RefID 3485) already read fine as-is and aren't listed here
)

# Doubles as both the section-heading wording AND the section DISPLAY ORDER on the subgroup tab
MODERATOR_LABELS <- c(
  "Targeting STBs as the primary aim of the intervention versus not" =
    "Targeting STBs vs. not specifically targeting STBs",
  "Adolescents in school-based settings versus young adults in college/university settings" =
    "Adolescents in school vs. college/university students",
  "Teacher/counselor involvement versus no school stakeholder involvement" =
    "Teacher/counselor involvement vs. no stakeholder involvement",
  "Multi-stakeholder involvement versus no school stakeholder involvement" =
    "Multi-stakeholder vs. no stakeholder involvement",
  "12-months follow-up versus  \u2264 3-months follow-up" =
    "12-month vs. \u22643-month follow-up",
  "17-20-months follow-up versus  \u2264 3-months follow-up" =
    "17-20-month vs. \u22643-month follow-up",
  "4-weeks intervention duration versus \u2264 1-week intervention duration" =
    "4-week vs. \u22641-week intervention duration",
  "4-104 weeks intervention duration versus \u2264 1-week intervention duration" =
    "4-104 week vs. \u22641-week intervention duration"
  # "High school sub-group" is NOT listed here -- it isn't a "versus" contrast 
)

# Groups related moderators under one broader SECTION HEADING 
MODERATOR_CATEGORY <- c(
  "Targeting STBs as the primary aim of the intervention versus not" =
    "Interventions targeting suicidal thoughts and behaviors (STBs)",
  "Adolescents in school-based settings versus young adults in college/university settings" =
    "School level",
  "Teacher/counselor involvement versus no school stakeholder involvement" =
    "Stakeholder involvement",
  "Multi-stakeholder involvement versus no school stakeholder involvement" =
    "Stakeholder involvement",
  "12-months follow-up versus  \u2264 3-months follow-up" =
    "Follow-up length",
  "17-20-months follow-up versus  \u2264 3-months follow-up" =
    "Follow-up length",
  "4-weeks intervention duration versus \u2264 1-week intervention duration" =
    "Intervention duration",
  "4-104 weeks intervention duration versus \u2264 1-week intervention duration" =
    "Intervention duration"
)

## Color palette ----

CERTAINTY_LEVELS <- c("Very Low", "Low", "Moderate", "High")
CERTAINTY_COLORS <- c(
  "Very Low" = "#8D1D58",
  "Low"      = "#8D1D58",
  "Moderate" = "#004F6E",
  "High"     = "#004F6E"
)
CERTAINTY_FILLED <- c("Very Low" = TRUE, "Low" = FALSE, "Moderate" = FALSE, "High" = TRUE)

# Shared design tokens for the app chrome (title, legend, sidebar, tooltip)
COLORS <- list(
  text = "#000000",
  text_muted = "#4D5859",
  text_light = "#A2AAAD",
  border = "#E6E6E6",
  background_light = "#F8F2E8",
  white = "#ffffff",
  subgroup = "#53C0D8"
)

# Legend key swatch shape 
LEGEND_SWATCH_STYLE <- "width:28px;height:10px;border-radius:4px;"

# Cell effect-bar shape 
EFFECT_BAR_STYLE <- "width:55px;height:9px;border-radius:4px;"

# "Are interventions beneficial?" tab intervention vs. comparator
MAIN_EFFECT_LABELS <- c("Harm", "No effect", "Benefit")
MAIN_EFFECT_COLORS <- c(
  "Harm"      = "#F5A604",
  "No effect" = "#A2AAAD",
  "Benefit"   = "#007030"
)

# "No effect" / "no difference" band, on each metric's own native scale 
NO_EFFECT_SMD_RANGE <- c(-0.05, 0.05)
NO_EFFECT_OR_RANGE <- c(0.91, 1.10)

# "Which intervention groups benefited most?" tab
SUBGROUP_EFFECT_LABELS <- c(
  "No difference detected", "Difference detected",
  "Favors first-named group", "Favors second-named group"
)
SUBGROUP_EFFECT_COLORS <- c(
  "No difference detected"    = COLORS$text_light,
  "Difference detected"       = "#4D5859",
  "Favors first-named group"  = "#0B5394",
  "Favors second-named group" = COLORS$subgroup
)

# Background color for the subgroup tab's section-heading rows 
GROUP_HEADER_BACKGROUND <- "#F8F2E8"

## Outcome consolidation ----

# Maps each raw outcome value onto one of the 3 outcome types
OUTCOME_GROUP_MAP <- c(
  "Suicidal Ideation"               = "Suicidal Ideation",
  "Suicide Attempts"                = "Suicide Attempts",
  "Suicidal Behaviors"              = "Suicide Attempts",
  "Suicidality"                     = "Suicide Attempts",
  "Suicide Prevention Competencies" = "Suicide Competencies",
  "Suicide Awareness"               = "Suicide Competencies",
  "Helping Skills"                  = "Suicide Competencies"
)
OUTCOME_GROUPS <- c("Suicidal Ideation", "Suicide Attempts", "Suicide Competencies")


# Load data ----

SPO_DATA_PATH <- here("data", "SPO_GRADE_Certainty.xlsx")
LINKS_DATA_PATH <- here("data", "spo_review_pdf_links.xlsx")

walk(
  c(SPO_DATA_PATH, LINKS_DATA_PATH),
  ~ if (!file.exists(.x)) stop("Data file not found at: ", .x)
)

raw_df <- import(SPO_DATA_PATH) %>% as_tibble()
names(raw_df) <- str_trim(names(raw_df))
raw_df <- raw_df %>% mutate(across(where(is.character), str_trim))


links_df <- import(LINKS_DATA_PATH) %>% as_tibble()
raw_df <- left_join(raw_df, links_df, by = "RefID")


# Helper functions ----

na_blank <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | x == "", "\u2014", x) # em dash
}

not_blank <- function(x) {
  x <- as.character(x)
  !is.na(x) & x != "" & x != "NA"
}

# strips stray straight double-quote characters 
clean_txt <- function(x) {
  x <- as.character(x)
  x <- str_remove_all(x, '^"+|"+$')
  str_trim(x)
}

# table-display overrides 
apply_label <- function(x, overrides) {
  coalesce(unname(overrides[x]), x)
}

# Flag if a moderator value is present at all to decide whether to show ANY secondary descriptor line
is_real_subgroup <- function(x) {
  not_blank(x) & str_to_lower(x) != "none"
}

# A Moderator value represents a genuine BETWEEN-subgroup comparison only when it names two things being compared 
is_subgroup_contrast <- function(x) {
  not_blank(x) & str_detect(str_to_lower(x), "\\bversus\\b|\\bvs\\.?\\b")
}

# Splits a moderator label style "X vs. Y" string into its two named sides
moderator_side <- function(x, side = c("first", "second")) {
  side <- match.arg(side)
  parts <- str_split_fixed(x, " vs\\. ", 2)
  if (side == "first") parts[, 1] else parts[, 2]
}

# Renders a moderator label style  "X vs. Y" string as two colored spans
render_comparison_text <- function(comparison_text, has_favor) {
  if (!has_favor) {
    return(tags$span(comparison_text, style = sprintf("color:%s;", SUBGROUP_EFFECT_COLORS[["Difference detected"]])))
  }
  parts <- str_split_fixed(comparison_text, " vs\\. ", 2)
  tagList(
    tags$span(parts[1, 1], style = sprintf("color:%s;", SUBGROUP_EFFECT_COLORS[["Favors first-named group"]])),
    tags$span(" vs. ", style = glue("color:{COLORS$text};")),
    tags$span(parts[1, 2], style = sprintf("color:%s;", SUBGROUP_EFFECT_COLORS[["Favors second-named group"]]))
  )
}

# Determines whether a "Favored group" value is the first- or second-named side of that row's Moderator "X versus Y" comparison to pick a color
determine_favor_side <- function(favored, moderator_first, moderator_second, has_verified, metric, est_val) {
  pmap_chr(
    list(favored, moderator_first, moderator_second, has_verified, metric, est_val),
    function(fav, first, second, verified, met, val) {
      if (is.na(fav)) {
        return(NA_character_)
      }

      if (isTRUE(verified) && !is.na(met) && !is.na(val)) {
        if (met == "OR") {
          return(if (val < 1) "first" else "second")
        }
        if (met == "SMD") {
          return(if (val > 0) "first" else "second")
        }
      }

      if (str_trim(str_to_lower(second)) == "not") {
        return(if (str_detect(str_trim(str_to_lower(fav)), "^not\\b")) "second" else "first")
      }

      word_set <- function(x) unique(str_extract_all(str_to_lower(x), "[a-z]+")[[1]])
      fw <- word_set(fav)
      first_overlap <- length(intersect(fw, word_set(first)))
      second_overlap <- length(intersect(fw, word_set(second)))
      if (first_overlap > second_overlap) {
        "first"
      } else if (second_overlap > first_overlap) {
        "second"
      } else {
        NA_character_
      }
    }
  )
}


# Prepare estimates ----

prep <- raw_df %>%
  mutate(
    display_intervention = apply_label(Intervention, INTERVENTION_LABELS),
    display_time = apply_label(clean_txt(Time), TIME_LABELS),
    display_moderator = apply_label(Moderator, MODERATOR_LABELS),
    Time_raw_clean = clean_txt(Time),
    Comparator_clean = str_replace_all(Comparator, "[\r\n]+", "; "),

    # first number in the raw Time string, used only to SORT rows chronologically 
    time_sort_key = coalesce(as.numeric(str_extract(Time, "[0-9]+")), 0),
    outcome_group = unname(OUTCOME_GROUP_MAP[Outcome]),
    metric = case_when(
      str_detect(Estimate, "^(Contrast\\s+)?OR") ~ "OR",
      str_detect(Estimate, "^(Contrast\\s+)?SMD") ~ "SMD",
      TRUE ~ NA_character_
    ),

    # first signed number after the first "="
    est_val = {
      m <- str_extract(Estimate, "=\\s*[-\u2212]?\\s*[0-9]+\\.?[0-9]*")
      m <- str_remove(m, "=")
      m <- str_trim(m)
      m <- str_replace(m, "\u2212", "-")
      m <- str_remove_all(m, " ")
      suppressWarnings(as.numeric(m))
    },

    # first "95% CI" pair in the string
    ci_low_raw = str_match(Estimate, "CI\\s*\\(\\s*([-\u2212]?\\s*[0-9.]+)\\s*,")[, 2],
    ci_high_raw = str_match(Estimate, ",\\s*([-\u2212]?\\s*[0-9.]+)\\s*\\)")[, 2],
    ci_low = suppressWarnings(as.numeric(str_remove_all(str_replace(ci_low_raw, "\u2212", "-"), " "))),
    ci_high = suppressWarnings(as.numeric(str_remove_all(str_replace(ci_high_raw, "\u2212", "-"), " "))),

    # "Are interventions beneficial?" tab: classify by the SIZE of the estimate
    main_effect_cat = case_when(
      is.na(est_val) | is.na(metric) ~ NA_character_,
      metric == "SMD" & between(est_val, NO_EFFECT_SMD_RANGE[1], NO_EFFECT_SMD_RANGE[2]) ~ "No effect",
      metric == "OR" & between(est_val, NO_EFFECT_OR_RANGE[1], NO_EFFECT_OR_RANGE[2]) ~ "No effect",
      metric == "SMD" & est_val > NO_EFFECT_SMD_RANGE[2] ~ "Benefit",
      metric == "SMD" & est_val < NO_EFFECT_SMD_RANGE[1] ~ "Harm",
      metric == "OR" & est_val < NO_EFFECT_OR_RANGE[1] ~ "Benefit",
      metric == "OR" & est_val > NO_EFFECT_OR_RANGE[2] ~ "Harm",
      TRUE ~ NA_character_
    ),
    row_type = if_else(is_subgroup_contrast(Moderator), "subgroup", "main"),

    # Every subgroup-tab (moderator/contrast) estimate should read as a "Contrast" estimate for consistency
    display_estimate = if_else(
      row_type == "subgroup" & !str_detect(Estimate, "^Contrast"),
      paste0("Contrast ", Estimate),
      Estimate
    ),

    # Favored group
    favored_group_raw = `Favored group`,

    # raw "X versus Y" split, used only by determine_favor_side() below to pick a color 
    moderator_first_raw = str_split_fixed(Moderator, "\\s+versus\\s+", 2)[, 1],
    moderator_second_raw = str_split_fixed(Moderator, "\\s+versus\\s+", 2)[, 2],

    # TRUE only when the Estimate reports both named subgroups' own within-group values 
    has_verified_direction = str_count(str_to_lower(Estimate), "within[- ]group") >= 2,
    favor_side = determine_favor_side(
      favored_group_raw, moderator_first_raw, moderator_second_raw,
      has_verified_direction, metric, est_val
    ),

    # the SAME short segment already shown in the row's own "X vs. Y" text used for the tooltip 
    favored_short = case_when(
      favor_side == "first" ~ moderator_side(display_moderator, "first"),
      favor_side == "second" ~ moderator_side(display_moderator, "second"),
      TRUE ~ NA_character_
    ),

    # "Which intervention groups benefited most?" tab coloring
    subgroup_effect_cat = case_when(
      is.na(favored_group_raw) | favored_group_raw == "" ~ NA_character_,
      str_to_lower(favored_group_raw) == "no difference" ~ "No difference detected",
      favor_side == "first" ~ "Favors first-named group",
      favor_side == "second" ~ "Favors second-named group",
      TRUE ~ "Difference detected"
    ),
    effect_cat = if_else(row_type == "subgroup", subgroup_effect_cat, main_effect_cat),

    # The actual text shown in the cell/tooltip 
    effect_label = case_when(
      row_type == "main" ~ main_effect_cat,
      row_type == "subgroup" & subgroup_effect_cat %in% c("Favors first-named group", "Favors second-named group") ~ paste0("Favors: ", favored_short),
      row_type == "subgroup" ~ subgroup_effect_cat,
      TRUE ~ NA_character_
    ),
    certainty_clean = ifelse(Certainty %in% CERTAINTY_LEVELS, Certainty, NA_character_)
  ) %>%
  unite("row_key", all_of(ROW_GROUP_VARS), sep = " || ", remove = FALSE, na.rm = TRUE)

# Produce warning if a row/outcome combination has more than one raw estimate  (only the first would be shown per cell in that case)
dup_check <- prep %>%
  count(row_key, Outcome) %>%
  filter(n > 1)
if (nrow(dup_check) > 0) {
  warning(
    nrow(dup_check),
    " row/outcome combination(s) have more than one estimate; only the first ",
    "will be shown per cell. Add a column to ROW_GROUP_VARS if every ",
    "estimate needs to be shown separately."
  )
}

# Produce message if more than one raw Outcome per display column for the same row (these get stacked together in one cell instead 
group_collisions <- prep %>%
  count(row_key, outcome_group) %>%
  filter(n > 1)
if (nrow(group_collisions) > 0) {
  message(
    nrow(group_collisions),
    " row(s) have more than one raw Outcome mapped into the same display ",
    "column -- these are stacked together in one cell, with a small ",
    "caption naming each one."
  )
}

# Produce message if a subgroup-contrast Moderator with no entry in MODERATOR_LABELS
unlabeled_moderators <- prep %>%
  filter(row_type == "subgroup", !Moderator %in% names(MODERATOR_LABELS)) %>%
  distinct(Moderator)
if (nrow(unlabeled_moderators) > 0) {
  message(
    "Moderator value(s) with no entry in MODERATOR_LABELS (shown as raw text): ",
    paste(unlabeled_moderators$Moderator, collapse = " | ")
  )
}

# Produce warning if any subgroup-tab row is missing a value in the new "Favored group" column
missing_favored_group <- prep %>%
  filter(row_type == "subgroup", is.na(favored_group_raw) | favored_group_raw == "") %>%
  distinct(RefID, Intervention, Outcome)
if (nrow(missing_favored_group) > 0) {
  warning(
    "Missing 'Favored group' value for: ",
    paste(paste0("RefID ", missing_favored_group$RefID, " / ", missing_favored_group$Outcome), collapse = "; "),
    " -- these rows will show as blank/unclassified until that column is filled in."
  )
}


# Build gap-map tables ----

build_gapmap_table <- function(data, group_by_moderator = FALSE) {

  data <- data %>%
    group_by(row_key) %>%
    mutate(row_has_favor = any(!is.na(favor_side))) %>%
    ungroup()

  row_meta <- data %>%
    distinct(row_key, .keep_all = TRUE) %>%
    arrange(
      if (group_by_moderator) match(Moderator, names(MODERATOR_LABELS)) else 0,
      RefID, Intervention, Moderator, time_sort_key
    ) %>%
    mutate(
      row_label_html = pmap_chr(
        list(RefID, display_intervention, display_time, Moderator, display_moderator, row_has_favor),
        function(refid, intervention, time_point, moderator_raw, moderator_display, has_favor) {
          heading <- if (DEV_MODE) paste0(refid, ": ", intervention) else intervention

          moderator_line <- if (group_by_moderator) {
            tags$div(
              style = "font-size:11px;font-style:italic;margin-top:2px;",
              render_comparison_text(moderator_display, has_favor)
            )
          } else if (is_subgroup_contrast(moderator_raw)) {
            tags$div(
              style = glue("font-size:11px;color:{COLORS$subgroup};margin-top:2px;"),
              paste0("Subgroup: ", moderator_display)
            )
          }

          label <- tags$div(
            tags$div(style = "font-weight:600;font-size:13px;line-height:1.35;", heading),
            tags$div(style = "font-size:11px;color:#868e96;margin-top:2px;", time_point),
            moderator_line
          )

          paste(as.character(label), collapse = "")
        }
      ),
      moderator_group = if (group_by_moderator) apply_label(Moderator, MODERATOR_CATEGORY) else NA_character_
    ) %>%
    select(row_key, row_label_html, moderator_group)

  cell_payload <- data %>%
    distinct(row_key, Outcome, .keep_all = TRUE) %>%
    transmute(
      row_key, outcome_group,
      estimate_payload = paste(row_key, Outcome, effect_cat, certainty_clean, na_blank(display_estimate), na_blank(effect_label), sep = CELL_SEP)
    ) %>%
    group_by(row_key, outcome_group) %>%
    summarise(payload = paste(estimate_payload, collapse = MULTI_SEP), .groups = "drop")

  wide <- cell_payload %>%
    pivot_wider(names_from = outcome_group, values_from = payload)

  for (col in setdiff(OUTCOME_GROUPS, names(wide))) wide[[col]] <- NA_character_
  wide <- wide %>% select(row_key, all_of(OUTCOME_GROUPS))

  row_meta %>%
    left_join(wide, by = "row_key") %>%
    select(row_key, moderator_group, row_label_html, all_of(OUTCOME_GROUPS))
}

# Inserts a full-width section-heading pseudo-row before each new moderator_group value 
insert_group_headers <- function(data) {
  data <- data %>% mutate(.order = row_number(), is_header = FALSE)

  headers <- data %>%
    distinct(moderator_group, .keep_all = TRUE) %>%
    mutate(
      is_header = TRUE,
      row_label_html = map_chr(moderator_group, function(g) {
        as.character(tags$div(
          style = "font-weight:700;font-style:italic;font-size:13px;white-space:normal;",
          g
        ))
      }),
      row_key = paste0("HEADER::", moderator_group),
      .order = .order - 0.5
    ) %>%
    mutate(across(all_of(OUTCOME_GROUPS), ~NA_character_))

  bind_rows(headers, data) %>%
    arrange(.order) %>%
    select(-.order, -moderator_group)
}

table_df_main <- build_gapmap_table(filter(prep, row_type == "main"), group_by_moderator = FALSE) %>%
  select(-moderator_group)

table_df_subgroup <- build_gapmap_table(filter(prep, row_type == "subgroup"), group_by_moderator = TRUE) %>%
  insert_group_headers()


# Reactable cell + column builders ----

# Pill-badge CSS for a GRADE certainty level 
certainty_badge_style <- function(level) {
  if (is.na(level) || !level %in% CERTAINTY_LEVELS) {
    return("display:none;")
  }

  color <- CERTAINTY_COLORS[[level]]
  filled <- isTRUE(CERTAINTY_FILLED[[level]])

  glue(
    "font-size:11px;font-weight:600;padding:1px 8px;border-radius:10px;",
    "border:1px solid {color};",
    "color:{if (filled) '#fff' else color};",
    "background:{if (filled) color else '#fff'};"
  )
}

# Renders one estimate's cell
render_estimate_block <- function(payload, effect_colors, show_outcome_note = FALSE) {
  parts <- strsplit(payload, CELL_SEP, fixed = TRUE)[[1]]
  row_key <- parts[1]
  outcome <- parts[2]
  effect <- parts[3]
  certainty <- parts[4]
  estimate <- parts[5]
  effect_label <- parts[6]

  effect_color <- coalesce(unname(effect_colors[effect]), COLORS$text_light)
  cert_color <- unname(CERTAINTY_COLORS[certainty])

  effect_display <- if (is.na(effect_label) || effect_label == "\u2014") "Not estimated" else effect_label
  certainty_display <- if (is.na(certainty)) "Not rated" else certainty
  cert_color_display <- if (is.na(cert_color)) "#000000" else cert_color
  outcome_note <- if (show_outcome_note) outcome else NA_character_

  cell_id <- paste(row_key, outcome, sep = CELL_SEP)
  onclick_js <- sprintf(
    "Shiny.setInputValue('cell_click', '%s', {priority: 'event'})",
    str_replace_all(cell_id, "'", "\\\\'")
  )

  # attributes feed the floating hover tooltip 
  tags$div(
    onclick = onclick_js,
    class = "effect-cell",
    `data-effect` = effect_display,
    `data-effect-color` = effect_color,
    `data-estimate` = estimate,
    `data-certainty` = certainty_display,
    `data-certainty-color` = cert_color_display,
    style = "cursor:pointer;padding:4px;",
    tags$div(
      style = "display:flex;flex-direction:column;align-items:center;gap:3px;",
      if (!is.na(outcome_note)) {
        tags$div(style = "font-size:10px;font-style:italic;color:#868e96;", outcome_note)
      },
      tags$div(
        style = glue("display:flex;align-items:center;justify-content:center;gap:6px;font-size:12px;color:{COLORS$text_light};"),
        tags$span("Effect:"),
        tags$div(style = sprintf("%sbackground:%s;", EFFECT_BAR_STYLE, effect_color))
      ),
      tags$div(
        style = glue("display:flex;align-items:center;justify-content:center;gap:6px;font-size:12px;color:{COLORS$text_light};"),
        tags$span("Certainty:"),
        if (!is.na(certainty)) tags$span(certainty, style = certainty_badge_style(certainty))
      )
    )
  )
}

# defines whether a cell belongs to a section heading or data row
make_outcome_coldef <- function(outcome_name, effect_colors, is_header_vec = NULL) {
  colDef(
    name = outcome_name,
    html = TRUE,
    align = "center",
    minWidth = 175,
    cell = function(value, index) {
      if (is.null(value) || is.na(value) || value == "") {
        if (!is.null(is_header_vec) && isTRUE(is_header_vec[index])) {
          return(tags$span(""))
        }
        return(tags$div(
          style = "display:flex;align-items:center;justify-content:center;height:100%;",
          tags$span("\u2014", style = "color:#adb5bd;")
        ))
      }

      sub_payloads <- strsplit(value, MULTI_SEP, fixed = TRUE)[[1]]
      blocks <- map(
        sub_payloads, render_estimate_block,
        effect_colors = effect_colors,
        show_outcome_note = length(sub_payloads) > 1
      )

      if (length(blocks) == 1) {
        blocks[[1]]
      } else {
        tags$div(
          style = "display:flex;flex-direction:column;gap:4px;",
          map(seq_along(blocks), function(i) {
            if (i == 1) blocks[[i]] else tagList(tags$hr(style = "margin:2px 0;border-color:#eee;"), blocks[[i]])
          })
        )
      }
    }
  )
}

build_table_columns <- function(effect_colors, is_header_vec = NULL) {
  header_col <- if (!is.null(is_header_vec)) list(is_header = colDef(show = FALSE)) else list()

  c(
    list(row_key = colDef(show = FALSE)),
    header_col,
    list(
      row_label_html = colDef(
        name = "Intervention",
        html = TRUE,
        minWidth = 280,
        sticky = "left"
      )
    ),
    setNames(
      lapply(OUTCOME_GROUPS, make_outcome_coldef, effect_colors = effect_colors, is_header_vec = is_header_vec),
      OUTCOME_GROUPS
    )
  )
}

table_columns_main <- build_table_columns(MAIN_EFFECT_COLORS)
table_columns_subgroup <- build_table_columns(SUBGROUP_EFFECT_COLORS, is_header_vec = table_df_subgroup$is_header)


# Legend builders ----

legend_effect_row <- function(label, effect_colors) {
  tags$div(
    style = "display:flex;align-items:center;gap:8px;margin-bottom:4px;",
    tags$div(style = sprintf("%sbackground:%s;", LEGEND_SWATCH_STYLE, effect_colors[[label]])),
    tags$span(label, style = "font-size:12px;")
  )
}

legend_certainty_row <- function(level) {
  tags$div(
    style = "margin-bottom:4px;",
    tags$span(level, style = certainty_badge_style(level))
  )
}

# One legend box per tab 
legend_box <- function(effect_labels, effect_colors, effect_heading, note_text = NULL) {
  div(
    class = "legend-box",
    fluidRow(
      column(
        6,
        div(class = "legend-heading", effect_heading),
        lapply(effect_labels, legend_effect_row, effect_colors = effect_colors)
      ),
      column(
        6,
        div(class = "legend-heading", "Certainty of evidence (GRADE)"),
        lapply(CERTAINTY_LEVELS, legend_certainty_row)
      )
    ),
    if (!is.null(note_text)) div(class = "legend-note legend-note-full", note_text)
  )
}

main_legend <- legend_box(
  MAIN_EFFECT_LABELS, MAIN_EFFECT_COLORS, "Effect vs. comparator",
  "Color shows the direction of the intervention's effect compared to its control group."
)

subgroup_legend <- legend_box(
  SUBGROUP_EFFECT_LABELS, SUBGROUP_EFFECT_COLORS, "Subgroup comparison",
  tagList(
    tags$div("Color shows whether a difference between subgroups was found, and which subgroup did better where that's known rather than whether the intervention itself helped or hurt."),
    tags$div(style = "margin-top:4px;", "Some comparisons don't state a clear direction and will show \"Difference detected\" without naming a side, due to their GRADE certainty rating."),
    tags$div(style = "margin-top:4px;", "Contrast estimates indicate analyses comparing intervention effectiveness (relative to controls) between two different groups or intervention approaches.")
  )
)


# UI ----

# Head CSS 
app_css <- glue(
  r"(
    body { font-family: -apple-system, 'Segoe UI', Roboto, sans-serif; }

    .app-title { font-size: 20px; font-weight: 700; margin: 14px 0 4px 0; }
    .app-subtitle { color: {{COLORS$text_light}}; margin-bottom: 16px; }
    .app-shared-note { font-size: 12px; color: #6c757d; margin: -8px 0 16px 0; }

    .legend-box {
      border: 1px solid #e9ecef; border-radius: 8px; padding: 14px 16px;
      background: #fafbfc; margin-bottom: 16px;
    }
    .legend-heading {
      font-weight: 700; font-size: 13px; margin-bottom: 8px;
      text-transform: uppercase; letter-spacing: .03em; color: {{COLORS$text_muted}};
    }
    .legend-note { font-size: 11px; color: #868e96; margin-top: 8px; }
    .legend-note-full { margin-top: 12px; padding-top: 10px; border-top: 1px solid #e9ecef; }

    .app-disclaimer {
      font-size: 12px; color: {{COLORS$text_muted}}; background: #fafbfc;
      border: 1px solid #e9ecef; border-radius: 8px; padding: 12px 16px; margin: 24px 0 8px 0;
    }

    .app-footer {
      font-size: 11px; color: #868e96; text-align: center; margin: 12px 0 12px 0;
    }
    .app-footer a { color: {{COLORS$subgroup}}; text-decoration: none; }
    .app-footer a:hover { text-decoration: underline; }

    .table-scroll { overflow-x: auto; border: 1px solid #e9ecef; border-radius: 8px; }

    #detail_sidebar {
      position: fixed; top: 0; right: 0; width: 420px; height: 100vh;
      background: #fff; box-shadow: -4px 0 16px rgba(0,0,0,.12);
      padding: 20px; overflow-y: auto; z-index: 1000;
    }
    .detail-field { margin-bottom: 14px; }
    .detail-label {
      font-size: 11px; text-transform: uppercase; letter-spacing: .03em;
      color: {{COLORS$text_muted}}; font-weight: 700; margin-bottom: 2px;
    }
    .detail-value { font-size: 14px; color: {{COLORS$text}}; }

    .effect-cell { cursor: pointer; }

    #floating_tooltip {
      display: none; position: fixed; z-index: 900; width: 270px;
      padding: 14px 16px; background: #ffffff; border: 1px solid #e1e5ea;
      border-radius: 12px; box-shadow: 0 6px 20px rgba(0,0,0,.16);
      text-align: left; color: {{COLORS$text}}; pointer-events: none;
    }
    .tooltip-row {
      display: grid; grid-template-columns: 85px 1fr; gap: 8px;
      margin-bottom: 8px; align-items: center;
    }
    .tooltip-label { font-size: 11px; font-weight: 700; color: {{COLORS$text_light}}; letter-spacing: .04em; }
    .tooltip-value { font-size: 13px; font-weight: 600; color: {{COLORS$text}}; }
    #floating_tooltip hr { border: 0; border-top: 1px solid #e9ecef; margin: 10px 0; }
    .tooltip-hint { font-size: 12px; color: {{COLORS$text_muted}}; font-weight: 500; }
  )",
  .open = "{{", .close = "}}"
)

# Floating hover tooltip
app_js <- r"(
  $(document).on('mouseenter', '.effect-cell', function() {
    var cell = $(this);
    var tooltip = $('#floating_tooltip');

    $('#tooltip_effect').text(cell.attr('data-effect')).css('color', cell.attr('data-effect-color'));
    $('#tooltip_estimate').text(cell.attr('data-estimate')).css('color', '#000000');
    $('#tooltip_certainty').text(cell.attr('data-certainty')).css('color', cell.attr('data-certainty-color'));

    var rect = this.getBoundingClientRect();
    tooltip.css({ display: 'block', visibility: 'hidden' });

    var tooltipWidth  = tooltip.outerWidth();
    var tooltipHeight = tooltip.outerHeight();

    var left = rect.left + (rect.width / 2) - (tooltipWidth / 2);
    var top  = rect.top - tooltipHeight - 10;

    left = Math.max(10, left);
    left = Math.min(left, window.innerWidth - tooltipWidth - 10);
    if (top < 10) top = rect.bottom + 10;  // not enough room above -> show below

    tooltip.css({ left: left + 'px', top: top + 'px', visibility: 'visible' });
  });

  $(document).on('mouseleave', '.effect-cell', function() { $('#floating_tooltip').hide(); });
  $(document).on('click', '.effect-cell', function() { $('#floating_tooltip').hide(); });
  $(window).on('scroll resize', function() { $('#floating_tooltip').hide(); });
)"

tooltip_row <- function(label, value_id) {
  tags$div(
    class = "tooltip-row",
    tags$span(class = "tooltip-label", label),
    tags$span(id = value_id, class = "tooltip-value")
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML(app_css)),
    tags$script(HTML(app_js))
  ),

  # floating hover tooltip shell (filled in by app_js on mouseenter)
  tags$div(
    id = "floating_tooltip",
    tooltip_row("EFFECT", "tooltip_effect"),
    tooltip_row("ESTIMATE", "tooltip_estimate"),
    tooltip_row("CERTAINTY", "tooltip_certainty"),
    tags$hr(),
    tags$div(class = "tooltip-hint", "Click within the cell for more details")
  ),
  div(class = "app-title", "Suicide Prevention: Overview of Reviews"),
  div(class = "app-subtitle", "Intervention benefits/harms by outcome domain, with GRADE certainty of evidence."),
  div(class = "app-shared-note", "Hover a cell for a quick summary; click for full details."),
  tabsetPanel(
    tabPanel(
      "Are interventions beneficial?",
      main_legend,
      div(class = "table-scroll", reactableOutput("gapmap_table_main"))
    ),
    tabPanel(
      "Does effectiveness vary across groups?",
      subgroup_legend,
      div(class = "table-scroll", reactableOutput("gapmap_table_subgroup"))
    )
  ),
  conditionalPanel(
    condition = "output.sidebar_visible",
    div(
      id = "detail_sidebar",
      actionButton("close_sidebar", "\u2715", style = "float:right;border:none;background:none;font-size:16px;"),
      uiOutput("detail_panel")
    )
  ),
  div(
    class = "app-disclaimer",
    "The evidence reported here should be interpreted cautiously. We did not validate included reviews' analyses ourselves or systematically evaluate the included reviews for errors. Additionally, we rated four of the five included reviews as critically low in methodological quality and at high risk of bias."
  ),
  div(
    class = "app-footer",
    "Dashboard layout inspired by the ",
    tags$a(href = "https://u-reach.org/", target = "_blank", rel = "noopener noreferrer", "U-REACH"),
    " evidence platforms for overview findings."
  )
)


# Server ----

server <- function(input, output, session) {
  selected_detail <- reactiveVal(NULL)

  output$gapmap_table_main <- renderReactable({
    reactable(
      table_df_main,
      columns = table_columns_main,
      searchable = TRUE,
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      wrap = FALSE,
      defaultPageSize = 25,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10, 25, 50, 100)
    )
  })

  output$gapmap_table_subgroup <- renderReactable({
    reactable(
      table_df_subgroup,
      columns = table_columns_subgroup,
      rowStyle = function(index) {
        if (isTRUE(table_df_subgroup$is_header[index])) {
          list(
            background = GROUP_HEADER_BACKGROUND,
            borderTop = "2px solid #000",
            borderBottom = "2px solid #000"
          )
        }
      },
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      wrap = FALSE,
      sortable = FALSE, # sorting would scatter heading rows from their sections
      searchable = FALSE, # same reason
      defaultPageSize = 100,
      showPageSizeOptions = FALSE
    )
  })

  observeEvent(input$cell_click, {
    parts <- strsplit(input$cell_click, CELL_SEP, fixed = TRUE)[[1]]
    rk <- parts[1]
    oc <- parts[2]
    detail <- prep %>%
      filter(row_key == rk, Outcome == oc) %>%
      slice(1)
    if (nrow(detail) == 1) selected_detail(detail)
  })

  observeEvent(input$close_sidebar, {
    selected_detail(NULL)
  })

  output$sidebar_visible <- reactive(!is.null(selected_detail()))
  outputOptions(output, "sidebar_visible", suspendWhenHidden = FALSE)

  detail_field <- function(label, value) {
    value <- as.character(value)
    if (is.na(value) || value == "") value <- "\u2014"
    div(
      class = "detail-field",
      div(class = "detail-label", label),
      div(class = "detail-value", value)
    )
  }

  # Everything below shows the RAW data value
  output$detail_panel <- renderUI({
    d <- selected_detail()
    req(d)

    applicability_text <- if (not_blank(d$`Applicability concerns`) && str_detect(d$`Applicability concerns`, "^None\\b")) {
      "None"
    } else {
      d$`Applicability concerns`
    }

    tagList(
      div(class = "legend-heading", "Details"),
      detail_field("Intervention", d$Intervention),
      detail_field("Outcome", d$Outcome),
      if (is_subgroup_contrast(d$Moderator)) {
        detail_field("Subgroup / moderator comparison", d$Moderator)
      } else if (is_real_subgroup(d$Moderator)) {
        detail_field("Population subgroup", d$Moderator)
      },
      if (d$row_type == "subgroup") {
        detail_field("Which group did better", d$favored_group_raw)
      },
      detail_field("Population", d$Population),
      detail_field("Time point", d$Time_raw_clean),
      detail_field("Comparator(s)", d$Comparator_clean),
      detail_field("Estimate", d$display_estimate),
      detail_field("Certainty (GRADE)", d$Certainty),
      hr(),
      div(class = "legend-heading", "GRADE domains"),
      detail_field("Study limitations", d$`Study Limitations`),
      detail_field("Inconsistency", d$Inconsistency),
      detail_field("Indirectness", d$Indirectness),
      detail_field("Imprecision", d$Imprecision),
      detail_field("Publication bias", d$`Publication Bias`),
      hr(),
      detail_field("Narrative interpretation", d$`Narrative Interpretation`),
      detail_field("Applicability concerns", applicability_text),
      if (not_blank(d$`Data Collection Notes`)) detail_field("Data collection notes", d$`Data Collection Notes`),
      if (not_blank(d$pdf_link)) {
        div(
          class = "detail-field",
          div(class = "detail-label", "Review Source"),
          tags$a(href = d$pdf_link, target = "_blank", rel = "noopener noreferrer", paste0(d$review_name, " \u2197"))
        )
      },
      if (DEV_MODE) {
        tagList(
          detail_field("Reference ID", d$RefID),
          detail_field("Estimate ID", d$EstimateID)
        )
      }
    )
  })
}


# Run app ----

shinyApp(ui, server)
