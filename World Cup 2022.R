##### World Cup 2022#####
## By: Stephan Teodosescu
## Updated Novemeber 2022

library(tidyverse)
library(gt) #for 538-themed tables
library(gtExtras)
library(extrafont) #for adding in new fonts
library(ggtext) #for sprucing up ggplot graphics using HTML and CSS stylings
library(ggsci)
library(RCurl)
library(magick) 
library(ggimage) #for working with logos
library(glue)
library(zoo)
library(scales)
library(googlesheets4)
library(ggalt) #for dumbbell plot
library(viridis)
library(ggrepel)
library(janitor)

# Read in Googlesheets data

probs <- read_sheet("https://docs.google.com/spreadsheets/d/1FgoKUOV2G9AgZxiEZi2xiev_xrLyqmn7VQfsarG_bEE/edit#gid=933173366")
probs <- probs[-(33:37),] # removing bottom rows from spreadsheet as it's mostly just metadata

# if above doesn't work due to authentication here is the downloaded version - read in with readr
probs <- read_csv("https://raw.githubusercontent.com/steodose/BlogPosts/master/World%20Cup%202022/Men's%20World%20Cup%202022%20Predictions%20-%20Summary.csv", col_types = cols(`Total avg.`= col_number(),
                                                                                                                                                                                `Total med.`= col_number(),
                                                                                                                                                                                `Total max.`= col_number(),
                                                                                                                                                                                `Total min.`= col_number()
                                                                                                                                                                                )) |> 
    clean_names()

probs <- probs[-(33:37),] # removing bottom rows from spreadsheet as it's mostly just metadata


# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Outfit") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'transparent', color = "transparent")
        )
}


# Define 538 table theme for Reactable table(s) below
theme_538 <- function() {
    reactable::reactableTheme(
        searchInputStyle = list(width = "31%", backgroundColor = "#F9F9F9"),
        style = list(
            fontFamily = "Outfit"
        ),
        headerStyle = list(
            "&:hover[aria-sort]" = list(
                background = "hsl(0, 0%, 80%)"),
            "&[aria-sort='ascending'], &[aria-sort='descending']" = list(
                background = "#555",
                color = "#FFF"
            ),
            borderColor = "#333"
        ),
        borderColor = "#CDCDCD"
    )
}

##### Make GT rankings table #####

# clean data
probs2 <- probs %>% 
    select(country:total_min) %>%
    mutate('logo' = paste0('flags/', country, '.png'),
           'rank' = row_number()) %>% 
    select(rank, logo, everything())

# Make GT table
gt_tbl <- gt(probs2) %>%
    ### Round Numbers
    fmt_number(columns = 4:7, decimals = 1, sep_mark = '') %>% 
    opt_table_font(
        font = list(
            google_font("Otufit"),
            default_fonts()
        )
    ) %>%
    tab_options(
        column_labels.background.color = "white",
        table.border.top.width = px(3),
        table.border.top.color = "transparent",
        table.border.bottom.color = "transparent",
        table.border.bottom.width = px(3),
        column_labels.border.top.width = px(3),
        column_labels.border.top.color = "transparent",
        column_labels.border.bottom.width = px(3),
        column_labels.border.bottom.color = "black",
        data_row.padding = px(3),
        source_notes.font.size = 12,
        table.font.size = 16,
        heading.align = "left",
    ) %>%
    ### Names
    cols_label(
        logo = '',
        total_avg = 'Average Win (%)',
        total_med = 'Median Win (%)',
        total_max = 'Maximum Win (%)',
        total_min = 'Minimum Win (%)'
    ) %>% 
    ### Logos
    gt_img_rows(logo, img_source = "local", height = 30) %>%
    ### Colors
    data_color(columns = 4:7,
               colors = scales::col_numeric(
                   palette = c("white", "#3fc1c9"),
                   domain = NULL)) %>%
    cols_align(align = "left",
               columns = 1) %>%
    tab_header(title = md("**Forecasting World Cup 2022**"),
               subtitle = glue("Probabilities of each country winning the FIFA World Cup, according to an ensemble of forecasts collated by Jan Van Haaren.")) %>%
    tab_source_note(
        source_note = md("DATA: Jan Van Haaren - KU Leuven<br>TABLE: @steodosescu"))

gt_tbl #Display table in the Rstudio viewer

#Save table in working directory
gtsave(gt_tbl, filename = 'WC_forecasts_2022.png')



