# 1.0 Tools ----
library(tidyverse)
library(jsonlite)
library(hexbin)
library(httr)
library(prismatic)
library(cowplot)


#source script
if(!exists("court_themes", mode="function")) source("Plot_themes.R")
if(!exists("plot_court", mode="function")) source("court_plot.R")
if(!exists("find_player_id_by_name", mode="function")) source("get_players_data.R")
if(!exists("get_shots_by_player_id_and_season", mode="function")) source("get_shot_data_api.R")
if(!exists("generate_hex_chart", mode="function")) source("hex_court_plot.R")


#function
fraction_to_percent_format = function(frac, digits = 1) {
  paste0(format(round(frac * 100, digits), nsmall = digits), "%")
}

percent_formatter = function(x) {
  scales::percent(x, accuracy = 1)
}

points_formatter = function(x) {
  scales::comma(x, accuracy = 0.01)
}


# 2.0 Get shot Data ----
MJ_data<- find_player_by_name('Michael Jordan')
MJ_id<- find_player_id_by_name('Michael Jordan')

Mj_shot_data<- get_shots_by_player_id_and_season(player_id = MJ_id, 
                                                 season ='1996-97', 
                                                 season_type = 'Regular Season')$player

league_shot_avg<-get_shots_by_player_id_and_season(player_id = MJ_id, 
                                                   season ='1996-97', 
                                                   season_type = 'Regular Season')$league_averages

simplified_areas = c("Center(C)" = "Center",
                     "Left Side(L)" = "Left",
                     "Left Side Center(LC)" = "Left",
                     "Right Side(R)" = "Right",
                     "Right Side Center(RC)" = "Right",
                     "Back Court(BC)" = "Backcourt"
                    )

Mj_shot_data$simplified_shot_areas<- simplified_areas[Mj_shot_data$shot_zone_area]


# 3.0 Shot analysis ----

#plot function
generate_heatmap_chart = function(shots, base_court, court_theme = court_themes$dark) {
  base_court +
    stat_density_2d(
      data = shots,
      aes(x = loc_x, y = loc_y, fill = stat(density / max(density))),
      geom = "raster", contour = FALSE, interpolate = TRUE, n = 200
    ) +
    geom_path(
      data = court_points,
      aes(x = x, y = y, group = desc),
      color = court_theme$lines
    ) +
    scale_fill_viridis_c(
      "Shot Frequency    ",
      limits = c(0, 1),
      breaks = c(0, 1),
      labels = c("lower", "higher"),
      option = "inferno",
      guide = guide_colorbar(barwidth = 15)
    ) +
    theme(legend.text = element_text(size = rel(0.6)))
}
generate_scatter_chart = function(shots, base_court, court_theme = court_themes$dark, alpha = 0.8, size = 2.5) {
  base_court +
    geom_point(
      data = shots,
      aes(x = loc_x, y = loc_y, color = shot_made_flag),
      alpha = alpha, size = size
    ) +
    scale_color_manual(
      "",
      values = c(made = court_theme$made, missed = court_theme$missed)
    )
}


Mj_shot_density<- generate_heatmap_chart(Mj_shot_data, plot_court())
Mj_shot_density + labs(title = 'Michael Jordan shot density', subtitle = 'Season 1995-1996')


Mj_hex_data<- calculate_hexbins_from_shots(shots = Mj_shot_data, league_averages = league_shot_avg, binwidths = c(1,1))
Mj_hex_plot<- generate_hex_chart(Mj_hex_data, plot_court(court_themes$dark), court_theme = court_themes$dark, metric = sym("bounded_fg_diff"))

Mj_hex_plot + labs(title = 'Michael Jordan field goal', subtitle = 'Season 1995-1996') +
  guides(fill=guide_legend(
    label.position = 'bottom', 
    title.position = 'top', 
    keywidth=.45,
    keyheight=.15, 
    default.unit="inch", 
    title.hjust = .5,
    title.vjust = 0,
    label.vjust = 3,
    nrow = 1))  +
  theme(text=element_text(size=14), 
        legend.spacing.x = unit(0, 'cm'), 
        legend.title=element_text(size=12, colour = 'white'), 
        legend.text = element_text(size = rel(0.6), colour = 'white'), 
        legend.margin=margin(-10,0,-1,0),
        legend.position = 'bottom',
        legend.box.margin=margin(-30,0,15,0), 
        plot.title = element_text(hjust = 0.5, vjust = -1, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 9, vjust = -.5), 
        plot.caption = element_text(face = "italic", size = 8), 
        plot.margin = margin(0, -5, 0, -5, "cm")) 

generate_hex_chart(Mj_hex_data, plot_court(), metric = sym("bounded_points_per_shot"))

generate_scatter_chart(Mj_shot_data, plot_court())
