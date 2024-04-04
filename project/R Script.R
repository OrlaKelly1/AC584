install.packages("tidyverse")
library("tidyverse")
install.packages("plotly")
library("plotly")
library(readr)
unicef_metadata <- read_csv("unicef_metadata.csv", 
                            col_types = cols(year = col_number(), 
                                             `Population, total` = col_number(), 
                                             `GDP per capita (constant 2015 US$)` = col_number()))
View(unicef_metadata)
library(readr)
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv", 
                               col_types = cols(time_period = col_number(), 
                                                obs_value = col_number()))
View(unicef_indicator_1)

#data joining
data_join <- full_join(unicef_metadata, unicef_indicator_1, by = c("country" = "country", "year" = "time_period"))

data_join <- unicef_metadata %>%
  full_join(unicef_indicator_1, by = c("country" = "country", "year" = "time_period"))

#map
map_world <- map_data("world")

map_data_join <- full_join(unicef_indicator_1, map_world, by = c("country" = "region"))

map_plot <- ggplot(map_data_join) +
  aes(x = long, y = lat, group = group, fill = obs_value, 
      text = paste("Country: ", country, "<br>% of children living in poverty: ", obs_value)) +
  geom_polygon() +
  labs(title = 'Childhood Poverty around the World') +
  scale_fill_gradient(low = "#006400", high = "#FF0000") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        text = element_text(family = 'serif'))   

map_plotly <- ggplotly(map_plot, tooltip = "text")
map_plotly

#timeseries
tooltip_text_ts <- paste(
  "Year: ", data_join$year, "<br>",
  "Country: ", data_join$country, "<br>",
  "GDP per capita: ", scales::dollar(data_join$`GDP per capita (constant 2015 US$)`), "<br>"
)

timeseries_plot_1 <- data_join %>%
  ggplot() +
  aes(year, `GDP per capita (constant 2015 US$)`, group = country, colour = country, 
      text = tooltip_text_ts) +
  geom_line() +
  labs(x = "Year", y = "GDP per Capita ", title = "GDP per capita by Country, 2000 - 2020 ") +
  theme_classic() +
  theme(legend.position = "none",
        text = element_text(family = 'serif'))


ggplotly(timeseries_plot_1, tooltip = "text")


#filter data for scatterplot
filtered_dataset_2020 <- data_join%>%
  filter( year== '2020')

#scatterplot_GDP&Poverty

tooltip_text_sp1 <- paste(
  "GDP per capita: ", scales::dollar(filtered_dataset_2020$`GDP per capita (constant 2015 US$)`), "<br>",
  "Children Living in Poverty: ", filtered_dataset_2020$obs_value, "<br>",
  "Country: ", filtered_dataset_2020$country
)

scatterplot_1 <- filtered_dataset_2020 %>%
  ggplot() +
  aes(`GDP per capita (constant 2015 US$)`, obs_value, colour = country, 
      text = tooltip_text_sp1) +
  geom_point(alpha = 0.65) +
  labs(x = "GDP per Capita", y = "Children Living in Poverty", 
       title = "Correlation between GDP per capita and % Children Living below the poverty line") +
  theme(legend.position = "none",
        text = element_text(family = 'serif')) +
  scale_x_continuous(limits = c(0, 20000))

ggplotly(scatterplot_1, tooltip = "text")

#scatterplot_LifeExp&Poverty

tooltip_text_sp2 <- paste(
  "Life Expectancy: ", filtered_dataset_2020$`Life expectancy at birth, total (years)`, "<br>",
  "Children Living in Poverty: ", filtered_dataset_2020$obs_value, "<br>",
  "Country: ", filtered_dataset_2020$country
)

# Calculate linear regression
lm_model <- lm(obs_value ~ `Life expectancy at birth, total (years)`, data = filtered_dataset_2020)

# Scatterplot
scatterplot_2 <- filtered_dataset_2020 %>%
  ggplot() +
  aes(x = `Life expectancy at birth, total (years)`, y = obs_value, colour = country, size = obs_value, text = tooltip_text_sp2) +
  geom_point(alpha = 0.65) +
  geom_abline(intercept = coef(lm_model)[1], slope = coef(lm_model)[2], color = "black") +  # Add regression line
  labs(x = "Life Expectancy", y = "Children Living in Poverty", 
       title = "Relationship between Life Expectancy and % Children Living below the poverty line") +
  theme(legend.position = "none", text = element_text(family = 'serif')) +
  scale_x_continuous(limits = c(50, 85))


scatterplotly_2 <- ggplotly(scatterplot_2, tooltip = "text")

scatterplotly_2

#barchart - life exp in African countries 2000-2020

summary_data_barchart <- data_join %>%
  group_by(country, year) %>%
  summarise(m_Life_expectancy = mean(`Life expectancy at birth, total (years)`, na.rm = TRUE))

african_countries <- c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cabo Verde", "Cameroon", "Central African Republic", "Chad", "Comoros", "Congo", "Cote d'Ivoire", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")

summary_data_barchart_african <- summary_data_barchart %>%
  filter(country %in% african_countries)

summary_data_barchart_african <- summary_data_barchart_african %>%
  filter(year >= 2000 & year <= 2020)

custom_palette <- scales::seq_gradient_pal(low = "#006400", high = "#90EE90")(seq(0, 1, length.out = 49))


bar_chart_1 <- ggplot(summary_data_barchart_african, aes(reorder(country, -m_Life_expectancy), m_Life_expectancy, fill = country, text = paste("Country: ", country, "<br>Mean Life Expectancy: ", m_Life_expectancy))) +
  geom_col() +
  facet_wrap(~ year) +
  labs(x = NULL, y = "Mean Life Expectancy", title = "Mean Life Expectancy in African Countries") + 
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), legend.position = "none",
        text = element_text(family = 'serif')) +
  scale_fill_manual(values = custom_palette)

plotly_chart <- ggplotly(bar_chart_1, tooltip = "text")

plotly_chart

