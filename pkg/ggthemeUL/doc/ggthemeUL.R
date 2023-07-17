## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(ggplot2)
library(ggthemeUL)

## ---- echo=FALSE, results='asis'----------------------------------------------
primaryColors <- c(
  `red`     = "#E03127",
  `antracit` = "#58595b",
  `medium`     = "#A7A8AA",
  `lajt`    = "#E8E9EA"
)

for (i in seq_along(primaryColors)) {
  cat('<div style="width: 100px; height: 100px; background-color:', primaryColors[i], 
      '; display: flex; justify-content: center; align-items: center; color: white; margin: 10px; display: inline-block;">', 
      names(primaryColors)[i], "<br>", primaryColors[i], '</div>')
}

## ----echo=FALSE, results='asis'-----------------------------------------------
coldColors <- c(
  `darkblue`   = "#0033a0",
  `navyblue`   = "#0082C0",
  `turquoise`   = "#00B1AC",
  `green`   = "#00694E"
)

for (i in seq_along(coldColors)) {
  cat('<div style="width: 100px; height: 100px; background-color:', coldColors[i], 
      '; display: flex; justify-content: center; align-items: center; color: white; margin: 10px; display: inline-block;">', 
      names(coldColors)[i], "<br>", coldColors[i],  '</div>')
}

## ---- echo=FALSE, results='asis'----------------------------------------------
warmColors <- c(
  `yellow`   = "#EACE12",
  `orange`   = "#CB511C",
  `burgundy`   = "#9A2F31",
  `pink`   = "#C43788"
)

for (i in seq_along(warmColors)) {
  cat('<div style="width: 100px; height: 100px; background-color:', warmColors[i], 
      '; display: flex; justify-content: center; align-items: center; color: white; margin: 10px; display: inline-block;">', 
      names(warmColors)[i],"<br>", warmColors[i],  '</div>')
}

## -----------------------------------------------------------------------------
ul_color("lajt")

## ----include=FALSE------------------------------------------------------------
set.seed(1)  
n <- 100 
age <- rnorm(n, mean = 35, sd = 5)
height <- rnorm(n, mean = 150 + 0.1 * age, sd = 5)
weight <- rnorm(n, mean = 100 + 0.5 * height - 1 * age, sd = 5)/2

SatisfactionLevels <- c("Strongly Disagree", "Disagree", "Agree", "Strongly Agree")
SatisfactionLevelsWithNeutral <- c("Strongly Disagree", "Disagree", "Neither", "Agree", "Strongly Agree")
df <- data.frame(
  respondent_id = 1:n,
  country = sample(x = c("Slovenia", "Croatia", "Austria", "France"), n, replace = TRUE),
  gender = sample(x = c("Male", "Female"), n, replace = TRUE),
  height = height,
  weight = weight,
  age = age,
  satisfaction = sample(SatisfactionLevels, n, replace = TRUE),
  satisfactionWithNeutral = sample(SatisfactionLevelsWithNeutral, n, replace = TRUE)
)
df$satisfaction <- factor(df$satisfaction, levels = SatisfactionLevels)
df$satisfactionWithNeutral <- factor(df$satisfactionWithNeutral, levels = SatisfactionLevelsWithNeutral)

## ----fig.height=5, fig.show='hold', fig.width=7-------------------------------
basicChart <- ggplot(df, aes(x = country)) +  
  geom_bar() + 
  geom_hline(yintercept = 5) +
  labs(x = element_blank(),        
       y = "Frequency",        
       title = "Lorem ipsum dolor sit amet",        
       caption = "Data source: this is all fake data.",        
       subtitle = "Eiusmod tempor incididunt ut labore et dolore magna.")

basicChart

basicChart +
  geom_bar(fill = ul_color("navyblue")) +   
  geom_hline(yintercept = 5, color = ul_color("red")) +
  theme_ul(plot.background.fill =  "#E8E9EA")

## ----fig.height=5, fig.show='hold', fig.width=7-------------------------------
basicChart +
  geom_bar(fill = ul_color("navyblue")) +   
  geom_hline(yintercept = 5, color = ul_color("red")) +
  theme_ul(plot.background.fill =  "#E8E9EA") +
  theme(plot.title = element_text(color="red"))

## ----fig.height=5, fig.show='hold', fig.width=7-------------------------------
basicChart <- ggplot(df, aes(x = gender, y = height, fill = country)) +
  geom_boxplot() +
  labs(y = "Sentiment", x = element_blank()) +
  theme_ul(legend.justification = c(0, 1)) +
  labs(x = element_blank(),        
       y = "Height",        
       title = "Lorem ipsum dolor sit amet",        
       caption = "Data source: this is all fake data.",        
       subtitle = "Eiusmod tempor incididunt ut labore et dolore magna.",
       fill = "Country") 

basicChart + scale_fill_ul() 
basicChart + scale_fill_ul("cold") 

## ----fig.height=5, fig.show='hold', fig.width=7-------------------------------
basicChart + scale_fill_ul("navyblue", reverse = TRUE) 

## ----fig.height=5, fig.show='hold', fig.width=7-------------------------------
basicChartCont <- ggplot(df, aes(x = age, y = height, color = weight)) +   
  geom_point(size = 5) +   
  theme_ul(legend.justification = c(0, 1)) +
  labs(x = "Age",        
       y = "Height",        
       title = "Lorem ipsum dolor sit amet",        
       caption = "Data source: this is all fake data.",        
       subtitle = "Eiusmod tempor incididunt ut labore et dolore magna.",
       color = "Weight") 

basicChartCont + scale_color_ul(palette = "navyblue", discrete = FALSE)
basicChartCont + scale_color_ul(palette = "navyblue", discrete = FALSE, values = c(0, 0.8, 1))

## ----fig.height=5, fig.show='hold', fig.width=7-------------------------------
ggplot(df, aes(y = country, fill = satisfaction)) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  scale_fill_ul("redGreen") +
  theme_ul() +
    labs(x = element_blank(),        
       y = element_blank(),        
       title = "Lorem ipsum dolor sit amet",        
       caption = "Data source: this is all fake data.",        
       subtitle = "Eiusmod tempor incididunt ut labore et dolore magna.",
       fill = "Level of Agreement") 

## ----fig.height=5, fig.show='hold', fig.width=8-------------------------------
basicChart <- ggplot(df, aes(y = country, fill = satisfactionWithNeutral)) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  guides(fill = guide_legend(nrow = 1)) +
  theme_ul() +
  labs(fill = element_blank()) +
  theme_ul(plot.background.fill = ul_color("lajt")) +
      labs(x = element_blank(),        
       y = element_blank(),        
       title = "Lorem ipsum dolor sit amet",        
       caption = "Data source: this is all fake data.",        
       subtitle = "Eiusmod tempor incididunt ut labore et dolore magna.",
       fill = "Level of Agreement") 

basicChart + scale_fill_ul("redGreen")

## ----fig.height=5, fig.show='hold', fig.width=8-------------------------------
ggplot(df, aes(y = country, fill = satisfactionWithNeutral)) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_ul("redGreen", neutralColor = "lajt") +   
  theme_ul(legend.key = element_rect(color = ul_color("antracit"), 
                                     fill = "transparent"), 
           plot.background.fill = ul_color("lajt")) +
  labs(fill = element_blank()) +
      labs(x = element_blank(),        
       y = element_blank(),        
       title = "Lorem ipsum dolor sit amet",        
       caption = "Data source: this is all fake data.",        
       subtitle = "Eiusmod tempor incididunt ut labore et dolore magna.",
       fill = "Level of Agreement") 

ggplot(df, aes(y = country, fill = satisfactionWithNeutral)) +
  geom_bar(position = position_fill(reverse = TRUE), color = ul_color("antracit")) +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_ul("redGreen", neutralColor = "lajt") +   
  theme_ul(plot.background.fill = ul_color("lajt")) +
  labs(fill = element_blank()) +
      labs(x = element_blank(),        
       y = element_blank(),        
       title = "Lorem ipsum dolor sit amet",        
       caption = "Data source: this is all fake data.",        
       subtitle = "Eiusmod tempor incididunt ut labore et dolore magna.",
       fill = "Level of Agreement") 

## ----fig.height=5, fig.show='hold', fig.width=7-------------------------------
basicChartCont + scale_color_ul(palette = "redGreen", discrete = FALSE)

## ----fig.height=5, fig.show='hold', fig.width=7-------------------------------
basicChartCont + scale_color_ul(palette = "redGreen", discrete = FALSE, midpoint = 70)
basicChartCont + scale_color_ul(palette = "redGreen", 
                            discrete = FALSE, 
                            values = c(0, 
                                       scales::rescale(70, to = c(0, 1), from = range(df$weight)),
                                       1))

