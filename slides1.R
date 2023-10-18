
library(tidyverse)
library(colorspace)
library(patchwork)
library(broom)
library(DT)
library(palmerpenguins)

options(width = 200)
knitr::opts_chunk$set(
  fig.width = 3,
  fig.height = 3,
  fig.align = "center",
  dev.args = list(bg = 'transparent'),
  out.width = "100%",
  fig.retina = 3,
  echo = FALSE,
  warning = FALSE,
  message = FALSE,
  cache = FALSE
)
theme_set(ggthemes::theme_gdocs(base_size = 12) +
  theme(plot.background =
        element_rect(fill = 'transparent', colour = NA),
        axis.line.x = element_line(color = "black",
                                   linetype = "solid"),
        axis.line.y = element_line(color = "black",
                                   linetype = "solid"),
        plot.title.position = "plot",
        plot.title = element_text(size = 18),
        panel.background  =
          element_rect(fill = 'transparent', colour = "black"),
        legend.background =
          element_rect(fill = 'transparent', colour = NA),
        legend.key        =
          element_rect(fill = 'transparent', colour = NA)
  )
)


#| echo: false
#| eval: false
## # divergingx_hcl(palette="Zissou 1", n=10)
## # [1] "#3B99B1" "#40ABA5" "#6FB798" "#9FC095" "#C7C98A"
## # [6] "#EABE23" "#E8A419" "#E78802" "#EA6400" "#F5191C"
## # specplot(divergingx_hcl(palette="Zissou 1", n=10))



plan <- tribble(~time, ~topic,
                "1:30-1:45", "Why, philosophy and benefits",
                "1:45-2:05", "Organising data to map variables to plots",
                "2:05-2:35", "Making a variety of plots",
                "2:35-3:00", "Do but don't, and cognitive principles",
                "3:00-3:30", "COFFEE BREAK")
knitr::kable(plan)



cars_lm <- lm(dist ~ speed, data = cars)
cars_all <- augment(cars_lm)
ggplot(cars_all, aes(x=.fitted, y=.resid)) + geom_point()


#| message: false
#| warning: false
library(StatsBombR)
library(SBpitch)
load("data/aus_brazil.rda")
shots = aus_brazil %>%
  filter(type.name=="Shot" & is.na(pass.outcome.name))
create_Pitch(grass_colour = "#FFFFFF",
             background_colour =  "#FFFFFF") +
  geom_point(data=shots, aes(x=location.x, y=location.y,
                   colour=possession_team.name))  +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  coord_equal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.background =
          element_rect(fill = 'transparent', colour = NA),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        legend.key =
          element_rect(fill = 'transparent', colour = NA))


#| label: tb-trend
#| message: false
#| warning: false
#| fig-width: 6
#| fig-align: center
tb <- read_csv(here::here("data/TB_notifications_2023-08-21.csv"))
tb_sub <- tb %>%
  filter(iso3 %in% c("AUS", "URY")) %>%
  filter(year > 1995)
ggplot(tb_sub, aes(x=year, y=c_newinc, colour=iso3)) +
  geom_point() +
  geom_smooth(se=F) +
  scale_x_continuous("Year", breaks = seq(1980, 2020, 10), labels = c("80", "90", "00", "10", "20")) +
  ylab("TB incidence") +
  scale_colour_discrete_divergingx(palette="Zissou 1") +
  facet_wrap(~iso3, ncol=2, scales="free_y") +
  theme(legend.position = "none")



#| label: ury-tb-trend
#| message: false
#| warning: false
#| fig-width: 6
#| fig-align: center
tb_ury <- tb %>%
  filter(iso3 == "URY") %>%
  filter(year > 1995)
p1 <- ggplot(tb_ury, aes(x=year, y=c_newinc)) +
  geom_point() +
  geom_smooth(se=F, colour="#F5191C") +
  scale_x_continuous("Year", breaks = seq(1980, 2020, 10), labels = c("80", "90", "00", "10", "20")) +
  ylab("TB incidence") +
  ggtitle("Plot A")

p2 <- ggplot(tb_ury, aes(x=year, y=c_newinc)) +
  geom_col(fill="#F5191C") +
  scale_x_continuous("Year", breaks = seq(1980, 2020, 10), labels = c("80", "90", "00", "10", "20")) +
  ylab("TB incidence") +
  ggtitle("Plot B")

p1 + p2


#| eval: false
#| echo: true
## install.packages("ggplot2")


#| eval: false
#| echo: true
#| code-line-numbers: false
## install.packages("tidyverse")


#| eval: false
#| echo: true
#| code-line-numbers: false
## install.packages("nullabor")


#| message: false
#| warning: false
tb_ury %>%
  select(iso3, year, newrel_m014:newrel_m65, newrel_f014:newrel_f65) %>%
  filter(year > 2012) %>%
  slice_head(n=11) %>%
  datatable(options = list(dom = 't'))


#| message: false
#| warning: false
grad <- read_csv(here::here("data/graduate-programs.csv"))
grad %>% slice_head(n=7) %>% datatable(options = list(dom = 't'))


#| message: false
#| warning: false
#| eval: false
## melbtemp <- read.fwf(here::here("data/ASN00086282.dly"),
##    c(11, 4, 2, 4, rep(c(5, 1, 1, 1), 31)), fill=T)
## datatable(melbtemp[1:12,c(1,2,3,4,seq(5,100,4))], options = list(dom = 't'))


#| eval: false
## # Montevideo airport weather
## library(rnoaa)
## stations <- ghcnd_stations()
## montevideo <- ghcnd("UYM00086580") #CARRASCO INTL
## save(montevideo, file="data/montevideo.rda")
## montevideo2 <- ghcnd("UYM00086560") # LAGUNA DE LOS PATOS INTL
## save(montevideo2, file="data/montevideo2.rda")
##


#| message: false
#| warning: false
load("data/montevideo2.rda")
datatable(montevideo2[2100:2110,c(1,2,3,4,seq(5,128,4))], options = list(dom = 't'))


#| echo: true
#| eval: false
## ggplot(mpg,
##        aes(
##          x=displ,
##          y=hwy,
##          color=class)) +
##   geom_point()


#| label: ury-tb
#| fig-height: 2.5
ggplot(tb_ury,
       aes(x=year,
           y=c_newinc)) +
  geom_point() +
  scale_x_continuous("Year",
    breaks = seq(1980, 2020, 10),
    labels = c("80", "90", "00", "10", "20")) +
  ylab("TB incidence")


#| label: ury-tb
#| echo: true
#| eval: false
#| code-summary: "Code for URY TB plot"
#| code-fold: true

## ggplot(tb_ury,
##        aes(x=year,
##            y=c_newinc)) +
##   geom_point() +
##   scale_x_continuous("Year",
##     breaks = seq(1980, 2020, 10),
##     labels = c("80", "90", "00", "10", "20")) +
##   ylab("TB incidence")


#| label: ury-tb-bar
#| fig-height: 2.5
ggplot(tb_ury, aes(x=year, y=c_newinc)) +
  geom_point() +
  geom_smooth(se=F, colour="#F5191C") +
  scale_x_continuous("Year", breaks = seq(1980, 2020, 10), labels = c("80", "90", "00", "10", "20")) +
  ylab("TB incidence")


#| label: ury-tb-bar
#| echo: true
#| eval: false
#| code-summary: "Code for URY TB plot"
#| code-fold: true

## ggplot(tb_ury, aes(x=year, y=c_newinc)) +
##   geom_point() +
##   geom_smooth(se=F, colour="#F5191C") +
##   scale_x_continuous("Year", breaks = seq(1980, 2020, 10), labels = c("80", "90", "00", "10", "20")) +
##   ylab("TB incidence")


#| label: penguins1
#| fig-height: 3.5
#| out-width: 80%
ggplot(penguins,
       aes(x=flipper_length_mm,
           y=bill_length_mm,
           color=species)) +
  geom_point(alpha=0.8) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size="8"))


#| label: penguins1
#| echo: true
#| eval: false
#| code-summary: "Code for penguins plot"
#| code-fold: true
## ggplot(penguins,
##        aes(x=flipper_length_mm,
##            y=bill_length_mm,
##            color=species)) +
##   geom_point(alpha=0.8) +
##   scale_color_discrete_divergingx(palette="Zissou 1") +
##   theme(legend.title = element_blank(),
##         legend.position = "bottom",
##         legend.direction = "horizontal",
##         legend.text = element_text(size="8"))


#| label: penguins2
#| fig-height: 3.5
#| out-width: 80%
ggplot(penguins,
       aes(x=flipper_length_mm,
           y=bill_length_mm,
           color=species)) +
  geom_density2d(alpha=0.8) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size="8"))


#| label: penguins2
#| echo: true
#| eval: false
#| code-summary: "Code for penguins plot"
#| code-fold: true

## ggplot(penguins,
##        aes(x=flipper_length_mm,
##            y=bill_length_mm,
##            color=species)) +
##   geom_density2d(alpha=0.8) +
##   scale_color_discrete_divergingx(palette="Zissou 1") +
##   theme(legend.title = element_blank(),
##         legend.position = "bottom",
##         legend.direction = "horizontal",
##         legend.text = element_text(size="8"))


#| echo: true
tb_ury_sa <- tb_ury %>%
  filter(year > 2012) %>%
  select(iso3, year,
         newrel_f014:newrel_f65,
         newrel_m014:newrel_m65) %>%
  pivot_longer(cols=newrel_f014:newrel_m65,
               names_to = "sex_age",
               values_to = "count") %>%
  filter(!is.na(count)) %>%
  separate(sex_age, into=c("stuff",
                           "sex_age")) %>%
  mutate(sex = str_sub(sex_age, 1, 1),
         age = str_sub(sex_age, 2,
                       str_length(sex_age))) %>%
  mutate(age = case_when(
    age == "014" ~ "0-14",
    age == "1524" ~ "15-24",
    age == "2534" ~ "25-34",
    age == "3544" ~ "35-44",
    age == "4554" ~ "45-54",
    age == "5564" ~ "55-64",
    age == "65" ~ "65")) %>%
  select(iso3, year, sex, age, count)



tb_ury_sa %>% datatable(options = list(dom = 't'))



tb_ury_sa %>% datatable(options = list(dom = 't'))


#| fig-height: 2.5
#| fig-width: 10
#| out-width: 100%
ggplot(tb_ury_sa,
       aes(x=year,
           y=count,
           fill=sex)) +
  geom_col(position="fill") +
  facet_wrap(~age, ncol=7) +
  ylab("") +
  scale_fill_discrete_divergingx(palette="ArmyRose") +
  scale_x_continuous("year",
    breaks = seq(2013, 2021, 2),
    labels = c("13", "15", "17", "19", "21")) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        axis.text = element_text(size="10"))


#| fig-height: 4
#| fig-width: 10
#| out-width: 80%
ggplot(tb_ury_sa,
       aes(x=year,
           y=count,
           fill=sex)) +
  geom_col() +
  facet_grid(sex~age, scales = "free_y") +
  ylab("count") +
  scale_fill_discrete_divergingx(palette="ArmyRose") +
  scale_x_continuous("year",
    breaks = seq(2013, 2021, 2),
    labels = c("13", "15", "17", "19", "21")) +
  theme(legend.position = "none",
        axis.text = element_text(size="10"))


#| fig-height: 4
#| fig-width: 10
#| out-width: 80%
ggplot(tb_ury_sa,
       aes(x=year,
           y=count,
           colour=sex)) +
  geom_point() +
  geom_smooth(se=F, alpha=0.7) +
  facet_grid(sex~age, scales = "free_y") +
  ylab("count") +
  scale_colour_discrete_divergingx(palette="ArmyRose") +
  scale_x_continuous("year",
    breaks = seq(2013, 2021, 2),
    labels = c("13", "15", "17", "19", "21")) +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.title = element_blank(),
        axis.text = element_text(size="10"))


#| eval: false
#| echo: true
#| code-line-numbers: "5"
## ggplot(tb_ury_sa,
##        aes(x=year,
##            y=count,
##            fill=sex)) +
##   geom_col(position="fill") +
##   facet_wrap(~age)


#| eval: false
#| echo: true
#| code-line-numbers: "5"
## ggplot(tb_ury_sa,
##        aes(x=year,
##            y=count,
##            fill=sex)) +
##   geom_col() +
##   facet_grid(sex~age)


#| eval: false
#| echo: true
#| code-line-numbers: "5,6"
## ggplot(tb_ury_sa,
##        aes(x=year,
##            y=count,
##            colour=sex)) +
##   geom_point() +
##   geom_smooth() +
##   facet_grid(sex~age)


#| eval: false
#| echo: true
#| code-line-numbers: "2,6"
## ggplot(tb_ury_sa,
##        aes(x=age,
##            y=count,
##            fill=sex)) +
##   geom_col(position="fill") +
##   facet_wrap(~year)


#| fig-height: 6
#| fig-width: 10
#| out-width: 100%
ggplot(tb_ury_sa,
       aes(x=age,
           y=count,
           fill=sex)) +
  geom_col(position="fill") +
  facet_wrap(~year, ncol=3) +
  ylab("proportion") +
  scale_fill_discrete_divergingx(palette="Fall") +
  theme(legend.title = element_blank(),
        axis.text = element_text(size="10"))


#| eval: false
#| echo: true
## facet_wrap(..., scales="free_y")


#| eval: false
#| echo: true
#| code-line-numbers: "6"
## ggplot(tb_sub, aes(x=year,
##                    y=c_newinc,
##                    colour=iso3)) +
##   geom_point() +
##   geom_smooth(se=F) +
##   facet_wrap(~iso3, ncol=2,
##              scales="free_y") +
##   theme(legend.position = "none")


#| eval: false
#| echo: true
#| code-line-numbers: "2,4,5,7,8,10,11,13"
## ggplot() +
##   geom_point(data = tb_ury,
##     aes(x=year, y=c_newinc),
##     colour="#F5191C") +
##   geom_point(data = tb_aus,
##     aes(x=year, y=c_newinc),
##     colour="#3B99B1") +
##   geom_smooth(data = tb_ury,
##     aes(x=year, y=c_newinc),
##     colour="#F5191C", se=F) +
##   geom_smooth(data = tb_aus,
##     aes(x=year, y=c_newinc),
##     colour="#3B99B1", se=F)



vis_spacing <- 'style="padding-left:20px;"'
vis_spacing1 <- 'style="padding-left:10px;"'


#| fig-width: 7
#| fig-height: 5
ggplot(tb_ury_sa,
       aes(x=year,
           y=count,
           colour=sex)) +
  geom_point() +
  geom_smooth(se=F) +
  facet_wrap(~age, ncol = 4) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  scale_x_continuous("year",
    breaks = seq(2013, 2021, 2),
    labels = c("13", "15", "17", "19", "21")) +
  theme(axis.text = element_text(size="10")) +
  ggtitle("Arrangement A")


#| fig-width: 7
#| fig-height: 5
ggplot(tb_ury_sa,
       aes(x = year, y = count, colour = age)) +
  geom_line() + geom_point() +
  facet_wrap(~sex, ncol = 2) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  scale_x_continuous("year",
    breaks = seq(2013, 2021, 2),
    labels = c("13", "15", "17", "19", "21")) +
  theme(axis.text = element_text(size="10")) +
  ggtitle("Arrangement B")


#| fig-width: 5
#| fig-height: 3.5
tb_ury_sa %>%
  filter(age %in% c("35-44", "45-54"),
         sex == "m") %>%
  ggplot(mapping=aes(x=year,
                 y=count)) +
  geom_point() +
  geom_smooth(aes(colour=age), se=F, method="lm") +
  facet_wrap(~age, ncol = 2) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  scale_x_continuous("year",
    breaks = seq(2013, 2021, 2),
    labels = c("13", "15", "17", "19", "21")) +
  theme(legend.position="none",
        axis.text = element_text(size="10"))



#| fig-width: 3
#| fig-height: 3
#| out-width: 60%
tb_ury_sa %>%
  filter(age %in% c("35-44", "45-54"),
         sex == "m") %>%
  ggplot(mapping=aes(x=year,
                 y=count)) +
  geom_smooth(aes(colour=age), se=F, method="lm") +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  scale_x_continuous("year",
    breaks = seq(2013, 2021, 2),
    labels = c("13", "15", "17", "19", "21")) +
  theme(legend.position="none",
        axis.text = element_text(size="10"))



#| echo: true
library(nullabor)
data(electoral)
ggplot(electoral$polls,
       aes(x=Democrat,
           y=Margin)) +
  geom_boxplot()

#| echo: false
#| eval: false
ggplot(electoral$polls,
       aes(x=1,
           y=Margin,
           colour=Democrat)) +
  geom_point()

ggplot(electoral$polls,
       aes(x=Margin,
           fill=Democrat)) +
  geom_histogram()
