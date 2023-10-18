
library(tidyverse)
library(colorspace)
library(patchwork)
library(broom)
library(DT)
library(palmerpenguins)
library(nullabor)

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
                "3:30-3:50", "What is your plot testing?",
                "3:50-4:10", "Creating null samples",
                "4:10-4:40", "Conducting a lineup test",
                "4:40-5:00", "Testing for best plot design")
knitr::kable(plan)


#| eval: false
#| echo: true
## LM_FIT <- lm(VAR2 ~ VAR1,
##              data = DATA)
## FIT_ALL <- augment(LM_FIT)
## ggplot(FIT_ALL, aes(x=.FITTED,
##                     y=.RESID)) +
##   geom_point()


#| fig-width: 4
#| fig-height: 4
cars_lm <- lm(dist ~ speed, data = cars)
cars_all <- augment(cars_lm)
ggplot(cars_all, aes(x=.fitted, y=.resid)) + geom_point()


#| eval: false
#| echo: true
## ggplot(DATA,
##        aes(x=VAR1,
##            y=VAR2,
##            color=CLASS)) +
##   geom_point()


#| fig-width: 4
#| fig-height: 4.5
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


#| eval: false
#| echo: true
## LM_FIT <- lm(VAR2 ~ VAR1,
##              data = DATA)
## FIT_ALL <- augment(LM_FIT)
## ggplot(FIT_ALL, aes(x=.FITTED,
##                     y=.RESID)) +
##   geom_point()


#| eval: false
#| echo: true
## ggplot(DATA,
##        aes(x=VAR1,
##            y=VAR2,
##            color=CLASS)) +
##   geom_point()


#| eval: false
#| echo: true
## ggplot(DATA,
##        aes(x=VAR1)) +
##   geom_histogram()


#| eval: false
#| echo: true
## ggplot(DATA,
##        aes(x=VAR1,
##            fill=VAR2)) +
##   geom_bar(position="fill")


#| eval: false
#| echo: true
## ggplot(DATA,
##        aes(x=VAR1,
##            y=VAR2)) +
##   geom_point() +
##   geom_smooth()


#| fig-width: 3
#| fig-height: 2.6
#| out-width: 70%
t_df <- tibble(x=seq(-4, 4, 0.1), y=dt(x, df=5))
t_cr1 <- tibble(x=c(-4, seq(-4, -2.57, 0.05), -2.57),
                y=c(0, dt(x[-c(1,31)], df=5), 0))
t_cr2 <- tibble(x=c(2.57, seq(2.57, 4, 0.05), 4),
                y=c(0, dt(x[-c(1,31)], df=5), 0))
ggplot() +
  geom_hline(yintercept = 0) +
  geom_line(data=t_df, aes(x=x, y=y)) +
  geom_polygon(data=t_cr1, aes(x=x, y=y), fill="#F5191C", alpha=0.8) +
  geom_polygon(data=t_cr2, aes(x=x, y=y), fill="#F5191C", alpha=0.8) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank())



#| eval: false
#| echo: true
## ggplot(DATA,
##        aes(x=VAR1,
##            y=VAR2,
##            color=CLASS)) +
##   geom_point()


#| fig-width: 4
#| fig-height: 8
#| out-width: 60%
p1 <- ggplot(penguins,
       aes(x=flipper_length_mm,
           y=bill_length_mm,
           color=species)) +
  geom_point(alpha=0.8) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  ggtitle("Original") +
  theme(legend.position = "none")
set.seed(235)
p2 <- ggplot(penguins,
       aes(x=flipper_length_mm,
           y=bill_length_mm,
           color=sample(species, replace = TRUE))) +
  geom_point(alpha=0.8) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  ggtitle("Permuted") +
  theme(legend.position = "none")
p1/p2


#| eval: false
#| echo: true
## LM_FIT <- lm(VAR2 ~ VAR1,
##              data = DATA)
## FIT_ALL <- augment(LM_FIT)
## ggplot(FIT_ALL, aes(x=.FITTED,
##                     y=.RESID)) +
##   geom_point()


#| fig-width: 3
#| fig-height: 5.5
#| out-width: 60%
cars_lm <- lm(dist ~ speed, data = cars)
cars_all <- augment(cars_lm)
set.seed(1235)
ggplot(lineup(null_lm(dist ~ speed, method="rotate"),
                    cars_all, n=2, pos=1), aes(x=.fitted, y=.resid)) +
  geom_point() +
  facet_wrap(~.sample, ncol=1) +
  theme(axis.title = element_blank(),
        axis.text = element_blank())


#| code-fold: true
#| echo: true
#| fig-width: 12
#| fig-height: 8
#| out.width: 80%
set.seed(241)
ggplot(lineup(null_permute("species"), penguins, n=15),
       aes(x=flipper_length_mm,
           y=bill_length_mm,
           color=species)) +
  geom_point(alpha=0.8) +
  facet_wrap(~.sample, ncol=5) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank())


#| echo: true
pvisual(10, 10, 15)


#| code-fold: true
#| echo: true
#| fig-width: 9
#| fig-height: 6
#| out.width: 80%
data(wasps)
set.seed(258)
wasps_l <- lineup(null_permute("Group"), wasps[,-1], n=15)
wasps_l <- wasps_l %>%
  mutate(LD1 = NA, LD2 = NA)
for (i in unique(wasps_l$.sample)) {
  x <- filter(wasps_l, .sample == i)
  xlda <- MASS::lda(Group~., data=x[,1:42])
  xp <- MASS:::predict.lda(xlda, x, dimen=2)$x
  wasps_l$LD1[wasps_l$.sample == i] <- xp[,1]
  wasps_l$LD2[wasps_l$.sample == i] <- xp[,2]
}
ggplot(wasps_l,
       aes(x=LD1,
           y=LD2,
           color=Group)) +
  geom_point(alpha=0.8) +
  facet_wrap(~.sample, ncol=5) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.grid.major = element_blank())


#| echo: true
pvisual(1, 10, 15)


#| label: coin
head <- '<img src="images/heads.jpg" height = "70px" style="vertical-align:middle;">'
tail <- '<img src="images/tails.jpg" height = "70px" style="vertical-align:middle;">'



set.seed(924)
samp10 <- sample(rep(c(head, tail), c(7, 3)))
cat(paste0(samp10, collapse = ""))



samp100 <- sample(rep(c(head, tail), c(70, 30)))
cat(paste0(samp100[1:42], collapse = ""))



sum(dbinom(7:10, 10, 0.5))



sum(dbinom(70:100, 100, 0.5))


#| eval: false
#| echo: true
## nullabor::pvisual()


#| fig-width: 8
#| fig-height: 5.5
#| out-width: 90%
library(dslabs)
set.seed(357)
ggplot(lineup(null_dist("temp", "exp",
                        list(rate = 1 / mean(dslabs::stars$temp))),
                        stars, n=15),
       aes(x=temp)) +
  geom_density(fill="black", alpha=0.7) +
  facet_wrap(~.sample, ncol=5, scales="free") +
  theme(axis.title = element_blank(),
        axis.text = element_blank())


#| eval: false
#| echo: true
## ggplot(stars, aes(x=temp)) +
##   geom_density()


#| eval: false
#| echo: true
## lineup(null_dist("temp", "exp",
##   list(rate = 1 /
##          mean(dslabs::stars$temp))),
##   stars, n=15)


#| eval: false
#| echo: true
## pvisual(n=??, k=??, m=15)



tb <- read_csv(here::here("data/TB_notifications_2023-08-21.csv"))
tb_ury <- tb %>%
  filter(iso3 == "URY") %>%
  filter(year > 1995)
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


#| fig-width: 8
#| fig-height: 5.5
#| out-width: 90%
l1 <- ggplot(tb_ury_sa,
       aes(x=year,
           y=count,
           fill=sex)) +
  geom_col(position="fill") +
  facet_wrap(~age, ncol=7) +
  scale_fill_discrete_divergingx(palette="ArmyRose") +
  theme(legend.position="none",
        axis.title = element_blank(),
        axis.text = element_blank())
set.seed(446)
l2 <- tb_ury_sa %>%
  uncount(count) %>%
  group_by(age) %>%
  mutate(sex = sample(sex, replace = FALSE)) %>%
  count(sex, age, year) %>%
  ggplot(aes(x=year,
             y=n,
             fill=sex)) +
  geom_col(position="fill") +
  facet_wrap(~age, ncol=7) +
  scale_fill_discrete_divergingx(palette="ArmyRose") +
  theme(legend.position="none",
        axis.title = element_blank(),
        axis.text = element_blank())
l3 <- tb_ury_sa %>%
  uncount(count) %>%
  group_by(age) %>%
  mutate(sex = sample(sex, replace = FALSE)) %>%
  count(sex, age, year) %>%
  ggplot(aes(x=year,
             y=n,
             fill=sex)) +
  geom_col(position="fill") +
  facet_wrap(~age, ncol=7) +
  scale_fill_discrete_divergingx(palette="ArmyRose") +
  theme(legend.position="none",
        axis.title = element_blank(),
        axis.text = element_blank())
l1 + l2 + l3 + plot_layout(ncol=1)


#| eval: false
#| echo: true
## ggplot(tb_ury_sa,
##        aes(x=year,
##            y=count,
##            fill=sex)) +
##   geom_col(position="fill") +
##   facet_wrap(~age, ncol=7)


#| eval: false
#| echo: true
## b_ury_sa %>%
##   uncount(count) %>%
##   group_by(age) %>%
##   mutate(sex = sample(sex,
##     replace = FALSE)) %>%
##   count(sex, age, year)


#| eval: false
#| echo: true
## pvisual(n=??, k=??, m=3)


#| eval: false
#| echo: true
## ggplot(lineup(null_permute('mpg'), mtcars), aes(mpg, wt)) +
##   geom_point() +
##   facet_wrap(~ .sample)


#| fig-width: 8
#| fig-height: 6
#| out-width: 80%
set.seed(601)
ggplot(lineup(null_permute("year"), tb_ury, n=15),
       aes(x=year, y=c_newinc)) +
  geom_point() +
  geom_smooth(se=F, colour="#F5191C") +
  facet_wrap(~.sample, ncol=5) +
  theme(axis.title = element_blank(),
        axis.text = element_blank())


#| fig-width: 8
#| fig-height: 5
#| out-width: 90%
set.seed(601)
ggplot(lineup(null_permute("year"), tb_ury, n=15),
       aes(x=year, y=c_newinc)) +
  geom_col(fill="#F5191C") +
  facet_wrap(~.sample, ncol=5) +
  theme(axis.title = element_blank(),
        axis.text = element_blank())


#| message: false
#| warning: false
#| fig-width: 6
#| fig-align: center
#| out-width: 60%
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
## ?? / ??


#| eval: true
#| fig-width: 8
#| fig-height: 6
library(StatsBombR)
library(SBpitch)
load("data/aus_brazil.rda")
passes = aus_brazil %>%
  filter(type.name=="Pass" & is.na(pass.outcome.name))
#create_Pitch() +
set.seed(622)
ggplot(data=lineup(null_permute("possession_team.name"),
                         passes, n=12),
             aes(x=location.x,
                 y=location.y,
                 colour=possession_team.name))  +
  geom_point(alpha=0.7) +
  scale_color_discrete_divergingx(palette="Temps", nmax=2, rev=TRUE) +
  facet_wrap(~.sample, ncol=4) +
  coord_equal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank())


#| eval: true
#| fig-width: 8
#| fig-height: 6
library(StatsBombR)
library(SBpitch)
load("data/aus_brazil.rda")
passes = aus_brazil %>%
  filter(type.name=="Pass" & is.na(pass.outcome.name))
#create_Pitch() +
set.seed(622)
ggplot(data=lineup(null_permute("possession_team.name"),
                         passes, n=12),
             aes(x=location.x,
                 y=location.y,
                 colour=possession_team.name))  +
  geom_density2d(alpha=0.7) +
  scale_color_discrete_divergingx(palette="Temps", nmax=2, rev=TRUE) +
  facet_wrap(~.sample, ncol=4) +
  coord_equal() +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text = element_blank())


#| message: false
#| warning: false
#| fig-width: 8
#| fig-height: 3
#| fig-align: center
#| out-width: 60%
p1 <- create_Pitch() +
  geom_point(data=passes,
             aes(x=location.x,
                 y=location.y,
                 colour=possession_team.name)) +
  scale_color_discrete_divergingx(palette="Temps", nmax=2, rev=TRUE) +
  coord_equal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size="8"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  ggtitle("Plot A")
p2 <- create_Pitch() +
  geom_density2d(data=passes,
             aes(x=location.x,
                 y=location.y,
                 colour=possession_team.name)) +
  scale_color_discrete_divergingx(palette="Temps", nmax=2, rev=TRUE) +
  coord_equal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size="8"),
        axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  ggtitle("Plot B")

p1 + p2


#| eval: false
#| echo: true
## ?? / ??


#| eval: false
#| echo: true
## ggplot(stars, aes(x=temp)) +
##   geom_density()

set.seed(357)
ggplot(lineup(null_dist("temp", "exp",
                        list(rate = 1 / mean(dslabs::stars$temp))),
              stars, n=15),
       aes(x=temp)) +
  geom_density(fill="black", alpha=0.7) +
  facet_wrap(~.sample, ncol=5, scales="free") +
  theme(axis.title = element_blank(),
        axis.text = element_blank())

set.seed(357)
ggplot(lineup(null_dist("temp", "exp",
                        list(rate = 1 / mean(dslabs::stars$temp))),
              stars, n=15),
       aes(x=temp)) +
  geom_histogram(fill="black", binwidth=2000) +
  facet_wrap(~.sample, ncol=5, scales="free") +
  theme(axis.title = element_blank(),
        axis.text = element_blank())

set.seed(357)
ggplot(lineup(null_dist("temp", "exp",
                        list(rate = 1 / mean(dslabs::stars$temp))),
              stars, n=15),
       aes(x=1, y=temp)) +
  ggbeeswarm::geom_quasirandom() +
  facet_wrap(~.sample, ncol=5, scales="free") +
  theme(axis.title = element_blank(),
        axis.text = element_blank())
