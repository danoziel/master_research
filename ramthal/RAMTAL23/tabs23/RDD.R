library(dplyr)

# Mean plots ----
rd_land_2022 <- cultivated_land_2022 %>% 
  filter(acre_drip<7) %>% 
  left_join(rmtl_InOut) %>% rename(in_project=in1_out0) %>% 
  filter(!is.na(elevation)) %>% 
  mutate(elevation=ifelse(elevation=="7+","7",elevation),
         Elevation=as.numeric(elevation)) %>%
  #filter(south1_north0==1, !is.na(distance_km)) %>% 
  group_by(hh_id,in_project,Elevation) %>% 
  summarise(DI=sum(acre_drip ),
            Ir=sum(acre_ir) 
            ) %>% 
  group_by(in_project,Elevation) %>% 
  summarise(DI=mean(DI ),
            Ir=mean(Ir) 
  ) %>% 
  mutate(Elevation=ifelse(in_project==0,Elevation*-1,Elevation)) %>% 
  ungroup(
  ) # %>% filter(elevation >(-2) )


rd_land_2022 %>% 
  ggplot() +
  #geom_line(aes(x = Elevation, y = DI, group = in_project, color = as.factor(in_project)), size = 1) +
  geom_line(aes(x = Elevation, y = Ir, group = in_project, color = as.factor(in_project)), size = 1) +
  scale_color_manual(values = c("orange3", "royalblue4"), guide = "none") +
  labs(x = "Out of Project                              In Project\nElevation", y = "Acre") +  # X axis title with two rows
  theme_classic() + scale_x_continuous(breaks = seq(-7, 7, 1))
  # + facet_wrap(~season, ncol = 1)



rd_gca_2022 <- 
  df_gca %>% filter(Year==2022) %>% 
  left_join(rmtl_InOut) %>% 
  filter(!is.na(elevation)) %>% 
  mutate(elevation=ifelse(elevation=="7+","7",elevation),
         Elevation=as.numeric(elevation)) %>%
#filter(south1_north0==1, !is.na(distance_km)) %>% 
  group_by(in_project,Elevation) %>% 
  summarise(GCA=mean(gca),
            NCA=mean(nca)) %>% 
  mutate(Elevation=ifelse(in_project==0,Elevation*-1,Elevation)) %>% 
  ungroup(
  ) # %>% filter(elevation >(-2) )

# Create the line plot with custom colors for GCA and NCA
ggplot(rd_gca_2022) +
  geom_line(aes(x = Elevation, y = GCA, group = in_project, color = as.factor(in_project)), size = 1.5) +  # GCA color mapping
  scale_color_manual(values = c("orange3", "royalblue4"), 
                     guide = "none") +  # Custom colors for GCA (lines)
  geom_line(aes(x = Elevation, y = NCA, group = in_project, color = as.factor(in_project)), size = 1) +  # NCA color mapping
  labs(x = "Out of Project                              In Project\nElevation", y = "Acre") +  # X axis title with two rows
  ggtitle("Table. Mean land size by elevation", 
          subtitle = "in thick line - 'Gross Crop Area' in acre and in thin line 'Net sown area' in acre") +  # Title and subtitle
  theme_classic() + 
  scale_x_continuous(breaks = seq(-7, 7, 1))  # Customize x-axis ticks


# SpatialRDD ----

library(SpatialRDD)
library(stargazer) # easy way to make model output look more appealing (R-inline, html, or latex)
library(sf)


data(cut_off, polygon_full, polygon_treated)
library(tmap)
tm_shape(polygon_full) + tm_polygons() + 
  tm_shape(polygon_treated) + tm_polygons(col = "grey") + 
  tm_shape(cut_off) + tm_lines(col = "red")


set.seed(1088) # set a seed to make the results replicable
points_samp.sf <- sf::st_sample(polygon_full, 1000)
points_samp.sf <- sf::st_sf(points_samp.sf) # make it an sf object bc st_sample only created the geometry list-column (sfc)
points_samp.sf$id <- 1:nrow(points_samp.sf) # add a unique ID to each observation
# visualise results together with the line that represents our RDD cutoff
tm_shape(points_samp.sf) + tm_dots() + tm_shape(cut_off) + tm_lines(col = "red")


points_samp.sf$treated <- assign_treated(points_samp.sf, polygon_treated, id = "id")
# tm_shape(points_samp.sf) + tm_dots("treated", palette = "Set1") + tm_shape(cut_off) + tm_lines(col = "red")
tm_shape(points_samp.sf) + 
  tm_dots("treated", fill.scale = tm_scale("Set1")) +   # use tm_scale() for color scale
  tm_shape(cut_off) + 
  tm_lines(col = "red")


# first we define a variable for the number of "treated" and control which makes the code more readable in the future
NTr <- length(points_samp.sf$id[points_samp.sf$treated == 1])
NCo <- length(points_samp.sf$id[points_samp.sf$treated == 0])
# the treated areas get a 10 percentage point higher literacy rate
points_samp.sf$education[points_samp.sf$treated == 1] <- 0.7
points_samp.sf$education[points_samp.sf$treated == 0] <- 0.6
# and we add some noise, otherwise we would obtain regression coeffictions with no standard errors
# we draw from a normal with mean 0 and a standard devation of 0.1
points_samp.sf$education[points_samp.sf$treated == 1] <- rnorm(NTr, mean = 0, sd = .1) +
  points_samp.sf$education[points_samp.sf$treated == 1]
points_samp.sf$education[points_samp.sf$treated == 0] <- rnorm(NCo, mean = 0, sd = .1) +
  points_samp.sf$education[points_samp.sf$treated == 0]
# let's also add a placebo outcome that has no jump
points_samp.sf$placebo <- rnorm(nrow(points_samp.sf), mean = 1, sd = .25)
# visualisation split up by groups
library(ggplot2)
ggplot(points_samp.sf, aes(x = education)) + geom_histogram(binwidth = .01) + facet_grid(treated ~ .)


list(lm(education ~ treated, data = points_samp.sf),
     lm(placebo   ~ treated, data = points_samp.sf)) %>% stargazer::stargazer(type = "text")


points_samp.sf$dist2cutoff <- as.numeric(sf::st_distance(points_samp.sf, cut_off))
# tm_shape(points_samp.sf[points_samp.sf$dist2cutoff < 3000, ]) + 
#   tm_dots("education", palette = "RdYlGn", size = .1) + 
#   tm_shape(cut_off) + tm_lines()
tm_shape(points_samp.sf[points_samp.sf$dist2cutoff < 3000, ]) + 
  tm_dots("education", size = 0.3) +
  tm_shape(cut_off) + tm_lines()

lm(education ~ treated, data = points_samp.sf[points_samp.sf$dist2cutoff < 3000, ]) %>% stargazer::stargazer(type = "text")

points_samp.sf$distrunning <- points_samp.sf$dist2cutoff
# give the non-treated one's a negative score
points_samp.sf$distrunning[points_samp.sf$treated == 0] <- -1 * points_samp.sf$distrunning[points_samp.sf$treated == 0]
ggplot(data = points_samp.sf, aes(x = distrunning, y = education)) + geom_point() + geom_vline(xintercept = 0, col = "red")


library(rdrobust)
summary(rdrobust(points_samp.sf$education, points_samp.sf$distrunning, c = 0))

rdplot(points_samp.sf$education, points_samp.sf$distrunning, c = 0, ci = 95, 
       kernel = "triangular", y.label = "education", x.label = "distance to border")


library(rddapp)
summary(rd_est(education ~ distrunning, data = points_samp.sf, t.design = "g"))


plot(rd_est(education ~ distrunning, data = points_samp.sf, t.design = "g"), fit_line = c("quadratic", "optimal"), bin_n = 50)




points_samp.sf$segment10 <- border_segment(points_samp.sf, cut_off, 10)
points_samp.sf$segment15 <- border_segment(points_samp.sf, cut_off, 15)
tm_shape(points_samp.sf) + tm_dots("segment10", size = 0.1) + tm_shape(cut_off) + tm_lines()
tm_shape(points_samp.sf) + tm_dots("segment15", size = 0.1) + tm_shape(cut_off) + tm_lines()













library(haven)
mlda <- read_dta("C:/Users/Dan/Downloads/Chapter-4-Table-4-1-Fig-4-2-4-4-4-5/AEJfigs.dta")
ddf=df4  %>% filter(distance_km==1.5,south1_north0==1)


mlda$over21 = mlda$agecell>=21
ddf$over500 = ddf$in_project==1
attach(mlda)

#We then fit two models, the second has a quadratic age term.
fit=lm(all~agecell+over21)
fit2=lm(all~agecell+I(agecell^2)+over21)
fit=lm(acre_cult~in_project +  over500 , ddf)

#We plot the two models fit and fit2, using predicted values.

predfit = predict(fit, mlda)
predfit2=predict(fit2,mlda)
# plotting fit
plot(predfit~agecell,type="l",ylim=range(85,110),
     col="red",lwd=2, xlab="Age",
     ylab= "Death rate from all causes (per 100,000)")
# adding fit2
points(predfit2~agecell,type="l",col="blue")
# adding the data points
points(all~agecell)


library(coefplot)
multiplot(fit,fit2,predictors="over21")

library(ggplot2)
age3=ggplot(mlda, aes(x = agecell, y = all,colour=over21)) + 
  geom_point() + ylim(80,115)
age4=age3 + stat_smooth(method = loess)
age4










                         

