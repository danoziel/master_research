# ----
library(tidyverse)
library(tableone)

library(corrplot)
library(kableExtra)
library(formattable)
library(gridExtra)
library(scales)
library(extrafont)
library(sjPlot)
library(RColorBrewer)

 names(df)
#----
Aphid+Cicdld+Empsca+Bemisia_tabaci+
Orius+Rchnaf+Tingid+Lygde+Lepidoptera+
Fly+Moskito+Yatus+Betle+Cocinle+Trips+
Ant+Par_Wasp+Spdr+Tricho_oligsta+Encrsia+
Telenms+Ashklp0719+Asheshkl07+Agrtis0719+Hlyts0719   
# ----

Veg_spc+Flower_smr

# ----
A550Avcd+A550GDSH+A550Drtrd+A550grnhs+
A550stlmnt  +A550rd  +A550ntcltv+A550rsrvr+A550agbld+a550mata+a550net+a550strm+
  A250avcd+a250gdsh+a250drtrd+a250grnhs+a250stl+a250road+a250ntcltv+a250rsrvr+
  a250agbld+a250mata+a250net+a250strm+a100avcd+a100gdsh+a100drtrd+a100grnhs+
  a100stl+a100road+a100ntcltv+a100rsrvr+
  a100agbld+a100mata+a100net
 
 round(cor(df),
       digits = 2 # rounded to 2 decimals
 )
# -----
df <-  For_Dan_JulyDT %>% filter(Ashklp0719 > 0)
c <- cor.test(df$Ashklp0719,df$A550agbld)
c


ggplot(df, aes(x=A550GDSH, y=Ashklp0719)) + geom_point()+  geom_smooth(method=lm)

# results----


A550agbld 0.7669
a100mata -0.559 / a250agbld 0.80 / a100drtrd 0.65
a100stl 0.8 / a100agbld 0.8

cor.test(df$Ashklp0719,df$a250agbld)

sip  <-tribble(~"Variable", ~"r", ~"p",
               "A550agbld",0.76 , 0.02, 
               "a250agbld",0.8,0.01,
               "a100drtrd",0.65,0.07,
               "a100stl",0.8,0.02,
               "a100agbld",0.8,0.01
               )

kable(sip,booktabs= T, align = "lcc") %>%
  kable_styling( position = "left") %>% 
  add_header_above(c(" " = 1, "Pearson correlation coefficients" = 2))

 

mod1 <- lm(Ashklp0719~a250agbld ,df)
mod2 <- lm(Ashklp0719~a100stl ,df)
mod3 <- lm(Ashklp0719~a100agbld ,df)
mod4 <- lm(Ashklp0719~a100drtrd ,df)
mod15 <- lm(Ashklp0719~ A550agbld+a100mata,df)

tab_model(mod1)
tab_model(mod2)
tab_model(mod3)
tab_model(mod4)
tab_model(mod15)

tab_model(mod15,digits=3,p.style="star",show.se = TRUE,string.ci = "Conf. Int (95%)")
