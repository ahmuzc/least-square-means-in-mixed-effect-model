library(nlme);library(tidyverse);library(emmeans)
data("Orthodont")
Orthodont%>%group_by(age,Sex)%>%
  summarise(mean=mean(distance))
# A tibble: 8 × 3
# Groups:   age [4]
# age Sex     mean
# <dbl> <fct>  <dbl>
#   1     8 Male    22.9
# 2     8 Female  21.2
# 3    10 Male    23.8
# 4    10 Female  22.2
# 5    12 Male    25.7
# 6    12 Female  23.1
# 7    14 Male    27.5
# 8    14 Female  24.1
with(Orthodont,interaction.plot(
  age,Sex,distance
))

lme.fit <- lme(distance~age+Sex+age:Sex,
               random = ~1|Subject,
               data = Orthodont,
               method = 'ML')
rg <- ref_grid(lme.fit,
               at=list(age=c(8,10,12,14)))
emmip(rg,Sex~age,CIs=T)
emmeans(rg,~age|Sex)
# Sex = Male:
#   age emmean    SE df lower.CL upper.CL
# 8   22.6 0.531 26     21.5     23.7
# 10   24.2 0.483 26     23.2     25.2
# 12   25.8 0.483 26     24.8     26.7
# 14   27.3 0.531 26     26.2     28.4
# 
# Sex = Female:
#   age emmean    SE df lower.CL upper.CL
# 8   21.2 0.640 25     19.9     22.5
# 10   22.2 0.582 25     21.0     23.4
# 12   23.1 0.582 25     21.9     24.3
# 14   24.1 0.640 25     22.8     25.4
# 
# Degrees-of-freedom method: containment 
# Confidence level used: 0.95 
