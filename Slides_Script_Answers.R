# title: "Responding to analysis and communication: Data science the R way"
# subtitle: "DataTeka"
# author: "Tatjana Kecojevic"
# date: "26 April 2018"
# **Tip**💡:
#  - When start working on a new R code/R Project in [RStudio IDE](https://support.rstudio.com/hc/en-us/sections/200107586-Using-the-RStudio-IDE) use 
# ***File -> New Project*** 
#  This way your working directory would be set up when you start a new project and it will save all your files in it. Next time you open your project it would set project's directory as a working directory... It would help you with so much [more](https://support.rstudio.com/hc/en-us/articles/200526207-Using-Projects). 
# ---
## Dataset
# **gapminder** dataset available from **gapminder** package.
# For each of 142 countries, the package provides values for life expectancy, GDP per capita, and population, every five years, from 1952 to 2007.

DT::datatable(head(gapminder::gapminder, 4))

##Gapminder Data

gapminder::gapminder[1:3,]

install.packages("dplyr", repos = "http://cran.us.r-project.org")
install.packages("ggplot2", repos = "http://cran.us.r-project.org")
install.packages("gapminder", repos = "http://cran.us.r-project.org")

## 1st look at the data: <span style="color:blue">`dim()`</span> & <span style="color:blue">`head()`</span>
  
library(gapminder)
dim(gapminder)
head(gapminder, n=10)

##Examine the structure of the data: <span style="color:blue">`str()`</span>

str(gapminder) 


##Do it in a tidy way: glimpse()
  
library(dplyr)
glimpse(gapminder) 

##Select your variables
#1) that ends with letter `p`
#2) starts with letter `o`. Try to do this selection using base R.  

##Solutions:

gm_pop_gdp <- select(gapminder, ends_with("p"))
head(gm_pop_gdp, n = 1)

gm_cc <- select(gapminder, starts_with("co"))
head(gm_cc, n = 1)

gm_cc <- gapminder[c("country", "continent")]

##Create new variables of existing variables: <span style="color:blue">`mutate()`</span>

gapminder2 <- mutate(gapminder, LifeExp_month = lifeExp * 12) 
head(gapminder2, n = 2)

## Filter your data:
# Use `gapminder2` `df` to filter:
# 1) only Europian countries and save it as `gapmEU`
# 2) only Europian countries from 2000 onward and save it as `gapmEU21c`
# 3) rows where the life expectancy is greater than 80
#
# Don't forget to **use `==` instead of `=`**! and
# Don't forget the quotes ** `""` **
---
##Solutions:

gapmEU <- filter(gapminder2, continent == "Europe") 
head(gapmEU, 2)


gapmEU21c <- filter(gapminder2, continent == "Europe" & year >= 2000)
head(gapmEU21c, 2)


filter(gapminder2, lifeExp > 80)

## Arranging your data
# 1) Arrange countries in `gapmEU21c` `df` by life expectancy in ascending and descending order.
# 2) Using `gapminder df`
# - Find the records with the smallest population
# - Find the records with the largest life expectancy.
# ---
##Solution 1):
gapmEU21c_h2l <- arrange(gapmEU21c, lifeExp)
head(gapmEU21c_h2l, 2)
gapmEU21c_l2h <- arrange(gapmEU21c, desc(lifeExp)) #<<
head(gapmEU21c_l2h, 2)
# ---
## Solution 2):
arrange(gapminder, pop)

arrange(gapminder, desc(lifeExp))

# ---
##Solution: Summarise your data

summarise(gapminder, max_lifeExp = max(lifeExp), max_gdpPercap = max(gdpPercap))

summarise(gapminder, mean_lifeExp = mean(lifeExp), mean_gdpPercap = mean(gdpPercap))


#**Do you know what this code does?**

gapminder_pipe <- gapminder %>%
  filter(continent == "Europe" & year ==  2007) %>%
  mutate(pop_e6 = pop / 1000000)
plot(gapminder_pipe$pop_e6, gapminder_pipe$lifeExp, cex = 0.5, col = "red")


# Can we make it look better? 😁

## ggplot()
# 1. "Initialise" a plot with `ggplot()`
# 2. Add layers with `geom_` functions

library(ggplot2)
ggplot(gapminder_pipe, aes(x = pop_e6, y = lifeExp)) +
  geom_point(col ="red")

# ggplot() gallery
       
       
ggplot(data = gapminder, mapping = aes(x = lifeExp), binwidth = 10) +
  geom_histogram()
#
ggplot(data = gapminder, mapping = aes(x = lifeExp)) +
  geom_density()
#
ggplot(data = gapminder, mapping = aes(x = continent, color = continent)) +
  geom_bar()
#
ggplot(data = gapminder, mapping = aes(x = continent, fill = continent)) +
 geom_bar()
      
##Confer with your neighbours:

m1 <- lm(gapminder_pipe$lifeExp ~ gapminder_pipe$pop_e6)
summary(m1)

## Your turn!
       
# Use gapminder data.
       
# **Does the life expectancy depend upon the GDP per capita?**
# 1) Have a glance at the data. (tip: `sample_n(df, n)`)
# 2) Produce a scattep plot: what does it tell you?
# 3) Fit a regression model: is there a relationship? How strong is it?
# Is the relationship linear? What conclusion(s) can you draw?
# 4) What are the other questions you could ask; could you provide the answers to them?
       
## Possible Solution: code Q1; sample
sample_n(gapminder, 30)

## Possible Solution: code Q2; Plot the data;
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(alpha = 0.2, shape = 21, fill = "blue", colour="black", size = 5) +
  geom_smooth(method = "lm", se = F, col = "maroon3") +
  geom_smooth(method = "loess", se = F, col = "limegreen") 

## Possible Solution: code Q3; simple regression model

my.model <- lm(gapminder_pipe$lifeExp ~ gapminder_pipe$gdpPercap)
summary(my.model)

## Adding layers to your ggplot()
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, col = "red")) +
  geom_point(alpha = 0.2, shape = 21, fill = "blue", colour="black", size = 5) +
  geom_smooth(method = "lm", se = F, col = "maroon3") +
  geom_smooth(method = "loess", se = F, col = "limegreen") +
  labs (title= "Life Exp. vs. Population Size", 
       x = "population", y = "Life Exp.") +
       theme(legend.position = "none", 
       panel.border = element_rect(fill = NA, 
       colour = "black",
       size = .75),
  plot.title=element_text(hjust=0.5)) +
  geom_text(x = 80000, y = 125, label = "regression line", col = "maroon3") +
  geom_text(x = 90000, y = 75, label = "smooth line", col = "limegreen")

## **There is a challenge:** 
# - `dplyr`'s `group_by()` function enables you to group your data. It allows you to create a separate df that splits the original df by a variable.
# - `boxplot()` function produces boxplot(s) of the given (grouped) values.
#Knowing about `group_by()` and `boxplot()` function, coud you compute the median life expectancy for year 2007 by continent and visualise your result?

## Possible Solution: 
gapminder %>%
  group_by(continent) %>%
  summarise(lifeExp = median(lifeExp))

## Possible Solution: 
# visualise the information code
ggplot(gapminder, aes(x = continent, y = lifeExp)) +
  geom_boxplot(outlier.colour = "hotpink") +
  geom_jitter(position = position_jitter(width = 0.1, height = 0), alpha = .2) +
  labs (title= "Life Exp. vs. Continent", 
       x = "Continent", y = "Life Exp.") +
  theme(legend.position = "none", 
       panel.border = element_rect(fill = NA, 
                                   colour = "black",
                                   size = .75),
       plot.title=element_text(hjust=0.5))

##Let's do Elain's Dance!!! 😃🎵🎶
