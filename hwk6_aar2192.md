hwk6\_aar2192
================
Amadeia Rector
11/20/2018

Problem 1
=========

This problem uses the dataset on homicides given by the Washington Post.

### Part a

Create a city\_state variable (e.g. “Baltimore, MD”), and a binary variable indicating whether the homicide is solved. Omit cities Dallas, TX; Phoenix, AZ; and Kansas City, MO – these don’t report victim race. Also omit Tulsa, AL – this is a data entry mistake. Modifiy victim\_race to have categories white and non-white, with white as the reference category. Be sure that victim\_age is numeric.

``` r
homicide_df = 
  read_csv("data/homicide-data.csv", na = c("", "NA", "Unknown")) %>%
  mutate(
    city_state = str_c(city, state, sep = ", "),
    resolution = case_when(
      disposition == "Closed without arrest" ~ "unresolved",
      disposition == "Open/No arrest"        ~ "unresolved",
      disposition == "Closed by arrest"      ~ "resolved"
    )
  ) %>% 
  filter(city_state != "Tulsa, AL") %>% 
  filter(city_state != "Dallas, TX") %>% 
  filter(city_state != "Phoenix, AZ") %>% 
  filter(city_state != "Kansas City, MO") %>% 
  filter(victim_race != is.na(victim_race)) %>% 
  mutate(victim_race = ifelse(victim_race %in% "White", "white", "non-white"),
         victim_race = fct_relevel(victim_race, "white"),
         victim_age = as.numeric(victim_age))
## Parsed with column specification:
## cols(
##   uid = col_character(),
##   reported_date = col_integer(),
##   victim_last = col_character(),
##   victim_first = col_character(),
##   victim_race = col_character(),
##   victim_age = col_integer(),
##   victim_sex = col_character(),
##   city = col_character(),
##   state = col_character(),
##   lat = col_double(),
##   lon = col_double(),
##   disposition = col_character()
## )
```

I cleaned the dataset given fromthe Washington post. Age was converted to a numerical variable, race was made into a binary variable, and observations with missing data from race were deleted. Additionally, four cities were deleted from the dataset due to missing information/incorrect coding.

### Part b

For the city of Baltimore, MD, use the glm function to fit a logistic regression with resolved vs unresolved as the outcome and victim age, sex and race (as just defined) as predictors. Save the output of glm as an R object; apply the broom::tidy to this object; and obtain the estimate and confidence interval of the adjusted odds ratio for solving homicides comparing non-white victims to white victims keeping all other variables fixed.

``` r
fit_logistic_balt =
  homicide_df %>% 
  mutate(resolved = as.numeric(resolution == "resolved")) %>% 
  filter(city == "Baltimore") %>% 
  glm(resolved ~ victim_age + victim_race + victim_sex, data = ., family = binomial())

fit_logistic_balt %>% 
  broom::tidy() %>% 
  mutate(CI_lower = estimate - 1.96*std.error,
         CI_upper = estimate + 1.96*std.error,
         OR = exp(estimate),
         CI_lower = exp(CI_lower),
         CI_upper = exp(CI_upper)) %>%
  select(term, log_OR = estimate, OR, CI_lower, CI_upper, p.value) %>% 
  filter(term=="victim_racenon-white") %>% 
  select(term, OR, CI_lower, CI_upper) %>% 
  knitr::kable(digits = 3)
```

| term                  |     OR|  CI\_lower|  CI\_upper|
|:----------------------|------:|----------:|----------:|
| victim\_racenon-white |  0.441|      0.313|       0.62|

The data was filtered to just look at Baltimore cases. A logistic regression was conducted to investigate the association between race and resolved cases controlling for age and sex of the victim.

The estimated adjusted OR for solving homicides comparing non-white victims to white victims keeping all other variables fixed is 0.441 with a 95% confidence interval of 0.313 to 0.620. Non-white victims have 0.441 times the likelihood of having a resolved case compared to white victims in Baltimore. We are 95% confident that the true OR lies between 0.313 to 0.620, adjusting for sex and age.

### Part c

Now run glm for each of the cities in your dataset, and extract the adjusted odds ratio (and CI) for solving homicides comparing non-white victims to white victims. Do this within a “tidy” pipeline, making use of purrr::map, list columns, and unnest as necessary to create a dataframe with estimated ORs and CIs for each city.

``` r
  homicide_df %>% 
  mutate(resolved = as.numeric(resolution == "resolved")) %>% 
  group_by(city_state) %>% 
  nest() %>% 
  mutate(models = map(data, ~glm(resolved ~ victim_age + victim_race + victim_sex, data = ., family = binomial())),
         models = map(models, broom:::tidy)) %>% 
  select(-data) %>% 
  unnest() %>% 
  filter(term=="victim_racenon-white") %>% 
  mutate(CI_lower = estimate - 1.96*std.error,
         CI_upper = estimate + 1.96*std.error,
         OR = exp(estimate),
         CI_lower = exp(CI_lower),
         CI_upper = exp(CI_upper)) %>%
  select(city_state, OR, CI_lower, CI_upper) %>% 
  knitr::kable(digits = 3)
```

| city\_state        |     OR|  CI\_lower|  CI\_upper|
|:-------------------|------:|----------:|----------:|
| Albuquerque, NM    |  0.739|      0.447|      1.223|
| Atlanta, GA        |  0.753|      0.432|      1.313|
| Baltimore, MD      |  0.441|      0.313|      0.620|
| Baton Rouge, LA    |  0.668|      0.313|      1.425|
| Birmingham, AL     |  1.039|      0.615|      1.756|
| Boston, MA         |  0.127|      0.052|      0.307|
| Buffalo, NY        |  0.392|      0.214|      0.719|
| Charlotte, NC      |  0.558|      0.321|      0.969|
| Chicago, IL        |  0.562|      0.431|      0.733|
| Cincinnati, OH     |  0.318|      0.184|      0.551|
| Columbus, OH       |  0.861|      0.638|      1.161|
| Denver, CO         |  0.602|      0.359|      1.009|
| Detroit, MI        |  0.652|      0.488|      0.870|
| Durham, NC         |  1.003|      0.404|      2.489|
| Fort Worth, TX     |  0.838|      0.555|      1.266|
| Fresno, CA         |  0.445|      0.229|      0.864|
| Houston, TX        |  0.873|      0.699|      1.090|
| Indianapolis, IN   |  0.505|      0.382|      0.667|
| Jacksonville, FL   |  0.658|      0.502|      0.862|
| Las Vegas, NV      |  0.763|      0.592|      0.982|
| Long Beach, CA     |  0.794|      0.388|      1.626|
| Los Angeles, CA    |  0.666|      0.483|      0.918|
| Louisville, KY     |  0.392|      0.259|      0.593|
| Memphis, TN        |  0.778|      0.521|      1.162|
| Miami, FL          |  0.577|      0.376|      0.885|
| Milwaukee, wI      |  0.632|      0.403|      0.991|
| Minneapolis, MN    |  0.646|      0.345|      1.209|
| Nashville, TN      |  0.902|      0.656|      1.241|
| New Orleans, LA    |  0.467|      0.295|      0.738|
| New York, NY       |  0.532|      0.279|      1.012|
| Oakland, CA        |  0.213|      0.104|      0.435|
| Oklahoma City, OK  |  0.681|      0.478|      0.971|
| Omaha, NE          |  0.170|      0.094|      0.307|
| Philadelphia, PA   |  0.644|      0.486|      0.852|
| Pittsburgh, PA     |  0.282|      0.161|      0.493|
| Richmond, VA       |  0.447|      0.162|      1.238|
| San Antonio, TX    |  0.689|      0.461|      1.030|
| Sacramento, CA     |  0.781|      0.449|      1.359|
| Savannah, GA       |  0.605|      0.284|      1.288|
| San Bernardino, CA |  0.880|      0.393|      1.972|
| San Diego, CA      |  0.483|      0.298|      0.785|
| San Francisco, CA  |  0.458|      0.290|      0.723|
| St. Louis, MO      |  0.577|      0.406|      0.820|
| Stockton, CA       |  0.376|      0.196|      0.719|
| Tampa, FL          |  1.159|      0.587|      2.288|
| Tulsa, OK          |  0.596|      0.408|      0.869|
| Washington, DC     |  0.514|      0.260|      1.017|

Using the function glm, I conducted logistic regression for all of the cities through use of map statements and list columns. I extracted only the estimated adjusted ORs and their 95% confidence intervals.

### Part d

Create a plot that shows the estimated ORs and CIs for each city. Organize cities according to estimated OR, and comment on the plot.

``` r
homicide_cities_OR = 
  homicide_df %>% 
  mutate(resolved = as.numeric(resolution == "resolved")) %>% 
  group_by(city_state) %>% 
  nest() %>% 
  mutate(models = map(data, ~glm(resolved ~ victim_age + victim_race + victim_sex, data = ., family = binomial())),
         models = map(models, broom:::tidy)) %>% 
  select(-data) %>% 
  unnest() %>% 
  filter(term=="victim_racenon-white") %>% 
  mutate(CI_lower = estimate - 1.96*std.error,
         CI_upper = estimate + 1.96*std.error,
         OR = exp(estimate),
         CI_lower = exp(CI_lower),
         CI_upper = exp(CI_upper)) %>%
  select(city_state, OR, CI_lower, CI_upper)

homicide_cities_OR %>% 
  mutate(city_state = fct_reorder(city_state, OR)) %>% 
  ggplot(aes(x = city_state, y = OR)) +
  geom_point() +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper)) +
  labs(x = "Cities", y = "Estimated Adjusted OR", title = "OR for Resolved Murders Comparing Non-white to White Race") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

<img src="hwk6_aar2192_files/figure-markdown_github/unnamed-chunk-4-1.png" width="90%" />

The city with the lowest estimated OR is Boston, MA followed by Omaha, NE and Oakland, CA. The 95% Confidence Interval is narrow for these cities as well. A low estimated OR here means that the number of resolved cases where the victim is non-white is less than that of white victims, adjusting for age and sex. There is a higher OR for the cities Tampa, FL, Birmingham, AL, and Durham, NC. However, The confidence interval for these cities is very wide. These three cities are the only ones with an estimated adjusted OR higher than 1. This goes to show that there is a stark disparity with regard to how race factors into the resolution of a murder across all cities.
