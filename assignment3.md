Statistical assignment 3
================
Simone Long\_135288
15/02/2020

## Read data

``` r
library(tidyverse)
library(data.table)

files <- dir(
             "C:/Users/simon/OneDrive/Documents/datan3_2019/data/UKDA-6614-tab/tab",
             pattern = "indresp",
             recursive = TRUE,
             full.names = TRUE)

files <- files[stringr::str_detect(files, "ukhls")]

vars <- c("memorig", "sex_dv", "age_dv", "vote6")

for (i in 1:7) {
        varsToSelect <- paste(letters[i], vars, sep = "_")
        varsToSelect <- c("pidp", varsToSelect)
        data <- fread(files[i], select = varsToSelect)
        if (i == 1) {
                all7 <- data  
        }
        else {
                all7 <- full_join(all7, data, by = "pidp")
        }
        rm(data)
} 
```

## Reshape data (20 points)

``` r
Long <- all7 %>%
  gather(a_memorig:g_vote6, key = "variable", value = "value") %>%
  separate(variable, into = c("wave", "variable"), sep = "_", extra =  "merge") %>%
   spread(key = variable, value = value)
```

## Filter and recode (20 points)

``` r
Long <- Long %>%
        filter(memorig == 1) %>%
        mutate(sex_dv = ifelse(sex_dv == 2, "female",
                           ifelse(sex_dv == 1, "male", NA))) %>%
        mutate(vote6 = recode(vote6,
                              `1` = 1,
                              `2` = 2,
                              `3` = 3,
                              `4` = 4,
                              .default = NA_real_))
        
  table(Long$sex_dv)
```

    ## 
    ## female   male 
    ## 117665 100342

``` r
  table(Long$vote6)
```

    ## 
    ##     1     2     3     4 
    ## 21660 70952 56134 52145

## Calculate mean political interest by sex and wave (10 points)

``` r
meanVote6 <- Long %>%
        group_by(wave, sex_dv) %>%
        filter(!is.na(sex_dv)) %>%
        summarise(voteavg = mean(vote6, na.rm = TRUE))
        
meanVote6
```

    ## # A tibble: 14 x 3
    ## # Groups:   wave [7]
    ##    wave  sex_dv voteavg
    ##    <chr> <chr>    <dbl>
    ##  1 a     female    2.84
    ##  2 a     male      2.53
    ##  3 b     female    2.82
    ##  4 b     male      2.51
    ##  5 c     female    2.87
    ##  6 c     male      2.54
    ##  7 d     female    2.89
    ##  8 d     male      2.55
    ##  9 e     female    2.87
    ## 10 e     male      2.51
    ## 11 f     female    2.81
    ## 12 f     male      2.47
    ## 13 g     female    2.73
    ## 14 g     male      2.42

## Reshape the data frame with summary statistics (20 points)

``` r
meanVote6 %>%
       spread(wave, voteavg)
```

    ## # A tibble: 2 x 8
    ##   sex_dv     a     b     c     d     e     f     g
    ##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 female  2.84  2.82  2.87  2.89  2.87  2.81  2.73
    ## 2 male    2.53  2.51  2.54  2.55  2.51  2.47  2.42

Across all seven waves, women tend to be slightly less interested in
politics than men on average. There are a variety of possible reasons
for this. It could be that the sample of women for this dataset is
uniquely disinterested in poitics. It could also be that the mean for
interest in politics could be influenced in one direction based on age,
rather than sex alone (i.e. maybe younger/older people tend to be
more/less interested). But from a psychological perspective, it could be
that women don’t tend to engage as much politically because they don’t
see themselves represented in politics; there are far more male
politicans than female, so it is conceivable that women don’t think
about politics as much because they don’t feel they have a voice.

## Estimate stability of political interest (30 points)

``` r
Long <- Long %>%
  pivot_wider(id_cols = c(pidp, sex_dv), names_from = wave,
              values_from = c(age_dv, vote6))

Long <- Long %>%
  drop_na(vote6_a:vote6_g) %>%
  select(-c(age_dv_b:age_dv_g))

Long <- Long %>%
  mutate(delta = 
           abs((vote6_b - vote6_a) +
           (vote6_c - vote6_b) +
           (vote6_d - vote6_c) +
           (vote6_e - vote6_d) +
           (vote6_f - vote6_e) +
           (vote6_g - vote6_f)))

sex.mean <- Long %>%
  group_by(sex_dv) %>%
  summarise(delta.mean = mean(delta))

knitr::kable(
  sex.mean,
 col.names = c("Sex", "Mean Delta")
)
```

| Sex    | Mean Delta |
| :----- | ---------: |
| female |  0.5072194 |
| male   |  0.5008023 |

``` r
age.mean <- Long %>%
  group_by(age_dv_a) %>%
  summarise(delta.mean = mean(delta))

knitr::kable(
  age.mean,
 col.names = c("Age", "Mean Delta")
)
```

| Age | Mean Delta |
| --: | ---------: |
|  15 |  2.0000000 |
|  16 |  0.6357143 |
|  17 |  0.7045455 |
|  18 |  0.6231884 |
|  19 |  0.7471264 |
|  20 |  0.5056180 |
|  21 |  0.5882353 |
|  22 |  0.4313725 |
|  23 |  0.5084746 |
|  24 |  0.4214286 |
|  25 |  0.5063291 |
|  26 |  0.4969697 |
|  27 |  0.5988701 |
|  28 |  0.5510204 |
|  29 |  0.4877049 |
|  30 |  0.5096154 |
|  31 |  0.4807692 |
|  32 |  0.6036036 |
|  33 |  0.5518868 |
|  34 |  0.5024631 |
|  35 |  0.4651163 |
|  36 |  0.4696970 |
|  37 |  0.5060729 |
|  38 |  0.4964029 |
|  39 |  0.5765472 |
|  40 |  0.5462687 |
|  41 |  0.4888179 |
|  42 |  0.5077399 |
|  43 |  0.4601449 |
|  44 |  0.4882943 |
|  45 |  0.5034247 |
|  46 |  0.4560261 |
|  47 |  0.4798762 |
|  48 |  0.4713376 |
|  49 |  0.5145631 |
|  50 |  0.4798535 |
|  51 |  0.4244373 |
|  52 |  0.5234899 |
|  53 |  0.4612676 |
|  54 |  0.4560811 |
|  55 |  0.4671533 |
|  56 |  0.5136054 |
|  57 |  0.4891304 |
|  58 |  0.3980892 |
|  59 |  0.4191176 |
|  60 |  0.4833948 |
|  61 |  0.4613181 |
|  62 |  0.4403409 |
|  63 |  0.4875346 |
|  64 |  0.4630225 |
|  65 |  0.4981132 |
|  66 |  0.5312500 |
|  67 |  0.5283843 |
|  68 |  0.5123153 |
|  69 |  0.4717949 |
|  70 |  0.5263158 |
|  71 |  0.5572917 |
|  72 |  0.5224719 |
|  73 |  0.5155280 |
|  74 |  0.5611511 |
|  75 |  0.4878049 |
|  76 |  0.5094340 |
|  77 |  0.5833333 |
|  78 |  0.6530612 |
|  79 |  0.7058824 |
|  80 |  0.5822785 |
|  81 |  0.6666667 |
|  82 |  0.5471698 |
|  83 |  0.7948718 |
|  84 |  0.4333333 |
|  85 |  0.5789474 |
|  86 |  0.6923077 |
|  87 |  0.7000000 |
|  88 |  0.6666667 |
|  89 |  1.0000000 |
|  90 |  0.7500000 |
|  91 |  1.0000000 |
|  95 |  1.0000000 |

``` r
library(ggplot2)
age.mean %>%
  ggplot(aes(x=age_dv_a, y=delta.mean)) +
  geom_point() +
  stat_smooth(method = 'lm', formula = y~poly(x,2)) +
  xlab("Age") +
  ylab("Mean Delta")
```

![](assignment3_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

When looking at the mean change in political interest broken down by
gender, it seems that women tend slightly more to vary in degree of
interest than do men. This difference isn’t incredibly significant, but
could potentially be explained by such factors as growing female
representation in politics and/or the increasing presence of news on
social media (the latter not necessarily being specific to women).

As far as age is concerned, it seems that the youngest and the oldest
individuals experience more variability in political engagement, with
teenagers aged 15 having the highest tendency to switch every so often.
Adolescence is a time of uncertainty and self-discovery, so it is
unsurprising that young people would have varying degrees of political
interest. This could also be in conjunction with the exposure social
media provides to world news and political debate. The elderly exhibit
similar proclivities, with those older than 85 also tending to alternate
between greater and lesser engagement. This could quite simply be due to
apathy brought on by old age, as the older one gets, the less they may
feel they are affected by politics.
