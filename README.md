
This document shows how we calculated which flow disruptions were most
commonly found in disruption cascades.

All patient and hospital information has been removed from the data. To
further deidentify the patients, each case was set to start at noon (720
minutes into the day), and all disruption notes were removed.

## Load Data

``` r
rm(list = ls())
library(knitr)
```

    ## Warning: package 'knitr' was built under R version 3.6.2

``` r
library(readtv)
library(tidyverse)
```

    ## Warning: package 'ggplot2' was built under R version 3.6.2

    ## Warning: package 'tibble' was built under R version 3.6.2

    ## Warning: package 'tidyr' was built under R version 3.6.2

    ## Warning: package 'readr' was built under R version 3.6.2

    ## Warning: package 'purrr' was built under R version 3.6.2

    ## Warning: package 'dplyr' was built under R version 3.6.2

``` r
f_name = 'data/events_phi_free.csv'
```

``` r
events = read_csv(f_name) %>% 
  group_by(Case) %>% 
  group_modify(~ mutate(.x, RelativeTime = Time - min(Time, na.rm = TRUE)))
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   Case = col_double(),
    ##   Event.Type = col_character(),
    ##   Time = col_double(),
    ##   Phase = col_double()
    ## )

``` r
glimpse(events)
```

    ## Rows: 1,974
    ## Columns: 5
    ## Groups: Case [41]
    ## $ Case         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ Event.Type   <chr> "COO", "TRN", "TRN", "EQ", "TRN", "EQ", "COO", "EQ", "TR…
    ## $ Time         <dbl> 720, 740, 743, 743, 751, 753, 756, 767, 769, 772, 773, 7…
    ## $ Phase        <dbl> 1, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,…
    ## $ RelativeTime <dbl> 0, 20, 23, 23, 31, 33, 36, 47, 49, 52, 53, 58, 69, 70, 7…

## Load CPA Markers

See `plot_gen2.R` for how the markers were calculated. The source code
to derive the markers was output from read-tv.

    ## Rows: 152
    ## Columns: 3
    ## Groups: Case [41]
    ## $ Case <dbl> 1, 1, 1, 1, 2, 2, 2, 2, 4, 4, 4, 4, 5, 5, 5, 5, 6, 6, 6, 6, 11, …
    ## $ cpts <dbl> 16, 106, 134, 179, 115, 122, 128, 197, 76, 176, 196, 333, 23, 11…
    ## $ vals <dbl> 0.23529412, 1.36666667, 3.00000000, 1.37777778, 1.84482759, 0.28…

``` r
getEvent = function(case, cpt, pre = TRUE) {
  if(pre) {
    rank_fn = cume_dist
    `%cmp%` = `<`
  } else {
    `%cmp%` = `>=`
    rank_fn = min_rank
  }
  
  events %>% 
    filter(Case == case) %>% 
    filter(RelativeTime %cmp% cpt) %>% 
    filter(rank_fn(RelativeTime) == 1) %>% 
    filter(row_number(RelativeTime) == max(row_number(RelativeTime)))
}
```

``` r
cpa_markers_events = cpa_markers %>% 
  group_by(Case, cpts) %>% 
  group_modify(~ data.frame(vals = .x$vals, 
                pre = getEvent(.x$Case, .x$cpts),
                post = getEvent(.x$Case, .x$cpts, FALSE)),
               keep = TRUE)
```

    ## Warning: The `keep` argument of `group_modify()` is deprecated as of dplyr 1.0.0.
    ## Please use the `.keep` argument instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

## Jump Rates

``` r
cpa_markers_events = cpa_markers_events %>% 
  group_by(Case) %>% 
  group_modify(~ .x %>% mutate(jump_rate = vals - lag(vals, default = 0),
                               is_cascade = vals >= 3,
                               jump_type =  if_else(jump_rate > 0, 
                                                    'increases', 'decreases'),
                               start = lag(cpts, default = 0),
                               end = cpts
                               )
               )
```

``` r
cpa_markers_events %>% 
  select(Case, cpts, vals, jump_rate, jump_type, is_cascade, start, end) %>% 
  head %>% 
  kable
```

| Case | cpts |      vals |  jump\_rate | jump\_type | is\_cascade | start | end |
| ---: | ---: | --------: | ----------: | :--------- | :---------- | ----: | --: |
|    1 |   16 | 0.2352941 |   0.2352941 | increases  | FALSE       |     0 |  16 |
|    1 |  106 | 1.3666667 |   1.1313725 | increases  | FALSE       |    16 | 106 |
|    1 |  134 | 3.0000000 |   1.6333333 | increases  | TRUE        |   106 | 134 |
|    1 |  179 | 1.3777778 | \-1.6222222 | decreases  | FALSE       |   134 | 179 |
|    2 |  115 | 1.8448276 |   1.8448276 | increases  | FALSE       |     0 | 115 |
|    2 |  122 | 0.2857143 | \-1.5591133 | decreases  | FALSE       |   115 | 122 |

``` r
cpa_markers_events %>% glimpse
```

    ## Rows: 152
    ## Columns: 18
    ## Groups: Case [41]
    ## $ Case              <dbl> 1, 1, 1, 1, 2, 2, 2, 2, 4, 4, 4, 4, 5, 5, 5, 5, 6, …
    ## $ cpts              <dbl> 16, 106, 134, 179, 115, 122, 128, 197, 76, 176, 196…
    ## $ vals              <dbl> 0.23529412, 1.36666667, 3.00000000, 1.37777778, 1.8…
    ## $ pre.Case          <dbl> 1, 1, 1, 1, 2, 2, 2, 2, 4, 4, 4, 4, 5, 5, 5, 5, 6, …
    ## $ pre.Event.Type    <chr> "COO", "TRN", "EQ", "EXT", "TRN", "TRN", "SDM", "TR…
    ## $ pre.Time          <dbl> 720, 817, 851, 897, 832, 832, 847, 914, 785, 893, 8…
    ## $ pre.Phase         <dbl> 1, 3, 3, 4, 3, 3, 3, 3, 1, 3, 3, 4, 1, 3, 3, 4, 3, …
    ## $ pre.RelativeTime  <dbl> 0, 97, 131, 177, 112, 112, 127, 194, 65, 173, 178, …
    ## $ post.Case         <dbl> 1, 1, 1, 1, 2, 2, 2, 2, 4, 4, 4, 4, 5, 5, 5, 5, 6, …
    ## $ post.Event.Type   <chr> "TRN", "COO", "EXT", "EQ", "IC", "IC", "TRN", "COM"…
    ## $ post.Time         <dbl> 740, 826, 856, 899, 845, 845, 848, 917, 799, 896, 9…
    ## $ post.Phase        <dbl> 2, 3, 3, 4, 3, 3, 3, 3, 2, 3, 3, 4, 1, 3, 3, 4, 3, …
    ## $ post.RelativeTime <dbl> 20, 106, 136, 179, 125, 125, 128, 197, 79, 176, 198…
    ## $ jump_rate         <dbl> 0.2352941, 1.1313725, 1.6333333, -1.6222222, 1.8448…
    ## $ is_cascade        <lgl> FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALS…
    ## $ jump_type         <chr> "increases", "increases", "increases", "decreases",…
    ## $ start             <dbl> 0, 16, 106, 134, 0, 115, 122, 128, 0, 76, 176, 196,…
    ## $ end               <dbl> 16, 106, 134, 179, 115, 122, 128, 197, 76, 176, 196…

## Cascade Marker Events

``` r
cascade_start_events = cpa_markers_events %>% 
  filter(jump_type == "increases" & is_cascade)

nrow(cpa_markers_events)
```

    ## [1] 152

What percent of surgeries?

``` r
n_distinct(cascade_start_events$Case)/
  n_distinct(cpa_markers_events$Case)
```

    ## [1] 0.5853659

\[
P(pre|ET)
\]

``` r
cascade_start_events %>% 
  ungroup %>% 
  count(pre.Event.Type) %>% 
  arrange(desc(n)) %>% 
  mutate(`ratio (%)` = n/sum(events$Event.Type == pre.Event.Type) * 100) %>% 
  head %>% 
  kable
```

| pre.Event.Type | n | ratio (%) |
| :------------- | -: | --------: |
| COM            | 6 | 2.2388060 |
| TRN            | 6 | 2.2388060 |
| EQ             | 5 | 1.8656716 |
| COO            | 3 | 1.1194030 |
| SDM            | 3 | 1.1194030 |
| EXT            | 1 | 0.3731343 |

\[
P(post|ET)
\]

``` r
cascade_start_events %>% 
  ungroup %>% 
  filter(jump_type == "increases") %>% 
  count(post.Event.Type) %>% 
  arrange(desc(n)) %>% 
  mutate(`ratio (%)` = n/sum(events$Event.Type == post.Event.Type) * 100) %>% 
  head %>% 
  kable
```

| post.Event.Type | n | ratio (%) |
| :-------------- | -: | --------: |
| TRN             | 8 | 3.5874439 |
| SDM             | 6 | 2.6905830 |
| COO             | 4 | 1.7937220 |
| COM             | 3 | 1.3452915 |
| EXT             | 3 | 1.3452915 |
| IC              | 1 | 0.4484305 |

\(P(ET_{post}|ET_{pre})\)

``` r
cascade_start_events %>% 
  ungroup %>% 
  count(pre.Event.Type, post.Event.Type) %>% 
  arrange(desc(n)) %>% 
  head %>% 
  kable
```

| pre.Event.Type | post.Event.Type | n |
| :------------- | :-------------- | -: |
| TRN            | TRN             | 3 |
| COM            | COM             | 2 |
| COM            | SDM             | 2 |
| EQ             | SDM             | 2 |
| SDM            | TRN             | 2 |
| TRN            | COO             | 2 |

\(P(trigger|ET)\)

``` r
getTrigger = function(pre.RelativeTime, pre.Event.Type,
                      post.RelativeTime, post.Event.Type,
                      cpts)
  if_else(abs(pre.RelativeTime - cpts) <= 3,
          pre.Event.Type, post.Event.Type)

cascade_start_events %>% 
  mutate(Event.Type = getTrigger(pre.RelativeTime, pre.Event.Type,
                                 post.RelativeTime, post.Event.Type,
                                 cpts)) %>% 
  group_by(Event.Type) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  mutate(`ratio (%)` = n/sum(events$Event.Type == Event.Type) * 100) %>% 
  head %>% 
  kable
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

| Event.Type | n | ratio (%) |
| :--------- | -: | --------: |
| TRN        | 7 | 2.3648649 |
| COM        | 5 | 1.6891892 |
| EQ         | 5 | 1.6891892 |
| COO        | 3 | 1.0135135 |
| SDM        | 3 | 1.0135135 |
| EXT        | 1 | 0.3378378 |

Common cascade event types.

``` r
getMarker = function(case, rel_time) 
  cpa_markers_events %>% 
  filter(Case == case & start <= rel_time & end > rel_time) %>% 
  select(c_start = start, c_end = end, is_cascade, vals) %>% 
  top_n(1)
```

``` r
cascadeEvents = function(case) {
  cme = cpa_markers_events %>% filter(Case == case)
  
  rtSafe = function(case) {
    t_end = cme %>% 
      filter(Case == case) %>% 
      {max(.$end)}
    rt = events %>% filter(Case == case) %>% pull(RelativeTime)
    if_else(rt >= t_end, t_end - 1, rt)
  }
  
  rts = rtSafe(case)
  rts %>% 
    purrr::map(function(rt){
      cme$start %>% 
        purrr::imap(~ rt >= .x & rt <= cme$end[.y] & cme$is_cascade[.y]) %>% 
        purrr::reduce(~ .x | .y)
    }) %>% 
    unlist
}
```

``` r
getMarker(1, 5)
```

    ## Adding missing grouping variables: `Case`

    ## Selecting by vals

    ## # A tibble: 1 x 5
    ## # Groups:   Case [1]
    ##    Case c_start c_end is_cascade  vals
    ##   <dbl>   <dbl> <dbl> <lgl>      <dbl>
    ## 1     1       0    16 FALSE      0.235

``` r
events_and_cascades = events %>% 
  group_by(Case) %>% 
  group_modify(~ .x %>% 
                 mutate(is_cascade = cascadeEvents(unique(.x$Case))) %>% 
                 select(-"Case"), 
               keep = TRUE) %>% 
  ungroup
```

# Which Event Types are Most Likely in Cascades

``` r
event_counts = events %>% 
  ungroup %>% 
  count(Event.Type) %>% 
  rename(n = n)
```

``` r
table1 = events_and_cascades %>% 
  filter(is_cascade) %>% 
  count(Event.Type) %>%
  rename(n_cascade = n) %>% 
  merge(event_counts) %>% 
  mutate(p_cascade_given_et = n_cascade/n,
         p_et = n/sum(n),
         p_et_given_cascade = n_cascade/sum(n_cascade)) %>%    
  arrange(desc(p_cascade_given_et)) 
```

``` r
table1 %>% kable
```

| Event.Type | n\_cascade |   n | p\_cascade\_given\_et |     p\_et | p\_et\_given\_cascade |
| :--------- | ---------: | --: | --------------------: | --------: | --------------------: |
| TRN        |        117 | 376 |             0.3111702 | 0.1904762 |             0.2733645 |
| COM        |         64 | 278 |             0.2302158 | 0.1408308 |             0.1495327 |
| ENV        |          4 |  18 |             0.2222222 | 0.0091185 |             0.0093458 |
| SDM        |         32 | 148 |             0.2162162 | 0.0749747 |             0.0747664 |
| IC         |         26 | 126 |             0.2063492 | 0.0638298 |             0.0607477 |
| EQ         |         90 | 460 |             0.1956522 | 0.2330294 |             0.2102804 |
| PF         |         11 |  58 |             0.1896552 | 0.0293820 |             0.0257009 |
| COO        |         71 | 385 |             0.1844156 | 0.1950355 |             0.1658879 |
| EXT        |         13 | 125 |             0.1040000 | 0.0633232 |             0.0303738 |

``` r
events_and_cascades %>% 
  count(is_cascade) %>%
  mutate(p = n/sum(n)) %>% 
  kable
```

| is\_cascade |    n |         p |
| :---------- | ---: | --------: |
| FALSE       | 1546 | 0.7831814 |
| TRUE        |  428 | 0.2168186 |
