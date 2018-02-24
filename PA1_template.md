# Reproducible Research: Peer Assessment 1
GJAllen  

### Loading and preprocessing the data
Just reading in the data, but let's also require some packages that I like.


```r
library(data.table)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(magrittr, warn.conflicts = FALSE)
activity <- data.table::fread("activity.csv") %>% 
  mutate(date = as.Date(date))
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

### What is mean total number of steps taken per day?
Let's first make a histogram:

```r
steps.by.day <- activity %>% 
  group_by(date) %>% 
  summarize(steps = sum(steps, na.rm = TRUE))
  
hist(steps.by.day$steps, breaks = 10, xlab = "Total Steps per Day", main = "Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The mean:

```r
mean(steps.by.day$steps, na.rm = TRUE)
```

```
## [1] 9354.23
```

The median:

```r
median(steps.by.day$steps, na.rm = TRUE)
```

```
## [1] 10395
```

### What is the average daily activity pattern?
Let's have a look first at the pattern (sorry I'm really stretching).

```r
interval.aggregate <- activity %>% 
  group_by(interval) %>% 
  summarize(steps = mean(steps, na.rm = TRUE))
ggplot(interval.aggregate) +
  aes(interval, steps) +
  geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

And now for the maximum (it's interval 835):

```r
filter(interval.aggregate, steps == max(steps))
```

```
## # A tibble: 1 x 2
##   interval    steps
##      <int>    <dbl>
## 1      835 206.1698
```

### Imputing missing values
The total number of missing values (it looks like there are 2304 missing values for steps):

```r
colSums(is.na(activity))
```

```
##    steps     date interval 
##     2304        0        0
```

Let's impute the missing values. In this case, we'll just use the average value for each interval.

```r
imputed.activity <- interval.aggregate %>% 
  rename(average.steps = steps) %>% 
  right_join(activity, "interval") %>% 
  mutate(steps = ifelse(is.na(steps), average.steps, steps)) %>% 
  select(-average.steps)
```

Now the histogram, mean, and median:

```r
steps.by.day.imputed <- imputed.activity %>% 
  group_by(date) %>% 
  summarize(steps = sum(steps, na.rm = TRUE))

hist(steps.by.day.imputed$steps, breaks = 10, xlab = "Total Steps per Day", 
     main = "Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean(steps.by.day.imputed$steps)
```

```
## [1] 10766.19
```

```r
median(steps.by.day.imputed$steps)
```

```
## [1] 10766.19
```

There are differences.

### Are there differences in activity patterns between weekdays and weekends?
Let's add the weekday indicator:

```r
imputed.activity %<>% 
  mutate(weekday = weekdays(date),
         weekday = ifelse(weekday %in% c("Saturday", "Sunday"), 
                          "Weekend",
                          "Weekday"))
5/7
```

```
## [1] 0.7142857
```

```r
prop.table(table(imputed.activity$weekday))
```

```
## 
##   Weekday   Weekend 
## 0.7377049 0.2622951
```
It looks pretty reasonable.  Now lets add make the panel plot:

```r
imputed.activity %>%
  group_by(weekday, interval) %>% 
  summarize(steps = mean(steps, na.rm = TRUE)) %>% 
  ggplot() +
  aes(interval, steps) +
  geom_line() +
  facet_wrap(~ weekday)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
  
Yes there are differences.
