stp25tools
================

<!-- Attaching package: ‘stp25tools’ -->
<!-- The following objects are masked from ‘package:stp25aggregate’: -->
<!--     get_label, Label, set_label, wrap_label, XLS -->

## Funktionen

- Pivot-Funktionen: *Long(), Wide(), Dapply(), dapply2(), transpose2()*

- Zusammenfuegen von Data Frame: *Merge2, Rbind2, combine_data_frame*

- Objekt in Data Frame umwandeln: *fix_to_df, fix_to_tibble, list_to_df*

- Vectoren fuer den Zugriff auf Data Frame erstellen: *Cs, XLS,
  paste_names*

- String umbrechen: *wrap_string*

- Element zu Vectoren oder Listen hinzufuegen: *add_to, add_row_df*

- Intern Daten mit Formel aufbereiten: *prepare_data2, print*

- Vectoren transformieren: *as_numeric, as_factor, factor2, as_cut,
  as_logical, rev.factor,as_rev, cat_bmi*

- Daten importieren: *get_data*

- Fehlende Daten ergänzen und transformieren: *na_approx, auto_trans*

- Label verwalten: *Label, delet_label, get_label, set_label*

- Bereinigen von Data Frame und strings: *clean_names,
  cleansing_umlaute, cleansing_umlaute2*

- Rechen operationen: *auc_trapezoid*

## Factor

``` r
factor2(c(1,0,0,0,1,1,0), 
        male = 1, female = 0)
```

    ## [1] male   female female female male   male   female
    ## Levels: male female

## Numeric

``` r
x <-
  c("3,6",  "> 15100",  "+1",  "-1",
    "$10 -> expensive", "cheap: $2.50", "free $0 !!",
    "699.91  ",  " 228.4031.9 ",
    "",  NA,  "hallo1",  "-77"
  )
 
 
cbind(
  stp = as_numeric(x,  na.string = c("-77")),
  fct = as_numeric(factor(x)),
  rdr = readr::parse_number(x,  na = c("-77"))
)
```

    ##              stp        fct        rdr
    ##  [1,]     3.6000     3.6000    36.0000
    ##  [2,] 15100.0000 15100.0000 15100.0000
    ##  [3,]     1.0000     1.0000     1.0000
    ##  [4,]    -1.0000    -1.0000    -1.0000
    ##  [5,]    10.0000    10.0000    10.0000
    ##  [6,]     2.5000     2.5000     2.5000
    ##  [7,]     0.0000     0.0000     0.0000
    ##  [8,]   699.9100   699.9100   699.9100
    ##  [9,]   228.4031   228.4031   228.4031
    ## [10,]         NA         NA         NA
    ## [11,]         NA         NA         NA
    ## [12,]     1.0000     1.0000     1.0000
    ## [13,]         NA   -77.0000         NA

## Get Data

### Direkter Import aus Text

``` r
dat <-
  get_data("
sex treatment control
m  2 3
f  3 4
", tabel_expand = TRUE,id.vars = 1)

xtabs(~ sex + value, dat)
```

    ##    value
    ## sex control treatment
    ##   f       4         3
    ##   m       3         2

``` r
dat <- 
  get_data(
"sex treatment  neg  pos
f   KG          3   3
f   UG          4   5
m   KG          5   4
m   UG          4   2
",
  tabel_expand = TRUE,
  id.vars = 1:2,
  value = "befund"
)

ftable(xtabs(~ sex + treatment + befund, dat))
```

    ##               befund neg pos
    ## sex treatment               
    ## f   KG                 3   3
    ##     UG                 4   5
    ## m   KG                 5   4
    ##     UG                 4   2

### Daten File Import

``` r
 file.exists("R/dummy.csv")
```

    ## [1] TRUE

``` r
#' get_data("R/dummy.csv", dec = ",", na.strings = "-", skip=1, label=1)
#' get_data("R/dummy.xlsx", na.strings = "-")
 get_data("R/dummy.xlsx")
```

    ## # A tibble: 5 × 5
    ##      id group.student x         y     z
    ##   <dbl> <chr>         <chr> <dbl> <dbl>
    ## 1     1 A             1      4.3   59.4
    ## 2     2 B             4      3.24  47.3
    ## 3     3 C             8      4.02  32.2
    ## 4     4 D             -      1.25  NA  
    ## 5     5 E             9      1.23  36.4

``` r
#' 
 x <- get_data("R/dummy.sav")
 get_label(x)[1:4]
```

    ##               lfdn      external.lfdn             tester           dispcode 
    ##           "number"    "external lfdn"           "tester" "disposition code"

### Daten speichern und aus Codebook rekostruieren

``` r
 save_data(dat, "demo.xlsx", include.codebook=TRUE)
```

    ## Writing file to:
    ## C:/Users/wpete/Dropbox/3_Forschung/R-Project/stp25tools/demo.xlsx

``` r
 DF2 <- use_codebook(file = "demo.xlsx")
```

    ## 
    ## Use data from file demo.xlsx 
    ## # A tibble: 6 × 4
    ##   sex   treatment Var1  befund
    ##   <chr> <chr>     <chr> <chr> 
    ## 1 f     KG        f+KG  neg   
    ## 2 f     KG        f+KG  neg   
    ## 3 f     KG        f+KG  neg   
    ## 4 f     UG        f+UG  neg   
    ## 5 f     UG        f+UG  neg   
    ## 6 f     UG        f+UG  neg   
    ## 
    ## Label and levels from file demo.xlsx 
    ## # A tibble: 4 × 3
    ##   names     label     value.labels                     
    ##   <chr>     <chr>     <chr>                            
    ## 1 sex       sex       factor: f | m                    
    ## 2 treatment treatment factor: KG | UG                  
    ## 3 Var1      Var1      factor: f+KG | f+UG | m+KG | m+UG
    ## 4 befund    befund    factor: neg | pos                
    ## 
    ##  sex : character -> factor
    ##  treatment : character -> factor
    ##  Var1 : character -> factor
    ##  befund : character -> factor

``` r
 head(DF2)
```

    ## # A tibble: 6 × 4
    ##   sex   treatment Var1  befund
    ##   <fct> <fct>     <fct> <fct> 
    ## 1 f     KG        f+KG  neg   
    ## 2 f     KG        f+KG  neg   
    ## 3 f     KG        f+KG  neg   
    ## 4 f     UG        f+UG  neg   
    ## 5 f     UG        f+UG  neg   
    ## 6 f     UG        f+UG  neg

## Transpose

### Wide

``` r
dat
```

    ##   month student  A B
    ## 1     1     Amy 19 6
    ## 2     2     Amy 27 7
    ## 3     3     Amy 16 8
    ## 4     1     Bob 28 5
    ## 5     2     Bob 10 6
    ## 6     3     Bob 29 7

``` r
df2<- dat %>% Wide(student,  c(A, B))
dat %>% Wide(student,  c("A", "B"))
```

    ## # A tibble: 3 × 5
    ##   month Amy_A Bob_A Amy_B Bob_B
    ##   <int> <dbl> <dbl> <dbl> <dbl>
    ## 1     1    19    28     6     5
    ## 2     2    27    10     7     6
    ## 3     3    16    29     8     7

``` r
dat[-3] %>% Wide(student,  B)
```

    ## # A tibble: 3 × 3
    ##   month   Amy   Bob
    ##   <int> <dbl> <dbl>
    ## 1     1     6     5
    ## 2     2     7     6
    ## 3     3     8     7

``` r
dat  %>% Wide(student ~ month)
```

    ## Using B as value column: use value to override.

    ## # A tibble: 2 × 4
    ##   student   `1`   `2`   `3`
    ##   <chr>   <dbl> <dbl> <dbl>
    ## 1 Amy         6     7     8
    ## 2 Bob         5     6     7

``` r
dat  %>% Wide(month ~ student, A)
```

    ## # A tibble: 3 × 3
    ##   month   Amy   Bob
    ##   <int> <dbl> <dbl>
    ## 1     1    19    28
    ## 2     2    27    10
    ## 3     3    16    29

``` r
dat  %>% Wide(student ~ month, A)
```

    ## # A tibble: 2 × 4
    ##   student   `1`   `2`   `3`
    ##   <chr>   <dbl> <dbl> <dbl>
    ## 1 Amy        19    27    16
    ## 2 Bob        28    10    29

### Long

``` r
df2
```

    ## # A tibble: 3 × 5
    ##   month Amy_A Bob_A Amy_B Bob_B
    ##   <int> <dbl> <dbl> <dbl> <dbl>
    ## 1     1    19    28     6     5
    ## 2     2    27    10     7     6
    ## 3     3    16    29     8     7

``` r
df2  %>% Long(Amy_A, Amy_B, Bob_A, Bob_B, by =  ~ month)
```

    ## # A tibble: 12 × 3
    ##    month variable value
    ##    <int> <fct>    <dbl>
    ##  1     1 Amy_A       19
    ##  2     1 Amy_B        6
    ##  3     1 Bob_A       28
    ##  4     1 Bob_B        5
    ##  5     2 Amy_A       27
    ##  6     2 Amy_B        7
    ##  7     2 Bob_A       10
    ##  8     2 Bob_B        6
    ##  9     3 Amy_A       16
    ## 10     3 Amy_B        8
    ## 11     3 Bob_A       29
    ## 12     3 Bob_B        7

``` r
dat %>%
  tidyr::gather(variable, value,-(month:student)) %>%
  tidyr::unite(temp, student, variable) %>%
  tidyr::spread(temp, value)
```

    ##   month Amy_A Amy_B Bob_A Bob_B
    ## 1     1    19     6    28     5
    ## 2     2    27     7    10     6
    ## 3     3    16     8    29     7

Das geht nicht Mehr:

``` {
#  df_w2 <- Wide(df, student, c("A", "B")))

 stp25aggregate::Long(
       list(A=c("Amy_A", "Bob_A" ), 
            B=c("Amy_B", "Bob_B")), 
       df2,
       by =  ~ month,
       key = "student",
       key.levels= c("Amy", "Bob"))
```

### Pivot-Transpose

``` r
dat
```

    ##   pos x y
    ## 1   A 1 3
    ## 2   B 2 4
    ## 3   C 3 5

``` r
transpose2(dat)
```

    ##   Item A B C
    ## x    x 1 2 3
    ## y    y 3 4 5

``` r
transpose2(
  dat,
  key = "Item",
  col.names = c("A-level", "C-level", "D-Level"),
  row.names = c("x-axis", "y-axis")
)
```

    ##     Item A-level C-level D-Level
    ## x x-axis       1       2       3
    ## y y-axis       3       4       5

## add_to

Element zu Liste hinzufügen.

``` r
add_to(list(a = 1:3, b = LETTERS[1:5]),
       c = 1, d = 2)
```

    ## $a
    ## [1] 1 2 3
    ## 
    ## $b
    ## [1] "A" "B" "C" "D" "E"
    ## 
    ## $c
    ## [1] 1
    ## 
    ## $d
    ## [1] 2

Element zu Data-Frame hinzufügen.

``` r
df <-   data.frame(
Source = c("A", "B", "C", "F"),
x = 1:4,
y = 1:4,
stringsAsFactors = FALSE
)

 add_to(df, c("Erste Zeile" = 1, "Dritte" = 3))
```

    ##        Source  x  y
    ## 5 Erste Zeile NA NA
    ## 1           A  1  1
    ## 2           B  2  2
    ## 6      Dritte NA NA
    ## 3           C  3  3
    ## 4           F  4  4

``` r
add_to(df, "Erste Zeile")
```

    ##        Source  x  y
    ## 5 Erste Zeile NA NA
    ## 1           A  1  1
    ## 2           B  2  2
    ## 3           C  3  3
    ## 4           F  4  4

``` r
add_to(df, c("Erste Zeile", "Zweite"))
```

    ##        Source  x  y
    ## 5 Erste Zeile NA NA
    ## 1           A  1  1
    ## 6      Zweite NA NA
    ## 2           B  2  2
    ## 3           C  3  3
    ## 4           F  4  4

``` r
add_to(df, c("Erste Zeile" = 1, "letzte" = 5))
```

    ##        Source  x  y
    ## 5 Erste Zeile NA NA
    ## 1           A  1  1
    ## 2           B  2  2
    ## 3           C  3  3
    ## 4           F  4  4
    ## 6      letzte NA NA

``` r
add_to(df, list("G", 5), pos = -1)
```

    ##   Source x  y
    ## 1      A 1  1
    ## 2      B 2  2
    ## 3      C 3  3
    ## 4      F 4  4
    ## 5      G 5 NA

``` r
add_to(df, data.frame(  Source = c("G", "H"),
                        x = 5:6
), pos = -1)
```

    ##   Source x  y
    ## 1      A 1  1
    ## 2      B 2  2
    ## 3      C 3  3
    ## 4      F 4  4
    ## 5      G 5 NA
    ## 6      H 6 NA

## cbind() und rbind()

``` r
df1 = data.frame(CustomerId = c(1:6), Product = c(rep("Oven", 3), rep("Television", 3)))
df2 = data.frame(CustomerId = c(4:7), Product = c(rep("Television", 2), rep("Air conditioner", 2)))
df3 = data.frame(
   CustomerId = c(4:7),
   Product = c(rep("Television", 2), rep("Air conditioner", 2)),
   State = c(rep("California", 2), rep("New Jersey", 2))
 )

 Rbind2(df1, df3)
```

    ##    which CustomerId         Product      State
    ## 1    df1          1            Oven       <NA>
    ## 2    df1          2            Oven       <NA>
    ## 3    df1          3            Oven       <NA>
    ## 4    df1          4      Television       <NA>
    ## 5    df1          5      Television       <NA>
    ## 6    df1          6      Television       <NA>
    ## 7    df3          4      Television California
    ## 8    df3          5      Television California
    ## 9    df3          6 Air conditioner New Jersey
    ## 10   df3          7 Air conditioner New Jersey

``` r
 #dplyr::bind_rows(df1, df2)
```

## merge()

``` r
n<-10
df1 <- 
  data.frame(
  origin = sample(c("A", "B", "C", "D", "E"), n, replace = T),
  N = sample(seq(9, 27, 0.5), n, replace = T),
  P = sample(seq(0.3, 4, 0.1), n, replace = T),
  C = sample(seq(400, 500, 1), n, replace = T))
df2 <-
  data.frame(
    origin = sample(c("A", "B", "C", "D", "E"), n, replace = T),
    foo1 = sample(c(T, F), n, replace = T),
    X = sample(seq(145600, 148300, 100), n, replace = T),
    Y = sample(seq(349800, 398600, 100), n, replace = T))
df3 <-
  data.frame(origin = sample(c("A", "B", "C", "D", "E"), n, replace = T))
df4 <-
  data.frame(origin = sample(c("A", "B", "C", "D", "E"), n, replace = T))

df1$id <- df2$id <- df3$id <- df4$id <- paste("P", sprintf("%02d", c(1:n)), sep = "")  
 
Merge2(df1, df2, df3, df4, by = "id")
```

    ##     id origin.x    N   P   C origin.y  foo1      X      Y origin.z origin.u
    ## 1  P01        E 18.0 1.4 490        D FALSE 145800 386500        A        C
    ## 2  P02        C 25.5 2.6 495        B  TRUE 148300 395600        D        A
    ## 3  P03        A 12.0 0.3 433        A FALSE 145600 358600        D        D
    ## 4  P04        C 24.5 2.9 437        B  TRUE 147600 392700        A        C
    ## 5  P05        D 24.5 2.3 482        E  TRUE 148100 395600        C        E
    ## 6  P06        B 17.5 0.5 447        A  TRUE 145800 364300        E        E
    ## 7  P07        B 10.5 2.0 447        E FALSE 147700 373700        B        D
    ## 8  P08        B  9.0 0.9 457        D FALSE 146100 370300        C        C
    ## 9  P09        C 21.0 0.8 439        E FALSE 147300 376000        E        E
    ## 10 P10        E 16.0 0.5 406        E FALSE 147700 388600        E        E

## cbind data.frame aber listenweise

``` r
m <- data.frame(
  Item = 1:3,
  a = (1:3),
  b = (1:3) * 2,
  c = (1:3) * 3
)
sd <- data.frame(
  Item = (1:3),
  a = (1:3) * 4,
  b = (1:3) * 5,
  c = (1:3) * 6
)
combine_data_frame(m, sd)
```

    ##   Item a_m a_sd b_m b_sd c_m c_sd
    ## 1    1   1    4   2    5   3    6
    ## 2    2   2    8   4   10   6   12
    ## 3    3   3   12   6   15   9   18

``` r
combine_data_frame(m, sd, by = NULL)
```

    ##   Item_m Item_sd a_m a_sd b_m b_sd c_m c_sd
    ## 1      1       1   1    4   2    5   3    6
    ## 2      2       2   2    8   4   10   6   12
    ## 3      3       3   3   12   6   15   9   18

## list_to_df

``` r
x <- list(
  M1 = data.frame(
    Source = c("Intercept", "A", "B" , "C", "Residual"),
    b = c(0, 1, 2, 3, 0),
    y = c((1:4) + 10, 0)
  ),

  M2 = data.frame(
    Source = c("Intercept", "A", "C", "B",  "D",  "E", "Residual"),
    x = c(0, 1, 3, 2, 4, 5, 0),
    y = c((1:6) + 21, 0)
  ),
  M3 = data.frame(
    Source = c("A", "B", "C", "D", "Residual"),
    x = c((1:4), 0),
    y = c((1:4) + 20, 0)
  ),

  M1 = data.frame(
    Source = c("A", "B",  "D", "Residual"),
    x = c(1, 2, 4, 0),
    y = c((1:3) + 30, 0)
  )

)

# Spezialfall
list_to_df(x, last = "Residual")
```

    ##      Source M1_b M1_y M2_x M2_y M3_x M3_y M1.1_x M1.1_y
    ## 1 Intercept    0   11    0   22   NA   NA     NA     NA
    ## 2         A    1   12    1   23    1   21      1     31
    ## 3         B    2   13    2   25    2   22      2     32
    ## 4         C    3   14    3   24    3   23     NA     NA
    ## 6         D   NA   NA    4   26    4   24      4     33
    ## 7         E   NA   NA    5   27   NA   NA     NA     NA
    ## 5  Residual    0    0    0    0    0    0      0      0

``` r
fix_to_df(x)
```

    ##      Source M1_b M1_y M2_x M2_y M3_x M3_y M1.1_x M1.1_y
    ## 1 Intercept    0   11    0   22   NA   NA     NA     NA
    ## 2         A    1   12    1   23    1   21      1     31
    ## 3         B    2   13    2   25    2   22      2     32
    ## 4         C    3   14    3   24    3   23     NA     NA
    ## 5  Residual    0    0    0    0    0    0      0      0
    ## 6         D   NA   NA    4   26    4   24      4     33
    ## 7         E   NA   NA    5   27   NA   NA     NA     NA

``` r
data(infert, package = "datasets")
infert$case  <- factor(infert$case ,1:0, c("case", "control") )

#infert$spontaneous <- factor(infert$spontaneous)
#infert$induced2    <- factor(infert$induced==0)
# tab_1<- xtabs(~  case, infert)
# tab_2x2<- xtabs(~ induced2 + case, infert)
tab_3x2<- xtabs(~ induced + case, infert)
# tab_3x3<- xtabs(~ induced + education, infert)
# tab_3x3x2<- xtabs(~ induced + education+case, infert)
# tab_3x2x3<- xtabs(~ induced +case+ education, infert)
 
tab_3x2
```

    ##        case
    ## induced case control
    ##       0   47      96
    ##       1   23      45
    ##       2   13      24

``` r
fix_to_df(tab_3x2)
```

    ##   induced case_case case_control
    ## 1       0        47           96
    ## 2       1        23           45
    ## 3       2        13           24

## auto_trans

Automatische Taranformation von numerischen Variablen entsprechend ihere
Verteilungseigenschaft

``` r
x <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 7, 7, 7, 7, 9, 20, 30)
x_trans<-auto_trans(x)
auto_trans(x_trans)
```

    ##  [1]  1  1  1  1  2  2  2  2  3  3  3  3  3  3  7  7  7  7  9 20 30
    ## attr(,"name")
    ## [1] "Re-trans"

``` r
x <- 100 - x
auto_trans(x)
```

    ##  [1] 0.6931472 0.6931472 0.6931472 0.6931472 1.0986123 1.0986123 1.0986123
    ##  [8] 1.0986123 1.3862944 1.3862944 1.3862944 1.3862944 1.3862944 1.3862944
    ## [15] 2.0794415 2.0794415 2.0794415 2.0794415 2.3025851 3.0445224 3.4339872
    ## attr(,"link")
    ## function(x)
    ##   log(101 - x)
    ## <bytecode: 0x000001d1c9efcb38>
    ## <environment: namespace:stp25tools>
    ## attr(,"inverse")
    ## function(x)
    ##   101 - (exp(x))
    ## <bytecode: 0x000001d1c9efb7f8>
    ## <environment: namespace:stp25tools>
    ## attr(,"name")
    ## [1] "negative skew (max-Log)"

``` r
par(mfrow=c(2,2))

boxplot(beta~group, dat)
boxplot(auto_trans(beta, treshhold = .4)~group, dat)
boxplot(exp~group, dat)
boxplot(auto_trans(exp, treshhold = .4)~group, dat)
```

![](README_files/figure-gfm/boxplots-1.png)<!-- -->

``` r
#boxplot(weibull~group, dat)
#boxplot(unif~group, dat)
```

``` r
for(i in 1:4){

  dat$x<- dat[[i]]
  fit0 <- lm(x~ group, dat)
  dat$x<- auto_trans(dat$x, treshhold = .4)
  fit1 <- lm(x~ group, dat)



  p0<-plot(effects::effect("group", fit0),
           main="orginal",
           ylab = names(dat)[i])
  p1<-plot(effects::effect("group", fit1),
           main="non-trans",
           ylab = "")
  p2<-plot(effects::effect("group", fit1,
                  transformation =
                    list(link = attr(dat$x, "link"),
                         inverse = attr(dat$x, "inverse"))),
           main=attr(dat$x, "name"),
           ylab = ""
           )
   gridExtra::grid.arrange(p0, p1, p2, ncol = 3)
}
```

![](README_files/figure-gfm/model-fit-1.png)<!-- -->![](README_files/figure-gfm/model-fit-2.png)<!-- -->![](README_files/figure-gfm/model-fit-3.png)<!-- -->![](README_files/figure-gfm/model-fit-4.png)<!-- -->

## separate multiple choice

Aufdroeseln von Mehrfachantworten

``` r
 #require(stp25tools)
lbl <- c (
  '1 Heart Failure' = 1,
  '2 Rhythm Abnormality' = 2,
  '3 Valve Dysfunction' = 3,
  '4 Bleeding with OAC' = 4,
  '5 ACS' = 5,
  '6 Neurological Event' = 6,
  '7 Neoplastic Disease' = 7,
  '8 Others' = 8,
  '0 No Complications' = 0
)

x <- c(0,
       "1,3,6,8,4",
       2,
       "",
       "8,4",
       #  3.8,100,
       "4,6,8,3",
       "2,3,4,5")

#x <- gsub("\\.", ",", x)
rslt <-
  separate_multiple_choice(x ,
                           sep = ",",
                           as_logical = TRUE,
                           label = lbl)
```

    ## 
    ## ----------------------------------------------------------------
    ## Warnung: wenn komische Leerzeichen daher kommen gut aufpassen!
    ## Das was unten kommt wird aufgedröselt.
    ## [1] "0"         "1,3,6,8,4" "2"         "2,3,4,5"   "4,6,8,3"   "8,4"      
    ## [7] "zz_9999"  
    ## 
    ## ----------------------------------------------------------------

    ## Warning: Expected 9 pieces. Missing pieces filled with `NA` in 7 rows [1, 2, 3, 4, 5, 6,
    ## 7].

``` r
stp25stat2::Tbll_desc(rslt)
```

    ## 
    ## Hallo Wolfgang!

    ## # A tibble: 8 × 3
    ##   Item                         n     m      
    ## * <chr>                        <chr> <chr>  
    ## 1 "0 No Complications true "   6     17% (1)
    ## 2 "1 Heart Failure true "      6     17% (1)
    ## 3 "2 Rhythm Abnormality true " 6     33% (2)
    ## 4 "3 Valve Dysfunction true "  6     50% (3)
    ## 5 "4 Bleeding with OAC true "  6     67% (4)
    ## 6 "5 ACS true "                6     17% (1)
    ## 7 "6 Neurological Event true " 6     33% (2)
    ## 8 "8 Others true "             6     50% (3)

## Creation of dummy variables and reverse

``` r
 z <- gl(3, 2, 12, labels = c("apple", "salad", "orange"))
table(z)
```

    ## z
    ##  apple  salad orange 
    ##      4      4      4

``` r
levels(z) <- list("veg"   = "salad", "fruit" = c("apple", "orange"))
table(z)
```

    ## z
    ##   veg fruit 
    ##     4     8

``` r
z <- factor_to_dummy(z)
table(z)
```

    ##    fruit
    ## veg 0 1
    ##   0 0 8
    ##   1 4 0

``` r
z <- dummy_to_factor(z)
table(z)
```

    ## z
    ##   veg fruit 
    ##     4     8
