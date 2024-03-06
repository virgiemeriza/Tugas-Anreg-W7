##### *Meriza Immanuela Virgie - G1401221076*

``` r
library(readxl)
anreg <- read_xlsx("C:/Users/mivir/Documents/data anreg w7.xlsx")
View(anreg)
```

## **PLOT DATA**

``` r
plot(anreg$Y~anreg$X)
```

![](Tugas-anreg-w7_files/figure-markdown_github/unnamed-chunk-2-1.png)

Dari scatter plot tersebut, terlihat bahwa peubah x dan peubah y tidak
berhubungan linier

## **MODEL AWAL**

``` r
library(lmtest)
modelreg <- lm(formula = Y~X, data = anreg)
summary(modelreg)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ X, data = anreg)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -7.1628 -4.7313 -0.9253  3.7386  9.0446 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 46.46041    2.76218   16.82 3.33e-10 ***
    ## X           -0.75251    0.07502  -10.03 1.74e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 5.891 on 13 degrees of freedom
    ## Multiple R-squared:  0.8856, Adjusted R-squared:  0.8768 
    ## F-statistic: 100.6 on 1 and 13 DF,  p-value: 1.736e-07

``` r
modelreg
```

    ## 
    ## Call:
    ## lm(formula = Y ~ X, data = anreg)
    ## 
    ## Coefficients:
    ## (Intercept)            X  
    ##     46.4604      -0.7525

Karena p-value \< 0.05 (α), maka dapat dikatakan bahwa x mempengaruhi y
pada taraf nyata 5%. Nilai adj R-squared menggambarkan bagaimana nilai x
dapat menjelaskan keragaman nilai y, pada konteks ini sebesar 0,8768
atau sebesar 87,68%.

``` r
error = modelreg$residuals
error
```

    ##          1          2          3          4          5          6          7 
    ##  9.0446035  7.3021275  3.8071435 -1.9353325 -0.9253005 -7.1627605 -6.8952045 
    ##          8          9         10         11         12         13         14 
    ## -7.1326645 -2.8751405 -4.8651085 -4.5975525  3.6700035  1.4225115  2.6900675 
    ##         15 
    ##  8.4526075

## **PENGUJIAN ASUMSI**

#### UJI KENORMALAN

*H*0
= Sisaan menyebar normal

*H*1 = Sisaan tidak menyebar normal

``` r
library(plotly)
qqnorm(anreg$Y)
qqline(anreg$Y, col = "blue")
```

![](Tugas-anreg-w7_files/figure-markdown_github/unnamed-chunk-5-1.png)

``` r
shapiro.test(error)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  error
    ## W = 0.92457, p-value = 0.226

Karena p-value \< 0.05, maka tidak tolak *H*0 yang berarti cukup bukti
untuk menyatakan bahwa data menyebar normal. Sehingga asumsi kenormalan
data terpenuhi

#### UJI AUTOKORELASI

*H*0 = Tidak ada autokorelasi

*H*1 = Terdapat autokorelasi

``` r
dwtest(modelreg)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  modelreg
    ## DW = 0.48462, p-value = 1.333e-05
    ## alternative hypothesis: true autocorrelation is greater than 0

Didapatkan p-value \< 0.05, maka tolak *H*0 yang berarti tidak cukup
bukti untuk menyatakan bahwa tidak ada autokorelasi dalam residual.
Sehingga asumsi tidak terpenuhi

#### UJI HOMOKEDASTISITAS

*H*0 = Varian sisaan konstan

*H*1 = Varian sisaan tidak konstan

``` r
bptest(modelreg, studentize = TRUE, data = anreg)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  modelreg
    ## BP = 0.52819, df = 1, p-value = 0.4674

Dari hasil uji tersebut, terlihat bahwa p-value \> 0.05, maka tidak
tolak *H*0 yang berarti cukup bukti bahwa varian sisaan konstan.
Sehingga, asumsi terpenuhi

## KESIMPULAN DAN TRANSFORMASI

Dapat disimpulkan bahwa dari semua pengujian yang dilakukan, ditemukan
bahwa ada satu asumsi yang tidak terpenuhi. Masih terdapat autokorelasi
yang terjadi dalam data.

Kemudian akan dilakukan tranformasi data untuk dapat memenuhi asumsi
tersebut. Karena nilai B1 (-0,75251) \< 0, maka transformasi dilakukan
dengan mengecilkan nilai X dan/atau Y dengan membuat menjadi pangkat 1/2
atau akar dari data asli.

#### TRANSFORMASI

``` r
x.trans <- sqrt(anreg$X)
x.trans
```

    ##  [1] 1.414214 2.236068 2.645751 3.162278 3.741657 4.358899 5.099020 5.567764
    ##  [9] 5.830952 6.164414 6.708204 7.211103 7.280110 7.745967 8.062258

``` r
y.trans <- sqrt(anreg$Y)
y.trans
```

    ##  [1] 7.348469 7.071068 6.708204 6.082763 5.916080 5.000000 4.472136 4.000000
    ##  [9] 4.242641 3.605551 2.828427 3.316625 2.828427 2.000000 2.449490

``` r
model.trans <- lm(y.trans~x.trans, data = anreg)
model.trans
```

    ## 
    ## Call:
    ## lm(formula = y.trans ~ x.trans, data = anreg)
    ## 
    ## Coefficients:
    ## (Intercept)      x.trans  
    ##      8.7125      -0.8134

#### UJI AUTOKORELASI

``` r
dwtest(model.trans)
```

    ## 
    ##  Durbin-Watson test
    ## 
    ## data:  model.trans
    ## DW = 2.6803, p-value = 0.8629
    ## alternative hypothesis: true autocorrelation is greater than 0

p-value yang didapat adalah 0.8629 di mana \> 0.05, maka tidak tolak H0.
Hal tersebut berarti tidak terjadi autokorelasi pada data yang sudah
ditransformasi

#### REGRESI SETELAH TRANSFORMASI

``` r
plot(y.trans~x.trans)
```

![](Tugas-anreg-w7_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
summary(model.trans)
```

    ## 
    ## Call:
    ## lm(formula = y.trans ~ x.trans, data = anreg)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.42765 -0.17534 -0.05753  0.21223  0.46960 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  8.71245    0.19101   45.61 9.83e-16 ***
    ## x.trans     -0.81339    0.03445  -23.61 4.64e-12 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2743 on 13 degrees of freedom
    ## Multiple R-squared:  0.9772, Adjusted R-squared:  0.9755 
    ## F-statistic: 557.3 on 1 and 13 DF,  p-value: 4.643e-12

Model Regresi Terbaik (Transformasi):
*Y*.*d**u**g**a* = 8.71245 − 0.81339*x* + *e*
Model tersebut berarti bahwa setiap 1 kenaikan nilai x, akan menurunkan
nilai rata-rata Y sebanyak 0.81339. Sedangkan nilai 8.71245 (B0)
merupakan nilai rata-rata Y ketika x bernilai 0
