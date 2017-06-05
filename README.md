
<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Build Status](https://travis-ci.org/kota7/striprtf.svg?branch=master)](https://travis-ci.org/kota7/striprtf) [![CRAN Status](https://www.r-pkg.org/badges/version/striprtf)](https://www.r-pkg.org/badges/version/striprtf) [![](http://cranlogs.r-pkg.org/badges/striprtf)](https://cran.r-project.org/package=striprtf)

striprtf: Extract Text from RTF (Rich Text Format) File
=======================================================

Installation
------------

This package is now on CRAN.

``` r
install.packages("striprtf")
```

Alternatively, install development version from Github using `devtools` library.

``` r
devtools::install_github("kota7/striprtf")
```

Usage
-----

The package exports two main functions:

-   `read_rtf` takes a path to a Rich Text Format (RTF) file and extracts plain text out of it.
-   `strip_rtf` does the same with string input instead of file path.

``` r
library(striprtf)
x <- read_rtf(system.file("extdata/king.rtf", package = "striprtf"))
head(x)
#> [1] "I am happy to join with you today in what will go down in history as the greatest demonstration for freedom in the history of our nation."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> [2] "Five score years ago, a great American, in whose symbolic shadow we stand today, signed the Emancipation Proclamation. This momentous decree came as a great beacon light of hope to millions of Negro slaves who had been seared in the flames of withering injustice. It came as a joyous daybreak to end the long night of their captivity."                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
#> [3] "But 100 years later, the Negro still is not free. One hundred years later, the life of the Negro is still sadly crippled by the manacles of segregation and the chains of discrimination. One hundred years later, the Negro lives on a lonely island of poverty in the midst of a vast ocean of material prosperity. One hundred years later, the Negro is still languished in the corners of American society and finds himself an exile in his own land. And so we've come here today to dramatize a shameful condition."                                                                                                                                                                                                                                                                                                                                 
#> [4] "In a sense we've come to our nation's capital to cash a check. When the architects of our republic wrote the magnificent words of the Constitution and the Declaration of Independence, they were signing a promissory note to which every American was to fall heir. This note was a promise that all men -- yes, black men as well as white men -- would be guaranteed the unalienable rights of life, liberty, and the pursuit of happiness."                                                                                                                                                                                                                                                                                                                                                                                                             
#> [5] "It is obvious today that America has defaulted on this promissory note insofar as her citizens of color are concerned. Instead of honoring this sacred obligation, America has given the Negro people a bad check, a check that has come back marked \"insufficient funds.\""                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
#> [6] "But we refuse to believe that the bank of justice is bankrupt. We refuse to believe that there are insufficient funds in the great vaults of opportunity of this nation. And so we've come to cash this check, a check that will give us upon demand the riches of freedom and security of justice. We have also come to this hallowed spot to remind America of the fierce urgency of now. This is no time to engage in the luxury of cooling off or to take the tranquilizing drug of gradualism. Now is the time to make real the promises of democracy. Now is the time to rise from the dark and desolate valley of segregation to the sunlit path of racial justice. Now is the time to lift our nation from the quicksands of racial injustice to the solid rock of brotherhood. Now is the time to make justice a reality for all of God's children."
```

The package has also been tested with documents in East Asian languages.

``` r
read_rtf(system.file("extdata/amenimo.rtf", package = "striprtf"))
#>  [1] "雨ニモマケズ"                     "風ニモマケズ"                    
#>  [3] "雪ニモ夏ノ暑サニモマケヌ"         "丈夫ナカラダヲモチ"              
#>  [5] "慾ハナク"                         "決シテ瞋ラズ"                    
#>  [7] "イツモシヅカニワラッテヰル"       "一日ニ玄米四合ト"                
#>  [9] "味噌ト少シノ野菜ヲタベ"           "アラユルコトヲ"                  
#> [11] "ジブンヲカンジョウニ入レズニ"     "ヨクミキキシワカリ"              
#> [13] "ソシテワスレズ"                   "野原ノ松ノ林ノノ"                
#> [15] "小サナ萓ブキノ小屋ニヰテ"         "東ニ病気ノコドモアレバ"          
#> [17] "行ッテ看病シテヤリ"               "西ニツカレタ母アレバ"            
#> [19] "行ッテソノ稲ノ朿ヲ負ヒ"           "南ニ死ニサウナ人アレバ"          
#> [21] "行ッテコハガラナクテモイヽトイヒ" "北ニケンクヮヤソショウガアレバ"  
#> [23] "ツマラナイカラヤメロトイヒ"       "ヒドリノトキハナミダヲナガシ"    
#> [25] "サムサノナツハオロオロアルキ"     "ミンナニデクノボートヨバレ"      
#> [27] "ホメラレモセズ"                   "クニモサレズ"                    
#> [29] "サウイフモノニ"                   "ワタシハナリタイ"                
#> [31] ""                                 "南無無辺行菩薩"                  
#> [33] "南無上行菩薩"                     "南無多宝如来"                    
#> [35] "南無妙法蓮華経"                   "南無釈迦牟尼仏"                  
#> [37] "南無浄行菩薩"                     "南無安立行菩薩"                  
#> [39] ""                                 ""
read_rtf(system.file("extdata/mean.rtf", package = "striprtf"))
#> [1] "詩曰：「衣錦尚絅」，惡其文之著也。故君子之道，闇然而日章；小人之道，的然而日亡。君子之道，淡而不厭，簡而文，溫而理，知遠之近，知風之自，知微之顯，可與入德矣。"
#> [2] ""                                                                                                                                                              
#> [3] "『中庸』　Doctrine of the Mean"                                                                                                                                
#> [4] ""                                                                                                                                                              
#> [5] ""
```

Important Change in the Function Names
--------------------------------------

From ver 0.3.1, the functions are renamed as follows:

-   `striprtf` --&gt; `read_rtf`
-   `rtf2text` --&gt; `strip_rtf`

See NEWS for other updates.

Tables (v0.4.1+)
----------------

Supports tables in documents. Use `row_start`, `row_end`, `cell_end` arguments to adjust the format the tables. Suppports line breaks (and other special characters) within cells.

The parser is made robust from v0.4.5.
Tested with files generated by Microsoft Word, Google Doc, and Libre Office Writer.

``` r
# example file added at v0.4.2
read_rtf(system.file("extdata/shakespeare.rtf", package = "striprtf"),
         row_start = "**", row_end = "", cell_end = " --- ")
#> [1] "Shakespeare quotes"                                                                                                                                            
#> [2] ""                                                                                                                                                              
#> [3] "**The Tempest --- ﻿We are such stuff as dreams are made on, \nand our little life is rounded with a sleep. --- "                                                
#> [4] "**Hamlet --- ﻿There is nothing either good or bad, \nbut thinking makes it so. --- "                                                                            
#> [5] "**Romeo and Juliet --- ﻿Swear not by the moon, the inconstant moon,\nThat monthly changes in her circled orb,\nLest that thy love prove likewise variable. --- "
#> [6] ""                                                                                                                                                              
#> [7] ""                                                                                                                                                              
#> [8] ""                                                                                                                                                              
#> [9] ""
```

Note:

-   No support for nested tables
-   No support for merged cells

References
----------

-   [Stack overflow thread](http://stackoverflow.com/a/188877) where the algorithm has been discussed.
-   [Gilson Filho's implementation for python 3](https://gist.github.com/gilsondev/7c1d2d753ddb522e7bc22511cfb08676)
-   [RTF specification 1.5](http://www.biblioscape.com/rtf15_spec.htm)
-   [RTF specification latest](https://www.microsoft.com/en-us/download/details.aspx?id=10725)
