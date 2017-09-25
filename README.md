# Statistics - Group Comparisons
The aim of this repository is to show the statistical workflow when comparing groups of data in order to prove if there is significant difference between them.
As data I provide some borrowed from my [master thesis](https://www.linkedin.com/in/odeibarredo/detail/treasury/summary/?entityUrn=urn%3Ali%3Afs_treasuryMedia%3A(ACoAACDk650BBiDdPC-DOcNkGvDFwViWdAt4Htc%2C1499870688517)) (in spanish), which consisted in a first approach to establishing background tissue concentration in macroinvertebrates of rivers from mining areas of northern Spain. This time we will only perform a statistical analysis to compare three different taxa to set if their metal levels really differ between them. The three taxa are: [Heptageniidae](http://eol.org/pages/2762776/details) (scraper), [Hydropsychidae](http://eol.org/pages/1125/overview) (collector-filterer) and [Rhyacophilidae](http://eol.org/pages/1147/overview) (predator).

![alt text](https://github.com/odeibarredo/Statistics_Group-Comparisons-IN-PROGRESS-/blob/master/img/01%20macro.jpg)

## Workflow

![alt text](https://github.com/odeibarredo/Statistics_Group-Comparisons-IN-PROGRESS-/blob/master/img/02%20analysis%20flowchart.jpg)

The first step is always to take a quick view of the basic features of the data, perfoming some descriptive analysis.

```r
> summary(mydata) # quick first look at global data

 Taxa                As               Se               Cd              Hg        
 Heptageniidae :27   Min.   :0.1100   Min.   : 0.500   Min.   :0.010   Min.   :0.0300  
 Hydropsychidae:23   1st Qu.:0.6325   1st Qu.: 1.930   1st Qu.:0.080   1st Qu.:0.0700  
 Rhyacophilidae:20   Median :1.0900   Median : 3.605   Median :0.255   Median :0.1000  
                     Mean   :1.9741   Mean   : 4.961   Mean   :0.622   Mean   :0.1359  
                     3rd Qu.:2.9775   3rd Qu.: 6.478   3rd Qu.:0.865   3rd Qu.:0.1500  
                     Max.   :7.2300   Max.   :20.280   Max.   :2.820   Max.   :0.5200  
```


![alt text](https://github.com/odeibarredo/Statistics_Group-Comparisons-IN-PROGRESS-/blob/master/img/03%20descriptive_plot.png)


At first look we could think that there may be signifficant differences in the next cases:
- As -> the three taxas
- Se -> Hepta vs Hydro and Rhya
- Cd -> Hepta vs Hydro and Rhya
- Hg -> none

In the next step we will prove if our hypothesis is true or not. To know whics statistical test we need to use we have to know if our data follows a normal distribution or not. For this case we use three approaches:
- [Skewness](https://en.wikipedia.org/wiki/Skewness) must be in range of -2 and 2
- p value os [Shapiro-Wilk](https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test) must be > 0.01
- p value of [Levene's homogeneity of variance](https://en.wikipedia.org/wiki/Levene%27s_test) test must be > 0.01


