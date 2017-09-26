# STATISTICS - GROUP COMPARISONS
The aim of this repository is to show the statistical workflow when comparing groups of data in order to prove if there is significant difference between them.
As data I provide some borrowed from my [master thesis](https://www.linkedin.com/in/odeibarredo/detail/treasury/summary/?entityUrn=urn%3Ali%3Afs_treasuryMedia%3A(ACoAACDk650BBiDdPC-DOcNkGvDFwViWdAt4Htc%2C1499870688517)) (in spanish), which consisted in a first approach to establishing background tissue concentration in macroinvertebrates of rivers from mining areas of northern Spain. This time we will only perform a statistical analysis to compare three different taxa to set if their metal levels really differ between them. The three taxa are: [Heptageniidae](http://eol.org/pages/2762776/details) (scraper), [Hydropsychidae](http://eol.org/pages/1125/overview) (collector-filterer) and [Rhyacophilidae](http://eol.org/pages/1147/overview) (predator).

![alt text](https://github.com/odeibarredo/Statistics_Group-Comparisons-IN-PROGRESS-/blob/master/img/01%20macro.jpg)

## WORKFLOW

![alt text](https://github.com/odeibarredo/Statistics_Group-Comparisons-IN-PROGRESS-/blob/master/img/02%20analysis%20flowchart.jpg)

### DESCRIPTIVE ANALYSIS

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

In the next step we will prove if our hypothesis is true or not.

### STATISTICAL ANALYSIS

#### NORMAL DISTRIBUTION

To know which statistical test we need to use we have to know if our data follows a **normal distribution or not**. For this case we'll use three approaches:
- [Skewness](https://en.wikipedia.org/wiki/Skewness) must be in range of -2 and 2
- p value os [Shapiro-Wilk](https://en.wikipedia.org/wiki/Shapiro%E2%80%93Wilk_test) must be > 0.01
- p value of [Levene's homogeneity of variance](https://en.wikipedia.org/wiki/Levene%27s_test) test must be > 0.01

This are the results:

```r
--------------------------------------------------
      Taxa        Metal   Shapiro.Wilk   Skewness 
---------------- ------- -------------- ----------
 Heptageniidae     As        0.292         0.23   

 Heptageniidae     Se        0.216         0.77   

 Heptageniidae     Cd        0.456         0.3    

 Heptageniidae     Hg       *0.009*        1.54   

 Hydropsychidae    As       *0.004*        0.68   

 Hydropsychidae    Se        0.334         0.76   

 Hydropsychidae    Cd        0.106       **2.6**  

 Hydropsychidae    Hg         *0*          1.74   

 Rhyacophilidae    As       *0.001*        0.28   

 Rhyacophilidae    Se         *0*          0.33   

 Rhyacophilidae    Cd         *0*          1.81   

 Rhyacophilidae    Hg       *0.001*        1.53   
--------------------------------------------------

Table: *p<0.01; **-2<p>2


---------------------
 Metal   Levene.Test 
------- -------------
  As         *0*     

  Se         *0*     

  Cd         *0*     

  Hg        0.154    
---------------------

Table: *p<0.01

```

As we can see, some of the varaibles adjust to normal distribution and others don't. To better understand the difference we'll create some visualization for each one of the tests. For **Shapiro-Wilk** we can perform a [QQ plot](https://en.wikipedia.org/wiki/Q%E2%80%93Q_plot), which is commonly used to detect deviations from the normal distribution.

![alt text](https://github.com/odeibarredo/Statistics_Group-Comparisons-IN-PROGRESS-/blob/master/img/04%20QQplots.png)

Cd in Heptageniidae fits the QQplot, Cd in Rhyacophilidae doesn't.

We can visualize the **skweness** with histograms.

![alt text](https://github.com/odeibarredo/Statistics_Group-Comparisons-IN-PROGRESS-/blob/master/img/05%20skewness.jpg)

In the case of Hydropsychidae the density line is skewed to the left.

Finally to better understand the **Levene's test** we can build some boxplots and pay attention to the whiskers.

![alt text](https://github.com/odeibarredo/Statistics_Group-Comparisons-IN-PROGRESS-/blob/master/img/06%20boxplot_simple.png)

Heptageniidae shows a lot of variance in all metals except in Hg, that's why Levene fails for the first three metals.

 _**NOTE:**
One important thing to consider is the amount of samples that we are working with, less than 30 for each metal and taxa. The smallest the population the harder the probability of having normal ditribution. Outliers can have a big impact._

#### DATA TRANSFORMATION AND SECOND ROUND CHECKING DISTRIBUTION

None of the metals pass the three test we stablished on all taxa. Transforming the data we may achieve normal distribution, so that's
the next step. Take note though, that in Cd in Hydropsychidae showed **skewness, a parameter that does not change even if the data is 
transformed**,so we already know that the data referring to Cd must be treated with **non-parametric test**.

After a logarithm transformation this are the results for SW and Levene:

```r
---------------------------------------
      Taxa        Metal   Shapiro.Wilk 
---------------- ------- --------------
 Heptageniidae     As         *0*      

 Heptageniidae     Se        0.912     

 Heptageniidae     Cd        0.449     

 Heptageniidae     Hg         0.18     

 Hydropsychidae    As        0.243     

 Hydropsychidae    Se        0.383     

 Hydropsychidae    Cd       *0.002*    

 Hydropsychidae    Hg        0.162     

 Rhyacophilidae    As        0.959     

 Rhyacophilidae    Se        0.315     

 Rhyacophilidae    Cd        0.815     

 Rhyacophilidae    Hg        0.365     
---------------------------------------

Table: *p<0.01

---------------------
 Metal   Levene.Test 
------- -------------
  As        0.609    

  Se        0.597    

  Cd        0.551    

  Hg        0.919    
---------------------

Table: *p<0.01
```

So now, all cases pass the Levene's Test, and only two cases do not pass the SW -> As and Cd. This means that we can **perform parametric test with the transformed data for Se and Hg, and non-parametric test for As and Cd with original data**.

#### SIGNIFFICANT DIFFERENCES?
##### PARAMETRIC TESTS

First we run a **global test** to see if there are signfficant differences between groups, in this case an [ANOVA](https://en.wikipedia.org/wiki/Analysis_of_variance). If the the **_p value_ is <0.05 there are differences**, and we have to perform a **paired comparison** as a **post-hoc** analysis, in this case [Bonferroni](https://www.google.es/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=0ahUKEwjPqb_M3cLWAhVYFMAKHSiwCIAQFggmMAA&url=https%3A%2F%2Fen.wikipedia.org%2Fwiki%2FBonferroni_correction&usg=AFQjCNHhNP_wLeSijzDEnOTgTtm4zF25Fw).

Results:

```r
---------------
 Metal   Anova 
------- -------
  Se      *0*  

  Hg     0.057 
---------------

Table: *p<0.05

--------------------------------------------------
          Comparisons               Se       Hg   
-------------------------------- --------- -------
 Heptageniidae - Hydropsychidae     *0*     0.068 

 Heptageniidae - Rhyacophilidae   *0.004*   0.28  

 Hydropsychidae - Rhyacophilidae  *0.001*     1                            
--------------------------------------------------

Table: *p<0.05
```


##### NON-PARAMETRIC TESTS

As non-parametric tests we'll use [Kruskal-Wallis](https://en.wikipedia.org/wiki/Kruskal%E2%80%93Wallis_one-way_analysis_of_variance) as global comparation and a Dunn's Test as post hoc paired comparison.

Results:

```r
------------------------
 Metal   Kruskal.Wallis 
------- ----------------
  As          *0*       

  Cd          *0*       
------------------------

Table: *p<0.05

--------------------------------------------------
          Comparisons               As       Cd   
-------------------------------- --------- -------
 Heptageniidae - Hydropsychidae     *0*      *0*  

 Heptageniidae - Rhyacophilidae     *0*      *0*  

 Hydropsychidae - Rhyacophilidae  *0.001*   0.079 
 --------------------------------------------------

Table: *p<0.05

```

So now we know which cases have signifficant differences we can visualize the boxplots from before with annotations of this cases

![alt text](https://github.com/odeibarredo/Statistics_Group-Comparisons-IN-PROGRESS-/blob/master/img/07%20boxplot_diff.png)

### PCA
As a final step we can check the correlation between the metals and the taxa with a PCA

![alt text](https://github.com/odeibarredo/Statistics_Group-Comparisons-IN-PROGRESS-/blob/master/img/08%20PCA.png)

So most influencial metals in taxa differentiation are As and Se; Cd has a bit of differentiation power towards Heptageniidae; Hg shows same accumulation levels in three taxa.

# THE END!!! #
