# Statistics_Group-Comarisons
The aim of this repository is to show the statistical workflow when comparing groups of data in order to prove if there is significant difference between them.
As data I provide some borrowed from my [master thesis](https://www.linkedin.com/in/odeibarredo/detail/treasury/summary/?entityUrn=urn%3Ali%3Afs_treasuryMedia%3A(ACoAACDk650BBiDdPC-DOcNkGvDFwViWdAt4Htc%2C1499870688517)) (in spanish), which consisted in a first approach to establishing background tissue concentration in macroinvertebrates of rivers from mining areas of northern Spain. This time we will only perform a statistical analysis to compare three different taxa to set if their metal levels really differ between them. The three taxa are: Heptageniidae (scraper), Hydropsychidae (collector-filterer) and Rhyacophilidae (predator).

IMAgen macro

## Workflow

imagen WORKFLOW

The first step is always to take a quick view of the basic features of the data, perfoming som descriptive analysis.

CODIGO SUMMARY

IMAGEN DESCRIP

At first look we could think that there may be signifficant differences in the next cases:
- As -> the three taxas
- Se -> Hepta vs Hydro and Rhya
- Cd -> Hepta vs Hydro and Rhya
- Hg -> none

In the next step we will prove if our hypothesis is true or not. To know whics statistical test we need to use we have to know if our data follows a normal distribution or not. For this case we use three approaches:
- Skewness must be in range of -2 and 2
- p value os Shapiro-Wilk must be > 0.01
- p value of [Levene's homogeneity of variance](https://en.wikipedia.org/wiki/Levene%27s_test) test must be > 0.01

[Levene's homogeneity of variance](https://en.wikipedia.org/wiki/Levene%27s_test)
