
# polypharmacy <img src="man/figures/sticker/stiker2.png" width = "150" align="right" />

This package analyse prescription drugs deliveries to calculate several
indicators of polypharmacy corresponding to the various definitions
found in the literature.

It is essential to know the concepts used to calculate the various
polypharmacy indicators to adequately use this package.  
The core of the package is the `data_process()` function that creates
the `data.table` of pharmacists drug deliveries by restructuring the
drug delivery records (usually extracted from a pharmacy or a health
insurance information system) into continuous periods of drug
availability, applying user-defined arguments such as the grace periods
between renewals or the longest treatment duration that an individual
may accumulate through the successive renewals.

Then, each polypharmacy indicator can be computed using the
corresponding function (`ind_simult()`, `ind_stdcumul()`,
`ind_wcumul()`, `ind_stdcontinuous()`, `ind_ucontinuous()`) or using the
overall function `indicators()` and select all the desired indicator(s)
to be calculated at once.

Prior to running `data_process()` the user may need to pre-process the
table of original drug delivery records to break down combination drug
into their individual components (`drugs_bkdn()`) and/or to overwrite
the treatment duration of specified drugs with constant time periods
(`cst_trt_dur()`).

## Installation

> Waiting for first CRAN submit.

## Development version

To get a bug fix or to use a feature from the development version, you
can install the development version of `polypharamcy` from GitHub.

``` r
# install.packages("remotes")
remotes::install_github("guiboucher/polypharmacy")
```

## References

-   Bjerrum, L., Rosholm, J. U., Hallas, J., & Kragstrup, J. (1997).
    Methods for estimating the occurrence of polypharmacy by means of a
    prescription database. \[journal article\]. European Journal of
    Clinical Pharmacology, 53(1), 7-11. DOI:
    [10.1007/s002280050329](https://link.springer.com/article/10.1007/s002280050329)
-   Chan, D.-C., Hao, Y.-T., & Wu, S.-C. (2009a). Characteristics of
    outpatient prescriptions for frail Taiwanese elders with long-term
    care needs. Pharmacoepidemiology and Drug Safety, 18(4), 327-334.
    DOI:
    [10.1002/pds.1712](https://onlinelibrary.wiley.com/doi/abs/10.1002/pds.1712)
-   Fincke, B. G., Snyder, K., Cantillon, C., Gaehde, S., Standring, P.,
    Fiore, L., . . . Gagnon, D. R. (2005). Three complementary
    definitions of polypharmacy: methods, application and comparison of
    findings in a large prescription database. Pharmacoepidemiol Drug
    Saf, 14(2), 121-128. DOI:
    [10.1002/pds.966](https://onlinelibrary.wiley.com/doi/abs/10.1002/pds.966)
-   Hovstadius, B., Astrand, B., & Petersson, G. (2009). Dispensed drugs
    and multiple medications in the Swedish population: an
    individual-based register study.(Technical advance)(Report). BMC
    Clinical Pharmacology, 9(11), 11. DOI:
    [10.1186/1472-6904-9-11](https://link.springer.com/article/10.1186%2F1472-6904-9-11)
-   Hovstadius, B., Astrand, B., & Petersson, G. (2010). Assessment of
    regional variation in polypharmacy. Pharmacoepidemiology and Drug
    Safety, 19(4), 375-383. DOI:
    [10.1002/pds.1921](https://onlinelibrary.wiley.com/doi/abs/10.1002/pds.1921)
-   Kennerfalk, A., Ruigómez, A., Wallander, M.-A., Wilhelmsen, L., &
    Johansson, S. (2002). Geriatric Drug Therapy and Healthcare
    Utilization in the United Kingdom. Annals of Pharmacotherapy, 36(5),
    797-803. DOI:
    [10.1345/aph.1A226](https://journals.sagepub.com/doi/10.1345/aph.1A226)
-   Masnoon, N., Shakib, S., Kalisch-Ellett, L., & Caughey, G. E.
    (2017). What is polypharmacy? A systematic review of definitions.
    BMC Geriatr, 17(1), 230. DOI:
    [10.1186/s12877-017-0621-2](https://bmcgeriatr.biomedcentral.com/articles/10.1186/s12877-017-0621-2)
-   Narayan, S. W., & Nishtala, P. S. (2015). Associations of
    Potentially Inappropriate Medicine Use with Fall-Related
    Hospitalisations and Primary Care Visits in Older New Zealanders: A
    Population-Level Study Using the Updated 2012 Beers Criteria.
    \[journal article\]. Drugs - Real World Outcomes, 2(2), 137-141.
    DOI:
    [10.1007/s40801-015-0020-y](https://link.springer.com/article/10.1007%2Fs40801-015-0020-y)
-   Nishtala, P. S., & Salahudeen, M. S. (2015). Temporal Trends in
    Polypharmacy and Hyperpolypharmacy in Older New Zealanders over a
    9-Year Period: 2005-2013. Gerontology, 61(3), 195-202. DOI:
    [10.1159/000368191](https://www.karger.com/Article/Abstract/368191)
-   Park, H. Y., Ryu, H. N., Shim, M. K., Sohn, H. S., & Kwon, J. W.
    (2016). Prescribed drugs and polypharmacy in healthcare service
    users in South Korea: an analysis based on National Health Insurance
    Claims data. Int J Clin Pharmacol Ther, 54(5), 369-377. DOI:
    [10.5414/cp202484](https://www.dustri.com/article_response_page.html?artId=14225&doi=10.5414/CP202484&L=0)
-   Veehof, L., Stewart, R., Haaijer-Ruskamp, F., & Jong, B. M. (2000).
    The development of polypharmacy. A longitudinal study. Fam Pract,
    17(3), 261-267. DOI:
    [10.1093/fampra/17.3.261](https://academic.oup.com/fampra/article/17/3/261/514657)
