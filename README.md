# Best states for data scientists 2023

## Summary

Searching for a new job may be a hard task as not all states present equal
opportunity for applicants. It gets even harder when an employee has to consider
the comfort of life described in social and ecological terms like high standard
of living and warm weather combined with economic factors like house prices and
salary range.

This repository takes open source data describing life in US states by multiple 
dimensions and uses genetic algorithm to pick top-5 states, where a data scientist
can earn a lot and live a comfortable life both in economic and in social areas.

## Data sources

There are **3 sources** of data for this research:

- US census data from [US Census Bureau](https://www.census.gov/quickfacts/fact/table/US/PST045221)

- US occupational and wage statistics from [US Bureau of labor statistics](https://www.bls.gov/oes/current/oes152051.htm)

- [LinkedIn](https://www.linkedin.com) data on open jobs

## Methodology

The data is split into **2 groups**. First group represents weights that reduce
a state's attractiveness for employees. Typical examples of this class are
weight of people below poverty line, median house value or share of mortgage in
a household's monthly expenses. Second category constitutes the factors which
are valuable for job-seekers like median salary, skewness of wage distribution,
average high temperature and number of open jobs.

Some weights are highly correlated like a number of people with disability and
a share of poor people. Variables with strong relationship shift the result of the
research. PCA is performed to replace the related features with independent 
principal components to avoid bias. There are **7 elements** that explain **90%**
of variation.

Values from each of 2 groups are ranked. Then the genetic algorithm is applied
to minimize weight while maximizing value using [GA](https://cran.r-project.org/web/packages/GA/index.html) library in `R`.

