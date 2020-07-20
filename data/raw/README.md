# Raw data

This folder contains two CSV files derived from 1976--2013 New Zealand census data.

`counts.csv` provides usual resident employment counts by urban area, industry, occupation, and census year.
We capture
urban areas by 2013 urban area codes,
industries by a grouping of 2013 NZSIOC codes, and 
occupations by one-digit 1999 NZSCO codes.

The allocations to 2013 urban areas and to 2013 NZSIOC industry groups are based on user-derived correspondences.
`nzsioc-groups.csv` provides a crosswalk between
2006 ANZSIC codes,
2013 NZSIOC codes (grouped to match the classification used by Statistics New Zealand to generate input-output tables), and
our grouping of 2013 NZSIOC codes.

## Dictionary

Variable(s) | Description
--- | ---
`anzsic06` | 2006 ANZSIC code
`emp<yy>` | Employment count for year `<yy>`
`nzsco99_1d`, `nzsco99_1d_desc` | One-digit 1999 NZSCO code and description
`nzsioc13`, `nzsioc13_desc` | 2013 NZSIOC code and description
`nzsioc13_gp`, `nzsioc13_gp_desc` | 2013 NZSIOC group code and description
`ua13`, `ua13_desc` | 2013 urban area code and description

## Confidentialisation

We confidentialise the data in `counts.csv` according to the [2013 Census confidentiality rules](http://archive.stats.govt.nz/Census/2013-census/methodology/confidentiality-how-applied.aspx).
In particular, we
(i) round employment counts randomly to base three and
(ii) suppress cells with raw employment counts smaller than six.

## Disclaimer

Access to the New Zealand census data was provided by Statistics New Zealand under conditions designed to give effect to the security and confidentiality provisions of [the Statistics Act 1975](http://www.legislation.govt.nz/act/public/1975/0001/latest/DLM430705.html).
The files in this folder are the work of the authors, not Statistics New Zealand.
