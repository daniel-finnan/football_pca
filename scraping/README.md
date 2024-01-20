# Python scraping for Premier League data

The code in this folder carries out scraping to retrieve both the individual statistics for clubs in each season and the data from the league table. The website used for this scraping is contained within a `.env` file so isn't made public and neither is the data itself.

[Selenium](https://www.selenium.dev/) is used to access the webpages and navigate, dealing with modal boxes for cookies and pop-up adverts. A sleep function contained within `helpers.py` is used to avoid being detected as a scraper, and various checks are carried out to ensure that the relevant components and data have loaded on the page.

Nevertheless, this code is likely to be broken with updates to the HTML for the site.

The methodology is as follows:

1. Scrap the statistics and league tables.
2. Save the data in HTML format.
3. Use regular expressions (tables) or [Beautiful Soup](https://beautiful-soup-4.readthedocs.io) (statistics) to extract the data from the saved pages.
4. Save the data in a [Pandas](https://pandas.pydata.org/) dataframe in pickle format.
5. Merge the two together, carry out some basic cleaning and processing, then export in `csv` format ready for analysis in R.

The full analysis is available in [README.Rmd](/README.Rmd) for use in RStudio.