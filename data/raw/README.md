## Raw

These data sets consists of the raw versions of the web scraped data (`game_reviews`, `game_awards`, `game_info`) and the original `esports` data set from [RAN.KIRSH on Kaggle](https://www.kaggle.com/datasets/rankirsh/esports-earnings/data). Data sets created specifically for the relational database design can be found under `games_database`. The csv files are a nice demonstration of the material I was able to extract. It is important to know that the information used to create my specific relational database mutates variables within each of these raw files. These raw files give the viewer a starting point if they chose to personally modify new table but may not want to go through the process of web scraping the data themselves.

-   `esports_raw.csv` --\> data collected from [RAN.KIRSH on Kaggle](https://www.kaggle.com/datasets/rankirsh/esports-earnings/data)

    -   data set under the name `HistoricalEsportData.csv`

-   `game_info_raw.csv` --\> collected from [Wikipedia](https://en.wikipedia.org/wiki/Category:Video_games_by_year)

-   `game_awards_raw.csv` --\> collected from [Wikipedia](https://en.wikipedia.org/wiki/The_Game_Awards)

-   `game_reviews_raw.csv` --\> collected from [OpenCritic](https://opencritic.com/)
