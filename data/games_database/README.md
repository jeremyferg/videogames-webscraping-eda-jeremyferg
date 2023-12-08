## Games Database

The games database is a collect of video game information used in the exploratory analysis of the final report. This database can be reused and revised in the future as well. Please refer to `games_relational_database_design.png` for a visual representation of the database and the relationships connecting each data set. Please refer to `games_database_codebook.csv` for an explanation of each variable within the database.

The R script used to revise and form this database, `game_relational_database_editing.R` is also located in this folder.

The following data sets create the database:

-   `games` --\> game titles and their respective IDs

-   `publishers` --\> publisher names and their respective IDs

-   `game_series` --\> series names and their respective IDs

-   `game_genres` --\> genre names and their respective IDs

-   `game_modes` --\> game mode names and their respective IDs

-   `game_info` --\> general information about video games from 2013 to 2023

-   `publisher_stocks` --\> stock information of nine notable video game publishers from July, 2013 to July 2023

    -   publishers include: Nintendo, Electronic Arts, Square Enix, Sega, Activision Blizzard, Take-Two, Konami, Capcom, and Bandai Namco

-   `esports` --\> historical data concerning the monthly earnings and participation of esports events

-   `award_category` --\> category titles from The Game Awards and their respective IDs

-   `game_awards` --\> information about games' nominations and results at The Game Awards from 2014 to 2022

-   `game_review_companies` --\> names of game review companies and their respective IDs

-   `game_reviewers` --\> names of game reviewers and their respective IDs

-   `game_reviews` --\> game_reviews of games from 2013 - 2023, including the their review scores
