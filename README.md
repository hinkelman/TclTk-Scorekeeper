# TclTk-Scorekeeper

A basketball scorekeeper app built with R's [tcltk](https://r-universe.dev/manuals/tcltk.html) package. It approximates the functionality of [Shiny Scorekeeper](https://github.com/hinkelman/Shiny-Scorekeeper) and uses the same data functions from [scorekeepeR](https://github.com/hinkelman/scorekeepeR).

### Installation

```
install.packages(c("dplyr", "ids", "remotes"))
remotes::install_github("hinkelman/scorekeepeR")
```

### Usage

From the repository directory, run

```
Rscript app.R
```

or `source("app.R")` from an interactive R session. Data are stored as CSV files in `data/` (relative to the working directory) and game logs in `data/gamelogs/`.

### Tabs

* **Roster** – Add/delete teams and players. Double-click a cell to edit it (Return/Tab commits, Tab moves to the next cell, Escape cancels). Add players from other rosters. Save changes, then **Set roster** to start scoring a game.
* **Scorekeeper** – Select a player and click buttons to tally stats. Toggle **UNDO** (or Ctrl+Z) to decrement instead. Mark players that did not play (DNP). Enter game info and **Save game stats**.
* **Stats Viewer** – Filter by league, team, season, scoring margin, opponent, and date; group by team, game, and/or player; view per game or total stats. Click a column heading to sort.
* **About** – Instructions.

### Differences from Shiny Scorekeeper

* Setting a roster starts a new game (new GameID) and asks before discarding unsaved game stats.
* The app asks before closing with unsaved changes.
* Games without a final score are included in the Stats Viewer rather than dropped by the scoring margin filter, and blank team fields show as `(blank)` in the filters.
* Per game player stats are divided by games played (GP) rather than games.
