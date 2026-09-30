# About tab: instructions for using the app.

about_text = list(
  list("Tcl/Tk Scorekeeper", "h1"),
  list(paste("A basketball scorekeeper app built with R's tcltk package. It is a port of",
             "Shiny Scorekeeper (https://github.com/hinkelman/Shiny-Scorekeeper) and uses the",
             "same data functions from the scorekeepeR package. The app is designed for use on",
             "a desktop computer while watching film of a game."), "p"),
  list("Create and Set Roster", "h2"),
  list(paste("Initially, the Teams table is empty. Add a row and double-click on cells to edit",
             "(Return or Tab commits an edit; Tab moves to the next cell; Escape cancels).",
             "Selecting a row in the Teams table brings up the Roster table. Add rows and",
             "double-click on cells to fill out the roster. Players previously entered on rosters",
             "for other teams can be selected from the list below the roster and added to it.",
             "Players are linked to multiple teams via a PlayerID. Updating a linked player's name",
             "on one team updates the player's name on all teams. The same player can have",
             "different numbers on different teams."), "p"),
  list(paste("The app detects unsaved changes to the tables. Save changes to enable setting a",
             "roster for use in scoring a game. The app tracks only one team in each game.",
             "However, if you include a player on each roster with the first name of 'Opponent',",
             "you can track team stats for the opponent with that \"player\" on the roster."), "p"),
  list("Score Game", "h2"),
  list(paste("Setting a roster starts a new game. The full roster is shown to the left of the",
             "scorekeeper buttons. Clicking a scorekeeper button changes the stats of the selected",
             "player. Toggling UNDO (or pressing Ctrl+Z) switches between incrementing and",
             "decrementing a statistic. Selecting the players that did not play (DNP) allows for",
             "correct tallying of per game statistics. Enter the date, opponent, and final score,",
             "then click 'Save game stats'. Game stats are immediately available in the Stats",
             "Viewer, but need to be saved to be available in a new session. A game log is",
             "written to data/gamelogs/ when game stats are saved."), "p"),
  list("View Statistics", "h2"),
  list(paste("Filter the summary stats table with the options on the left (click items to",
             "toggle their selection) and by choosing different combinations of Team, Game, and",
             "Player under 'Group by'. Click on column headings to sort columns."), "p"),
  list("Data", "h2"),
  list(paste("Data are stored in five CSV files (Teams, Players, Rosters, Games, GameStats) in",
             "the data/ directory relative to the working directory."), "p")
)

build_about_tab <- function(parent){
  txt = tktext(parent, wrap = "word", relief = "flat", padx = 16, pady = 12,
               background = col_bg, borderwidth = 0, highlightthickness = 0)
  sb = ttkscrollbar(parent, orient = "vertical", command = function(...) tkyview(txt, ...))
  tkconfigure(txt, yscrollcommand = function(...) tkset(sb, ...))
  tkgrid(txt, sb, sticky = "nsew")
  tkgrid.columnconfigure(parent, 0, weight = 1)
  tkgrid.rowconfigure(parent, 0, weight = 1)

  tktag.configure(txt, "h1", font = "SkValueFont", spacing3 = 8)
  tktag.configure(txt, "h2", font = "SkTitleFont", spacing1 = 12, spacing3 = 4)
  tktag.configure(txt, "p", spacing3 = 6, lmargin1 = 0)
  for (x in about_text) tkinsert(txt, "end", paste0(x[[1]], "\n"), x[[2]])
  tkconfigure(txt, state = "disabled")
}
