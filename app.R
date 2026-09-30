# loads packages, data, and global variables
source(file.path("R", "globals.R"))
source(file.path("R", "widgets.R"))
source(file.path("R", "roster.R"))
source(file.path("R", "scorekeeper.R"))
source(file.path("R", "statsviewer.R"))
source(file.path("R", "about.R"))

# keeps from drawing UI until it is complete
tclServiceMode(FALSE)

# Fonts and styles ----------------------------------------------------------

make_font <- function(name, ...){
  if (!(name %in% as.character(tkfont.names()))) tkfont.create(name, ...)
}
default_size = abs(as.integer(tclvalue(tkfont.actual("TkDefaultFont", "-size"))))
make_font("SkTitleFont", size = default_size + 1)
make_font("SkValueFont", size = default_size + 12)

tcl("ttk::style", "theme", "use", "clam")
tcl("ttk::style", "configure", "Score.TButton", padding = c(10, 8))
tcl("ttk::style", "configure", "Accent.TButton", padding = c(10, 4))
tcl("ttk::style", "configure", "TNotebook.Tab", padding = c(14, 4))

# Main window -----------------------------------------------------------------

base = tktoplevel()
tkwm.title(base, "Scorekeeper")
tkwm.minsize(base, 1100, 640)

mainframe = ttkframe(base, padding = 10)
tkpack(mainframe, fill = "both", expand = TRUE)

nb = ttknotebook(mainframe)
tkpack(nb, fill = "both", expand = TRUE)

tabs = list(
  "Roster" = build_roster_tab,
  "Scorekeeper" = build_scorekeeper_tab,
  "Stats Viewer" = build_statsviewer_tab,
  "About" = build_about_tab
)
for (nm in names(tabs)){
  tab = ttkframe(nb, padding = 10)
  tkadd(nb, tab, text = nm)
  tabs[[nm]](tab)
}

# stats viewer reflects in-memory data so refresh it each time the tab is shown
tkbind(nb, "<<NotebookTabChanged>>", function(){
  if (tclvalue(tkindex(nb, "current")) == "2") refresh_stats(1)
})

# ask before closing with unsaved changes
on_close <- function(){
  unsaved = c(if (roster_unsaved()) "teams/rosters",
              if (!is.null(rv$sk_roster) && game_unsaved()) "game stats")
  if (length(unsaved) > 0 &&
      !confirm(paste0("You have unsaved changes to ", paste(unsaved, collapse = " and "),
                      ". Quit anyway?"), title = "Unsaved changes"))
    return()
  tkdestroy(base)
}
tcl("wm", "protocol", base, "WM_DELETE_WINDOW", on_close)

# show UI
tclServiceMode(TRUE)
# Start the main event loop (when run with Rscript)
if (!interactive()) tkwait.window(base)
