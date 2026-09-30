# Reusable widgets built on tcltk. Each widget is an environment that holds
# the tk objects plus any R-side state (e.g., the values behind a listbox).

# Treeview (table) ---------------------------------------------------------

make_table <- function(parent, height = 10, selectmode = "browse"){
  e = new.env()
  e$frame = ttkframe(parent)
  e$tv = ttktreeview(e$frame, show = "headings", selectmode = selectmode, height = height)
  e$ysb = ttkscrollbar(e$frame, orient = "vertical",
                       command = function(...) tkyview(e$tv, ...))
  e$xsb = ttkscrollbar(e$frame, orient = "horizontal",
                       command = function(...) tkxview(e$tv, ...))
  tkconfigure(e$tv,
              yscrollcommand = function(...) tkset(e$ysb, ...),
              xscrollcommand = function(...) tkset(e$xsb, ...))
  tkgrid(e$tv, e$ysb, sticky = "nsew")
  tkgrid(e$xsb, sticky = "ew")
  tkgrid.columnconfigure(e$frame, 0, weight = 1)
  tkgrid.rowconfigure(e$frame, 0, weight = 1)
  e
}

# cols: named vector where names are headings and values are column names in data
# stretch = FALSE keeps column widths fixed so wide tables scroll horizontally
# instead of squeezing columns to fit
table_set_columns <- function(e, cols, widths = 110, anchors = "w", command = NULL,
                              stretch = TRUE){
  headings = if (is.null(names(cols))) cols else names(cols)
  widths = rep_len(widths, length(cols))
  anchors = rep_len(anchors, length(cols))
  tkconfigure(e$tv, columns = as.tclObj(unname(cols), drop = FALSE))
  for (i in seq_along(cols)){
    cmd = if (is.null(command)) "" else local({col = cols[[i]]; function() command(col)})
    tcl(e$tv, "heading", cols[[i]], text = headings[i], anchor = anchors[i], command = cmd)
    tcl(e$tv, "column", cols[[i]], width = widths[i], minwidth = 40, anchor = anchors[i],
        stretch = stretch)
  }
  e$cols = cols
}

fmt_cell <- function(x){
  if (is.na(x)) return("")
  if (is.double(x) && x != round(x)) return(format(round(x, 2), nsmall = 2))
  as.character(x)
}

# row ids in the treeview are row indices of data
table_fill <- function(e, data){
  tcl(e$tv, "delete", tcl(e$tv, "children", ""))
  if (is.null(data)) return(invisible())
  for (i in seq_len(nrow(data))){
    vals = vapply(e$cols, function(col) fmt_cell(data[[col]][i]), character(1))
    tcl(e$tv, "insert", "", "end", id = i, values = as.tclObj(vals, drop = FALSE))
  }
}

table_selected <- function(e){
  sel = tclvalue(tcl(e$tv, "selection"))
  if (sel == "") NULL else as.integer(strsplit(sel, " ")[[1]])
}

table_select <- function(e, row){
  if (is.null(row) || !(as.character(row) %in% strsplit(tclvalue(tcl(e$tv, "children", "")), " ")[[1]]))
    return(invisible())
  tcl(e$tv, "selection", "set", row)
  tcl(e$tv, "see", row)
}

table_clear_selection <- function(e){
  tcl(e$tv, "selection", "set", "")
}

# Double-click a cell to edit. on_edit(row, col, value) receives the row index,
# the column name (as in e$cols), and the new value. Return and Tab commit the
# edit (Tab moves to the next cell); Escape cancels.
table_make_editable <- function(e, on_edit){
  tkbind(e$tv, "<Double-1>", function(x, y){
    row = tclvalue(tcl(e$tv, "identify", "row", x, y))
    col = tclvalue(tcl(e$tv, "identify", "column", x, y))
    if (row == "" || col == "") return()
    table_edit_cell(e, as.integer(row), as.integer(sub("#", "", col)), on_edit)
  })
}

table_edit_cell <- function(e, row, col_idx, on_edit){
  tcl("update", "idletasks")
  tcl(e$tv, "see", row)
  bbox = as.integer(strsplit(tclvalue(tcl(e$tv, "bbox", row, paste0("#", col_idx))), " ")[[1]])
  if (length(bbox) != 4) return()
  col = e$cols[[col_idx]]
  old = tclvalue(tcl(e$tv, "set", row, col))
  var = tclVar(old)
  ent = ttkentry(e$tv, textvariable = var)
  # drop the "all" bindtag so Tab doesn't also move focus to the next widget
  tcl("bindtags", ent, as.tclObj(c(ent$ID, "TEntry"), drop = FALSE))
  tkplace(ent, x = bbox[1], y = bbox[2], width = bbox[3], height = bbox[4])
  tkfocus(ent)
  tkselection.range(ent, 0, "end")
  tkicursor(ent, "end")

  done = FALSE
  finish <- function(save, next_cell = FALSE){
    if (done) return()
    done <<- TRUE
    val = tclvalue(var)
    tkdestroy(ent)
    tkfocus(e$tv)
    if (save && val != old) on_edit(row, col, val)
    if (next_cell){
      n_rows = length(strsplit(tclvalue(tcl(e$tv, "children", "")), " ")[[1]])
      if (col_idx < length(e$cols)){
        table_edit_cell(e, row, col_idx + 1, on_edit)
      } else if (row < n_rows) {
        table_edit_cell(e, row + 1, 1, on_edit)
      }
    }
  }
  tkbind(ent, "<Return>", function() finish(TRUE))
  tkbind(ent, "<KP_Enter>", function() finish(TRUE))
  tkbind(ent, "<Tab>", function() finish(TRUE, next_cell = TRUE))
  tkbind(ent, "<Escape>", function() finish(FALSE))
  tkbind(ent, "<FocusOut>", function() finish(TRUE))
}

# Listbox (multiple selection) ---------------------------------------------

make_listbox <- function(parent, height = 6, width = 24, all_none = FALSE,
                         on_select = NULL){
  e = new.env()
  e$values = character()
  e$labels = character()
  e$on_select = on_select
  e$var = tclVar()
  e$frame = ttkframe(parent)
  e$lb = tklistbox(e$frame, listvariable = e$var, selectmode = "multiple",
                   exportselection = FALSE, height = height, width = width,
                   activestyle = "none", relief = "solid", borderwidth = 1,
                   highlightthickness = 0, selectbackground = "#0d6efd",
                   selectforeground = "white")
  e$sb = ttkscrollbar(e$frame, orient = "vertical",
                      command = function(...) tkyview(e$lb, ...))
  tkconfigure(e$lb, yscrollcommand = function(...) tkset(e$sb, ...))
  tkgrid(e$lb, e$sb, sticky = "nsew")
  tkgrid.columnconfigure(e$frame, 0, weight = 1)
  if (all_none){
    btns = ttkframe(e$frame)
    tkpack(ttkbutton(btns, text = "Select all", width = 10,
                     command = function() {listbox_select(e, e$values); listbox_fire(e)}),
           side = "left", padx = c(0, 4))
    tkpack(ttkbutton(btns, text = "Deselect all", width = 10,
                     command = function() {listbox_select(e, NULL); listbox_fire(e)}),
           side = "left")
    tkgrid(btns, sticky = "w", pady = c(2, 0))
  }
  tkbind(e$lb, "<<ListboxSelect>>", function() listbox_fire(e))
  e
}

listbox_fire <- function(e) if (!is.null(e$on_select)) e$on_select()

listbox_selected <- function(e){
  idx = as.integer(tclvalue(tkcurselection(e$lb)) |> strsplit(" ") |> unlist())
  e$values[idx + 1]
}

listbox_select <- function(e, values){
  tkselection.clear(e$lb, 0, "end")
  for (i in which(e$values %in% values)) tkselection.set(e$lb, i - 1)
}

# select = "keep": keep selection if choices are unchanged, otherwise select all
listbox_set <- function(e, values, labels = values, select = c("keep", "all", "none")){
  select = match.arg(select)
  unchanged = identical(values, e$values)
  prev = listbox_selected(e)
  e$values = values
  e$labels = labels
  tclObj(e$var) = as.tclObj(labels, drop = FALSE)
  sel = switch(select,
               keep = if (unchanged) prev else values,
               all = values,
               none = NULL)
  listbox_select(e, sel)
}

# Value box ----------------------------------------------------------------

make_value_box <- function(parent, title){
  e = new.env()
  e$value = tclVar("--")
  e$sub = tclVar("")
  e$frame = tkframe(parent, bg = col_bg, highlightthickness = 1,
                    highlightbackground = col_border, padx = 14, pady = 8)
  tkgrid(tklabel(e$frame, text = title, bg = col_bg, fg = col_text, font = "SkTitleFont"),
         sticky = "w")
  tkgrid(tklabel(e$frame, textvariable = e$value, bg = col_bg, fg = col_text,
                 font = "SkValueFont"), sticky = "w")
  tkgrid(tklabel(e$frame, textvariable = e$sub, bg = col_bg, fg = col_muted),
         sticky = "w")
  e
}

value_box_set <- function(e, value, sub = ""){
  tclvalue(e$value) = value
  tclvalue(e$sub) = sub
}

# Misc ---------------------------------------------------------------------

set_state <- function(widget, enabled){
  tkconfigure(widget, state = if (isTRUE(enabled)) "normal" else "disabled")
}

confirm <- function(message, title = "Confirm"){
  tclvalue(tkmessageBox(title = title, message = message, icon = "warning",
                        type = "yesno", default = "no")) == "yes"
}
