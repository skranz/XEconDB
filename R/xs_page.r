xs.show.edit.page.tab = function(gameId, stage=NULL, xs=app$xs, app=getApp()) {
  restore.point("xs.show.edit.page.tab")
	
	rg = get.rg(gameId=gameId)
	
	if (is.character(stage) | is.numeric(stage))
	stage = rg$stages[[stage]]

	postfix = paste0(gameId,"_", stage$name)
  tabId = paste0("tab_pageedit_",postfix)
 
  if (tabId %in% xs$tabs) {
    w2tabs.select("xsTabs", tabId)
    return()
  }
  
  xs$tabs = c(xs$tabs, tabId)
  
  divId = paste0("div_pageedit_",postfix)
  tab=list(id=tabId,caption=paste0("p-", substring(stage$name,1,8)), closable=TRUE,div_id = divId)
  w2tabs.add(id="xsTabs", tabs=list(tab))
  ui = xs.edit.page.panel.ui(gameId, stage=stage)
  appendToHTML(selector="#mainDiv", as.character(hidden_div(id=divId, ui)))
  w2tabs.select("xsTabs", tabId)
}


xs.edit.page.panel.ui = function(gameId, stage, xs=app$xs, app=getApp(),...) {
	restore.point("xs.page.edit.panel.ui")

	rg = get.rg(gameId = gameId)
	if (is.null(stage)) stage = rg$stages[[1]]
	if (is.character(stage) | is.numeric(stage))
		stage = rg$stages[[stage]]
	
	stage.name = stage$name
	stages.names = get.names(rg$stages)
	
	page = load.rg.stage.page(rg=rg, stage=stage)
	page = merge.lines(page)
	
	ns = NS(paste0(gameId,"-",stage.name))
  form.sel = ids2sel(c(ns("ace"),ns("stage"))) 
  
  ui = list(
  	HTML("<table><tr><td>"),
    smallButton(ns("saveBtn"), "Save",  "data-form-selector"=form.sel),
  	HTML("</td><td>"),
    selectInput(ns("stage"),"",choices = stages.names, selected=stage.name),
  	 HTML("</td></tr></table>"),
  	HTML(aceEditorHtml(ns("ace"),value = page, mode="html",wordWrap = TRUE))
  )
	buttonHandler(ns("saveBtn"),fun = save.page.click, stage.name=stage.name, gameId=gameId)
	selectChangeHandler(ns("stage"),fun=xs.edit.page.stage.change,stage.name=stage.name, gameId=gameId)
	
  ui
}


xs.edit.page.stage.change = function(gameId, stage.name, value,..., app=getApp()) {
	ns = NS(paste0(gameId,"-",stage.name))
	restore.point("xs.edit.page.stage.change")
	# open new tab for new stage page
	updateSelectInput(app$session,ns("stage"),selected = stage.name)
	xs.show.edit.page.tab(gameId=gameId, stage=value)
	# reset select component in current tab

}

save.page.click = function(gameId, stage.name, formValues,...) {
	ns = NS(paste0(gameId,"-",stage.name))
	txt = formValues[[ns("ace")]]
	restore.point("save.page.click")

	save.stage.page(txt=txt, gameId=gameId, stage.name = stage.name)
	
	
}
