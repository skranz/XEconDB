xs.show.run.tab = function(gameId, xs=app$xs, app=getApp()) {
  restore.point("xs.show.run.tab")
  cat("\nxs.show.run.tab")
  tabId = paste0("tab_run_",gameId)
 
  if (tabId %in% xs$tabs) {
    w2tabs.select("xsTabs", tabId)
    return()
  }
  
  # close other run tabs
  run.tabs = xs$tabs[str.starts.with(xs$tabs,"tab_run_")]
  for (rtabId in run.tabs) {
  	divId = paste0("div_run_", str.right.of(rtabId,"tab_run_"))
  	w2tabs.destroy.tab.content(divId)
  	w2tabs.close("xsTabs", rtabId)
  }
  xs$tabs = setdiff(xs$tabs, run.tabs)
  
  xs$tabs = c(xs$tabs, tabId)
  
  divId = paste0("div_run_",gameId)
  tab=list(id=tabId,caption=paste0("Run ", gameId), closable=TRUE,div_id = divId)
  w2tabs.add(id="xsTabs", tabs=list(tab))
  ui = xs.run.panel.ui(gameId)
  appendToHTML(selector="#mainDiv", as.character(hidden_div(id=divId, ui)))
  w2tabs.select("xsTabs", tabId)
}

xs.run.panel.ui = function(gameId, xs=app$xs, app=getApp()) {
	restore.point("xs.run.panel.ui")
	
	ns = NS(paste0("run-",gameId))
	
	xs$run.gameId = gameId
	rg = get.rg(gameId=gameId)
  variants = rg$variants
  variant = variants[[1]]
 
  form.sel = ids2sel(c(ns("variant"))) 
  ui = list(
    selectInput(ns("variant"),"Variant:",choices = variants, selected=variant),
    smallButton(ns("newMatchBtn"), "Start",  "data-form-selector"=form.sel),
    smallButton(ns("refreshPageBtn"), "Refresh",  "data-form-selector"=form.sel),
    smallButton(ns("editPageBtn"), "Edit Page",  "data-form-selector"=form.sel),
  	uiOutput(ns("runUI"))
  )
  buttonHandler(ns("newMatchBtn"), xs.new.match.click)
  buttonHandler(ns("refreshPageBtn"), xs.refresh.match.page.click)
  buttonHandler(ns("editPageBtn"), xs.edit.page.click)
  
  ui

}

xs.edit.page.click = function(...) {
	restore.point("xs.edit.page.click")
	em = get.em()
	if (!isTRUE(em$act.stage>0)) return()
	try(xs.show.edit.page.tab(gameId=em$gameId, stage=em$stage))
	
}

xs.refresh.match.page.click = function(formValues,..., xs=app$xs, app=getApp()) {
	restore.point("xs.refresh.match.page.click")
	em = get.em()
	try(em.show.current.page(em=em))
}


xs.new.match.click = function(formValues,..., xs=app$xs, app=getApp()) {
	restore.point("xs.new.match.click")
	gameId = xs$run.gameId
	ns = NS(paste0("run-",gameId))
	variant = formValues[[ns("variant")]]
	xs.new.match(variant=variant)
}

xs.new.match = function(gameId=xs$run.gameId, variant=NULL, xs=app$xs, app=getApp()) {
	restore.point("xs.new.match")
	xs$run.variant = variant
	
	ns = NS(paste0("run-",gameId))
	vg = get.vg(gameId=gameId, variant=variant)
	
	container.ids = sapply(seq_len(vg$params$numPlayers), function(i) ns(paste0("uiPlayer",i)))
	em = new.em(vg=vg,app.li = list(app),container.ids = container.ids)
	
	xs$em = em
	set.app.em(em, app=app)
	

  panel.li = lapply(seq_len(em$n), function(i) {
    tabPanel(title=paste0("Player ",i), value=paste0("tabPlayer",i), 
             uiOutput(ns(paste0("uiPlayer",i))))
  })
  tabset.ui = do.call("tabsetPanel", c(list(id="playersTabset"),panel.li))
  setUI(ns("runUI"), tabset.ui)
  dsetUI(ns("runUI"), tabset.ui)

  em.start.match(em=em)
}


