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
	
	xm = as.environment(list(gameId=gameId, ns=ns, act.stage=0))
	xs$xm = xm
	
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
	xm = get.xm()
	if (!isTRUE(xm$act.stage>0)) return()
	xs.show.edit.page.tab(gameId=xm$gameId, stage=xm$stage)
	
}


xs.refresh.match.page.click = function(formValues,..., xs=app$xs, app=getApp()) {
	restore.point("xs.refresh.match.page.click")
	xm = get.xm()
	
	if (!isTRUE(xm$act.stage>0)) return()
	
	for (player in seq_len(xm$vg$params$numPlayers)) {
		xs.set.stage.ui(stage=xm$act.stage,player = player,xm=xm)
	}
}


xs.new.match.click = function(formValues,..., xs=app$xs, app=getApp()) {
	restore.point("xs.new.match.click")
	xm=xs$xm
	variant = formValues[[xm$ns("variant")]]
	xm = new.xm(gameId = xm$gameId,variant=variant, xs=xs)	
}

get.xm = function(xs = app$xs, app=getApp()) {
	xs$xm
}

new.xm = function(gameId, variant=NULL, xs, app=getApp()) {
	restore.point("new.xm")
	
	ns = NS(paste0("run-",gameId))

	xm = as.environment(list(gameId=gameId, ns=ns, variant=variant))
	xs$xm = xm
	
	vg = get.vg(gameId=xm$gameId, variant=xm$variant)
	xm$vg = vg
	
	n = vg$params$numPlayers
	xm$values = vg$params
	
	xm$act.stage = 0
  xm$is.waiting = rep(TRUE,n)
	
 
  panel.li = lapply(seq_len(n), function(i) {
    tabPanel(title=paste0("Player ",i), value=paste0("tabPlayer",i), 
             uiOutput(ns(paste0("uiPlayer",i))))
  })
  tabset.ui = do.call("tabsetPanel", c(list(id="playersTabset"),panel.li))
  setUI(ns("runUI"), tabset.ui)
  dsetUI(ns("runUI"), tabset.ui)
	xs.run.next.stages()  
}

xs.run.next.stages = function(xm=get.xm()) {
  restore.point("xm.run.next.stages")
  #stop("jfjdf")
  vg = xm$vg
  n = vg$params$numPlayers
  
  if (xm$act.stage == length(vg$stages)) {
    return(finish.game(xm))
  }
  
  act.stage = xm$act.stage = xm$act.stage + 1
  restore.point("run.next.stages.2")
  

  stage = vg$stages[[act.stage]]
  xm$stage.name = stage$name

  run.stage = TRUE
  if (!identical(stage$condition,"")) {
    run.stage = eval.or.return(call=stage$condition, envir=xm$values)
  } else {
    run.stage = TRUE
  }
  
  if (run.stage) {
    xs.run.stage.computations(stage, xm)
    
    players = eval.or.return(stage$player,xm$values)
    if (identical(players,"")) players = NULL
    i = 1
    xm$is.waiting = !seq_len(n) %in% players
    for (i in players) {
      xs.set.stage.ui(stage = stage,player = i,xm = xm) 
    }
    for (i in setdiff(seq_len(vg$params$numPlayers), players)) {
      stage.ui = wait.ui()
      setUI(xm$ns(paste0("uiPlayer",i)),stage.ui)  
      dsetUI(xm$ns(paste0("uiPlayer",i)),stage.ui)  
    }
    
    if (length(players)==0)
      return(xs.run.next.stages(xm))
    
  } else {
    return(xs.run.next.stages(xm))
  }
  return(xm)
}

xs.run.stage.computations = function(stage, xm) {
  restore.point("xs.run.stage.computations")
	
	# draw from random variables
	for (rv in stage$nature) {
		var = rv$name
		set = eval.or.return(rv$set, xm$values)
		prob = eval.or.return(rv$prob, xm$values)
		if (nchar(prob)==0) prob=NULL
		val = sample(set,1,prob=prob)
		
		xm$values[[var]] = val
	}
  
	# compute transformations
	for (tr in stage$compute) {
		var= tr$name
		val = eval.or.return(tr$formula, xm$values)
		xm$values[[var]] = val
	}
	
}


xs.set.stage.ui = function(stage, player=1, xm) {
  restore.point("set.stage.ui")
	if (is.numeric(stage) | is.character(stage)) {
		stage = xm$vg$stages[[stage]]
	}
	
  id = xm$ns(paste0("uiPlayer",player))
  if (!xm$is.waiting[player]) {
  	stage.ui = try(xs.make.stage.ui(stage,player,xm))
  } else {
  	stage.ui = try(wait.ui(xm))
  }
  
  
  if (is(stage.ui, "try-error")) {
  	stage.ui = HTML(paste0("An error occured when parsing the page for stage ", stage$name,":<br><br>", as.character(stage.ui)))
  }
  ui = tagList(
  	
  )
  
  setUI(id,stage.ui)  
 	dsetUI(id,stage.ui)  
}

get.page.ns = function(stage.name, player) {
	NS(paste0("page-",stage.name,"-",player))
	
}

xs.make.stage.ui = function(stage, player, xm) {
	restore.point("xm.make.stage.ui")
	vg = xm$vg
	page = load.rg.stage.page(stage, rg=vg)
	
	xm$page.values = c(xm$values, list(.player = player))
	xm$player = player
	xm$stage = stage
	
	
	cr = compile.rmd(text=page, out.type = "shiny")
	
  ui = render.compiled.rmd(cr,envir=xm$page.values,use.commonmark=FALSE)
	#txt = replace.whiskers(page,values = xm$page.values)
	#html = md2html(txt)
	ui
}

wait.ui = function(...) {
  ui = h3("Wait for the other players...")
  ui
}

xs.submit.btn.click = function(formValues, player, stage.name,action.ids, ..., xm=get.xm()) {
	restore.point("xs.submit.btn.click")
	cat("\nsubmit.btn.clicked!\n")
	for (id in action.ids) {
		if (isTRUE(length(formValues[[id]])==0) |  isTRUE(formValues[[id]]=="")) {
			errorMessage(get.page.ns(stage.name = stage.name,player=player)("msg"),"Please make all required choices, before you continue.")
			return()
		}
	}
	
	avals = lapply(formValues[action.ids], convert.atom)
	
	
	xm$values[names(action.ids)] = avals
	xs.run.next.stages()
}

submitPageBtn = function(label="Press to proceed",xm=get.xm(),...) {
	restore.point("submitPageBtn")
	
	ns = get.page.ns(xm$stage$name,xm$player)

	id = paste0(ns("submitPageBtn"))
	action.ids = sapply(names(xm$stage$actions),get.action.input.id, xm=xm)
	

	buttonHandler(id, xs.submit.btn.click, player=xm$player, stage.name = xm$stage$name, action.ids=action.ids)

	dsetUI(ns("msg"),"")

	as.character(
		tagList(
			uiOutput(ns("msg")),
			smallButton(id,label, form.ids = action.ids)
		)
	)
}

actionField = function(name,label=NULL,choiceLabels=NULL, inputType="auto",xm=get.xm(),...) {
	vg = xm$vg
	stage = xm$stage
	action = stage$actions[[name]]
	if (identical(choiceLabels,""))
		choiceLabels = NULL
	restore.point("actionField")
	
	if (!is.null(label)) {
		label = replace.whiskers(label, xm$page.values,whisker.start = "<<", whisker.end = ">>")
	}
	
	id = get.action.input.id(name=name,xm=xm) 
  choices = eval.or.return(action$set, xm$page.values)
	
	if (inputType=="auto") {
    if (length(choices)<=12){
      inputType="radio"      
    } else {
      inputType="selectize"
    }
	}
  #inputType = "selectize"
  
  if (!is.null(choiceLabels)) {
    choices = as.list(choices)
    names(choices) = choiceLabels
  }
  if (inputType=="radio") {
    ui = radioButtons(inputId = id,label = label,choices = choices, selected=NA)
  } else {
    ui = selectizeInput(inputId = id,label = label,choices = choices, selected=NA)    
  }
  
  html = as.character(ui)
	html	
}

get.action.input.id = function(name, stage=xm$stage, player=xm$player, vg=xm$vg, xm=NULL) {
	id = paste0(xm$vg$vg.id,"-action-",name, "-",xm$player) 
	id
}
