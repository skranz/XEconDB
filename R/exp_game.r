
# new experiment match
new.em = function(vg=vg, subIds=NULL, app.li = NULL, container.ids = "mainUI") {
	restore.point("new.em")
	
	if (is.null(subIds)) {
		subIds = paste0("SubTest_", seq_len(vg$params$numPlayers))
	}
	
	em = as.environment(nlist(gameId=vg$gameId, variant=vg$variant, vg=vg, subIds=subIds, app.li=app.li, container.ids = container.ids))
	em$em = em
	
	vg = get.vg(gameId=em$gameId, variant=em$variant)
	em$vg = vg
	
	n = vg$params$numPlayers
	em$n = n
	em$players = seq_len(n)
	em$values = vg$params
	
	em$act.stage = 0
	em$stage = NULL
  em$is.waiting = em$stage.finished = rep(TRUE,n)
  em
}


em.make.stage.ui = function(stage, player, em) {
	restore.point("em.make.stage.ui")
	vg = em$vg
	page = load.rg.stage.page(stage, rg=vg)
	
	em$page.values = c(em$values, list(.player = player))
	
	# will only be temporary assigned
	em$player = player
	em$stage = stage
	em$ns = NS(paste0("em-page-",stage$stage.name,"-",player))
	
	# set global em for rendered fields
	app = getApp()
	app$glob$em = em
	
	cr = compile.rmd(text=page, out.type = "shiny",envir = em$page.values,blocks = "render")
  ui = render.compiled.rmd(cr,envir=em$page.values,use.commonmark=FALSE)
	ui
}

get.em.container.id = function(em, player=1) {
	if (is.null(em$container.ids)) return("mainUI")
	pos = min(player,length(em$container.ids))
	em$container.ids[[pos]]
}

get.em.player.app = function(em, player=1) {
	restore.point("get.em.player.ap")
	if (is.null(em$app.li)) return(getApp())
	pos = min(player,length(em$app.li))
	em$app.li[[pos]]
} 

em.show.current.page = function(em, player=seq_len(em$vg$params$numPlayers)) {
  restore.point("em.show.current.page.multi")
 	
	if (length(player)>1) {
		for (p in player) {
			em.show.current.page(em, player=p)
		}
		return()
	}
  restore.point("em.show.current.page")
	
	# may differ depending whether we are in
	# test mode in the xs or not
	container.id = get.em.container.id(em, player=player)
	
	# each subject will have a separate app
	app = get.em.player.app(em=em, player=player)
	
	stage.num = em$act.stage
	if (stage.num < 1 | stage.num > length(em$vg$stages)) {
		ui = wait.ui(em=em, player)
		setUI(container.id,ui, app=app)
		dsetUI(container.id,ui, app=app)
	}
	
	stage = em$vg$stages[[stage.num]]

  if (!em$is.waiting[player]) {
  	stage.ui = try(em.make.stage.ui(stage=stage,player=player,em=em))
  } else {
  	stage.ui = try(wait.ui(em))
  }
  
  
  if (is(stage.ui, "try-error")) {
  	stage.ui = HTML(paste0("An error occured when parsing the page for stage ", stage$name,":<br><br>", as.character(stage.ui)))
  }

  setUI(container.id,stage.ui, app=app)  
 	dsetUI(container.id,stage.ui, app=app)  
}

em.start.match = function(em) {
	restore.point("em.start.match")
	em$values = em$vg$params
	em$act.stage = 0
	em$stage = NULL
  em$is.waiting = rep(TRUE,em$n)
  em.run.next.stages(em=em)
}


em.run.next.stages = function(em) {
  restore.point("em.run.next.stages")
  vg = em$vg
  n = em$n
  
  if (em$act.stage == length(vg$stages)) {
    return(em.finish.match(em))
  }
  
  act.stage = em$act.stage = em$act.stage + 1
  restore.point("run.next.stages.2")
  

  em$stage = stage = vg$stages[[act.stage]]
  em$stage.name = stage$name

  run.stage = TRUE
  if (!identical(stage$condition,"")) {
    run.stage = eval.or.return(call=stage$condition, envir=em$values)
  } else {
    run.stage = TRUE
  }
  
  if (run.stage) {
    em.run.stage.computations(stage, em)
    
    players = eval.or.return(stage$player,em$values)
    if (identical(players,"")) players = NULL
    i = 1
    em$is.waiting = !seq_len(n) %in% players
    em$stage.finished = em$is.waiting
    if (length(players)==0)
      return(em.run.next.stages(em))
    
    em.show.current.page(em=em)
  } else {
    return(em.run.next.stages(em))
  }
  return(em)
}

em.finish.match = function(em) {
	ui = tags$p("The game is finished")
	for (player in seq_along(em$players)) {
		app = get.em.player.app(em=em, player=player)
		id = get.em.container.id(em=em, player=player)
		setUI(id,ui, app=app)
		dsetUI(id,ui, app=app)
	}
	
	
	cat("\n\nThe game is finished...")
}

# simply perform all computations:
# draw random variables and compute transformations
# for a stage.
# Should be called when a stage is reached in
# a running experiment
em.run.stage.computations = function(stage, em) {
  restore.point("em.run.stage.computations")
	
	# draw from random variables
	for (rv in stage$nature) {
		var = rv$name
		set = eval.or.return(rv$set, em$values)
		prob = eval.or.return(rv$prob, em$values)
		if (nchar(prob)==0) prob=NULL
		val = sample(set,1,prob=prob)
		
		em$values[[var]] = val
	}
  
	# compute transformations
	for (tr in stage$compute) {
		var= tr$name
		val = eval.or.return(tr$formula, em$values)
		em$values[[var]] = val
	}
	
}





get.page.ns = function(stage.name, player) {
	NS(paste0("page-",stage.name,"-",player))
}

wait.ui = function(...) {
  ui = h3("Please wait...")
  ui
}

em.submit.btn.click = function(formValues, player, stage.name,action.ids, ..., em=get.em()) {
	restore.point("em.submit.btn.click")
	cat("\nsubmit.btn.clicked!\n")
	for (id in action.ids) {
		if (isTRUE(length(formValues[[id]])==0) |  isTRUE(formValues[[id]]=="")) {
			errorMessage(get.page.ns(stage.name = stage.name,player=player)("msg"),"Please make all required choices, before you continue.")
			return()
		}
	}
	
	if (length(formValues)>0 & length(action.ids)>0) {
		avals = lapply(formValues[action.ids], convert.atom)
		em$values[names(action.ids)] = avals
	}
	
	em$stage.finished[player] = TRUE
	if(all(em$stage.finished)) {
		em.run.next.stages(em)
	} else {
		em$is.waiting[player] = TRUE
		em.show.current.page(em = em, player=player)
	}
}

submitPageBtn = function(label="Press to continue",em=get.em(),player=em$player,...) {
	restore.point("submitPageBtn")
	
	ns = get.page.ns(em$stage$name,em$player)

	id = paste0(ns("submitPageBtn"))
	action.ids = sapply(names(em$stage$actions),get.action.input.id, em=em)
	


	app = get.em.player.app(em=em, player=player)
	
	buttonHandler(id, em.submit.btn.click, player=em$player, stage.name = em$stage$name, action.ids=action.ids,app = app)
	
	dsetUI(ns("msg"),"", app=app)

	as.character(
		tagList(
			uiOutput(ns("msg")),
			smallButton(id,label, form.ids = action.ids)
		)
	)
}

actionField = function(name,label=NULL,choiceLabels=NULL, inputType="auto",em=get.em(),player=em$player,action.name = name, ...) {
	vg = em$vg
	stage = em$stage
	action = stage$actions[[action.name]]
	if (identical(choiceLabels,""))
		choiceLabels = NULL
	restore.point("actionField")
	
	if (!is.null(label)) {
		label = replace.whiskers(label, em$page.values,whisker.start = "<<", whisker.end = ">>")
	}
	
	id = get.action.input.id(name=name,em=em, player=player) 
  choices = eval.or.return(action$set, em$page.values)
	
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

stratMethRows = function(action,ref.var,ref.vals, html = paste0(
'<td>{{ref.val}}</td>
<td> {{stratMethInput(inputType="select")}}</td>'),em=get.em(),player=em$player,as.tr = FALSE, ...) {
	restore.point("stratMethTable")
	vg = em$vg
	stage = em$stage

	stratMethInput = function(inputType="select",choiceLabels=NULL,...) {
		actionField(name = paste0(action,"_",ref.val),label = "",inputType = inputType,choiceLabels = choiceLabels, em=em, action.name=action)
	}

	values = c(nlist(action, ref.var, ref.vals,stratMethInput), em$page.values)
	
	ref.val = 0
	html = unlist(lapply(ref.vals, function(loc.ref.val) {
		ref.val <<- loc.ref.val
		values$ref.val = ref.val
		replace.whiskers(merge.lines(html), values, eval=TRUE)
	}))

	if (as.tr) {
		html = paste0("<tr>", html,"</tr>", collapse="\n")
	} else {
		html = paste0(html, collapse="\n")
	}
	html
}



get.action.input.id = function(name, stage=em$stage, player=em$player, vg=em$vg, em=NULL) {
	id = paste0(em$vg$vg.id,"-action-",name, "-",em$player) 
	id
}

set.app.em = function(em, app=getApp()) {
	app$experiment.match = em
}

get.em = function(...,app=getApp()) {
	em = 	app$experiment.match
	if (!is.null(em)) return(em)
	app$glob$em
}

