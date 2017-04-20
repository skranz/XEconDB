# xecon studio

xecon.glob = new.env()

examples.xsApp = function() {
	set.storing(TRUE)
	restore.point.options(display.restore.point = TRUE)
  projects.dir = "D:/libraries/XEconDB/projects"
  app = xsApp(projects.dir, never.load.tg=FALSE, demo.mode = FALSE)
  viewApp(app)
  viewApp(app,launch.browser = TRUE)
}


xsApp = function(projects.dir, project=1, otree.dir=NULL, otree.url="http://localhost:8000", never.load.tg = FALSE, demo.mode=FALSE) {
  restore.point("xsApp")
  
  library(shinyEventsUI)
  addXEconRessourcePath()
  xs.load.ressources()
  
  app = eventsApp()
  app$xs = xs = new.env()
  
  xs$projects.dir = projects.dir
  xs$otree.dir = otree.dir
  xs$otree.url = otree.url
  xs$never.load.tg = never.load.tg
  xs$demo.mode = demo.mode
  
  setwd(projects.dir)
  
  xs$projects = list.dirs(projects.dir, full.names=FALSE, recursive = FALSE)
  
  if (is.numeric(project)) {
    project = min(project, length(xs$projects))
    if (project>0) {
      project = xs$projects[project]
    } else {
      project = NULL
    }
  }
  xs.init.project(project, xs)
  
  xs$tabs = NULL
  
  app$ui = xs.ui()
  appInitHandler(function(app,xs=app$xs,...) {
    xs$tabs=NULL
  })
  
  eventHandler("parseNodeEvent","parseNodeEvent", function(...) {
    args = list(...)
    restore.point("parseNodeEvent")
    value = args$value
    value
    .GlobalEnv$parsedNode = value
    cat("\nNode is parsed...")
    print(value)
  })
  app
}

xs.init.project = function(project,xs) {
  xs$project.dir = file.path(xs$projects.dir,project)
  xs$games.dir = file.path(xs$project.dir,"games")
  
  if (is.null(xs[["otree.dir"]]))
    xs$otree.dir = file.path(xs$project.dir,"oTree")
  
  
  xs$project = project
  xs.get.gamesId(xs)
  
}

xs.get.gamesId = function(xs) {
  restore.point("xs.get.gamesId")
  
  if (is.null(xs$project)) return(NULL)
  gamesId = list.dirs(paste0(xs$project,"/games"), recursive=FALSE, full.names=FALSE)
  xs$gamesId = gamesId
  gamesId
}



xs.ui = function(app=getApp(), xs=app$xs) {
  restore.point("xs.ui")
  
  json.opts ="
  defaults: {
    resizable: true,
    closable: false,
    slideable: true,
    spacing_open: 5
  },
  north: {
    size: 'auto',
    resizable: false,
    closable: false,
    slideable: false,
    spacing_open: 0
  },
  east: {
    resizable: true,
    spacing_open: 0,
    spacing_closed: 0,
    size: 0
  }
  "

  tree.nodes = xs.project.tree.nodes(xs=xs)

  cm = tagList(
  	 treeNodeContextMenu(id="cmProjGame",node.class = "projNode_games", items=list(new = list(name="New game"))),
  	 treeNodeContextMenu(id="cmProjGame",node.class = "projNode_game", items=list(new = list(name="New game"), del=list(name="Delete game"), rename=list(name="Rename game"), duplicate = list(name="Duplicate game")
  	))
  )
  
  contextMenuHandler("cmProjGame", function(key,data, session=NULL,...){
  	args = list(...)
  	nodeType = data$nodeType
  	gameId = data$gameId
  	restore.point("cmProjGameHandler")
  	cat("\ncontext menu key: ", key)
  	
  	if (key=="new" && nodeType %in% c("game","games")) {
  	  xs.new.game.click()	
  	} else if (key=="del" && nodeType == "game") {
  	  xs.delete.game(gameId = gameId)
  	} else if (key=="duplicate" && nodeType == "game") {
  	  xs.duplicate.game(gameId = gameId)
  	} else if (key=="rename" && nodeType == "game") {
  	  xs.rename.game(gameId = gameId)
  	}
  	
  })
  		
  		
  tree = fancytree(id="projTree", source=tree.nodes)

  projects.items = data.frame(text = c("New Project", xs$projects))
  menubar.items = list(
    list(type = "menu", id = "projectsMenu", caption = "projects", items = projects.items),
    list(type="break"),
    list(type="button", id="menuMetaBtn",caption="Background"),
    list(type="button", id="menuDataBtn",caption="Data")
  )
  menubar = w2toolbar(id="xsMenubar", items=menubar.items,js.on.render="xsPanesLayoutVar.resizeAll();")

  menubar = w2toolbar(id="xsMenubar", 
  	items=list(list(type="html",html=xs$project.dir)),
  	js.on.render="xsPanesLayoutVar.resizeAll();"
  )
  
  
  library(dplyr)
  tabs = w2tabs(id="xsTabs",tabs=list())

  
  
  panes = jqueryLayoutPanes(id="xsPanes",json.opts=json.opts,
  	north = div(menubar,thinHR()),
    west = tagList(
      tree,
      cm
    ),
    center = div(
      tabs,
      div(id = "mainDiv")
    )
  )  
  
  
  init.js = paste0('xecon.init(',xecon.glob$xs_types.json,');')
  
  www.dir = system.file('www', package='XEconDB')
  ui = bootstrapPage(
    contextMenuHeader(),
    fancytreeHeader(extensions=c("table","gridnav","dnd")),
    w2header(),
    aceEditorHeader(),
    jqueryLayoutHeader(),
    handsontableHeader(),
    includeCSS(paste0(www.dir,"/xs.css")),
    singleton(tags$head(tags$script(src="xecon/xs_gametree.js"))),
    singleton(tags$head(tags$link(href="xecon/xs.css"))),
    panes,
    bottomScript(HTML(init.js))
  )
  
  
  
  eventHandler("close","xsTabs", function(...,tabId, divId, xs=app$xs) {
  	restore.point("xsTabs close")
  	cat("xsTabs.close: ", tabId)
  	
  	# destroy content of equilibrium tabs
  	# in order to work correctly when
  	# closed and opened again
  	#if (str.starts.with(tabId,"tab_eq_")) {
  	#	w2tabs.destroy.tab.content(divId)
  	#}
  	
    xs$tabs = setdiff(xs$tabs, tabId)
  })
  
  clickHandler("xsTabs", function(...) {
    args = list(...)
    restore.point("xsTabsClick")
  })
  clickHandler("projTree", function(...) {
    args = list(...)
    restore.point("projTreeClick")
    nodeType = args$data$nodeType
    if (is.null(nodeType)) return(NULL)
    if (nodeType == "game") {
      xs.show.game.tab(args$data$gameId)
    }
    
  })
  ui
}

xs.project.tree.nodes = function(xs=app$xs, app=getApp()) {
  restore.point("xs.project.tree.nodes")

  n = length(xs$gamesId)
  game.nodes = NULL
  if (length(n)>0) {
    game.nodes = data_frame(key = paste0("gameNode_",xs$gamesId), title=xs$gamesId, expanded=TRUE, nodeType = "game", gameId=xs$gamesId, extraClasses="projNode_game")
  }
  tree.nodes = list(
    list(key = "projTreeGames", title = "Games", folder=TRUE, expanded=TRUE, nodeType="games", children = game.nodes,extraClasses="projNode_games")
  )
  tree.nodes  
  
}

xs.update.project.tree = function(xs=app$xs, app=getApp()) {
  restore.point("xs.update.project.tree")
  tree.nodes = xs.project.tree.nodes(xs)
  fancytree.update.source("projTree",tree.nodes)
}

showConfirmModal = function(ok.fun, content=HTML(msg), msg="Please confirm", ok.label="Ok", cancel.label="Cancel", title="",fade=FALSE, ns=NS(random.string(20)), cancel.fun=function(...) {removeModal()},form.ids = NULL, args=list(), ...) {
	restore.point("showConfirmModal")
	
	
	ok.id = ns("okBtn")
	cancel.id = ns("cancelBtn")
	
	buttonHandler(ok.id, function(formValues,...) {
		fun.args = c(list(formValues=formValues),args)
		res = do.call(ok.fun,fun.args)
  	if (identical(res,FALSE)) return()
  	removeModal()
  })
  buttonHandler(cancel.id, function(...) {
  	cancel.fun(...)
  })
  
  showModal(modalDialog(fade = fade,title=title,content,...,
  	footer = tagList(
  		smallButton(ok.id,ok.label,form.ids = form.ids),
  		smallButton(cancel.id,cancel.label)
  	)
  ))

}


showGameNameModal = function(ok.fun, default.name="", msg="Enter the new game name", title="", help.txt = "", ns=NS(random.string(10)), ...) {
	restore.point("showGameNameModal")
	
	content = tagList(
		HTML(msg),
		textInput(ns("newId"),"", value=default.name),
		uiOutput(ns("help"))
	)
	dsetUI(ns("help"),"")
	new.ok.fun = function(formValues, ...) {
		restore.point("new.ok.fun")
		gameId = formValues[[ns("newId")]]
		res = check.new.gameId(gameId)
		if (!res$ok) {
			ui = HTML(colored.html(res$msg))
			setUI(ns("help"),ui)
			dsetUI(ns("help"),ui)
			return(FALSE)
		}
		ok.fun(gameId=gameId)
		return(TRUE)
	}
	showConfirmModal(new.ok.fun,content=content, title=title, form.ids = ns("newId"))
}

check.new.gameId = function(gameId) {
	if (length(gameId)==0) {
		return(list(ok=FALSE, msg="You have to enter a gameId."))
	}
	
	
	if (nchar(gameId)==0) {
		return(list(ok=FALSE,msg="You have to enter a gameId."))
	}
	allowed.chars = c(letters,LETTERS,0:9)
	chars = strsplit(gameId, split="", fixed=TRUE)[[1]]
	if (!all(chars %in% allowed.chars)) {
		return(list(ok=FALSE, msg="Your gameId can only consist of letters and numbers. Use camel case, e.g. 'UltimatumGame', to compose words. Underscores, like in 'ultimatum_game', are not allowed because underscores are used to specify game variants."))
		
	}
	
	if (does.game.exist(gameId)) {
		return(list(ok=FALSE, msg=paste0("A game with gameId '",gameId,"' does already exist in your project.")))
	}
	return(list(ok=TRUE, msg=""))
}


xs.delete.game = function(gameId, xs = app$xs, app=getApp()) {
  restore.point("xs.delete.game")
	if (isTRUE(xs$demo.mode)) {
		demo.mode.alert(); return();
	}
	
  xs$gamesId = setdiff(xs$gamesId, gameId)

  # remove complete game director
  dir = file.path(xs$games.dir, gameId)
	ok.fun = function(...){
 		cat("\n delete ", dir)
		close.game.tabs(gameId)
		try(unlink(dir, recursive=TRUE))
		
	} 
  showConfirmModal(ok.fun = ok.fun,
  	title = "Confirm Deletion",
  	msg = paste0("Are you sure you want to delete the game ", gameId, " including all pages and computed equilibria?"),
  	ok.label = "Delete"
  )
  
  # 
  # buttonHandler("delOkBtn", function(...) {
  # 	cat("\n delete ", dir)
  # 	#try(unlink(dir, recursive=TRUE))
  # 	removeModal()
  # })
  # buttonHandler("cancelModalBtn", function(...) {
  # 	removeModal()
  # })
  # 
  # showModal(modalDialog(fade = FALSE,title="Confirm Deletion",tagList(
  # 	tags$p(paste0("Are you sure you want to delete the game ", gameId, " including all pages and computed equilibria?"))
  # ), footer = tagList(smallButton("delOkBtn","Delete"),smallButton("cancelModalBtn","Cancel"))
  # ))
  
  
  xs.update.project.tree()
  
}

xs.new.game.click = function(gameId="NewGame", xs=app$xs, app=getApp(), json=NULL) {
  restore.point("xs.new.game.click")
	if (isTRUE(xs$demo.mode)) {
		demo.mode.alert(); return();
	}
	ok.fun = function(gameId,...) {
		cat("\nmake new game...")
		xs.new.game(gameId = gameId)
	}
	showGameNameModal(ok.fun, title="Create new game",default.name = "")	
}



xs.new.game = function(gameId="NewGame", xs=app$xs, app=getApp(), json=NULL) {
  restore.point("xs.new.game")
	if (isTRUE(xs$demo.mode)) {
		demo.mode.alert(); return();
	}
  
	make.game.dir(gameId)
	
  if (is.null(json))
    json = empty.jg.json(gameId)
  
  file = file.path(xs$games.dir,gameId,paste0(gameId,".json"))
  writeLines(json, file)
  
  xs$gamesId = unique(c(xs$gamesId,gameId))
  xs.show.game.tab(gameId)
  xs.update.project.tree()
}

xs.duplicate.game = function(gameId, xs=app$xs, app=getApp()) {
	restore.point("xs.duplicate.game")
	cat("\nduplicate game", gameId,"\n")
	if (isTRUE(xs$demo.mode)) {
		demo.mode.alert(); return();
	}
	
	index=2
	while((newId <- paste0(gameId,index)) %in% xs$gamesId) index = index+1
	
	oldId = gameId
	ns = "xs-dupl-game"
	
	dupl.fun = function(gameId,...) {
		restore.point("dupl.fun")
		newId = gameId
		jg = get.jg(oldId)
		jg$gameId = newId
		json = jg.to.json(jg)
		make.game.dir(newId)
		# copy pages
		file.copy(from=get.pages.dir(oldId),to=get.game.dir(newId),recursive = TRUE)
		xs.new.game(gameId=newId,json=json, xs=xs)
	}
	showGameNameModal(dupl.fun,title=paste0("Duplicate game ", oldId),default.name = newId)
}

xs.rename.game = function(gameId, xs=app$xs, app=getApp()) {
	restore.point("xs.rename.game")
	cat("\nrename game", gameId,"\n")
	if (isTRUE(xs$demo.mode)) {
		demo.mode.alert(); return();
	}
	
	oldId = gameId
	ns = "xs-rename-game"
	rename.fun = function(gameId,...) {
		restore.point("rename.fun")
		newId = gameId
		jg = get.jg(oldId)
		jg$gameId = newId
		json = jg.to.json(jg)
		make.game.dir(newId)
		# copy pages
		file.copy(from=get.pages.dir(oldId),to=get.game.dir(newId),recursive = TRUE)
		old.dir = get.game.dir(oldId)
		try(unlink(old.dir,recursive = TRUE))
		xs$gamesId = setdiff(xs$gamesId, oldId)

		xs.new.game(gameId=newId,json=json, xs=xs)
	}
	showGameNameModal(rename.fun,title=paste0("Rename game ", oldId),default.name = oldId,msg = "Note that renaming will delete the cache of all equilibrium computation.<br>Enter the new name.")
}


does.game.exist = function(gameId, project.dir=get.project.dir()) {
	dir.exists(file.path(project.dir,"games", gameId))
}

xs.show.game.tab = function(gameId, xs=app$xs, app=getApp()) {
  restore.point("xs.show.game.tab")
  cat("xs.show.game.tab")
  tabId = paste0("tab_game_",gameId)
  if (tabId %in% xs$tabs) {
    w2tabs.select("xsTabs", tabId)
    return()
  }
  xs$tabs = c(xs$tabs, tabId)
  
  divId = paste0("div_game_",gameId)
  tab=list(id=tabId,caption=gameId, closable=TRUE,div_id = divId, keep_closed_content=TRUE)
  w2tabs.add(id="xsTabs", tabs=list(tab))
  
  ui = xs.game.ui(gameId)
  appendToHTML(selector="#mainDiv", as.character(hidden_div(id=divId, ui)))
  w2tabs.select("xsTabs", tabId)
}


xs.game.ui = function(gameId, xs = app$xs, app=getApp()) {
  restore.point("xs.game.edit.ui")
  ns = NS(gameId)
  
	jg = try(get.jg(gameId))
	cat("\n",jg$stages[[1]]$name)
  if (is(jg,"try-error")) {
    ui = tagList(h4("Error when parsing json file:"), p(as.character(jg)))
    return(ui)
  }

  varparId = paste0("xsVarPar_",gameId)
 	treeId = paste0("xsGameTree_",gameId)
  json = jg.to.json(jg)
  
  table = paste0('<table id="',treeId,'"  width="">
    <colgroup>
    <col></col>
    <col></col>
    <col width="*"></col>
    </colgroup>
    <thead>
        <tr> <th>Nodes</th> <th>Value</th> <th>Info</th></tr>
    </thead>
    <tbody>
    </tbody>
  ')

  btnId = paste0("saveBtn_",gameId)
  checkBtnId = paste0("checkBtn_",gameId)
  js = paste0('xecon.initGame("',gameId,'",',json,')')
  ui = tagList(
    smallButton(btnId,"Save"),
    smallButton(checkBtnId,"Check"),
    smallButton(ns("otreeBtn"),"To OTree"),
    smallButton(ns("eqBtn"),"Equilibria"),
    smallButton(ns("runBtn"),"Run"),
    uiOutput(ns("msg")),
  	# varpar table
		#HTML(paste0('<div id="',varparId,'"></div>')),
  	# game tree
    div(
      HTML(table)
    ),
    tags$script(HTML(js))
  )
  buttonHandler(btnId,gameId=gameId, function(gameId,...) {
    callJS("xecon.parseAndSendGame",gameId,"save")
  })
  buttonHandler(checkBtnId,gameId=gameId, function(gameId,...) {
    callJS("xecon.parseAndSendGame",gameId,"check")
  })
  buttonHandler(ns("otreeBtn"),gameId=gameId,xs.to.otree.click)
  buttonHandler(ns("eqBtn"),gameId=gameId,xs.eq.click)
  buttonHandler(ns("runBtn"),gameId=gameId,xs.run.click)
  
  eventHandler("parseGameEvent","parseGameEvent",function(mode,...) {
    args = list(...)
    restore.point("parseGameEvent")
    if (mode=="save") {
      xs.save.game.click(...)
    } else {
      xs.check.game.click(...)
    }
    cat("Game is parsed.")
    content = args$content
    print(content)
  })
  
  ui
}

xs.eq.click = function(gameId,...,xs=app$xs, app=getApp()) {
  restore.point("xs.eq.click")
	xs.show.eq.tab(gameId=gameId)
}


xs.run.click = function(gameId,...,xs=app$xs, app=getApp()) {
  restore.point("xs.run.click")
	cat("\nxs.run.click called!\n")
	xs.show.run.tab(gameId=gameId)
}



xs.to.otree.click = function(gameId,...,xs=app$xs, app=getApp()) {
  restore.point("xs.to.otree.click")
	if (isTRUE(xs$demo.mode)) {
		demo.mode.alert(); return();
	}
	
	
  ns = NS(gameId)
  jg = get.jg(gameId)
  timedMessage(ns("msg"),"Export to otree...", millis = Inf)
  jg.to.otree(jg, otree.dir = xs$otree.dir, msg.id=ns("msg"))  
  timedMessage(ns("msg"),"Export to otree... all files written.")
  
  if (!is.null(xs$otree.url)) {
    timedMessage(ns("msg"),"Export to otree: Call 'otree resetdb'. See console window for output...", millis=Inf)
    otree.resetdb(otree.dir = xs$otree.dir)
    timedMessage(ns("msg"),"Export to otree: Call 'otree runserver'. See console window for output...", millis=Inf)
    otree.runserver(otree.dir = xs$otree.dir)
    timedMessage(ns("msg"),millis=Inf,ui=tagList(
    	tags$a(href=xs$otree.url, target="_blank", paste0("Open otree server under ", xs$otree.url))
    ))
    open.url.from.app(xs$otree.url)
  }
  
}



xs.save.game.click = function(json, value, gameId,...,xs=app$xs, app=getApp()) {
  restore.point("xs.save.game.click")
	if (isTRUE(xs$demo.mode)) {
		demo.mode.alert(); return();
	}
	
  ns = NS(gameId)
  cat("\nsave game...")
  li = fromJSON(json)
  
  
  new.game = (!identical(gameId,li$gameId))
  
  
  gameId = li$gameId
  
  json = paste0('{"game": ',json,'}')
  file = file.path(xs$games.dir,gameId,paste0(gameId,".json"))
  writeLines(json, file)
  
  if (new.game) {
    xs.new.game(gameId=gameId, json=json)
  }
  timedMessage(ns("msg"),paste0("Game saved to ", file), millis=2000)
  
  
}

xs.check.game.click = function(json, value, gameId,..., xs= app$xs, app=getApp()) {
  restore.point("xs.check.game.click")
  cat("\ncheck game...")
  ns = NS(gameId)
  setUI(ns("msg"),HTML("Check game syntax..."))
  dsetUI(ns("msg"),HTML("Check game syntax..."))
  #return()
  callJS("xecon.clearGameTreeErrors",gameId)
  
  jg = value
  rg = get.rg(jg = jg, games.dir=xs$games.dir)
  if (rg$kel$count>0) {
    callJS("xecon.showGameTreeErrors",gameId, rg$kel$log)
    timedMessage(ns("msg"),"There are problems found. Scroll below for details.")

    return(FALSE)
  }
  
  for (variant in rg$variants) {
	  vg = get.vg(rg=rg,variant = variant, games.dir=xs$games.dir, always.new=TRUE)
	  if (vg$kel$count>0) {
	    callJS("xecon.showGameTreeErrors",gameId, vg$kel$log)
	    timedMessage(ns("msg"),"There are problems. Scroll below for details.")
	    return(FALSE)
	  }
  }
  
  #tg = get.tg(vg=vg, games.dir=xs$games.dir,branching.limit=1000)
  
  #if (tg$kel$count>0) {
  #  callJS("xecon.showGameTreeErrors",gameId, tg$kel$log)
  #  timedMessage(ns("msg"),"There are problems. Scroll below for details.")
  #  return(FALSE)
  #}
  timedMessage(ns("msg"),"Congrats, no errors found!")
  
  
}



addXEconRessourcePath = function() {
  restore.point("addXEconRessourcePath")
  www.dir = system.file('www', package='XEconDB')
  # init ressource paths
  shiny::addResourcePath(
    prefix = 'xecon',
    directoryPath = www.dir
  )

}


xs.load.ressources = function() {
  restore.point("xs.load.ressources")
  file = system.file('spec/xs_types.json', package='XEconDB')
  xecon.glob$xs_types.json = merge.lines(readLines(file, warn=FALSE))
  xecon.glob$xs_types = fromJSON(xecon.glob$xs_types.json,simplifyDataFrame = FALSE,simplifyMatrix = FALSE,simplifyVector = FALSE)
}

r.to.js.arg = function(x) {
  if (is.list(x) | length(x)>1) {
    return(toJSON(x))
  }
  if (is.character(x) | is.factor(x)) {
    return(paste0('"',x,'"'))
  }
  x
}

js.call = function(.fun,...,.args=list(...), .json.args = lapply(.args, r.to.js.arg)) {
  args = list(...)
  restore.point("js.call")
  code = paste0(.fun,"(",paste0(.json.args, collapse=","),");")
  code
}

jg.to.json = function(jg) {
	toJSON(list(game=jg),auto_unbox = TRUE)
}

# an empty game with given gameId
empty.jg.json = function(gameId) {
  paste0('
  {"game": {
    "gameId": "', gameId,'",
    "gameInfo": {
        "label": "",
        "tags": "",
        "descr": "",
        "articles": "",
        "variantOf": ""
    },
    "varpar": [
        [
            "variants<U+2193> params<U+2192>",
            "numPlayers",
            "descr"
        ],
        [
            "base",
            "2",
            "The base variant"
        ]
    ],
    "stages": [
       {
            "name": "resultsStage",
            "player": "[1,2]",
            "condition": "",
            "observe": "",
            "nature": [],
            "actions": [],
            "special": {
                "beliefs": [],
                "freetext": []
            },
            "compute": [
                {
                    "name": "payoff_1",
                    "formula": "=0"
                },
                {
                    "name": "payoff_2",
                    "formula": "=0"
                }
            ]
        }
    ]
}}
  ')
}


demo.mode.alert = function(title="Action not feasible in gtree demo mode", msg='<p>This action cannot be performed since gtree runs in demo mode on this public server.</p>
<p>For unrestricted usage please visit the gtree Github page<br><br> 
<a target ="_blank" href="https://github.com/skranz/gtree">https://github.com/skranz/gtree</a>
<br><br>
and follow the installation instructions to install your own local version of gtree.') {
	showModal(modalDialog(HTML(msg),title=title,easyClose = TRUE))
}

close.game.tabs = function(gameId,types=c("run","eq"), xs=app$xs, app=getApp()) {
	restore.point("close.game.tabs")
	
	types.prefix = c(game="tab_game_",run="tab_run_",eq="tab_eq_")
	type = types[1]
	for (type in types) {
		prefix = types.prefix[type]
		tabs = xs$tabs[str.starts.with(xs$tabs,prefix)]
		right = str.right.of(tabs, prefix)
		tabs = tabs[has.substr(right,gameId)]
		tabs
		xs$tabs = setdiff(xs$tabs, tabs)
	  for (tabId in tabs) {
			if (type == "run") {
  			divId = paste0("div_run_", str.right.of(tabId,"tab_run_"))
  			w2tabs.destroy.tab.content(divId)
			} else if (type == "eq") {
  			divId = paste0("div_eq_", str.right.of(tabId,"tab_eq_"))
  			w2tabs.destroy.tab.content(divId)
			}	  	
  		w2tabs.close("xsTabs", tabId)
	  }

	}

}
