# xecon studio

xecon.glob = new.env()

examples.xsApp = function() {
  projects.dir = "D:/libraries/XEconDB/projects"
  app = xsApp(projects.dir)
  viewApp(app,launch.browser = TRUE)
}


xsApp = function(projects.dir, project=1, otree.dir=NULL, otree.url="http://localhost:8000") {
  restore.point("xsApp")
  
  library(shinyEventsUI)
  addXEconRessourcePath()
  xs.load.ressources()
  
  app = eventsApp()
  app$xs = xs = new.env()
  
  xs$projects.dir = projects.dir
  xs$otree.dir = otree.dir
  xs$otree.url = otree.url
  
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
  xs.load.games(xs)
  
}

xs.load.games = function(xs) {
  restore.point("xs.load.games")
  
  if (is.null(xs$project)) return(NULL)
  gamesId = list.files(paste0(xs$project,"/games"), pattern=glob2rx("*.json"), full.names=FALSE)
  gamesId = str.remove.ends(gamesId,0,5)
  xs$gamesId = gamesId
  xs$games = lapply(gamesId, function(gameId) {
    as.environment(list(gameId=gameId))
  })  
  names(xs$games) = gamesId
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
  	 treeNodeContextMenu(id="cmProjGame",node.class = "projNode_game", items=list(new = list(name="New game"), del=list(name="Remove game"), rename=list(name="Rename game"), duplicate = list(name="Duplicate game")
  	))
  )
  
  contextMenuHandler("cmProjGame", function(key,data, session=NULL,...){
  	args = list(...)
  	nodeType = data$nodeType
  	gameId = data$gameId
  	restore.point("cmProjGameHandler")
  	cat("\ncontext menu key: ", key)
  	
  	if (key=="new" && nodeType %in% c("game","games")) {
  	  xs.new.game()	
  	} else if (key=="del" && nodeType == "game") {
  	  xs.delete.game(gameId = gameId)
  	} else if (key=="duplicate" && nodeType == "game") {
  	  xs.duplicate.game(gameId = gameId)
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
  
  
  
  eventHandler("close","xsTabs", function(...,tabId, xs=app$xs) {
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

xs.delete.game = function(gameId, xs = app$xs, app=getApp()) {
  restore.point("xs.delete.game")
  xs$gamesId = setdiff(xs$gamesId, gameId)
  xs$games = xs$games[xs$gamesId]

  file = file.path(xs$games.dir,paste0(gameId,".json"))
  try(file.remove(file))
  
  xs.update.project.tree()
  
}

xs.new.game = function(gameId="NewGame", xs=app$xs, app=getApp(), json=NULL) {
  restore.point("xs.new.game")
  
  if (is.null(json))
    json = empty.jg(gameId)
  
  file = file.path(xs$games.dir,paste0(gameId,".json"))
  writeLines(json, file)
  
  xs$games[[gameId]] = as.environment(list(gameId=gameId))
  xs$gamesId = unique(c(xs$gamesId,gameId))
  xs.show.game.tab(gameId)
  xs.update.project.tree()
}

xs.duplicate.game = function(gameId, xs=app$xs, app=getApp()) {
	restore.point("xs.duplicate.game")
	cat("\nduplicate game", gameId,"\n")
	
	index=2
	while((newId <- paste0(gameId,index)) %in% names(xs$games)) index = index+1
	
	game=xs$games[[gameId]]
	content = game$content
	content$game$gameId = newId
	
	json = toJSON(content,auto_unbox = TRUE)

	xs.new.game(gameId=newId,json=json, xs=xs)
	 
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
  tab=list(id=tabId,caption=gameId, closable=TRUE,div_id = divId)
  w2tabs.add(id="xsTabs", tabs=list(tab))
  ui = xs.game.ui(gameId)
  appendToHTML(selector="#mainDiv", as.character(hidden_div(id=divId, ui)))
  w2tabs.select("xsTabs", tabId)
}

xs.load.game.content = function(gameId, xs = app$xs, app=getApp()) {
  restore.point("xs.load.game.content")
  game = xs$games[[gameId]]
  game$content.json = merge.lines(readLines(paste0(xs$games.dir,"/",gameId,".json"),warn = FALSE))
  game$content = try(fromJSON(game$content.json,simplifyDataFrame = FALSE,simplifyMatrix = FALSE,simplifyVector = FALSE))
  
  #varpar = game$content$game$varpar
  #game$content$game$tvarpar = t(do.call(rbind, varpar))
  #game$content.json = toJSON(game$content)
  
  game$content.error = is(game$content, "try-error")
  game$error.msg = as.character(game$content)
  game
}

xs.game.ui = function(gameId, xs = app$xs, app=getApp()) {
  restore.point("xs.game.edit.ui")
  game = xs$games[[gameId]]
  ns = NS(gameId)
  
  # always reload content
  game = xs.load.game.content(gameId=gameId)
  xs$games[[gameId]] = game
  if (game$content.error) {
    game$ui = tagList(h4("Error when parsing json file:"), p(as.character(game$error.msg)))
    return(game$ui)
  }

  game$varparId = paste0("xsVarPar_",gameId)
  game$treeId = paste0("xsGameTree_",gameId)
  
  table = paste0('<table id="',game$treeId,'"  width="">
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
  js = paste0('xecon.initGame("',gameId,'",',game$content.json,')')
  game$ui = tagList(
    actionButton(btnId,"Save"),
    actionButton(checkBtnId,"Check"),
    actionButton(ns("otreeBtn"),"To OTree"),
    actionButton(ns("gambitBtn"),"To Gambit"),
    uiOutput(ns("msg")),
  	# varpar table
		#HTML(paste0('<div id="',game$varparId,'"></div>')),
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
  buttonHandler(ns("gambitBtn"),gameId=gameId,xs.to.gambit.click)
  
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
  
  game$ui
}

xs.to.gambit.click = function(gameId,...,xs=app$xs, app=getApp()) {
  restore.point("xs.to.gambit.click")
  ns = NS(gameId)
  game = xs.load.game.content(gameId)
  if (game$content.error) {
    timedMessage(ns("msg"),paste0("Error when parsing .json file of the game:", game$error.msg))
    return()
  }
	jg = game$content$game
	
	timedMessage(ns("msg"),paste0("Create Gambit extensive form games..."),millis=Inf)
	
	rg = get.rg(jg=jg)
	variants = rg$variants
  
	for (variant in variants) {
		timedMessage(ns("msg"),paste0("Create Gambit extensive form game for variant ", variant, "..."),millis=Inf)
		
		tg = get.tg(variant = variant, rg = rg)
		tg.to.efg(tg=tg, path=xs$games.dir)
	}
  
}


xs.to.otree.click = function(gameId,...,xs=app$xs, app=getApp()) {
  restore.point("xs.to.otree.click")
  ns = NS(gameId)
  game = xs.load.game.content(gameId)
  if (game$content.error) {
    timedMessage(ns("msg"),paste0("Error when parsing .json file of the game:", game$error.msg))
    return()
  }
  timedMessage(ns("msg"),"Export to otree...", millis = Inf)
  jg = game$content$game
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
  ns = NS(gameId)
  cat("\nsave game...")
  li = fromJSON(json)
  new.game = (!identical(gameId,li$gameId))
  gameId = li$gameId
  
  json = paste0('{"game": ',json,'}')
  file = paste0(xs$games.dir,"/",gameId,".json")
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
  vg = get.vg(rg=rg, games.dir=xs$games.dir, always.new=TRUE)
  if (vg$kel$count>0) {
    callJS("xecon.showGameTreeErrors",gameId, vg$kel$log)
    timedMessage(ns("msg"),"There are problems. Scroll below for details.")
    return(FALSE)
  }
  
  #tg = get.tg(vg=vg, games.dir=xs$games.dir,max.rows=1000)
  
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

