# xecon studio

xecon.glob = new.env()

examples.xsApp = function() {
	restore.point.options(display.restore.point = TRUE)
  projects.dir = "D:/libraries/XEconDB/projects"
  app = xsApp(projects.dir, never.load.tg=FALSE)
  viewApp(app)
  viewApp(app,launch.browser = TRUE)
}


xsApp = function(projects.dir, project=1, otree.dir=NULL, otree.url="http://localhost:8000", never.load.tg = FALSE) {
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
  gamesId = list.files(paste0(xs$project,"/games"), pattern=glob2rx("*.json"), full.names=FALSE)
  gamesId = str.remove.ends(gamesId,0,5)
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

xs.delete.game = function(gameId, xs = app$xs, app=getApp()) {
  restore.point("xs.delete.game")
  xs$gamesId = setdiff(xs$gamesId, gameId)

  file = file.path(xs$games.dir,paste0(gameId,".json"))
  try(file.remove(file))
  
  xs.update.project.tree()
  
}

xs.new.game = function(gameId="NewGame", xs=app$xs, app=getApp(), json=NULL) {
  restore.point("xs.new.game")
  
  if (is.null(json))
    json = empty.jg.json(gameId)
  
  file = file.path(xs$games.dir,paste0(gameId,".json"))
  writeLines(json, file)
  
  xs$gamesId = unique(c(xs$gamesId,gameId))
  xs.show.game.tab(gameId)
  xs.update.project.tree()
}

xs.duplicate.game = function(gameId, xs=app$xs, app=getApp()) {
	restore.point("xs.duplicate.game")
	cat("\nduplicate game", gameId,"\n")
	
	index=2
	while((newId <- paste0(gameId,index)) %in% xs$gamesId) index = index+1
	
	
	jg = get.jg(gameId)
	jg$gameId = newId
	
	json = jg.to.json(jg)
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