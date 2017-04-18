examples.run.game = function() {
  setwd("D:/libraries/XEconDB/")
  set.restore.point.options(display.restore.point=TRUE)
  options(shiny.trace=!TRUE)
  init.ee()
  gameId = "UltimatumGame"
  
  sfg = load.sfg(gameId)
  
  dir = "D:/libraries/XEconDB/gx/projects/UltimatumGame/forms"
  file = "Forms_UltimatumGame.yaml"
  forms = read.yaml(paste0(dir,"/",file))
  run.game(sfg,forms=forms)  
  
  
  #sfg = load.sfg("ug_stages")
  #sfg = load.sfg("LureOfAuthorityAlternative")
  stages = sfg.stages(sfg)
  sfg.variables.table(sfg)
  #obs = sfg.objects(sfg, parentType="payoffs")
  options(shiny.reactlog=TRUE)
  run.game(sfg, proposer=NULL)  

  dat = load.game.data(gameId)
  dat = filter(dat, offer <=35, variant=="B")
  prop = data.action.proposer(dat)
  prop = loop.data.action.proposer(dat)
  prop$row = 11

}

run.game = function(sfg, variant=sfg.variants(sfg)[1], proposer=NULL, launch.browser=rstudio::viewer, select.match = TRUE, title=NULL, forms=NULL) {
  restore.point("run.game")

  sfg.obj = tt.object(sfg,1)
  if (is.null(title))
    title = sfg.obj$gameId  
  
  ui = fluidPage(title = title, theme = NULL,shiny.game.ui())
  app = eventsApp(set.as.default = TRUE, verbose = FALSE)
  setAppUI(ui=ui)
  
  if (is.null(variant)) select.match = TRUE

  if (select.match) {
    game = init.shiny.raw.game(sfg=sfg, forms=forms)
    game$variant = variant
    game$proposer = proposer
    set.shiny.game.ui(game.start.ui(game = game))
  } else {
    game = init.shiny.game(sfg=sfg,variant=variant, proposer=proposer, forms=forms)
    set.shiny.game.ui(shiny.game.panel.ui(game))
    run.next.stages(game)
  }
  runEventsApp(app, ui=ui, launch.browser=launch.browser)
}

shiny.game.ui = function() {
  uiOutput("shinyGameUI")
}

set.shiny.game.ui = function(ui) {
  setUI("shinyGameUI",ui)
}

# Game without specificiation of variants, data or proposer
init.shiny.raw.game = function(sfg, data=NULL, forms=NULL) {
  restore.point("init.shiny.raw.game")
  values = list()
  obj = tt.object(sfg)
  
  stages = sfg.stages(sfg)
  n = sfg.num.players(sfg)
  act.stage = 0
  is.waiting = rep(TRUE,n)
  var.tab = sfg.variables.table(sfg)
  
  subject.id = paste0("SHINY_",sample.int(1e3,n,replace=FALSE))
  
  act.actions = act.observe = vector("list",n)
  
  know = replicate(2,"variant", simplify=FALSE)
  
  proposer = vector("list",n)
  
  game = list(sfg=sfg, stages=stages,gameId=obj$gameId, n=n, act.stage=act.stage, act.actions=act.actions,act.observe=act.observe, is.waiting=TRUE,var.tab=var.tab, values=values, variant=NULL, data=data, subject.id=subject.id, know = know, proposer = proposer, forms=forms)

  game = as.environment(game)
  game
}

init.shiny.game = function(game=NULL,sfg=game$sfg, variant=game$variant,data=game$data, proposer = game$proposer) {
  restore.point("init.shiny.game")
  
  if (is.null(game))
    game = init.shiny.raw.game(sfg)
  
  # init all parameters
  values = list(variant=variant)
  pars = sfg.parameters(sfg)
  par = pars[[1]]
  for (par in pars) {
    values[[get.name(par)]] <- sapply(par$formula.calls, eval, envir=values)
  }
  game$values = values
  
  # init proposers
  game$proposer = proposer
  for (i in seq_along(proposer)) {
    prop = proposer[[i]]
    if (!is.null(prop))
      prop$new.match(player=i,game=game)
  }
  
  game$act.stage = 0
  game$is.waiting = rep(TRUE,game$n)

  game
}

nextStageBtnClick = function(id, session,game, player,...) {
  restore.point("nextStageBtnClick")
  
  #stop("huhfuhfu")
  actions = game$act.actions.obj[[player]]
  action.names = names(actions)
  
  game$know[[player]] = union(game$know[[player]], action.names)
  
  
  # Read decisions
  if (length(actions)>0) {
    an = action.names[[1]]
    for (an in action.names) {
      val = isolate(session$input[[paste0(an,".",player)]])
      action =actions[[an]] 
      
      if (!is.null(action$set))
        val = as(val, class(action$set))
      
      game$values[[an]] = val
    }
  }
  
  run.next.stages(game)
}

run.next.stages = function(game) {
  restore.point("run.next.stages")
  #stop("jfjdf")
  if (game$act.stage == length(game$stages)) {
    return(finish.game(game))
  }
  
  act.stage = game$act.stage = game$act.stage + 1
  restore.point("run.next.stages.2")
  
  
  if (act.stage==3)
    restore.point("run.next.stages.3")
  
  stage = game$stages[[act.stage]]
  stage.name = names(game$stages)[[act.stage]]
  game$stage.name = stage.name
  cat("\nrun.stage ",act.stage, ": ", stage.name)

  run.stage = TRUE
  if (!is.null(stage$condition.call)) {
    run.stage = eval.formula(call=stage$condition.call, envir=game$values)
  } else {
    run.stage = TRUE
  }
  
  if (run.stage) {
    run.stage.computations(act.stage, game)
    
    if (!is.null(stage$player))
      restore.point("iudhuidhg with players")
    
    players = eval.formula(text=stage$player, envir=game$values)
    i = 1
    for (i in players) {
      set.act.actions.and.observe(stage=stage.name, player=i, game=game)
      set.stage.ui(stage = stage.name,player = i,game = game) 
    }
    for (i in setdiff(1:game$n, players)) {
      id = paste0("uiPlayer",i)
      stage.ui = wait.ui(game = game)
      setUI(id,stage.ui)  
    }
    
    if (length(players)==0)
      return(run.next.stages(game))
    
  } else {
    return(run.next.stages(game))
  }
  return(game)
}

finish.game = function(game) {
  restore.point("finish.game")
  df.row = game.to.data.row(game)
  
  if (is.null(game$data)) {
    game$data = df.row
  } else {
    game$data = rbind(game$data, df.row)
  }
  
  game$finished = TRUE
  game.results.ui = game.results.ui(game)
  set.shiny.game.ui(game.results.ui)
  return(game)

}

run.stage.computations = function(stage.ind, game) {
  restore.point("run.stage.computations")
  stage.name = names(game$stages)[stage.ind]
  
  vars = sfg.stage.variables(game$sfg,stage = stage.name)
  for (var in vars) {
    var.name = get.name(var)
    val = NULL
    if (is.subtype(get.typeName(var),"formulaVariable")) {
      val = sapply(var$formula.calls, eval, envir=game$values)
    } else if (is.subtype(get.typeName(var),"randomVariable")) {
      val= sample.randomVariable(var,T=1, env=game$values)
    }
    if (!is.null(val))
      game$values[[var.name]] = val
    if (length(val)>1) {
      var.names = paste0(var.name,"_",seq_along(val))
      li = as.list(val)
      names(li) = var.names
      game$values[var.names] = li 
    }
    
  }
  
  
}

sample.randomVariable = function(obj,T=1, env) {
  restore.point("sample.randomVariable")
  prob = sapply(obj$prob.calls, function(ca) {
    eval(ca, env)
  })
  sample(obj$set, size=T, replace=TRUE, prob=prob)
}

shiny.game.panel.ui = function(game) {
  restore.point("shiny.game.panel.ui")

  panel.li = lapply(1:game$n, function(i) {
    tabPanel(title=paste0("Player ",i), value=paste0("tabPlayer",i), 
             uiOutput(paste0("uiPlayer",i)))
  })
  tabset.ui = do.call("tabsetPanel", c(list(id="playersTabset"),panel.li))
  
  return(tabset.ui)
}



set.stage.ui = function(stage, player=1, game) {
  restore.point("set.stage.ui")
  id = paste0("uiPlayer",player)
  stage.ui = make.stage.ui(stage,player,game)
  setUI(id,stage.ui)  
}

eval.obj.condition = function(obj, env) {
  if (is.null(obj$condition))
    return(TRUE)
  eval(obj$condition.call,env)
}

# Set the current actions and observations of a player given that
# a particular stage is shown
set.act.actions.and.observe = function(stage, player, game) {
  restore.point("set.act.actions.and.observe")
  sfg = game$sfg
  row = which(is.subtype(sfg$typeName,"stage") & sfg$name==stage)
  st = tt.subtree(sfg,row)

  rows = which(is.subtype(st$typeName,"action") | is.subtype(st$typeName,"freeAction"))

  game$act.actions[[player]] = NULL

  actions = tt.objects(st, rows)
  
  if (length(actions)>0) {
    cond = sapply(actions, eval.obj.condition, env=game$values)
    actions = actions[cond]  
  }
  game$act.actions[[player]] = names(actions)
  game$act.actions.obj[[player]] = actions

  observe = sfg.objects(st, parentType="observe")
  li = lapply(observe, function(obs) {
    if (is.null(obs$condition)) return(obs$variables)
    
    cond = eval(parse(text=obs$condition, srcfile=NULL), game$values)
    if (cond) return(obs$variables)
    NULL    
  })  
  observe =  unlist(li)
  game$act.observe[[player]] = observe
  game$know[[player]] = union(game$know[[player]], observe)
}

get.act.proposed.actions = function(stage,player, game) {
  restore.point("get.act.proposed.actions")
  actions = game$act.actions[[player]]
  if (length(actions)==0)
    return(list)
  prop = game$proposer[[player]]
  if (is.null(prop)) {
    li = lapply(actions, function(action) NULL)
  } else {
    li = lapply(actions, function(action) {
      prop$ask(action=action,stage=stage, player=player, game=game)$value    
    })    
  }
  names(li) = actions
  li
}



newMatchBtnClick = function(session,game, ...) {
  variant = isolate(session$input$variant)
  restore.point("newMatchBtnClick")  

  game = init.shiny.game(game=game)
  set.shiny.game.ui(shiny.game.panel.ui(game))
  run.next.stages(game)  
}

game.start.ui = function(sfg=game$sfg,game=NULL) {
  restore.point("game.start.ui")
  variants = sfg.variants(sfg)
  
  variant = game$variant
  if (is.null(variant)) variant = variants[[1]]
  
  btnId = "newMatchBtn"
  buttonHandler(btnId,newMatchBtnClick, if.handler.exists="replace", game=game)

  cat("\n after buttonHandler")
  ui = list(
    selectInput(inputId = "variant","Variant:",choices = variants, selected=variant),
    actionButton(btnId, "New Match...")
  )
  ui
}

game.results.ui = function(game) {
  restore.point("game.results.ui")
#   results = lapply(names(game$values), function(var) {
#     p(paste0(var, " = ", paste0(game$values[[var]],collapse=",")))
#   })
#   results = c(list(h3("Results:"),results))
  
  variants = sfg.variants(sfg = game$sfg)
  btnId = "newMatchBtn"
  buttonHandler(btnId,newMatchBtnClick, sfg=game$sfg, if.handler.exists="replace", game=game)

  nr = 100
  rows = NROW(game$data):max(1,NROW(game$data)-nr)
  data.html = html.table(game$data[rows,], row.names=FALSE)
  
  cat("\n adfter buttonHandler")
  ui = fluidRow(column(offset=1,width=11,
    selectInput(inputId = "variant","Variant for next match:",choices = variants, selected=game$variant),
    actionButton(btnId, "New Match..."),
    h4("Results (all matches):"),
    HTML(data.html)
  ))

  ui
   
}

unlist.value.list = function(val) {
  # Generate vector variables
  len = sapply(val, length)
  names = unlist(use.names=FALSE,lapply(seq_along(val), function(i) {
    if (len[i]==0) return(NULL)
    if (len[i]==1)
      return(names(val)[i])
    paste0(names(val)[i],"_", 1:len[i])
  }))
  li = lapply(seq_along(val), function(i) {
    as.list(val[[i]])
  })
  li = do.call("c",li)
  
  names(li) = names
  li
  
}

game.to.data.row = function(game, output.format = "data.table") {
  restore.point("game.to.table.rows")
  
  val = game$values
  li = unlist.value.list(val)  

  var.tab = game$var.tab
  
  out = replicate(NROW(var.tab),NA, simplify=FALSE)
  names(out)=var.tab$name
  fields = intersect(var.tab$name, names(li))
  out[fields] = li[fields]
  
  n = game$n
  subject.list = as.list(game$subject.id)
  names(subject.list) = paste0("subject_",1:n)
  out = c(out, subject.list, list(TIME_STAMP=as.character(now())))
  
  if (output.format =="data.table")
    return(as.data.table(out))
  if (output.format =="data.frame")
    return(as.data.frame(out,stringsAsFactors=FALSE))
  out
}

