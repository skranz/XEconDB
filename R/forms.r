examples.forms = function() {
  dir = "D:/libraries/XEconDB/gx/projects/UltimatumGame/forms"
  file = "Forms_UltimatumGame.yaml"
  
  forms = read.yaml(paste0(dir,"/",file))
  
  form.name = "acceptStage"
  values = list(player=1, cake=10, offer = 4)
  
  init.form(form.name,forms,values,action.objs)

  
  form.name = "resultsStage"
  values = list(player=1, cake=10, offer = 4, accept=1, payoff_1=6, payoff_2=4)
  
  res = init.form(form.name,forms,values)
  form = res$form; values = res$values
  
  forms
}

# resolve _includes and _if
init.form = function(form.name, forms, values=NULL,...) {
  restore.point("init.forms")
  oform = forms[[form.name]]

  form = include.and.switch.form.obj(oform, forms, values)
  values = add.form.transform.values(form, values)
  list(form=form, values=values)
}

include.and.switch.form.obj = function(obj, forms, values) {
  restore.point("include.and.switch.form.obj")
  
  if (!is.list(obj)) return(obj)
  names = names(obj)
  if (length(names)==0) return(obj)
  
  next.obj = NULL
  if (str.starts.with(names[1],"_if ")) {
    conds = str.right.of(names,"_if ")
    ok = sapply(conds, function(cond) {
      eval.formula(text=cond, envir=values)
    })
    if (!any(ok)) 
      return(NULL)
    ind = which(ok)[1]
      
    next.obj = obj[[ind]]
    return(include.and.switch.form.obj(next.obj,forms,values))  
  }
  
  li = lapply(names, function(name) {
    cobj = obj[[name]] 
    if (str.starts.with(name,"_include")) {
      next.obj = forms[[cobj]]
      next.obj = include.and.switch.form.obj(next.obj,forms,values)
      return(next.obj)
    }
    next.obj = include.and.switch.form.obj(cobj,forms,values)
    li = list(next.obj)
    names(li) = name
    li
  })
  do.call(c,li)
}

add.form.transform.values = function(form, values) {
  restore.point("add.form.transform.values")
  trans = form$transform
  if (length(trans)==0) return(values)
  
  tr.name = names(trans)[1]
  for (tr.name in names(trans)) {
    tab = as.data.frame(rbindlist(trans[[tr.name]]))
    val.name = str.right.of(tr.name,"_")
    row = match(values[[val.name]],tab[,1])
    val.code =  tab[[2]][row] 
    values[[tr.name]] = eval(parse(text=val.code),values)
  }
  values
}


make.stage.ui = function(stage,player=1,game, forms=game$forms,...) {
  restore.point("make.stage.ui")
  actions = game$act.actions.obj[[player]]
  proposed.actions = get.act.proposed.actions(stage,player, game) 
  obs.var = game$act.observe[[player]]
  values = c(list(player=player),game$values)
  
  form = forms[[stage]]
  form.name = stage
  if (is.null(form)) {
    form.fun = "defaultForm"
  } else {
    res = init.form(form.name,forms,values)
    form = res$form; values = res$values
    form.fun = form$formType
  }
  args = list(stage=stage, player=player, values=values, actions=actions, proposed.actions=proposed.actions, obs.var=obs.var, form=form)
  ui = do.call(form.fun, args)
  
  ui =   fluidRow(column(offset=1,width = 11,ui))
  
  btnId = paste0("nextStageBtn",player)
  buttonHandler(btnId,nextStageBtnClick, player=player,game=game, if.handler.exists="replace")

  ui

}

defaultForm = function(stage=stage, player=player, values=values, actions=actions, proposed.actions=proposed.actions,obs.var,...) {
  restore.point("defaultForm")
  
  if (length(actions)>0) {
    restore.point("make.stage.ui.with.actions")
    li = lapply(seq_along(actions), function(i) {
      make.action.ui(obj = actions[[i]], player=player, value=proposed.actions[[i]])
    })
    li = do.call("c",li)
    actions.ui = li
  } else {
    actions.ui = NULL
  }

  if (length(obs.var)>0) {
    li = lapply(obs.var, function(obs.var) {
      p(paste0(obs.var,": ", paste0(values[[obs.var]],collapse=",")))
    })
    observe.ui = c(list(h3("Observations:"),li))
  } else {
    observe.ui = NULL
  }
    
  btnId = paste0("nextStageBtn",player)
  ui = list(
    h2(stage),
    observe.ui,
    actions.ui,
    actionButton(btnId, "continue")
  )
  ui
}


basicForm = function(stage=stage, player=player, values=values, actions=actions, proposed.actions=proposed.actions,obs.var,form,...) {
  restore.point("basicForm")
  
  if (length(actions)>0) {
    restore.point("make.stage.ui.with.actions")
    i = 1
    li = lapply(seq_along(actions), function(i) {
      name = names(actions)[i]
      aobj = actions[[i]]
      
      args = list(
        label = name,
        choices=aobj$set,
        choiceLabels = NULL,
        player = player,
        value = proposed.actions[[i]]
      )
      fobj = form$actions[[name]]
      fobj = lapply(fobj, function(x) remove.quotes(x))
      args[names(fobj)] = fobj
      

      ui = do.call(make.action.ui, c(list(obj=aobj),args))
    })
    li = do.call("c",li)
    actions.ui = li
  } else {
    actions.ui = NULL
  }

  text = compile.form.text(form$text, values)
  title = form$title
    
  btnId = paste0("nextStageBtn",player)
  ui = list(
    h2(title),
    HTML(text),
    actions.ui,
    actionButton(btnId, "continue")
  )
  ui
}


make.action.ui = function(obj,id=paste0(get.name(obj),".",player),player=1, inputType="auto", value=NULL, label=get.name(obj),choiceLabels=obj$set, choices=obj$set) {
  restore.point("make.action.ui")
  
  typeName = get.typeName(obj)
  if (is.subtype(typeName,"freeAction"))
    return(make.free.action.ui(obj,id,player,inputType,value=value,label=label))
  
  if (is.null(inputType))
    inputType="auto"
  
  if (inputType=="auto") {
    if (length(obj$set)<=12){
      inputType="radio"      
    } else {
      inputType="selectize"
    }
  }
  if (!is.null(choiceLabels)) {
    choices = as.list(choices)
    names(choices) = choiceLabels
  }
  if (inputType=="radio") {
    ui = radioButtons(inputId = id,label = label,choices = choices, selected=value)
  } else {
    ui = selectizeInput(inputId = id,label = label,choices = choices, selected=value)    
  }
  list(ui)
}


make.free.action.ui = function(obj,id=paste0(get.name(obj),".",player),player=1, inputType="auto", value=NULL, label=get.name(obj)) {
  restore.point("make.free.action.ui")
  
  if (is.null(value))
    value = ""
  
  typeName = get.typeName(obj)
  if (is.subtype(typeName,"freeTextMessage")) {
    #ui = aceEditor(outputId = id,value="Entertext", height="75px",
    #               showLineNumbers=FALSE, highlightActiveLine=FALSE)
    ui = textInput(inputId = id,label = label, value=value)
    res = list(ui,tags$style(type='text/css', paste0("#",id," { width: 300px; }")))
    return(res)
  }
  stop(paste0("Have not yet implemented an ui for type ",typeName))
}

compile.form.text = function(txt, values, markdown=TRUE) {
  if (length(txt)>1) return(sapply(txt,compile.form.text,values=values,markdown=markdown))
  if (is.null(txt)) return(NULL)
  txt = whisker.render(txt,values)
  if (markdown) {
    html = markdownToHTML(text=txt, fragment.only=TRUE)
    return(html)
  }
  return(txt)
}
