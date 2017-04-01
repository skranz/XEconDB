examples.gambit.efg = function() {
  setwd("D:/libraries/XEconDB/")
  setwd("D:/libraries/XEconDB/projects/UltimatumGame/games")
	
  file = "MaxUltimatum_base.efg"
  sol = gambit.solve.pure(file=file)

	
  library(XEconDB)

  init.ee()
   
  gameId = "UltimatumGame"
  gameId = "ReducedLureOfAuthority"
  
  
  sfg = load.sfg(gameId)
  #sfg = extract.variant.sfg("randomOrPlayer", asfg)
  
  
  efg = make.efg(sfg, variant=1, add.sg.df=FALSE)
  efg = reduce.efg(efg)
  
  Rprof(tmp <- tempfile())
  nfg = efg.to.nfg(efg)
  Rprof()
  summaryRprof(tmp)
  unlink(tmp)

  nfg = nfg.solve.pure(nfg)
  eq.df = nfg$eq.df
  
  copy.into.env(source=efg)

  txt = make.gambit.efg(efg=efg, reduce=TRUE)
  
  sol = gambit.solve.pure(efg = efg)
  out.df = sol$out.df
  out.df
  
  
  util = c("",lossAvUtil(2, r = 0.7))
  util = envyUtil(1:2,alpha=1,n=2)
  efg = set.efg.util(util=util, efg=efg)
  copy.into.env(source=efg)
  
  file = paste0(efg$efg.name,".efg")
  util = NULL
  txt = make.gambit.efg(efg=efg,file = file)
  
  sol = gambit.solve.pure(file=file, efg = efg)
  out.df = sol$out.df
  out.df
  
  #gambit.solve.mixed(file=file, efg=efg)
  odf = gambit.eq.out(sol,efg = efg)
  odf 
}


gambit.solve.pure = function(efg, file=paste0(efg$efg.name,".efg"), solver = "gambit-enumpure -q -P",efg.path=get.ee()$gambit.efg.path, bin.path=get.ee()$gambit.bin.path, eq.only=FALSE, reduce.out = TRUE) {
  
  restore.point("gambit.solve.pure")
  ise.df=efg$ise.df
  
  meq = gambit.solve.mixed(efg, file, solver,efg.path, bin.path, eq.only=TRUE)
  
  # check if it is a pure strategy equilibrium
  #as.data.frame(summarise(group_by(meq,eq, .info.set, player,level), prob=sum(prob)))
  
  if (is.null(meq))
    return(NULL)
  
  peq = mixed.to.pure.eq(meq)
  if (eq.only) return(peq)
  
  out.df = get.eq.out(peq,efg = efg, reduce=reduce.out)
  
  #pay.df = summarise
  sol = list(eq.df = peq, out.df = out.df)
  sol
}

mixed.to.pure.eq = function(meq) {
  summarize(group_by(meq,eq,.info.set,player,level),
    .move.ind = which(prob==1)          
  )
}

#' Finds one or all mixed strategy equilibria
gambit.solve.mixed = function(efg, file=paste0(gameId,".efg"), solver = "gambit-logit -q -e",efg.path=get.ee()$gambit.efg.path, bin.path=get.ee()$gambit.bin.path, gameId=efg$gameId, eq.only=FALSE) {
  
  restore.point("gambit.solve.mixed")
  ise.df = efg$ise.df
  base.mixed.eq = tidyr::unnest(ise.df,"moves.li") %>% 
             rename(move=moves.li) %>%
             select(-num.moves)

  #solver = "gambit-enumpure -q -P -D"
    
  com = paste0(bin.path,"/", solver," ",efg.path,"/",file)
  res  = system(com, intern=TRUE)
  status = attr(res,"status")
  if (isTRUE(status==1)) {
    stop(res)
  }
  res
  txt  = substring(res,4) # remove the initial "NE,"
  
  # no equilibrium found
  if (length(txt)==0)
    return(NULL)
  txt.li   = strsplit(txt,",",fixed=TRUE)
  eq = txt.li[[1]]
  eq.li = lapply(seq_along(txt.li), function(i) {
    restore.point("gambit.mixed.eq.inner")
    eq = as.numeric(txt.li[[i]])
    eq.df = cbind(eq=i,base.mixed.eq, prob = eq)
    eq.df
  })
  eq.df = rbindlist(eq.li)
  as_data_frame(eq.df)
}

make.gambit.efg = function(efg, file=paste0(efg$efg.name,".efg"), path=get.ee()$gambit.efg.path, reduce=TRUE) {
  restore.point("gambit.efg.body.txt")
  header.df=efg$header.df; lev.li=efg$lev.li;
  
  if (reduce & !isTRUE(efg$is.reduced)) {
    efg = reduce.efg(efg)
  }
  
  oco.df=efg$oco.df; ise.df=efg$ise.df; n=efg$n
  
  
  # make txt for all terminal nodes
  u.mat = select(oco.df, starts_with("u_"))
  
  
  #oco.df = cbind(oco.df, u.mat)
  oco.txt = util.df.to.gambit.txt(u.mat) 

  # a text that will be appended to (some) output nodes
  # describing the action and nature nodes before them 
  # in the gambit tree format
  pre.txt = rep("", length(oco.txt))
  
  levs = header.df$level[header.df$type %in% c("action","nature") & efg$active.lev]
  nature.info.set.start = 1+NROW(ise.df)
  
  counter = 0
  while(counter< length(levs)) {
    counter = counter+1
    lev = levs[counter]
    header = header.df[lev,]
    lev.df = lev.li[[lev]]
    if (header$type=="action") {
      ltxt = action.level.to.gambit.txt(header = header,lev.df = lev.df, ise.df=ise.df)
    } else if (header$type=="nature") {
      ltxt = nature.level.to.gambit.txt(header = header,lev.df = lev.df,info.set.start = nature.info.set.start)
      nature.info.set.start = nature.info.set.start + length(ltxt)
    }

  
    # match outcome rows
    if (lev==1) {
      text.row=1
      
    } else {
      keys = unique(header.df$var[1:(lev-1)])
      df = filter(lev.df, .active, .move.ind==1)
      df = s_select(df,".node.ind", keys)
      
      mdf = left_join(df, s_select(oco.df,".outcome",keys),by=keys)
      sdf = summarise(group_by(mdf,.node.ind), text.row = min(.outcome) )  
      text.row = sdf$text.row
    }
    # add current text before the smallest outcome row
    
    pre.txt[text.row] = paste0(pre.txt[text.row],ltxt,"\n")
  }
  
  body.txt = paste0(pre.txt, oco.txt)

  player.names = paste0("pl",1:n)
  header.txt = paste0('EFG 2 R "',efg$gameId,'" { ',paste0('"', player.names, '"', collapse=" "),' }')

  txt = c(header.txt, body.txt)
  writeLines(txt, paste0(path,"/",file))
  if (verbose)
    display("Written to ", path,"/", file )
  invisible(txt)
}

util.df.to.gambit.txt = function(util.df=NULL) {


  #if (is.null(util.df))
  #  util.df = select(oco.df,starts_with("u_"))
  
  payoff.str = paste.matrix.cols(as.matrix(util.df))
# t "" 1 "Outcome 1" { 10.000000 2.000000 }
# t "" 2 "Outcome 2" { 0.000000 10.000000 }
  txt = paste0('t ',

    # a text string, giving the name of the node
    '"" ',
    # a nonnegative integer specifying the outcome
    1:NROW(util.df),
    # (optional) the name of the outcome
    ' "" ',
    # the payoffs to each player for the outcome
    '{ ', payoff.str,' }'
  )
  txt
}


action.level.to.gambit.txt = function(header, lev.df, ise.df) {
  restore.point("action.level.to.gambit.txt")

  df = filter(lev.df, .active)
  com = paste0('paste0("\\"',header$var,'_",',header$var,',"\\"", collapse=" ")')
  df = summarise_(group_by(lev.df,.node.ind,.node,.info.set),moves_str = com) %>% ungroup
  
# p "" 1 1 "(1,1)" { "H" "L" } 0
# p "" 2 1 "(2,1)" { "h" "l" } 0   
  txt = paste0('p "',
#     a text string, giving the name of the node
    df$.node,'" ',
#     a positive integer specifying the player who owns the node
    header$player,' ',
#     a positive integer specifying the information set
    match(df$.info.set,ise.df$.info.set),' ',
#     (optional) the name of the information set
    '"',df$.info.set,'" ',
#     (optional) a list of action names for the information set
    '{ ', df$moves_str," }",
#     a nonnegative integer specifying the outcome
    " 0"
  )
  txt
}


nature.level.to.gambit.txt = function(header, lev.df,info.set.start = 1) {
  restore.point("nature.level.to.gambit.txt")


  info.sets = unique(lev.df$.info.set)

  df = filter(lev.df, .active)
# c "" 2 "(0,2)" { "2g" 0.500000 "2b" 0.500000 } 0
  
  com = paste0('paste0("\\"',header$var,'_",',header$var,',"\\" ",.prob, collapse=" ")')
  df = summarise_(group_by(lev.df,.node.ind,.node),moves_str = com) %>% ungroup
  txt = paste0('c "',
#     a text string, giving the name of the node
    df$.node,'" ',
#     a positive integer specifying the information set number
    1:NROW(df)+info.set.start-1,' ',
#     (optional) the name of the information set
    '"',df$.node,'" ',
#     (optional) a list of actions at the information set with their corresponding probabilities
    '{ ', df$moves_str," }",
#     a nonnegative integer specifying the outcome
    " 0"
  )
  txt
}

