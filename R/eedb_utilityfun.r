# Some utility function that transform payoffs
examples.utility.fun = function() {
	ineqAvUtil()
}


payoffUtil = function(player=1,n=NULL) {
	paste0("payoff_",player)
}


# envyUtil:
#   descr: Fehr-Schmidt inequality aversion without guilt but envy only. 
#   alpha:
#     descr: the degree of envy
#     default: 0.5

ineqAvUtil = function(player = 1,alpha=0.75,beta=0.5,n=2) {
  restore.point("ineqAvUtil")
  np = length(player)
  util =  vector("character",np)
  counter = 1
  for (counter in 1:np) {
    i = player[counter]
    j = (1:n)[-i]
    util[counter] = paste0("payoff_",i,
      # envy
      " - (",alpha,"/",n-1,")*(",
        paste0("pmax(payoff_",j,"-payoff_",i,",0)",collapse="+"),
      ")",
      # guilt
      " - (",beta,"/",n-1,")*(",
        paste0("pmax(payoff_",i,"-payoff_",j,",0)",collapse="+"),
      ")"
    )
  }
  lab = paste0("ineq",alpha*100,"_",beta*100)
  names(util)=rep(lab,np)
  util
}

# envyUtil:
#   descr: Fehr-Schmidt inequality aversion without guilt but envy only. 
#   alpha:
#     descr: the degree of envy
#     default: 0.5

envyUtil = function(player = 1,alpha=0.5,n=2) {
  np = length(player)
  util =  vector("character",np)
  for (counter in 1:np) {
    i = player[counter]
    j = (1:n)[-i]
    util[counter] = paste0("payoff_",i," - (",alpha,"/",n-1,")*(",
              paste0("pmax(payoff_",j,"-payoff_",i,",0)",collapse="+"),")")

  }
  lab = paste0("envy",alpha*100)
  names(util)=rep(lab,np)
  util
}
example.get.envy.util = function() {
  envyUtil(alpha=0.5,n=3)
  
  util =uniform.loss.aversion.util(start=1,end=3)
  expr = parse(text=util[1]) 
  dp = 0.1
  payoff_1 = seq(0,5,by=dp)
  u = eval(expr)
  plot(payoff_1,u)
  plot(payoff_1[-1],diff(u)/dp)
}

# Loss aversion utility with a continuous multiple reference point that 
# is uniformely distributed between start and end. Normalized such that material payoffs of 0 remain 0
unifLossAvUtil = function(player=1,rmin=0,rmax=1,lambda=2,n=NULL) {
	restore.point("unifLossAvUtil")
  start = rmin; end = rmax
  util = sapply(player,function(i) {
    x = paste0("payoff_",i)
    
    if (is.numeric(start) & is.numeric(end)) {
    	u.start = ((lambda-1)*start^2)/(2*(end-start))+lambda*start
    	u.end = lambda*end-((lambda-1)*(end^2/2-end*start))/(end-start)
    	code.start = ""
    } else {
    	code.start = paste0(
    		".start = ", start,";\n .end = ",end,";\n" 
    	)
  		start = ".start"
  		end = ".end"
  
    	u.start = paste0("((",lambda,"-1)*",start,"^2)/(2*(",end,"-",start,"))+",lambda,"*",start)
    	u.end = paste0(lambda,"*",end,"-((",lambda,"-1)*(",end,"^2/2-",end,"*",start,"))/(",end,"-",start,")")
    }
   
    
    code = paste0("{",code.start,"\n
  ifelse(",x,">",end,",",u.end,"+(",x,"-",end,"),
    ifelse(",x,"<",start,",",u.start,"-",lambda,"*(",start,"-",x,"),
       ",lambda,"*",x,"-((",lambda,"-1)*(",x,"^2/2-",start,"*",x,"))/(",end,"-",start,")
    )
  )}")
  })
  
  if (is.character(start)) {
   	lab = paste0("unifLossAv_lambda",lambda)
  } else {
  	lab = paste0("unifLossAv_start",start,"_end",end,"_lambda",lambda)
  }
  names(util)=rep(lab,length(player))
  util = replace.payoff_i.in.util(util, players=player)
  util
}

# Loss aversion utility with a continuous multiple reference point that 
# is uniformely distributed between start and end. Normalized such that material payoffs of 0 remain 0
lossAvUtil = function(player=1,r,lambda=2,n=NULL) {
	restore.point("lossAvUtil")
  r.str = paste0("c(",paste0(r, collapse=","),")")
  x = paste0("payoff_",player)
  util = paste0("loss.aversion.util(",x,",r=",r.str,",lambda=",lambda,")")
  
  lab = paste0("lossAv_r",r,"_lambda",lambda)
  names(util)=rep(lab,length(player))
  util = replace.payoff_i.in.util(util, players=player)
  util
}


loss.aversion.util = function(payoffs,r,lambda=2) {
  restore.point("loss.aversion.util.fun") 
  x = payoffs
  if (length(r)==1) { # single reference point
    u = pmax(x-r,0) - lambda*pmax(r-x,0)     
    return(u)
  } else { # multiple reference points
    xr = expand.grid(x,r)
    uv = pmax(xg-rg,0) - lambda*pmax(rg-xg,0)  
    um = matrix(uv,NROW(x),NROW(r))
    u = apply(um,1,mean)
    return(u)  
  } 

}


loss.aversion.util.fun = function(x,r,lambda=2,n=2) {
  restore.point("loss.aversion.util.fun") 
  
  xr = expand.grid(x,r)
  xg = xr[,1]
  rg = xr[,2]
  
  #matrix(rg,NROW(x),NROW(r))
  uv = pmax(xg-rg,0) - lambda*pmax(rg-xg,0)  
  um = matrix(uv,NROW(x),NROW(r))
  u = apply(um,1,mean)
  return(u)  
  
}

replace.payoff_i.in.util = function(utils.str, players=seq_len(utils.str)) {
	for (i in seq_along(utils.str)) {
		player = players[i]
		utils.str = gsub("payoff_i",paste0("payoff_",player), utils.str,fixed=TRUE)
	}
	utils.str
}

examples.loss.aversion.util.fun = function() {
  x = 0:100
  r = 20:80
  #r = c(50,51)
  u = loss.aversion.util.fun(x=x,r=r,lambda=2)
  plot(u)
  plot(diff(u))
  abline(a=0,b=1,col="red")
}

WSumMin = function(player=1,min_weight=1,n=2) {
	sum = paste0("payoff_",seq_len(n), collapse="+")
	min = paste0("pmin(",paste0("payoff_",seq_len(n),collapse=", "),")")
	u = paste0(sum," + ", min_weight, " * ", min)
	names(u) = paste0("W_SumMin_",min_weight)
	u
		
}