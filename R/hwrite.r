html.table = function(x,page=NULL, border=2,
  row.names=TRUE,
  style='padding: 0px 10px;margin:2px; header-align:center;text-align:center',
  table.style='padding: 0px 10px;margin:2px; header-align:center;text-align:center', ...) 
{
  
  hwrite(x=x,page=page,border=border,row.names=row.names,style=style, table.style=table.style,...)
}