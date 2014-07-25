MTS <- function(styles, n, label){
  measure <- integer()
  bp <- seq(0,100,n)
  Tcolor <- character()  
  Tmeasure <- character()
  Tlabel <- character()
  
  for(i in 1:n){
    measure[i] <- sum(styles==i)
    Tcolor <- paste0(Tcolor,
                     '<TD class=\'',paste("gp",i,sep=""), '\'> </TD>',
                     sep="")
    Tlabel <- paste0(Tlabel,
                     '<td>', label[i], '</td>',
                     sep="")
    Tmeasure <- paste0(Tmeasure,
                       '<td>', measure[i], '</td>',
                       sep="")
  }
    
  Tcolor<- paste0('<tr>
                      <th>Text Colour</th>',
                      Tcolor,
                  '</tr>', collapse='')
  Tlabel<-paste0('<tr>
                      <th>Time(%)</th>',
                      Tlabel,
                 '</tr>', collapse='')
  Tmeasure<-paste0('<tr>
                      <th># of Lines</th>',
                      Tmeasure,
                   '</tr>', collapse='')
  
  table<-paste0('\n<a name="Legend"></a><h1><i>Legend</i></h1><div><table border="2" style="background-color:white;">',          
                Tcolor,Tlabel,Tmeasure,                
                '</table>', sep='') 
  
  return(table)
}