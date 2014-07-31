maketable<-function(styles){
  measure<-integer()
  measure[1]<-sum(styles==1)
  measure[2]<-sum(styles==2)
  measure[3]<-sum(styles==3)
  measure[4]<-sum(styles==4)
  number<- character()
  number[1] <- paste0('<tr><TD class="gp1"> </TD><td>', 'Executed lines that were executed','</td><td>', measure[1], '</td></tr>', sep='')
  number[2] <- paste0('<tr><TD class="gp2"> </TD><td>', 'Pre lines that almost certainly were executed','</td><td>', measure[2], '</td></tr>', sep='')
  number[3] <- paste0('<tr><TD class="gp3"> </TD><td>', 'Post lines that probably were executed','</td><td>', measure[3], '</td></tr>', sep='')
  number[4] <- paste0('<tr><TD class="gp4"> </TD><td>', 'Not seen lines that had no evidences of being executed','</td><td>', measure[4], '</td></tr>', sep='')
  number<- paste0(number, collapse='')
  table<-paste0('\n<a name="Legend"></a><h1><i>Legend</i></h1><div><table border="2" style="background-color:white;">          
             <tr><th>Text Colour</th><th>Lines discription</th><th># of Lines</th></tr>',
                number, '</table>', sep='')
  return(table)
}