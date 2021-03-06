plot_missing <- function(data, percent=FALSE, x_vert=FALSE) {
  library(tidyverse)
  library(patchwork)
  library(ggplot2)
  
  missing_patterns <- data.frame(is.na(data)) %>%
    group_by_all() %>%
    count(name = "count", sort = TRUE) %>%
    ungroup()
  num_col = ncol(missing_patterns)
  tidypatterns <- missing_patterns[,1:num_col-1] %>%
    rownames_to_column("id") %>%
    gather(key, value, -id)
  
  complete_bool = rowSums(missing_patterns[,1:num_col-1]) == 0
  complete_row = which(complete_bool)
  tidypatterns <- tidypatterns %>%
    mutate( ToHighlight = ifelse(id %in% complete_row, "TRUE", "FALSE" ) )
  tidypatterns <- tidypatterns %>%
    mutate( color = as.factor(as.integer(!value)+as.integer(as.logical(ToHighlight))) )
  
  missing_patterns2<-missing_patterns[1:num_col-1]
  missing_patterns2 <- missing_patterns2 %>%
    mutate( ToHighlight = ifelse(rowSums(.)==0, "TRUE", "FALSE" ) )
  missing_patterns['ToHighlight'] = missing_patterns2['ToHighlight']
  tidypatterns2 <- tidypatterns
  tidypatterns2$id <- sprintf("%02d", as.numeric(as.character(tidypatterns$id)))
  
  a = levels(fct_reorder(tidypatterns$key,-tidypatterns$value,.fun='sum'))
  b = colSums(is.na(data))
  reorder_idx <- match(a,names(b))
  rows = b[reorder_idx]
  key = names(rows)
  df <- data.frame(key,rows)
  rownames(df) <- 1:nrow(df)
  df2<- merge(tidypatterns2,df,by='key')
  
  p1<-df2%>%
    mutate(key = fct_reorder(key,-rows)) %>%
    ggplot(aes(x = key,y = fct_rev(id), fill = color))+
    geom_tile(color = "white") +
    scale_fill_manual(values = c('0'="purple",'1'="light grey",'2'="dark grey"))+
    #scale_alpha_manual(values = c(alpha('grey',0.1),alpha('purple',0.1)))+
    xlab('variable')+
    ylab('missing pattern')+
    guides(fill=FALSE, color=FALSE)+
    theme_bw()+
    annotate("text",x =ncol(missing_patterns)/2 ,y = nrow(missing_patterns)-complete_row+1,label = "complete cases")
  
  
  if(x_vert){
    p1 <- p1 + theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))
  }
  
  
  if (percent == FALSE){
    p2<-ggplot(df,aes(x = reorder(key,-rows),y = rows)) +
      geom_col(fill = "lightblue")+
      ylab('num rows \n missing:')+
      labs(x = "")+
      ggtitle('Missing value patterns')+
      theme_bw()
    
    p3<-ggplot(missing_patterns,aes(x = nrow(missing_patterns):1,y = count,fill = ToHighlight)) +
      geom_col()+
      scale_x_continuous(breaks = 1:nrow(missing_patterns), labels = nrow(missing_patterns):1)+
      scale_fill_manual(values = c('FALSE'="lightblue",'TRUE'="blue"))+
      ylab('row count')+
      coord_flip()+
      theme_bw()+
      theme(axis.title.y=element_blank())+
      guides(fill=FALSE, color=FALSE)
  }
  if (percent == TRUE){
    n = nrow(data)
    df['rows'] <- df['rows']/n*100
    
    p2<-ggplot(df,aes(x = reorder(key,-rows),y = rows)) +
      geom_col(fill = "lightblue")+
      ylab('% rows \n missing:')+
      scale_y_continuous(limits=c(0,100))+
      labs(x = "")+
      ggtitle('Missing value patterns')+
      theme_bw()
    missing_patterns3 = missing_patterns
    missing_patterns3['count']<-missing_patterns3['count']/n*100
    p3<-ggplot(missing_patterns3,aes(x = nrow(missing_patterns3):1,y = count,fill = ToHighlight)) +
      geom_col()+
      scale_x_continuous(breaks = 1:nrow(missing_patterns3), labels = nrow(missing_patterns3):1)+
      scale_fill_manual(values = c('FALSE'="lightblue",'TRUE'="blue"))+
      ylab('% rows')+
      scale_y_continuous(limits=c(0,100))+
      coord_flip()+
      theme_bw()+
      theme(axis.title.y=element_blank())+
      guides(fill=FALSE, color=FALSE)
  }
  
  if(x_vert){
    p2 <- p2 + theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))
  }
  
  
  
  layout <- "
  BBBBBB#
  BBBBBB#
  AAAAAAC
  AAAAAAC
  AAAAAAC
  AAAAAAC
  AAAAAAC
  "
  p1 + p2 + p3 +
    plot_layout(design = layout)
  
  
  
  
  
}
