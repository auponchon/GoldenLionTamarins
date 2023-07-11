#' Title
#'
#' @param data a dataframe with Groups, Date of Observation and other variables
#' @param freq a dataframe with frequency of observation per group
#' @param title_plot title for the plot
#' @param title_file title for the tiff file
#' @param W width for tiff file
#' @param H height for tiff file
#'
#' @return a tiff file saved in output folder
#' @export 
#'
#' @examples
#' 
#' 
plot_continuity<-function(data,freq,title_plot,title_file,W,H){
  
  check<-data %>% 
    dplyr::filter(!GLT %in% bad & Disp==0 & Death==0 & Solo==0) %>%
    mutate(Group=as.factor(Group)) %>%
    mutate(Group=factor(Group,
                        levels=freq$Group)) %>% 
    dplyr::filter(!is.na(Group)) %>% 
    ggplot(.,aes(x=DateObs,
                 y=Group)) +
    geom_point(aes(colour=Group),
               shape=15,
               size=1.2,
               show.legend=F) +
    scale_colour_manual(values=colorme) +
    scale_y_discrete(guide=guide_axis(n.dodge = 2)) + 
    # scale_x_date(limits=c(as.Date("1989-01-01",format="%F"),
    #                       as.Date("2023-01-01",format="%F")),
    #              breaks=seq(as.Date("1989-01-01",format="%F"),
    #                         as.Date("2022-11-01",format="%F"),
    #                         "6 years")) +
    theme_bw() +
    labs(x="Date",
         y="Groups",
         title=title_plot) +
    theme(legend.position = "none")
  
  # if(nrow(fq) > 50){
  #   
  # }
  # 
  tiff(here::here("outputs","Monitoring effort","Conservation regions",title_file),
       width=W,
       height=H,
       res=600,
       compression="lzw")
  
  
  print(check)
  dev.off()
  
}
