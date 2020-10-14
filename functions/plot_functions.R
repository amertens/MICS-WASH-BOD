


theme_ki <- function() {
  theme_bw() %+replace%
    theme(
      strip.background = element_blank(),
      legend.position="none",
      plot.title = element_text(size = 16, face = "bold"),
      strip.text = element_text(size=14),
      axis.title = element_text(size=12),
      axis.text.y = element_text(size=10),
      axis.text.x = element_text(size=10, angle = 0, hjust = 0.5, vjust=.1)
    )
}

#hbgdki pallets
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")
tableau11 <- c("Black","#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

theme_set(theme_ki())


#-------------------------------------------------------------------------------------------
# Plot functions
#-------------------------------------------------------------------------------------------
ki_desc_plot <- function(d, Disease, Measure, Birth, Severe, Age_range, 
                         Cohort="pooled",
                         xlabel="Age category",
                         ylabel="",
                         Region=NULL,
                         h1=0,
                         h2=3,
                         strip.text.size=18,
                         yrange=NULL,
                         returnData=F) {
  
  df <- d %>% filter(
    disease == Disease &
      measure == Measure &
      birth == Birth &
      severe == Severe &
      age_range == Age_range &
      !is.na(region) & !is.na(agecat)
  )
  df <- droplevels(df)
  
  if (!is.null(Region)) {
    df <- df %>% filter(region == Region, cohort != "pooled")
  } else {
    df <- df %>% filter(cohort == Cohort)
  }
  
  # remove N= from labels
  df <- df %>% mutate(nmeas.f = gsub('N=', '', nmeas.f)) %>%
    mutate(nstudy.f = gsub('N=', '', nstudy.f))
  
  # remove text from labels
  df <- df %>% mutate(nmeas.f = gsub(' children', '', nmeas.f)) %>%
    mutate(nstudy.f = gsub(' studies', '', nstudy.f))
  
  # Remove 'months' from x axis labels
  df <- df %>% arrange(agecat)
  df$agecat <- as.character(df$agecat)
  df$agecat <- gsub(" months", "", df$agecat)
  df$agecat <- factor(df$agecat, levels=unique(df$agecat))
  
  if (min(df$lb) < 0) {
    print("Warning: some lower bounds < 0")
  }
  
  p <- ggplot(df,aes(y=est,x=agecat)) +
    geom_errorbar(aes(color=region, ymin=lb, ymax=ub), width = 0) +
    geom_point(aes(fill=region, color=region), size = 2) +
    geom_text(aes(x = agecat, y = est, label = round(est)), hjust = 1.5) +
    scale_color_manual(values=tableau11, drop=TRUE, limits = levels(df$measure)) +
    xlab(xlabel)+
    ylab(ylabel) +
    
    # add space to the left and right of points on x axis
    # to accommodate point estimate labels
    scale_x_discrete(expand = expand_scale(add = 1)) +
    
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))  +
    
    theme(
      axis.text.x = element_text(margin =
                                   margin(t = 0, r = 0, b = 0, l = 0),
                                 size = 14)) +
    theme(axis.title.y = element_text(size = 14)) +
    
    ggtitle("")
  
  if(!is.null(Region)) {
    p <- p + facet_wrap(~cohort) +
      theme(strip.text = element_text(size=strip.text.size, margin = margin(t = 0)))
  }else {
    p <- p + facet_grid(~region) +
      theme(strip.text = element_text(size=14, margin = margin(t = 0))) 
  }
  
  if(!is.null(yrange)){
    p <- p + coord_cartesian(ylim=yrange)
  }
  
  
  
  if(returnData){
    return(list(plot=p,data=df))
  }else{
    return(list(plot=p))
  }
}
