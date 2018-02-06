
# Tod wRVU Analysis -------------------------------------------------------
  # Using wRVU data from Vizient: Provided by Ron Falk on 2018-02-03

# 2. Load Packages --------------------------------------------------------

  require(tidyverse)
  require(extrafont)
  loadfonts(quiet = TRUE)
  require(here)  
  require(readxl)
  require(skimr)
  require(ggplot2)
  require(plotly)
  

# 3. Create Data Directory ------------------------------------------------

 
# 4. Load Data ------------------------------------------------------------

  rvu <- read_excel('data/Copy of CompAndWRVUData(1179).xlsx', sheet = 1)
  
  skim(rvu)
  

# Create Some Figures -----------------------------------------------------

  
# Compensation by Specialty 
  
  rvu1 <- rvu %>% select(Specialty, comp) %>% arrange(desc(comp)) %>% 
    ggplot(aes(x = reorder(Specialty, comp), y = comp))+
      geom_bar(stat = "identity", fill = c(rep("#cccccc", 4), "#3366cc", 
                                           rep("#cccccc", 9)))+
      theme_bw(base_size = 14, base_family = "Roboto")+
      coord_flip()+
      scale_y_continuous(breaks = seq(100000, 300000, 100000), 
                         labels = c("100", "200", "300"))+
      ylab("Compensation in $1000s") +
      xlab("")+
      ggtitle("Compensation by Specialty (Source: Vizient)")+
      theme(plot.title = element_text(hjust = 0.5))

  ggsave(filename = "2018-02-05_RVU-Comp.png" , plot = rvu1,
         width = 12, height = 8, units = "in", device = "png")
  
    
# wRVU by Specialty
  
   rvu2 <- rvu %>% select(Specialty, comp, wRVU) %>% 
    ggplot(aes(x=wRVU, y = comp))+
      geom_point(aes(color = Specialty))+
      scale_color_discrete()+
      theme_bw(base_size = 14, base_family = "Roboto")+
      geom_smooth(method = "lm")+
      xlab("wRVU")+
      ylab("Compensation ($1000s)")+
      ggtitle("Nephrology is Clear Outlier")+
      theme(axis.title = element_text(hjust=0.5))+
      scale_y_continuous(breaks = seq(150000, 350000, 50000),
                         labels = as.character(seq(150, 350, 50)))

  ggplotly(rvu2)      
      
     
  