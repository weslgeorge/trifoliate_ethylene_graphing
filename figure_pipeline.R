# use this script to generate figures for slides!

library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidymodels)
library(ggrepel)


############################ Data wrangling ############################

data_file_df<- read.csv(file = "test_data.csv") # loads in data, be sure to set working directory to source file location

# data_file_df$water_treatment<-as.numeric(data_file_df$water_treatment) # make sure all of your treatment values are numeric (likley not necissary)
# data_file_df$treatment_1<-as.numeric(data_file_df$treatment_1)
# data_file_df$treatment_2<-as.numeric(data_file_df$treatment_2)


all_data_long <- pivot_longer(data_file_df, cols = c('water_treatment', 'treatment_1', 'treatment_2'),
                                   names_to = 'treatment', values_to = 'standardized_gc_hieght') # pivots out ethyelene data so we can plot it all at once 
all_weight <-   pivot_longer(data_file_df, cols = c('mass_water_trifoliate', 'mass_test_trifoliate_1','mass_test_trifoliate_2'),
                                names_to = 'trifoliate', values_to = 'mass') # pivots out mass incase you want to do some investigation about the mass trends on your leaves

all_data_long$standardized_gc_hieght <- as.numeric(all_data_long$standardized_gc_hieght) # make sure all of your treatment values are numeric (likley not necissary)
all_data_long$mass <- all_weight$mass  # adds mass values back to df

all_data_long <- all_data_long %>% 
  select('Family','Generation','M2_rep','M3_rep','Date.Sown','Date.Tested',
         'treatment','standardized_gc_hieght','mass') %>%   # simplifies DF can vary to names you like and informtion you want to keep
  filter(mass < 2) %>% 
  filter(mass > 0.1) %>% # filter for trifoliates of a good size (will depend on leaf species and what you think is best)
  filter(Family %in% c(130)) %>%  # filter for the families you want to look at!
  mutate(M2_and_M3 = paste0(M2_rep,M3_rep)) # generates a unique id for each plant

## now lets add some factors for the treatments to go in the same order as the trifoliates they were attached to!

all_data_long$treatment <- as.factor(all_data_long$treatment) # changes the treatment values to be factors, this is so we can assign them an order by chaning thier levels

all_data_long$treatment = ordered(all_data_long$treatment,
                                  levels = c("water_treatment","treatment_1","treatment_2")) # change the order of this list (c()) to change the order of the levels
levels(all_data_long$treatment) # use to double check the order of levels is as intended 

#################### Plotting in ggplot2 ##########################

# now we can make some plots! Ethylene data is highly variable day by day, but consistent with comparing lines 
# because of this I recommend doing one last filter prior to plotting for the single day you want to plot 

feb_21_df <- all_data_long %>% 
  filter(Date.Tested == "2/21/23")

# data overview plot for one experiment
ggplot(feb_21_df, mapping = aes(x=treatment, y = standardized_gc_hieght))+
  geom_boxplot(alpha = 0.3, 
               mapping = aes(fill = treatment),
               outlier.shape = NA)+ # this adds boxplot information and colors it based on the treatment type
  scale_fill_manual(values = c("#1155cc","#d9ead3","#e16666"))+ # this adds colors manually to the fill, make sure the order of the colors is the same as the order of the factor levels used
  geom_line(aes(group = M2_and_M3),alpha = 0.3)+ # this adds lines to graph, they are grouped to M2_and_M3 so that each line represents one leaf with three connected trifoliates
  geom_point(aes(group = M2_and_M3),
             position = position_dodge(width = 0.1),
             alpha = 0.6)+ # this adds points to graph, they are dodged so that we can see overlapping points, and are grouped by the unique plant ID M2_M3rep 
  facet_grid(cols = vars(M2_rep))+ # this separates data by parentage 
  labs(title = "130 Family Nromalized data",
       x = "Treatment",
       y = "Ethylene nL/(g*2h)",
       caption = "")+ # labs add labels to all axis 
  theme_light()+ # this adds a general theme to the whole plot
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ # makes x axis labels at 45 degree angle
  theme(legend.position = 'none') # this removes the fill legend from being on the side (I think it is a bit redundant)

# here is a plot that shows all the data at once, it can be a bit harder to look at so I still reccomend breaking things up 
# until you are ready to pool the data together in the next section
# raw data overview 

ggplot(all_data_long[all_data_long$Date.Tested != "12/16/22",],
       mapping = aes(x=treatment, y = standardized_gc_hieght))+
  geom_boxplot(alpha = 0.3, mapping = (aes(fill = treatment)))+
  geom_line(aes(group = M2_and_M3),alpha = 0.3)+
  geom_point(aes(color = Date.Tested, group = M2_and_M3),
             position = position_dodge(width = 0.1))+
  facet_grid(cols = vars(M2_rep), rows = vars(Date.Tested))+
  scale_fill_manual(values = c("#1155cc","#d9ead3","#e16666"))+
  labs(title = "130 Family Data Overview",
       x = "Treatment",
       y = "Ethylene nL/(g*2h)",
       caption = "")+
  theme_light()+
  theme(legend.position = 'none')

############ Getting z scores normalized to low controls #####################

# for my data, we wanted to normalize the data to 'low control' plants that seemed to be homozygus for a lower ethylene response to our
# treatment peptide 

# this allowed us to compare and pool results across days and experiments since the values of ethylene are highly variable day by day
# but trends tend to hold the same regardless of daily gc height values 

# first lets summarize the means and standard deviations for each experimnetal day
all_long_normalized_130_to_B <- all_data_long %>%  # my analysis originally had multiple families in the single data frame 
  filter(Family == 130) %>%                        # so I filtered to a single familiy in each analysis for pooling 
  filter(M2_rep == 'B') %>% # low control to normalize to, B was the M2 individual that carried homozygus progeny here
  group_by(Family,treatment,Date.Tested) %>% # ethylene varies day by day so it is important to group for each day tested here
  dplyr::summarise(mean = mean(standardized_gc_hieght, na.rm = T), sd = sd(standardized_gc_hieght, na.rm = T), .groups = 'keep') 

family_130 <- all_data_long %>% # again if you have multiple families, it is important to break them down prior to this analysis
  filter(Family == 130)

family_130<- left_join(family_130, all_long_normalized_130_to_B,
                       by = c("Family", "treatment", "Date.Tested")) # adds back in the mean and sd information

family_130_zscore <- family_130 %>% # this step actually makes the z-score for each standardized_gc_hieght relative to the low control mean
  group_by(Date.Tested,treatment,M2_rep,M3_rep) %>% 
  dplyr::mutate(standardized_gc_hieght_z_score = (standardized_gc_hieght-mean)/sd) %>% # generates z-score
  mutate(M2_and_M3 = paste0(M2_rep,M3_rep)) # generates a unique id for each plant

#################### Plotting in ggplot2 ##########################

# normalized data overview
ggplot(family_130_zscore, mapping = aes(x=treatment, y = standardized_gc_hieght_z_score))+
  geom_boxplot(alpha = 0.3, mapping = (aes(fill = treatment)))+
  geom_line(aes(group = M2_and_M3),alpha = 0.3)+
  geom_point(aes(color = Date.Tested, group = M2_and_M3),
             position = position_dodge(width = 0.1))+
  facet_grid(cols = vars(M2_rep), rows = vars(Date.Tested))+
  scale_fill_manual(values = c("#1155cc","#d9ead3","#e16666"))+
  labs(title = "130 Family Nromalized data",
       x = "Treatment",
       y = "Ethylene Normalized to M2 Family B mean",
       caption = "")+
  theme_light()+
  theme(legend.position = 'none')+
  coord_cartesian(y = c(-2,4)) # this zooms in to a manual part of the plot 

# normalized no faceting by date
ggplot(family_130_zscore, mapping = aes(x=treatment, y = ethylene_z_score))+
  geom_boxplot(alpha = 0.3, mapping = (aes(fill = treatment)))+
  geom_line(aes(group = M2_and_M3_and_M4),alpha = 0.3)+
  geom_point(aes(color = Date.Tested, group = M2_and_M3_and_M4),
             position = position_dodge(width = 0.1))+
  facet_grid(cols = vars(M2_rep))+
  scale_fill_manual(values = c("#1155cc","#d9ead3","#e16666"))+
  labs(title = "130 Family Nromalized data",
       x = "Treatment",
       y = "Ethylene Normalized to M2 Family B mean",
       caption = "")+
  theme_light()+
  coord_cartesian(y = c(-2,4))

#### Normalized pooling 

family_130_zscore_lowpool_summary <- family_130_zscore %>% 
  filter(M2_rep == "C") %>% # segregating line
  filter(Date.Tested != "8/12/22") %>%  # did not collect tissue for this line
 # filter(Date.Tested == "2/21/23") %>% 
  filter(treatment == "In") %>% 
  filter(Generation == "M3") %>% 
  filter(ethylene_z_score < 0.5)  # cutoff for being similar to B
family_130_zscore_lowpool_summary$pool<- "L" # adds pool info

family_130_zscore_highpool_summary <- family_130_zscore %>% 
  filter(M2_rep == "C") %>% # segregating line
  filter(Date.Tested != "8/12/22") %>%  # did not collect tissue for this date
#  filter(Date.Tested == "2/21/23") %>% 
  filter(treatment == "In") %>% 
  filter(Generation == "M3") %>% # I do not trust the M4 generation yet
  filter(ethylene_z_score > 1)  # cutoff for being dissimilar to B
family_130_zscore_highpool_summary$pool<- "H" # adds pool info

hist(family_130_zscore$ethylene_z_score[family_130_zscore$treatment == "In"])
length(family_130_zscore_highpool_summary)
length(family_130_zscore_lowpool_summary)



family_130_zscore_highpool_sum <- family_130_zscore_highpool_summary %>% 
  select(Family, Generation, M2_rep, M3_rep, Date.Tested, mass, ethylene, ethylene_z_score,pool)

family_130_zscore_lowpool_sum <- family_130_zscore_lowpool_summary %>% 
  select(Family, Generation, M2_rep, M3_rep, Date.Tested, mass, ethylene, ethylene_z_score,pool)

write.csv(family_130_zscore_highpool_sum, file = 
            "high_130C_pool_zs_mar_2023.csv")
write.csv(family_130_zscore_lowpool_sum, file = 
            "low_130C_pool_zs_mar_2023.csv")

pool_130_df <- rbind(family_130_zscore_highpool_summary,family_130_zscore_lowpool_summary)

colnames(family_130_zscore)

family_130_zscore_with_pool_info<- left_join(family_130_zscore, pool_130_df, by = colnames(family_130_zscore))
#graphing pool information


write.csv(family_130_zscore_with_pool_info, file = 
            "130C_pool_all_zs_mar_2023.csv")

ggplot(family_130_zscore_with_pool_info, mapping = aes(x=treatment, y = ethylene_z_score))+
  geom_boxplot(alpha = 0.3, mapping = (aes(fill = treatment)))+
  geom_line(aes(group = M2_and_M3_and_M4),alpha = 0.3)+
  geom_point(aes(color = pool, group = M2_and_M3_and_M4),
             position = position_dodge(width = 0.1))+
  facet_grid(cols = vars(M2_rep))+
  scale_fill_manual(values = c("#1155cc","#d9ead3","#e16666"))+
  labs(title = "130 Family Nromalized data",
       x = "Treatment",
       y = "Ethylene Normalized to M2 Family B mean",
       caption = "")+
  theme_light()+
  coord_cartesian(y = c(-2,4))

# raw data overview with pool
ggplot(family_130_zscore_with_pool_info, mapping = aes(x=treatment, y = ethylene))+
  geom_boxplot(alpha = 0.3, mapping = (aes(fill = treatment)))+
  geom_line(aes(group = M2_and_M3_and_M4),alpha = 0.3)+
  geom_point(aes(color = pool, group = M2_and_M3_and_M4),
             position = position_dodge(width = 0.1))+
  facet_grid(cols = vars(M2_rep), rows = vars(Date.Tested))+
  scale_fill_manual(values = c("#1155cc","#d9ead3","#e16666"))+
  labs(title = "130 Family Data Overview",
       x = "Treatment",
       y = "Ethylene nL/(g*2h)",
       caption = "")+
  theme_light()



#### ASPB POSTER PLOTS ####

##### 130 #####
family_130_zscore_with_pool_info_simplified <- family_130_zscore_with_pool_info %>% 
  filter(M2_rep %in% c("B","C")) %>% 
  filter(Date.Tested !="12/1/22") %>% 
  mutate(M2_poster_lab = paste0("M2-",M2_rep), Bulk = pool)


high_list_130 <- family_130_zscore_with_pool_info_simplified$M2_and_M3_and_M4[family_130_zscore_with_pool_info_simplified$pool == "H"]
low_list_130 <- family_130_zscore_with_pool_info_simplified$M2_and_M3_and_M4[family_130_zscore_with_pool_info_simplified$pool == "L"]

family_130_zscore_with_pool_info_simplified <- family_130_zscore_with_pool_info_simplified %>% 
  mutate(Bulk = case_when(M2_and_M3_and_M4 %in% high_list_130 ~ "High",
                          M2_and_M3_and_M4 %in% low_list_130 ~ "Low"))


# Lets add some factors
family_130_zscore_with_pool_info_simplified$Date.Tested <- as.factor(family_130_zscore_with_pool_info_simplified$Date.Tested)

# now order the factors so col is first
family_130_zscore_with_pool_info_simplified$Date.Tested = ordered(x=family_130_zscore_with_pool_info_simplified$Date.Tested, 
                                                             c('10/27/22','1/23/23','2/21/23'))

# use to check levels of your factors
levels(family_130_zscore_with_pool_info_simplified$Date.Tested)

poster_130_norm_pool_plot <- ggplot(data =family_130_zscore_with_pool_info_simplified , 
                                    mapping = aes(x=factor(treatment, levels = c("H2O","In","SubPep12"),
                                                           labels = c("H2O","In11","VuSubpep")), y = ethylene_z_score))+
  geom_boxplot(alpha = 0.3, mapping = (aes(fill = treatment)))+
  geom_line(aes(color = Bulk,group = M2_and_M3_and_M4),alpha = 0.3)+
  geom_point(aes(color = Bulk, group = M2_and_M3_and_M4),
             position = position_dodge(width = 0.1))+
  facet_grid(cols = vars(M2_poster_lab))+
  scale_fill_manual(values = c("#1155cc","#d9ead3","#e16666"))+
  labs(title = "M1 130 Family M3 Screen Normalized",
       x = "Treatment after wounding",
       y = "Ethylene Response Normalized to B mean",
       caption = "")+
  coord_cartesian(y = c(-2,4))+
  theme_linedraw()+
  scale_color_manual(values = c("#00800d","#e46d16","#7f7f7f"),labels = c("High","Low","NA"))+
  theme(title = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 18, face = "plain"),
        axis.title.y = element_text(size = 18, face = "plain"),
        strip.text.x = element_text(size = 18, color = 'black'),
        strip.text.y = element_text(size = 18, color = 'black'),
        strip.background =element_rect(fill="#90C8AC"))+ # changes facet font)
  guides(x = guide_axis(angle = 45), fill = FALSE)
#scale_color_manual(values = c("#afe6b8","#eefae6","#facb9c","#9fc5e8"))


ggsave(plot = poster_130_norm_pool_plot,width = 9, height = 7, dpi = 600, filename = "poster_130_norm_pool_plot.png")

poster_130_raw_pool_plot <- ggplot(data =family_130_zscore_with_pool_info_simplified , 
                                    mapping = aes(x=treatment, y = ethylene))+
  geom_boxplot(alpha = 0.3, mapping = (aes(fill = treatment)))+
  geom_line(aes(color = pool,group = M2_and_M3_and_M4),alpha = 0.3)+
  geom_point(aes(color = pool, group = M2_and_M3_and_M4),
             position = position_dodge(width = 0.1))+
  facet_grid(cols = vars(M2_rep),rows = vars(factor(Date.Tested, levels = c('10/27/22','1/23/23','2/21/23'))))+
  scale_fill_manual(values = c("#1155cc","#d9ead3","#e16666"))+
  labs(title = "130 Family",
       x = "",
       y = "Ethylene nL/(g*2h)",
       caption = "")+
  theme_classic()+
  theme_linedraw()+
  theme(legend.position = 'none',
        title = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 18, face = "plain"),
        axis.title.y = element_text(size = 18, face = "plain"),
        strip.text.x = element_text(size = 18, color = 'black'),
        strip.text.y = element_text(size = 18, color = 'black'),
        strip.background =element_rect(fill="#90C8AC")) # changes facet font)
#scale_color_manual(values = c("#afe6b8","#eefae6","#facb9c","#9fc5e8"))
?factor()

theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14))
ggplot(data = family_491_zscore_with_pool_info_simplified, mapping = aes(x = ))

ggsave(plot = poster_130_raw_pool_plot,width = 9, height = 8, dpi = 600, filename = "poster_130_raw_pool_plot.png")


View(family_130_zscore_with_pool_info_simplified)
a <- family_130_zscore_with_pool_info_simplified[family_130_zscore_with_pool_info_simplified$treatment == "In",]
b <- a[a$M2_rep == "C",]
ggplot(data =a , 
                                   mapping = aes(x=M2_rep, y = ethylene))+
  geom_boxplot(alpha = 0.3, mapping = (aes(fill = treatment)))+
  geom_line(aes(color = pool,group = M2_and_M3_and_M4),alpha = 0.3)+
  geom_point(aes(color = pool, group = M2_and_M3_and_M4),
             position = position_dodge(width = 0.1))+
  facet_grid(rows = vars(factor(Date.Tested, levels = c('10/27/22','1/23/23','2/21/23'))))+
  scale_fill_manual(values = c("#1155cc","#d9ead3","#e16666"))+
  labs(title = "130 Family",
       x = "",
       y = "Ethylene nL/(g*2h)",
       caption = "")+
  theme_classic()+
  theme_linedraw()+
  theme(legend.position = 'none',
        title = element_text(size = 20, face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 18, face = "plain"),
        axis.title.y = element_text(size = 18, face = "plain"),
        strip.text.x = element_text(size = 18, color = 'black'),
        strip.text.y = element_text(size = 18, color = 'black'),
        strip.background =element_rect(fill="#90C8AC")) # changes facet font)
dcas9_lateral_count_aov <- aov(ethylene_z_score ~ M2_rep, data= a)
summary(dcas9_lateral_count_aov)

