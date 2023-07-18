# use this script to generate figures for slides!

library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidymodels)
library(ggrepel)


############################ Data wrangling ############################

# the first step to any good plotting R script is getting the data ready to be plotted

data_file_df<- read.csv(file = "test_data.csv") # loads in data, be sure to set working directory to source file location

# this data file has a large series of columns with each row representing an individual plant and 
# columns for all the important test information we want to plot
# specifically we want to plot the standardized height values from the Gas Chromatograph under columns 
# water treatment, treatment 1, and treatment 2
# to do this easily in ggplot it is useful to get all the data under a single column 

all_data_long <- pivot_longer(data_file_df, cols = c('water_treatment', 'treatment_1', 'treatment_2'),
                                   names_to = 'treatment', values_to = 'standardized_gc_hieght') # pivots out ethyelene data so we can plot it all at once 
all_weight <-   pivot_longer(data_file_df, cols = c('mass_water_trifoliate', 'mass_test_trifoliate_1','mass_test_trifoliate_2'),
                                names_to = 'trifoliate', values_to = 'mass') # pivots out mass in case you want to do some investigation about the mass trends on your leaves

all_data_long$standardized_gc_hieght <- as.numeric(all_data_long$standardized_gc_hieght) # make sure all of your treatment values are numeric (likley not necissary)
all_data_long$mass <- all_weight$mass  # adds mass values back to df

all_data_long <- all_data_long %>% 
  select('Family','Generation','M2_rep','M3_rep','Date.Sown','Date.Tested',
         'treatment','standardized_gc_hieght','mass') %>%   # simplifies DF can vary to names you like and information you want to keep
  filter(mass < 2) %>% 
  filter(mass > 0.1) %>% # filter for trifoliates of a good size (will depend on leaf species and what you think is best for your experiment)
  filter(Family %in% c(130)) %>%  # filter for the families you want to look at! (this dataset only includes one family, but sometimes it is useful to compare family information)
  mutate(M2_and_M3 = paste0(M2_rep,M3_rep)) # generates a unique id for each plant

## now lets add some factors for the treatments to go in the same order as the trifoliates they were attached to!

# factors are important for getting the order left to right correct in ggplot2, we can change this order by assigning levels to the factors
all_data_long$treatment <- as.factor(all_data_long$treatment) # changes the treatment values to be factors, this is so we can assign them an order by changing their levels

all_data_long$treatment = ordered(all_data_long$treatment,
                                  levels = c("water_treatment","treatment_1","treatment_2")) # change the order of this list (c()) to change the order of the levels in the order that they appear in this list
levels(all_data_long$treatment) # use to double check the order of levels is as intended 

# and again I felt it useful to assign factor levels for the dates for later plotting (try running the script without this step to see how the order changes, normally the default levels are alphabetical or in order of first number)
all_data_long$Date.Tested <- as.factor(all_data_long$Date.Tested) # changes the treatment values to be factors, this is so we can assign them an order by chaning thier levels

all_data_long$Date.Tested = ordered(all_data_long$Date.Tested,
                                  levels = c("10/27/22","1/23/23","2/21/23")) # change the order of this list (c()) to change the order of the levels
levels(all_data_long$Date.Tested) # use to double check the order of levels is as intended 


#################### Plotting in ggplot2 ##########################

# now we can make some plots! Ethylene data is highly variable day by day, but consistent with comparing lines 
# because of this I recommend doing one last filter prior to plotting for the single day experiment you want to plot 
# as you do experiments day to day 

feb_21_df <- all_data_long %>% 
  filter(Date.Tested == "2/21/23") # filters for specific experiment day


# heads up, I will try to caption each layer of the ggplot when they first appear, 
# but otherwise try experimenting by playing around with the layers yourself to get something you like
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
  labs(title = "130 Family Data",
       x = "Treatment",
       y = "Ethylene nL/(g*2h)",
       caption = "Date Tested: 2/21/23")+ # labs add labels to all axis, and a caption for the date
  theme_light()+ # this adds a general theme to the whole plot
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+ # makes x axis labels at 45 degree angle
  theme(legend.position = 'none') # this removes the fill legend from being on the side (I think it is a bit redundant)

# here is a plot that shows all the data at once, it can be a bit harder to look at so I still reccomend breaking things up 
# until you are ready to pool the data together in the next section

# raw data overview for all dates tested
ggplot(all_data_long,
       mapping = aes(x=treatment, y = standardized_gc_hieght))+
  geom_boxplot(alpha = 0.3, mapping = aes(fill = treatment),
               outlier.shape = NA)+
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
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(legend.position = 'none')

############ Getting z scores normalized to low controls #####################

# for my data, we wanted to normalize the data to 'low control' plants that seemed to be homozygus for a lower ethylene response to our
# treatment peptide 

# this allowed us to compare and pool results across days and experiments since the values of ethylene are highly variable day by day
# but trends tend to hold the same regardless of daily gc height values 

# first lets summarize the means and standard deviations for each experimental day
all_long_normalized_130_to_B <- all_data_long %>%  # my analysis originally had multiple families in the single data frame 
  filter(Family == 130) %>%                        # so I filtered to a single familiy in each analysis for pooling 
  filter(M2_rep == 'B') %>% # low control to normalize to, B was the M2 individual that carried homozygus progeny here
  group_by(Family,treatment,Date.Tested) %>% # ethylene varies day by day so it is important to group for each day tested here
  dplyr::summarise(mean = mean(standardized_gc_hieght, na.rm = T), sd = sd(standardized_gc_hieght, na.rm = T), .groups = 'keep') 

family_130 <- all_data_long %>% # again if you have multiple families, it is important to break them down prior to this analysis
  filter(Family == 130) # for the practice csv file I included this isn't that necessary

family_130<- left_join(family_130, all_long_normalized_130_to_B,
                       by = c("Family", "treatment", "Date.Tested")) # adds back in the mean and sd information

family_130_zscore <- family_130 %>% # this step makes the z-score for each standardized_gc_hieght relative to the low control mean
  group_by(Date.Tested,treatment,M2_rep,M3_rep) %>% 
  dplyr::mutate(standardized_gc_hieght_z_score = (standardized_gc_hieght-mean)/sd) # generates z-score

# this will create z-scores based on the mean of the family B within each treatment type, 
# so the water z-score is relative to the B families water treated trifoliate leaf mean and sd

#################### Plotting z-score values in ggplot2 ##########################

# normalized data overview
ggplot(family_130_zscore,
       mapping = aes(x=treatment, y = standardized_gc_hieght_z_score))+
  geom_boxplot(alpha = 0.3, mapping = aes(fill = treatment),
               outlier.shape = NA)+
  geom_line(aes(group = M2_and_M3),alpha = 0.3)+
  geom_point(aes(color = Date.Tested, group = M2_and_M3),
             position = position_dodge(width = 0.1),
             alpha = 0.3)+
  facet_grid(cols = vars(M2_rep), rows = vars(Date.Tested))+
  scale_fill_manual(values = c("#1155cc","#d9ead3","#e16666"))+
  scale_color_manual(values = c("#00800d","#e46d16","#7f7f7f"))+ # adds colors to the dates
  labs(title = "130 Family Normalized Data",
       x = "Treatment",
       y = "Ethylene Normalized to M2 Family B mean",
       caption = "")+
  theme_light()+
  theme(legend.position = 'none')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  coord_cartesian(y = c(-2,4)) # this zooms in to a manual part of the plot 

# now we can better compare the values across experimental days! every number value (ex, 1 2 3 ect) tells you 
# how mant standard deviations a trifoliate was away from the average of family Bs trifoliate with the same
# experimental treatment 


# normalized no faceting by date
ggplot(family_130_zscore, mapping = aes(x=treatment, y = standardized_gc_hieght_z_score))+
  geom_boxplot(alpha = 0.3, mapping = aes(fill = treatment),
               outlier.shape = NA)+
  geom_line(aes(group = M2_and_M3),alpha = 0.3)+
  geom_point(aes(color = Date.Tested, group = M2_and_M3),
             position = position_dodge(width = 0.1),
             alpha = 0.5)+
  facet_grid(cols = vars(M2_rep))+
  scale_fill_manual(values = c("#1155cc","#d9ead3","#e16666"))+
  scale_color_manual(values = c("#00800d","#e46d16","#7f7f7f"))+ 
  labs(title = "130 Family Normalized Data",
       x = "Treatment",
       y = "Ethylene Normalized to M2 Family B mean",
       caption = "")+
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  coord_cartesian(y = c(-2,4))

# here is what the data looks like if we collapse everything together, It gets a bit messier so you can think
# about tweaking the alpha values on all the lines or points, or doing away with them if they are no longer
# adding anything to the plot due to clutter 

# hope these plotting examples and z-score example helped! feel free to reach out if you have any questions!

