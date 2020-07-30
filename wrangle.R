#### Script to wrangle data 


# Always good to load these packages. 
library(tidyverse)
library(magrittr) # Lets you use the pipe: %>% 

# Read in data
full_data <- read_csv("./fulldata.csv")

# Functions to take a peek at the data
full_data %>% dim()
full_data %>% glimpse()
full_data %>% str()
full_data %>% head() %>% View()

# Ok lets make a quick figure

full_data %>% 
  filter(Omic_Type == "Transcriptomics") %>% # Filter Omic_Type column to only get rows with "Transcriptomics"
  top_n(2000) %>% # Just a little function to select the top_n(X) rows. Just to let me get fewer rows
  dplyr::select(one_of("Molecule", "Timepoint")) %>% # Select function lets you select certain columns. You specify the columns in the one_of()
  filter(Timepoint %in% c("Within 12 hrs, Within 24 hrs", "Within 48 hrs")) %>% # See above
  mutate(Timepoint = as.character(Timepoint)) %>% # This might be something funny I did, but I had to mutate the column so its class became a character, rather than a factor. Try running this without this line of code and see what happens.
  ggplot(aes(x = Molecule, fill = Timepoint)) + # Ggplot - the main R graphing package. You specify you X and Y axis here. You also specify other variables you want to include. Like fill, which lets you color by a certain variable.
  geom_bar() + # This is a geom, it specficies how you want to plot the data. So here its is a bar graph. There is also geom_scatter, geom_line, etc, etc
  theme(axis.text.x = element_text(angle = 90)) # Theme lets you specify the minor things like appearance. I ALWAYS have to google what my specifications will look like. So dont worry its alot, but google is your bestfriend.




# This will let you reorder the x axis labels based on the count
full_data %>% 
  filter(Omic_Type == "Transcriptomics") %>% 
  top_n(2000) %>% 
  dplyr::select(one_of("Molecule", "Timepoint")) %>% 
  filter(Timepoint %in% c("Within 12 hrs, Within 24 hrs", "Within 48 hrs")) %>% 
  mutate(Timepoint = as.character(Timepoint)) %>% 
  group_by(Timepoint, Molecule) %>% # Group by is a great function to group by unique values in a column(s) and summarize something
  summarize(count = n()) %>% # Here we are summarizing the total rows. So we essentially collapsed the replicates. It will produce a new column with the name "count" 
  ggplot(aes(x = reorder(Molecule, -count), y = count, fill = Timepoint)) + # Now we can specify we want to reorder Molecule based on the value of count
  geom_bar(stat = "identity") + # Honestly, I do not know what stat = "identity" means. Sorry! Its just something I have memorized that needs to go here when you specify the counts manually.
  theme(axis.text.x = element_text(angle = 90)) 
