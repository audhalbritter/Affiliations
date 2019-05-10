## ----runAffiliations

# Load libraries
library("tidyverse")
library("googlesheets")
library("readxl")
library("xlsx")

#gs_auth()
gs_ls() 
# which google sheets do you have access to?
CM <- gs_title(x = "ClimMani protocols overview", verbose = TRUE)
#download data
affiliation <- gs_read(ss = CM, ws = "ClimMani people", range = cell_rows(1:115)) %>% as.tibble()

### testing encoding stuff...
stringi::stri_enc_detect("Mar<U+00ED>a")
iconv("Bj\366rn", from = "UTF-16BE", to = "UTF-8")
bad <- c(9)
stringi::stri_enc_detect(affiliation$First)[[48]]
iconv(affiliation$First[bad], from = "ISO-8859-1", to = "UTF-8")
###


# Prepare data
affiliation <- affiliation %>% 
  select(-full, -Remark, -FormattingTeam, -Affiliation1_full, -Affiliation2_full, -Affiliation3_full) %>% 
  rename(First = FirstName, Last = LastName, Main = MainAuthors, ChapterL = ChapterLeader) %>%
  # gather all affiliations in one column
  gather(key = Number, value = Affiliation, Affiliation1,  Affiliation2, Affiliation3) %>% 
  filter(!is.na(Affiliation)) %>% 
  mutate(Country = ifelse(First == "Sean" & Affiliation == "Department of Botany and Biodiversity Research Centre, University of British Columbia, Vancouver, Canada", "Canada", Country)) %>% 
  mutate(Country = ifelse(First == "Sean" & Affiliation == "Earth and Environmental Sciences Division, Los Alamos National Laboratory, Los Alamos, USA", "USA", Country)) %>% 
  mutate(Country = ifelse(First == "Sean" & Affiliation == "Biosphere 2 and Department of Ecology & Evolutionary Biology, University of Arizona, Tucson, USA", "USA", Country)) %>% 
  mutate(Country = ifelse(First == "Benjamin" & Affiliation == "Environmental Change Institute, School of Geography and the Environment, University of Oxford, Oxford, UK", "UK", Country)) 
  

# Function to get affiliations ranked from 1 to n
rankNR <- function(x){
  su=sort(unique(x))
  for (i in 1:length(su)) x[x==su[i]] = i
  return(x)
}



NameAffList <- affiliation %>% 
  filter(is.na(Main)) %>% # remove main authors
  arrange(Last, Affiliation) %>% 
  rowwise() %>% 
  # extract the first letter of each first name and put a dot after each letter
  #mutate(
    #Initials = paste(stringi::stri_extract_all(regex = "\\b([[:alpha:]])", str = First, simplify = TRUE), collapse = ". "),
    #Initials = paste0(Initials, ".")) %>%
  ungroup() %>% 
  # add a column from 1 to n
  mutate(ID = 1:n()) %>%
  group_by(Affiliation) %>% 
  # replace ID with min number (same affiliations become the same number)
  mutate(ID = min(ID)) %>% 
  ungroup() %>% 
  # use function above to assign new ID from 1 to n
  mutate(ID = rankNR(ID)) %>%
  #Paste Last and Initials
  #mutate(name = paste0(Last, ", ", Initials)) 
  # Past First and lat name
  mutate(name = paste(First, Last, sep = " "))

# Create a list with all Affiliations
NameAffList %>% 
  distinct(ID, Affiliation) %>% 
  arrange(ID) %>% 
  mutate(ID = paste0("^", ID, "^")) %>% 
  mutate(Affiliation2 = paste(ID, Affiliation, sep = "")) %>% 
  mutate(Affiliation2 = enc2utf8(as.character(Affiliation2))) %>% 
  pull(Affiliation2) %>% 
  paste(collapse = ", ")

 
# Create a list with all names
NameAffList %>%   
  group_by(Last, name) %>% 
  summarise(affs = paste(ID, collapse = ",")) %>% 
  mutate(
    affs = paste0("^", affs, "^"),
    nameID = paste0(name, affs)     
         ) %>% 
  pull(nameID) %>% 
  paste(collapse = ", ")

