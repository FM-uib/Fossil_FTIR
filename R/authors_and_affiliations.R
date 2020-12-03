dat <- data.frame(FirstName = c("Florian", 
                                "Valeria", 
                                "Boris", 
                                "Alistair W R"),
                  LastName = c("Muthreich", 
                               "Tafintseva", 
                               "Zimmermann", 
                               "Seddon"),
                  Affiliation1 = c("University of Bergen, Department of Biological Sciences, Norway", 
                                   "University of Life Sciences, Ås, Norway",
                                   "University of Life Sciences, Ås, Norway",
                                   "University of Bergen, Department of Biological Sciences, Norway"),
                  Affiliation2 = c(NA, 
                                   NA, 
                                   NA, 
                                   "Bjerknes Centre for Climate Research, Bergen"))
dat <- dat %>% 
  # gather all affiliations in one column
  gather(key = Number, value = Affiliation, Affiliation1, Affiliation2) %>%
  # remove rows with no Affiliations
  dplyr::filter(!is.na(Affiliation))

# Function to get affiliations ranked from 1 to n (this function was found on Stack Overflow)
rankID <- function(x){
  su=sort(unique(x))
  for (i in 1:length(su)) x[x==su[i]] = i
  return(x)
}


NameAffList <- dat %>% 
  dplyr::arrange(LastName, Affiliation) %>% 
  rowwise() %>% 
  # extract the first letter of each first name and put a dot after each letter
  mutate(
    Initials = paste(stringi::stri_extract_all(regex = "\\b([[:alpha:]])", str = FirstName, simplify = TRUE), collapse = ". "),
    Initials = paste0(Initials, ".")) %>%
  ungroup() %>% 
  # add a column from 1 to n
  mutate(ID = 1:n()) %>%
  group_by(Affiliation) %>% 
  # replace ID with min number (same affiliations become the same number)
  mutate(ID = min(ID)) %>% 
  ungroup() %>% 
  # use function above to assign new ID from 1 to n
  mutate(ID = rankID(ID)) %>%
  #Paste Last and Initials
  mutate(name = paste0(LastName, ", ", Initials)) %>%
  arrange(LastName, ID)

#NameAffList$ID = factor(NameAffList$ID)

Authors = NameAffList %>%   
  group_by(LastName, name) %>% 
  summarise(affs = paste(ID, collapse = ",")) %>% 
  mutate(
    affs = paste0("^", affs, "^"),
    nameID = paste0(name, affs))
Authors = Authors[match(unique(dat$LastName),Authors$LastName),] %>%
  #arrange(unique(dat$LastName)) %>%
  pull(nameID) %>% 
  paste(collapse = ", ")

Affiliations = NameAffList %>% 
  distinct(ID, Affiliation) %>% 
  arrange(ID) %>% 
  mutate(ID = paste0("^", ID, "^")) %>% 
  mutate(Affiliation2 = paste(ID, Affiliation, sep = "")) %>% 
  pull(Affiliation2) %>% 
  paste(collapse = ", ")