# Summary Bibliometric analyses

# OVERVIEW 
total_articles<- summarise(eb_references,total_articles=n_distinct(refID))
total_authors<- summarise(eb_refine,total_authors=n_distinct(groupID))
countries<-levels(as.factor(eb_refine$country))
country_count<-nlevels(as.factor(eb_refine$country))

authors_per_article_distribution<-eb_refine %>% count(author_order)

mean_authors_per_article<-eb_refine %>% group_by(refID) %>% 
  summarise(number_authors=max(author_order)) %>% 
  summarize(Mean = mean(number_authors, na.rm=TRUE),
            SD=sd(number_authors, na.rm=TRUE), 
            SE=(sd(number_authors, na.rm=TRUE)/sqrt(n()))) 



# BY YEAR


total_articles_yr<-eb_references %>% group_by(year=as.factor(PY)) %>%  summarise(total_articles=n_distinct(refID))
total_authors_yr<- eb_refine %>% group_by(year=as.factor(PY)) %>% summarise(authors=n_distinct(groupID))

distribution_yr<-eb_refine %>% group_by(year=as.factor(PY)) %>% count(author_order)

mean_authors_per_article_yr<-eb_refine %>% group_by(year=as.factor(PY),refID) %>% 
  summarise(number_authors=max(author_order)) %>% 
  summarize(Mean = mean(number_authors, na.rm=TRUE),
            SD=sd(number_authors, na.rm=TRUE), 
            SE=(sd(number_authors, na.rm=TRUE)/sqrt(n()))) 



# to fix
countries_yr<-select(eb_refine, PY,country)
countries_yr$PY<-as.factor(countries_yr$PY)
countries_yr$country<-as.factor(countries_yr$country)
countries_yr<-countries_yr %>% group_by(PY,country) %>% nlevels(country)
str(countries_yr)
authors_per_article_d