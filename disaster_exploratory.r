library(tidyverse)
library(tidytext)
library(lubridate)
library(ggtext)

# read in train ds
train = read_csv('./data/train.csv')
str(train)

# how many tweets are about real disasters
summary(train$target) # ~43% are real disasters
head(train)
tail(train)
# how many tweets have keywords or location info?
sum(is.na(train$keyword))/nrow(train)*100 # 0.8% of the tweets don't have keywoords
sum(is.na(train$location))/nrow(train)*100 # 33% of the tweets don't have location

# preprocess text to extract useful info (info on km? digits? hashtags? mentions police?)
train_words = train %>% 
                unnest_tokens(word, text, token='tweets', to_lower = F) %>%
                anti_join(stop_words)

# count URL occurrence, hashstags, mentions
train_words = train_words %>%
  mutate(isUrl = case_when(str_detect(word, 'http') ~ 1,
                           TRUE ~ 0),
         isHashtag = case_when(str_detect(word, '^#') ~ 1,
                               TRUE ~ 0),
         isMention = case_when(str_detect(word, '^@') ~ 1,
                               TRUE ~ 0),
         isAllCaps = case_when(str_detect(word, '[[:upper:]]+$') ~ 1,
                               TRUE ~ 0),
         isCapitalized = case_when(str_detect(word, '^[[:upper:]][[:lower:]]+$') ~ 1,
                                   TRUE ~ 0),
         isDigit = case_when(str_detect(word, '[:digit:]+') ~ 1,
                             TRUE ~ 0))

head(train_words)
train_words[train_words$id==1,]
train_words[train_words$id==nrow(train),]

# count number of words
train_words = train_words %>%
  add_count(id, name='n_words')

summary(train_words$n_words)

# compare top 20 words in target VS non-target
top20 = train_words %>% 
  group_by(target) %>%
  count(word, sort=TRUE) %>% 
  top_n(20)

# plot top 20 words
ggplot(top20, aes(x=n, y=reorder_within(word,n, factor(target)), fill=factor(target)))+
  geom_col(color='black')+
  scale_y_reordered()+
  facet_wrap(~target, scales='free')+
  theme_classic()

# check length of tweets target vs non-target
ggplot(train_words, aes(factor(target), n_words, fill=factor(target))) +
  geom_boxplot()

# check top hashtags
hash = train_words %>% filter(isHashtag == 1)
head(hash)

top20hash = hash %>% 
  group_by(target) %>%
  count(word, sort=TRUE) %>% 
  top_n(20)

ggplot(top20hash, aes(x=n, y=reorder_within(word,n, factor(target)), fill=factor(target)))+
  geom_col(color='black')+
  scale_y_reordered()+
  facet_wrap(~target, scales='free')+
  theme_classic()

# check presence of urls, mentions, hashtags, in target vs non-target and count them
train_pro = train_words %>% 
  group_by(id) %>%
  summarise(keyword = unique(keyword),
            location = unique(location),
            target = unique(target),
            n_url = sum(isUrl),
            n_hash = sum(isHashtag),
            n_mention = sum(isMention),
            n_words = unique(n_words),
            n_caps = sum(isCapitalized),
            n_AllCaps = sum(isAllCaps),
            n_digits = sum(isDigit),
            words = str_c(word, collapse = " "))
head(train_pro)

logm1 = glm(target ~ n_url*n_url*n_hash*n_mention*n_words*n_caps*n_AllCaps*n_digits, data=train_pro, family='binomial')
summary(logm1)

# save dataset
write_csv(train_pro, 'train_pro.csv')




### test ds ----
test = read_csv('./data/test.csv')

test_word = test %>% 
  unnest_tokens(word, text, token='tweets', to_lower = F) %>%
  anti_join(stop_words)

# count URL occurrence, hashstags, mentions
test_word = test_word %>%
  mutate(isUrl = case_when(str_detect(word, 'http') ~ 1,
                           TRUE ~ 0),
         isHashtag = case_when(str_detect(word, '^#') ~ 1,
                               TRUE ~ 0),
         isMention = case_when(str_detect(word, '^@') ~ 1,
                               TRUE ~ 0),
         isAllCaps = case_when(str_detect(word, '[[:upper:]]+$') ~ 1,
                               TRUE ~ 0),
         isCapitalized = case_when(str_detect(word, '^[[:upper:]][[:lower:]]+$') ~ 1,
                                   TRUE ~ 0),
         isDigit = case_when(str_detect(word, '[:digit:]+') ~ 1,
                             TRUE ~ 0))

# count number of words
test_word = test_word %>%
  add_count(id, name='n_words')

test = test_word %>% 
  group_by(id) %>%
  summarise(keyword = unique(keyword),
            location = unique(location),
            n_url = sum(isUrl),
            n_hash = sum(isHashtag),
            n_mention = sum(isMention),
            n_words = unique(n_words),
            n_caps = sum(isCapitalized),
            n_AllCaps = sum(isAllCaps),
            n_digits = sum(isDigit),
            words = str_c(word, collapse = " "))

# save dataset
write_csv(test, './submission/test.csv')

