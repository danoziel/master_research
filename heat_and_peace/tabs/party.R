# "party" value
x <- peace_index %>% select(1,6,2,3,13,15)
part <- x %>% filter(!is.na(party))
part <- part %>% group_by(date1) %>%  summarise(sum(party))

# in these dates range - NO "party" value
# date1-    63-195
# date-  1999-05-53 : 2010-05-30


# leftrigt
x <- peace_index %>% select(1,6,2,3,13,15)
leftri <- x %>% filter(!is.na(leftrigt))
leftri <- leftri %>% group_by(date1) %>%  summarise(sum(leftrigt))
# 14 questionnaire total in 1999-2009

x <- peace_index %>% select(1,6,2,3,pvote99, pvote09, pvote06, pvote03)

p99 <- x %>% filter(!is.na(pvote99))
p99 <- p99 %>% group_by(date1) %>%  summarise(sum(pvote99))

p03 <- x %>% filter(!is.na(pvote03))
p03 <- p03 %>% group_by(date1) %>%  summarise(sum(pvote03))

p06 <- x %>% filter(!is.na(pvote06))
p06 <- p06 %>% group_by(date1) %>%  summarise(sum(pvote06))

p09 <- x %>% filter(!is.na(pvote09))
p09 <- p09 %>% group_by(date1) %>%  summarise(sum(pvote09))

x %>% filter(date1==108)




