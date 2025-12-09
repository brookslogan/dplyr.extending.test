
library(dplyr)
library(magrittr)

toy_tbl1 <- tibble(u = c(1,1,1,2,2), t = c(1:3,1:2), v = 1:5)

toy_tbl1 %>%
  new_keyed_df4(c("u", "t")) %>%
  {.[]}

toy_tbl1 %>%
  new_keyed_df4(c("u", "t")) %>%
  {.[,1L]}

toy_tbl1 %>%
  new_keyed_df4(c("u", "t")) %>%
  {.[,1:2]}

toy_tbl1 %>%
  new_keyed_df4(c("u", "t")) %>%
  {.[c(1,1),]}

toy_tbl1 %>%
  new_keyed_df4(c("u", "t")) %>%
  {.[1:3,2]}

toy_tbl1 %>%
  new_keyed_df4(c("u", "t")) %>%
  {.[3:4,2]}

toy_tbl1 %>%
  new_keyed_df4(c("u", "t")) %>%
  {.[c(0,0),]}

toy_tbl1 %>%
  new_keyed_df4(c("u", "t")) %>%
  {.[-c(1,1),]}

toy_tbl1 %>%
  new_keyed_df4(c("u", "t")) %>%
  {.[10L,]}

toy_tbl1 %>%
  new_keyed_df4(c("u", "t")) %>%
  {.[NA_integer_,]}


toy1_ktbl1 <- new_keyed_tibble1(toy_tbl1, c("u", "t"))
toy1_ktbl2 <- new_keyed_df2(toy_tbl1, c("u", "t"))
toy1_ktbl4 <- new_keyed_df4(toy_tbl1, c("u", "t"))
toy1_tsbl <- tsibble::as_tsibble(toy_tbl1, key = u, index = t)

# tsibble bug?
toy1_tsbl[1:4,2:3] # still tsibble?
toy1_tsbl[1:4,][,2:3] # not

bench::mark(
  toy_tbl1[1:3,],
  toy1_ktbl1[1:3,],
  toy1_ktbl2[1:3,],
  toy1_ktbl4[1:3,],
  toy1_tsbl[1:3,],
  check = FALSE,
  min_time = 5,
  max_iterations = 1e9
)

# TODO also compare vs. data.table?  but it doesn't seem to require key uniqueness... its logic key is based on sortedness

prof(for (i in 1:100000) toy1_ktbl4[1:3,])


prof(for (i in 1:100000) toy_tbl1[1:3,])

toy1_ktbl4 %>% inset(1:3, "v", value = 11:13)

toy1_ktbl4 %>% inset(1:3, "u", value = 2)

toy1_ktbl4 %>% inset(1:3, "u", value = 3)

toy1_ktbl4 %>% inset("u", value = 3)

toy1_ktbl4 %>% mutate(v = v + 10)

toy1_ktbl4 %>% mutate(u = if_else(u == 1, 2, u))

toy1_ktbl4 %>% mutate(u = if_else(u == 1, 3, u))

toy1_ktbl4 %>% mutate(u = 3)

# FIXME
vctrs::vec_rbind(toy1_ktbl4, toy1_ktbl4)

# TODO consider having a kdf4_slice class that temporarily allows key violations, so default group_by type approach might work?

# TODO when filtering to a single key value, consider storing that key value in metadata?  already checking for existence. nice for printing. but will it add too much complexity or overhead?
