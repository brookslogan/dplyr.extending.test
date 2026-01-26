
library(dplyr)
library(tidyr)
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

# bench::mark(
#   toy_tbl1[1:3,],
#   toy1_ktbl1[1:3,],
#   toy1_ktbl2[1:3,],
#   toy1_ktbl4[1:3,],
#   toy1_tsbl[1:3,],
#   check = FALSE,
#   min_time = 5,
#   max_iterations = 1e9
# )

# TODO also compare vs. data.table?  but it doesn't seem to require key uniqueness... its logic key is based on sortedness

# prof(for (i in 1:100000) toy1_ktbl4[1:3,])


# prof(for (i in 1:100000) toy_tbl1[1:3,])

toy1_ktbl4 %>% inset(1:3, "v", value = 11:13)

toy1_ktbl4 %>% inset(1:3, "u", value = 2)

toy1_ktbl4 %>% inset(1:3, "u", value = 3)

toy1_ktbl4 %>% inset("u", value = 3)

toy1_ktbl4 %>% mutate(v = v + 10)

toy1_ktbl4 %>% mutate(u = if_else(u == 1, 2, u))

toy1_ktbl4 %>% mutate(u = if_else(u == 1, 3, u))

toy1_ktbl4 %>% mutate(u = 3)

expect_error(vctrs::vec_rbind(toy1_ktbl4, toy1_ktbl4), regexp = "contained duplicate values")

# ... vec_cbind doesn't seem to follow using vec_ptype2 to determine the type of the container, which is probably good because it would not make sense and would create headaches.
vctrs::vec_cbind(toy1_ktbl4, toy1_ktbl4)

expect_error(vctrs::vec_rbind(toy1_ktbl4, tibble() %>% new_keyed_df4(character())), class = "vctrs_error_incompatible_type")

# TODO consider having a kdf4_slice class that temporarily allows key violations, so default group_by type approach might work?

# TODO when filtering to a single key value, consider storing that key value in metadata?  already checking for existence. nice for printing. but will it add too much complexity or overhead?

toy_tbl2 <- tibble(u = c(1,1,2,2), t = 1, h = c("am", "pm", "am", "pm"), v2 = 1:4)
toy2_ktbl4 <- new_keyed_df4(toy_tbl2, c("u", "t", "h"))

inner_join(toy1_ktbl4, toy2_ktbl4, by = c("u", "t"))

inner_join(toy1_ktbl4 %>% group_by(u), toy2_ktbl4, by = c("u", "t"))

inner_join(toy1_ktbl4, toy2_ktbl4 %>% group_by(u), by = c("u", "t"))

inner_join(
  new_keyed_df4(tibble(k = 11:14, v = 1), "k"),
  new_keyed_df4(tibble(k = 21:24, v = 1), "k"),
  by = "v",
  relationship = "many-to-many"
)

inner_join(
  new_keyed_df4(tibble(u = 1, k = 11:14, v = 1), "k"),
  new_keyed_df4(tibble(u = 1, k = 21:24, v = 1), "k"),
  by = "u",
  relationship = "many-to-many"
)

inner_join(
  new_keyed_df4(tibble(k = 11:14, v = 1), "k"),
  new_keyed_df4(tibble(v = 1, vsynonym = "one", v2 = 5), "vsynonym"),
  by = "v",
  relationship = "many-to-one"
)

inner_join(
  new_keyed_df4(tibble(k = 11:14, v = 1), "k"),
  new_keyed_df4(tibble(v = 1, vsynonym = "one", v2 = 5), "vsynonym"),
  by = "v"
)

inner_join(
  new_keyed_df4(tibble(k = 1), "k"),
  tibble(k = 1, v = 1:5),
  by = "k",
  multiple = "first"
)

# tbl1 <- tibble(g = c(rep(1, 50), rep(2, 50)), t = c(1:50, 1:50), v1 = 1:100)
# tbl2 <- tibble(g = c(rep(1, 50), rep(2, 50)), t = c(1:50, 1:50), v2 = 1:100)
# bench::mark(
#   inner_join(tbl1, tbl2, by = c("g", "t")),
#   inner_join(tbl1, tbl2, by = c("g", "t"), relationship = "one-to-one"),
#   inner_join(tbl1, tbl2, by = c("g", "t"), relationship = "many-to-one"),
#   inner_join(tbl1, tbl2, by = c("g", "t"), relationship = "one-to-many"),
#   inner_join(tbl1, tbl2, by = c("g", "t"), relationship = "many-to-many"),
#   min_time = 20
# )

left_join(
  new_keyed_df4(tibble(g = 1, t = 1:5), c("g", "t")),
  new_keyed_df4(tibble(g = 2, t = 1:5), c("g", "t")),
  by = "g"
)

left_join(
  new_keyed_df4(tibble(g = 1, t = 1:5), c("g", "t")),
  new_keyed_df4(tibble(g = 2, t = 1:5), c("g", "t"))
)

cross_join(
  new_keyed_df4(tibble(g = 1, t = 1:5), c("g", "t")),
  new_keyed_df4(tibble(g = 2, v = 1:5), c("g", "t"))
)

nest_join(
  new_keyed_df4(tibble(g = c(1,2)), "g"),
  new_keyed_df4(tibble(g = c(1,1,2,2), t = c(1:2, 1:2)), c("g", "t")),
  by = "g",
  name = "y"
)$y

my_y <- new_keyed_df4(tibble(g = c(1,1,2,2), t = c(1:2, 1:2)), c("g", "t"))
nest_join(
  new_keyed_df4(tibble(g = c(1,2)), "g"),
  my_y,
  by = "g"
)$my_y

my_y <- new_keyed_df4(tibble(g = c(1,1,2,2), t = c(1:2, 1:2)), c("g", "t"))
nest_join(
  new_keyed_df4(tibble(g = c(1,2)), "g"),
  my_y,
  by = "g",
  keep = TRUE
)$my_y

nest_join(
  tibble(g = c(1,2)),
  new_keyed_df4(tibble(g = c(1,1,2,2,3,3), t = c(1,2,1,2,1,2)), c("g", "t")),
  name = "y"
)$y

new_keyed_df4(tibble(g = c(1,1,2), t = c(1:2, 1)), c("g", "t")) %>% complete(g, t)

nest(new_keyed_df4(tibble(g = c(1,1,2,2), t = c(1:2, 1:2)), c("g", "t")), data = g)
