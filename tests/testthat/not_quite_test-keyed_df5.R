
toy_kdf5 <- as_keyed_df5(tibble(g = c(1,1,2,2), t = c(1,2,1,2), v = 1:4), c("g", "t"))

toy_kdf5[c("t", "v")]

toy_kdf5[c("t", "v")][1:2,]

toy2_kdf5 <- as_keyed_df5(tibble(g = c(1,1,2,2), t = c(1,2,1,2), v = 1:4), c("g", "t"), tibble(as_of = 5))

toy2_kdf5

toy2_kdf5[c("t", "v")]

toy2_kdf5[c("t", "v")][1:2,]
