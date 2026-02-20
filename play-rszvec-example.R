library(rszvec)
rszvec_install()

col <- rszvec_open("/tmp/rszvec_example", dim = 4)

rszvec_add(col, "doc_1", c(0.1, 0.2, 0.3, 0.4))
rszvec_add(col, "doc_2", c(0.2, 0.3, 0.4, 0.1))

res <- rszvec_search(col, c(0.4, 0.3, 0.3, 0.1), n = 10)
print(res)

unlink("/tmp/rszvec_example", recursive = TRUE)
