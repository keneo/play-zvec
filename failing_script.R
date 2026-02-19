# install.packages("reticulate")
library(reticulate)

# Use a dedicated env (recommended)
# reticulate::conda_create("rzvec")
# reticulate::conda_install("rzvec", "pip")
# reticulate::conda_install("rzvec", "python=3.11")
# reticulate::py_install("zvec", envname="rzvec", pip=TRUE)

use_virtualenv(file.path(getwd(), "rzvec-venv"), required = TRUE)
zvec <- import("zvec")

schema <- zvec$CollectionSchema(
  name = "example",
  vectors = zvec$VectorSchema("embedding", zvec$DataType$VECTOR_FP32, as.integer(4))
)

col <- zvec$create_and_open(path = "./zvec_example", schema = schema)

col$insert(list(
  zvec$Doc(id="doc_1", vectors=dict(embedding=c(0.1,0.2,0.3,0.4))),
  zvec$Doc(id="doc_2", vectors=dict(embedding=c(0.2,0.3,0.4,0.1)))
))

res <- col$query(
  zvec$VectorQuery("embedding", vector=c(0.4,0.3,0.3,0.1)),
  topk = as.integer(10)
)

res