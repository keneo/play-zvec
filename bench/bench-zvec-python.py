"""
bench-zvec-python.py

Pure-Python zvec benchmark — no R, no reticulate.
Called by bench-rszvec-vs-zvec.R to establish the baseline.

Writes bench-zvec-python.csv with columns:
  operation, impl, n, dim, reps, total_secs, per_op_ms
"""

import csv
import os

# Ensure output files land next to this script, not the caller's cwd.
os.chdir(os.path.dirname(os.path.abspath(__file__)))
import random
import shutil
import time
import zvec

# ---------------------------------------------------------------------------
# Parameters  (must match the R bench script)
# ---------------------------------------------------------------------------

SEED            = 42
DIM             = 128
N_SINGLE        = 300
N_BULK_SIZES    = [500, 2000, 5000]
N_SEARCH_SIZES  = [1000, 10000, 50000]
N_QUERIES       = 100

random.seed(SEED)


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def make_col(path):
    schema = zvec.CollectionSchema(
        name    = "bench",
        vectors = zvec.VectorSchema(
            name      = "vec",
            data_type = zvec.DataType.VECTOR_FP32,
            dimension = DIM,
        ),
    )
    return zvec.create_and_open(path=path, schema=schema)


def fresh_dir(tag):
    d = f"/tmp/zvec_bench_py_{tag}"
    shutil.rmtree(d, ignore_errors=True)
    return d


def rand_vec():
    return [random.gauss(0, 1) for _ in range(DIM)]


results = []

def record(operation, n, reps, secs):
    results.append({
        "operation": operation,
        "impl":      "zvec_py",
        "n":         n,
        "dim":       DIM,
        "reps":      reps,
        "total_secs": round(secs, 6),
        "per_op_ms":  round(secs / reps * 1000, 6),
    })


print(f"zvec Python baseline  dim={DIM}")

# ---------------------------------------------------------------------------
# 1. Single insert
# ---------------------------------------------------------------------------

print(f"\n1. Single insert  (n={N_SINGLE})")

ids  = [f"d{i}" for i in range(N_SINGLE)]
vecs = [rand_vec() for _ in range(N_SINGLE)]

d   = fresh_dir("single")
col = make_col(d)
t0  = time.perf_counter()
for i in range(N_SINGLE):
    col.insert([zvec.Doc(id=ids[i], vectors={"vec": vecs[i]})])
t = time.perf_counter() - t0
col = None
shutil.rmtree(d, ignore_errors=True)

record("single_insert", N_SINGLE, N_SINGLE, t)
print(f"  zvec_py  {t/N_SINGLE*1000:.2f} ms/call")

# ---------------------------------------------------------------------------
# 2. Bulk insert
# ---------------------------------------------------------------------------

print("\n2. Bulk insert")

for n in N_BULK_SIZES:
    ids  = [f"d{i}" for i in range(n)]
    vecs = [rand_vec() for _ in range(n)]
    docs = [zvec.Doc(id=ids[i], vectors={"vec": vecs[i]}) for i in range(n)]

    d   = fresh_dir(f"bulk_{n}")
    col = make_col(d)
    t0  = time.perf_counter()
    batch = 1000
    for s in range(0, n, batch):
        col.insert(docs[s:s + batch])
    t = time.perf_counter() - t0
    col = None
    shutil.rmtree(d, ignore_errors=True)

    record("bulk_insert", n, n, t)
    print(f"  zvec_py  n={n:>5}  {n/t:>8.0f} docs/s")

# ---------------------------------------------------------------------------
# 3. Search latency
# ---------------------------------------------------------------------------

print(f"\n3. Search latency  ({N_QUERIES} queries)")

for n in N_SEARCH_SIZES:
    ids  = [f"d{i}" for i in range(n)]
    vecs = [rand_vec() for _ in range(n)]
    docs = [zvec.Doc(id=ids[i], vectors={"vec": vecs[i]}) for i in range(n)]
    qs   = [rand_vec() for _ in range(N_QUERIES)]

    d   = fresh_dir(f"search_{n}")
    col = make_col(d)
    batch = 1000
    for s in range(0, n, batch):
        col.insert(docs[s:s + batch])

    t0 = time.perf_counter()
    for q in qs:
        col.query(
            vectors        = zvec.VectorQuery(field_name="vec", vector=q),
            topk           = 10,
            filter         = None,
            include_vector = False,
            output_fields  = None,
            reranker       = None,
        )
    t = time.perf_counter() - t0
    col = None
    shutil.rmtree(d, ignore_errors=True)

    record("search", n, N_QUERIES, t)
    print(f"  zvec_py  n={n:>6}  {t/N_QUERIES*1000:.2f} ms/query")

# ---------------------------------------------------------------------------
# Save
# ---------------------------------------------------------------------------

out = "bench-zvec-python.csv"
with open(out, "w", newline="") as f:
    writer = csv.DictWriter(f, fieldnames=results[0].keys())
    writer.writeheader()
    writer.writerows(results)

print(f"\nSaved {len(results)} rows -> {out}")
