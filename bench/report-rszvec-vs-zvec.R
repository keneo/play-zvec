# report-rszvec-vs-zvec.R
#
# Reads bench-rszvec-vs-zvec.csv (produced by bench-rszvec-vs-zvec.R) and
# renders a comparison report as a multi-panel PDF.
#
# Requires ggplot2 and scales. Run with renv bypassed so the system library
# is used:
#   Rscript --no-init-file bench/report-rszvec-vs-zvec.R

# Ensure paths are relative to this script, not the caller's working directory.
.bench_dir <- local({
  args <- commandArgs(trailingOnly = FALSE)
  f    <- sub("--file=", "", args[grep("--file=", args)])
  if (length(f)) dirname(normalizePath(f, mustWork = FALSE)) else getwd()
})
setwd(.bench_dir)

if (!requireNamespace("ggplot2", quietly = TRUE))
  stop("Install ggplot2 first:  install.packages('ggplot2')")
if (!requireNamespace("scales",  quietly = TRUE))
  stop("Install scales first:   install.packages('scales')")

library(ggplot2)
library(scales)

csv <- "bench-rszvec-vs-zvec.csv"
if (!file.exists(csv))
  stop("Run bench-rszvec-vs-zvec.R first to produce ", csv)

df <- read.csv(csv, stringsAsFactors = FALSE)

# ---------------------------------------------------------------------------
# Tidy labels
# ---------------------------------------------------------------------------

IMPL_LEVELS <- c("zvec_py", "rzvec", "rszvec")
IMPL_LABELS <- c("zvec_py" = "zvec Python\n(direct reticulate)",
                 "rzvec"   = "rzvec\n(thin R wrappers)",
                 "rszvec"  = "rszvec\n(high-level R API)")
IMPL_COLORS <- c("zvec_py" = "#2166ac",
                 "rzvec"   = "#4dac26",
                 "rszvec"  = "#d6604d")

df$impl <- factor(df$impl, levels = IMPL_LEVELS, labels = IMPL_LABELS[IMPL_LEVELS])

fmt_n <- function(x) {
  ifelse(x >= 1e6, paste0(x / 1e6, "M"),
  ifelse(x >= 1e3, paste0(x / 1e3, "K"), as.character(x)))
}

theme_bench <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor   = element_blank(),
      strip.text         = element_text(face = "bold"),
      legend.position    = "bottom",
      legend.title       = element_blank(),
      plot.title         = element_text(face = "bold"),
      plot.subtitle      = element_text(color = "grey40", size = 10),
      axis.title         = element_text(size = 10),
      plot.margin        = margin(10, 15, 10, 10)
    )
}

impl_scale <- scale_fill_manual(
  values = setNames(IMPL_COLORS, IMPL_LABELS[IMPL_LEVELS])
)

# ---------------------------------------------------------------------------
# Figure 1: Single insert overhead  (ms per call)
# ---------------------------------------------------------------------------

d1 <- df[df$operation == "single_insert", ]

p1 <- ggplot(d1, aes(x = impl, y = per_op_ms, fill = impl)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.2f ms", per_op_ms)),
            vjust = -0.4, size = 3.5) +
  impl_scale +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Single insert — per-call overhead",
    subtitle = sprintf("Each impl inserts %d docs one at a time; lower = faster",
                       unique(d1$reps)),
    x = NULL,
    y = "Time per call (ms)"
  ) +
  theme_bench()

# ---------------------------------------------------------------------------
# Figure 2: Bulk insert throughput  (docs / second)
# ---------------------------------------------------------------------------

d2            <- df[df$operation == "bulk_insert", ]
d2$throughput <- d2$n / d2$total_secs
d2$n_label    <- factor(fmt_n(d2$n), levels = fmt_n(sort(unique(d2$n))))

p2 <- ggplot(d2, aes(x = n_label, y = throughput, fill = impl)) +
  geom_col(position = position_dodge(0.75), width = 0.65) +
  geom_text(aes(label = comma(round(throughput))),
            position = position_dodge(0.75),
            vjust = -0.4, size = 2.8) +
  impl_scale +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Bulk insert — throughput",
    subtitle = "Includes doc-object creation + batched insert; higher = faster",
    x        = "Number of documents inserted",
    y        = "Throughput (docs / second)"
  ) +
  theme_bench()

# ---------------------------------------------------------------------------
# Figure 3: Search latency vs. collection size  (ms per query)
# ---------------------------------------------------------------------------

d3         <- df[df$operation == "search", ]
d3$n_label <- factor(fmt_n(d3$n), levels = fmt_n(sort(unique(d3$n))))

p3 <- ggplot(d3, aes(x = n_label, y = per_op_ms, color = impl, group = impl)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(aes(label = sprintf("%.2f", per_op_ms)),
            vjust = -0.7, size = 3, show.legend = FALSE) +
  scale_color_manual(values = setNames(IMPL_COLORS, IMPL_LABELS[IMPL_LEVELS])) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  labs(
    title    = "Search latency vs. collection size",
    subtitle = sprintf("Average over %d queries (top-10); lower = faster",
                       unique(d3$reps)),
    x = "Collection size",
    y = "Latency per query (ms)"
  ) +
  theme_bench() +
  theme(legend.position = "bottom")

# ---------------------------------------------------------------------------
# Figure 4: Overhead ratio  (impl latency / zvec_py latency)
# ---------------------------------------------------------------------------

base_impl <- IMPL_LABELS["zvec_py"]

d4 <- do.call(rbind, lapply(split(df, df$operation), function(sub) {
  base <- sub[sub$impl == base_impl, c("n", "per_op_ms")]
  merged <- merge(sub, base, by = "n", suffixes = c("", "_base"))
  merged$ratio <- merged$per_op_ms / merged$per_op_ms_base
  merged
}))
d4 <- d4[d4$impl != base_impl, ]
d4$op_label <- c(
  "single_insert" = "Single insert",
  "bulk_insert"   = "Bulk insert",
  "search"        = "Search"
)[d4$operation]
d4$n_label <- fmt_n(d4$n)

p4 <- ggplot(d4, aes(x = n_label, y = ratio, fill = impl)) +
  geom_col(position = position_dodge(0.75), width = 0.65) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  geom_text(aes(label = sprintf("%.2fx", ratio)),
            position = position_dodge(0.75),
            vjust = -0.4, size = 2.8) +
  impl_scale +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  facet_wrap(~ op_label, scales = "free_x") +
  labs(
    title    = "Overhead ratio vs. direct Python",
    subtitle = "1× = same speed as raw reticulate zvec_py; dashed line = baseline",
    x        = "n",
    y        = "Overhead ratio (× zvec_py)"
  ) +
  theme_bench()

# ---------------------------------------------------------------------------
# Render to PDF
# ---------------------------------------------------------------------------

out <- "bench-rszvec-vs-zvec-report.pdf"
pdf(out, width = 9, height = 7)
print(p1)
print(p2)
print(p3)
print(p4)
dev.off()

cat("Report written to", out, "\n")

# ---------------------------------------------------------------------------
# Console summary table
# ---------------------------------------------------------------------------

cat("\n--- Summary table ---\n")
fmt_row <- function(op, impl_col, n, metric, value, unit) {
  cat(sprintf("  %-14s  %-26s  n=%-7s  %s = %.2f %s\n",
              op, impl_col, fmt_n(n), metric, value, unit))
}
for (i in seq_len(nrow(df))) {
  r    <- df[i, ]
  impl <- as.character(r$impl)
  if (r$operation == "bulk_insert") {
    fmt_row(r$operation, impl, r$n, "throughput", r$n / r$total_secs, "docs/s")
  } else {
    fmt_row(r$operation, impl, r$n, "latency   ", r$per_op_ms, "ms")
  }
}
