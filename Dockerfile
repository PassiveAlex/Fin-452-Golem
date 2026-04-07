# ── Base image ────────────────────────────────────────────────────────────────
# rocker/r-ver supports linux/amd64 and linux/arm64 natively (Apple Silicon,
# standard x86 servers). No --platform flag is used; buildx handles targeting.
FROM rocker/r-ver:4.3

# ── System libraries ──────────────────────────────────────────────────────────
# Required by R package dependencies (curl, openssl, xml2, arrow, fonts,
# numeric/Fortran packages, etc.)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libfontconfig1-dev \
    zlib1g-dev \
    libgfortran5 \
    libblas-dev \
    liblapack-dev \
    && rm -rf /var/lib/apt/lists/*

# ── remotes (stable, widely tested in Docker builds) ─────────────────────────
RUN Rscript -e "install.packages('remotes', repos = 'https://cloud.r-project.org')"

WORKDIR /app

# ── Layer-cache trick: install deps before copying full source ─────────────────
# Copying only DESCRIPTION/NAMESPACE first means the expensive install step is
# skipped on rebuilds where only R source files changed.
COPY DESCRIPTION NAMESPACE ./
# Only install Imports + Depends — Suggests are dev/test tools not needed at runtime.
RUN Rscript -e "remotes::install_deps(dependencies = c('Imports', 'Depends'), upgrade = 'never')"

# ── Copy full package source (includes inst/extdata/eia_fundamentals.feather) ──
COPY . .

# ── Install the package itself ────────────────────────────────────────────────
RUN R CMD INSTALL --no-multiarch --with-keep.source .

EXPOSE 3838

# ── Launch ────────────────────────────────────────────────────────────────────
# host 0.0.0.0 is required so the port is reachable outside the container.
# EIA_API_KEY is optional — the bundled feather cache works without it.
CMD ["Rscript", "-e", "fin452golem::run_app(options = list(host = '0.0.0.0', port = 3838))"]
