#!/bin/sh
set -eu

. /etc/os-release

cat <<EOF > /etc/apt/sources.list.d/cran.sources
Types: deb
URIs: http://cloud.r-project.org/bin/linux/debian/
Suites: ${VERSION_CODENAME}-cran40/
Components:
Signed-By: /usr/share/keyrings/cran.gpg
EOF
cat "$(dirname $0)/cran.gpg" > "/usr/share/keyrings/cran.gpg"

apt update

apt install --no-install-recommends \
  chromium \
  cmake \
  gdb \
  gfortran \
  git-absorb \
  git-lfs \
  libblas-dev \
  libcurl4-openssl-dev \
  libfribidi-dev \
  libgdal-dev \
  libgit2-dev \
  libgoogle-perftools-dev \
  libharfbuzz-dev \
  liblapack-dev \
  libmagick++-dev \
  libmpich-dev \
  libnode-dev \
  libpcre2-dev \
  libpng-dev \
  libpoppler-cpp-dev \
  libpq-dev \
  libreadline-dev \
  libssl-dev \
  libudunits2-dev \
  libxml2-dev \
  libxxhash0 \
  lmodern \
  pandoc \
  parallel \
  preview-latex-style \
  pspg \
  qpdf \
  r-recommended \
  texlive-fonts-extra \
  texlive-fonts-recommended \
  texlive-lang-european \
  texlive-latex-recommended \
  texlive-pictures \
  texlive-xetex \
  tidy \
  x11-apps \
  zlib1g-dev \

# Quarto install
QUARTO_GH_REPO="quarto-dev/quarto-cli"
QUARTO_DEB_URL="$(curl -s "https://api.github.com/repos/${QUARTO_GH_REPO}/releases/latest" | awk '/"browser_download_url": ".*amd64\.deb"/ { gsub(/\"/,""); print $2 }')"
QUARTO_DEB_PATH=/tmp/"$(basename "${QUARTO_DEB_URL}")"
wget -c -O "${QUARTO_DEB_PATH}" "${QUARTO_DEB_URL}"
dpkg -i "${QUARTO_DEB_PATH}"
apt-get install --fix-missing
rm -- "${QUARTO_DEB_PATH}"
