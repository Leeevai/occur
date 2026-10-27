#!/usr/bin/env bash
# Run the same checks as .github/workflows/ci.yml on the local machine.
# Usage: scripts/ci-local.sh [--quick]
#   --quick  skips the package-dependency check and the docs build.
set -euo pipefail
cd "$(dirname "$0")/.."

echo "== compile"
raco make -v main.rkt

echo "== test"
raco test -j "${OCCUR_JOBS:-8}" .

if [ "${1:-}" != "--quick" ]; then
  echo "== package dependencies"
  raco setup --check-pkg-deps --pkgs occur

  echo "== docs"
  if [ -f scribblings/occur.scrbl ]; then
    dest="$(mktemp -d)"
    raco scribble --htmls --dest "$dest" scribblings/occur.scrbl
    rm -rf "$dest"
  fi
fi
echo "== ok"
