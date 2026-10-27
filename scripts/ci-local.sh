#!/usr/bin/env bash
# Run the same checks as .github/workflows/ci.yml on the local machine.
# Usage: scripts/ci-local.sh [--quick]
#   --quick  skips the package-dependency check and the docs build.
set -euo pipefail
cd "$(dirname "$0")/.."

echo "== compile"
raco make -v main.rkt
# Recompile every module so that raco test never loads stale bytecode
# after a dependency changed (fresh CI checkouts have no bytecode at all).
raco make -j "${OCCUR_JOBS:-8}" $(find . -name '*.rkt' -not -path '*/compiled/*' -not -path './doc/*')

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
