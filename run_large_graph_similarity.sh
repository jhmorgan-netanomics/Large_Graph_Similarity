#!/usr/bin/env bash
set -euo pipefail

if [ -z "${JULIA_NUM_THREADS:-}" ]; then
	export JULIA_NUM_THREADS=auto
fi

exec "$(dirname "$0")/large_graph_similarity_bin" "$@"