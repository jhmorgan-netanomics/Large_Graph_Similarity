if (-not $env:JULIA_NUM_THREADS) {
	$env:JULIA_NUM_THREADS = "auto"
}

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path

& "$scriptDir\large_graph_similarity_bin.exe" $args