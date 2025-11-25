#   Large Graph Similarity Precompilation Script
#   Jonathan H. Morgan, Ph.D.

#   Load Diffusion Sim
    using Large_Graph_Similarity

#   Load Common Dependencies
    using CSV, DataFrames, EzXML, StatsBase, SparseArrays, ArgParse, ProgressMeter, Statistics, Random

#   Verify that the precompile script has loaded
    @info "precompile_exec.jl loaded"