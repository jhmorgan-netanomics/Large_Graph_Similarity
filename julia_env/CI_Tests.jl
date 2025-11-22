#Test Script of Large_Graph_Similarity's Client Script
#Jonathan H. Morgan
#10-21 November 2025

#   Pulling-In BEND_2022 & Activating Local Environment
    cd("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity")
    using Pkg
    Pkg.activate("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/julia_env")
    Pkg.status()


################
#   PACKAGES   #
################
using CSV
using DataFrames
using LinearAlgebra
using SparseArrays
using Statistics
using StatsBase
using Large_Graph_Similarity

#################
#   FUNCTIONS   #
#################


#######################
#   CI CLIENT TESTS   #
#######################