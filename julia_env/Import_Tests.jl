# Generalizing ORA Import Functions to Handle Datasets Other than Twitter
# Jonathan H. Morgan, Ph.D.
# 28 May 2026

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

#############
#   TESTS   #
#############

#	ORA XML Import Smoke Test
	test_dir = "/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data"

	test_files = [
		"BK12_Telegram_Summary_Network.xml",
		"Pac Rim Day 1.xml",
		"Balikatan_2022_Processed.xml",
	]

	for file in test_files
		#	Build filepath
			filepath = joinpath(test_dir, file)

		#	Run import
			println("\nTesting: $file")
			out = load_ora_xml(filepath)

		#	Report nodesets
			println("Nodesets:")
			for (key, df) in out.nodesets
				println("  $key => $(nrow(df)) nodes, $(ncol(df)) columns")
			end

		#	Report networks
			println("Networks:")
			for (key, nt) in out.networks
				println("  $key => $(nrow(nt.edges)) edges; $(nt.sourceNodeset) → $(nt.targetNodeset)")
			end
	end

#	Validate ORA import outputs
	for file in test_files
		#	Load XML
			println("\nValidating: $file")
			out = load_ora_xml(joinpath(test_dir, file))

		#	Check top-level structure
			@assert haskey(out, :nodesets)
			@assert haskey(out, :nodeset_meta)
			@assert haskey(out, :networks)

		#	Check nodesets
			for (key, df) in out.nodesets
				@assert haskey(out.nodeset_meta, key)
				@assert hasproperty(df, Symbol("Node ID"))
				@assert hasproperty(df, Symbol("Node Label"))
			end

		#	Check networks
			for (_, net) in out.networks
				@assert haskey(out.nodesets, net.sourceNodeset)
				@assert haskey(out.nodesets, net.targetNodeset)

				@assert hasproperty(net.edges, :src)
				@assert hasproperty(net.edges, :dst)
				@assert hasproperty(net.edges, :weight)
			end

		#	Report success
			println("  Passed validation")
	end

