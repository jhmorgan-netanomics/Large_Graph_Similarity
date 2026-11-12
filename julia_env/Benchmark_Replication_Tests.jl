#Benchmark Replication Tests: Measures Against Results from Other Packages
#12 November 2026
#
#	Each measure is computed on the Balikatan 2022 Agent x Agent - All Communication
#	network and compared with a result saved from another package (ORA, igraph,
#	scoredec, Pajek, Python, R). The benchmark files are in Test_Data. The
#	comparisons are those of Graph_Comparison_Tests.jl, gathered here as tests so
#	that a change to the source can be checked against them in one run.
#
#	Each comparison is recorded with its statistic and the level it must meet. The
#	levels are the agreement found when the benchmarks were first made (October to
#	November 2025), with a small allowance where the measure involves a random
#	search (Leiden).
#
#	Run from the Large_Graph_Similarity folder:
#		julia -t auto --project=julia_env julia_env/Benchmark_Replication_Tests.jl
#
#	To run the same benchmarks against another copy of the source (for example an
#	earlier version), set LGS_SOURCE to its Large_Graph_Similarity.jl:
#		LGS_SOURCE=/path/to/Large_Graph_Similarity.jl julia -t auto --project=julia_env julia_env/Benchmark_Replication_Tests.jl
#
#	Output: Test_Data/benchmark_replication_results.csv (override with
#	LGS_BENCHMARK_OUTPUT).

################
#   PACKAGES   #
################
	using CSV
	using DataFrames
	using Statistics
	using StatsBase
	using Test

#	The Module under Test
	if haskey(ENV, "LGS_SOURCE")
		include(ENV["LGS_SOURCE"])
		using .Large_Graph_Similarity
	else
		using Large_Graph_Similarity
	end
	const LGS = Large_Graph_Similarity

#############
#   PATHS   #
#############
	const TEST_DATA = normpath(joinpath(@__DIR__, "..", "Test_Data"))
	const OUTPUT_PATH = get(ENV, "LGS_BENCHMARK_OUTPUT", joinpath(TEST_DATA, "benchmark_replication_results.csv"))

######################
#   TEST FUNCTIONS   #
######################

#	Results Table
	const RESULTS = DataFrame(measure = String[], benchmark = String[], statistic = String[],
							  value = Float64[], requirement = String[], passed = Bool[])

#	Helper Function for the Tests: Record One Comparison
	function record(measure::String, benchmark::String, statistic::String, value::Real, requirement::String, passed::Bool)
		"""
		Args:
			measure::String: the measure compared
			benchmark::String: the package and file it is compared with
			statistic::String: what was computed (largest difference, rank correlation, ...)
			value::Real: the statistic
			requirement::String: the level the statistic must meet
			passed::Bool: whether it met that level
		Returns:
			Bool: passed
		Notes:
			Adds one row to RESULTS and prints it.
		"""

		#	Record and print
			push!(RESULTS, (measure, benchmark, statistic, Float64(value), requirement, passed))
			println(passed ? "PASS  " : "FAIL  ", rpad(measure, 44), rpad(statistic, 30), rpad(string(round(Float64(value); sigdigits = 6)), 14), requirement)

		#	Assembling Result
			return passed
	end

#	Helper Function for the Tests: Read a Benchmark File Keyed by Node Id
	function read_benchmark(file::String, column_names::Vector{String})
		"""
		Args:
			file::String: file name in Test_Data
			column_names::Vector{String}: names given to its leading columns, the first being "node"
		Returns:
			DataFrame: the leading columns, with node ids as strings
		"""

		#	Read with node ids as text
			table = CSV.read(joinpath(TEST_DATA, file), DataFrame; types = Dict(1 => String))
			table = table[:, 1:length(column_names)]
			rename!(table, column_names)

		#	Assembling Result
			return table
	end

#	Helper Function for the Tests: Join Computed Scores to a Benchmark
	function joined(computed::DataFrame, benchmark::DataFrame)
		"""
		Args:
			computed::DataFrame: scores with a :node column
			benchmark::DataFrame: benchmark scores with a :node column
		Returns:
			DataFrame: rows present in both, with node ids as strings
		"""

		#	Join on node id
			left = copy(computed)
			left.node = String.(string.(left.node))

		#	Assembling Result
			return innerjoin(left, benchmark, on = :node)
	end

#	Helper Function for the Tests: Largest Absolute Difference Between Two Columns
	function largest_difference(table::DataFrame, column_1, column_2)
		"""
		Args:
			table::DataFrame: joined scores
			column_1, column_2: the two columns to compare
		Returns:
			Float64: the largest absolute difference
		"""

		#	Assembling Result
			return maximum(abs.(Float64.(table[!, column_1]) .- Float64.(table[!, column_2])))
	end

############
#   DATA   #
############
	balikatan = load_ora_xml(joinpath(TEST_DATA, "Balikatan_2022_Processed.xml"))
	edges = balikatan.networks["Agent x Agent - All Communication"].edges
	nodes = balikatan.nodesets["Agent"][:, 1:2]
	rename!(nodes, ["id", "label"])
	n_nodes = nrow(nodes)

#############
#   TESTS   #
#############

#	Helper Function for the Tests: Write the Results Table
	function write_results()
		"""
		Returns:
			Nothing
		Notes:
			Called when the test set ends, whether or not every comparison passed.
		"""

		#	Write and report
			CSV.write(OUTPUT_PATH, RESULTS)
			println("\n", count(RESULTS.passed), " of ", nrow(RESULTS), " benchmark comparisons met their requirement. Table: ", OUTPUT_PATH)

		#	Early return
			return nothing
	end

atexit(write_results)
@testset "Benchmark replication" begin

	#	Degree Measures against ORA
		@testset "Degree (ORA)" begin
			ora = read_benchmark("Agent_Agent_AllCommunication_DegreeMeasures.csv",
								 ["node", "total_scaled", "out_scaled", "in_scaled", "total", "in", "out"])
			#	Weighted degree: equal on every node that has a link
				for (label, computed, column) in (
						("weighted in-degree", in_degree(edges; weighted = true), "in"),
						("weighted out-degree", out_degree(edges; weighted = true), "out"),
						("weighted total degree", total_degree(edges; weighted = true, normalize = false, agg_func = sum,
															  drop_self_loops = false, count_self_loops_once = true), "total"))
					table = joined(computed, ora)
					difference = largest_difference(table, 2, column)
					@test record(label, "ORA", "largest difference", difference, "exactly 0 on every node with a link", difference == 0.0 && nrow(table) == nrow(computed))
				end

			#	Normalized total degree. The denominators differ (ORA divides by all nodes,
			#	this package by the nodes with links, less one), so the two are proportional.
				table = joined(total_degree(edges; weighted = true, normalize = true, drop_self_loops = false, count_self_loops_once = true), ora)
				ratios = Float64.(table[!, 2])[table.total_scaled .> 0] ./ table.total_scaled[table.total_scaled .> 0]
				spread = maximum(abs.(ratios ./ median(ratios) .- 1.0))
				@test record("normalized total degree", "ORA", "largest deviation from a constant ratio", spread, "below 1e-6 (proportional to ORA)", spread < 1e-6)

			#	Normalized in- and out-degree. These also leave self-loops out, which ORA
			#	counts, so the 41 nodes with a self-loop depart from proportionality.
				for (label, computed, column) in (
						("normalized in-degree", in_degree(edges; weighted = true, normalize = true), "in_scaled"),
						("normalized out-degree", out_degree(edges; weighted = true, normalize = true), "out_scaled"))
					table = joined(computed, ora)
					rho = corspearman(Float64.(table[!, 2]), Float64.(table[!, column]))
					@test record(label, "ORA", "Spearman rank correlation", rho, "at least 0.97 (self-loops are treated differently)", rho >= 0.97)
				end
		end

	#	Local Clustering against ORA and igraph
		@testset "Local clustering (ORA, igraph)" begin
			#	Ego network density against ORA (ORA reports three decimals)
				ora = read_benchmark("Density_Local_Clustering.csv", ["node", "screen_name", "ora_clustering"])
				density = local_clustering_coefficient(edges; method = :local_density, directed = true)
				table = joined(density, ora)
				difference = largest_difference(table, 3, "ora_clustering")
				@test record("local clustering (ego density)", "ORA", "largest difference", difference, "below 0.0006 (ORA rounds to 3 decimals)", difference < 0.0006)

			#	Clemente and Grassi directed weighted clustering, and Barrat, against igraph
				igraph = read_benchmark("Balikatan_Clemente_Grassi.csv",
										["node", "cg_cycle_ig", "cg_middleman_ig", "cg_in_ig", "cg_out_ig", "cg_total_ig", "barrat_local_ig"])
				table = joined(weighted_clustering_coefficient(edges; directed = true, agg_func = sum), igraph)
				for column in ("cg_cycle", "cg_middleman", "cg_in", "cg_out", "cg_total")
					difference = largest_difference(table, column, column * "_ig")
					@test record("weighted clustering: $column", "igraph (R)", "largest difference", difference, "below 1e-10", difference < 1e-10 && nrow(table) > 0)
				end

			#	Barrat's coefficient. A reciprocated pair of links is combined by sum here and
			#	by maximum in the benchmark, so nodes with such a pair in a triangle differ.
				n_differing = count(abs.(table.barrat_local .- table.barrat_local_ig) .> 1e-9)
				rho = corspearman(table.barrat_local, Float64.(table.barrat_local_ig))
				@test record("weighted clustering: barrat_local", "igraph (R)", "nodes that differ", n_differing, "no more than 30 of $(nrow(table)) (reciprocated links are combined differently)", n_differing <= 30)
				@test record("weighted clustering: barrat_local", "igraph (R)", "Spearman rank correlation", rho, "at least 0.9999", rho >= 0.9999)

			#	The same measures on the five-node example
				toy = DataFrame(src = ["A","A","B","B","C","D","E"], dst = ["B","C","C","D","A","A","B"],
								weight = [1.0, 5.0, 2.0, 3.0, 1.0, 4.0, 1.0])
				expected = CSV.read(joinpath(TEST_DATA, "clustering_comparison.csv"), DataFrame)
				computed = weighted_clustering_coefficient(toy; directed = true, agg_func = sum)
				difference = maximum(maximum(abs.(expected[!, column] .- computed[!, column])) for column in ("cg_cycle", "cg_middleman", "cg_in", "cg_out", "cg_total"))
				@test record("weighted clustering: five-node example", "igraph (R)", "largest difference", difference, "below 1e-10", difference < 1e-10)
		end

	#	PageRank against ORA
		@testset "PageRank (ORA)" begin
			ora = read_benchmark("Balikatan_2022_All_Comm_PageRank.csv", ["node", "ora_page_rank"])
			scores = pagerank_local_ora(edges; mode = :in, weighted = true)
			table = joined(DataFrame(node = scores.node_names, page_rank = scores.scores), ora)
			rho = corspearman(Float64.(table.page_rank), Float64.(table.ora_page_rank))
			@test record("PageRank (ORA style)", "ORA", "Spearman rank correlation", rho, "at least 0.9998", rho >= 0.9998)
			difference = largest_difference(table, "page_rank", "ora_page_rank")
			@test record("PageRank (ORA style)", "ORA", "largest difference", difference, "below 0.002", difference < 0.002)
		end

	#	SALSA against R
		@testset "SALSA (R)" begin
			r_scores = read_benchmark("Balikatan_AllComm_SALSA_Hub_R.csv", ["node", "r_salsa_hub", "out_degree"])
			hub = salsa_centrality(edges; weighted = false, score = :hub)
			table = joined(hub, r_scores)
			difference = sum(abs.(Float64.(table[!, 2]) .- table.r_salsa_hub))
			@test record("SALSA hub (binary)", "R: stationary distribution of the hub chain", "summed absolute difference", difference, "below 1e-9", difference < 1e-9 && nrow(table) == nrow(r_scores))
		end

	#	Leiden Communities against ORA
		@testset "Leiden (ORA)" begin
			ora = read_benchmark("All_Comm_Lieden_Group_Assignments.csv", ["node", "leiden_group"])
			communities = leiden_community_detection(edges; nodes = nodes, resolution = 1.0, directed = true, weighted = true, seed = 42)
			names_of_nodes = communities.node_names isa DataFrame ? communities.node_names.id : communities.node_names
			table = joined(DataFrame(node = names_of_nodes, community = communities.membership), ora)
			ari = adjusted_rand_index(table.community, Int.(table.leiden_group))
			@test record("Leiden communities (weighted, resolution 1)", "ORA", "adjusted Rand index", ari, "at least 0.80 (the search is random; 0.958 was recorded in November 2025)", ari >= 0.80)
		end

	#	Modularity Vitality against Python and ORA
		@testset "Modularity vitality (Python, ORA)" begin
			#	Same partition as the Python run
				partition = read_benchmark("balikatan_all_comm_partition_leiden_gamma1.csv", ["node", "community"])
				python = read_benchmark("balikatan_all_comm_vitality_results_python_gamma1.csv", ["node", "vitality"])
				vitality = modularity_vitality(edges; nodes = nodes, directed = true, weighted = true, resolution_sweep = false,
											   provided_membership = partition)
				table = joined(vitality.results_df[:, 1:2], python)
				difference = largest_difference(table, "modularity_vitality", "vitality")
				@test record("modularity vitality (given partition)", "Python", "largest difference", difference, "below 1e-10", difference < 1e-10 && nrow(table) == n_nodes)

			#	Hub and bridge scores against ORA (each package finds its own partition)
				ora = read_benchmark("All_Comm_Modularity_Vitality_Scores.csv", ["node", "ora_bridge", "ora_hub"])
				own = modularity_vitality(edges; nodes = nodes, directed = false, weighted = true, resolution_sweep = false, resolution = 1.0, seed = 42)
				table = joined(own.results_df[:, [1, 3, 4]], ora)
				hub_rho = corspearman(Float64.(table.modularity_vitality_hub), Float64.(table.ora_hub))
				bridge_rho = corspearman(Float64.(table.modularity_vitality_bridge), Float64.(table.ora_bridge))
				@test record("modularity vitality: hub", "ORA", "Spearman rank correlation", hub_rho, "at least 0.97 (0.9916 when first compared)", hub_rho >= 0.97)
				@test record("modularity vitality: bridge", "ORA", "Spearman rank correlation", bridge_rho, "at least 0.97 (0.9950 when first compared)", bridge_rho >= 0.97)
		end

	#	Core Decomposition against igraph, ORA, and scoredec
		@testset "Core decomposition (igraph, ORA, scoredec)" begin
			#	k-core against igraph. The definitions differ: igraph counts parallel links and
			#	self-loops and its "all" is undirected, where this package follows ORA (links
			#	binarized, self-loops dropped, "total" = in + out). The difference is therefore
			#	not zero; it must stay what it was when the two were first compared.
				igraph = read_benchmark("Balikatan_AllComm_KCore_Scores.csv", ["node", "igraph_all", "igraph_out", "igraph_in"])
				for (mode, column, known) in (("total", "igraph_all", 33), ("out", "igraph_out", 43), ("in", "igraph_in", 245))
					table = joined(rename!(core_decomposition(edges; weighted = false, nodes = nodes, mode = mode), ["node", "core"]), igraph)
					difference = sum(abs.(Int.(table.core) .- Int.(table[!, column])))
					@test record("k-core ($mode)", "igraph (R)", "summed absolute difference", difference, "exactly $known (the known difference in definition)", difference == known && nrow(table) == n_nodes)
				end

			#	Undirected k-core against ORA
				ora = read_benchmark("Balikatan_AllComm_ORA_KCore_Scores.csv", ["node", "ora_k_core"])
				table = joined(rename!(core_decomposition(edges; weighted = false, nodes = nodes, mode = "undirected"), ["node", "core"]), ora)
				difference = sum(abs.(Int.(table.core) .- Int.(table.ora_k_core)))
				@test record("k-core (undirected)", "ORA", "summed absolute difference", difference, "exactly 0", difference == 0 && nrow(table) == n_nodes)

			#	s-core against scoredec
				scoredec = read_benchmark("Balikatan_AllComm_SCore_Scores.csv", ["node", "scoredec_total", "scoredec_out", "scoredec_in"])
				for mode in ("total", "out", "in")
					table = joined(rename!(core_decomposition(edges; weighted = true, nodes = nodes, mode = mode), ["node", "core"]), scoredec)
					difference = sum(abs.(Float64.(table.core) .- Float64.(table[!, "scoredec_" * mode])))
					@test record("s-core ($mode)", "scoredec (R)", "summed absolute difference", difference, "exactly 0", difference == 0 && nrow(table) == n_nodes)
				end
		end

	#	Two-Step Reach against igraph
		@testset "Two-step reach (igraph)" begin
			igraph = read_benchmark("Balikatan_AllComm_2Reach_Counts.csv", ["node", "in_reach_2", "out_reach_2", "undirected_reach_2"])
			for (mode, column) in (("in", "in_reach_2"), ("out", "out_reach_2"), ("all", "undirected_reach_2"))
				table = joined(rename!(hop_reach_k(edges; nodes = nodes, mode = mode, k = 2), ["node", "reach"]), igraph)
				difference = sum(abs.(Int.(table.reach) .- Int.(table[!, column])))
				@test record("two-step reach ($mode)", "igraph (R)", "summed absolute difference", difference, "exactly 0", difference == 0 && nrow(table) == n_nodes)
			end
		end

	#	Triad Census against Pajek
		@testset "Triad census (Pajek)" begin
			triad_types = ["003", "012", "102", "021D", "021U", "021C", "111D", "111U",
						   "030T", "030C", "201", "120D", "120U", "120C", "210", "300"]
			pajek = parse.(Float64, readlines(joinpath(TEST_DATA, "traid_census_observed.vec"))[2:end])
			census = triad_census(edges; weighted = false, graph_type = :directed, reciprocity_collapse = true)
			table = innerjoin(DataFrame(triad = triad_types, pajek = pajek), census, on = :triad)
			difference = sum(abs.(table.pajek .- Float64.(table.count)))
			@test record("triad census (reciprocity collapsed)", "Pajek", "summed absolute difference", difference, "exactly 0 over 16 types", difference == 0 && nrow(table) == 16)
		end
end
