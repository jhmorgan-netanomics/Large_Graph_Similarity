#Regression Tests for the Comparator: Self-Identity and Two-Mode Networks
#11 November 2026
#
#	These tests accompany the repairs recorded in LGS_Pending_Fixes_28Oct2026.md.
#	Each test set names the finding it guards. The tests call the functions of the
#	Large_Graph_Similarity module, not the working copies that Comparator_Tests.jl
#	defines for development.
#
#	Run from the Large_Graph_Similarity folder:
#		julia -t auto --project=julia_env julia_env/Self_Identity_Tests.jl

################
#   PACKAGES   #
################
using CSV
using DataFrames
using LinearAlgebra
using Random
using SparseArrays
using Test
using Large_Graph_Similarity

const LGS = Large_Graph_Similarity

#################
#   FUNCTIONS   #
#################

#	Helper Function for the Tests: Identical or Jointly Undefined Features
	function features_identical(result::NamedTuple)
		"""
		Args:
			result::NamedTuple: output of network_comparator
		Returns:
			Bool: true when every feature is equal in both networks or NaN in both
		Notes:
			Signed zeros count as equal.
		"""

		#	Compare the Two Value Columns
			values_1 = result.combined_features.network_1_values
			values_2 = result.combined_features.network_2_values

		#	Assembling Result
			return all((values_1 .== values_2) .| (isnan.(values_1) .& isnan.(values_2)))
	end

#	Helper Function for the Tests: Directed Ring
	function ring_edges(n::Int)
		"""
		Args:
			n::Int: number of nodes
		Returns:
			DataFrame: edge list of a directed ring with :src and :dst
		Notes:
			Every node has in-degree 1 and out-degree 1.
		"""

		#	Build Node Ids
			ids = [string("n", lpad(i, 3, '0')) for i in 1:n]

		#	Assembling Result
			return DataFrame(src = ids, dst = circshift(ids, -1))
	end

#	Helper Function for the Tests: Read One Score from the CLI Output
	function cli_score(output_dir::AbstractString, pair_prefix::AbstractString, metric::AbstractString)
		"""
		Args:
			output_dir::AbstractString: CLI output directory
			pair_prefix::AbstractString: "<name_1>_<name_2>"
			metric::AbstractString: row of the similarity scores table
		Returns:
			Float64: the score
		Notes:
			Reads <pair_prefix>_similarity_scores.csv.
		"""

		#	Read the Scores Table
			scores = CSV.read(joinpath(output_dir, "$(pair_prefix)_similarity_scores.csv"), DataFrame)

		#	Assembling Result
			return scores.value[findfirst(==(metric), scores.metric)]
	end

#	Helper Function for the Tests: Look Up a Global Statistic
	function stat_value(global_stats::DataFrame, measure::AbstractString)
		"""
		Args:
			global_stats::DataFrame: table with :measure and :value
			measure::AbstractString: name of the statistic
		Returns:
			String: the stored value
		Notes:
			Values are stored as strings.
		"""

		#	Assembling Result
			return global_stats.value[findfirst(==(measure), global_stats.measure)]
	end

#	Helper Function for the Tests: Look Up a Node Measure
	function node_value(node_stats::DataFrame, id::AbstractString, column::Symbol)
		"""
		Args:
			node_stats::DataFrame: node-level measures with :node
			id::AbstractString: node id
			column::Symbol: measure wanted
		Returns:
			Union{Missing,Float64}: the node's value
		Notes:
			A two-mode measure is missing for nodes of the other mode.
		"""

		#	Assembling Result
			return node_stats[findfirst(==(id), node_stats.node), column]
	end

#	Helper Function for the Tests: Weight of One Projected Tie
	function tie_weight(projection::NamedTuple, a::AbstractString, b::AbstractString)
		"""
		Args:
			projection::NamedTuple: output of project_bipartite
			a::AbstractString: first node id
			b::AbstractString: second node id
		Returns:
			Float64: weight of the tie a-b
		Notes:
			Ties are listed once, with the smaller id first.
		"""

		#	Assembling Result
			return projection.edges.weight[findfirst((projection.edges.src .== a) .& (projection.edges.dst .== b))]
	end

#	Helper Function for the Tests: SALSA by Power Iteration
	function salsa_by_iteration(edges::DataFrame; score::Symbol = :hub, weighted::Bool = false)
		"""
		Args:
			edges::DataFrame: edge list with :src, :dst, optional :weight
			score::Symbol: :hub or :authority
			weighted::Bool: use link weights
		Returns:
			Dict{String,Float64}: node id => stationary probability of its chain
		Notes:
			An independent check on salsa_centrality, which uses the closed form.
			Iterates x' = x' P from the uniform distribution; self-loops are removed.
		"""

		#	Build the Matrix as salsa_centrality Does
			clean = LGS._aggregate_multi_edges(edges; agg_func = weighted ? sum : maximum)
			A, _, node_names = LGS._edgelist_to_sparse_matrix(clean; weighted = weighted && hasproperty(clean, :weight))
			A = SparseMatrixCSC{Float64,Int}(A)
			n = size(A, 1)
			for i in 1:n
				A[i, i] = 0.0
			end
			dropzeros!(A)

		#	Degrees (zero degrees guarded)
			out_degree = vec(sum(A, dims = 2))
			in_degree = vec(sum(A, dims = 1))
			out_degree[out_degree .== 0] .= 1.0
			in_degree[in_degree .== 0] .= 1.0

		#	Power Iteration on the Left
			x = fill(1.0 / n, n)
			for _ in 1:200_000
				y = score == :hub ? A * ((A' * (x ./ out_degree)) ./ in_degree) : A' * ((A * (x ./ in_degree)) ./ out_degree)
				y ./= sum(y)
				settled = sum(abs, y .- x) < 1e-14
				x = y
				settled && break
			end

		#	Assembling Result
			return Dict(string(name) => value for (name, value) in zip(node_names, x))
	end

#	Helper Function for the Tests: SALSA Scores by Node Id
	function salsa_scores(edges::DataFrame; score::Symbol = :hub, weighted::Bool = false)
		"""
		Args:
			edges::DataFrame: edge list with :src, :dst, optional :weight
			score::Symbol: :hub or :authority
			weighted::Bool: use link weights
		Returns:
			Dict{String,Float64}: node id => salsa_centrality score
		Notes:
			Wraps salsa_centrality for lookups by id.
		"""

		#	Assembling Result
			result = salsa_centrality(edges; score = score, weighted = weighted)
			return Dict(string(node) => value for (node, value) in zip(result.node, result[:, 2]))
	end

#	Helper Function for the Tests: Two-Mode Network from an ORA File
	function two_mode_network(metanet::NamedTuple, network_name::AbstractString)
		"""
		Args:
			metanet::NamedTuple: output of load_ora_xml
			network_name::AbstractString: name of a two-mode network
		Returns:
			Tuple{DataFrame,DataFrame}: (edges, nodes), where nodes has :id, :label, :mode
		Notes:
			The mode of a node is the name of its nodeset.
		"""

		#	Isolate the Network and Its Nodesets
			network = metanet.networks[network_name]
			sources = metanet.nodesets[network.sourceNodeset][:, 1:2]
			targets = metanet.nodesets[network.targetNodeset][:, 1:2]

		#	Label Modes
			rename!(sources, ["id", "label"])
			rename!(targets, ["id", "label"])
			sources.mode .= network.sourceNodeset
			targets.mode .= network.targetNodeset

		#	Assembling Result
			return network.edges, vcat(sources, targets)
	end

#################
#   TEST DATA   #
#################

#	Balikatan 2022, Agent x Agent - All Communication (3,146 arcs, 41 of them self-loops)
	const TEST_DATA_DIR = joinpath(@__DIR__, "..", "Test_Data")
	const BALIKATAN_ARCS_PATH = joinpath(TEST_DATA_DIR, "Balikatan_2022_All_Communication_Arcs.csv")
	const BALIKATAN_NODES_PATH = joinpath(TEST_DATA_DIR, "Balikatan_2022_All_Communication_Nodes.csv")
	const BALIKATAN_ORA_PATH = joinpath(TEST_DATA_DIR, "Balikatan_2022_Processed.xml")

	balikatan_arcs = LGS.CLI._load_edge_list(BALIKATAN_ARCS_PATH; verbose = false)
	balikatan_nodes = LGS.CLI._load_node_list(BALIKATAN_NODES_PATH; verbose = false)

#	The Same Network without Its Self-Loops (the case that exposed finding 6)
	balikatan_noloops = filter(row -> row.src != row.dst, balikatan_arcs)

#	Balikatan's Two-Mode Networks
	balikatan_ora = load_ora_xml(BALIKATAN_ORA_PATH)
	hashtag_edges, hashtag_nodes = two_mode_network(balikatan_ora, "Agent x Hashtag")
	sender_edges, sender_nodes = two_mode_network(balikatan_ora, "Agent x Tweet - Sender")
	mentions_edges, mentions_nodes = two_mode_network(balikatan_ora, "Tweet x Agent - Mentions")

#	An Observed One-Mode Hashtag Network to Compare a Projection With
	co_occurrence_edges = balikatan_ora.networks["Hashtag x Hashtag - Co-Occurrence"].edges
	co_occurrence_nodes = balikatan_ora.nodesets["Hashtag"][:, 1:2]
	rename!(co_occurrence_nodes, ["id", "label"])

#	A Small Two-Mode Network with Known Answers: h1 is used by a1, a2, a3; h2 by a1, a2
	toy_edges = DataFrame(src = ["a1", "a2", "a3", "a1", "a2"], dst = ["h1", "h1", "h1", "h2", "h2"])
	toy_nodes = DataFrame(id = ["a1", "a2", "a3", "h1", "h2"], label = ["a1", "a2", "a3", "h1", "h2"],
						  mode = ["Agent", "Agent", "Agent", "Hashtag", "Hashtag"])

#	The Four One-Mode Graph Types
	const GRAPH_TYPES = [(directed = true,  weighted = true),
						 (directed = true,  weighted = false),
						 (directed = false, weighted = true),
						 (directed = false, weighted = false)]

#############
#   TESTS   #
#############

@testset "Large_Graph_Similarity regression tests" begin

	#	Finding 6: Link Statistics Are Read by Name
		@testset "Finding 6: link statistics parser" begin
			#	Defined statistics
				defined = "(count = 41, min = 1.0, max = 12.0, mean = 2.5, std = 1.25e-1, sum = 102.5)"
				@test LGS._parse_link_stats(defined) == [41.0, 1.0, 12.0, 2.5, 0.125, 102.5]

			#	Undefined statistics stay NaN
				no_loops = "(count = 0, min = NaN, max = NaN, mean = NaN, std = NaN, sum = 0.0)"
				parsed = LGS._parse_link_stats(no_loops)
				@test parsed[1] == 0.0
				@test all(isnan, parsed[2:5])
				@test parsed[6] == 0.0

			#	Order in the string does not matter
				reordered = "(sum = 6.0, std = 0.0, mean = 2.0, max = 2.0, min = 2.0, count = 3)"
				@test LGS._parse_link_stats(reordered) == [3.0, 2.0, 2.0, 2.0, 0.0, 6.0]
		end

	#	Finding 1: PageRank Is a Function of the Network Alone
		@testset "Finding 1: PageRank is reproducible" begin
			first_run = pagerank_stitched(balikatan_arcs; mode = :in, weighted = true, stitch_by = :nodes)
			second_run = pagerank_stitched(balikatan_arcs; mode = :in, weighted = true, stitch_by = :nodes)
			@test first_run.scores == second_run.scores
			@test first_run.converged
		end

	#	SALSA: Stationary Distribution of the Hub and Authority Chains (Lempel & Moran, 2001)
		@testset "SALSA centrality" begin
			#	Hubs that share a successor split their component by out-degree
				shared = DataFrame(src = ["a", "a", "a", "b"], dst = ["x", "y", "z", "x"])
				hubs = salsa_scores(shared; score = :hub)
				authorities = salsa_scores(shared; score = :authority)
				@test hubs["a"] ≈ 0.75 && hubs["b"] ≈ 0.25
				@test hubs["x"] == 0.0
				@test authorities["x"] ≈ 0.5 && authorities["y"] ≈ 0.25 && authorities["z"] ≈ 0.25
				@test authorities["a"] == 0.0

			#	Two components: degree share, times the component's share of the nodes (Boldi & Vigna, 2014)
				stars = DataFrame(src = ["C1", "C1", "C1", "C2"], dst = ["A", "B", "C", "D"])
				hubs = salsa_scores(stars; score = :hub)
				authorities = salsa_scores(stars; score = :authority)
				@test hubs["C1"] ≈ 0.5 && hubs["C2"] ≈ 0.5
				@test all(authorities[id] ≈ 0.25 for id in ["A", "B", "C", "D"])

			#	Weights: strength takes the place of degree
				weighted_edges = DataFrame(src = ["a", "b"], dst = ["x", "x"], weight = [3.0, 1.0])
				@test salsa_scores(weighted_edges; score = :hub, weighted = true)["a"] ≈ 0.75
				@test salsa_scores(weighted_edges; score = :hub, weighted = false)["a"] ≈ 0.5

			#	Regular cases stay uniform (the cases of test_salsa in Graph_Comparison_Tests.jl)
				cycle = DataFrame(src = ["1", "2", "3", "4", "5", "2", "3", "4", "5", "1"],
								  dst = ["2", "3", "4", "5", "1", "1", "2", "3", "4", "5"])
				@test all(value ≈ 0.2 for value in values(salsa_scores(cycle; score = :hub)))
				@test all(value ≈ 0.2 for value in values(salsa_scores(cycle; score = :authority)))

			#	Balikatan: the closed form equals the stationary distribution found by iteration
				for weighted in (false, true), score in (:hub, :authority)
					closed_form = salsa_scores(balikatan_arcs; score = score, weighted = weighted)
					iterated = salsa_by_iteration(balikatan_arcs; score = score, weighted = weighted)
					@test maximum(abs(closed_form[id] - iterated[id]) for id in keys(iterated)) < 1e-9
					@test sum(values(closed_form)) ≈ 1.0
					@test length(unique(round.(collect(values(closed_form)); digits = 12))) > 10
				end
		end

	#	Finding 2: The Leiden Partition Ignores Row Order
		@testset "Finding 2: Leiden ignores row order" begin
			forward = leiden_community_detection(balikatan_arcs; nodes = balikatan_nodes, weighted = true,
												 directed = true, show_progress = false)
			backward = leiden_community_detection(reverse(balikatan_arcs); nodes = reverse(balikatan_nodes),
												  weighted = true, directed = true, show_progress = false)
			forward_map = Dict(zip(String.(forward.node_names.id), forward.membership))
			backward_map = Dict(zip(String.(backward.node_names.id), backward.membership))
			@test forward_map == backward_map
			@test forward.modularity == backward.modularity
		end

	#	Regression Test 1: Balikatan All Communication against Itself
		@testset "Balikatan against itself" begin
			balikatan_self = network_comparator(balikatan_arcs, balikatan_nodes, balikatan_arcs, balikatan_nodes;
												directed = true, weighted = true, resolution = 1.0)
			@test balikatan_self.overall_similarity == 1.0
			@test features_identical(balikatan_self)
			@test nrow(balikatan_self.undefined_features) == 0
			@test balikatan_self.comparison_info.value[1] == "one-mode directed weighted"
		end

	#	Regression Test 2: Without Self-Loops, for Every Graph Type (finding 6)
		@testset "Balikatan without self-loops against itself" begin
			for graph_type in GRAPH_TYPES
				#	Compare the network with itself
					noloops_self = network_comparator(balikatan_noloops, balikatan_nodes, balikatan_noloops, balikatan_nodes;
													  directed = graph_type.directed, weighted = graph_type.weighted,
													  resolution = 1.0)
					@test noloops_self.overall_similarity == 1.0
					@test features_identical(noloops_self)

				#	The self-loop mean and std are undefined on both sides, and reported
					undefined = noloops_self.undefined_features
					@test Set(undefined.measure) == Set(["self_loops_mean", "self_loops_std"])
					@test all(==("both"), undefined.undefined_in)
			end
		end

	#	Regression Test 3: The Same Network Listed in Another Order (finding 2)
		@testset "Row order does not change the score" begin
			#	Reversed rows
				reversed = network_comparator(balikatan_arcs, balikatan_nodes,
											  reverse(balikatan_arcs), reverse(balikatan_nodes);
											  directed = true, weighted = true, resolution = 1.0)
				@test reversed.overall_similarity == 1.0

			#	Shuffled rows
				rng = MersenneTwister(20261111)
				shuffled = network_comparator(balikatan_arcs, balikatan_nodes,
											  balikatan_arcs[shuffle(rng, 1:nrow(balikatan_arcs)), :],
											  balikatan_nodes[shuffle(rng, 1:nrow(balikatan_nodes)), :];
											  directed = true, weighted = true, resolution = 1.0)
				@test shuffled.overall_similarity == 1.0
		end

	#	The Resolution Sweep (CHAMP) Holds the Same Property
		@testset "Resolution sweep against itself" begin
			sweep_self = network_comparator(balikatan_noloops, balikatan_nodes,
											reverse(balikatan_noloops), reverse(balikatan_nodes);
											directed = true, weighted = true, resolution_sweep = true, seed = 42)
			@test sweep_self.overall_similarity == 1.0
		end

	#	Regression Test 4: Two Copies through the CLI (the path ION_Comparator uses)
		@testset "Two copies through the CLI" begin
			mktempdir() do output_dir
				#	Run the comparison
					args = ["--mode", "comparison",
							"--edgelist-1", BALIKATAN_ARCS_PATH, "--nodelist-1", BALIKATAN_NODES_PATH, "--name-1", "copy_a",
							"--edgelist-2", BALIKATAN_ARCS_PATH, "--nodelist-2", BALIKATAN_NODES_PATH, "--name-2", "copy_b",
							"--output-dir", output_dir,
							"--directed", "true", "--weighted", "true", "--resolution", "1.0",
							"--verbose", "false"]
					LGS.CLI.cli_main(args)

				#	Check the written scores
					@test cli_score(output_dir, "copy_a_copy_b", "overall_similarity") == 1.0
					@test isfile(joinpath(output_dir, "copy_a_copy_b_undefined_features.csv"))
					@test isfile(joinpath(output_dir, "copy_a_copy_b_comparison_info.csv"))
			end
		end

	#	Regression Test 5: A Measure Defined in One Network Only (finding 5)
		@testset "Finding 5: a one-sided undefined measure" begin
			#	A ring has no degree variance; two chords give it some
				ring = ring_edges(40)
				ring_with_chords = vcat(ring, DataFrame(src = ["n001", "n001"], dst = ["n011", "n021"]))
				ring_nodes = DataFrame(id = ring.src, label = ring.src)

			#	Compare
				one_sided = network_comparator(ring, ring_nodes, ring_with_chords, ring_nodes;
											   directed = true, weighted = false, resolution = 1.0)
				@test isfinite(one_sided.overall_similarity)
				@test 0.0 < one_sided.overall_similarity < 1.0
				@test all(isfinite, one_sided.type_contributions.distance)

			#	Degree assortativity is reported as undefined in the ring
				undefined = one_sided.undefined_features
				assortativity_row = findfirst(==("degree assortativity"), undefined.measure)
				@test assortativity_row !== nothing
				@test undefined.undefined_in[assortativity_row] == "network_1"
		end

	#	Finding 4: Two-Mode Measures on a Network with Known Answers
		@testset "Finding 4: two-mode measures" begin
			#	Statistics of the toy network
				global_stats, triads, node_stats = bipartite_constructor(toy_edges, toy_nodes; weighted = false)
				@test stat_value(global_stats, "source_mode") == "Agent"
				@test stat_value(global_stats, "target_mode") == "Hashtag"
				@test stat_value(global_stats, "num_backward_edges") == "0"
				@test parse(Float64, stat_value(global_stats, "forward_density")) ≈ 5 / 6 atol = 1e-6
				@test nrow(triads) == 0

			#	One four-cycle (a1-h1-a2-h2) among six three-paths
				@test parse(Float64, stat_value(global_stats, "four_cycle_clustering")) ≈ 4 / 6 atol = 1e-6

			#	Degrees are normalized by the size of the other mode
				@test node_value(node_stats, "a1", :source_out_degree_normalized) == 1.0
				@test node_value(node_stats, "a3", :source_out_degree_normalized) == 0.5
				@test node_value(node_stats, "h1", :target_in_degree_normalized) == 1.0
				@test ismissing(node_value(node_stats, "a1", :target_in_degree_normalized))

			#	Latapy's coefficient: a3 overlaps a1 and a2 by 1/2 each
				@test node_value(node_stats, "a3", :source_pairwise_clustering) ≈ 0.5
				@test node_value(node_stats, "a1", :source_pairwise_clustering) ≈ 0.75
				@test node_value(node_stats, "a3", :source_two_step_reach_normalized) == 1.0

			#	The feature set has no triad, community, or transitivity features
				features = bipartite_feature_builder(global_stats, triads, node_stats)
				@test !("Triad Census" in features.type)
				@test !("Community Structure" in features.type)
				@test !any(occursin.("transitivity", features.measure))
				@test !any(occursin.("backward", features.measure))
		end

	#	Finding 4: Declaring the Modes
		@testset "Finding 4: mode validation" begin
			#	A link within a mode
				within_edges = vcat(toy_edges, DataFrame(src = ["a1"], dst = ["a2"]))
				@test_throws ArgumentError bipartite_constructor(within_edges, toy_nodes)

			#	An id listed in both modes
				shared_nodes = vcat(toy_nodes, DataFrame(id = ["a1"], label = ["a1"], mode = ["Hashtag"]))
				@test_throws ArgumentError bipartite_constructor(toy_edges, shared_nodes)

			#	Modes inferred from direction when no node sends and receives
				inferred_stats, _, _ = bipartite_constructor(toy_edges, nothing; weighted = false)
				@test stat_value(inferred_stats, "source_mode") == "source"

			#	A one-mode network is not accepted as two-mode
				@test_throws ArgumentError bipartite_constructor(balikatan_arcs, nothing)
		end

	#	Regression Test 6: A Two-Mode Network against Itself (finding 4)
		@testset "Two-mode networks against themselves" begin
			for weighted in (true, false)
				#	Agent x Hashtag, second copy with shuffled rows
					rng = MersenneTwister(4)
					hashtag_self = network_comparator(hashtag_edges, hashtag_nodes,
													  hashtag_edges[shuffle(rng, 1:nrow(hashtag_edges)), :], reverse(hashtag_nodes);
													  bipartite = true, weighted = weighted)
					@test hashtag_self.overall_similarity == 1.0
					@test features_identical(hashtag_self)
					@test !("Triad Census" in hashtag_self.combined_features.type)
					@test !("Community Structure" in hashtag_self.combined_features.type)
					@test hashtag_self.comparison_info.value[1] == "two-mode " * (weighted ? "weighted" : "binary")
			end

			#	Agent x Tweet - Sender: every tweet has one sender
				sender_self = network_comparator(sender_edges, sender_nodes, sender_edges, sender_nodes;
												 bipartite = true, weighted = true)
				@test sender_self.overall_similarity == 1.0
				@test Set(sender_self.undefined_features.measure) == Set(["degree assortativity", "four_cycle_clustering"])
				@test all(==("both"), sender_self.undefined_features.undefined_in)
		end

	#	Finding 4: Links in Both Directions
		@testset "Finding 4: links in both directions" begin
			#	Add two links from hashtags back to agents
				two_way_edges = vcat(toy_edges, DataFrame(src = ["h1", "h2"], dst = ["a1", "a3"]))
				two_way = network_comparator(two_way_edges, toy_nodes, reverse(two_way_edges), toy_nodes;
											 bipartite = true, weighted = false)
				@test two_way.overall_similarity == 1.0
				@test "reciprocity" in two_way.combined_features.measure
				@test "backward_density" in two_way.combined_features.measure

			#	Against the one-way network both share one feature set, and differ
				mixed_direction = network_comparator(toy_edges, toy_nodes, two_way_edges, toy_nodes;
													 bipartite = true, weighted = false)
				@test 0.0 < mixed_direction.overall_similarity < 1.0
				@test "reciprocity" in mixed_direction.combined_features.measure
		end

	#	Finding 4: Comparisons That Are Refused
		@testset "Finding 4: refused comparisons" begin
			#	Two-mode against one-mode
				@test_throws ArgumentError network_comparator(hashtag_edges, hashtag_nodes, balikatan_arcs, balikatan_nodes;
															  bipartite = (true, false))
				@test_throws ArgumentError network_comparator(hashtag_edges, hashtag_nodes, balikatan_arcs, balikatan_nodes;
															  bipartite = true)

			#	Different modes
				@test_throws ArgumentError network_comparator(hashtag_edges, hashtag_nodes, sender_edges, sender_nodes;
															  bipartite = true)

			#	Same modes, opposite directions
				@test_throws ArgumentError network_comparator(sender_edges, sender_nodes, mentions_edges, mentions_nodes;
															  bipartite = true)

			#	Different modes are compared role by role on request
				by_role = network_comparator(hashtag_edges, hashtag_nodes, sender_edges, sender_nodes;
											 bipartite = true, match_modes = false)
				@test 0.0 < by_role.overall_similarity < 1.0
		end

	#	Finding 4: Projection
		@testset "Finding 4: projection" begin
			#	Known weights on the toy network
				by_count = project_bipartite(toy_edges, toy_nodes; onto = "Agent", weighting = :count)
				by_newman = project_bipartite(toy_edges, toy_nodes; onto = "source", weighting = :newman)
				by_jaccard = project_bipartite(toy_edges, toy_nodes; onto = "agent", weighting = :jaccard)
				@test by_count.n_links == 3
				@test tie_weight(by_count, "a1", "a2") == 2.0
				@test tie_weight(by_count, "a1", "a3") == 1.0
				@test tie_weight(by_newman, "a1", "a2") ≈ 1.5
				@test tie_weight(by_newman, "a2", "a3") ≈ 0.5
				@test tie_weight(by_jaccard, "a1", "a2") ≈ 1.0
				@test tie_weight(by_jaccard, "a1", "a3") ≈ 0.5
				@test by_count.onto == "Agent" && by_count.dropped == "Hashtag"

			#	Guards
				@test_throws ArgumentError project_bipartite(toy_edges, toy_nodes; onto = "Tweet")
				@test_throws ArgumentError project_bipartite(toy_edges, toy_nodes; onto = "Agent", max_pairs = 1)

			#	No two agents share a tweet, so that projection is empty and says so
				empty_projection = @test_logs (:warn,) project_bipartite(sender_edges, sender_nodes; onto = "Agent")
				@test empty_projection.n_links == 0

			#	A projection can be compared with an observed one-mode network
				hashtags_by_agent = project_bipartite(hashtag_edges, hashtag_nodes; onto = "Hashtag")
				projected = network_comparator(hashtags_by_agent.edges, hashtags_by_agent.nodes, co_occurrence_edges, co_occurrence_nodes;
											   directed = false, weighted = true, resolution = 1.0)
				@test 0.0 < projected.overall_similarity < 1.0
		end

	#	Finding 4: Two-Mode Networks through the CLI
		@testset "Finding 4: two-mode networks through the CLI" begin
			mktempdir() do output_dir
				#	Two copies of Agent x Hashtag, recognised from the ORA nodesets
					ora_args = ["--mode", "comparison",
								"--ora-xml-1", BALIKATAN_ORA_PATH, "--network-name-1", "Agent x Hashtag", "--name-1", "tags_a",
								"--ora-xml-2", BALIKATAN_ORA_PATH, "--network-name-2", "Agent x Hashtag", "--name-2", "tags_b",
								"--output-dir", output_dir, "--weighted", "true", "--verbose", "false"]
					LGS.CLI.cli_main(ora_args)
					@test cli_score(output_dir, "tags_a_tags_b", "overall_similarity") == 1.0
					info = CSV.read(joinpath(output_dir, "tags_a_tags_b_comparison_info.csv"), DataFrame)
					@test info.value[findfirst(==("feature_set"), info.item)] == "two-mode weighted"

				#	Two-mode against one-mode is refused
					mixed_args = ["--mode", "comparison",
								  "--ora-xml-1", BALIKATAN_ORA_PATH, "--network-name-1", "Agent x Hashtag", "--name-1", "tags",
								  "--ora-xml-2", BALIKATAN_ORA_PATH, "--network-name-2", "Hashtag x Hashtag - Co-Occurrence", "--name-2", "cooc",
								  "--output-dir", output_dir, "--verbose", "false"]
					@test_throws ArgumentError LGS.CLI.cli_main(mixed_args)

				#	The same comparison goes through once a projection is asked for
					LGS.CLI.cli_main(vcat(mixed_args, ["--project-onto", "Hashtag"]))
					@test 0.0 < cli_score(output_dir, "tags_cooc", "overall_similarity") < 1.0
					info = CSV.read(joinpath(output_dir, "tags_cooc_comparison_info.csv"), DataFrame)
					@test info.value[findfirst(==("feature_set"), info.item)] == "one-mode undirected weighted"
					@test info.value[findfirst(==("projection_1_onto"), info.item)] == "Hashtag"
			end
		end
	#	Node-Level Distributions: Geometric Axis, Fixed Bin Width, Linear Binning
		@testset "Node measure comparison" begin
			#	Linear binning shares each value between the two nearest bin centres
				@test LGS._linear_bin_masses([0.5, 1.5], 0.0, 1.0, 2) == [0.5, 0.5]
				@test LGS._linear_bin_masses([1.0], 0.0, 1.0, 2) == [0.5, 0.5]
				@test LGS._linear_bin_masses([0.75], 0.0, 1.0, 2) == [0.75, 0.25]
				@test sum(LGS._linear_bin_masses(collect(0.0:0.01:3.0), 0.0, 0.25, 12)) ≈ 1.0

			#	The largest value is counted
				@test LGS._linear_bin_masses([0.0, 2.0], 0.0, 1.0, 2) == [0.5, 0.5]

			#	Jensen-Shannon divergence: 0 for identical, log(2) for disjoint
				@test LGS._jensen_shannon_divergence([0.25, 0.75], [0.25, 0.75]) == 0.0
				@test LGS._jensen_shannon_divergence([1.0, 0.0], [0.0, 1.0]) ≈ log(2)

			#	The typical value weights the two networks equally
				@test LGS._pooled_typical_value([1.0, 1.0, 1.0, 1.0], [5.0, 0.0]) == 1.0
				@test LGS._pooled_typical_value([1.0], [5.0, 5.0, 5.0, 0.0]) == 1.0
				@test LGS._pooled_typical_value([1.0, 0.0], [5.0]) == 5.0
				@test LGS._pooled_typical_value([0.0, 0.0], [0.0]) == 0.0

			#	Scaled Wasserstein-1 distance: a shift of every value by one
				@test LGS._scaled_wasserstein([1.0, 2.0, 3.0], [2.0, 3.0, 4.0]) ≈ 1.0 / 5.0
				@test LGS._scaled_wasserstein([1.0, 2.0], [1.0, 2.0]) == 0.0
				@test LGS._scaled_wasserstein([0.0, 0.0], [1.0, 1.0]) ≈ 1.0

			#	Rescaling both networks' values leaves the divergence unchanged
				rng = MersenneTwister(20261112)
				x = exp.(randn(rng, 400))
				y = exp.(1.2 .* randn(rng, 300))
				base = LGS._distribution_divergence(x, y)
				scaled = LGS._distribution_divergence(1000.0 .* x, 1000.0 .* y)
				@test scaled.js_divergence ≈ base.js_divergence
				@test scaled.n_bins == base.n_bins
				@test 0.0 < base.js_divergence <= log(2)

			#	A change far smaller than a bin moves the divergence by a like amount
				nudged = copy(x)
				nudged[1] += 1e-9
				@test LGS._distribution_divergence(x, nudged).js_divergence < 1e-12

			#	A heavy tail does not hide a difference among the typical nodes
				body = vcat(fill(1.0, 500), fill(2.0, 500), [1.0e6])
				shifted_body = vcat(fill(1.0, 300), fill(2.0, 700), [1.0e6])
				@test sqrt(LGS._distribution_divergence(body, shifted_body).js_divergence) > 0.1

			#	The bin width sets the number of bins
				@test LGS._distribution_divergence(x, y; bin_width = 0.0625).n_bins >= 2 * base.n_bins - 1

			#	A measure that is zero everywhere carries no difference
				@test LGS._distribution_divergence(zeros(5), zeros(7)).js_divergence == 0.0

			#	The comparator returns one score, with its binning on record
				noloops_pair = network_comparator(balikatan_arcs, balikatan_nodes, balikatan_noloops, balikatan_nodes;
												  directed = true, weighted = true, resolution = 1.0)
				@test 0.0 < noloops_pair.overall_similarity < 1.0
				@test noloops_pair.overall_similarity == 1.0 / (1.0 + noloops_pair.overall_distance)
				@test !hasproperty(noloops_pair, :overall_similarity_raw)
				info = noloops_pair.comparison_info
				@test info.value[findfirst(==("bin_width"), info.item)] == "0.125"
				@test info.value[findfirst(==("lgs_version"), info.item)] == LGS.LGS_VERSION

			#	One distribution feature and one diagnostic row per node measure
				features = noloops_pair.combined_features
				distribution_features = features.measure[startswith.(features.measure, "jsd_")]
				diagnostics = noloops_pair.node_measure_diagnostics
				@test !any(startswith.(features.measure, "jsd_raw_")) && !any(startswith.(features.measure, "jsd_asinh_"))
				@test Set(distribution_features) == Set("jsd_" .* diagnostics.measure)
				@test length(distribution_features) == nrow(diagnostics)
				@test all(0.0 .<= diagnostics.wasserstein .<= 1.0)
				@test all(diagnostics.expected_noise .>= 0.0)
				for row in eachrow(diagnostics)
					feature_row = findfirst(==("jsd_" * row.measure), features.measure)
					@test features.network_1_values[feature_row] - features.network_2_values[feature_row] ≈ sqrt(row.js_divergence)
				end

			#	The bin width is an argument, and must be positive
				finer = network_comparator(balikatan_arcs, balikatan_nodes, balikatan_noloops, balikatan_nodes;
										   directed = true, weighted = true, resolution = 1.0, bin_width = 0.0625)
				@test finer.comparison_info.value[findfirst(==("bin_width"), finer.comparison_info.item)] == "0.0625"
				@test finer.overall_similarity != noloops_pair.overall_similarity
				@test_throws ArgumentError network_comparator(balikatan_arcs, balikatan_nodes, balikatan_arcs, balikatan_nodes;
															  directed = true, weighted = true, bin_width = 0.0)

			#	The CLI writes one set of files and takes the bin width
				mktempdir() do output_dir
					args = ["--mode", "comparison",
							"--edgelist-1", BALIKATAN_ARCS_PATH, "--nodelist-1", BALIKATAN_NODES_PATH, "--name-1", "copy_a",
							"--edgelist-2", BALIKATAN_ARCS_PATH, "--nodelist-2", BALIKATAN_NODES_PATH, "--name-2", "copy_b",
							"--output-dir", output_dir, "--directed", "true", "--weighted", "true",
							"--bin-width", "0.25", "--verbose", "false"]
					LGS.CLI.cli_main(args)
					for suffix in ["comparison", "similarity_scores", "type_contributions", "node_measure_diagnostics"]
						@test isfile(joinpath(output_dir, "copy_a_copy_b_$(suffix).csv"))
					end
					@test !isfile(joinpath(output_dir, "copy_a_copy_b_comparison_raw.csv"))
					info = CSV.read(joinpath(output_dir, "copy_a_copy_b_comparison_info.csv"), DataFrame; types = String)
					@test info.value[findfirst(==("bin_width"), info.item)] == "0.25"
				end
		end

	#	Weighted Triad Census: Peak Position within the Threshold Grid
		@testset "Triad census peak position" begin
			#	The position runs from 0 at the lowest threshold to 1 at the highest, on the log scale
				@test LGS._relative_peak_position(1.0, 1.0, 100.0, 0.2, "102") == 0.0
				@test LGS._relative_peak_position(100.0, 1.0, 100.0, 0.2, "102") == 1.0
				@test LGS._relative_peak_position(10.0, 1.0, 100.0, 0.2, "102") ≈ 0.5

			#	It does not depend on the unit of the weights
				@test LGS._relative_peak_position(10.0, 1.0, 100.0, 0.2, "102") ≈ LGS._relative_peak_position(5000.0, 500.0, 50000.0, 0.2, "102")

			#	Undefined for the empty triad, a type that never occurs, and a single-threshold grid
				@test isnan(LGS._relative_peak_position(100.0, 1.0, 100.0, 0.9, "003"))
				@test isnan(LGS._relative_peak_position(1.0, 1.0, 100.0, 0.0, "030C"))
				@test isnan(LGS._relative_peak_position(1.0, 1.0, 1.0, 0.2, "102"))

			#	Balikatan against a copy with every link weight multiplied by 1000
				rescaled_arcs = copy(balikatan_arcs)
				rescaled_arcs[!, 3] = 1000.0 .* Float64.(rescaled_arcs[!, 3])
				rescaled = network_comparator(balikatan_arcs, balikatan_nodes, rescaled_arcs, balikatan_nodes;
											  directed = true, weighted = true, resolution = 1.0)

			#	The census table keeps the peak threshold and adds the position
				census = rescaled.triad_census_counts_1
				@test hasproperty(census, :peak_tau) && hasproperty(census, :peak_position)
				@test isnan(census.peak_position[findfirst(==("003"), census.triad)])
				@test all(isnan.(census.peak_position[census.peak_density .== 0.0]))
				defined = census.peak_position[.!isnan.(census.peak_position)]
				@test !isempty(defined) && all(0.0 .<= defined .<= 1.0)

			#	The score uses the position, not the threshold, and has no position for the empty triad
				features = rescaled.combined_features
				@test !any(endswith.(features.measure, "_peak_tau"))
				@test count(endswith.(features.measure, "_peak_position")) == 15
				@test !("003_peak_position" in features.measure)

			#	Every triad feature is on a bounded scale
				triad_rows = features[features.type .== "Triad Census", :]
				@test all(isfinite.(triad_rows.network_1_values))
				@test all(endswith.(triad_rows.measure, "_AUMC_density") .| (0.0 .<= triad_rows.network_1_values .<= 1.0))

			#	Rescaling every link weight leaves the triad features unchanged
				rescaled_triads = rescaled.combined_features[rescaled.combined_features.type .== "Triad Census", :]
				position_rows = endswith.(rescaled_triads.measure, "_peak_position")
				@test rescaled_triads.network_1_values[position_rows] ≈ rescaled_triads.network_2_values[position_rows]

			#	A position that is undefined adds nothing and is not reported as undefined
				@test !any(endswith.(rescaled.undefined_features.measure, "_peak_position"))
		end
end
