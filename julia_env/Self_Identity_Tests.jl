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

	#	Network Type: Every Network Is Tested for Direction and Weight
		@testset "Network type is tested, not assumed" begin
			#	A network of 40 nodes with links that run one way and weights that vary
				rng = MersenneTwister(20261113)
				ids = ["n" * lpad(i, 2, "0") for i in 1:40]
				pairs = unique([(rand(rng, 1:40), rand(rng, 1:40)) for _ in 1:220])
				pairs = [(i, j) for (i, j) in pairs if i < j]
				one_way = DataFrame(src = [ids[i] for (i, j) in pairs], dst = [ids[j] for (i, j) in pairs],
									weight = Float64.(rand(rng, 1:6, length(pairs))))
				nodes = DataFrame(id = ids, label = ids)

			#	The same ties listed in both directions at the same weight: a symmetric network
				both_ways = vcat(one_way, DataFrame(src = one_way.dst, dst = one_way.src, weight = one_way.weight))

			#	A second symmetric network: the first with eight ties removed
				fewer = one_way[9:end, :]
				fewer_both_ways = vcat(fewer, DataFrame(src = fewer.dst, dst = fewer.src, weight = fewer.weight))

			#	The tests themselves
				@test LGS._found_network_type(both_ways, nodes).symmetric
				@test !LGS._found_network_type(one_way, nodes).symmetric
				@test !LGS._found_network_type(both_ways, nodes).binary
				@test LGS._found_network_type(select(one_way, :src, :dst), nodes).binary
				@test nrow(LGS._found_network_type(both_ways, nodes).undirected_edges) == nrow(one_way)

			#	A link whose reverse has another weight is not symmetric
				uneven = copy(both_ways)
				uneven.weight[1] += 1.0
				@test !LGS._found_network_type(uneven, nodes).symmetric

			#	Declared directed, both symmetric: compared as undirected
				declared_directed = network_comparator(both_ways, nodes, fewer_both_ways, nodes; directed = true, weighted = true, seed = 7)
				info = declared_directed.comparison_info
				value_of(item) = info.value[findfirst(==(item), info.item)]
				@test value_of("feature_set") == "one-mode undirected weighted"
				@test value_of("declared_type") == "directed weighted"
				@test value_of("found_type_1") == "symmetric, weights vary"
				@test occursin("compared as undirected", value_of("type_note"))

			#	The score is that of the same ties declared undirected, listed once
				declared_undirected = network_comparator(one_way, nodes, fewer, nodes; directed = false, weighted = true, seed = 7)
				@test declared_directed.overall_similarity == declared_undirected.overall_similarity
				@test 0.0 < declared_directed.overall_similarity < 1.0

			#	A symmetric network against itself scores exactly 1
				@test network_comparator(both_ways, nodes, both_ways, nodes; directed = true, weighted = true, seed = 7).overall_similarity == 1.0

			#	Only one network symmetric: the declaration stands, and the record says so
				mixed = network_comparator(both_ways, nodes, one_way, nodes; directed = true, weighted = true, seed = 7)
				mixed_info = mixed.comparison_info
				@test mixed_info.value[findfirst(==("feature_set"), mixed_info.item)] == "one-mode directed weighted"
				@test occursin("network 1 is symmetric and the other is not", mixed_info.value[findfirst(==("type_note"), mixed_info.item)])

			#	Declared weighted, every weight 1 in both: compared as binary
				ones_1 = DataFrame(src = one_way.src, dst = one_way.dst, weight = ones(nrow(one_way)))
				ones_2 = DataFrame(src = fewer.src, dst = fewer.dst, weight = ones(nrow(fewer)))
				declared_weighted = network_comparator(ones_1, nodes, ones_2, nodes; directed = true, weighted = true, seed = 7)
				weighted_info = declared_weighted.comparison_info
				@test weighted_info.value[findfirst(==("feature_set"), weighted_info.item)] == "one-mode directed binary"
				@test occursin("compared as binary", weighted_info.value[findfirst(==("type_note"), weighted_info.item)])
				declared_binary = network_comparator(ones_1, nodes, ones_2, nodes; directed = true, weighted = false, seed = 7)
				@test declared_weighted.overall_similarity == declared_binary.overall_similarity

			#	Declared binary on weights that vary: a choice, kept and recorded
				ignoring_weights = network_comparator(one_way, nodes, fewer, nodes; directed = true, weighted = false, seed = 7)
				ignoring_info = ignoring_weights.comparison_info
				@test ignoring_info.value[findfirst(==("feature_set"), ignoring_info.item)] == "one-mode directed binary"
				@test occursin("weights vary and are not used", ignoring_info.value[findfirst(==("type_note"), ignoring_info.item)])

			#	Declaration and links agree: nothing to note
				agreeing = network_comparator(one_way, nodes, fewer, nodes; directed = true, weighted = true, seed = 7)
				@test agreeing.comparison_info.value[findfirst(==("type_note"), agreeing.comparison_info.item)] == ""

			#	The same tests run in the app's analysis of one network
				mktempdir() do output_dir
					edge_path = joinpath(output_dir, "both_ways.csv")
					node_path = joinpath(output_dir, "nodes.csv")
					CSV.write(edge_path, both_ways)
					CSV.write(node_path, nodes)
					LGS.CLI.cli_main(["--mode", "analysis", "--edgelist-1", edge_path, "--nodelist-1", node_path, "--name-1", "symmetric",
									  "--output-dir", output_dir, "--directed", "true", "--weighted", "true", "--verbose", "false"])
					analysed = CSV.read(joinpath(output_dir, "symmetric_node_measures.csv"), DataFrame)
					@test "k_core_all" in names(analysed) && !("k_core_in" in names(analysed))
				end
		end

	#	Core Decompositions: One Feature per Decomposition
		@testset "Core numbers are compared as node measures" begin
			#	Two directed weighted networks that differ
				rng = MersenneTwister(20261114)
				ids = ["n" * lpad(i, 2, "0") for i in 1:50]
				arcs = unique([(rand(rng, 1:50), rand(rng, 1:50)) for _ in 1:400])
				arcs = [(i, j) for (i, j) in arcs if i != j]
				first_network = DataFrame(src = [ids[i] for (i, j) in arcs], dst = [ids[j] for (i, j) in arcs],
										  weight = Float64.(rand(rng, 1:5, length(arcs))))
				second_network = first_network[21:end, :]
				nodes = DataFrame(id = ids, label = ids)
				result = network_comparator(first_network, nodes, second_network, nodes; directed = true, weighted = true, seed = 7)
				features = result.combined_features

			#	One distribution feature per decomposition, and no feature per level
				core_rows = features[in.(features.type, Ref(["K-Core Decomposition", "S-Core Decomposition"])), :]
				@test Set(core_rows.measure) == Set("jsd_" .* ["k_core_in", "k_core_out", "k_core_undirected", "s_core_in", "s_core_out", "s_core_undirected"])
				@test !any(occursin.(r"_core_(in|out|undirected|all)_[0-9]", features.measure))

			#	They are in the diagnostics, on the scale of every other node measure
				diagnostics = result.node_measure_diagnostics
				@test issubset(["k_core_in", "s_core_undirected"], diagnostics.measure)
				@test all(abs.(core_rows.network_1_values .- core_rows.network_2_values) .<= sqrt(log(2)) + 1e-12)
				@test any(core_rows.network_1_values .!= core_rows.network_2_values)

			#	Only the triad census carries a group weight
				@test all(features.weight[features.type .!= "Triad Census"] .== 1.0)
				@test sum(features.weight[features.type .== "Triad Census"]) ≈ 1.0

			#	Undirected networks: one k-core and one s-core feature
				undirected = network_comparator(first_network, nodes, second_network, nodes; directed = false, weighted = true, seed = 7)
				undirected_cores = undirected.combined_features.measure[in.(undirected.combined_features.type, Ref(["K-Core Decomposition", "S-Core Decomposition"]))]
				@test Set(undirected_cores) == Set(["jsd_k_core_all", "jsd_s_core_all"])

			#	Two-mode networks: each mode's core numbers against its counterpart
				agents = ["a" * string(i) for i in 1:12]
				tags = ["h" * string(i) for i in 1:6]
				links = unique([(rand(rng, 1:12), rand(rng, 1:6)) for _ in 1:40])
				two_mode_1 = DataFrame(src = [agents[i] for (i, j) in links], dst = [tags[j] for (i, j) in links], weight = ones(length(links)))
				two_mode_2 = two_mode_1[5:end, :]
				mode_nodes = DataFrame(id = vcat(agents, tags), label = vcat(agents, tags), mode = vcat(fill("Agent", 12), fill("Hashtag", 6)))
				two_mode = network_comparator(two_mode_1, mode_nodes, two_mode_2, mode_nodes; weighted = false, bipartite = true)
				two_mode_cores = two_mode.combined_features.measure[two_mode.combined_features.type .== "K-Core Decomposition"]
				@test Set(two_mode_cores) == Set(["jsd_source_k_core", "jsd_target_k_core"])
				@test network_comparator(two_mode_1, mode_nodes, two_mode_1, mode_nodes; weighted = false, bipartite = true).overall_similarity == 1.0
		end

	#	Angular Similarity of the Links: Reported When the Two Networks Are on the Same Nodes
		@testset "Angular similarity of the links" begin
			#	The measure itself
				@test LGS._angular_similarity([1.0, 2.0, 3.0], [1.0, 2.0, 3.0]) == 1.0
				@test LGS._angular_similarity([1.0, 0.0], [0.0, 1.0]) == 0.0
				@test LGS._angular_similarity([1.0, 0.0], [1.0, 1.0]) ≈ 0.5
				@test LGS._angular_similarity([2.0, 4.0], [1.0, 2.0]) ≈ 1.0 atol = 1e-7
				@test isnan(LGS._angular_similarity([0.0, 0.0], [1.0, 1.0]))

			#	Two directed weighted networks on the same 40 nodes: the second lacks eight links and reweights three
				rng = MersenneTwister(20261115)
				ids = ["n" * lpad(i, 2, "0") for i in 1:40]
				arcs = unique([(rand(rng, 1:40), rand(rng, 1:40)) for _ in 1:260])
				arcs = [(i, j) for (i, j) in arcs if i != j]
				first_network = DataFrame(src = [ids[i] for (i, j) in arcs], dst = [ids[j] for (i, j) in arcs],
										  weight = Float64.(rand(rng, 1:5, length(arcs))))
				second_network = first_network[9:end, :]
				second_network.weight[1:3] .+= 1.0
				nodes = DataFrame(id = ids, label = ids)
				result = network_comparator(first_network, nodes, second_network, nodes; directed = true, weighted = true, seed = 7)
				agreement = result.edge_agreement
				value_of(item) = agreement.value[findfirst(==(item), agreement.item)]

			#	Computed, with the counts behind it
				@test value_of("computed") == "true"
				@test value_of("node_overlap") == "1.0"
				@test value_of("links_1_only") == "8" && value_of("links_2_only") == "0"
				@test value_of("links_both") == string(nrow(first_network) - 8)
				@test value_of("links_reweighted") == "3"

			#	The value is the angular similarity of the two aligned weight vectors
				x = first_network.weight
				y = vcat(zeros(8), second_network.weight)
				expected = 1.0 - 2.0 * acos(sum(x .* y) / sqrt(sum(abs2, x) * sum(abs2, y))) / pi
				@test result.edge_angular_similarity ≈ expected
				@test result.edge_angular_similarity_binary ≈ 1.0 - 2.0 * acos(sqrt((nrow(first_network) - 8) / nrow(first_network))) / pi
				@test 0.0 < result.edge_angular_similarity < 1.0

			#	A network against itself
				itself = network_comparator(first_network, nodes, first_network, nodes; directed = true, weighted = true, seed = 7)
				@test itself.edge_angular_similarity == 1.0 && itself.edge_angular_similarity_binary == 1.0

			#	It is not part of the score
				without = network_comparator(first_network, nodes, second_network, nodes; directed = true, weighted = true, seed = 7,
											 match_nodes_on = nothing)
				@test without.overall_similarity == result.overall_similarity
				@test isnan(without.edge_angular_similarity)
				@test occursin("switched off", without.edge_agreement.value[findfirst(==("note"), without.edge_agreement.item)])

			#	Below the node overlap threshold it is not computed, and the record says why
				half_network = first_network[in.(first_network.src, Ref(ids[1:24])) .& in.(first_network.dst, Ref(ids[1:24])), :]
				half_nodes = DataFrame(id = ids[1:24], label = ids[1:24])
				low = network_comparator(first_network, nodes, half_network, half_nodes; directed = true, weighted = true, seed = 7)
				low_value(item) = low.edge_agreement.value[findfirst(==(item), low.edge_agreement.item)]
				@test isnan(low.edge_angular_similarity)
				@test low_value("computed") == "false"
				@test low_value("node_overlap") == "0.6"
				@test occursin("below min_node_overlap = 0.75", low_value("note"))
				@test occursin("not computed", low.comparison_info.value[findfirst(==("edge_agreement_note"), low.comparison_info.item)])

			#	The threshold is an argument; a node in one network only has no links in the other
				lowered = network_comparator(first_network, nodes, half_network, half_nodes; directed = true, weighted = true, seed = 7,
											 min_node_overlap = 0.5)
				@test 0.0 < lowered.edge_angular_similarity < 1.0
				@test lowered.edge_agreement.value[findfirst(==("nodes_1_only"), lowered.edge_agreement.item)] == "16"
				@test_throws ArgumentError network_comparator(first_network, nodes, second_network, nodes; min_node_overlap = 1.5)

			#	Ids changed by a transformation: matched on a column that carries the identity
				new_id = Dict(id => "ext-" * string(1000 + i) for (i, id) in enumerate(ids))
				relabelled = DataFrame(src = [new_id[s] for s in second_network.src], dst = [new_id[d] for d in second_network.dst],
									   weight = second_network.weight)
				relabelled_nodes = DataFrame(id = [new_id[id] for id in ids], label = ids, original_id = ids)
				keyed_nodes = DataFrame(id = ids, label = ids, original_id = ids)
				by_id = network_comparator(first_network, keyed_nodes, relabelled, relabelled_nodes; directed = true, weighted = true, seed = 7)
				@test isnan(by_id.edge_angular_similarity)
				by_column = network_comparator(first_network, keyed_nodes, relabelled, relabelled_nodes; directed = true, weighted = true, seed = 7,
											   match_nodes_on = :original_id)
				@test by_column.edge_angular_similarity ≈ result.edge_angular_similarity
				@test by_column.edge_agreement.value[findfirst(==("links_1_only"), by_column.edge_agreement.item)] == "8"

			#	Two nodes with one identity, or a column that is absent, are errors
				clashing_nodes = copy(relabelled_nodes)
				clashing_nodes.original_id[2] = clashing_nodes.original_id[1]
				@test_throws ArgumentError network_comparator(first_network, keyed_nodes, relabelled, clashing_nodes; directed = true, weighted = true,
															  match_nodes_on = :original_id)
				@test_logs (:warn, r"not found") match_mode = :any network_comparator(first_network, nodes, second_network, nodes; directed = true, weighted = true, match_nodes_on = :no_such_column)

			#	A symmetric network gives the same value listed both ways or once
				pairs = [(i, j) for (i, j) in arcs if i < j]
				once = DataFrame(src = [ids[i] for (i, j) in pairs], dst = [ids[j] for (i, j) in pairs], weight = Float64.(rand(rng, 1:4, length(pairs))))
				once_fewer = once[6:end, :]
				twice = vcat(once, DataFrame(src = once.dst, dst = once.src, weight = once.weight))
				twice_fewer = vcat(once_fewer, DataFrame(src = once_fewer.dst, dst = once_fewer.src, weight = once_fewer.weight))
				listed_twice = network_comparator(twice, nodes, twice_fewer, nodes; directed = true, weighted = true, seed = 7)
				listed_once = network_comparator(once, nodes, once_fewer, nodes; directed = false, weighted = true, seed = 7)
				@test listed_twice.edge_angular_similarity ≈ listed_once.edge_angular_similarity
				@test listed_twice.edge_agreement.value[findfirst(==("links_1_only"), listed_twice.edge_agreement.item)] == "5"

			#	Two-mode networks: nodes of both modes are matched
				agents = ["a" * string(i) for i in 1:12]
				tags = ["h" * string(i) for i in 1:6]
				links = unique([(rand(rng, 1:12), rand(rng, 1:6)) for _ in 1:40])
				two_mode_1 = DataFrame(src = [agents[i] for (i, j) in links], dst = [tags[j] for (i, j) in links], weight = ones(length(links)))
				two_mode_2 = two_mode_1[4:end, :]
				mode_nodes = DataFrame(id = vcat(agents, tags), label = vcat(agents, tags), mode = vcat(fill("Agent", 12), fill("Hashtag", 6)))
				two_mode = network_comparator(two_mode_1, mode_nodes, two_mode_2, mode_nodes; weighted = false, bipartite = true)
				@test two_mode.edge_angular_similarity ≈ 1.0 - 2.0 * acos(sqrt((length(links) - 3) / length(links))) / pi

			#	The app writes both values with the score, and the agreement table; ids can be matched on a node-list column
				mktempdir() do output_dir
					paths = Dict(name => joinpath(output_dir, name * ".csv") for name in ("edges_1", "nodes_1", "edges_2", "nodes_2"))
					CSV.write(paths["edges_1"], first_network)
					CSV.write(paths["nodes_1"], keyed_nodes)
					CSV.write(paths["edges_2"], relabelled)
					CSV.write(paths["nodes_2"], relabelled_nodes)
					LGS.CLI.cli_main(["--mode", "comparison", "--edgelist-1", paths["edges_1"], "--nodelist-1", paths["nodes_1"], "--name-1", "first",
									  "--edgelist-2", paths["edges_2"], "--nodelist-2", paths["nodes_2"], "--name-2", "second",
									  "--output-dir", output_dir, "--directed", "true", "--weighted", "true", "--seed", "7",
									  "--match-nodes-on", "original_id", "--verbose", "false"])
					scores = CSV.read(joinpath(output_dir, "first_second_similarity_scores.csv"), DataFrame)
					@test scores.value[findfirst(==("edge_angular_similarity"), scores.metric)] ≈ result.edge_angular_similarity
					@test "edge_angular_similarity_binary" in scores.metric
					written = CSV.read(joinpath(output_dir, "first_second_edge_agreement.csv"), DataFrame; types = String)
					@test written.value[findfirst(==("computed"), written.item)] == "true"
				end
		end

		@testset "Reference network compared with many networks" begin
			#	A reference and three networks of its type, on other nodes
				rng = MersenneTwister(20261113)
				reference_ids = ["r" * string(i) for i in 1:30]
				reference_links = unique([(rand(rng, 1:30), rand(rng, 1:30)) for _ in 1:140])
				reference_edges = DataFrame(src = [reference_ids[i] for (i, j) in reference_links],
											dst = [reference_ids[j] for (i, j) in reference_links],
											weight = Float64.(rand(rng, 1:5, length(reference_links))))
				reference_nodes = DataFrame(id = reference_ids, label = reference_ids)
				others = Any[]
				for k in 1:3
					ids = ["n$(k)_" * string(i) for i in 1:(26 + 2k)]
					links = unique([(rand(rng, 1:length(ids)), rand(rng, 1:length(ids))) for _ in 1:(110 + 15k)])
					push!(others, (name = "network_$k",
								   edges = DataFrame(src = [ids[i] for (i, j) in links], dst = [ids[j] for (i, j) in links],
													 weight = Float64.(rand(rng, 1:5, length(links)))),
								   nodes = DataFrame(id = ids, label = ids)))
				end

			#	Each comparison equals the pairwise comparison, with the reference analysed once
				reference = reference_analysis(reference_edges, reference_nodes; directed = true, weighted = true, seed = 11)
				@test reference.label == "one-mode directed weighted"
				for other in others
					pairwise = network_comparator(reference_edges, reference_nodes, other.edges, other.nodes; directed = true, weighted = true, seed = 11)
					against = compare_to_reference(reference, other.edges, other.nodes)
					@test against.overall_similarity == pairwise.overall_similarity
					@test against.overall_distance == pairwise.overall_distance
					@test isequal(against.combined_features, pairwise.combined_features)
					@test isequal(against.node_measure_diagnostics, pairwise.node_measure_diagnostics)
					@test isnan(against.edge_angular_similarity)
				end

			#	A network on the reference's nodes gets the angular similarity as well
				perturbed = reference_edges[6:end, :]
				pairwise = network_comparator(reference_edges, reference_nodes, perturbed, reference_nodes; directed = true, weighted = true, seed = 11)
				against = compare_to_reference(reference, perturbed, reference_nodes)
				@test against.overall_similarity == pairwise.overall_similarity
				@test against.edge_angular_similarity == pairwise.edge_angular_similarity
				@test 0.0 < against.edge_angular_similarity < 1.0
				@test compare_to_reference(reference, reference_edges, reference_nodes).overall_similarity == 1.0

			#	Saved to disk and read back, the reference gives the same result
				mktempdir() do folder
					save_reference(reference, folder)
					for file in ("reference_settings.csv", "reference_global_stats.csv", "reference_triad_census.csv",
								 "reference_node_measures.csv", "reference_edges.csv", "reference_nodes.csv")
						@test isfile(joinpath(folder, file))
					end
					loaded = load_reference(folder)
					@test loaded.fingerprint == reference.fingerprint
					@test loaded.label == reference.label
					from_disk = compare_to_reference(loaded, others[1].edges, others[1].nodes)
					in_memory = compare_to_reference(reference, others[1].edges, others[1].nodes)
					@test from_disk.overall_similarity == in_memory.overall_similarity
					@test isequal(from_disk.combined_features, in_memory.combined_features)
				end

			#	A network of another type is refused: direction, weight, or mode
				symmetric_edges = vcat(others[1].edges, DataFrame(src = others[1].edges.dst, dst = others[1].edges.src, weight = others[1].edges.weight))
				symmetric_edges = combine(groupby(symmetric_edges, [:src, :dst]), :weight => maximum => :weight)
				for row in eachrow(symmetric_edges)
					mirror = findfirst((symmetric_edges.src .== row.dst) .& (symmetric_edges.dst .== row.src))
					symmetric_edges.weight[mirror] = row.weight
				end
				binary_edges = DataFrame(src = others[2].edges.src, dst = others[2].edges.dst, weight = ones(nrow(others[2].edges)))
				agents = ["a" * string(i) for i in 1:12]
				tags = ["h" * string(i) for i in 1:6]
				two_mode_links = unique([(rand(rng, 1:12), rand(rng, 1:6)) for _ in 1:40])
				two_mode_edges = DataFrame(src = [agents[i] for (i, j) in two_mode_links], dst = [tags[j] for (i, j) in two_mode_links],
										   weight = Float64.(rand(rng, 1:4, length(two_mode_links))))
				two_mode_nodes = DataFrame(id = vcat(agents, tags), label = vcat(agents, tags), mode = vcat(fill("Agent", 12), fill("Hashtag", 6)))
				@test_throws ArgumentError compare_to_reference(reference, symmetric_edges, nothing)
				@test_throws ArgumentError compare_to_reference(reference, binary_edges, others[2].nodes)
				@test_throws ArgumentError compare_to_reference(reference, two_mode_edges, two_mode_nodes; bipartite = true)

			#	A network may be given without a node table, or with a vector of ids
				with_table = compare_to_reference(reference, others[1].edges, others[1].nodes)
				@test compare_to_reference(reference, others[1].edges, nothing).overall_similarity == with_table.overall_similarity
				@test compare_to_reference(reference, others[1].edges, others[1].nodes.id).overall_similarity == with_table.overall_similarity
				@test network_comparator(reference_edges, nothing, others[1].edges, nothing; seed = 11).overall_similarity == with_table.overall_similarity
				no_table = reference_analysis(reference_edges, nothing; seed = 11)
				@test compare_to_reference(no_table, others[1].edges, nothing).overall_similarity == with_table.overall_similarity
				@test network_comparator(symmetric_edges, nothing, symmetric_edges, nothing; seed = 11).overall_similarity == 1.0

			#	The batch records a refusal or a failure and carries on
				broken = () -> error("file could not be read")
				networks = Any[(others[1].name, others[1].edges, others[1].nodes),
							   ("symmetric", symmetric_edges, nothing),
							   ("binary", binary_edges, others[2].nodes),
							   (name = "loaded_on_demand", load = () -> (others[3].edges, others[3].nodes)),
							   (name = "broken", load = broken),
							   ("same_nodes", perturbed, reference_nodes)]
				seen = String[]
				batch = reference_comparator(reference_edges, reference_nodes, networks; directed = true, weighted = true, seed = 11,
											 on_result = (name, result) -> push!(seen, name))
				@test batch.summary.name == ["network_1", "symmetric", "binary", "loaded_on_demand", "broken", "same_nodes"]
				@test batch.summary.status == ["compared", "refused", "refused", "compared", "failed", "compared"]
				@test seen == ["network_1", "loaded_on_demand", "same_nodes"]
				@test batch.reference_status == "computed"
				@test all(!isempty, batch.summary.reason[[2, 3, 5]])
				@test occursin("undirected", batch.summary.reason[2])
				@test occursin("binary", batch.summary.reason[3])
				@test batch.summary.similarity[1] == compare_to_reference(reference, others[1].edges, others[1].nodes).overall_similarity
				@test batch.summary.similarity[4] == compare_to_reference(reference, others[3].edges, others[3].nodes).overall_similarity
				@test all(isnan, batch.summary.similarity[[2, 3, 5]])
				@test isnan(batch.summary.edge_angular_similarity[1])
				@test batch.summary.edge_angular_similarity[6] == against.edge_angular_similarity
				@test !isempty(batch.summary.top_features[1])
				@test haskey(batch.results, "network_1") && !haskey(batch.results, "symmetric")

			#	A two-mode reference: same result as the pairwise comparison, and a one-mode network is refused
				other_links = unique([(rand(rng, 1:12), rand(rng, 1:6)) for _ in 1:45])
				other_two_mode = DataFrame(src = [agents[i] for (i, j) in other_links], dst = [tags[j] for (i, j) in other_links],
										   weight = Float64.(rand(rng, 1:4, length(other_links))))
				two_mode_reference = reference_analysis(two_mode_edges, two_mode_nodes; weighted = true, bipartite = true, seed = 11)
				pairwise = network_comparator(two_mode_edges, two_mode_nodes, other_two_mode, two_mode_nodes; weighted = true, bipartite = true, seed = 11)
				against_two_mode = compare_to_reference(two_mode_reference, other_two_mode, two_mode_nodes)
				@test against_two_mode.overall_similarity == pairwise.overall_similarity
				@test against_two_mode.edge_angular_similarity == pairwise.edge_angular_similarity
				@test_throws ArgumentError compare_to_reference(two_mode_reference, others[1].edges, others[1].nodes; bipartite = false)
				reversed = DataFrame(src = other_two_mode.dst, dst = other_two_mode.src, weight = other_two_mode.weight)
				@test_throws ArgumentError compare_to_reference(two_mode_reference, reversed, two_mode_nodes)

			#	A saved reference is used again only when the network, the settings, the seed, and the version are the same
				mktempdir() do folder
					small = Any[(others[1].name, others[1].edges, others[1].nodes)]
					first_run = reference_comparator(reference_edges, reference_nodes, small; reference_dir = folder, seed = 11)
					@test first_run.reference_status == "computed and saved"
					second_run = reference_comparator(reference_edges, reference_nodes, small; reference_dir = folder, seed = 11)
					@test startswith(second_run.reference_status, "reused")
					@test second_run.summary.similarity == first_run.summary.similarity
					other_seed = reference_comparator(reference_edges, reference_nodes, small; reference_dir = folder, seed = 12)
					@test startswith(other_seed.reference_status, "recomputed") && occursin("seed", other_seed.reference_status)
					other_network = reference_comparator(perturbed, reference_nodes, small; reference_dir = folder, seed = 12)
					@test startswith(other_network.reference_status, "recomputed") && occursin("fingerprint", other_network.reference_status)
					no_seed = reference_comparator(perturbed, reference_nodes, small; reference_dir = folder)
					@test startswith(no_seed.reference_status, "recomputed")
					again_no_seed = reference_comparator(perturbed, reference_nodes, small; reference_dir = folder)
					@test startswith(again_no_seed.reference_status, "recomputed") && occursin("seed", again_no_seed.reference_status)
				end

			#	The app: a table of networks, one without a node list; the reference is reused on the second run
				mktempdir() do output_dir
					data_dir = joinpath(output_dir, "data")
					mkpath(data_dir)
					CSV.write(joinpath(data_dir, "reference_edges.csv"), reference_edges)
					CSV.write(joinpath(data_dir, "reference_nodes.csv"), reference_nodes)
					CSV.write(joinpath(data_dir, "one_edges.csv"), others[1].edges)
					CSV.write(joinpath(data_dir, "one_nodes.csv"), others[1].nodes)
					CSV.write(joinpath(data_dir, "two_edges.csv"), others[2].edges)
					CSV.write(joinpath(data_dir, "binary_edges.csv"), binary_edges)
					CSV.write(joinpath(data_dir, "networks.csv"),
							  DataFrame(name = ["one", "two", "binary", "absent"],
										edgelist = ["one_edges.csv", "two_edges.csv", "binary_edges.csv", "no_such_file.csv"],
										nodelist = ["one_nodes.csv", "", "", ""]))
					args = ["--mode", "reference", "--edgelist-1", joinpath(data_dir, "reference_edges.csv"),
							"--nodelist-1", joinpath(data_dir, "reference_nodes.csv"), "--name-1", "target",
							"--network-table", joinpath(data_dir, "networks.csv"), "--output-dir", joinpath(output_dir, "run"),
							"--directed", "true", "--weighted", "true", "--seed", "11", "--verbose", "false"]
					LGS.CLI.cli_main(args)
					run_dir = joinpath(output_dir, "run")
					written = CSV.read(joinpath(run_dir, "reference_comparison_summary.csv"), DataFrame)
					@test written.name == ["one", "two", "binary", "absent"]
					@test written.status == ["compared", "compared", "refused", "failed"]
					@test written.similarity[1] ≈ compare_to_reference(reference, others[1].edges, others[1].nodes).overall_similarity
					@test written.similarity[2] ≈ compare_to_reference(reference, others[2].edges, nothing).overall_similarity
					@test isfile(joinpath(run_dir, "reference", "reference_node_measures.csv"))
					@test isfile(joinpath(run_dir, "one", "target_one_similarity_scores.csv"))
					@test isfile(joinpath(run_dir, "one", "target_one_weight_scale.csv"))
					@test isfile(joinpath(run_dir, "two", "two_node_measures.csv"))
					@test !isdir(joinpath(run_dir, "binary"))
					info = CSV.read(joinpath(run_dir, "reference_comparison_info.csv"), DataFrame; types = String)
					@test info.value[findfirst(==("reference_statistics"), info.item)] == "computed and saved"
					LGS.CLI.cli_main(args)
					info = CSV.read(joinpath(run_dir, "reference_comparison_info.csv"), DataFrame; types = String)
					@test startswith(info.value[findfirst(==("reference_statistics"), info.item)], "reused")
					@test CSV.read(joinpath(run_dir, "reference_comparison_summary.csv"), DataFrame).similarity[1:2] == written.similarity[1:2]
					@test_throws ArgumentError LGS.CLI.cli_main(vcat(args, ["--edgelist-2", joinpath(data_dir, "one_edges.csv")]))
				end
		end

		@testset "Weight scale: one heavy link or the distribution of weights" begin
			#	Two directed weighted networks that differ in one link: weight 2 in one, 9 in the other
				rng = MersenneTwister(20261116)
				ids = ["w" * lpad(i, 2, "0") for i in 1:30]
				arcs = [(i, j) for (i, j) in unique([(rand(rng, 1:30), rand(rng, 1:30)) for _ in 1:160]) if i != j]
				weights = Float64.(rand(rng, 1:3, length(arcs)))
				weights[1] = 4.0
				base = DataFrame(src = [ids[i] for (i, j) in arcs], dst = [ids[j] for (i, j) in arcs], weight = weights)
				heavy = copy(base)
				heavy.weight[2] = 9.0
				nodes = DataFrame(id = ids, label = ids)
				result = network_comparator(base, nodes, heavy, nodes; directed = true, weighted = true, seed = 5)
				scale = Dict(row.statistic => row for row in eachrow(result.weight_scale))
				@test scale["largest_weight"].network_1 == 4.0 && scale["largest_weight"].network_2 == 9.0
				@test scale["largest_weight"].ratio == 2.25
				@test scale["second_largest_weight"].network_2 == 4.0
				@test scale["links_at_largest_weight"].network_2 == 1.0
				@test scale["median"].ratio == 1.0
				@test scale["links"].network_1 == length(arcs)

			#	The weights themselves hardly differ; divided by the largest weight they differ a great deal
				@test scale["sqrt_jsd_weights"].network_1 < 0.1
				@test scale["sqrt_jsd_weights_over_largest"].network_1 > 5.0 * scale["sqrt_jsd_weights"].network_1

			#	The measures divided by the largest weight are marked, and the note names them
				diagnostics = result.node_measure_diagnostics
				@test sort(diagnostics.measure[diagnostics.divided_by_largest_weight]) == ["in_degree_normalized", "out_degree_normalized"]
				note = result.comparison_info.value[findfirst(==("weight_scale_note"), result.comparison_info.item)]
				@test occursin("the largest weights differ (4.0 and 9.0, ratio 2.25)", note)
				@test occursin("in_degree_normalized", note)

			#	Equal largest weights are said to be equal; the same network has no divergence
				same = network_comparator(base, nodes, base, nodes; directed = true, weighted = true, seed = 5)
				same_scale = Dict(row.statistic => row for row in eachrow(same.weight_scale))
				@test same_scale["sqrt_jsd_weights"].network_1 == 0.0 && same_scale["sqrt_jsd_weights_over_largest"].network_1 == 0.0
				@test startswith(same.comparison_info.value[findfirst(==("weight_scale_note"), same.comparison_info.item)], "the largest weights are equal")

			#	Undirected: total degree is the marked measure, and self-links do not set the divisor
				pairs = unique([(min(i, j), max(i, j)) for (i, j) in arcs])
				undirected_1 = DataFrame(src = [ids[i] for (i, j) in pairs], dst = [ids[j] for (i, j) in pairs], weight = Float64.(rand(rng, 1:3, length(pairs))))
				undirected_2 = vcat(undirected_1, DataFrame(src = ["w01"], dst = ["w01"], weight = [50.0]))
				undirected = network_comparator(undirected_1, nodes, undirected_2, nodes; directed = false, weighted = true, seed = 5)
				@test undirected.weight_scale.ratio[1] == 1.0
				undirected_diagnostics = undirected.node_measure_diagnostics
				@test undirected_diagnostics.measure[undirected_diagnostics.divided_by_largest_weight] == ["total_degree_normalized"]

			#	Binary networks have no weight scale
				binary = network_comparator(base, nodes, heavy, nodes; directed = true, weighted = false, seed = 5)
				@test nrow(binary.weight_scale) == 0
				@test !any(binary.node_measure_diagnostics.divided_by_largest_weight)
				@test occursin("binary", binary.comparison_info.value[findfirst(==("weight_scale_note"), binary.comparison_info.item)])

			#	The score does not use any of it, and a comparison with a reference reports the same table
				reference = reference_analysis(base, nodes; directed = true, weighted = true, seed = 5)
				against = compare_to_reference(reference, heavy, nodes)
				@test against.overall_similarity == result.overall_similarity
				@test isequal(against.weight_scale, result.weight_scale)
				@test isequal(against.node_measure_diagnostics, result.node_measure_diagnostics)
				batch = reference_comparator(base, nodes, Any[("heavy", heavy, nodes)]; directed = true, weighted = true, seed = 5)
				@test batch.summary.weight_scale_note[1] == note
		end

		@testset "Reference comparison on observed networks" begin
			#	Reference: Balikatan 2022. Compared: Pac Rim 2023 (day 1) and Pac Sentry 2025 (day 6), synthetic networks on other nodes.
			#	Each is the Agent x Agent - All Communication network of its ORA file.
				test_data = dirname(BALIKATAN_ORA_PATH)
				balikatan_2022 = load_ora_xml(BALIKATAN_ORA_PATH)
				reference_nodes = balikatan_2022.nodesets["Agent"][:, 1:2]
				rename!(reference_nodes, ["id", "label"])
				reference_arcs = balikatan_2022.networks["Agent x Agent - All Communication"].edges
				reference = reference_analysis(reference_arcs, reference_nodes; directed = true, weighted = true, seed = 11)
				@test reference.label == "one-mode directed weighted"

			#	Saved and read back, as a batch run uses it
				saved_dir = mktempdir()
				save_reference(reference, saved_dir)
				loaded = load_reference(saved_dir)

			#	Each comparison with the reference equals the comparison of the pair
				for file in ("Pac Rim Day 1.xml", "PAO_Day_6.xml")
					path = joinpath(test_data, file)
					if !isfile(path)
						#	Recorded as broken, so that a missing file is seen in the summary
							@test_skip isfile(path)
							continue
					end
					meta_network = load_ora_xml(path)
					nodes = meta_network.nodesets["Agent"][:, 1:2]
					rename!(nodes, ["id", "label"])
					arcs = meta_network.networks["Agent x Agent - All Communication"].edges
					pairwise = network_comparator(reference_arcs, reference_nodes, arcs, nodes; directed = true, weighted = true, seed = 11)
					for against in (compare_to_reference(reference, arcs, nodes), compare_to_reference(loaded, arcs, nodes))
						@test against.overall_similarity == pairwise.overall_similarity
						@test isequal(against.combined_features, pairwise.combined_features)
						@test isequal(against.type_contributions, pairwise.type_contributions)
						@test isequal(against.node_measure_diagnostics, pairwise.node_measure_diagnostics)
						@test isequal(against.undefined_features, pairwise.undefined_features)
						@test isequal(against.weight_scale, pairwise.weight_scale)
						@test isnan(against.edge_angular_similarity)
					end
					@test 0.0 < pairwise.overall_similarity < 1.0
				end
				rm(saved_dir; recursive = true)
		end

		@testset "Node key: ids by default, names when asked" begin
			#	One network under two id schemes; the label is the same in both. A third copy has neither in common.
				rng = MersenneTwister(20261117)
				labels = ["person_" * lpad(i, 2, "0") for i in 1:36]
				arcs = [(i, j) for (i, j) in unique([(rand(rng, 1:36), rand(rng, 1:36)) for _ in 1:220]) if i != j]
				weights = Float64.(rand(rng, 1:4, length(arcs)))
				order = shuffle(rng, 1:36)
				ids_a = ["a" * lpad(i, 3, "0") for i in 1:36]
				ids_b = ["b" * lpad(order[i], 3, "0") for i in 1:36]
				edges_a = DataFrame(src = [ids_a[i] for (i, j) in arcs], dst = [ids_a[j] for (i, j) in arcs], weight = weights)
				edges_b = DataFrame(src = [ids_b[i] for (i, j) in arcs], dst = [ids_b[j] for (i, j) in arcs], weight = weights)
				nodes_a = DataFrame(id = ids_a, label = labels)
				nodes_b = DataFrame(id = ids_b, label = labels)

			#	With the name as the key the two copies are one network: the score is exactly 1 and so is the angular similarity
				by_name = network_comparator(edges_a, nodes_a, edges_b, nodes_b; directed = true, weighted = true, seed = 3, match_nodes_on = :name)
				@test by_name.overall_similarity == 1.0
				@test by_name.edge_angular_similarity == 1.0
				@test by_name.comparison_info.value[findfirst(==("node_key"), by_name.comparison_info.item)] == "label"
				@test network_comparator(edges_a, nodes_a, edges_b, nodes_b; seed = 3, match_nodes_on = :label).overall_similarity == 1.0

			#	Node measures come back under the ids the caller gave
				@test sort(string.(by_name.node_measures_1.node)) == sort(ids_a)
				@test sort(string.(by_name.node_measures_2.node)) == sort(ids_b)

			#	By default ids are the key: the two copies share no node, so there is no angular similarity; the score is still computed
				by_id = network_comparator(edges_a, nodes_a, edges_b, nodes_b; directed = true, weighted = true, seed = 3)
				@test isnan(by_id.edge_angular_similarity)
				@test 0.0 < by_id.overall_similarity <= 1.0
				@test by_id.comparison_info.value[findfirst(==("node_key"), by_id.comparison_info.item)] == "id"

			#	A name that is asked for and absent is reported, and ids are used: the result is that of the default
				bare_b = DataFrame(id = ids_b)
				fallback = @test_logs (:warn, r"not found in network 2") match_mode = :any network_comparator(edges_a, nodes_a, edges_b, bare_b; directed = true, weighted = true, seed = 3, match_nodes_on = :name)
				@test fallback.overall_similarity == by_id.overall_similarity
				@test occursin("was asked for and not found in network 2", fallback.comparison_info.value[findfirst(==("node_key"), fallback.comparison_info.item)])

			#	Two nodes with one name are an error; a node without a name keeps its id
				duplicated = copy(nodes_b)
				duplicated.label[2] = duplicated.label[1]
				@test_throws ArgumentError network_comparator(edges_a, nodes_a, edges_b, duplicated; seed = 3, match_nodes_on = :name)
				blank_a, blank_b = copy(nodes_a), copy(nodes_b)
				blank_a.label[5] = ""
				blank_b.label[5] = ""
				blank = network_comparator(edges_a, blank_a, edges_b, blank_b; directed = true, weighted = true, seed = 3, match_nodes_on = :name)
				@test 35 / 37 - 1e-9 <= parse(Float64, blank.edge_agreement.value[findfirst(==("node_overlap"), blank.edge_agreement.item)]) <= 35 / 37 + 1e-9

			#	A membership given by node id follows the key
				membership = DataFrame(node = ids_a, community = [1 + (i % 3) for i in 1:36])
				membership_b = DataFrame(node = ids_b, community = [1 + (i % 3) for i in 1:36])
				with_membership = network_comparator(edges_a, nodes_a, edges_b, nodes_b; seed = 3, match_nodes_on = :name,
													 provided_membership_1 = membership, provided_membership_2 = membership_b)
				@test with_membership.overall_similarity == 1.0

			#	Graphs on different nodes need no key: nothing is asked for and nothing is reported
				other = DataFrame(src = ["x" * string(i) for (i, j) in arcs[1:150]], dst = ["x" * string(j) for (i, j) in arcs[1:150]], weight = weights[1:150])
				structural = @test_logs min_level = Base.CoreLogging.Warn network_comparator(edges_a, nodes_a, other, nothing; directed = true, weighted = true, seed = 3)
				@test 0.0 < structural.overall_similarity < 1.0

			#	A reference fixes the key; a network without the field is identified by id and still compared
				reference = reference_analysis(edges_a, nodes_a; directed = true, weighted = true, seed = 3, match_nodes_on = :name)
				against = compare_to_reference(reference, edges_b, nodes_b)
				@test against.overall_similarity == 1.0 && against.edge_angular_similarity == 1.0
				@test sort(string.(against.node_measures_1.node)) == sort(ids_a)
				@test isequal(against.combined_features, by_name.combined_features)
				without_field = @test_logs (:warn,) match_mode = :any compare_to_reference(reference, edges_b, bare_b)
				@test isnan(without_field.edge_angular_similarity) && 0.0 < without_field.overall_similarity <= 1.0
				mktempdir() do folder
					first_run = reference_comparator(edges_a, nodes_a, Any[("copy", edges_b, nodes_b)]; reference_dir = folder, seed = 3, match_nodes_on = :name)
					@test first_run.summary.similarity == [1.0]
					@test startswith(reference_comparator(edges_a, nodes_a, Any[("copy", edges_b, nodes_b)]; reference_dir = folder, seed = 3, match_nodes_on = :name).reference_status, "reused")
					other_key = reference_comparator(edges_a, nodes_a, Any[("copy", edges_b, nodes_b)]; reference_dir = folder, seed = 3)
					@test startswith(other_key.reference_status, "recomputed") && occursin("node_key", other_key.reference_status)
				end

			#	The app: --match-nodes-on name; a field that is not in the file is reported and ids are used
				mktempdir() do output_dir
					paths = Dict(name => joinpath(output_dir, name * ".csv") for name in ("edges_a", "nodes_a", "edges_b", "nodes_b"))
					CSV.write(paths["edges_a"], edges_a); CSV.write(paths["nodes_a"], nodes_a)
					CSV.write(paths["edges_b"], edges_b); CSV.write(paths["nodes_b"], nodes_b)
					common = ["--mode", "comparison", "--edgelist-1", paths["edges_a"], "--nodelist-1", paths["nodes_a"], "--name-1", "a",
							  "--edgelist-2", paths["edges_b"], "--nodelist-2", paths["nodes_b"], "--name-2", "b",
							  "--directed", "true", "--weighted", "true", "--seed", "3", "--verbose", "false"]
					LGS.CLI.cli_main(vcat(common, ["--output-dir", joinpath(output_dir, "by_name"), "--match-nodes-on", "name"]))
					scores = CSV.read(joinpath(output_dir, "by_name", "a_b_similarity_scores.csv"), DataFrame)
					@test scores.value[findfirst(==("overall_similarity"), scores.metric)] == 1.0
					LGS.CLI.cli_main(vcat(common, ["--output-dir", joinpath(output_dir, "absent"), "--match-nodes-on", "handle"]))
					info = CSV.read(joinpath(output_dir, "absent", "a_b_comparison_info.csv"), DataFrame; types = String)
					@test startswith(info.value[findfirst(==("node_key"), info.item)], "id (handle was asked for and not found")
				end
		end
end
