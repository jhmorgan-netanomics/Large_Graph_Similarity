#Test Script for the Large Graph Comparison Functions
#Jonathan H. Morgan
#22 October 2025

#   Pulling-In BEND_2022 & Activating Local Environment
    cd("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity")
    using Pkg
    Pkg.activate("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/julia_env")
    Pkg.status()

################
#   PACKAGES   #
################

#   Precompile Packages
#   Pkg.precompile()

#   Load Packages
    using CSV
    using DataFrames
	using LinearAlgebra
	using SparseArrays
    using Large_Graph_Similarity

######################
#   TEST FUNCTIONS   #
######################

#   Helper Function: Import Checks
    function summarize_ora(out)
		#	Developer Notes
			#	Simple console summary for a loaded ORA MetaNetwork.

		#	Nodesets
			println("=== Node Sets ===")
			for k in sort(collect(keys(out.nodesets)))
				df = out.nodesets[k]
				println(" • ", rpad(k, 10), ": ", nrow(df), " rows, ", ncol(df), " cols")
				if ncol(df) > 1
					println("    first cols: ", join(Symbol.(names(df)[1:min(6, ncol(df))]), ", "))
				end
			end

		#	Networks
			println("\n=== Networks ===")
			println("Total: ", length(out.networks))
			for (nid, nt) in sort(collect(out.networks); by = x -> x[1])
				edges = nt.edges
				println(" • ", nid, "  [", nt.sourceNodeset, " → ", nt.targetNodeset, "]  ",
				        nrow(edges), " links; directed=", nt.isDirected,
				        "; binary=", nt.isBinary, "; allowSelfLoops=", nt.allowSelfLoops,
				        "; hadMissingWeights=", nt.hadMissingWeights)
			end

		#	Basic invariants
			println("\n=== Checks ===")
			req = Set(["Agent","Tweet","Hashtag","URL"])
			have = Set(collect(keys(out.nodesets)))
			println("Nodesets present: ", join(sort(collect(have)), ", "))
			missing = setdiff(req, have)
			println("Missing required nodesets: ", isempty(missing) ? "none" : join(collect(missing), ", "))

		#	Done
			return nothing
	end

#	Helper Function for transitivity_netstat: build adjacency from edges
	function _build_undirected_adjacency(edges::DataFrame; drop_self_loops::Bool=true)
		"""
		Args:
			edges::DataFrame: edge list with src and dst
			drop_self_loops::Bool: remove self-loops
		Returns:
			Tuple{SparseMatrixCSC, Vector{Int}}: adjacency matrix and node list
		Notes:
			Creates symmetric binary adjacency for undirected graph
		"""
		
		#	Extract edges and filter self-loops if needed
			if drop_self_loops
				mask = edges.src .!= edges.dst
				src = edges.src[mask]
				dst = edges.dst[mask]
			else
				src = edges.src
				dst = edges.dst
			end
		
		#	Get unique nodes and create mapping
			all_nodes = unique(vcat(src, dst))
			n = length(all_nodes)
			node_to_idx = Dict(all_nodes[i] => i for i in 1:n)
		
		#	Map edges to indices
			src_idx = [node_to_idx[s] for s in src]
			dst_idx = [node_to_idx[d] for d in dst]
		
		#	Build symmetric binary adjacency
			A = sparse(vcat(src_idx, dst_idx), vcat(dst_idx, src_idx), 
			          ones(2*length(src_idx)), n, n)
			A = min.(A, 1.0)  # Ensure binary
		
		#	Return adjacency and node list
			return A, all_nodes
	end

#	Helper Function for transitivity_netstat: count 2-paths and triangles explicitly
	function _count_triples_explicit(A::SparseMatrixCSC)
		"""
		Args:
			A::SparseMatrixCSC: binary symmetric adjacency
		Returns:
			Tuple{Int, Int}: (closed_triples, total_triples)
		Notes:
			Explicitly counts ordered triples following NetStat definition
		"""
		
		#	Initialize counters
			n = size(A, 1)
			total_triples = 0
			closed_triples = 0
		
		#	Iterate over all possible middle nodes j
			for j in 1:n
				#	Get neighbors of j
					neighbors_j = findall(x -> x > 0, A[:, j])
					k_j = length(neighbors_j)
				
				#	Count all ordered pairs of neighbors (forms 2-paths through j)
					for idx_i in 1:k_j
						i = neighbors_j[idx_i]
						for idx_k in 1:k_j
							if idx_i != idx_k  # Ensure i != k
								k = neighbors_j[idx_k]
								#	This is a 2-path: i-j-k
									total_triples += 1
								#	Check if it's closed (i-k edge exists)
									if A[i, k] > 0
										closed_triples += 1
									end
							end
						end
					end
			end
		
		#	Return counts
			return closed_triples, total_triples
	end

#	NetStat-compatible transitivity calculation
	function transitivity_netstat(edges::DataFrame; drop_self_loops::Bool=true, verbose::Bool=false)
		"""
		Args:
			edges::DataFrame: edge list with src and dst columns
			drop_self_loops::Bool: remove self-loops (default=true)
			verbose::Bool: print diagnostic information (default=false)
		Returns:
			Float64: transitivity coefficient
		Notes:
			Direct implementation of NetStat specification for debugging
		"""
		
		#	Build undirected binary adjacency
			A, nodes = _build_undirected_adjacency(edges; drop_self_loops=drop_self_loops)
		
		#	Count triples explicitly
			closed, total = _count_triples_explicit(A)
		
		#	Compute additional diagnostics if verbose
			if verbose
				#	Count triangles and edges
					n = size(A, 1)
					num_edges = nnz(A) ÷ 2  # Divide by 2 for undirected
					
				#	Count triangles via matrix multiplication
					A_dense = Matrix(A)
					tri_6 = sum((A_dense * A_dense) .* A_dense)
					num_triangles = tri_6 ÷ 6
					
				#	Alternative 2-path count via degrees
					degrees = vec(sum(A, dims=2))
					alt_total = sum(d * (d - 1) for d in degrees)
					
				#	Print diagnostics
					println("\n=== Transitivity Diagnostics ===")
					println("Number of nodes: $n")
					println("Number of edges: $num_edges")
					println("Number of triangles: $num_triangles")
					println("Closed triples (ordered): $closed")
					println("Total triples (ordered): $total")
					println("Alternative total (via degrees): $alt_total")
					println("Matrix formula (6*tri/deg_sum): $(tri_6 / alt_total)")
					println("Explicit formula (closed/total): $(closed / total)")
					
				#	Check degree distribution
					println("\nDegree distribution:")
					deg_counts = Dict{Int,Int}()
					for d in degrees
						deg_counts[Int(d)] = get(deg_counts, Int(d), 0) + 1
					end
					for d in sort(collect(keys(deg_counts)))
						println("  Degree $d: $(deg_counts[d]) nodes")
					end
			end
		
		#	Return transitivity
			return total > 0 ? closed / total : 0.0
	end

#	Alternative implementation 
	function transitivity_matrix_method(edges::DataFrame; drop_self_loops::Bool=true)
		"""
		Args:
			edges::DataFrame: edge list
			drop_self_loops::Bool: remove self-loops
		Returns:
			Float64: transitivity via matrix method
		Notes:
			Uses the tri6/den approach from your original function
		"""
		
		#	Build undirected binary adjacency
			A, _ = _build_undirected_adjacency(edges; drop_self_loops=drop_self_loops)
		
		#	Compute via matrix method
			degrees = vec(sum(A, dims=2))
			den = sum(d * (d - 1) for d in degrees)
			
			if den == 0
				return 0.0
			end
		
		#	Count triangles
			A_dense = Matrix(A)
			tri6 = sum((A_dense * A_dense) .* A_dense)
		
		#	Return transitivity
			return tri6 / den
	end

#	Test with a simple known example
	function test_transitivity_methods()
		"""
		Test with a simple triangle graph where we know the answer
		"""
		
		#	Create a simple triangle: nodes 1,2,3 all connected
			println("Testing with a complete triangle (3 nodes, 3 edges):")
			edges_triangle = DataFrame(src=[1,2,3], dst=[2,3,1])
			
		#	Expected: 1.0 (all 2-paths are closed)
			result1 = transitivity_netstat(edges_triangle; verbose=true)
			result2 = transitivity_matrix_method(edges_triangle)
			
			println("\nResults for triangle:")
			println("  NetStat method: $result1")
			println("  Matrix method: $result2")
			println("  Expected: 1.0")
		
		#	Create a path graph: 1-2-3 (no triangle)
			println("\n" * "="^50)
			println("Testing with a path (3 nodes, 2 edges):")
			edges_path = DataFrame(src=[1,2], dst=[2,3])
			
		#	Expected: 0.0 (no closed 2-paths)
			result1 = transitivity_netstat(edges_path; verbose=true)
			result2 = transitivity_matrix_method(edges_path)
			
			println("\nResults for path:")
			println("  NetStat method: $result1")
			println("  Matrix method: $result2")
			println("  Expected: 0.0")
		
		#	Create a square with one diagonal
			println("\n" * "="^50)
			println("Testing with a square plus diagonal (4 nodes, 5 edges):")
			edges_square = DataFrame(src=[1,2,3,4,1], dst=[2,3,4,1,3])
			
		#	This creates 2 triangles sharing an edge
			result1 = transitivity_netstat(edges_square; verbose=true)
			result2 = transitivity_matrix_method(edges_square)
			
			println("\nResults for square with diagonal:")
			println("  NetStat method: $result1")
			println("  Matrix method: $result2")
	end

#	Helper Function for degree calculations: edge list to sparse adjacency matrix
	function _edgelist_to_sparse_matrix(edges::DataFrame; weighted::Bool=true)
		"""
		Args:
			edges::DataFrame: DataFrame with src, dst, and optionally weight columns
			weighted::Bool: use weights if true and available (default = true)
		Returns:
			Tuple{SparseMatrixCSC{Float64,Int64}, Dict{Any,Int}, Vector{Any}}
		Notes:
			Returns (adj_matrix, node_to_idx, idx_to_node).
			Handles arbitrary node identifiers.
		"""
		
		#	Extract unique nodes and create mappings
			all_nodes = unique(vcat(edges.src, edges.dst))
			n = length(all_nodes)
			node_to_idx = Dict(node => i for (i, node) in enumerate(all_nodes))
			idx_to_node = all_nodes
		
		#	Map edges to indices
			src_idx = [node_to_idx[s] for s in edges.src]
			dst_idx = [node_to_idx[d] for d in edges.dst]
		
		#	Determine weights
			if weighted && hasproperty(edges, :weight)
				#	Use provided weights
					weights = Float64.(edges.weight)
			else
				#	Unweighted: use 1.0 for all edges
					weights = ones(Float64, nrow(edges))
			end
		
		#	Build sparse adjacency matrix
			adj_matrix = sparse(src_idx, dst_idx, weights, n, n)
		
		#	Return matrix and mappings
			return (adj_matrix, node_to_idx, idx_to_node)
	end

#	Helper Function for degree calculations: aggregate duplicate edges
	function _aggregate_multi_edges(edges::DataFrame; agg_func::Function=sum)
		"""
		Args:
			edges::DataFrame: DataFrame with src, dst, and optionally weight columns
			agg_func::Function: aggregation function for duplicate edges (default = sum)
		Returns:
			DataFrame: edges with duplicates aggregated
		Notes:
			Handles agg_func even when no weights exist.
			When no weights exist and agg_func=maximum, creates binary presence.
		"""
		
		#	Check if weights exist
			has_weights = hasproperty(edges, :weight)
		
		#	Group and aggregate
			if has_weights
				#	Aggregate weights for duplicate edges
					grouped = combine(groupby(edges, [:src, :dst]), 
					                 :weight => agg_func => :weight)
			else
				#	Handle based on agg_func
					if agg_func == maximum
						#	For maximum without weights: binary presence (any edge = 1)
							grouped = combine(groupby(edges, [:src, :dst])) do _
								DataFrame(weight = 1.0)
							end
					elseif agg_func == sum
						#	For sum without weights: count edges
							grouped = combine(groupby(edges, [:src, :dst]), 
							                 nrow => :weight)
					else
						#	For other functions: apply to ones
							grouped = combine(groupby(edges, [:src, :dst])) do grp
								DataFrame(weight = agg_func(ones(nrow(grp))))
							end
					end
			end
		
		#	Return aggregated edges
			return grouped
	end

#	Transitivity after k-core pruning (undirected, binary, loopless; NetStat/ORA style)
	function transitivity_after_kcore(edges::DataFrame;
	                                  k_core_min::Int=2,
	                                  drop_self_loops::Bool=true,
	                                  verbose::Bool=true)
		"""
		Args:
			edges::DataFrame: edge list with :src, :dst (weights ignored)
			k_core_min::Int: minimum k for k-core pruning (default = 2)
			drop_self_loops::Bool: drop u→u before building the simple graph (default = true)
			verbose::Bool: print diagnostics before/after pruning (default = true)
		Returns:
			Float64: global transitivity on the k-core subgraph
		Notes:
			Pipeline:
			1) Collapse multi-edges to binary presence (any parallel edge -> 1).
			2) Drop self-loops (if requested).
			3) Canonicalize endpoints to a simple undirected edge set.
			4) Build symmetric binary adjacency A.
			5) Prune to k-core (iteratively remove nodes with degree < k).
			6) Compute transitivity = (6T) / Σ_i k_i (k_i − 1) on the pruned graph.
		"""

		#	Validate input
			if !hasproperty(edges, :src) || !hasproperty(edges, :dst)
				throw(ArgumentError("edges must have :src and :dst columns"))
			end
			if nrow(edges) == 0
				return 0.0
			end

		#	Collapse multi-edges to binary presence
			clean_edges = _aggregate_multi_edges(edges; agg_func=maximum)

		#	Optional: drop self-loops at edge level
			if drop_self_loops
				if hasproperty(clean_edges, :weight)
					clean_edges = clean_edges[clean_edges.src .!= clean_edges.dst, [:src, :dst, :weight]]
				else
					clean_edges = clean_edges[clean_edges.src .!= clean_edges.dst, [:src, :dst]]
				end
			end

		#	Canonicalize to simple undirected edges (min, max) and deduplicate
			edges_simple = DataFrame(
				src = min.(clean_edges.src, clean_edges.dst),
				dst = max.(clean_edges.src, clean_edges.dst)
			)
			edges_simple = unique(edges_simple)

		#	Pre-pruning counts
			all_nodes_pre = unique(vcat(edges_simple.src, edges_simple.dst))
			n_pre = length(all_nodes_pre)
			m_pre = nrow(edges_simple)

		#	Build symmetric, binary, zero-diagonal adjacency A
			edges_bidirectional = vcat(
				edges_simple,
				DataFrame(src = edges_simple.dst, dst = edges_simple.src)
			)
			A, _, _ = _edgelist_to_sparse_matrix(edges_bidirectional; weighted=false)
			A = max.(A, A')
			A = A .- spdiagm(0 => diag(A))
			A = spzeros(Float64, size(A)...) .+ (A .> 0)

		#	k-core pruning (FIX: reset `active` after shrinking A)
			if k_core_min > 0
				deg    = vec(sum(A, dims=2))
				active = trues(size(A, 1))
				changed = true
				while changed
					changed = false
					for i in 1:length(active)
						if active[i] && deg[i] < k_core_min
							active[i] = false
							changed = true
						end
					end
					if changed
						A = A[active, active]              # shrink matrix
						deg = vec(sum(A, dims=2))          # recompute degrees for new size
						active = trues(size(A, 1))         # FIX: reset active to new size
					end
				end
			end

		#	Handle degenerate case after pruning
			if size(A, 1) == 0
				if verbose
					println("k-core pruning removed all nodes (k = $k_core_min). Returning 0.0.")
				end
				return 0.0
			end

		#	Compute ORA/NetStat transitivity = (6T) / Σ_i k_i (k_i − 1)
			k = vec(sum(A, dims=2))
			den = sum(k .* (k .- 1))
			if den == 0
				if verbose
					println("No connected triples after pruning (denominator = 0). Returning 0.0.")
				end
				return 0.0
			end

		#	Triangle count via tri6 = sum((A*A) .* A) == 6 * (#triangles)
			tri6 = sum((A * A) .* A)
			trans = tri6 / den

		#	Diagnostics
			if verbose
				m_post = Int(nnz(A) ÷ 2)
				n_post = size(A, 1)
				triangles = tri6 / 6
				connected_triples = den / 2
				println("\n=== Transitivity after k-core pruning ===")
				println("k-core (k ≥ $k_core_min)")
				println("Nodes  (pre → post): $n_pre → $n_post")
				println("Edges  (pre → post): $m_pre → $m_post")
				println("Triangles (post):    $triangles")
				println("Connected triples:    $connected_triples")
				println("Transitivity:         $trans\n")
			end

		#	Result
			return trans
	end
	@doc raw"""
	**Description**
	Compute ORA/NetStat-style global transitivity on the undirected, binary, loopless *k*-core of the input edge list.  
	This is a diagnostic/test helper that lets you see how *k*-core pruning (default 2-core) affects the transitivity value.

	**Usage**
	`transitivity_after_kcore(edges::DataFrame; k_core_min::Int=2, drop_self_loops::Bool=true, verbose::Bool=true)`

	**Arguments**
	- `edges::DataFrame`: Edge list with `:src`, `:dst` (weights ignored).
	- `k_core_min::Int`: Minimum degree for *k*-core pruning (default `2`).
	- `drop_self_loops::Bool`: Drop self-loops before building the simple graph (default `true`).
	- `verbose::Bool`: Print diagnostics before/after pruning (default `true`).

	**Details**
	1. Collapses multi-edges to binary presence; drops self-loops if requested.
	2. Canonicalizes to a simple undirected graph and builds a symmetric, binary, zero-diagonal adjacency.
	3. Prunes iteratively to the *k*-core (`k_core_min`), removing nodes with degree `< k`.
	4. Returns transitivity = `(6 * #triangles) / Σ_i k_i (k_i − 1)` on the pruned graph,
	i.e., the fraction of connected triples that are closed.

	**Value**
	`Float64`: The transitivity value on the *k*-core subgraph.

	**Notes**
	This mirrors the ORA/NetStat undirected, binary notion of transitivity; it is not the Watts–Strogatz average local clustering.  
	Use this to experiment with `k_core_min=2` to see if your value moves closer to ORA’s reported number.

	**Examples**
	```julia
	t2 = transitivity_after_kcore(agent_agent_all_com.edges; k_core_min=2, drop_self_loops=true, verbose=true)
	# Try different k values:
	t3 = transitivity_after_kcore(agent_agent_all_com.edges; k_core_min=3, drop_self_loops=true, verbose=false)
	References

	NetStat/ORA definition of transitivity (fraction of closed triples).

	Seidman, S. B. (1983). Network structure and minimum degree. Social Networks, 5(3), 269–287. (k-core)
	""" transitivity_after_kcore

##########################
#   GRAPH IMPORT TESTS   #
##########################

#   Loading Balikatan_2022_Processed
    import_directory = "/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data"
    ora_xml = "Balikatan_2022_Processed.xml"
    file_location = string(import_directory, "/", ora_xml)
    balikatan_2022 = load_ora_xml(file_location)
    
#   Examining Nodeset
    balikatan_2022.nodesets
    agents = balikatan_2022.nodesets["Agent"]
    tweets = balikatan_2022.nodesets["Tweet"]
    hashtags = balikatan_2022.nodesets["Hashtag"]
    urls =  balikatan_2022.nodesets["URL"]

#   Examining Networks
    agent_agent_sender = balikatan_2022.networks["Agent x Tweet - Sender"]
    agent_agent_all_com = balikatan_2022.networks["Agent x Agent - All Communication"]

#   Performing Checks
    summarize_ora(balikatan_2022)

######################################
#   MEASURE TESTS: DEGREE MEASURES   #
######################################

#	CALCULATE DEGREE MEASURES

#   Agent x Agent - All-Communication: In-Degree
    println("\n--- In-Degree ---")
	all_comm_in_deg = in_degree(agent_agent_all_com.edges; weighted=false)
	println(all_comm_in_deg)

#   Agent x Agent - All-Communication: Out-Degree
	println("\n--- Out-Degree ---")
	out_deg = out_degree(agent_agent_all_com.edges; weighted=false)
	println(out_deg)

#   Agent x Agent - All-Communication: Total Degree
    println("\n--- Total Degree ---")
	total_deg = total_degree(agent_agent_all_com.edges; weighted=false, drop_self_loops=false,
							 count_self_loops_once=true)
	println(total_deg)

#   Agent x Agent - All-Communication: Degree Ratio
    println("\n--- Degree Ratio ---")
	ratio = degree_ratio(agent_agent_all_com.edges; weighted=false)
	println(ratio)
   
#   Agent x Agent - All-Communication: Weighted In-Degree
	println("\n--- Weighted In-Degree ---")
	all_comm_wgt_in_deg = in_degree(agent_agent_all_com.edges; weighted=true)
	println(all_comm_wgt_in_deg)

#   Agent x Agent - All-Communication: Weighted Out-Degree
	println("\n--- Weighted Out-Degree ---")
	wgt_out_deg = out_degree(agent_agent_all_com.edges; weighted=true)
	println(wgt_out_deg)
	wgt_out_deg[(1:10),:]

#   Agent x Agent - All-Communication: Weighted Total Degree
	println("\n--- Weighted Total Degree ---")
	wgt_total_deg = total_degree(agent_agent_all_com.edges; weighted=true,
	                      		 normalize=false, agg_func = sum,
	                      	     ignore_direction=false, drop_self_loops=false,
								 count_self_loops_once=true)
	println(wgt_total_deg)
	wgt_total_deg[(1:10),:]

#   Agent x Agent - All-Communication: Weighted Degree Ratio
	println("\n--- Weighted Degree Ratio ---")
	wgt_ratio = degree_ratio(agent_agent_all_com.edges; weighted=true)
	println(wgt_ratio)
	wgt_ratio[(1:10),:]

#   Freeman Normalizations
	in_deg_norm = in_degree(agent_agent_all_com.edges; weighted=true, normalize=true)
	out_deg_norm = out_degree(agent_agent_all_com.edges; weighted=true, normalize=true)
	total_deg_norm = total_degree(agent_agent_all_com.edges; weighted=true, normalize=true, drop_self_loops=false,
								 count_self_loops_once=true)

#   COMPARE TO ORA

#	Import ORA Degree Scores
	file_location = "/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data/Agent_Agent_AllCommunication_DegreeMeasures.csv"
	ora_degree_scores = CSV.read(file_location, DataFrame; types=Dict(1 => String))
	rename!(ora_degree_scores, ["node", "Centrality, Total-Degree_Scale", "Centrality, Out-Degree_Scaled",
 								"Centrality, In-Degree_Scaled", "Centrality, Total-Degree", "Centrality, In-Degree",
 								"Centrality, Out-Degree"])

#	Comparing Weighted In-Degree Scores
	leftjoin!(all_comm_wgt_in_deg, ora_degree_scores[:,[1,6]], on=:node)
	all_comm_wgt_in_deg[!,3] = convert.(Int64, all_comm_wgt_in_deg[:,3])
	all_comm_wgt_in_deg.delta = all_comm_wgt_in_deg[:,2] -  all_comm_wgt_in_deg[:,3]
	sum(all_comm_wgt_in_deg.delta)

#	Comparing Weighted Out-Degree Scores
	leftjoin!(wgt_out_deg, ora_degree_scores[:,[1,7]], on=:node)
	wgt_out_deg[!,3] = convert.(Int64, wgt_out_deg[:,3])
	wgt_out_deg.delta = wgt_out_deg[:,2] - wgt_out_deg[:,3]
	sum(wgt_out_deg.delta)

#	Comparing Weighted Total-Degree Scores
	leftjoin!(wgt_total_deg, ora_degree_scores[:,[1,5]], on=:node)
	wgt_total_deg[!,3] = convert.(Int64, wgt_total_deg[:,3])
	wgt_total_deg.delta = wgt_total_deg[:,2] - wgt_total_deg[:,3]
	sum(wgt_total_deg.delta)

#	Comparing Normalized In-Degree
	leftjoin!(in_deg_norm, ora_degree_scores[:,[1,4]], on=:node)
	in_deg_norm[!,3] = convert.(Float64, in_deg_norm[:,3])
	in_deg_norm.delta = in_deg_norm[:,2] - in_deg_norm[:,3]
	sum(in_deg_norm.delta)

#	Comparing Normalized Out-Degree
	leftjoin!(out_deg_norm, ora_degree_scores[:,[1,3]], on=:node)
	out_deg_norm[!,3] = convert.(Float64, out_deg_norm[:,3])
	out_deg_norm.delta = out_deg_norm[:,2] - out_deg_norm[:,3]
	sum(out_deg_norm.delta)

#	Comparing Normalized Total-Degree
	leftjoin!(total_deg_norm, ora_degree_scores[:,[1,2]], on=:node)
	total_deg_norm[!,3] = convert.(Float64, total_deg_norm[:,3])
	total_deg_norm.delta = total_deg_norm[:,2] - total_deg_norm[:,3]
	sum(total_deg_norm.delta)

######################################
#   MEASURE TESTS: LOCAL STRUCTURE   #
######################################

#   CALCULATE LOCAL STRUCTURE MESURES

# 	Basic Transitivity Test
	test_transitivity_methods()

#	2-Core Transitivity Test
	transitivity_after_kcore(agent_agent_all_com.edges)
 
#	Local Clustering Coefficient: Watts DJ, Strogatz SH (1998)
	strogatz_local_clustering = local_clustering_coefficient(agent_agent_all_com.edges, directed=true, weighted=false)

#	Global Clustering Coefficient (Full Graph: 0.232, Largetst Component: 0.229)
	transitivity = global_clustering_coefficient(agent_agent_all_com.edges; directed=false, weighted=false, method=:transitivity, drop_self_loops=true)

	mean_ego_network_density = global_clustering_coefficient(agent_agent_all_com.edges; weighted= false, directed=true, method=:average)

#	Weighted Global Clustering Coefficient: Barrat et al. (2004)
	barrat_clustering_coefficients = weighted_clustering_coefficient(agent_agent_all_com.edges; directed=false, agg_func=sum)

#	Directed Weighted Clustering (Clemente & Grassi, 2018)
	cg_clustering_coefficients = weighted_clustering_coefficient(agent_agent_all_com.edges; directed=true, agg_func=sum)

#   Local Reciprocity (Fraction of Reciprocated Edges)

#   COMPARISON TESTS

#	Import Comparison Data
	wgt_local_clustering = CSV.read("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data/clustering_comparison.csv", DataFrame)

	ora_local_clustering = CSV.read("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data/Density_Local_Clustering.csv", DataFrame; types=Dict(1 => String))
	ora_local_clustering = ora_local_clustering[:,(1:3)]
	rename!(ora_local_clustering, ["node", "screen_name", "clusteringCoefficient-1-Balikatan_2022"])

#	Construct CG Clustering Toy Graph
	test_edges = DataFrame(src = ["A","A","B","B","C","D","E"], dst = ["B","C","C","D","A","A","B"],
      				       weight = [1.0, 5.0, 2.0, 3.0, 1.0, 4.0, 1.0])

#	Local Clustering
	leftjoin!(strogatz_local_clustering, ora_local_clustering, on=:node)
	strogatz_local_clustering = strogatz_local_clustering[:,[1,3,4,2]]
	strogatz_local_clustering[!,3] = convert.(Float64, strogatz_local_clustering[:,3])
	strogatz_local_clustering.delta = strogatz_local_clustering[:,3] .- strogatz_local_clustering[:,4] 

#	Weighted Directed Clustering
	julia_results = weighted_clustering_coefficient(test_edges; directed=true, agg_func=sum)
	delta_scores = DataFrame(cg_cycle_delta = wgt_local_clustering.cg_cycle .- julia_results.cg_cycle,
							 cg_middleman_delta = wgt_local_clustering.cg_middleman .- julia_results.cg_middleman,
							 cg_in_delta = wgt_local_clustering.cg_in .- julia_results.cg_in,
							 cg_out_delta = wgt_local_clustering.cg_out .- julia_results.cg_out,
							 cg_total_delta = wgt_local_clustering.cg_total .- julia_results.cg_total,
							 barrat_local_delta = wgt_local_clustering.barrat_local .- julia_results.barrat_local)

	weighted_clustering_coefficient(test_edges; directed=false, agg_func=sum)

#	TO DO:
#	1) Investigate source of Local Clustering Differences
#	2) Perform Weighted Directed Clustering Coeffiecent Test in R on the Agent x Agent -All Communication Network to Make supported
#      the differences are at the same scale as the Toy example.
#	3) Implement Local Reciprocity (Fraction of Reciprocated Edges) Measure

####################################################
#   MEASURE TESTS: INFLUENCE CENTRALITY MEASURES   #
####################################################