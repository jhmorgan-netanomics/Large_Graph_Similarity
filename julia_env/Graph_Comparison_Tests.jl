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
	using Statistics
	using StatsBase
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

#	Helper Function for local_clustering_coefficient: extract ego network with diagnostics
	function _extract_ego_network_debug(adj::SparseMatrixCSC, node_idx::Int; directed::Bool=true)
		"""
		Args:
			adj::SparseMatrixCSC: adjacency matrix
			node_idx::Int: index of ego node
			directed::Bool: whether graph is directed
		Returns:
			Tuple: (neighbors, ego_subnet, ego_edges_info)
		Notes:
			Enhanced version that returns diagnostic information.
		"""
		
		#	Get neighbors based on direction
			if directed
				out_neighbors = findall(!iszero, adj[node_idx, :])
				in_neighbors = findall(!iszero, adj[:, node_idx])
				neighbors = unique(vcat(out_neighbors, in_neighbors))
			else
				neighbors = findall(!iszero, adj[node_idx, :])
			end
			
		#	Remove self-loops
			filter!(n -> n != node_idx, neighbors)
			
		#	Build ego network submatrix
			ego_nodes = vcat([node_idx], neighbors)
			ego_subnet = adj[ego_nodes, ego_nodes]
			
		#	Count different edge types
			n_neighbors = length(neighbors)
			edges_from_ego = nnz(adj[node_idx, neighbors])
			edges_to_ego = nnz(adj[neighbors, node_idx])
			edges_between_neighbors = nnz(adj[neighbors, neighbors])
			
		#	Diagnostic info
			ego_edges_info = Dict(
				"n_neighbors" => n_neighbors,
				"edges_from_ego" => edges_from_ego,
				"edges_to_ego" => edges_to_ego,
				"edges_between_neighbors" => edges_between_neighbors,
				"total_ego_edges" => edges_from_ego + edges_to_ego + edges_between_neighbors
			)
			
		#	Return results
			return neighbors, ego_subnet, ego_edges_info
	end

#	Local Clustering Coefficient with ORA-compatible calculation
	function local_clustering_coefficient_ora(edges::DataFrame;
	                                          directed::Bool=true,
	                                          weighted::Bool=false,
	                                          include_ego_edges::Bool=false,
	                                          ora_method::Symbol=:standard,
	                                          agg_func::Function=sum)
		"""
		Args:
			edges::DataFrame: edge list with src, dst, and optionally weight columns
			directed::Bool: treat graph as directed (default = true)
			weighted::Bool: use edge weights (default = false, uses binary)
			include_ego_edges::Bool: include edges to/from ego in density (default = false)
			ora_method::Symbol: :standard, :double_denom, :with_selfloops, :half_triangles
			agg_func::Function: aggregation for parallel edges (default = sum)
		Returns:
			DataFrame: columns [node, clustering_coefficient, debug_info]
		Notes:
			Tests different ORA calculation methods:
			- :standard = k*(k-1)
			- :double_denom = 2*k*(k-1)  
			- :with_selfloops = k*k
			- :half_triangles = counts partial triangles as 0.5
		"""
		
		#	Validation
			if !hasproperty(edges, :src) || !hasproperty(edges, :dst)
				throw(ArgumentError("edges DataFrame must have src and dst columns"))
			end
		
		#	Handle empty edge list
			if nrow(edges) == 0
				return DataFrame(node=[], clustering_coefficient=Float64[], debug_info=[])
			end
		
		#	Aggregate multi-edges
			clean_edges = _aggregate_multi_edges(edges; agg_func=agg_func)
		
		#	Build adjacency matrix
			adj, node_to_idx, idx_to_node = _edgelist_to_sparse_matrix(clean_edges; weighted=false)
		
		#	Initialize results
			n = length(idx_to_node)
			clustering_values = zeros(Float64, n)
			debug_info = Vector{Dict{String,Any}}(undef, n)  # Changed to Dict{String,Any}
		
		#	Calculate clustering coefficient for each node
			for i in 1:n
				#	Extract ego network with diagnostics
					neighbors, ego_subnet, ego_edges_info = _extract_ego_network_debug(adj, i; directed=directed)
					
				#	Convert to Any type dict for flexibility
					debug_dict = Dict{String,Any}()
					for (k, v) in ego_edges_info
						debug_dict[k] = v
					end
					
				#	Skip if insufficient neighbors
					k = length(neighbors)
					if k < 2
						clustering_values[i] = 0.0
						debug_dict["method"] = string(ora_method)
						debug_dict["max_edges"] = 0
						debug_dict["actual_edges"] = 0
						debug_info[i] = debug_dict
						continue
					end
					
				#	Get actual edges count
					actual_edges = Float64(ego_edges_info["edges_between_neighbors"])
					
				#	Calculate max edges based on method
					if ora_method == :standard
						#	Standard directed/undirected
							if directed
								max_edges = k * (k - 1)
							else
								max_edges = k * (k - 1) / 2
							end
							
					elseif ora_method == :double_denom
						#	Double denominator for directed
							if directed
								max_edges = 2 * k * (k - 1)
							else
								max_edges = k * (k - 1)
							end
							
					elseif ora_method == :with_selfloops
						#	Include self-loops in max count
							if directed
								max_edges = k * k
							else
								max_edges = k * (k + 1) / 2
							end
							
					elseif ora_method == :half_triangles
						#	Count partial triangles as 0.5
							if directed
								#	Check for complete vs partial triangles
									complete_triangles = 0
									partial_triangles = 0
									for ni in 1:k
										for nj in (ni+1):k
											n_i = neighbors[ni]
											n_j = neighbors[nj]
											edge_ij = adj[n_i, n_j] > 0
											edge_ji = adj[n_j, n_i] > 0
											if edge_ij && edge_ji
												complete_triangles += 2
											elseif edge_ij || edge_ji
												partial_triangles += 1
											end
										end
									end
									actual_edges = complete_triangles + 0.5 * partial_triangles
								max_edges = k * (k - 1)
							else
								max_edges = k * (k - 1) / 2
							end
							
					else
						throw(ArgumentError("Unknown ora_method: $ora_method"))
					end
					
				#	Include ego edges if requested
					if include_ego_edges
						#	Add edges to/from ego
							total_nodes = k + 1
							if directed
								max_edges = total_nodes * (total_nodes - 1)
							else
								max_edges = total_nodes * (total_nodes - 1) / 2
							end
							actual_edges = Float64(ego_edges_info["total_ego_edges"])
					end
					
				#	Calculate clustering
					if max_edges > 0
						clustering_values[i] = actual_edges / max_edges
					else
						clustering_values[i] = 0.0
					end
					
				#	Add method info to debug
					debug_dict["method"] = string(ora_method)
					debug_dict["max_edges"] = max_edges
					debug_dict["actual_edges"] = actual_edges
					debug_info[i] = debug_dict
			end
		
		#	Assembling Result
			result = DataFrame(
				node = idx_to_node,
				clustering_coefficient = clustering_values,
				debug_info = debug_info
			)
			return result
	end

#	Diagnostic Function for Special Nodes
	function diagnose_node_clustering(edges::DataFrame, node_id::String; directed::Bool=true)
		"""
		Args:
			edges::DataFrame: edge list
			node_id::String: node to diagnose
			directed::Bool: graph directionality
		Returns:
			Dict: detailed diagnostics
		Notes:
			Helps identify why a node's clustering differs from ORA.
		"""
		
		#	Filter ego network edges
			ego_edges_out = edges[edges.src .== node_id, :]
			ego_edges_in = edges[edges.dst .== node_id, :]
			
		#	Get neighbors
			out_neighbors = unique(ego_edges_out.dst)
			in_neighbors = unique(ego_edges_in.src)
			all_neighbors = unique(vcat(out_neighbors, in_neighbors))
			
		#	Get edges between neighbors
			neighbor_edges = edges[
				(edges.src .∈ Ref(all_neighbors)) .& 
				(edges.dst .∈ Ref(all_neighbors)),
				:
			]
			
		#	Calculate different clustering variants
			k = length(all_neighbors)
			n_neighbor_edges = nrow(neighbor_edges)
			
		#	Standard clustering (neighbors only)
			if k >= 2
				max_edges_neighbors = directed ? k * (k - 1) : k * (k - 1) / 2
				clustering_neighbors_only = n_neighbor_edges / max_edges_neighbors
			else
				clustering_neighbors_only = 0.0
			end
			
		#	Full ego network clustering
			total_ego_edges = nrow(ego_edges_out) + nrow(ego_edges_in) + n_neighbor_edges
			if k >= 1
				max_edges_full = directed ? (k + 1) * k : (k + 1) * k / 2  
				clustering_full_ego = total_ego_edges / max_edges_full
			else
				clustering_full_ego = 0.0
			end
			
		#	Alternative calculation (may match ORA for small networks)
			clustering_alt = k >= 2 ? n_neighbor_edges / (2 * k * (k - 1)) : 0.0
			
		#	Assembling diagnostics
			return Dict(
				"node" => node_id,
				"n_neighbors" => k,
				"out_neighbors" => length(out_neighbors),
				"in_neighbors" => length(in_neighbors),
				"edges_between_neighbors" => n_neighbor_edges,
				"total_ego_edges" => total_ego_edges,
				"clustering_neighbors_only" => clustering_neighbors_only,
				"clustering_full_ego" => clustering_full_ego,
				"clustering_alternative" => clustering_alt,
				"neighbor_list" => all_neighbors
			)
	end

#	Test Function for Local Weighted Reciprocity
	function test_local_weighted_reciprocity()
		"""
		Args:
			None
		Returns:
			Nothing (prints test results)
		Notes:
			Tests local_weighted_reciprocity on 5 ego network topologies.
			Compares computed values against analytical calculations.
		"""
		
		#	Test setup
			println("=" ^ 60)
			println("Testing Local Weighted Reciprocity")
			println("=" ^ 60)
			test_passed = true
		
		#	Test 1: Star Network (Core-Periphery)
			println("\nTest 1: Star Network (Ego at center)")
			println("-" ^ 40)
			
		#	Create star: Ego connects to A,B,C with no reciprocation
			edges1 = DataFrame(
				src = ["Ego", "Ego", "Ego"],
				dst = ["A", "B", "C"],
				weight = [3, 2, 4]
			)
			
		#	Analytical expectations
			expected1 = Dict(
				"Ego" => (r=0.0, out=9.0, recip=0.0),  # No reciprocation
				"A" => (r=0.0, out=0.0, recip=0.0),    # No outgoing edges
				"B" => (r=0.0, out=0.0, recip=0.0),
				"C" => (r=0.0, out=0.0, recip=0.0)
			)
			
		#	Run function
			result1 = local_weighted_reciprocity(edges1; weighted=true)
			
		#	Validate
			for row in eachrow(result1)
				exp = expected1[row.node]
				pass = isapprox(row.r, exp.r; atol=1e-6) &&
				       isapprox(row.out_strength, exp.out; atol=1e-6) &&
				       isapprox(row.reciprocated, exp.recip; atol=1e-6)
				println("  $(row.node): r=$(round(row.r, digits=3)), expected $(exp.r) - $(pass ? "✓" : "✗")")
				test_passed = test_passed && pass
			end
		
		#	Test 2: Closed Cycles (Triangular Network)
			println("\nTest 2: Closed Cycles Network")
			println("-" ^ 40)
			
		#	Create triangles: Ego-A-B form triangle, Ego-C reciprocal
			edges2 = DataFrame(
				src = ["Ego", "Ego", "A", "B", "A", "C"],
				dst = ["A", "B", "Ego", "Ego", "B", "Ego"],
				weight = [4, 3, 2, 3, 1, 5]
			)
			
		#	Analytical expectations
			# Ego: out to A(4), B(3); in from A(2), B(3), C(5)
			# recip = min(4,2) + min(3,3) + min(0,5) = 2+3+0 = 5
			# r = 5/7 ≈ 0.714
			
			# A: out to Ego(2), B(1); in from Ego(4)
			# recip = min(2,4) + min(1,0) = 2+0 = 2
			# r = 2/3 ≈ 0.667
			
			expected2 = Dict(
				"Ego" => (r=5/7, out=7.0, recip=5.0),
				"A" => (r=2/3, out=3.0, recip=2.0),
				"B" => (r=1.0, out=3.0, recip=3.0),  # B→Ego(3) reciprocated
				"C" => (r=0.0, out=5.0, recip=0.0)   # C→Ego(5) not reciprocated
			)
			
		#	Run function
			result2 = local_weighted_reciprocity(edges2; weighted=true)
			
		#	Validate
			for row in eachrow(result2)
				exp = expected2[row.node]
				pass = isapprox(row.r, exp.r; atol=1e-6)
				println("  $(row.node): r=$(round(row.r, digits=3)), expected $(round(exp.r, digits=3)) - $(pass ? "✓" : "✗")")
				test_passed = test_passed && pass
			end
		
		#	Test 3: Bow-tie Network (Ego bridges two groups)
			println("\nTest 3: Bow-tie Network")
			println("-" ^ 40)
			
		#	Create bow-tie: Ego connects groups {A,B} and {C,D}
			edges3 = DataFrame(
				src = ["Ego", "Ego", "Ego", "Ego", "A", "B", "C", "D", "A", "C"],
				dst = ["A", "B", "C", "D", "Ego", "Ego", "Ego", "Ego", "B", "D"],
				weight = [3, 2, 4, 1, 3, 1, 2, 1, 2, 3]
			)
			
		#	Analytical expectations
			# Ego: out to A(3),B(2),C(4),D(1); in from A(3),B(1),C(2),D(1)
			# recip = min(3,3) + min(2,1) + min(4,2) + min(1,1) = 3+1+2+1 = 7
			# r = 7/10 = 0.7
			
			expected3 = Dict(
				"Ego" => (r=0.7, out=10.0, recip=7.0),
				"A" => (r=3/5, out=5.0, recip=3.0),  # A→Ego(3),B(2); reciprocated Ego(3)
				"B" => (r=1/1, out=1.0, recip=1.0),  # B→Ego(1); reciprocated
				"C" => (r=2/5, out=5.0, recip=2.0),  # C→Ego(2),D(3); reciprocated Ego(2)
				"D" => (r=1/1, out=1.0, recip=1.0)   # D→Ego(1); reciprocated
			)
			
		#	Run function
			result3 = local_weighted_reciprocity(edges3; weighted=true)
			
		#	Validate
			for row in eachrow(result3)
				exp = expected3[row.node]
				pass = isapprox(row.r, exp.r; atol=1e-6)
				println("  $(row.node): r=$(round(row.r, digits=3)), expected $(round(exp.r, digits=3)) - $(pass ? "✓" : "✗")")
				test_passed = test_passed && pass
			end
		
		#	Test 4: Uniformly Connected Network
			println("\nTest 4: Uniformly Connected Network")
			println("-" ^ 40)
			
		#	Create complete graph with uniform weights
			nodes4 = ["Ego", "A", "B", "C"]
			src4 = String[]
			dst4 = String[]
			weight4 = Float64[]
			
			for i in nodes4, j in nodes4
				if i != j
					push!(src4, i)
					push!(dst4, j)
					push!(weight4, 2.0)  # Uniform weight
				end
			end
			
			edges4 = DataFrame(src=src4, dst=dst4, weight=weight4)
			
		#	Analytical expectations - all nodes identical
			# Each node: 3 outgoing edges of weight 2, all reciprocated
			# recip = 3 * min(2,2) = 6
			# r = 6/6 = 1.0
			
			expected4_r = 1.0
			
		#	Run function
			result4 = local_weighted_reciprocity(edges4; weighted=true)
			
		#	Validate
			for row in eachrow(result4)
				pass = isapprox(row.r, expected4_r; atol=1e-6)
				println("  $(row.node): r=$(round(row.r, digits=3)), expected $(expected4_r) - $(pass ? "✓" : "✗")")
				test_passed = test_passed && pass
			end
		
		#	Test 5: Mixed Reciprocity Network
			println("\nTest 5: Mixed Reciprocity Network")
			println("-" ^ 40)
			
		#	Create mixed: equal-weight reciprocal + asymmetric reciprocal + one-way
			edges5 = DataFrame(
				src = ["Ego", "Ego", "Ego", "A", "B"],
				dst = ["A", "B", "C", "Ego", "Ego"],
				weight = [4, 3, 5, 4, 1]  # A perfect, B asymmetric, C one-way
			)
			
		#	Analytical expectations
			# Ego: out to A(4),B(3),C(5); in from A(4),B(1)
			# recip = min(4,4) + min(3,1) + min(5,0) = 4+1+0 = 5
			# r = 5/12 ≈ 0.417
			
			# A: perfect reciprocation with Ego
			# r = 4/4 = 1.0
			
			# B: asymmetric with Ego
			# r = 1/1 = 1.0 (its one edge is reciprocated)
			
			expected5 = Dict(
				"Ego" => (r=5/12, out=12.0, recip=5.0),
				"A" => (r=1.0, out=4.0, recip=4.0),
				"B" => (r=1.0, out=1.0, recip=1.0),
				"C" => (r=0.0, out=0.0, recip=0.0)
			)
			
		#	Run function
			result5 = local_weighted_reciprocity(edges5; weighted=true)
			
		#	Validate
			for row in eachrow(result5)
				exp = expected5[row.node]
				pass = isapprox(row.r, exp.r; atol=1e-6)
				println("  $(row.node): r=$(round(row.r, digits=3)), expected $(round(exp.r, digits=3)) - $(pass ? "✓" : "✗")")
				test_passed = test_passed && pass
			end
		
		#	Test normalization methods
			println("\nTest 6: Normalization Methods")
			println("-" ^ 40)
			
		#	Test z-score normalization
			result_z = local_weighted_reciprocity(edges5; weighted=true, normalize=:zscore)
			println("  Z-score normalization: mean=$(round(mean(result_z.r_norm), digits=3)) (should be ≈0)")
			
		#	Test rank normalization
			result_r = local_weighted_reciprocity(edges5; weighted=true, normalize=:rank)
			println("  Rank normalization: range=[$(round(minimum(result_r.r_norm), digits=3)), $(round(maximum(result_r.r_norm), digits=3))] (should be [0,1])")
		
		#	Summary
			println()
			println(repeat("=", 60))
			println("Overall Test Result: $(test_passed ? "PASSED ✓" : "FAILED ✗")")
			println(repeat("=", 60))
	end

#	Test Component Scaled Page Rank Centrality
	function test_pagerank_stitched()
		"""
		Args:
			None
		Returns:
			Nothing (prints test results)
		Notes:
			Tests pagerank_stitched with 3 components (9, 4, and 3 nodes).
			Validates that stitching weights are computed correctly for each method.
		"""
		
		#	Setup test output
			println("=" ^ 60)
			println("Testing PageRank Stitching Methods")
			println("=" ^ 60)
			all_passed = true
		
		#	Create 3-component graph
			println("\nBuilding 3-Component Test Graph")
			println("-" ^ 40)
			
		#	Component 1: 9-node ring with varied weights
			comp1_edges = DataFrame(
				src = ["A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9"],
				dst = ["A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A1"],
				weight = [1.0, 2.0, 1.0, 3.0, 2.0, 1.0, 2.0, 1.0, 2.0]
			)
			
		#	Component 2: 4-node strongly connected with uniform weights
			comp2_edges = DataFrame(
				src = ["B1", "B1", "B2", "B3", "B4", "B4"],
				dst = ["B2", "B3", "B4", "B4", "B1", "B2"],
				weight = [2.0, 2.0, 2.0, 2.0, 2.0, 2.0]
			)
			
		#	Component 3: 3-node path
			comp3_edges = DataFrame(
				src = ["C1", "C2"],
				dst = ["C2", "C3"],
				weight = [5.0, 5.0]
			)
			
		#	Combine all edges
			all_edges = vcat(comp1_edges, comp2_edges, comp3_edges)
			
		#	Calculate expected component properties
			total_nodes = 16
			comp1_nodes = 9
			comp2_nodes = 4
			comp3_nodes = 3
			
			comp1_edge_weight = sum(comp1_edges.weight)  # 15.0
			comp2_edge_weight = sum(comp2_edges.weight)  # 12.0
			comp3_edge_weight = sum(comp3_edges.weight)  # 10.0
			total_edge_weight = comp1_edge_weight + comp2_edge_weight + comp3_edge_weight  # 37.0
			
			println("Component 1: $comp1_nodes nodes, total edge weight = $comp1_edge_weight")
			println("Component 2: $comp2_nodes nodes, total edge weight = $comp2_edge_weight")
			println("Component 3: $comp3_nodes nodes, total edge weight = $comp3_edge_weight")
			println("Total: $total_nodes nodes, total edge weight = $total_edge_weight")
		
		#	Test 1: Stitching by nodes
			println("\n" * ("=" ^ 60))
			println("Test 1: Stitch by Nodes")
			println("-" ^ 40)
			
		#	Expected weights
			exp_weight1_nodes = comp1_nodes / total_nodes  # 9/16 = 0.5625
			exp_weight2_nodes = comp2_nodes / total_nodes  # 4/16 = 0.25
			exp_weight3_nodes = comp3_nodes / total_nodes  # 3/16 = 0.1875
			
		#	Run PageRank
			result_nodes = pagerank_stitched(all_edges; 
				stitch_by=:nodes, 
				alpha=0.85, 
				weighted=true,
				final_norm=:L1)
			
		#	Extract component weights
			comp_weights_nodes = result_nodes.component_weights
			
		#	Validate weights
			println("Expected component weights:")
			println("  Component 1: $(round(exp_weight1_nodes, digits=4))")
			println("  Component 2: $(round(exp_weight2_nodes, digits=4))")
			println("  Component 3: $(round(exp_weight3_nodes, digits=4))")
			println("\nActual component weights:")
			println("  Component 1: $(round(comp_weights_nodes[1], digits=4))")
			println("  Component 2: $(round(comp_weights_nodes[2], digits=4))")
			println("  Component 3: $(round(comp_weights_nodes[3], digits=4))")
			
		#	Check weights
			pass1 = isapprox(comp_weights_nodes[1], exp_weight1_nodes; atol=1e-6)
			pass2 = isapprox(comp_weights_nodes[2], exp_weight2_nodes; atol=1e-6)
			pass3 = isapprox(comp_weights_nodes[3], exp_weight3_nodes; atol=1e-6)
			test1_passed = pass1 && pass2 && pass3
			
			println("\nWeight validation: $(test1_passed ? "PASSED ✓" : "FAILED ✗")")
			
		#	Verify scores sum to 1 (L1 norm)
			score_sum = sum(result_nodes.scores)
			println("Score sum (should be 1.0): $(round(score_sum, digits=6))")
			sum_check = isapprox(score_sum, 1.0; atol=1e-6)
			println("L1 normalization: $(sum_check ? "PASSED ✓" : "FAILED ✗")")
			
			all_passed = all_passed && test1_passed && sum_check
		
		#	Test 2: Stitching by edges
			println("\n" * ("=" ^ 60))
			println("Test 2: Stitch by Edges")
			println("-" ^ 40)
			
		#	Expected weights
			exp_weight1_edges = comp1_edge_weight / total_edge_weight  # 15/37 ≈ 0.405
			exp_weight2_edges = comp2_edge_weight / total_edge_weight  # 12/37 ≈ 0.324
			exp_weight3_edges = comp3_edge_weight / total_edge_weight  # 10/37 ≈ 0.270
			
		#	Run PageRank
			result_edges = pagerank_stitched(all_edges; 
				stitch_by=:edges, 
				alpha=0.85, 
				weighted=true,
				final_norm=:L1)
			
		#	Extract component weights
			comp_weights_edges = result_edges.component_weights
			
		#	Validate weights
			println("Expected component weights:")
			println("  Component 1: $(round(exp_weight1_edges, digits=4))")
			println("  Component 2: $(round(exp_weight2_edges, digits=4))")
			println("  Component 3: $(round(exp_weight3_edges, digits=4))")
			println("\nActual component weights:")
			println("  Component 1: $(round(comp_weights_edges[1], digits=4))")
			println("  Component 2: $(round(comp_weights_edges[2], digits=4))")
			println("  Component 3: $(round(comp_weights_edges[3], digits=4))")
			
		#	Check weights
			pass1 = isapprox(comp_weights_edges[1], exp_weight1_edges; atol=1e-6)
			pass2 = isapprox(comp_weights_edges[2], exp_weight2_edges; atol=1e-6)
			pass3 = isapprox(comp_weights_edges[3], exp_weight3_edges; atol=1e-6)
			test2_passed = pass1 && pass2 && pass3
			
			println("\nWeight validation: $(test2_passed ? "PASSED ✓" : "FAILED ✗")")
			
		#	Check different from nodes weights
			weights_differ = !isapprox(comp_weights_edges[1], comp_weights_nodes[1]; atol=1e-3)
			println("Weights differ from nodes method: $(weights_differ ? "YES ✓" : "NO ✗")")
			
			all_passed = all_passed && test2_passed
		
		#	Test 3: Stitching by personalization
			println("\n" * ("=" ^ 60))
			println("Test 3: Stitch by Personalization")
			println("-" ^ 40)
			
		#	Create non-uniform personalization vector
			n_total = length(result_nodes.node_names)
			pers = zeros(n_total)
			
		#	Find indices for each component
			comp1_indices = findall(n -> startswith(n, "A"), result_nodes.node_names)
			comp2_indices = findall(n -> startswith(n, "B"), result_nodes.node_names)
			comp3_indices = findall(n -> startswith(n, "C"), result_nodes.node_names)
			
		#	Assign personalization weights (non-uniform)
			pers[comp1_indices] .= 1.0  # Total: 9
			pers[comp2_indices] .= 2.0  # Total: 8
			pers[comp3_indices] .= 3.0  # Total: 9
			pers_sum = sum(pers)  # 26
			
		#	Expected weights
			exp_weight1_pers = 9.0 / pers_sum   # 9/26 ≈ 0.346
			exp_weight2_pers = 8.0 / pers_sum   # 8/26 ≈ 0.308
			exp_weight3_pers = 9.0 / pers_sum   # 9/26 ≈ 0.346
			
		#	Run PageRank
			result_pers = pagerank_stitched(all_edges; 
				stitch_by=:personalization, 
				personalization=pers,
				alpha=0.85, 
				weighted=true,
				final_norm=:L1)
			
		#	Extract component weights
			comp_weights_pers = result_pers.component_weights
			
		#	Validate weights
			println("Personalization setup:")
			println("  Component 1 nodes get weight 1.0 each (total: 9)")
			println("  Component 2 nodes get weight 2.0 each (total: 8)")
			println("  Component 3 nodes get weight 3.0 each (total: 9)")
			println("\nExpected component weights:")
			println("  Component 1: $(round(exp_weight1_pers, digits=4))")
			println("  Component 2: $(round(exp_weight2_pers, digits=4))")
			println("  Component 3: $(round(exp_weight3_pers, digits=4))")
			println("\nActual component weights:")
			println("  Component 1: $(round(comp_weights_pers[1], digits=4))")
			println("  Component 2: $(round(comp_weights_pers[2], digits=4))")
			println("  Component 3: $(round(comp_weights_pers[3], digits=4))")
			
		#	Check weights
			pass1 = isapprox(comp_weights_pers[1], exp_weight1_pers; atol=1e-6)
			pass2 = isapprox(comp_weights_pers[2], exp_weight2_pers; atol=1e-6)
			pass3 = isapprox(comp_weights_pers[3], exp_weight3_pers; atol=1e-6)
			test3_passed = pass1 && pass2 && pass3
			
			println("\nWeight validation: $(test3_passed ? "PASSED ✓" : "FAILED ✗")")
			
			all_passed = all_passed && test3_passed
		
		#	Test 4: Verify component isolation
			println("\n" * ("=" ^ 60))
			println("Test 4: Component Isolation Check")
			println("-" ^ 40)
			
		#	Scores within components should be proportional
			comp1_scores = result_nodes.scores[comp1_indices]
			comp2_scores = result_nodes.scores[comp2_indices]
			comp3_scores = result_nodes.scores[comp3_indices]
			
		#	Sum of scores per component
			comp1_score_sum = sum(comp1_scores)
			comp2_score_sum = sum(comp2_scores)
			comp3_score_sum = sum(comp3_scores)
			
			println("Component score sums (stitch by nodes):")
			println("  Component 1: $(round(comp1_score_sum, digits=4))")
			println("  Component 2: $(round(comp2_score_sum, digits=4))")
			println("  Component 3: $(round(comp3_score_sum, digits=4))")
			
		#	These should match the component weights
			isolation_check1 = isapprox(comp1_score_sum, exp_weight1_nodes; atol=1e-3)
			isolation_check2 = isapprox(comp2_score_sum, exp_weight2_nodes; atol=1e-3)
			isolation_check3 = isapprox(comp3_score_sum, exp_weight3_nodes; atol=1e-3)
			isolation_passed = isolation_check1 && isolation_check2 && isolation_check3
			
			println("\nComponent isolation: $(isolation_passed ? "PASSED ✓" : "FAILED ✗")")
			
			all_passed = all_passed && isolation_passed
		
		#	Final summary
			println("\n" * ("=" ^ 60))
			println("Test Summary")
			println("-" ^ 40)
			println("Stitch by nodes:          $(test1_passed ? "PASSED ✓" : "FAILED ✗")")
			println("Stitch by edges:          $(test2_passed ? "PASSED ✓" : "FAILED ✗")")
			println("Stitch by personalization: $(test3_passed ? "PASSED ✓" : "FAILED ✗")")
			println("Component isolation:       $(isolation_passed ? "PASSED ✓" : "FAILED ✗")")
			println("-" ^ 40)
			println("OVERALL: $(all_passed ? "ALL TESTS PASSED ✓" : "SOME TESTS FAILED ✗")")
			println("=" ^ 60)
	end

#	SALSA Sanity Tests on Graphs with Known Solutions
	function test_salsa()
		"""
		Args:
			None
		Returns:
			Nothing (prints test results)
		Notes:
			Tests SALSA centrality on graphs with closed-form expectations.
		"""
		
		#	Helper Function for test_salsa: build edge list
			edgelist(srcs, dsts) = DataFrame(src=srcs, dst=dsts)
		
		#	Helper Function for test_salsa: compare results
			function _report(name, got_df, got_col, expected, expected_nodes; tol=1e-6)
				#	Extract values in expected node order
					got_values = Float64[]
					for node in expected_nodes
						idx = findfirst(==(node), got_df.node)
						if isnothing(idx)
							push!(got_values, 0.0)
						else
							push!(got_values, got_df[idx, got_col])
						end
					end
				
				#	Calculate L1 error
					err = sum(abs.(got_values .- expected))
					pass = err < tol
					
				#	Print results
					println(name, ":")
					println("  Nodes:    ", join(expected_nodes, ", "))
					println("  Got:      ", string(round.(got_values, digits=6)))
					println("  Expected: ", string(round.(expected, digits=6)))
					println("  L1 error: ", round(err, digits=10), "  Pass: ", pass)
					println()
			end
		
		#	Test 1: Star OUT (hub-and-spoke)
			println("=" ^ 60)
			println("Test 1: Star OUT (center → leaves)")
			nodes1 = ["C", "L1", "L2", "L3", "L4"]
			edges1 = edgelist(fill("C", 4), ["L1", "L2", "L3", "L4"])
			
		#	Expected: hub mass on center, auth uniform on leaves
			hub1 = salsa_centrality(edges1; score=:hub)
			auth1 = salsa_centrality(edges1; score=:authority)
			exp_hub1 = [1.0, 0.0, 0.0, 0.0, 0.0]
			exp_auth1 = [0.0, 0.25, 0.25, 0.25, 0.25]
			_report("Hub scores", hub1, :salsa_hub, exp_hub1, nodes1)
			_report("Auth scores", auth1, :salsa_authority, exp_auth1, nodes1)
		
		#	Test 2: Star IN (leaves → center)
			println("Test 2: Star IN (leaves → center)")
			nodes2 = ["C", "L1", "L2", "L3", "L4"]
			edges2 = edgelist(["L1", "L2", "L3", "L4"], fill("C", 4))
			
		#	Expected: hub uniform on leaves, auth mass on center
			hub2 = salsa_centrality(edges2; score=:hub)
			auth2 = salsa_centrality(edges2; score=:authority)
			exp_hub2 = [0.0, 0.25, 0.25, 0.25, 0.25]
			exp_auth2 = [1.0, 0.0, 0.0, 0.0, 0.0]
			_report("Hub scores", hub2, :salsa_hub, exp_hub2, nodes2)
			_report("Auth scores", auth2, :salsa_authority, exp_auth2, nodes2)
		
		#	Test 3: Mutual pair (A ↔ B)
			println("Test 3: Mutual pair (bidirectional edge)")
			nodes3 = ["A", "B"]
			edges3 = edgelist(["A", "B"], ["B", "A"])
			
		#	Expected: symmetric uniform distribution
			hub3 = salsa_centrality(edges3; score=:hub)
			auth3 = salsa_centrality(edges3; score=:authority)
			exp3 = [0.5, 0.5]
			_report("Hub scores", hub3, :salsa_hub, exp3, nodes3)
			_report("Auth scores", auth3, :salsa_authority, exp3, nodes3)
		
		#	Test 4: Complete digraph (all pairs, no self-loops)
			println("Test 4: Complete regular digraph (n=4)")
			nodes4 = ["v1", "v2", "v3", "v4"]
			src4 = String[]
			dst4 = String[]
			for i in 1:4, j in 1:4
				if i != j
					push!(src4, nodes4[i])
					push!(dst4, nodes4[j])
				end
			end
			edges4 = edgelist(src4, dst4)
			
		#	Expected: uniform distribution
			hub4 = salsa_centrality(edges4; score=:hub)
			auth4 = salsa_centrality(edges4; score=:authority)
			exp4 = fill(0.25, 4)
			_report("Hub scores", hub4, :salsa_hub, exp4, nodes4)
			_report("Auth scores", auth4, :salsa_authority, exp4, nodes4)
		
		#	Test 5: Bidirectional cycle
			println("Test 5: Bidirectional cycle (n=5)")
			nodes5 = ["1", "2", "3", "4", "5"]
			src5 = String[]
			dst5 = String[]
			for i in 1:5
				j = (i % 5) + 1
				push!(src5, nodes5[i])
				push!(dst5, nodes5[j])  # i → j
				push!(src5, nodes5[j])
				push!(dst5, nodes5[i])  # j → i
			end
			edges5 = edgelist(src5, dst5)
			
		#	Expected: uniform distribution
			hub5 = salsa_centrality(edges5; score=:hub)
			auth5 = salsa_centrality(edges5; score=:authority)
			exp5 = fill(0.2, 5)
			_report("Hub scores", hub5, :salsa_hub, exp5, nodes5)
			_report("Auth scores", auth5, :salsa_authority, exp5, nodes5)
		
		#	Test 6: Disconnected components
			println("Test 6: Disconnected stars (qualitative check)")
			nodes6 = ["C1", "A", "B", "C", "C2", "D"]
			edges6 = edgelist(
				["C1", "C1", "C1", "C2"],
				["A", "B", "C", "D"]
			)
			
		#	Get results
			hub6 = salsa_centrality(edges6; score=:hub)
			auth6 = salsa_centrality(edges6; score=:authority)
			
		#	Extract scores in correct order
			hub_c1 = hub6[findfirst(==(("C1")), hub6.node), :salsa_hub]
			hub_c2 = hub6[findfirst(==(("C2")), hub6.node), :salsa_hub]
			hub_leaves1 = [hub6[findfirst(==(n), hub6.node), :salsa_hub] for n in ["A", "B", "C"]]
			hub_d = hub6[findfirst(==(("D")), hub6.node), :salsa_hub]
			
			auth_c1 = auth6[findfirst(==(("C1")), auth6.node), :salsa_authority]
			auth_c2 = auth6[findfirst(==(("C2")), auth6.node), :salsa_authority]
			auth_leaves1 = [auth6[findfirst(==(n), auth6.node), :salsa_authority] for n in ["A", "B", "C"]]
			auth_d = auth6[findfirst(==(("D")), auth6.node), :salsa_authority]
			
		#	Qualitative checks
			hub_check1 = hub_c1 > maximum(hub_leaves1)
			hub_check2 = hub_c2 > hub_d
			auth_check1 = minimum(auth_leaves1) > auth_c1
			auth_check2 = auth_d > auth_c2
			
			println("  Hub centers > leaves:")
			println("    C1 > {A,B,C}: ", hub_check1)
			println("    C2 > D: ", hub_check2)
			println("  Auth leaves > centers:")
			println("    {A,B,C} > C1: ", auth_check1)
			println("    D > C2: ", auth_check2)
			println("  Overall pass: ", all([hub_check1, hub_check2, auth_check1, auth_check2]))
			println("=" ^ 60)
	end

#	Test Function for Reciprocity Methods
	function test_reciprocity_methods()
		"""
		Args:
			None
		Returns:
			Nothing (prints test results)
		Notes:
			Tests all reciprocity methods on a 5-node network designed to show differences.
		"""
		
		#	Create test network with varied reciprocity patterns
			edges = DataFrame(
				src = ["A", "A", "B", "B", "C", "C", "D", "E"],
				dst = ["B", "C", "A", "D", "A", "D", "E", "D"],
				weight = [4, 2, 1, 3, 2, 5, 6, 0]  # E→D has 0 weight, will be dropped
			)
			
		#	Remove zero-weight edge for clarity
			edges = edges[edges.weight .> 0, :]
			
		#	Display network structure
			println("=" ^ 60)
			println("Test Network (5 nodes):")
			println("-" ^ 30)
			for row in eachrow(edges)
				println("  $(row.src) → $(row.dst) : $(row.weight)")
			end
			println()
			
		#	Analyze network structure
			println("Network Structure:")
			println("  A ↔ B: weights 4 and 1 (reciprocal, unequal)")
			println("  A ← C: weight 2 (one-way from C)")
			println("  A → C: weight 2 (one-way to C)")
			println("  B → D: weight 3 (one-way)")
			println("  C ↔ D: weights 5 and 0 (one-way, D→C missing)")
			println("  D → E: weight 6 (one-way)")
			println()
			
		#	Calculate all methods
			println("=" ^ 60)
			println("Reciprocity Results:")
			println("-" ^ 30)
			
		#	Arc-based unweighted
			rec_arc_unw = reciprocity(edges; weighted=false, mode=:arc_based)
			println("Arc-based (unweighted):    $(round(rec_arc_unw, digits=4))")
			println("  Calculation: 4 edges have reverse / 7 total edges")
			println("  Result: 4/7 ≈ 0.571")
			println()
			
		#	Arc-based weighted
			rec_arc_w = reciprocity(edges; weighted=true, mode=:arc_based)
			println("Arc-based (weighted):      $(round(rec_arc_w, digits=4))")
			println("  Calculation: weights of edges with reverse / total weight")
			println("  Edges with reverse: A→B(4), B→A(1), A→C(2), C→A(2)")
			println("  Result: (4+1+2+2) / (4+1+2+2+3+5+6) = 9/23 ≈ 0.391")
			println()
			
		#	Dyad-based unweighted
			rec_dyad_unw = reciprocity(edges; weighted=false, mode=:dyad_based)
			println("Dyad-based (unweighted):   $(round(rec_dyad_unw, digits=4))")
			println("  Calculation: mutual dyads / connected dyads")
			println("  Mutual dyads: A-B, A-C = 2")
			println("  Connected dyads: A-B, A-C, B-D, C-D, D-E = 5")
			println("  Result: 2/5 = 0.4")
			println()
			
		#	Dyad-based weighted (ORA mutual)
			rec_dyad_ora = reciprocity(edges; weighted=true, mode=:dyad_based, weighted_method=:ora_mutual)
			println("Dyad-based ORA mutual:     $(round(rec_dyad_ora, digits=4))")
			println("  Calculation: dyads with exact weight match / connected dyads")
			println("  Exact matches: A-C (both 2) = 1")
			println("  Connected dyads: 5")
			println("  Result: 1/5 = 0.2")
			println()
			
		#	Dyad-based weighted (Squartini)
			rec_dyad_sq = reciprocity(edges; weighted=true, mode=:dyad_based, weighted_method=:squartini)
			println("Dyad-based Squartini:      $(round(rec_dyad_sq, digits=4))")
			println("  Calculation: Σ min(w_ij, w_ji) / Σ w_ij")
			println("  Min weights: A→B:1, B→A:1, A→C:2, C→A:2, others:0")
			println("  Result: (1+1+2+2) / 23 = 6/23 ≈ 0.261")
			println()
			
		#	Summary comparison
			println("=" ^ 60)
			println("Summary of Results:")
			println("-" ^ 30)
			println("Arc-based (unweighted):    $(round(rec_arc_unw, digits=4))")
			println("Arc-based (weighted):      $(round(rec_arc_w, digits=4))")
			println("Dyad-based (unweighted):   $(round(rec_dyad_unw, digits=4))")
			println("Dyad-based ORA mutual:     $(round(rec_dyad_ora, digits=4))")
			println("Dyad-based Squartini:      $(round(rec_dyad_sq, digits=4))")
			println()
			
		#	Verify all methods give different results
			results = [rec_arc_unw, rec_arc_w, rec_dyad_unw, rec_dyad_ora, rec_dyad_sq]
			all_different = length(unique(round.(results, digits=4))) == 5
			println("All methods give different results: ", all_different)
			println("=" ^ 60)
	end

#	Leiden Tests Helper: Compare Two Specific Partitions
	function compare_partitions(partition1::Vector{Int}, partition2::Vector{Int};
	                           verbose::Bool=true)
		"""
		Args:
			partition1::Vector{Int}: first partition (labels per ORIGINAL node)
			partition2::Vector{Int}: second partition (labels per ORIGINAL node)
			verbose::Bool: print detailed comparison (default = true)
		Returns:
			NamedTuple: (ari, n_communities_1, n_communities_2, confusion_matrix, agreement_rate)
		Notes:
			- Requires adjusted_rand_index to be available.
			- Assumes both partitions are defined over the SAME node ordering/length.
		"""
		
		#	Validation
			@assert length(partition1) == length(partition2) "Partitions must be the same length"
		
		#	Calculate ARI
			ari = adjusted_rand_index(partition1, partition2)
		
		#	Get unique labels
			labels1 = sort(unique(partition1))
			labels2 = sort(unique(partition2))
		
		#	Build confusion matrix
			n1 = length(labels1)
			n2 = length(labels2)
			confusion = zeros(Int, n1, n2)
			
			map1 = Dict(label => i for (i, label) in enumerate(labels1))
			map2 = Dict(label => i for (i, label) in enumerate(labels2))
			
			for i in 1:length(partition1)
				r = map1[partition1[i]]
				c = map2[partition2[i]]
				confusion[r, c] += 1
			end
		
		#	Calculate naive agreement rate via best-per-row matches
			n_agreed = 0
			for r in 1:n1
				n_agreed += maximum(confusion[r, :])
			end
			agreement_rate = n_agreed / length(partition1)
		
		#	Print if verbose
			if verbose
				println("\nPartition Comparison:")
				println(repeat("-", 40))
				println("Partition 1: $(n1) communities")
				println("Partition 2: $(n2) communities")
				println("Adjusted Rand Index: $(round(ari, digits=4))")
				println("Node agreement rate: $(round(agreement_rate * 100, digits=2))%")
				
				if n1 <= 10 && n2 <= 10
					println("\nConfusion Matrix (rows=P1, cols=P2):")
					for r in 1:n1
						println("  ", collect(confusion[r, :]))
					end
				end
			end
		
		return (
			ari = ari,
			n_communities_1 = n1,
			n_communities_2 = n2,
			confusion_matrix = confusion,
			agreement_rate = agreement_rate
		)
	end

#	Leident Tests Helper: Community Size table from Membership
	function _community_sizes(membership::Vector{Int})
		"""
		Args:
			membership::Vector{Int}: community label per ORIGINAL node
		Returns:
			DataFrame: (community, count) sorted by count desc
		"""
		ct = countmap(membership)
		df = DataFrame(community = collect(keys(ct)), count = collect(values(ct)))
		sort!(df, :count, rev=true)
		return df
	end

#	Test Function for Leiden Community Detection (consistency via ARI)
	function test_leiden_consistency(edges::DataFrame;
	                                 resolution::Float64=1.0,
	                                 n_tests::Int=10,
	                                 weighted::Bool=false,
	                                 verbose::Bool=true)
		"""
		Args:
			edges::DataFrame: edge list with :src and :dst columns (and optional :weight)
			resolution::Float64: resolution parameter to test (default = 1.0)
			n_tests::Int: number of independent runs (default = 10)
			weighted::Bool: use edge weights if present (default = false)
			verbose::Bool: print detailed results (default = true)
		Returns:
			NamedTuple: (
				mean_ari, std_ari, min_ari, max_ari, ari_scores,
				mean_modularity, std_modularity, modularities,
				mean_communities, std_communities, min_communities, max_communities,
				n_communities_all, partitions
			)
		Notes:
			- Uses adjusted_rand_index(partition_a, partition_b) which you export elsewhere.
			- Partitions are aligned to ORIGINAL node order if produced by leiden_community_detection().
			- Fixed for Julia semantics:
				* String repeats use repeat("=", 60) not "=" ^ 60
				* Removed unsupported keyword n_runs
				* Pre-allocates typed vectors for stability
				* Guards small-n cases for ARI stats
		"""
		
		#	Print header
			if verbose
				println(repeat("=", 60))
				println("Testing Leiden Community Detection Consistency")
				println(repeat("=", 60))
				println("Resolution: $resolution")
				println("Number of test runs: $n_tests")
				println("Weighted: $weighted")
				println(repeat("-", 60))
			end
		
		#	Store results from each run (typed for performance)
			partitions = Vector{Vector{Int}}()
			modularities = Float64[]
			n_communities = Int[]
			
		#	Run Leiden multiple times
			for i in 1:n_tests
				if verbose && i % 2 == 0
					print(".")
				end
				
				result = leiden_community_detection(
					edges;
					resolution = resolution,
					n_iterations = 10,
					weighted = weighted,
					seed = nothing   # Different seed each time
				)
				
				push!(partitions, result.membership)
				push!(modularities, result.modularity)
				push!(n_communities, result.n_communities)
			end
			
			if verbose
				println()
				println(repeat("-", 60))
			end
		
		#	Calculate pairwise ARI scores (guard n_tests < 2)
			ari_scores = Float64[]
			if length(partitions) ≥ 2
				for i in 1:(n_tests-1)
					for j in (i+1):n_tests
						push!(ari_scores, adjusted_rand_index(partitions[i], partitions[j]))
					end
				end
			end
		
		#	Calculate statistics (handle empty ARI gracefully)
			mean_ari = isempty(ari_scores) ? NaN : Statistics.mean(ari_scores)
			std_ari  = isempty(ari_scores) ? NaN : Statistics.std(ari_scores)
			min_ari  = isempty(ari_scores) ? NaN : minimum(ari_scores)
			max_ari  = isempty(ari_scores) ? NaN : maximum(ari_scores)
			
			mean_modularity = Statistics.mean(modularities)
			std_modularity  = Statistics.std(modularities)
			
			mean_communities = Statistics.mean(n_communities)
			std_communities  = Statistics.std(n_communities)
			min_communities  = minimum(n_communities)
			max_communities  = maximum(n_communities)
		
		#	Print results
			if verbose
				println("RESULTS:")
				println(repeat("-", 60))
				
				println("\nPartition Consistency (ARI):")
				println("  Mean ARI:     $(round(mean_ari, digits=4))")
				println("  Std ARI:      $(round(std_ari, digits=4))")
				println("  Min ARI:      $(round(min_ari, digits=4))")
				println("  Max ARI:      $(round(max_ari, digits=4))")
				
				println("\nModularity:")
				println("  Mean:         $(round(mean_modularity, digits=4))")
				println("  Std:          $(round(std_modularity, digits=4))")
				
				println("\nNumber of Communities:")
				println("  Mean:         $(round(mean_communities, digits=2))")
				println("  Std:          $(round(std_communities, digits=2))")
				println("  Range:        [$min_communities, $max_communities]")
				println("  All values:   $n_communities")
				
				println("\nQuality Assessment:")
				if !isnan(mean_ari) && mean_ari > 0.8
					println("  ✓ EXCELLENT: Mean ARI > 0.8 indicates highly consistent partitions")
				elseif !isnan(mean_ari) && mean_ari > 0.6
					println("  ✓ GOOD: Mean ARI > 0.6 indicates reasonably consistent partitions")
				elseif !isnan(mean_ari) && mean_ari > 0.4
					println("  ⚠ MODERATE: Mean ARI between 0.4-0.6 suggests some inconsistency")
				else
					println("  ✗ POOR: Mean ARI < 0.4 (or insufficient runs) indicates issues or high stochasticity")
				end
				
				if std_communities < mean_communities * 0.2
					println("  ✓ Community count is stable (CV < 20%)")
				else
					println("  ⚠ Community count shows high variation")
				end
				
				println(repeat("=", 60))
			end
		
		#	Return comprehensive results
			return (
				mean_ari = mean_ari,
				std_ari = std_ari,
				min_ari = min_ari,
				max_ari = max_ari,
				ari_scores = ari_scores,
				mean_modularity = mean_modularity,
				std_modularity = std_modularity,
				modularities = modularities,
				mean_communities = mean_communities,
				std_communities = std_communities,
				min_communities = min_communities,
				max_communities = max_communities,
				n_communities_all = n_communities,
				partitions = partitions
			)
	end

#	CHAMP Test Helper Function: Build DataFrame from undirected edge list (optionally weighted)
	function _df_from_edges(edges::Vector{Tuple{Int,Int}}; weighted::Bool=false, w::Float64=1.0)
		"""
		Args:
			edges::Vector{Tuple{Int,Int}}: undirected edges as (u,v) with 1 ≤ u < v ≤ N
			weighted::Bool=false: include :weight column with constant w
			w::Float64=1.0: edge weight if weighted
		Returns:
			DataFrame with columns :src, :dst [, :weight]
		Notes:
			Produces a simple undirected edgelist without self-loops or multiedges.
		"""
		src = Int[]; dst = Int[]; wt = Float64[]
		for (u,v) in edges
			u == v && continue
			push!(src, u); push!(dst, v)
			weighted && push!(wt, w)
		end
		return weighted ? DataFrame(; src=src, dst=dst, weight=wt) : DataFrame(; src=src, dst=dst)
	end

#	CHAMP Test Helper Function: Complete graph K9 (uniformly connected; one optimal community at γ≈1)
	function build_complete_graph_9(; weighted::Bool=false, w::Float64=1.0)
		"""
		Returns:
			DataFrame edgelist for K9, and ground truth membership (all ones)
		"""
		edges = Tuple{Int,Int}[]
		for u in 1:9, v in (u+1):9
			push!(edges, (u,v))
		end
		df = _df_from_edges(edges; weighted=weighted, w=w)
		gt = fill(1, 9)  # single community
		return (edges=df, ground_truth=gt, name="K9")
	end

#	CHAMP Test Helper Function: Dumbbell graph (two cliques bridged by a single edge; optimal partition = 2 comms)
	function build_dumbbell_graph_9(; weighted::Bool=false, w_in::Float64=1.0, w_bridge::Float64=1.0)
		"""
		Structure:
			Left clique: 1–4
			Right clique: 5–9
			Bridge: (4,5)
		Returns:
			DataFrame edgelist, ground truth membership (two communities)
		"""
		#	Creating Empty Edgelist for Populating
			edges = Tuple{Int,Int}[]

		#	Left clique K4
			for u in 1:4, v in (u+1):4
				push!(edges, (u,v))
			end

		#	Right clique K5
			for u in 5:9, v in (u+1):9
				push!(edges, (u,v))
			end

		#	Bridge
			push!(edges, (4,5))

		#	Adding Internal Edges
			if weighted
				#	Build DF with in-clique = w_in, bridge = w_bridge
					df_in  = _df_from_edges([(u,v) for (u,v) in edges if !((u==4 && v==5))]; weighted=true,  w=w_in)
					df_br  = _df_from_edges([(4,5)];                                 weighted=true,  w=w_bridge)
					df = vcat(df_in, df_br)
			else
				#	Building Binary Community Edges
					df = _df_from_edges(edges; weighted=false)
			end

		#	Return Dumbell Graph
			gt = [1,1,1,1, 2,2,2,2,2]  # 1–4 vs 5–9
			return (edges=df, ground_truth=gt, name="Dumbbell")
	end

#	CHAMP Test Helper Function: Three cliques (3-3-3) weakly bridged in a triangle
	function build_clique_triangle_9(; weighted::Bool=false, w_in::Float64=1.0, w_bridge::Float64=0.2)
		"""
		Structure:
			Three fully connected cliques:
				C₁ = {1,2,3}, C₂ = {4,5,6}, C₃ = {7,8,9}
			Weak inter-clique bridges forming a triangle:
				(3,4), (6,7), (9,1)
		Args:
			weighted::Bool=false: include :weight column
			w_in::Float64=1.0:    intra-clique edge weight
			w_bridge::Float64=0.2:inter-clique bridge weight (set small to favor 3 modules)
		Returns:
			NamedTuple: (edges::DataFrame, ground_truth::Vector{Int}, name::String)
		Notes:
			Heuristic ground truth is 3 communities of size 3: [1,1,1, 2,2,2, 3,3,3].
			Increase γ or decrease w_bridge to make the 3-way split more pronounced.
		"""
		#	Define cliques
			C1 = 1:3; C2 = 4:6; C3 = 7:9

		#	Intra-clique edges (undirected, u < v)
			edges_in = Tuple{Int,Int}[]
			for C in (C1, C2, C3)
				for u in C, v in (u+1):last(C)
					push!(edges_in, (u, v))
				end
			end

		#	Weak triangle bridges
			edges_br = Tuple{Int,Int}[(3,4), (6,7), (9,1)]

		#	Build DataFrame (weighted or unweighted)
			if weighted
				df_in = _df_from_edges(edges_in; weighted=true, w=w_in)
				df_br = _df_from_edges(edges_br; weighted=true, w=w_bridge)
				df = vcat(df_in, df_br)
			else
				df = _df_from_edges(vcat(edges_in, edges_br); weighted=false)
			end

		#	Ground truth: 3 cliques
			gt = [1,1,1, 2,2,2, 3,3,3]

		return (edges=df, ground_truth=gt, name="CliqueTriangle9")
	end

#	CHAMP Test Harness: Sanity Checks on Three Stylized Graphs
	function run_champ_test_harness(; weighted::Bool=false,
	                                 n_runs_per_gamma::Int=5,
	                                 n_iterations_per_run::Int=10,
	                                 resolution_range::Tuple{Float64,Float64}=(0.5,1.8),
	                                 n_resolutions::Int=15,
	                                 seed::Union{Int,Nothing}=42,
	                                 verbose::Bool=true)
		"""
		Args:
			weighted::Bool=false: pass weights through to detection
			n_runs_per_gamma::Int=5: multi-start per γ
			n_iterations_per_run::Int=10: Leiden iterations per run
			resolution_range::Tuple=(0.5,1.8): default γ sweep
			n_resolutions::Int=15: points in sweep
			seed::Union{Int,Nothing}=42: base RNG seed
			verbose::Bool=true: print results
		Returns:
			NamedTuple with results for each graph (K9, Dumbbell, CliqueTriangle9)
		Notes:
			• K9: expect n≈1 (at γ=1, Q≈0; CHAMP may choose γ<1 ⇒ higher Q)
			• Dumbbell: expect n≈2, Q moderately high
			• CliqueTriangle9: expect n≈3 with weak bridges (higher γ or weaker bridges helps)
		"""
		#	Build fixtures
			K9            = build_complete_graph_9(; weighted=weighted)
			Dumbbell      = build_dumbbell_graph_9(; weighted=weighted, w_in=1.0, w_bridge=1.0)
			CliqueTriangle= build_clique_triangle_9(; weighted=weighted, w_in=1.0, w_bridge=0.2)

		#	Run CHAMP on each (supports per-graph sweep override)
			function _run_one(name, edf, gt; rr=resolution_range, nres=n_resolutions)
				#	Run CHAMP
					res = champ_community_detection(
						edf;
						resolution           = nothing,
						resolution_range     = rr,
						n_resolutions        = nres,
						weighted             = weighted,
						agg_func             = nothing,
						n_runs_per_gamma     = n_runs_per_gamma,
						n_iterations_per_run = n_iterations_per_run,
						seed                 = seed
					)

				#	Align membership to canonical node order 1..N before ARI
					N = maximum(vcat(Vector(edf.src), Vector(edf.dst)))
					pos = Dict{Int,Int}()	#	node id → index in res vectors
					for (i, id) in enumerate(res.node_names)
						pos[Int(id)] = i
					end
					aligned = Vector{Int}(undef, N)
					for u in 1:N
						aligned[u] = res.membership[pos[u]]
					end

				#	Compute ARI using exported function (if GT provided)
					ari = (isnothing(gt) || isempty(gt)) ? NaN : adjusted_rand_index(aligned, gt)

				return (name=name, result=res, ari_vs_gt=ari)
			end

			rK = _run_one(K9.name,            K9.edges,            K9.ground_truth)                          #	default sweep
			rD = _run_one(Dumbbell.name,      Dumbbell.edges,      Dumbbell.ground_truth)                    #	default sweep
			rT = _run_one(CliqueTriangle.name, CliqueTriangle.edges, CliqueTriangle.ground_truth;             #	nudged higher-γ sweep
			              rr=(0.9, 2.0), nres=max(n_resolutions, 21))

		#	Print summary
			if verbose
				println("=" ^ 60)
				println("CHAMP Sanity Tests (n=9)  |  weighted=$(weighted)  runs/γ=$(n_runs_per_gamma)  iters/run=$(n_iterations_per_run)")
				println("=" ^ 60)

				for r in (rK, rD, rT)
					res = r.result
					println("\n[", r.name, "]")
					println("  γ*           : ", round(res.resolution_used, digits=4))
					println("  communities  : ", res.n_communities)
					println("  modularity Q : ", round(res.modularity, digits=4))
					if !isnan(r.ari_vs_gt)
						println("  ARI vs GT    : ", round(r.ari_vs_gt, digits=4))
					end
				end

				println("\nHeuristics:")
				println("  • K9             → expect n=1; at γ=1, Q≈0 (CHAMP may pick γ<1 ⇒ Q>0)")
				println("  • Dumbbell       → expect n=2, Q moderate/high, ARI≈1.0 vs [1..4|5..9]")
				println("  • CliqueTriangle → expect n=3 with weak bridges; raise γ or lower w_bridge if merged")
				println("=" ^ 60)
			end

		#	Return Small Graph Solutions
			return (K9=rK, Dumbbell=rD, CliqueTriangle=rT)
	end

#	Modularity Vitality Tests: Comparing m calculations
	function diagnose_m_calculation(edges::DataFrame)
		"""
		Args:
			edges::DataFrame: edge list with :src, :dst, optional :weight
		Returns:
			Tuple: (m_original, m_from_A, m_with_diag, m_no_loops)
		Notes:
			Compares different methods for computing m (total edge weight / 2):
			- Original edge sum
			- From getSparseA (sum(A)/2)
			- From A with diagonal adjustment ((sum(A)+sum(diag(A)))/2)
			- From A excluding self-loops
		"""

		#	Method 1: From original edges (raw input)
			if hasproperty(edges, :weight)
				m_original = sum(Float64.(edges.weight))
			else
				m_original = nrow(edges)
			end

		#	Method 2: From getSparseA (Matt-faithful adjacency)
			A = getSparseA(edges)
			m_from_A = sum(A) / 2.0

		#	Method 3: Add back diagonal mass explicitly (loop weights contribute twice)
			m_with_diag = (sum(A) + sum(diag(A))) / 2.0

		#	Method 4: Remove all self-loops before summing
			A_no_diag = copy(A)
			for i in 1:size(A, 1)
				A_no_diag[i, i] = 0.0
			end
			m_no_loops = sum(A_no_diag) / 2.0

		#	Diagnostics summary
			println("=== m calculation diagnostics ===")
			println("Method 1 (original edges):        $m_original")
			println("Method 2 (sum(A)/2):              $m_from_A")
			println("Method 3 (sum(A)+diag)/2:         $m_with_diag")
			println("Method 4 (no self-loops, sum/2):  $m_no_loops")
			println("Number of self-loops:             $(sum(edges.src .== edges.dst))")
			println("Sum of diagonal (Aii):            $(sum(diag(A)))")

		#	Return Different M 
			return (m_original, m_from_A, m_with_diag, m_no_loops)
	end

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

#	Local Weighted Reciprocity (Squartini et al., 2013)
	ego_reciprocity = local_weighted_reciprocity(agent_agent_all_com.edges; normalize=:rank)

#   COMPARISON TESTS

#	Import Comparison Data
	ora_local_clustering = CSV.read("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data/Density_Local_Clustering.csv", DataFrame; types=Dict(1 => String))
	ora_local_clustering = ora_local_clustering[:,(1:3)]
	rename!(ora_local_clustering, ["node", "screen_name", "clusteringCoefficient-1-Balikatan_2022"])

	wgt_local_clustering = CSV.read("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data/clustering_comparison.csv", DataFrame)
	
	igraph_wgt_local_clustering = CSV.read("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data/Balikatan_Clemente_Grassi.csv",DataFrame; types=Dict(1 => String))
	rename!(igraph_wgt_local_clustering, ["node", "cg_cycle_ig", "cg_middleman_ig",  "cg_in_ig", "cg_out_ig", "cg_total_ig", "barrat_local_ig"])

#	Construct CG Clustering Toy Graph
	test_edges = DataFrame(src = ["A","A","B","B","C","D","E"], dst = ["B","C","C","D","A","A","B"],
      				       weight = [1.0, 5.0, 2.0, 3.0, 1.0, 4.0, 1.0])

#	Local Clustering
	strogatz_local_clustering = local_clustering_coefficient(agent_agent_all_com.edges, directed=true, weighted=false)
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

	leftjoin!(cg_clustering_coefficients, igraph_wgt_local_clustering, on=:node)
	cg_clustering_coefficients.cg_cycle_ig = convert.(Float64, cg_clustering_coefficients.cg_cycle_ig)
	cg_clustering_coefficients.cg_middleman_ig = convert.(Float64, cg_clustering_coefficients.cg_middleman_ig)
	cg_clustering_coefficients.cg_in_ig = convert.(Float64, cg_clustering_coefficients.cg_in_ig)
	cg_clustering_coefficients.cg_out_ig = convert.(Float64, cg_clustering_coefficients.cg_out_ig)
	cg_clustering_coefficients.cg_total_ig = convert.(Float64, cg_clustering_coefficients.cg_total_ig)
	cg_clustering_coefficients.barrat_local_ig = convert.(Float64, cg_clustering_coefficients.barrat_local_ig)

	all_comm_delta_scores = DataFrame(node = cg_clustering_coefficients.node,
									  cg_cycle_delta = cg_clustering_coefficients.cg_cycle .- cg_clustering_coefficients.cg_cycle_ig,
			  						  cg_middleman_delta = cg_clustering_coefficients.cg_middleman .- cg_clustering_coefficients.cg_middleman_ig,
									  cg_in_delta = cg_clustering_coefficients.cg_in .- cg_clustering_coefficients.cg_in_ig,
									  cg_out_delta = cg_clustering_coefficients.cg_out .- cg_clustering_coefficients.cg_out_ig,
									  cg_total_delta = cg_clustering_coefficients.cg_total .- cg_clustering_coefficients.cg_total_ig,
									  barrat_local_delta = cg_clustering_coefficients.barrat_local .- cg_clustering_coefficients.barrat_local_ig)

#   Local Reciprocity (Fraction of Reciprocated Edges)
	test_local_weighted_reciprocity()

####################################################
#   MEASURE TESTS: INFLUENCE CENTRALITY MEASURES   #
####################################################

#	CALCULATE INFLUENCE MEASURES

#	ORA-Style Local Page Rank
	page_rank_scores_local = pagerank_local_ora(agent_agent_all_com.edges; mode=:in, weighted=true)
	page_rank_local_df = DataFrame(node = page_rank_scores_local.node_names, page_rank=page_rank_scores_local.scores)

#	Component Scaled Page Rank
	page_rank_scores_scaled = pagerank_stitched(agent_agent_all_com.edges;  mode=:in, weighted=true, stitch_by=:nodes)
	page_rank_scale_df = DataFrame(node = page_rank_scores_scaled.node_names, page_rank = page_rank_scores_scaled.scores)

#	Hub Centrality: SALSA
	hub_centrality = salsa_centrality(agent_agent_all_com.edges; score=:hub)
		
#	Authority Centrality: SALSA
	authority_centrality = salsa_centrality(agent_agent_all_com.edges; score=:authority)

#	Leiden Community Detection
	all_comm_communities = leiden_community_detection(agent_agent_all_com.edges; n_iterations=10, n_runs=5, resolution=1.0, weighted=false)
	community_index = DataFrame(node = all_comm_communities.node_names, community = all_comm_communities.membership)
	comm_sizes = combine(groupby(community_index, :community), nrow => :count)
	sort!(comm_sizes, :count, rev = true)

	all_comm_communities_weighted = leiden_community_detection(agent_agent_all_com.edges; n_iterations=10, n_runs=5, resolution=1.0, weighted=true)
	community_index = DataFrame(node = all_comm_communities_weighted.node_names, community = all_comm_communities_weighted.membership)
	comm_sizes = combine(groupby(community_index, :community), nrow => :count)
	sort!(comm_sizes, :count, rev = true)

#	Modularity Vitality: Fixed Resolution & Sweep
	modularity_vitality(agent_agent_all_com.edges; resolution_sweep=false, resolution=1.0)
	modularity_vitality(agent_agent_all_com.edges; resolution_sweep=true, n_resolutions=20, weighted=true)

#	CONDUCT TESTS

#	Local Page Rank Comparisons: ORA vs. Julia
	ora_page_rank = CSV.read("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data/Balikatan_2022_All_Comm_PageRank.csv", DataFrame; types=Dict(1 => String))
	rename!(ora_page_rank, ["node", "Centrality, PageRank"])	
	leftjoin!(page_rank_local_df, ora_page_rank, on=:node)
	page_rank_local_df[!,3] = convert.(Float64, page_rank_local_df[:,3])
	page_rank_local_df.delta = abs.(page_rank_local_df[:,2] .-  page_rank_local_df[:,3])
	maximum(page_rank_local_df.delta)

	ρ = corspearman(Float64.(page_rank_local_df[:,2]), Float64.(page_rank_local_df[:,3]))
	println("Spearman Rank Correlation: ", round(ρ, digits=6))

#	Component Scaled Page Rank Tests
	test_pagerank_stitched()

#	SALSA Tests
	test_salsa()

#	Testing Leiden Community Detection: Unweighted
	res_unw = leiden_community_detection(agent_agent_all_com.edges; resolution=1.0, weighted=false, seed=42)
	println("Unweighted modularity: ", res_unw.modularity)
	display(_community_sizes(res_unw.membership))

	t_unw = test_leiden_consistency(agent_agent_all_com.edges; resolution=1.0, n_tests=10, weighted=false, verbose=true)

#	Testing Leiden Community Detection: Weighted
	res_w = leiden_community_detection(agent_agent_all_com.edges; resolution=1.0, weighted=true, seed=42)
	println("Unweighted modularity: ", res_w.modularity)
	display(_community_sizes(res_w.membership))

	t_w = test_leiden_consistency(agent_agent_all_com.edges; resolution=1.0, n_tests=10, weighted=true, verbose=true)

#	ORA Comparision Test: ARI of 0.95799
	all_comm_communities_weighted = leiden_community_detection(agent_agent_all_com.edges; resolution=1.0, weighted=true)
	community_index = DataFrame(node = all_comm_communities_weighted.node_names, community = all_comm_communities_weighted.membership)
	ora_leiden = CSV.read("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data/All_Comm_Lieden_Group_Assignments.csv", DataFrame, types=Dict(1 => String))
	rename!(ora_leiden, ["node", "leiden_group"])
	leftjoin!(community_index, 	ora_leiden, on=:node)
	community_index.leiden_group = convert.(Int64, community_index.leiden_group)
	ora_ari = adjusted_rand_index(community_index.community, community_index.leiden_group)
	print(ora_ari)

#	CHAMP Tests
	run_champ_test_harness(; weighted=false)
	run_champ_test_harness(; weighted=true)

	all_comm_CHAMP = champ_community_detection(agent_agent_all_com.edges; resolution = nothing, resolution_range     = (0.5,1.8),
						                       n_resolutions = 15, weighted = true, n_runs_per_gamma = 10, n_iterations_per_run = 10,
							                   seed = 45)

#	Modularity Vitality Tests: Python Tests
	community_index = CSV.read("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data/balikatan_all_comm_partition_leiden_gamma1.csv",
							   DataFrame, types=Dict(1 => String))
	rename!(community_index, ["node", "community"])
	community_index.community = community_index.community .+ 1
	fixed_modularity = modularity_vitality(agent_agent_all_com.edges; resolution_sweep=false, resolution=1.0, 
										   provided_membership= community_index)

	fixed_modularity = modularity_vitality(agent_agent_all_com.edges; resolution_sweep=false, resolution=1.0)

#	Modularity Vitality Function Tests
	edges = agent_agent_all_com.edges
	partition = CSV.read("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data/balikatan_all_comm_partition_leiden_gamma1.csv",
							               DataFrame, types=Dict(1 => String))
	rename!(partition, ["node", "community"])
	expected_sizes = Dict(0=>388, 2=>193, 5=>137, 16=>118)
	perform_sanity_checks = true
	node_col = :node
	community_col = :community
	sentinel_ids =  ["828033366712688640", 24112747, 25930421, 18749026]

#	Helper Function for modularity_vitality: getSparseA(edges) → A
	function getSparseA(edges::DataFrame; 
                    test_flag::Bool = false,
                    sentinel_node::AbstractString = "828033366712688640",  # MyriadCsPhantom
                    selfloop_node::AbstractString = "INDOPACOM")
		"""
		Args:
			edges::DataFrame: edge list with :src, :dst, optional :weight
			test_flag::Bool: when true, run dataset-specific sanity checks (e.g., MyriadCsPhantom, INDOPACOM)
			sentinel_node::AbstractString: node ID used for targeted checks (default = "828033366712688640")
			selfloop_node::AbstractString: node ID for self-loop checks (default = "INDOPACOM")
		Returns:
			SparseMatrixCSC{Float64,Int}
		Notes:
			Builds a symmetric adjacency matrix A matching Matt's implementation:
			- Aggregate duplicate edges by sum
			- Halve self-loops before symmetrization
			- Symmetrize by addition (A = A + A')
			- Returns A where diagonal equals the *original* loop weight
			General invariants (symmetry, shape, non-negativity, NaNs) always run.
			Dataset-specific checks only run when `test_flag=true`.
		"""

		#	Aggregate by sum
			clean_edges = _aggregate_multi_edges(edges; agg_func=sum)

		#	Ensure weight column exists
			if !hasproperty(clean_edges, :weight)
				clean_edges.weight = ones(Float64, nrow(clean_edges))
			else
				clean_edges.weight = Float64.(clean_edges.weight)
			end

		#	Store original self-loop weights (for optional verification)
			self_mask = clean_edges.src .== clean_edges.dst
			original_self_loops = Dict{Any,Float64}()
			if any(self_mask)
				for i in findall(self_mask)
					original_self_loops[clean_edges.src[i]] = clean_edges.weight[i]
				end
			end
			if test_flag && haskey(original_self_loops, selfloop_node)
				println("DEBUG getSparseA: $selfloop_node original self-loop weight = $(original_self_loops[selfloop_node])")
			end

		#	Halve self-loops before symmetrization
			clean_edges.weight[self_mask] ./= 2.0

		#	Optional: Verify halving worked for designated self-loop node
			if test_flag && haskey(original_self_loops, selfloop_node)
				idx = findfirst((clean_edges.src .== selfloop_node) .& (clean_edges.dst .== selfloop_node))
				if idx !== nothing
					println("DEBUG getSparseA: $selfloop_node self-loop after halving = $(clean_edges.weight[idx])")
					@assert clean_edges.weight[idx] ≈ original_self_loops[selfloop_node] / 2.0 "Self-loop halving failed for $selfloop_node"
				end
			end

		#	Calculate expected sum before symmetrization
			sum_before_symmetry = sum(clean_edges.weight)
			if test_flag
				println("DEBUG getSparseA: Sum of edge weights after halving = $sum_before_symmetry")
			end

		#	Build directed adjacency
			adj_dir, node_map, _ = _edgelist_to_sparse_matrix(clean_edges; weighted=true)

		#	Optional: Sentinel node checks (pre-symmetrization)
			if test_flag
				myriad_idx = get(node_map, sentinel_node, nothing)
				if myriad_idx !== nothing
					println("DEBUG getSparseA: Sentinel '$sentinel_node' mapped to matrix index $myriad_idx")
					row_sum = sum(adj_dir[myriad_idx, :])
					col_sum = sum(adj_dir[:, myriad_idx])
					println("DEBUG getSparseA: Sentinel row sum (out-edges) = $row_sum")
					println("DEBUG getSparseA: Sentinel col sum (in-edges) = $col_sum")
				else
					println("DEBUG getSparseA: Sentinel '$sentinel_node' not present in node_map")
				end
			end

		#	Symmetrize by addition
			A = adj_dir + adj_dir'

		#	Optional: Verify diagonal equals original self-loop for designated node
			if test_flag && haskey(node_map, selfloop_node)
				ind_idx = node_map[selfloop_node]
				diag_val = A[ind_idx, ind_idx]
				println("DEBUG getSparseA: $selfloop_node diagonal after symmetrization = $diag_val")
				if haskey(original_self_loops, selfloop_node)
					@assert abs(diag_val - original_self_loops[selfloop_node]) < 1e-10 "Diagonal should equal original self-loop weight for $selfloop_node"
				end
			end

		#	Total sum after symmetrization (general invariant)
			total_sum = sum(A)
			expected_sum = 2 * sum_before_symmetry
			if test_flag
				println("DEBUG getSparseA: Total sum of A = $total_sum")
				println("DEBUG getSparseA: Expected sum (2 * edge weights) = $expected_sum")
			end
			@assert abs(total_sum - expected_sum) < 1e-10 "Sum mismatch after symmetrization"

		#	Optional: Sentinel node degree and specific neighbor probes
			if test_flag
				myriad_idx = get(node_map, sentinel_node, nothing)
				if myriad_idx !== nothing
					myriad_degree = sum(A[myriad_idx, :])
					println("DEBUG getSparseA: Sentinel '$sentinel_node' total degree in A = $myriad_degree")
					#	Only assert the degree if this dataset is expected to have degree 3
					@assert abs(myriad_degree - 3.0) < 1e-10 "Sentinel '$sentinel_node' should have degree 3 in this test dataset"

					#	Try common neighbors if present in map
					for nbr in (selfloop_node, "PACAF", "US7thFleet")
						if haskey(node_map, nbr)
							nbr_idx = node_map[nbr]
							println("DEBUG getSparseA: A[sentinel, $nbr] = $(A[myriad_idx, nbr_idx])")
						end
					end
				end
			end

		#	General assertions to verify construction (always on)
			@assert issymmetric(A) "getSparseA: adjacency matrix must be symmetric"
			@assert sum(A .< 0.0) == 0 "getSparseA: adjacency matrix must not contain negative weights"
			@assert size(A,1) == size(A,2) "getSparseA: adjacency matrix must be square"
			@assert !any(isnan, A.nzval) "getSparseA: adjacency matrix contains NaN values"

		#	Optional: Summary
			if test_flag
				println("DEBUG getSparseA: Matrix dimensions = $(size(A))")
				println("DEBUG getSparseA: Number of non-zeros = $(nnz(A))")
			end

		#	Return Symmetrized & Self-Loop Halved Adjacency Matrix
			return A
	end

	A = getSparseA(edges, test_flag=false)

#	Helper Function for modularity_vitality: getGroupIndicator
	function getGroupIndicator(edges::DataFrame, A::SparseMatrixCSC,
	                           partition::DataFrame;
	                           node_col::Symbol = :node,
	                           community_col::Symbol = :community,
	                           expected_sizes::Union{Nothing,Dict{Int,Int}} = Dict(0=>388, 2=>193, 5=>137, 16=>118),
	                           perform_sanity_checks::Bool = true,
	                           test_flag::Bool = false)
		"""
		Args:
			A::SparseMatrixCSC: symmetric adjacency; used for node count n (= size(A,1))
			partition::DataFrame: two columns — node IDs and community labels
			node_col::Symbol: column name for node IDs (default = :node)
			community_col::Symbol: column name for community labels (default = :community)
			expected_sizes::Union{Nothing,Dict{Int,Int}}: map of expected community sizes by original label (default checks 0,2,5,16)
			perform_sanity_checks::Bool: run general validations (row one-hotness, empty columns)
			test_flag::Bool: when true, run **dataset-specific** checks (e.g., expected_sizes for known communities)
		Returns:
			SparseMatrixCSC{Float64,Int}: indicator matrix S (n×C), one-hot per node’s community
		Notes:
			- Community labels in `partition` may be arbitrary (e.g., 0, 2, 5, 16). This function remaps them to contiguous 1..C columns internally.
			- Row order is determined by node indices 1..n derived from `edges`; the `partition` may arrive in any order.
			- **Dataset-specific checks** (like assertions about specific community sizes) only run when `test_flag=true`.
		"""

		#	Validation
			n = size(A, 1)
			if size(A,1) != size(A,2)
				throw(ArgumentError("get_group_indicator: A must be square"))
			end
			if !(hasproperty(partition, node_col) && hasproperty(partition, community_col))
				throw(ArgumentError("get_group_indicator: partition must have columns $(node_col) and $(community_col)"))
			end

		#	Construct Partition Index (Input Partitions May Include Isolates)
			clean_edges = _aggregate_multi_edges(edges; agg_func=sum)
			adj, node_to_idx, idx_to_node = _edgelist_to_sparse_matrix(clean_edges;)
			if length(idx_to_node) != n
				throw(ArgumentError("get_group_indicator: node index derived from edges (=$(length(idx_to_node))) does not match A’s size n=$(n). Pass edges consistent with A, or provide an explicit node index mapping for A."))
			end

			node_index  = DataFrame(node = idx_to_node)
			leftjoin!(node_index, partition, on=:node)

			nodes = node_index[!, node_col]
			comms = node_index[!, community_col]

			if length(nodes) != n || length(comms) != n
				throw(ArgumentError("get_group_indicator: partition (after join) must have exactly n=$(n) rows (one per node in A)"))
			end

		#	Ensure every node has a community (isolates included)
			if any(ismissing, comms)
				missing_nodes = collect(node_index.node[ismissing.(comms)])
				throw(ArgumentError("get_group_indicator: missing community assignments for $(length(missing_nodes)) node(s) present in A (examples: $(first(missing_nodes, min(5, length(missing_nodes)))))"))
			end

		#	Normalize community type to Int (after confirming no missings)
			node_index[!, community_col] = convert.(Int, node_index[!, community_col])
			comms = node_index[!, community_col]

		#	Remap community labels to contiguous 1..C
			labels = sort(unique(comms))
			C = length(labels)
			label_to_col = Dict{eltype(labels),Int}(lab => i for (i, lab) in enumerate(labels))

		#	Build membership vector m (1..C) aligned to node index
		#	Community size tally (original labels; useful for debug / expected_sizes)
			community_index = combine(groupby(node_index, [community_col]), nrow => :size)

			m = Vector{Int}(undef, n)
			for i in 1:n
				m[i] = label_to_col[comms[i]]
			end

		#	Construct one-hot indicator S
			vals = ones(Float64, n)
			S = sparse(collect(1:n), m, vals, n, C)

		#	Sanity checks (general invariants always under perform_sanity_checks;
		#	Dataset-specific checks only when test_flag is true)
			if perform_sanity_checks
				#	One-hot per row
					row_sums = vec(sum(S, dims=2))
					if any(abs.(row_sums .- 1.0) .> eps(Float64))
						throw(AssertionError("get_group_indicator: each row of S must sum to 1"))
					end

				#	No empty columns for observed labels
					col_sums = vec(sum(S, dims=1))
					if any(col_sums .== 0.0)
						throw(AssertionError("get_group_indicator: found empty community column(s) after remapping"))
					end

				#	Dataset-specific assertions (e.g., known community sizes)
					if test_flag && expected_sizes !== nothing
						for (lab, expected) in expected_sizes
							if haskey(label_to_col, lab)
								col = label_to_col[lab]
								actual = Int(round(col_sums[col]))
								@assert actual == expected "get_group_indicator: community '$lab' size mismatch (actual=$actual, expected=$expected)"
							end
						end
					end
			end

		#	Return indicator matrix
			return S
	end

	S = getGroupIndicator(edges, A, partition; test_flag = false)
	
#	Helper Function for modularity_vitality: getDegMat(node_deg_by_group, rows, cols; …) → (degrees, deg_mat)
	function getDegMat(edges::DataFrame, S::SparseMatrixCSC, A::Union{Nothing,SparseMatrixCSC};
                   test_flag::Bool = false, sentinel_ids::Vector{String} = ["828033366712688640", "24112747", "25930421", "18749026"])
		"""
		Args:
			edges::DataFrame: edge list used to derive ID→index mapping for debug output
			S::SparseMatrixCSC: n×C one-hot indicator matrix (rows = nodes, cols = communities)
			A::Union{Nothing,SparseMatrixCSC}: n×n symmetric adjacency used to compute A*S

			test_flag::Bool: when true, run dataset-specific reporting (sentinel nodes, K_c / E_c)
			sentinel_ids::Vector{Any}: node IDs to report (defaults include MyriadCsPhantom & ego)

		Returns:
			Tuple:
				degrees::Vector{Float64}            # length n, total network degree per node
				deg_mat::SparseMatrixCSC{Float64,Int}  # n×C with degree at (i, cols[i]), zeros elsewhere

		Notes:
			- Total network degree per node: degrees[i] = sum_j A[i,j]
			- Community-internal degree for node i: (A*S)[i, cols[i]]
			- When `test_flag=true`, the function reports for sentinels:
				• k_i (total) and k_i^comm (internal)
				• K_c = ∑_{i∈c} k_i and
				  E_c = (∑_{i∈c} (A*S)[i,c] + ∑_{i∈c} A[i,i]) / 2
			  (matches “add self-loops, then divide by 2” convention).
		"""

		#	Validation
			@assert A !== nothing "getDegMat: A must be provided (needed to compute A * S)"

		#	Calculate Node Degree by Group
			node_deg_by_group = A * S

		#	Derive n, m, rows, cols from A and S (explicit, no prior state)
			n = size(A, 1)
			I, J, _ = findnz(S)
			m = zeros(Int, n)
			for k in eachindex(I)
				m[I[k]] = J[k]
			end

			rows = collect(1:n)
			cols = m

		#	Validation
			n, C = size(node_deg_by_group)
			if length(rows) != n || length(cols) != n
				throw(ArgumentError("getDegMat: rows/cols must have length n (n=$(n))"))
			end
			if any(x -> x < 1 || x > C, cols)
				throw(ArgumentError("getDegMat: cols must be in 1..C (C=$(C))"))
			end

		#	Compute degrees (network totals)
			degrees = vec(sum(node_deg_by_group, dims=2))  # size n

		#	Assemble degree placement matrix
			deg_mat = sparse(rows, cols, degrees, n, C)

		#	Dataset-specific reporting (sentinels; K_c / E_c)
			if test_flag
				#	Build external ID → row index mapping from edges (for readable debug)
					clean_edges = _aggregate_multi_edges(edges; agg_func=sum)
					_, node_to_idx, _ = _edgelist_to_sparse_matrix(clean_edges;)

				#	Helper: resolve external ID to row index (try exact key and stringified key)
					_resolve_index = let node_to_idx = node_to_idx
						id -> begin
							if haskey(node_to_idx, id)
								node_to_idx[id]
							elseif !(id isa AbstractString) && haskey(node_to_idx, string(id))
								node_to_idx[string(id)]
							else
								nothing
							end
						end
					end

				#	Looping Over Check Nodes
					for sid in sentinel_ids
						#	Setting-Up Tests
							i = _resolve_index(sid)
							if i === nothing
								println("DEBUG getDegMat: sentinel '", sid, "' not found in node_to_idx")
								continue
							end

							c = cols[i]
							k_i_total  = degrees[i]
							k_i_comm   = node_deg_by_group[i, c]

							println("DEBUG getDegMat: node=", sid,
									" (row ", i, ", comm ", c, ")",
									" | k_i (total) = ", k_i_total,
									" | k_i^comm (internal) = ", k_i_comm)

						#	Report K_c and E_c for this node's community
							in_c = findall(j -> cols[j] == c, 1:n)

							#	K_c: sum of degrees of community members
								K_c = sum(degrees[in_c])

							#	E_c via your convention
								sum_internal_deg = sum(node_deg_by_group[j, c] for j in in_c)
								sum_self_loops_c = sum(A[j, j] for j in in_c)
								E_c = (sum_internal_deg + sum_self_loops_c) / 2

								println("DEBUG getDegMat: community ", c,
										" | K_c (sum degrees) = ", K_c,
										" | E_c (internal weight) = ", E_c)
					end
			end

		#	Return result
			return degrees, deg_mat
	end

	degrees, deg_mat = getDegMat(edges, S, A, test_flag = false)

#	Helper Function for modularity_vitality: newMods(edges, A, S, resolution; …) → q1s (Q after removing each node)
	function newMods(edges::DataFrame,
	                 A::SparseMatrixCSC,
	                 S::SparseMatrixCSC,
	                 resolution::Float64;
	                 test_flag::Bool = false,
	                 sentinel_id::AbstractString = "828033366712688640")
		"""
		Args:
			edges::DataFrame: edge list (used for readable debug ID↔index mapping when test_flag=true)
			A::SparseMatrixCSC: n×n symmetric adjacency from getSparseA (self-loop convention preserved)
			S::SparseMatrixCSC: n×C one-hot group indicator from getGroupIndicator (rows sum to 1)
			resolution::Float64: γ parameter for calculate_modularity (used for Q₀ reporting in tests)

			test_flag::Bool: when true, prints a full walk-through for `sentinel_id`
			sentinel_id::AbstractString: external node ID to trace (default = "828033366712688640")

		Returns:
			Vector{Float64}: q1s of length n, where q1s[i] is the **modularity after removing node i**
			                 (identical to Matt Magelinski’s newMods return; vitality = Q₀ - q1s[i])

		Notes:
			- Follows Matt’s Python implementation algebra 1:1:
				A*S → node_deg_by_group
				internal_edges = (Σ_i (A*S)[i, m[i]] + Σ_i A[i,i]) / 2
				degrees from getDegMat; augment node_deg_by_group with deg_mat
				group_degs = (deg_mat + Diag(diag(A)) * S) column-sums
				internal_deg = (node_deg_by_group_aug[i, m[i]] - degrees[i])
				q1_links, q1_degrees, q1s per-node as in original code
			- Q₀ is computed **only for test reporting** via calculate_modularity(A, m, γ).
		"""

		#	Validation
			@assert issymmetric(A) "newMods: A must be symmetric"
			n = size(A, 1)
			@assert size(A,2) == n "newMods: A must be square"
			@assert size(S,1) == n "newMods: S must have n rows"

		#	S should be one-hot by row (light check)
			if test_flag
				rs = vec(sum(S, dims=2))
				if any(abs.(rs .- 1.0) .> eps(Float64))
					throw(AssertionError("newMods: S must be one-hot per row"))
				end
			end

		#	Mass and membership
			mass = sum(A) / 2.0
			if mass == 0.0
				return zeros(Float64, n)
			end

			I, J, _ = findnz(S)		# recover membership (contiguous 1..C)
			m = zeros(Int, n)
			for k in eachindex(I)
				m[I[k]] = J[k]
			end
			C = maximum(m)

		#	Node-degree-by-group and per-node totals (reuse your helper for consistency)
			degrees, deg_mat = getDegMat(edges, S, A; test_flag = false)	# prints elsewhere if requested
			node_deg_by_group = A * S

		#	Self-loops & internal edges (E_total)
		#  	The linear index trick above selects (i, m[i]) for i=1..n on an n×C matrix column-major.
			self_loops = sum(diag(A))
			internal_edges = (sum(node_deg_by_group[collect(1:n) .+ (m .- 1) .* n]) + self_loops) / 2.0
		
		#	Augment node_deg_by_group with deg_mat (Matt’s trick)
			node_deg_by_group_aug = node_deg_by_group + deg_mat

		#	Group degrees (per community) = (deg_mat + Diag(diag(A)) * S) column sums
			group_degs_mat = deg_mat + (spdiagm(0 => diag(A)) * S)
			group_degs = vec(sum(group_degs_mat, dims=1))	# length C, Float64

		#	Internal degree per node to its own community
			internal_deg = Array{Float64}(undef, n)
			for i in 1:n
				internal_deg[i] = node_deg_by_group_aug[i, m[i]] - degrees[i]
			end

		#	Star-center guard (avoid divide-by-zero when degrees[i] == mass)
			starCenter = degrees .== mass
			deg_safe = copy(degrees)
			deg_safe[starCenter] .= 0.0

		#	q1_links term
			q1_links = (internal_edges .- internal_deg) ./ (mass .- deg_safe)

		#	Expected_impact term using expanded form:
		# 	term1: sum(group_degs.^2) — scalar
			term1 = sum(group_degs .^ 2)

		# 	term2: 2 * (node_deg_by_group_aug * group_degs) — vector length n, then scalar multiply by -1
			term2_vec = vec(node_deg_by_group_aug * group_degs)	# length n

		#	 term3: row-wise sum of squares of node_deg_by_group_aug
			term3 = vec(sum(node_deg_by_group_aug .* node_deg_by_group_aug, dims=2))	# length n

			expected_impact = term1 .- 2.0 .* term2_vec .+ term3

		#	q1_degrees term
			den = 4.0 .* (mass .- deg_safe) .^ 2
			q1_degrees = expected_impact ./ den

		#	Final q1s (Q after removal)
			q1s = q1_links .- q1_degrees
			q1s[starCenter] .= 0.0

		#	Optional: test walk-through for sentinel_id
			if test_flag
				#	Readable externalID → rowIndex map
					clean_edges = _aggregate_multi_edges(edges; agg_func=sum)
					_, node_to_idx, _ = _edgelist_to_sparse_matrix(clean_edges;)

				#	Resolve sentinel row index
					i_s = haskey(node_to_idx, sentinel_id) ? node_to_idx[sentinel_id] : nothing
					i_s === nothing && println("DEBUG newMods: sentinel '", sentinel_id, "' not found in node_to_idx")

				#	Global/Q₀ and community context
					Q0 = calculate_modularity(A, m, resolution)
					println("DEBUG newMods: mass (m) = ", mass, " | Q₀ = ", Q0)

					if i_s !== nothing
						c_s = m[i_s]
						in_cs = findall(j -> m[j] == c_s, 1:n)
						K_cs = sum(degrees[in_cs])
						sum_internal_deg_cs = sum(node_deg_by_group[j, c_s] for j in in_cs)
						sum_self_loops_cs   = sum(A[j, j] for j in in_cs)
						E_cs = (sum_internal_deg_cs + sum_self_loops_cs) / 2.0

						println("DEBUG newMods: sentinel=", sentinel_id,
						        " (row ", i_s, ", comm ", c_s, ")")
						println("DEBUG newMods:  k_i (total) = ", degrees[i_s],
						        " | k_i^comm = ", node_deg_by_group[i_s, c_s])
						println("DEBUG newMods:  internal_edges (Σ_c E_c) = ", internal_edges)
						println("DEBUG newMods:  K_c(s) = ", K_cs, " | E_c(s) = ", E_cs)
						println("DEBUG newMods:  group_degs[c_s] = ", group_degs[c_s])

						println("DEBUG newMods:  internal_deg[i_s] = ", internal_deg[i_s])
						println("DEBUG newMods:  q1_links[i_s] = ", q1_links[i_s])
						println("DEBUG newMods:  q1_degrees[i_s] = ", q1_degrees[i_s])
						println("DEBUG newMods:  q1s[i_s] (Q after removal) = ", q1s[i_s])
						println("DEBUG newMods:  vitality[i_s] = Q₀ - q1s[i_s] = ", Q0 - q1s[i_s])
					end

				#	Cross-checks
					E_sum = 0.0
					for c in 1:C
						in_c = findall(j -> m[j] == c, 1:n)
						E_c = (sum(node_deg_by_group[j, c] for j in in_c) + sum(A[j, j] for j in in_c)) / 2.0
						E_sum += E_c
					end
					println("DEBUG newMods: Σ_c E_c = ", E_sum, " (should equal internal_edges)")

					if abs(E_sum - internal_edges) > 1e-9
						println("WARN newMods: Σ_c E_c (", E_sum, ") ≠ internal_edges (", internal_edges, ")")
					end
			end

		#	Return vector of Q after removal for each node
			return q1s
	end

#	Come Back Here







#	Compare Modularity Vitality to Python Scores
	python_results = CSV.read("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data/balikatan_all_comm_vitality_results_python_gamma1.csv", 
							  DataFrame, types=Dict(1 => String))

	vitality_comparison = leftjoin(fixed_modularity.results_df[:,(1:2)], python_results[:,(1:2)], on=:node)
	
#	Construct Comparison Set for Modularity Tests: ORA Tests
	res_1_modularity = modularity_vitality(agent_agent_all_com.edges; resolution_sweep=false, resolution=1.0)
	keep_index = DataFrame(node = res_1_modularity.results_df.node, keep = ones(Int64, length(res_1_modularity.results_df.node)))

	ora_moduarlity_scores = CSV.read("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data/All_Comm_Modularity_Vitality_Scores.csv", 
					                 DataFrame, types=Dict(1 => String))
	rename!(ora_moduarlity_scores, ["node", "Modularity_Vitality_Bridge_All_Comm", "Modularity_Vitality_Hub_All_Comm", "community"])
	leftjoin!(keep_index, ora_moduarlity_scores, on=:node)
	keep_index[!,3] = convert.(Float64, keep_index[:,3])
	keep_index[!,4] = convert.(Float64, keep_index[:,4])
	keep_index[!,5] = convert.(Int64, keep_index[:,5])
	select!(keep_index, [1,3,4,5])
	
#	Comparing Modularity Vitality Hub and Bridge Scores to ORA
	leftjoin!(keep_index, res_1_modularity.results_df[:,(1:3)], on=:node)
	keep_index = keep_index[:,[1,4,3,5,2,6]]
	keep_index.modularity_vitality_hub = convert.(Float64, keep_index.modularity_vitality_hub)
	keep_index.modularity_vitality_bridge = convert.(Float64, keep_index.modularity_vitality_bridge)

	hub_scores = keep_index[:,[1,3,4]]
	DataFrames.sort(hub_scores, [:Modularity_Vitality_Hub_All_Comm], rev=[true])

	bridge_scores = keep_index[:,[1,5,6]]
	DataFrames.sort(bridge_scores, [:Modularity_Vitality_Bridge_All_Comm], rev=[true])

##########################
#   CORE DECOMPOSITION   #
##########################


###################
#   LOCAL REACH   #
###################



############################
#   GRAPH-LEVEL FEATURES   #
############################


#######################
#   GLOBAL MEASURES   #
#######################

#	CALCULATE GLOBAL MEASURES

#   Global Reciprocity (Fraction of Reciprocated Edges): 0.004
	ora_reciprocity = reciprocity(agent_agent_all_com.edges, weighted=true, mode=:dyad_based, weighted_method=:ora_mutual)
	squartini_reciprocity = reciprocity(agent_agent_all_com.edges, weighted=true, mode=:dyad_based, weighted_method=:squartini)
	arc_reciprocity = reciprocity(agent_agent_all_com.edges, weighted=true, mode=:arc_based)

#	CONDUCT TESTS

#	Reciprocity Tests
	test_reciprocity_methods()