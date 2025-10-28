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

#	Component Scaled Page Rank
	

#	Hub Centrality: SALSA
	hub_centrality = salsa_centrality(agent_agent_all_com.edges; score=:hub)
		
#	Authority Centrality: SALSA
	authority_centrality = salsa_centrality(agent_agent_all_com.edges; score=:authority)

#	CONDUCT TESTS

#	Page Rank ORA Comparisons

#	SALSA Tests
	test_salsa()

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