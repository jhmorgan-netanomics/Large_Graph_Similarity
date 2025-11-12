#Test Script of Large_Graph_Similarity's Design Matrices and Comparator Functions
#Jonathan H. Morgan
#10 November 2025

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
using SparseArrays
using Statistics
using StatsBase
using Large_Graph_Similarity

#################
#   FUNCTIONS   #
#################

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

#	Helper: graph (nodes + edges) to sparse adjacency with fixed node universe
	function _graph_to_sparse_matrix(edges::DataFrame;
									nodes::Union{Nothing,DataFrame,AbstractVector{<:AbstractString}}=nothing,
									weighted::Bool=true)
		"""
		Args:
			edges::DataFrame
				Required columns: :src, :dst
				Optional column:  :weight
				src/dst are node IDs (treated as String; supports long IDs)
		
			nodes::Union{Nothing,DataFrame,Vector{<:AbstractString}}
				Nothing  → infer nodes from edges (isolates excluded)
				DataFrame: columns :id and :label (both string vectors). Uses :id as the ID universe.
				Vector   : string vector of node IDs forming the ID universe (includes isolates, if any)
		
			weighted::Bool
				If true and edges has :weight, use it; otherwise use ones.
				If false, ignore any :weight column and use ones.
		
		Returns:
			Tuple{SparseMatrixCSC{Float64,Int64}, Dict{Any,Int}, Vector{Any}}
				(adj_matrix, node_to_idx, idx_to_node)
		
		Notes:
			- When `nodes` is provided, the returned matrix is sized to that universe
			(so isolates are included). All edge endpoints must exist in `nodes`.
			- When `nodes` is not provided, falls back to `_edgelist_to_sparse_matrix`
			which infers the node set from edge endpoints only.
		"""

		#	Basic validation for edge columns
			@assert hasproperty(edges, :src) && hasproperty(edges, :dst) "_graph_to_sparse_matrix: edges must have :src and :dst"

		#	Fallback: no nodes supplied → just delegate to the existing helper
			if nodes === nothing
				return _edgelist_to_sparse_matrix(edges; weighted=weighted)
			end

		#	Build the fixed node universe (idx_to_node) and mapping (node_to_idx)
			ids = String[]
			if nodes isa DataFrame
				#	Nodes as a DataFrame of IDs and Labels (Screen Names)
					ndf = nodes::DataFrame
					@assert hasproperty(ndf, :id) && hasproperty(ndf, :label) "_graph_to_sparse_matrix: nodes DataFrame must have :id and :label"
					ids = String.(ndf.id)
			else
				#	Vector of node IDs
					ids = String.(nodes::AbstractVector{<:AbstractString})
			end

		#	Specifyign Node Specific Return Objects
			n = length(ids)
			node_to_idx = Dict{Any,Int}(id => i for (i, id) in enumerate(ids))
			if(typeof(nodes) == DataFrame)
				idx_to_node = nodes
			else
				idx_to_node = Vector{Any}(ids)  # keep Any to match requested return type
			end

		#	Map edge endpoints to indices (validate all endpoints are known)
			src_ids = String.(edges.src)
			dst_ids = String.(edges.dst)

			unknown_src = Set{String}(s for s in src_ids if !haskey(node_to_idx, s))
			unknown_dst = Set{String}(d for d in dst_ids if !haskey(node_to_idx, d))
			if !isempty(unknown_src) || !isempty(unknown_dst)
				missing_ids = union(unknown_src, unknown_dst)
				examples = join(collect(Iterators.take(missing_ids, 5)), ", ")
				throw(ArgumentError("_graph_to_sparse_matrix: edges reference IDs not present in supplied nodes (examples: $examples)"))
			end

			src_idx = [node_to_idx[s] for s in src_ids]
			dst_idx = [node_to_idx[d] for d in dst_ids]

		#	Determine edge weights per spec
			use_weights = weighted && hasproperty(edges, :weight)
			weights = use_weights ? Float64.(edges.weight) : ones(Float64, nrow(edges))

		#	Construct sparse adjacency (no symmetrization here; caller decides)
			adj_matrix = sparse(src_idx, dst_idx, weights, n, n)

		#	Return adjacency and mappings
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

#   Helper: sparse adjacency matrix to edge list
    function _sparse_matrix_to_edgelist(adj_matrix::SparseMatrixCSC{T,Int};
                                        include_diagonal::Bool = true,
                                        node_map::Union{Nothing,Dict{Any,Int}} = nothing) where {T<:Real}
        """
        Args:
            adj_matrix::SparseMatrixCSC{T,Int}:
                Sparse adjacency matrix. Nonzero entries are interpreted as edges
                from row index (src) to column index (dst). Weights may be Integer
                or Float; they are converted to Float64.

            include_diagonal::Bool:
                If true  → keep self-loops (i == j).
                If false → drop self-loops.

            node_map::Union{Nothing,Dict{Any,Int}}:
                Optional mapping from original node IDs → matrix indices,
                as returned by `_graph_to_sparse_matrix` (the `node_to_idx`
                dictionary). If provided, this function inverts the mapping
                so that the returned `:src` and `:dst` columns use the
                original node IDs instead of integer indices.

        Returns:
            DataFrame:
                Columns: :src, :dst, :weight

                If `node_map === nothing`:
                    :src, :dst   → Int indices (1-based, matching matrix indices)
                If `node_map !== nothing`:
                    :src, :dst   → original node IDs (keys of `node_map`)

                :weight        → Float64 edge weights

        Notes:
            - This is the inverse of `_edgelist_to_sparse_matrix` in spirit:
            if `node_map` is provided, it restores the original node IDs.
            - To aggregate multi-edges after conversion, use `_aggregate_multi_edges`.
        """

        #   Extract nonzero indices and values
            I, J, V = findnz(adj_matrix)   # row indices, col indices, values

        #   Optionally drop diagonal entries (self-loops)
            if !include_diagonal
                mask = I .!= J
                I = I[mask]
                J = J[mask]
                V = V[mask]
            end

        #   If no node_map is provided, keep matrix indices as src/dst
            if node_map === nothing
                return DataFrame(
                    src    = I,
                    dst    = J,
                    weight = Float64.(V),
                )
            end

        #   Invert node_map: index → original node ID
            idx_to_node = Dict{Int,Any}()
            for (node_id, idx) in node_map
                idx_to_node[idx] = node_id
            end

            src_ids = [idx_to_node[i] for i in I]
            dst_ids = [idx_to_node[j] for j in J]

        #   Build DataFrame with original node IDs
            return DataFrame(
                src    = src_ids,
                dst    = dst_ids,
                weight = Float64.(V),
            )
    end

#   Helper: symmetric sparse adjacency to undirected, collapsed edge list
    function _symmetric_sparse_to_undirected_edgelist(adj::SparseMatrixCSC{T,Int};
                                                    include_diagonal::Bool = true,
                                                    agg_func::Function = maximum,
                                                    node_map::Union{Nothing,Dict{Any,Int}} = nothing) where {T<:Real}
        """
        Assumes:
            - `adj` is symmetric (adj[i, j] == adj[j, i]).

        Behavior:
            - Treats each unordered pair {i, j} as a single edge.
            - Uses `src = min(i, j)`, `dst = max(i, j)` as a canonical orientation
            in index space.
            - Collapses duplicates by grouping on (src, dst) and aggregating weights
            with `agg_func` (default = maximum, which matches a binarized ORA view).
            - If `node_map` is supplied, maps indices back to original node IDs
            in the returned `:src` and `:dst` columns, and then enforces a
            canonical ordering of labels per edge (string(src) ≤ string(dst)).

        Arguments:
            adj::SparseMatrixCSC{T,Int}
                Symmetric adjacency matrix.

            include_diagonal::Bool
                If true  → keep self-loops (i == j).
                If false → drop self-loops.

            agg_func::Function
                Aggregation function for combining multiple weights for the same
                unordered pair (default = maximum; use `sum` for counts, etc.).

            node_map::Union{Nothing,Dict{Any,Int}}
                Optional mapping from original node IDs → matrix indices, as
                returned by `_graph_to_sparse_matrix` (the `node_to_idx`
                dictionary). If provided, the output `:src` and `:dst` will be
                original node IDs; otherwise they will be Int indices.

        Returns:
            DataFrame with columns:
                :src
                :dst
                :weight :: Float64

            - If `node_map === nothing`, :src and :dst are Int indices.
            - If `node_map !== nothing`, :src and :dst are original node IDs,
            with string(src) ≤ string(dst) for all rows.
        """
        #   Extract all nonzeros
            I, J, V = findnz(adj)

        #   Canonical orientation for unordered pairs (index space)
            src = min.(I, J)
            dst = max.(I, J)

        #   Optionally drop self-loops
            mask = include_diagonal ? trues(length(src)) : (src .!= dst)

            df = DataFrame(
                src    = src[mask],
                dst    = dst[mask],
                weight = Float64.(V[mask]),
            )

        #   Collapse duplicates so each {src, dst} appears once
            df_agg = combine(groupby(df, [:src, :dst]), :weight => agg_func => :weight)

        #   If no node_map is provided, leave indices as-is
            if node_map === nothing
                return df_agg
            end

        #   Invert node_map: index → original node ID
            idx_to_node = Dict{Int,Any}()
            for (node_id, idx) in node_map
                idx_to_node[idx] = node_id
            end

        #   Map indices back to original node IDs
            src_ids = [idx_to_node[i] for i in df_agg.src]
            dst_ids = [idx_to_node[j] for j in df_agg.dst]

            df_agg.src = src_ids
            df_agg.dst = dst_ids

        #   Enforce a canonical ordering on labels per edge:
        #   for undirected graphs, ensure string(src) ≤ string(dst)
            for row in eachrow(df_agg)
                if string(row.src) > string(row.dst)
                    tmp = row.src
                    row.src = row.dst
                    row.dst = tmp
                end
            end

        #   Return Edgelist with original node labels and stable src/dst order
            return df_agg
    end

#   Helper: summarize link stats into (link_group, values) + density map
    function _summarize_link_stats(stats_df::DataFrame)
        """
        Args:
            stats_df::DataFrame
                Expected columns:
                    :group   :: AbstractString
                    :count   :: Real
                    :min     :: Real
                    :max     :: Real
                    :mean    :: Real
                    :std     :: Real
                    :sum     :: Real
                    :density :: Real

        Returns:
            Tuple{DataFrame, Dict{String,Float64}}:
                (summary_df, density_map)

                summary_df:
                    Columns:
                        :link_group :: String
                        :values     :: String
                    Example row:
                        link_group = "all_links"
                        values     = "(count = 3114, min = 1.0, max = 1.0, mean = 1.0, std = 0.0, sum = 3114.0)"

                density_map:
                    Dict mapping link_group → density:
                        Dict("all_links" => 0.00343507,
                            "nonself_links" => 0.00343507,
                            "self_loops" => 0.00343507)
        Notes:
            - Assumes one row per group.
            - Values are interpolated as-is; no rounding is applied.
        """
        #   Prepare output containers
            link_group = String[]
            values_col = String[]
            density_map = Dict{String,Float64}()

        #   Build the summary strings and density map
            for row in eachrow(stats_df)
                #   Isolate the Group
                    g = String(row.group)

                #   Build the values string
                    values_str = "(count = $(row.count), " *
                                "min = $(row.min), "   *
                                "max = $(row.max), "   *
                                "mean = $(row.mean), " *
                                "std = $(row.std), "   *
                                "sum = $(row.sum))"

                #   Populate the Values
                    push!(link_group, g)
                    push!(values_col, values_str)

                #   Add Density
                    density_map[g] = Float64(row.density)
            end

        #   Construct the summary DataFrame
            summary_df = DataFrame(
                link_group = link_group,
                values     = values_col,
            )

        #   Return adjusted table and density values
            return (summary_df, density_map)
    end

############################
#   IMPORT TEST NETWORKS   #
############################

#   Loading Balikatan_2022_Processed
    import_directory = "/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data"
    ora_xml = "Balikatan_2022_Processed.xml"
    file_location = string(import_directory, "/", ora_xml)
    balikatan_2022 = load_ora_xml(file_location)

    agents = balikatan_2022.nodesets["Agent"]
    nodes = agents[:,(1:2)]
	rename!(nodes, ["id", "label"])

    agent_agent_all_com = balikatan_2022.networks["Agent x Agent - All Communication"]
    balikatan_arcs = agent_agent_all_com.edges

#   Loading TOTO 2024 Synthetic Network
  


#######################################################
#   ASSESSMENT OF THE DESIGN MATRICES' CONSTRUCTORS   #
#######################################################

#   Unwighted/Undirected Networks
    provided_membership = nothing
    edges = deepcopy(balikatan_arcs)
    weighted = false
    resolution_sweep = true
    n_resolutions = 15
    n_runs_per_gamma = 5
    n_iterations_per_run = 10
    resolution = 1.0
    directed = false
    seed = 49
    function undirected_binary_constructor(edges::DataFrame, nodes::Union{Nothing,DataFrame,AbstractVector{<:AbstractString}};
	                                       resolution_sweep::Bool=false, resolution::Float64=1.0, directed::Bool=false, weighted::Bool=false,
	                                       n_resolutions::Int=15, n_runs_per_gamma::Int=5, n_iterations_per_run::Int=10,
	                                       seed::Union{Int,Nothing}=nothing, 
                                           provided_membership::Union{Nothing,DataFrame,Vector{Int},Dict}=nothing)
        

        #   TRANSFORM THE NETWORK

        #	Prepare Edges with Appropriate Weights
			clean_edges = deepcopy(edges) 

        #   Check for Weights
            if weighted
				#	Weighted: Ensure Weight Column Exists
					if !hasproperty(clean_edges, :weight)
						clean_edges.weight = ones(Float64, nrow(clean_edges))
					else
						clean_edges.weight = Float64.(clean_edges.weight)
					end
					agg_func = sum
			else
				#	Unweighted: Force Binary Weights
					clean_edges.weight = ones(Float64, nrow(clean_edges))
					agg_func = maximum
			end

        #   Ensure Weights are Binary
            clean_edges = _aggregate_multi_edges(clean_edges; agg_func=maximum)

        #   Construct Adjacency Matrix
			adj_base, node_map, idx_to_node = _graph_to_sparse_matrix(clean_edges; nodes = nodes, weighted = false)

        #	Preserve a Copy of the Node Index for Generating Community Solutions	
			ni = deepcopy(idx_to_node)

        #   Ensure the Network is Symmetric
            adj = max.(adj_base, adj_base')

        #   GLOBAL MEASURES

        #   Convert Adjacency Matrix into Edges to Reading into Network Functions
            symmetric_edgelist = _symmetric_sparse_to_undirected_edgelist(adj; include_diagonal=true, node_map=node_map)

        #   Calculating Global Measures
            component_stats = component_statistics(symmetric_edgelist, nodes=ni, graph_type=:undirected)
            component_stat_names  = collect(keys(component_stats))      
            component_stat_values = collect(values(component_stats)) 

            link_stats = link_statistics(symmetric_edgelist; nodes = ni, graph_type = :undirected, weighted = false)
            link_stats_tuple = _summarize_link_stats(link_stats)
            link_stats_df = link_stats_tuple[1]

            degree_assortativity = assortativity_degree(symmetric_edgelist; graph_type = :undirected, weighted = false)
            transitivity = global_clustering_coefficient(symmetric_edgelist; directed=false, weighted=false, method=:transitivity, drop_self_loops=true)
            global_local_clustering_coeff = global_clustering_coefficient(symmetric_edgelist; directed=false, weighted=false, method=:average, drop_self_loops=true)

        #   Constructing Global Measure Index
            global_measures = [component_stat_names; link_stats_df.link_group; "degree assortativity"; "transitivity"; 
                               "local clustering coefficient"; "density"]
            global_values = string.([component_stat_values; link_stats_df.values; round(degree_assortativity, digits=6); round(transitivity, digits=6);  
                                    round(global_local_clustering_coeff,digits=6) ; round(link_stats.density[1], digits=6)])
            global_stats_df = DataFrame(measure=global_measures, value = global_values)

        #   MESO-LEVEL MEASURES

        #   Performing Community Detection (No Isolates/Edges Only)
            resolution_used = resolution
		    if provided_membership === nothing
				#	Detect Communities
					if resolution_sweep
						#	CHAMP Sweep
							community_solution = champ_community_detection(
								symmetric_edgelist;
								resolution = nothing,
								resolution_range = (0.5, 1.8),
								n_resolutions = n_resolutions,
								weighted = weighted,
								directed = directed,
								n_runs_per_gamma = n_runs_per_gamma,
								n_iterations_per_run = n_iterations_per_run,
								seed = seed,
								show_progress = true
							)
							resolution_used = community_solution.resolution_used
                            modularity = community_solution.modularity
							
						#	Create Partition DataFrame
							if community_solution.node_names isa DataFrame
								partition_df = DataFrame(
									node = String.(community_solution.node_names.id), 
									community = community_solution.membership
								)
							else
								partition_df = DataFrame(
									node = String.(community_solution.node_names), 
									community = community_solution.membership
								)
							end
					else
						#	Single Resolution Leiden
							community_solution = leiden_community_detection(
								symmetric_edgelist;
								resolution = resolution,
								n_iterations = n_iterations_per_run,
								n_runs = n_runs_per_gamma,
								weighted = weighted,
								directed = directed,
								seed = seed
							)
							resolution_used = resolution
                            modularity = community_solution.modularity
							
						#	Create Partition DataFrame
							if community_solution.node_names isa DataFrame
								partition_df = DataFrame(
									node = String.(community_solution.node_names.id), 
									community = community_solution.membership
								)
							else
								partition_df = DataFrame(
									node = String.(community_solution.node_names), 
									community = community_solution.membership
								)
							end
					end	
			else
				#	Process User-Provided Partition
					if provided_membership isa DataFrame
						#	Use Provided DataFrame
							pm = deepcopy(provided_membership)
							rename!(pm, lowercase.(string.(propertynames(pm))))
							@assert hasproperty(pm, :node) && hasproperty(pm, :community) "DataFrame needs :node and :community"
							partition_df = DataFrame(
								node = String.(pm.node),
								community = Int.(pm.community)
							)
							
					elseif provided_membership isa Vector
						#	Vector Aligned to Matrix Order
							@assert length(provided_membership) == n "Vector length must match node count"
							partition_df = DataFrame(
								node = node_ids,
								community = Int.(provided_membership)
							)
							
					elseif provided_membership isa Dict
						#	Map by Node ID
							communities = zeros(Int64, n)
							for i in 1:n
								communities[i] = get(provided_membership, node_ids[i], 0)
							end
							partition_df = DataFrame(
								node = node_ids,
								community = communities
							)
					end
			end

        #   Adding Modularity & Resolution if Known to Global Statistics
            if !isnothing(provided_membership)
                #   Produced Reduced Adjacency for the Purpose of Calculating Modularity
                    adj_symmetric, node_map_symmetric, idx_to_node_symmetric = _graph_to_sparse_matrix(symmetric_edgelist; weighted = false)

                #   Make Sure Partition Maps to Edge Only Matrix
                    keep_index = DataFrame(node = idx_to_node_symmetric, keep = ones(Int64, length(idx_to_node_symmetric)))
                    keep_index = leftjoin!(keep_index, partition_df, on=:node)
                    keep_index.community = convert.(Int64, keep_index.community)

                #   Calculate Modularity
                    modularity = calculate_modularity(adj_symmetric, keep_index.community, γ=resolution)
                    resolution_used = resolution
            end
        
        #   Adding Modularity & Resoltuion Paramer as the Final Global Stats
            partition_stats_df = DataFrame(measure = ["resolution", "modularity"]; value= string.(round.([modularity, resolution_used], digits=6)))
            global_measures = [global_stats_df; partition_stats_df]

        #   Calculating Group Statistics
            group_statistics_dict = group_statistics(symmetric_edgelist; membership=partition_df, directed = false, weighted = false,)

        #   Calculating Group Relationships
            node_stats = group_statistics_dict.node_stats
            node_stats.in_group_ratio = node_stats.total_degree_in_group ./ node_stats.total_degree

        #   Calculating K-Core Membership
            k_core_all = core_decomposition(symmetric_edgelist; weighted=false, mode="total")
	        rename!(k_core_all, ["node", "k_core_all"])
            leftjoin!(node_stats, k_core_all, on=:node)
            node_stats.k_core_all = convert.(Int64, node_stats.k_core_all)

        #   Calcularing 2-K Reachability
            all_hop_reach = hop_reach_k(symmetric_edgelist, mode="all", k=2) 
	        rename!(all_hop_reach, ["node", "undirected_reach_2"])
            leftjoin!(node_stats, all_hop_reach, on=:node)
            node_stats.undirected_reach_2 = convert.(Int64, node_stats.undirected_reach_2)
          
        #   Calucate the Triad Census
            triads_ud = triad_census(symmetric_edgelist; weighted=false, graph_type=:undirected)
            triad_count_sum = sum(triads_ud.count)
            triads_ud.proportion = round.(triads_ud.count ./ triad_count_sum, digits=6)

        #   NODE-LEVEL MESURES

        #   Calculating Normalized Total Degree (Dropping Self-Loops to Be Consistent with a N-1 Normalization)
            total_deg_norm = total_degree(symmetric_edgelist; directed=false, weighted=true, normalize=true, drop_self_loops=true,
								          count_self_loops_once=true, agg_func = maximum, n=nrow(ni))
            rename!(total_deg_norm, ["node", "total_degree_normalized"])
            leftjoin!(node_stats, total_deg_norm, on=:node)
           
        #   Transforming Missing Values Introduced by Nodes with Only Self-Loops into zeros
            node_stats.total_degree_normalized = coalesce.(node_stats.total_degree_normalized, 0)
            node_stats.total_degree_normalized = convert.(Float64, node_stats.total_degree_normalized)

        #   Calculating Node-Level Local Clustering
        #   This meaures does not assume symmetry. ORA looks at all ties in and out of the ego node, even in the Undirected Case.
            strogatz_local_clustering = local_clustering_coefficient(symmetric_edgelist; directed=false, weighted=false,
                                                                     method = :density, density_mode=:ego_nodes)   



    end

######################################
#   COMPARATOR FUNCTION ASSESSMENT   #
######################################


#############
#   TESTS   #
#############

#   Debug the actual computation
    function debug_local_clustering(edges; top_n=10)
        #   Prepare edges exactly as the function does
            clean_edges = _aggregate_multi_edges(edges; agg_func = maximum)
            edges_canonical = DataFrame(
                src = min.(clean_edges.src, clean_edges.dst),
                dst = max.(clean_edges.src, clean_edges.dst)
            )
            edges_simple = unique(edges_canonical)
            edges_bidirectional = vcat(
                edges_simple,
                DataFrame(src = edges_simple.dst, dst = edges_simple.src)
            )
            
            A, _, idx_to_node = _edgelist_to_sparse_matrix(edges_bidirectional; weighted = false)
            A = max.(A, A')
            A = spzeros(Float64, size(A)...) .+ (A .> 0)
            
            n = size(A, 1)
            results = []
            
            for i in 1:n
                neighbors = findall(!iszero, A[i, :])
                k_i = length(neighbors)
                
                if k_i >= 2
                    A_sub = A[neighbors, neighbors]
                    E_i = sum(A_sub) / 2.0
                    max_E_i = k_i * (k_i - 1) / 2.0
                    C_i = E_i / max_E_i
                    
                    node_name = idx_to_node isa DataFrame ? idx_to_node.id[i] : idx_to_node[i]
                    push!(results, (node=node_name, degree=k_i, clustering=C_i))
                end
            end
        
        #   Sort by degree and show top nodes
            sort!(results, by=x->x.degree, rev=true)
            
            println("Top $top_n nodes by degree:")
            for (i, r) in enumerate(results[1:min(top_n, length(results))])
                println("  $(r.node): degree=$(r.degree), clustering=$(round(r.clustering, digits=3))")
            end
            
            println("\nDegree distribution of nodes with k≥2:")
            deg_counts = countmap([r.degree for r in results])
            for (d, count) in sort(collect(deg_counts))
                println("  Degree $d: $count nodes")
            end
            
            println("\nAverage clustering: ", mean([r.clustering for r in results]))
            return results
    end

    results = debug_local_clustering(symmetric_edgelist)

#   Debug the actual computation
    function debug_local_clustering(edges; top_n=10)
        #   Prepare edges exactly as the function does
            clean_edges = _aggregate_multi_edges(edges; agg_func = maximum)
            edges_canonical = DataFrame(
                src = min.(clean_edges.src, clean_edges.dst),
                dst = max.(clean_edges.src, clean_edges.dst)
            )
            edges_simple = unique(edges_canonical)
            edges_bidirectional = vcat(
                edges_simple,
                DataFrame(src = edges_simple.dst, dst = edges_simple.src)
            )
            
            A, _, idx_to_node = _edgelist_to_sparse_matrix(edges_bidirectional; weighted = false)
            A = max.(A, A')
            A = spzeros(Float64, size(A)...) .+ (A .> 0)
            
            n = size(A, 1)
            results = []
            
            for i in 1:n
                neighbors = findall(!iszero, A[i, :])
                k_i = length(neighbors)
                
                if k_i >= 2
                    A_sub = A[neighbors, neighbors]
                    E_i = sum(A_sub) / 2.0
                    max_E_i = k_i * (k_i - 1) / 2.0
                    C_i = E_i / max_E_i
                    
                    node_name = idx_to_node isa DataFrame ? idx_to_node.id[i] : idx_to_node[i]
                    push!(results, (node=node_name, degree=k_i, clustering=C_i))
                end
            end
        
        #   Sort by degree and show top nodes
            sort!(results, by=x->x.degree, rev=true)
            
            println("Top $top_n nodes by degree:")
            for (i, r) in enumerate(results[1:min(top_n, length(results))])
                println("  $(r.node): degree=$(r.degree), clustering=$(round(r.clustering, digits=3))")
            end
            
            println("\nDegree distribution of nodes with k≥2:")
            deg_counts = countmap([r.degree for r in results])
            for (d, count) in sort(collect(deg_counts))
                println("  Degree $d: $count nodes")
            end
            
            println("\nAverage clustering: ", mean([r.clustering for r in results]))
            return results
    end

    results = debug_local_clustering(symmetric_edgelist)

    edges = deepcopy(symmetric_edgelist)
    function debug_ego_network(edges::DataFrame, ego;
                            directed::Bool=true)
        """
        Args:
            edges::DataFrame: edge list with :src and :dst columns
            ego: node id for which to extract the ego network
            directed::Bool: if true, keep edge directions; if false, treat as undirected
        Returns:
            NamedTuple with:
                - ego::Any
                - nodes::Vector: [ego; neighbors...] (ego is first)
                - ego_edges::DataFrame: all edges with endpoints in ego ∪ neighbors
                - neighbor_edges::DataFrame: edges only among neighbors (no ego)
        Notes:
            - Neighbors are defined as the union of in- and out-neighbors of ego.
            - For directed=false, the edge list is treated as undirected when
            determining neighbors, but ego_edges keeps the original rows.
        """

        #   Validation
            if !hasproperty(edges, :src) || !hasproperty(edges, :dst)
                throw(ArgumentError("edges DataFrame must have :src and :dst columns"))
            end

        #   Determine neighbors (weak neighborhood: in ∪ out)
            ego_edges = filter(row -> row.src == ego  || row.dst == ego, edges)

        #   Isolating Unique Number of Nodes
            ego_nodes = sort(unique((ego_edges.src; ego_edges.dst)))
            ego_neighbors = ego_nodes[(ego_nodes .!= ego)]

        #   Isolating Neighbor Edges
            neighbor_edges = filter(row -> (row.src in ego_neighbors) && (row.dst in ego_neighbors), edges)
            ego_network = [ego_edges; neighbor_edges]

        #   Calculate Undirected Ego Network Clustering Coefficient
            k = length(ego_neighbors)
            e = nrow(ego_network)

            ego_numerator = 2*e
            ego_denominator = k*(k-1)    
            ego_numerator/ego_denominator


        #   Calculating 


      
    end

    ego_info = debug_ego_network(symmetric_edgelist, 25930421; directed=true)


