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
using LinearAlgebra
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

#	Helper for Comparator Function: Skewness about the mean (central moment)
    function skew_about_mean(x; corrected::Bool = true)
        """
        Args:
            x: AbstractVector (numbers; `missing` allowed → skipped)
            corrected::Bool: If true, return Fisher's bias-corrected skewness (default).
                             If false, return population (moment) skewness m3 / m2^(3/2).
        Returns:
            Float64: skewness about the mean (NaN if undefined).
        Notes:
            Population (moment) skewness:
                m2 = mean((x - μ)^2), m3 = mean((x - μ)^3), skew = m3 / m2^(3/2).
            Fisher correction (n > 2):
                G1 = sqrt(n*(n-1)) / (n-2) * skew.
        """

        #	Validation & preprocessing (drop missings)
            xv = collect(skipmissing(x))
            n = length(xv)
            if n == 0
                return NaN
            end
            μ = mean(xv)

        #	Central moments m2, m3 (with /n convention)
            δ = @. (xv - μ)
            m2 = mean(@. δ^2)
            if m2 == 0.0
                return 0.0   # all identical values ⇒ zero skew
            end
            m3 = mean(@. δ^3)
            skew = m3 / (m2^(3/2))

        #	Optional Fisher correction
            if corrected
                if n < 3
                    return NaN
                end
                return sqrt(n*(n-1)) / (n-2) * skew
            else
                return skew
            end
    end

#	Helper for the Comparator Function: Excess kurtosis about the mean (central moment)
    function kurtosis_about_mean(x; corrected::Bool = true)
        """
        Args:
            x: AbstractVector (numbers; `missing` allowed → skipped)
            corrected::Bool: If true, return Fisher's bias-corrected **excess** kurtosis (default).
                             If false, return population **excess** kurtosis: m4/m2^2 - 3.
        Returns:
            Float64: excess kurtosis about the mean (NaN if undefined).
        Notes:
            Population (moment) excess kurtosis:
                m2 = mean((x - μ)^2), m4 = mean((x - μ)^4), g2 = m4/m2^2 - 3.
            Fisher correction (n > 3):
                G2 = ((n-1)/((n-2)*(n-3))) * ((n+1)*g2 + 6).
        """

        #	Validation & preprocessing (drop missings)
            xv = collect(skipmissing(x))
            n = length(xv)
            if n == 0
                return NaN
            end
            μ = mean(xv)

        #	Central moments m2, m4 (with /n convention)
            δ = @. (xv - μ)
            m2 = mean(@. δ^2)
            if m2 == 0.0
                return -3.0  # all identical values ⇒ variance 0 ⇒ excess kurtosis of a point mass
            end
            m4 = mean(@. δ^4)
            g2 = m4/(m2^2) - 3.0   # population **excess** kurtosis

        #	Optional Fisher correction
            if corrected
                if n < 4
                    return NaN
                end
                return ((n-1)/((n-2)*(n-3))) * ((n+1)*g2 + 6)
            else
                return g2
            end
    end

#	Helper: Undirected Binary Network Constructor for Comparisons
    function undirected_binary_constructor(edges::DataFrame, 
                                        nodes::Union{Nothing,DataFrame,AbstractVector{<:AbstractString}};
                                        resolution_sweep::Bool = false, 
                                        resolution::Float64 = 1.0, 
                                        directed::Bool = false, 
                                        weighted::Bool = false,
                                        n_resolutions::Int = 15, 
                                        n_runs_per_gamma::Int = 5, 
                                        n_iterations_per_run::Int = 10,
                                        seed::Union{Int,Nothing} = nothing, 
                                        provided_membership::Union{Nothing,DataFrame,Vector{Int},Dict} = nothing)
        """
        Helper function for network_comparator() that constructs undirected binary network and computes comprehensive statistics.
        
        Args:
            edges::DataFrame: Edge list with :src, :dst columns
            nodes: Node universe (includes isolates if present)
            resolution_sweep::Bool: Use CHAMP multi-resolution community detection
            resolution::Float64: Resolution parameter for community detection
            directed::Bool: Original network directionality (always converted to undirected)
            weighted::Bool: Original weight status (always converted to binary)
            n_resolutions::Int: Number of resolutions for CHAMP sweep
            n_runs_per_gamma::Int: Leiden runs per resolution
            n_iterations_per_run::Int: Iterations per Leiden run
            seed: Random seed for reproducibility
            provided_membership: Optional pre-computed community assignments
        Returns:
            Tuple of three DataFrames:
                1. global_stats_df: Network-level statistics
                2. triads_ud: Triad census distribution
                3. node_stats: Node-level metrics including community membership
        Notes:
            - Always produces undirected binary network regardless of input parameters
            - Symmetrizes via max(A, A') to preserve all connections
            - Computes statistics at global, meso (community), and node levels
            - Used internally by network_comparator() for standardized comparisons
        """

        #	========== NETWORK TRANSFORMATION ==========

        #	Create working copy and prepare weights
            clean_edges = deepcopy(edges) 

        #	Standardize weight column (will be binarized regardless of weighted parameter)
            if weighted
                #	Ensure weight column exists with proper type
                    if !hasproperty(clean_edges, :weight)
                        clean_edges.weight = ones(Float64, nrow(clean_edges))
                    else
                        clean_edges.weight = Float64.(clean_edges.weight)
                    end
                    agg_func = sum
            else
                #	Force binary weights
                    clean_edges.weight = ones(Float64, nrow(clean_edges))
                    agg_func = maximum
            end

        #	Binarize network (aggregate multi-edges to presence/absence)
            clean_edges = _aggregate_multi_edges(clean_edges; agg_func = maximum)

        #	Build adjacency matrix
            adj_base, node_map, idx_to_node = _graph_to_sparse_matrix(
                clean_edges; 
                nodes = nodes, 
                weighted = false
            )

        #	Preserve node index for community detection
            ni = deepcopy(idx_to_node)

        #	Symmetrize adjacency (max preserves all connections)
            adj = max.(adj_base, adj_base')

        #	========== GLOBAL NETWORK MEASURES ==========

        #	Convert symmetrized adjacency back to edge list for function compatibility
            symmetric_edgelist = _symmetric_sparse_to_undirected_edgelist(
                adj; 
                include_diagonal = true, 
                node_map = node_map
            )

        #	Component statistics
            component_stats = component_statistics(
                symmetric_edgelist, 
                nodes = ni, 
                graph_type = :undirected
            )
            component_stat_names = collect(keys(component_stats))      
            component_stat_values = collect(values(component_stats)) 

        #	Link statistics
            link_stats = link_statistics(
                symmetric_edgelist; 
                nodes = ni, 
                graph_type = :undirected, 
                weighted = false
            )
            link_stats_tuple = _summarize_link_stats(link_stats)
            link_stats_df = link_stats_tuple[1]

        #	Global clustering and assortativity
            degree_assortativity = assortativity_degree(
                symmetric_edgelist; 
                graph_type = :undirected, 
                weighted = false
            )
            
            transitivity = global_clustering_coefficient(
                symmetric_edgelist; 
                directed = false, 
                weighted = false, 
                method = :transitivity, 
                drop_self_loops = true
            )
            
            global_local_clustering_coeff = global_clustering_coefficient(
                symmetric_edgelist; 
                directed = false, 
                weighted = false, 
                method = :average, 
                drop_self_loops = true
            )

        #	Assemble global statistics DataFrame
            global_measures = [
                component_stat_names; 
                link_stats_df.link_group; 
                "degree assortativity"; 
                "transitivity"; 
                "local clustering coefficient"; 
                "density"
            ]
            
            global_values = string.([
                component_stat_values; 
                link_stats_df.values; 
                round(degree_assortativity, digits=6); 
                round(transitivity, digits=6);  
                round(global_local_clustering_coeff, digits=6); 
                round(link_stats.density[1], digits=6)
            ])
            
            global_stats_df = DataFrame(
                measure = global_measures, 
                value = global_values
            )

        #	========== MESO-LEVEL (COMMUNITY) MEASURES ==========

        #	Community detection or use provided membership
            resolution_used = resolution
            
            if provided_membership === nothing
                #	Perform community detection
                    if resolution_sweep
                        #	Multi-resolution CHAMP sweep
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
                            
                        #	Extract partition
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
                        #	Single resolution Leiden
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
                            
                        #	Extract partition
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
                #	Process user-provided partition
                    if provided_membership isa DataFrame
                        #	DataFrame with node and community columns
                            pm = deepcopy(provided_membership)
                            rename!(pm, lowercase.(string.(propertynames(pm))))
                            @assert hasproperty(pm, :node) && hasproperty(pm, :community) "DataFrame needs :node and :community"
                            partition_df = DataFrame(
                                node = String.(pm.node),
                                community = Int.(pm.community)
                            )
                            
                    elseif provided_membership isa Vector
                        #	Vector aligned to matrix order
                            @assert length(provided_membership) == length(ni) "Vector length must match node count"
                            partition_df = DataFrame(
                                node = String.(ni isa DataFrame ? ni.id : ni),
                                community = Int.(provided_membership)
                            )
                            
                    elseif provided_membership isa Dict
                        #	Dictionary mapping node IDs to communities
                            node_ids = ni isa DataFrame ? String.(ni.id) : String.(ni)
                            communities = zeros(Int64, length(node_ids))
                            for i in eachindex(node_ids)
                                communities[i] = get(provided_membership, node_ids[i], 0)
                            end
                            partition_df = DataFrame(
                                node = node_ids,
                                community = communities
                            )
                    end
            end

        #	Calculate modularity if using provided membership
            if !isnothing(provided_membership)
                #	Build adjacency for connected nodes only
                    adj_symmetric, node_map_symmetric, idx_to_node_symmetric = _graph_to_sparse_matrix(
                        symmetric_edgelist; 
                        weighted = false
                    )

                #	Align partition to connected nodes
                    keep_index = DataFrame(
                        node = idx_to_node_symmetric, 
                        keep = ones(Int64, length(idx_to_node_symmetric))
                    )
                    keep_index = leftjoin!(keep_index, partition_df, on = :node)
                    keep_index.community = convert.(Int64, keep_index.community)

                #	Calculate modularity
                    modularity = calculate_modularity(
                        adj_symmetric, 
                        keep_index.community, 
                        γ = resolution
                    )
                    resolution_used = resolution
            end
            
        #	Add modularity and resolution to global statistics
            partition_stats_df = DataFrame(
                measure = ["resolution", "modularity"], 
                value = string.(round.([resolution_used, modularity], digits=6))
            )
            global_measures = [global_stats_df; partition_stats_df]

        #	Calculate group-level statistics
            group_statistics_dict = group_statistics(
                symmetric_edgelist; 
                membership = partition_df, 
                directed = false, 
                weighted = false
            )

        #	Extract and enhance node statistics
            node_stats = group_statistics_dict.node_stats
            node_stats.in_group_ratio = node_stats.total_degree_in_group ./ node_stats.total_degree

        #	========== NODE-LEVEL MEASURES ==========

        #	K-core decomposition
            k_core_all = core_decomposition(
                symmetric_edgelist; 
                weighted = false, 
                mode = "total"
            )
            rename!(k_core_all, ["node", "k_core_all"])
            leftjoin!(node_stats, k_core_all, on = :node)
            node_stats.k_core_all = convert.(Int64, node_stats.k_core_all)

        #	2-hop reachability
            all_hop_reach = hop_reach_k(
                symmetric_edgelist, 
                mode = "all", 
                k = 2
            ) 
            rename!(all_hop_reach, ["node", "undirected_reach_2"])
            leftjoin!(node_stats, all_hop_reach, on = :node)
            node_stats.undirected_reach_2 = convert.(Int64, node_stats.undirected_reach_2)
            
        #	Triad census
            triads_ud = triad_census(
                symmetric_edgelist; 
                weighted = false, 
                graph_type = :undirected
            )
            triad_count_sum = sum(triads_ud.count)
            triads_ud.proportion = round.(triads_ud.count ./ triad_count_sum, digits=6)

        #	Normalized degree centrality (Freeman normalization)
            total_deg_norm = total_degree(symmetric_edgelist; directed = false, weighted = true, normalize = true, 
                                          drop_self_loops = true, count_self_loops_once = true, n = nrow(ni))
            rename!(total_deg_norm, ["node", "total_degree_normalized"])
            leftjoin!(node_stats, total_deg_norm, on = :node)
            
        #	Handle isolates (nodes with only self-loops get 0 degree)
            node_stats.total_degree_normalized = coalesce.(node_stats.total_degree_normalized, 0.0)
            node_stats.total_degree_normalized = convert.(Float64, node_stats.total_degree_normalized)

        #	Local clustering coefficient (ORA-style density version)
            local_density_clustering = local_clustering_coefficient(symmetric_edgelist; directed = false,method = :local_density)   
            rename!(local_density_clustering, ["node", "ego_density", "density_clustering_coefficient"])   
            leftjoin!(node_stats, local_density_clustering, on = :node)     
            node_stats.density_clustering_coefficient = convert.(Float64, node_stats.density_clustering_coefficient) 

        #	Modularity vitality (hub and bridge scores)
            modularity_scores = modularity_vitality(
                symmetric_edgelist; 
                directed = false, 
                resolution = resolution_used, 
                weighted = false, 
                resolution_sweep = false, 
                provided_membership = partition_df
            )
            leftjoin!(node_stats, modularity_scores.results_df[:,[1,3,4]], on = :node)
            
        #	Convert vitality scores to proper type
            var_names = names(modularity_scores.results_df[:,[3,4]])
            for i in eachindex(var_names)
                node_stats[!, var_names[i]] = convert.(Float64, node_stats[:, var_names[i]])
            end

        #	Return comprehensive statistics at all levels
            return global_measures, triads_ud, node_stats
    end

#	Helper: Symmetric Binary Feature Builder for Network Comparator
    function symmetric_binary_feature_builder(global_stats::DataFrame, triad_census_counts::DataFrame, node_measures::DataFrame)
        """
        Helper function for network_comparator() that builds standardized feature vector from network statistics.
        
        Args:
            global_stats::DataFrame: Global network measures from undirected_binary_constructor
            triad_census_counts::DataFrame: Triad census with columns [:triad, :count, :proportion]
            node_measures::DataFrame: Node-level statistics including community assignments
        Returns:
            DataFrame: Feature vector with columns [:type, :measure, :value]
        Notes:
            - Transforms raw statistics into normalized features
            - Groups features by type for interpretability
            - Pre-allocates arrays for efficiency
            - 2-K Undirected Reach is normalized by N(N-1), where 1 indicates that all nodes are reachable within 2 steps.
        """

        #	Input validation
            @assert hasproperty(global_stats, :measure) && hasproperty(global_stats, :value) "global_stats needs :measure and :value"
            @assert hasproperty(triad_census_counts, :triad) && hasproperty(triad_census_counts, :proportion) "triad_census needs :triad and :proportion"
            @assert hasproperty(node_measures, :node) "node_measures needs :node column"

        #	Deep copy inputs to prevent mutation
            global_stats = deepcopy(global_stats)
            triad_census_counts = deepcopy(triad_census_counts)
            node_measures = deepcopy(node_measures)

        #	========== GLOBAL NETWORK MEASURES ==========

        #	Prepare global stats with row ordering
            global_stats.measure = string.(global_stats.measure)
            global_stats.Obs_ID = 1:nrow(global_stats)
            global_stats = select(global_stats, :Obs_ID, :measure, :value)

        #	Extract key values for normalization
            graph_size = parse(Int64, global_stats[global_stats.measure .== "num_nodes", :value][1])
            num_wcc = parse(Int64, global_stats[global_stats.measure .== "num_wcc", :value][1])

        #	Process component size proportions
            size_measures = ["largest_wcc", "second_largest_wcc", "min_wcc_size", "largest_scc", "second_largest_scc"]
            size_idx = findall(in(size_measures), global_stats.measure)
            
            size_features = DataFrame(
                Obs_ID = global_stats.Obs_ID[size_idx],
                measure = global_stats.measure[size_idx] .* "_proportion",
                value = round.(parse.(Int64, global_stats.value[size_idx]) ./ graph_size, digits=6)
            )

        #	Process WCC type proportions
            type_measures = ["num_isolates", "num_dyads", "num_triads", "num_groups"]
            type_idx = findall(in(type_measures), global_stats.measure)
            
            type_features = DataFrame(
                Obs_ID = global_stats.Obs_ID[type_idx],
                measure = global_stats.measure[type_idx] .* "_proportion",
                value = round.(parse.(Int64, global_stats.value[type_idx]) ./ num_wcc, digits=6)
            )

        #	Retain raw component measures
            kept_measures = ["num_nodes", "num_edges", "num_scc", "bow_tie_scc_fraction", 
                            "bow_tie_in_fraction", "bow_tie_out_fraction"]
            kept_idx = findall(in(kept_measures), global_stats.measure)
            
            kept_features = DataFrame(
                Obs_ID = global_stats.Obs_ID[kept_idx],
                measure = global_stats.measure[kept_idx],
                value = parse.(Float64, global_stats.value[kept_idx])
            )

        #	Combine component features
            component_features = vcat(kept_features, size_features, type_features)
            component_features.type .= "Component Measure"

        #	========== LINK STATISTICS ==========

        #	Process link measures with robust parsing
            link_types = ["all_links", "nonself_links", "self_loops"]
            link_idx = findall(in(link_types), global_stats.measure)
            
            #	Pre-allocate result array (6 stats per type, not 7)
                n_link_features = length(link_types) * 6  
                link_data = Vector{NamedTuple{(:Obs_ID, :type, :measure, :value), Tuple{Int, String, String, Float64}}}(undef, n_link_features)
                
            #	Process each link type
                feature_idx = 1
                prefixes = Dict("all_links" => "all_link_", 
                            "nonself_links" => "non_self_", 
                            "self_loops" => "self_loops_")
                
                stat_names = ["count", "min", "max", "mean", "std", "sum"]
                
                for (i, mkey) in enumerate(link_types)
                    row_idx = link_idx[i]
                    vstr = global_stats.value[row_idx]
                    obsid = global_stats.Obs_ID[row_idx]
                    
                    #	Use regex to extract numeric values robustly
                        numbers = Float64[]
                        for m in eachmatch(r"=\s*([+-]?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?)", vstr)
                            push!(numbers, parse(Float64, m.captures[1]))
                        end
                        
                    #	Take first 6 values (excluding density which appears at end)
                        numbers = numbers[1:min(6, length(numbers))]
                        
                    #	Ensure we have 6 values
                        if length(numbers) < 6
                            @warn "Expected 6 values for $mkey, got $(length(numbers))"
                            resize!(numbers, 6)
                            numbers[length(numbers)+1:6] .= NaN
                        end
                        
                    #	Store in pre-allocated array
                        for (j, stat_name) in enumerate(stat_names)
                            link_data[feature_idx] = (
                                Obs_ID = obsid,
                                type = "Link Measure",
                                measure = prefixes[mkey] * stat_name,
                                value = numbers[j]
                            )
                            feature_idx += 1
                        end
                end
                
                link_features = DataFrame(link_data)

        #	Normalize non-self and self-loop counts to proportions
            num_edges = component_features[component_features.measure .== "num_edges", :value][1]
            
            for (count_name, prop_name) in [("non_self_count", "non_self_proportion"), 
                                            ("self_loops_count", "self_loops_proportion")]
                idx = findfirst(==(count_name), link_features.measure)
                if !isnothing(idx)
                    link_features.value[idx] = link_features.value[idx] / num_edges
                    link_features.measure[idx] = prop_name
                end
            end
            
            #	Remove redundant all_link_count
                filter!(row -> row.measure != "all_link_count", link_features)

        #	========== GLOBAL NETWORK METRICS ==========

        #	Process remaining global measures (including single density)
            global_measures = ["degree assortativity", "transitivity", "local clustering coefficient", 
                               "density", "resolution", "modularity"]
            global_idx = findall(in(global_measures), global_stats.measure)
            
            global_features = DataFrame(
                Obs_ID = global_stats.Obs_ID[global_idx],
                type = fill("Global Network Measure", length(global_idx)),
                measure = global_stats.measure[global_idx],
                value = parse.(Float64, global_stats.value[global_idx])
            )

        #	Combine all global-level features
            global_all = vcat(component_features, link_features, global_features)
            sort!(global_all, :Obs_ID)
            select!(global_all, Not(:Obs_ID))

        #	========== TRIAD CENSUS ==========

        #	Create triad census features
            triad_features = DataFrame(
                type = fill("Triad Census", nrow(triad_census_counts)),
                measure = triad_census_counts.triad,
                value = triad_census_counts.proportion
            )

        #	========== K-CORE DECOMPOSITION ==========

        #	Compute k-core membership distribution
            n_nodes = nrow(node_measures)
            k_core_groups = combine(
                groupby(node_measures, :k_core_all),
                nrow => :count
            )
            sort!(k_core_groups, :k_core_all)
            
            k_core_features = DataFrame(
                type = fill("K-Core Decomposition", nrow(k_core_groups)),
                measure = "k_core_all_" .* string.(k_core_groups.k_core_all),
                value = round.(k_core_groups.count ./ n_nodes, digits=6)
            )

        #	========== COMMUNITY STRUCTURE ==========

        #	Compute community size distribution
            community_groups = combine(
                groupby(node_measures, :community),
                nrow => :count
            )
            sort!(community_groups, :count, rev=true)
            
            community_features = DataFrame(
                type = fill("Community Structure", nrow(community_groups)),
                measure = "Community_" .* string.(1:nrow(community_groups)),
                value = round.(community_groups.count ./ n_nodes, digits=6)
            )

        #	========== NODE-LEVEL AGGREGATES ==========

        #	Normalize 2-step reach per node (undirected): proportion of nodes reachable within 2 steps
        #	(so 1.0 means the node can reach every other node within two hops)
            full_n = parse(Int64,global_stats.value[1])
            if hasproperty(node_measures, :undirected_reach_2)
                #	Guard against n_nodes ≤ 1
                    den = full_n * (full_n-1)
                    node_measures.undirected_reach_2_normalized = node_measures.undirected_reach_2 ./ den
            end

        #	Define measures and their types
            node_measures_config = [
                ("total_degree_normalized", "Degree Measures"),
                ("in_group_ratio", "Degree Measures"),
                ("undirected_reach_2_normalized", "Local Reach"),
                ("ego_density", "Local Structure"),
                ("density_clustering_coefficient", "Local Structure"),
                ("modularity_vitality_hub", "Influence"),
                ("modularity_vitality_bridge", "Influence")
            ]
            
        #	Pre-allocate node features array
            n_node_features = length(node_measures_config) * 5  # 5 stats per measure
            node_data = Vector{NamedTuple{(:type, :measure, :value), Tuple{String, String, Float64}}}(undef, n_node_features)
            
        #	Compute aggregate statistics efficiently
            feature_idx = 1
            for (col_name, feat_type) in node_measures_config
                if hasproperty(node_measures, Symbol(col_name))
                    col_data = node_measures[!, col_name]
                    
                    #	Compute statistics
                        stats = (
                            mean = mean(col_data),
                            median = median(col_data),
                            std = std(col_data),
                            skew = skew_about_mean(col_data),
                            kurtosis = kurtosis_about_mean(col_data)
                        )
                        
                    #	Store in pre-allocated array
                        for (stat_name, stat_value) in pairs(stats)
                            node_data[feature_idx] = (
                                type = feat_type,
                                measure = col_name * "_" * string(stat_name),
                                value = round(stat_value, digits=6)
                            )
                            feature_idx += 1
                        end
                end
            end
            
            node_features = DataFrame(node_data[1:feature_idx-1])

        #	========== COMBINE ALL FEATURES ==========

        #	Combine all feature DataFrames
            feature_vector = vcat(
                global_all,
                triad_features,
                k_core_features,
                community_features,
                node_features
            )
            
            return feature_vector
    end

#	Helper: Undirected Weighted Network Constructor for Comparisons
    function undirected_weighted_constructor(edges::DataFrame, 
                                            nodes::Union{Nothing,DataFrame,AbstractVector{<:AbstractString}};
                                            resolution_sweep::Bool = false, 
                                            resolution::Float64 = 1.0, 
                                            directed::Bool = false, 
                                            weighted::Bool = true, 
                                            n_resolutions::Int = 15, 
                                            n_runs_per_gamma::Int = 5, 
                                            n_iterations_per_run::Int = 10, 
                                            seed::Union{Int,Nothing} = nothing, 
                                            provided_membership::Union{Nothing,DataFrame,Vector{Int},Dict} = nothing)
        """
        Helper function for network_comparator() that constructs undirected weighted network and computes comprehensive statistics.
        
        Args:
            edges::DataFrame: Edge list with :src, :dst, optional :weight columns
            nodes: Node universe (includes isolates if present)
            resolution_sweep::Bool: Use CHAMP multi-resolution community detection
            resolution::Float64: Resolution parameter for community detection
            directed::Bool: Original network directionality (always converted to undirected)
            weighted::Bool: Whether to use weights (default true)
            n_resolutions::Int: Number of resolutions for CHAMP sweep
            n_runs_per_gamma::Int: Leiden runs per resolution
            n_iterations_per_run::Int: Iterations per Leiden run
            seed: Random seed for reproducibility
            provided_membership: Optional pre-computed community assignments
        Returns:
            Tuple of three elements:
                1. global_measures: DataFrame of network-level statistics
                2. triads_summary: Summary statistics of weighted triad census
                3. node_stats: DataFrame of node-level metrics including community membership
        Notes:
            - Produces undirected weighted network regardless of directed parameter
            - Symmetrizes via A + A' (preserving original self-loops)
            - Computes weighted versions of all metrics
            - Used internally by network_comparator() for weighted comparisons
        """

        #	========== NETWORK TRANSFORMATION ==========

        #	Create working copy and prepare weights
            clean_edges = deepcopy(edges) 

        #	Standardize weight column 
            if weighted
                #	Ensure weight column exists with proper type
                    if !hasproperty(clean_edges, :weight)
                        clean_edges.weight = ones(Float64, nrow(clean_edges))
                    else
                        clean_edges.weight = Float64.(clean_edges.weight)
                    end
                    agg_func = sum
            else
                #	Force binary weights
                    clean_edges.weight = ones(Float64, nrow(clean_edges))
                    agg_func = maximum
            end

        #   Checking if Edgelist is Symmetric
            src = clean_edges[:,1]
            dst = clean_edges[:,2]

        #   Canonical unordered form (always small→large)
            canon_src = min.(src, dst)
            canon_dst = max.(src, dst)

        #   Count unordered pairs
            pair_counts = combine(groupby(DataFrame(src=canon_src, dst=canon_dst), [:src, :dst]),
                                nrow => :count)

        #   Check if every unordered dyad appears exactly once
            is_already_symmetric = all(pair_counts.count .== 1)

            if !is_already_symmetric
                #	Build adjacency matrix
                    adj_base, node_map, idx_to_node = _graph_to_sparse_matrix(clean_edges; nodes = nodes, weighted = true)

                #	Preserve node index for community detection
                    ni = deepcopy(idx_to_node)

                #	Symmetrize adjacency
                    adj = adj_base + adj_base'

                #	Restore original self-loops (not doubled)
                    n = size(adj, 1)
                    @inbounds for i in 1:n
                        adj[i,i] = adj_base[i,i]
                    end

                #	Output unified symmetric edgelist
                    symmetric_edgelist = _symmetric_sparse_to_undirected_edgelist(adj; include_diagonal = true, node_map = node_map)
            else
                #   Already symmetric — nothing to fix
                    symmetric_edgelist = deepcopy(clean_edges)
            end

        #	========== GLOBAL NETWORK MEASURES ==========

        #	Component statistics
            component_stats = component_statistics(symmetric_edgelist, nodes = ni, graph_type = :undirected)
            component_stat_names = collect(keys(component_stats))      
            component_stat_values = collect(values(component_stats)) 

        #	Link statistics
            link_stats = link_statistics(symmetric_edgelist; nodes = ni, graph_type = :undirected, weighted = true)
            link_stats_tuple = _summarize_link_stats(link_stats)
            link_stats_df = link_stats_tuple[1]

        #	Global clustering and assortativity
            degree_assortativity = assortativity_degree(symmetric_edgelist; graph_type = :undirected, weighted = true)
            
            transitivity = global_clustering_coefficient(symmetric_edgelist; directed = false, weighted = true, method = :transitivity, drop_self_loops = true)
            
            global_local_clustering_coeff = global_clustering_coefficient(symmetric_edgelist; directed = false, weighted = true, method = :average, drop_self_loops = true)

        #	Assemble global statistics
            global_measures = [
                component_stat_names; 
                link_stats_df.link_group; 
                "degree assortativity"; 
                "transitivity"; 
                "local clustering coefficient"; 
                "density"
            ]
            
            global_values = string.([
                component_stat_values; 
                link_stats_df.values; 
                round(degree_assortativity, digits=6); 
                round(transitivity, digits=6); 
                round(global_local_clustering_coeff, digits=6);
                round(link_stats.density[1], digits=6)
            ])
            
            global_stats_df = DataFrame(measure = global_measures, value = global_values)

        #	========== MESO-LEVEL (COMMUNITY) MEASURES ==========

        #	Weighted triad census
            triads_w_ud = triad_census(symmetric_edgelist; weighted = true, graph_type = :undirected)
            triads_summary = triads_w_ud.summary

        #	Community detection or process provided membership
            resolution_used = resolution
            
            if provided_membership === nothing
                #	Perform community detection
                    if resolution_sweep
                        #	Multi-resolution CHAMP sweep
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
                            
                        #	Extract partition
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
                        #	Single resolution Leiden
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
                            
                        #	Extract partition
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
                #	Process user-provided partition
                    if provided_membership isa DataFrame
                        #	DataFrame with node and community columns
                            pm = deepcopy(provided_membership)
                            rename!(pm, lowercase.(string.(propertynames(pm))))
                            @assert hasproperty(pm, :node) && hasproperty(pm, :community) "DataFrame needs :node and :community"
                            partition_df = DataFrame(
                                node = String.(pm.node),
                                community = Int.(pm.community)
                            )
                            
                    elseif provided_membership isa Vector
                        #	Vector aligned to matrix order
                            @assert length(provided_membership) == length(ni) "Vector length must match node count"
                            partition_df = DataFrame(
                                node = String.(ni isa DataFrame ? ni.id : ni),
                                community = Int.(provided_membership)
                            )
                            
                    elseif provided_membership isa Dict
                        #	Dictionary mapping node IDs to communities
                            node_ids = ni isa DataFrame ? String.(ni.id) : String.(ni)
                            communities = zeros(Int64, length(node_ids))
                            for i in eachindex(node_ids)
                                communities[i] = get(provided_membership, node_ids[i], 0)
                            end
                            partition_df = DataFrame(
                                node = node_ids,
                                community = communities
                            )
                    end
            end

        #	Calculate modularity if using provided membership
            if !isnothing(provided_membership)
                #	Build adjacency for connected nodes
                    adj_symmetric, node_map_symmetric, idx_to_node_symmetric = _graph_to_sparse_matrix(symmetric_edgelist; weighted = false)

                #	Align partition to connected nodes
                    keep_index = DataFrame(
                        node = idx_to_node_symmetric, 
                        keep = ones(Int64, length(idx_to_node_symmetric))
                    )
                    keep_index = leftjoin!(keep_index, partition_df, on = :node)
                    keep_index.community = convert.(Int64, keep_index.community)

                #	Calculate modularity
                    modularity = calculate_modularity(adj_symmetric, keep_index.community, γ = resolution)
                    resolution_used = resolution
            end
            
        #	Add modularity and resolution to global statistics
            partition_stats_df = DataFrame(
                measure = ["resolution", "modularity"], 
                value = string.(round.([resolution_used, modularity], digits=6))
            )
            global_measures = [global_stats_df; partition_stats_df]

        #	Calculate group-level statistics
            group_statistics_dict = group_statistics(symmetric_edgelist; membership = partition_df, directed = false, weighted = true)

        #	Extract and enhance node statistics
            node_stats = group_statistics_dict.node_stats
            node_stats.in_group_ratio = node_stats.total_degree_in_group ./ node_stats.total_degree
            node_stats.internal_strength_fraction = node_stats.weighted_total_degree_in_group ./ node_stats.weighted_total_degree

        #	========== NODE-LEVEL MEASURES ==========

        #	K-core decomposition
            k_core_all = core_decomposition(symmetric_edgelist; weighted = false, mode = "total")
            rename!(k_core_all, ["node", "k_core_all"])
            leftjoin!(node_stats, k_core_all, on = :node)
            node_stats.k_core_all = convert.(Int64, node_stats.k_core_all)

        #	2-hop reachability
            all_hop_reach = hop_reach_k(symmetric_edgelist, mode = "all", k = 2) 
            rename!(all_hop_reach, ["node", "undirected_reach_2"])
            leftjoin!(node_stats, all_hop_reach, on = :node)
            node_stats.undirected_reach_2 = convert.(Int64, node_stats.undirected_reach_2)

        #	Normalized degree centrality
            total_deg_norm = total_degree(symmetric_edgelist; directed = false, weighted = true, normalize = true, drop_self_loops = true, count_self_loops_once = true, agg_func = maximum, n = nrow(ni))
            rename!(total_deg_norm, ["node", "total_degree_normalized"])
            leftjoin!(node_stats, total_deg_norm, on = :node)
            
        #	Handle isolates
            node_stats.total_degree_normalized = coalesce.(node_stats.total_degree_normalized, 0.0)
            node_stats.total_degree_normalized = convert.(Float64, node_stats.total_degree_normalized)

        #	Local clustering coefficient (ORA-style)
            local_density_clustering = local_clustering_coefficient(symmetric_edgelist; directed = false, method = :local_density)   
            rename!(local_density_clustering, ["node", "ego_density", "density_clustering_coefficient"])   
            leftjoin!(node_stats, local_density_clustering, on = :node)     
            node_stats.density_clustering_coefficient = convert.(Float64, node_stats.density_clustering_coefficient) 

        #	Weighted clustering coefficient (Barrat et al. 2004)
            barrat_clustering_coefficients = weighted_clustering_coefficient(symmetric_edgelist; directed = false, agg_func = sum)
            rename!(barrat_clustering_coefficients, ["node", "barrat_weighted_clustering"])
            barrat_clustering_coefficients.barrat_weighted_clustering = Array(barrat_clustering_coefficients.barrat_weighted_clustering)
            leftjoin!(node_stats, barrat_clustering_coefficients, on = :node)
            node_stats.barrat_weighted_clustering = convert.(Float64, node_stats.barrat_weighted_clustering)

        #	Modularity vitality (hub and bridge scores)
            modularity_scores = modularity_vitality(symmetric_edgelist; directed = false, resolution = resolution_used, weighted = true, resolution_sweep = false, provided_membership = partition_df)
            leftjoin!(node_stats, modularity_scores.results_df[:,[1,3,4]], on = :node)
            
        #	Convert vitality scores to proper type
            var_names = names(modularity_scores.results_df[:,[3,4]])
            for i in eachindex(var_names)
                node_stats[!, var_names[i]] = convert.(Float64, node_stats[:, var_names[i]])
            end

        #	Return comprehensive statistics at all levels
            return global_measures, triads_summary, node_stats
    end

#	Helper: Symmetric Weighted Feature Builder for Network Comparator
    function symmetric_weighted_feature_builder(global_stats::DataFrame, 
                                            triad_census_counts::DataFrame, 
                                            node_measures::DataFrame)
        """
        Helper function for network_comparator() that builds standardized feature vector from weighted network statistics.
        
        Args:
            global_stats::DataFrame: Global network measures from undirected_weighted_constructor
            triad_census_counts::DataFrame: Weighted triad census with columns [:triad, :AUMC_density, :peak_tau, :peak_density]
            node_measures::DataFrame: Node-level statistics including community assignments
        Returns:
            DataFrame: Feature vector with columns [:type, :measure, :value]
        Notes:
            - Transforms raw weighted statistics into normalized features
            - Groups features by type for interpretability
            - Pre-allocates arrays for efficiency
            - Deep copies inputs to prevent mutation
            - Includes weighted-specific metrics (Barrat clustering, strength fractions)
        """

        #	Input validation
            @assert hasproperty(global_stats, :measure) && hasproperty(global_stats, :value) "global_stats needs :measure and :value"
            @assert hasproperty(triad_census_counts, :triad) && hasproperty(triad_census_counts, :peak_density) "triad_census needs :triad and :peak_density"
            @assert hasproperty(node_measures, :node) "node_measures needs :node column"

        #	Deep copy inputs to prevent mutation
            global_stats = deepcopy(global_stats)
            triad_census_counts = deepcopy(triad_census_counts)
            node_measures = deepcopy(node_measures)

        #	========== GLOBAL NETWORK MEASURES ==========

        #	Prepare global stats with row ordering
            global_stats.measure = string.(global_stats.measure)
            global_stats.Obs_ID = 1:nrow(global_stats)
            global_stats = select(global_stats, :Obs_ID, :measure, :value)

        #	Extract key values for normalization
            graph_size = parse(Int64, global_stats[global_stats.measure .== "num_nodes", :value][1])
            num_wcc = parse(Int64, global_stats[global_stats.measure .== "num_wcc", :value][1])

        #	Process component size proportions
            size_measures = ["largest_wcc", "second_largest_wcc", "min_wcc_size", "largest_scc", "second_largest_scc"]
            size_idx = findall(in(size_measures), global_stats.measure)
            
            size_features = DataFrame(
                Obs_ID = global_stats.Obs_ID[size_idx],
                measure = global_stats.measure[size_idx] .* "_proportion",
                value = round.(parse.(Int64, global_stats.value[size_idx]) ./ graph_size, digits=6)
            )

        #	Process WCC type proportions
            type_measures = ["num_isolates", "num_dyads", "num_triads", "num_groups"]
            type_idx = findall(in(type_measures), global_stats.measure)
            
            type_features = DataFrame(
                Obs_ID = global_stats.Obs_ID[type_idx],
                measure = global_stats.measure[type_idx] .* "_proportion",
                value = round.(parse.(Int64, global_stats.value[type_idx]) ./ num_wcc, digits=6)
            )

        #	Retain raw component measures
            kept_measures = ["num_nodes", "num_edges", "num_scc", "bow_tie_scc_fraction", 
                            "bow_tie_in_fraction", "bow_tie_out_fraction"]
            kept_idx = findall(in(kept_measures), global_stats.measure)
            
            kept_features = DataFrame(
                Obs_ID = global_stats.Obs_ID[kept_idx],
                measure = global_stats.measure[kept_idx],
                value = parse.(Float64, global_stats.value[kept_idx])
            )

        #	Combine component features
            component_features = vcat(kept_features, size_features, type_features)
            component_features.type .= "Component Measure"

        #	========== LINK STATISTICS ==========

        #	Process link measures with robust parsing
            link_types = ["all_links", "nonself_links", "self_loops"]
            link_idx = findall(in(link_types), global_stats.measure)
            
        #	Pre-allocate result array (6 stats per type)
            n_link_features = length(link_types) * 6  
            link_data = Vector{NamedTuple{(:Obs_ID, :type, :measure, :value), Tuple{Int, String, String, Float64}}}(undef, n_link_features)
            
        #	Process each link type
            feature_idx = 1
            prefixes = Dict("all_links" => "all_link_", 
                        "nonself_links" => "non_self_", 
                        "self_loops" => "self_loops_")
            
            stat_names = ["count", "min", "max", "mean", "std", "sum"]
            
            for (i, mkey) in enumerate(link_types)
                #	Define update objects & parameters
                    row_idx = link_idx[i]
                    vstr = global_stats.value[row_idx]
                    obsid = global_stats.Obs_ID[row_idx]
                    
                #	Use regex to extract numeric values robustly
                    numbers = Float64[]
                    for m in eachmatch(r"=\s*([+-]?\d+(?:\.\d+)?(?:[eE][+-]?\d+)?)", vstr)
                        push!(numbers, parse(Float64, m.captures[1]))
                    end
                    
                #	Take first 6 values (excluding density)
                    numbers = numbers[1:min(6, length(numbers))]
                    
                #	Ensure we have 6 values
                    if length(numbers) < 6
                        @warn "Expected 6 values for $mkey, got $(length(numbers))"
                        resize!(numbers, 6)
                        numbers[length(numbers)+1:6] .= NaN
                    end
                    
                #	Store in pre-allocated array
                    for (j, stat_name) in enumerate(stat_names)
                        link_data[feature_idx] = (
                            Obs_ID = obsid,
                            type = "Link Measure",
                            measure = prefixes[mkey] * stat_name,
                            value = numbers[j]
                        )
                        feature_idx += 1
                    end
            end
            
            link_features = DataFrame(link_data)

        #	Normalize non-self and self-loop counts to proportions
            num_edges = component_features[component_features.measure .== "num_edges", :value][1]
            
            for (count_name, prop_name) in [("non_self_count", "non_self_proportion"), 
                                            ("self_loops_count", "self_loops_proportion")]
                idx = findfirst(==(count_name), link_features.measure)
                if !isnothing(idx)
                    link_features.value[idx] = link_features.value[idx] / num_edges
                    link_features.measure[idx] = prop_name
                end
            end
            
        #	Remove redundant all_link_count
            filter!(row -> row.measure != "all_link_count", link_features)

        #	========== GLOBAL NETWORK METRICS ==========

        #	Process remaining global measures
            global_measures = ["degree assortativity", "transitivity", "local clustering coefficient", 
                            "density", "resolution", "modularity"]
            global_idx = findall(in(global_measures), global_stats.measure)
            
            global_features = DataFrame(
                Obs_ID = global_stats.Obs_ID[global_idx],
                type = fill("Global Network Measure", length(global_idx)),
                measure = global_stats.measure[global_idx],
                value = parse.(Float64, global_stats.value[global_idx])
            )

        #	Combine all global-level features
            global_all = vcat(component_features, link_features, global_features)
            sort!(global_all, :Obs_ID)
            select!(global_all, Not(:Obs_ID))

        #	========== TRIAD CENSUS ==========

        #	Create triad census features (weighted-specific metrics)
            AUMC_density = triad_census_counts[:, 1:2]
            rename!(AUMC_density, ["measure", "value"])
            AUMC_density.measure = AUMC_density.measure .* "_AUMC_density"

            peak_tau = triad_census_counts[:, [1, 3]]
            rename!(peak_tau, ["measure", "value"])
            peak_tau.measure = peak_tau.measure .* "_peak_tau"

            peak_density = triad_census_counts[:, [1, 4]]
            rename!(peak_density, ["measure", "value"])
            peak_density.measure = peak_density.measure .* "_peak_density"

            triad_features = vcat(AUMC_density, peak_tau, peak_density)
            triad_features.type .= "Triad Census"

        #	========== K-CORE DECOMPOSITION ==========

        #	Compute k-core membership distribution
            n_nodes = nrow(node_measures)
            k_core_groups = combine(
                groupby(node_measures, :k_core_all),
                nrow => :count
            )
            sort!(k_core_groups, :k_core_all)
            
            k_core_features = DataFrame(
                type = fill("K-Core Decomposition", nrow(k_core_groups)),
                measure = "k_core_all_" .* string.(k_core_groups.k_core_all),
                value = round.(k_core_groups.count ./ n_nodes, digits=6)
            )

        #	========== COMMUNITY STRUCTURE ==========

        #	Compute community size distribution
            community_groups = combine(
                groupby(node_measures, :community),
                nrow => :count
            )
            sort!(community_groups, :count, rev=true)
            
            community_features = DataFrame(
                type = fill("Community Structure", nrow(community_groups)),
                measure = "Community_" .* string.(1:nrow(community_groups)),
                value = round.(community_groups.count ./ n_nodes, digits=6)
            )

        #	========== NODE-LEVEL AGGREGATES ==========

        #	Normalize 2-step reach (proportion of network reachable within 2 steps)
            full_n = parse(Int64, global_stats.value[1])
            if hasproperty(node_measures, :undirected_reach_2)
                #	Guard against n_nodes ≤ 1
                    den = max(full_n * (full_n - 1), 1)
                    node_measures.undirected_reach_2_normalized = node_measures.undirected_reach_2 ./ den
            end

        #	Define measures and their types
            node_measures_config = [
                ("total_degree_normalized", "Degree Measures"),
                ("in_group_ratio", "Degree Measures"),
                ("internal_strength_fraction", "Degree Measures"),
                ("undirected_reach_2_normalized", "Local Reach"),
                ("ego_density", "Local Structure"),
                ("density_clustering_coefficient", "Local Structure"),
                ("barrat_weighted_clustering", "Local Structure"),
                ("modularity_vitality_hub", "Influence"),
                ("modularity_vitality_bridge", "Influence")
            ]
            
        #	Pre-allocate node features array
            n_node_features = length(node_measures_config) * 5  # 5 stats per measure
            node_data = Vector{NamedTuple{(:type, :measure, :value), Tuple{String, String, Float64}}}(undef, n_node_features)
            
        #	Compute aggregate statistics efficiently
            feature_idx = 1
            for (col_name, feat_type) in node_measures_config
                if hasproperty(node_measures, Symbol(col_name))
                    col_data = node_measures[!, col_name]
                    
                    #	Compute statistics
                        stats = (
                            mean = mean(col_data),
                            median = median(col_data),
                            std = std(col_data),
                            skew = skew_about_mean(col_data),
                            kurtosis = kurtosis_about_mean(col_data)
                        )
                        
                    #	Store in pre-allocated array
                        for (stat_name, stat_value) in pairs(stats)
                            node_data[feature_idx] = (
                                type = feat_type,
                                measure = col_name * "_" * string(stat_name),
                                value = round(stat_value, digits=6)
                            )
                            feature_idx += 1
                        end
                end
            end
            
            node_features = DataFrame(node_data[1:feature_idx-1])

        #	========== COMBINE ALL FEATURES ==========

        #	Combine all feature DataFrames
            feature_vector = vcat(
                global_all,
                triad_features,
                k_core_features,
                community_features,
                node_features
            )

        #	Return feature vector
            return feature_vector
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
  


########################################################################
#   ASSESSMENT OF THE DESIGN MATRICES' CONSTRUCTORS & FEATURE VECTORS  #
########################################################################

#   Generating Undirected/Binary Graph Design Matrices from which to Create Feature Vectors
    global_stats, triad_census_counts, node_measures = undirected_binary_constructor(balikatan_arcs, nodes; directed=false, 
                                                                                     weighted=false, resolution_sweep=true)

    global_stats, triad_census_counts, node_measures = undirected_binary_constructor(balikatan_arcs, nodes; directed=false, 
                                                                                    weighted=false, resolution=1.0)

#	Constructing Undirected/Binary Feature Vector
    symmeric_binary_feature_vector = symmetric_binary_feature_builder(global_stats, triad_census_counts, node_measures)

#   Generating Undirected/Weighted Graph Design Matrices from which to Create Feature Vectors
    global_stats, triad_census_counts, node_measures = undirected_weighted_constructor(balikatan_arcs, nodes; directed=false, 
                                                                                       weighted=true, resolution=1.0)

    global_stats, triad_census_counts, node_measures = undirected_weighted_constructor(balikatan_arcs, nodes; directed=false, 
                                                                                       weighted=true, resolution_sweep=true)

#	Constructing Undirected/Weighted Feature Vector
    symmeric_weighted_feature_vector = symmetric_weighted_feature_builder(global_stats, triad_census_counts, node_measures)


#   Normalizing 2k-reach for directed graphs
#   2k_out/n-1
#   2k_in/n-1



######################################
#   COMPARATOR FUNCTION ASSESSMENT   #
######################################

