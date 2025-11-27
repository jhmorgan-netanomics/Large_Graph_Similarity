module CLI

using ArgParse
using CSV
using DataFrames
using Dates
using ..Large_Graph_Similarity  # Parent module that exports all the functions

#	Helper Function for CLI: Validate and Load Edge List
	function _load_edge_list(filepath::String; verbose::Bool = true)
		"""
		Args:
			filepath::String: Path to CSV/TSV edge list file
			verbose::Bool: Print diagnostic messages (default true)
		Returns:
			DataFrame: Edge list with :src, :dst, optional :weight columns
		Notes:
			Accepts column name variants:
			- src/dst (canonical)
			- source/target, Source Node ID/Target Node ID
			- from/to, From Node/To Node
			Auto-detects CSV vs TSV delimiter
		"""
		
		#	Validation
			if !isfile(filepath)
				throw(ArgumentError("Edge list file not found: $filepath"))
			end
		
		#	Delimiter Detection
			first_line = readline(filepath)
			delimiter = occursin("\t", first_line) ? '\t' : ','
			
			if verbose
				println("Loading edge list from: $filepath")
				println("Detected delimiter: $(delimiter == '\t' ? "TAB" : "COMMA")")
			end
		
		#	Load DataFrame
			df = CSV.read(filepath, DataFrame; delim = delimiter)
		
		#	Column Name Standardization
			original_names = names(df)
			lower_names = lowercase.(string.(original_names))
			
			idx_src = nothing
			idx_dst = nothing
		
		#	Source/Destination Column Detection
			if "src" in lower_names && "dst" in lower_names
				#	Canonical names found
					idx_src = findfirst(x -> x == "src", lower_names)
					idx_dst = findfirst(x -> x == "dst", lower_names)
			elseif any(occursin.("source", lower_names)) && any(occursin.("target", lower_names))
				#	Source/target variant
					idx_src = findfirst(n -> occursin("source", n), lower_names)
					idx_dst = findfirst(n -> occursin("target", n), lower_names)
			elseif any(occursin.("from", lower_names)) && any(occursin.("to", lower_names))
				#	From/to variant
					idx_src = findfirst(n -> occursin("from", n), lower_names)
					idx_dst = findfirst(n -> occursin("to", n), lower_names)
			else
				#	No valid column names found
					throw(ArgumentError(
						"Edge list must contain columns identifiable as (src,dst), " *
						"(source,target), or (from,to). Found: $(join(original_names, ", "))"
					))
			end
		
		#	Rename to Canonical Form
			rename!(df, original_names[idx_src] => :src,
					   original_names[idx_dst] => :dst)
		
		#	Weight Column Detection
			current_names = names(df)
			lower_current = lowercase.(string.(current_names))
			
			weight_idx = findfirst(n -> n == "weight", lower_current)
			
			if weight_idx === nothing
				#	Check for common variants
					weight_idx = findfirst(n -> occursin("weight", n) || occursin("link value", n), lower_current)
			end
			
			if weight_idx !== nothing
				#	Standardize weight column
					rename!(df, current_names[weight_idx] => :weight)
					df.weight = Float64.(df.weight)
			elseif verbose
				println("No weight column found - assuming unit weights")
			end
		
		#	Type Conversion
			df.src = string.(df.src)
			df.dst = string.(df.dst)
		
		#	Assembling Result
			result = select(df, :src, :dst)
			if hasproperty(df, :weight)
				result.weight = df.weight
			end
			
			if verbose
				n_edges = nrow(result)
				n_nodes = length(union(result.src, result.dst))
				println("Loaded $n_edges edges connecting $n_nodes unique nodes")
			end
		
		#	Return Edge List
			return result
	end

#	Helper Function for CLI: Load Node List
	function _load_node_list(filepath::String; verbose::Bool = true)
		"""
		Args:
			filepath::String: Path to node list file (CSV/TSV/text)
			verbose::Bool: Print diagnostic messages (default true)
		Returns:
			DataFrame: Node list with :id and :label columns
		Notes:
			ID column detection (case-insensitive):
			- Exact: "id"
			- Contains: "node" + "id" or "name" + "id"
			Label column detection:
			- Exact: "label"
			- Contains: "node" + "label"
			Single column files treated as node IDs
		"""
		#	Validation
			if !isfile(filepath)
				throw(ArgumentError("Node list file not found: $filepath"))
			end
			
			if verbose
				println("Loading node list from: $filepath")
			end
		
		#	Helper: Normalize Column Names
			normalize_name(name::Symbol) = normalize_name(String(name))
			function normalize_name(name::AbstractString)
				#	Convert to standard form
					s = lowercase(strip(name))
				#	Replace non-alphanumeric with spaces
					s = replace(s, r"[^0-9a-z]+" => " ")
					s = replace(s, r"\s+" => " ")
				#	Return normalized string
					return s
			end
		
		#	Helper: Detect ID Column
			function is_id_col(norm::AbstractString)
				#	Parse normalized name
					words = split(norm, ' ')
				
				#	Check for ID patterns
					if norm == "id"
						return true
					end
					
					has_id   = "id"   in words
					has_node = "node" in words
					has_name = "name" in words
					
					return (has_id && has_node) || (has_id && has_name)
			end
		
		#	Helper: Detect Label Column
			function is_label_col(norm::AbstractString)
				#	Parse normalized name
					words = split(norm, ' ')
				
				#	Check for label patterns
					if norm == "label"
						return true
					end
					
					has_label = "label" in words
					has_node  = "node"  in words
					
					return has_label && has_node
			end
		
		#	First attempt: load as structured CSV/TSV
			df = DataFrame()
			try
				#   Detect delimiter from first line
					first_line = open(filepath) do io
						readline(io)
					end
					delim = occursin('\t', first_line) ? '\t' : ','

					if verbose
						println("Detected delimiter: ", delim == '\t' ? "TAB" : "COMMA")
					end

        		#   Load as structured file with chosen delimiter
            		df = CSV.read(filepath, DataFrame; delim = delim)
			catch e
				#	If CSV.read itself fails, fall back to line-based loader
					if verbose
						println("Failed to read as CSV/TSV ($(typeof(e))): falling back to text mode")
					end
				
					lines = readlines(filepath)
					ids = String[strip(line) for line in lines if !isempty(strip(line))]
					result = DataFrame(id = ids, label = ids)
				
				#	Ensure String columns
					result.id    = string.(result.id)
					result.label = string.(result.label)
				
					if verbose
						println("Loaded $(length(ids)) nodes from text file")
					end
				
					return result
			end
		
		#	At this point, df is a valid DataFrame from CSV/TSV
			original_names = names(df)
			norm_names     = normalize_name.(original_names)
		
		# 	Column Detection 
			id_indices = Int[] 
			label_indices = Int[] 
			for (i, n) in enumerate(norm_names) 
				if is_id_col(n) push!(id_indices, i) 
				end 
			
				if is_label_col(n) push!(label_indices, i) 
				end 
			end 
	
		# 	Column Selection 
			id_idx = isempty(id_indices) ? nothing : first(id_indices) 
			label_idx = isempty(label_indices) ? nothing : first(label_indices) 
			result = DataFrame()
		
		#	Build Result Based on Available Columns 
			if id_idx !== nothing && label_idx !== nothing 
				# 	Both columns found 
					id_col = original_names[id_idx] 
					label_col = original_names[label_idx] 
					
					if verbose println("Using ID column: '$id_col'") 
						println("Using label column: '$label_col'") 
					end 
					
					ids = strip.(string.(df[!, id_col])) 
					
					labels = strip.(string.(df[!, label_col])) 
					
					result = DataFrame(id = ids, label = labels) 
		
			elseif id_idx !== nothing 
				# 	Only ID column found → use as both id and label 
					id_col = original_names[id_idx] 
		
					if verbose println("Using ID column: '$id_col'") 
						println("No label-like column found; using ID as label") 
					end 
		
					ids = strip.(string.(df[!, id_col])) 
		
					result = DataFrame(id = ids, label = ids) 
			elseif label_idx !== nothing 
				# 	Only label-like column found → use as both id and label 
					label_col = original_names[label_idx] 
		
					if verbose 
						println("Using label column: '$label_col' as both id and label") 
					end 
			
					labels = strip.(string.(df[!, label_col])) 
			
					result = DataFrame(id = labels, label = labels) 
			elseif ncol(df) == 1 
				#	Single column as IDs 
					if verbose 
						println("Single-column node file; using column '$(original_names[1])' as id and label") 
					end 
		
					ids = strip.(string.(df[!, 1])) 
			
					result = DataFrame(id = ids, label = ids) 
			else 
				# 	No valid columns found msg = 
					""" 
					Node list must contain an ID-like column ('ID', 'Node ID', 'Name ID', etc.) 
					or be a single-column file. Found columns: $(join(string.(original_names), ", ")) 
					""" 
					throw(ArgumentError(msg)) 
			end 
	
		# 	Report Success 
			if verbose 
				println("Loaded $(nrow(result)) nodes from CSV/TSV") 
			end 
			
		# 	ID & Label Are Proper String Vectors 
			result.id = string.(result.id) 
			result.label = string.(result.label) 
			
		# 	Return Nodelist 
			return result
	end

#	Helper Function for CLI: Load Partition
	function _load_partition(filepath::String; node_list = nothing, verbose::Bool = true)
		"""
		Args:
			filepath::String: Path to partition file
			node_list::Union{Nothing,DataFrame}: Reference node list for validation
			verbose::Bool: Print diagnostic messages
		Returns:
			DataFrame: Partition with :id and :community columns (community as Int)
		Notes:
			- Accepts CSV with ID-like and community-like columns
			- ID-like: "id" or any column whose name contains both "node" and "id"
			(case-insensitive), e.g. "Node ID", "node_id", "node-id"
			- Community-like: "community", "node community", or any column whose
			name contains "leiden" (case-insensitive), e.g. "leiden group"
			- If a single column is provided, it is treated as a community vector
			aligned to node_list.id
		"""

		#   Helper: coerce community vector to Int
			_coerce_to_int(vec, colname::AbstractString) = begin
				T = eltype(vec)
				if T <: Integer
					return Int.(vec)
				elseif T <: AbstractFloat
					return Int.(round.(vec))
				elseif T <: AbstractString
					try
						return parse.(Int, strip.(vec))
					catch
						msg = """
						Community column '$colname' must be coercible to Int.
						Found string values that cannot be parsed as integers.
						First few unique values: $(join(string.(unique(vec[1:min(end, 5)])), ", "))
						"""
						throw(ArgumentError(msg))
					end
				else
					msg = """
					Community column '$colname' has unsupported element type $(T).
					Expected an integer, float, or string column that can be converted to Int.
					"""
					throw(ArgumentError(msg))
				end
			end

		#   Validation
			if !isfile(filepath)
				throw(ArgumentError("Partition file not found: $filepath"))
			end

			if verbose
				println("Loading partition from: $filepath")
			end

		#   Load DataFrame (CSV/TSV with delimiter detection)
			df = DataFrame()
			try
				#   Detect delimiter from first line
					first_line = open(filepath) do io
						readline(io)
					end
					delim = occursin('\t', first_line) ? '\t' : ','

					if verbose
						println("Detected delimiter: ", delim == '\t' ? "TAB" : "COMMA")
					end

				#   Load as structured file with chosen delimiter
					df = CSV.read(filepath, DataFrame; delim = delim)
			catch e
				#   If CSV.read itself fails, fall back to line-based loader
					if verbose
						println("Failed to read partition as CSV/TSV ($(typeof(e))): falling back to text mode")
					end

					lines = readlines(filepath)
					vals = String[strip(line) for line in lines if !isempty(strip(line))]
					result = DataFrame(community = vals)

				#   Coerce to Int if possible
					result.community = _coerce_to_int(result.community, "community")

					if verbose
						println("Loaded $(length(vals)) community assignments from text file")
					end

					return result
			end

		#   Normalize column names for pattern matching
			original_names = names(df)
			lower_names    = lowercase.(string.(original_names))

		#   Locate ID-like column:
		#   - exact "id"
		#   - or contains both "node" and "id" (e.g., "Node ID", "node_id", "node-id")
			id_indices = Int[]
			for (i, cname) in enumerate(lower_names)
				if cname == "id"
					push!(id_indices, i)
				elseif occursin("node", cname) && occursin("id", cname)
					push!(id_indices, i)
				end
			end

			id_idx = nothing
			if !isempty(id_indices)
				id_idx = first(id_indices)
				if length(id_indices) > 1 && verbose
					println("WARNING: Multiple ID-like columns detected in partition; using column '$(original_names[id_idx])'")
				end
			end

		#   Locate community-like column:
		#   - exact "community" or "node community"
		#   - OR any column whose name contains "leiden" (e.g., "leiden group")
			comm_indices = Int[]

		#   First pass: exact community names
			for (i, cname) in enumerate(lower_names)
				if cname == "community" || cname == "node community"
					push!(comm_indices, i)
				end
			end

		#   Second pass: leiden-based names, if nothing found yet
			if isempty(comm_indices)
				for (i, cname) in enumerate(lower_names)
					if occursin("leiden", cname)
						push!(comm_indices, i)
					end
				end
			end

			comm_idx = nothing
			if !isempty(comm_indices)
				comm_idx = first(comm_indices)
				if length(comm_indices) > 1 && verbose
					println("WARNING: Multiple community-like columns detected in partition; using column '$(original_names[comm_idx])'")
				end
			end

		#   Main cases
			result = DataFrame()
			if id_idx !== nothing && comm_idx !== nothing
				#   Standard case: both ID-like and community-like columns present
					id_col   = original_names[id_idx]
					comm_col = original_names[comm_idx]

					if verbose
						println("Using ID column: '$id_col'")
						println("Using community column: '$comm_col'")
					end

					ids      = strip.(string.(df[!, id_col]))
					raw_comm = df[!, comm_col]
					communities = _coerce_to_int(raw_comm, string(comm_col))

					result = DataFrame(id = ids, community = communities)

			elseif ncol(df) == 1
				#   Single column partition vector; must align with node_list
					if node_list === nothing
						msg = """
						Single-column partition file detected, but no node list was provided.
						When using a single community column, the partition is assumed to be
						aligned with node_list.id, so a node list is required.
						"""
						throw(ArgumentError(msg))
					end

					raw_comm = df[!, 1]
					communities = _coerce_to_int(raw_comm, string(original_names[1]))

					if !hasproperty(node_list, :id)
						msg = """
						Provided node_list does not contain an :id column.
						Expected node_list.id to align with the partition vector.
						"""
						throw(ArgumentError(msg))
					end

					ids = strip.(string.(node_list.id))

					if length(ids) != length(communities)
						msg = """
						Length mismatch between node_list.id and partition vector.
						node_list.id length = $(length(ids)), partition length = $(length(communities)).
						They must be equal when using a single-column partition file.
						"""
						throw(ArgumentError(msg))
					end

					if verbose
						println("WARNING: Single-column partition detected – assuming row order matches node_list.id")
					end

					result = DataFrame(id = ids, community = communities)

			else
				#   No valid ID/community combination found
					msg = """
					Partition file must contain:
					- an ID-like column (e.g., 'id', 'Node ID', 'node_id', 'node-id'), and
					- a community-like column (e.g., 'community', 'node community',
					or any column name containing 'leiden'),
					or it must be a single-column file of community assignments aligned
					with node_list.id.
					Found columns: $(join(string.(original_names), ", "))
					"""
					throw(ArgumentError(msg))
			end

		#   Diagnostics
			if verbose
				n_nodes = nrow(result)
				n_comms = length(unique(result.community))
				println("Loaded partition with $n_nodes nodes in $n_comms communities")
			end

		#   Return partition
			rename!(result, ["node", "community"])
			result.node = string.(result.node)
			return result
	end

#	Helper Function for CLI: Process ORA Network Input
	function _process_ora_input(metanetwork_path::String, network_name::String, 
								ora_leiden::Union{Nothing,String}; verbose::Bool = true)
		"""
		Args:
			metanetwork_path::String: Path to ORA XML file
			network_name::String: Name of network to extract
			ora_leiden::Union{Nothing,String}: ORA community attribute name
			verbose::Bool: Print diagnostic messages
		Returns:
			NamedTuple: (edges, nodes, partition)
		Notes:
			Uses existing load_ora_xml function
			Lists available networks if requested network not found
			Ensures partition.community is Vector{Int} before returning
			Partition schema: DataFrame(node = nodes.id, community = ::Vector{Int})
		"""
		#	Load ORA XML
			if verbose
				println("Loading ORA metanetwork from: $metanetwork_path")
			end
		
			metanet = load_ora_xml(metanetwork_path)
		
		#	Check if network exists
			available_networks = keys(metanet.networks)
			
			if !(network_name in available_networks)
				error_msg = "Network '$network_name' not found in ORA file.\nAvailable networks:\n"
				for net in sort(collect(available_networks))
					error_msg *= "  - $net\n"
				end
				throw(ArgumentError(error_msg))
			end
		
		#	Extract network edges
			network = metanet.networks[network_name]
			edges = network.edges
		
		#	Determine node type from network name
			node_type = split(network_name, " x ")[1]
			
			if !(node_type in keys(metanet.nodesets))
				throw(ArgumentError("Node type '$node_type' not found in ORA file"))
			end
		
		#	Extract nodes
			agent_nodes = metanet.nodesets[node_type]
			nodes = agent_nodes[:, 1:2]
			rename!(nodes, ["id", "label"])
			if sum(isempty.(nodes.label)) == nrow(nodes)
				nodes.label = nodes.id
			end
		
		#	Extract partition if specified
			partition = nothing
			
			if ora_leiden !== nothing
				#	Checking if Column Name Matches Attributes
					if !(ora_leiden in names(agent_nodes))
						available_attrs = names(agent_nodes)[3:end]  # Skip ID and label
						error_msg = "Attribute '$ora_leiden' not found.\nAvailable attributes:\n"
						for attr in available_attrs
							error_msg *= "  - $attr\n"
						end
						throw(ArgumentError(error_msg))
					end
				
				#	Raw column for the community membership
					raw_col = agent_nodes[!, ora_leiden]
					col_type = eltype(raw_col)
				
				#	Build integer community vector
					comm = Vector[]
					
					if col_type <: Integer
						#	Already integer-like; just normalize to Int
							comm = Int.(raw_col)
					
					elseif col_type <: AbstractFloat
						#	Floating but numeric; round to nearest Int
							comm = round.(Int, raw_col)
					
					elseif col_type <: AbstractString
						#	First try to parse as plain integers (e.g., "1", "2", "3")
							parsed = tryparse.(Int, raw_col)
						
							if all(!isnothing, parsed)
								#	All entries parse cleanly to Int
									comm = Int.(something.(parsed))
							else
								#	General string labels: map each unique label to an Int code
									unique_labels = unique(raw_col)
									label_to_int = Dict{eltype(raw_col),Int}()
									
									for (i, lbl) in enumerate(unique_labels)
										label_to_int[lbl] = i
									end
									
									comm = [label_to_int[x] for x in raw_col]
							end
					
					else
						throw(ArgumentError("Unsupported type $(col_type) for ORA Leiden attribute '$ora_leiden'"))
					end
				
				#	Assemble partition DataFrame with integer communities
				#	NOTE: node column is aligned with nodes.id (not raw "Node ID" column name)
					partition = DataFrame(
						node = nodes.id,
						community = comm
					)
				
				if verbose
					n_comms = length(unique(partition.community))
					println("Extracted ORA partition '$ora_leiden' with $n_comms communities")
				end
			end
		
		#	Verbose summary of extracted network
			if verbose
				n_edges = nrow(edges)
				n_nodes = nrow(nodes)
				println("Extracted network '$network_name': $n_edges edges, $n_nodes nodes")
			end
		
		#	Return components
			return (edges = edges, nodes = nodes, partition = partition)
	end

#	Helper Function for CLI: Write Analysis Results
	function _write_analysis_results(global_stats::DataFrame, triad_census::DataFrame,
									  node_measures::DataFrame, feature_vector::DataFrame,
									  output_dir::String, prefix::String; verbose::Bool = true)
		"""
		Args:
			global_stats::DataFrame: Global network statistics
			triad_census::DataFrame: Triad census counts
			node_measures::DataFrame: Node-level measures
			feature_vector::DataFrame: Combined feature vector
			output_dir::String: Output directory path
			prefix::String: File name prefix
			verbose::Bool: Print diagnostic messages
		Returns:
			Nothing
		Notes:
			Creates output files with specified prefix
		"""
		
		#	Create output directory if needed
			mkpath(output_dir)
		
		#	Write global stats
			global_path = joinpath(output_dir, "$(prefix)_global_stats.csv")
			CSV.write(global_path, global_stats)
			if verbose
				println("Wrote global statistics to: $global_path")
			end
		
		#	Write triad census
			triad_path = joinpath(output_dir, "$(prefix)_triad_census.csv")
			CSV.write(triad_path, triad_census)
			if verbose
				println("Wrote triad census to: $triad_path")
			end
		
		#	Write node measures
			node_path = joinpath(output_dir, "$(prefix)_node_measures.csv")
			CSV.write(node_path, node_measures)
			if verbose
				println("Wrote node measures to: $node_path")
			end
		
		#	Write feature vector
			feature_path = joinpath(output_dir, "$(prefix)_feature_vector.csv")
			CSV.write(feature_path, feature_vector)
			if verbose
				println("Wrote feature vector to: $feature_path")
			end
		
		#	Return nothing
			return nothing
	end

#	Helper Function for CLI: Write Comparison Results
	function _write_comparison_results(comparison_result::NamedTuple, 
									   output_dir::String;
									   name_1::String = "network_1",
									   name_2::String = "network_2",
									   verbose::Bool = true)
		"""
		Args:
			comparison_result::NamedTuple: Output from network_comparator
			output_dir::String: Output directory path
			name_1::String: Name prefix for first network (default = "network_1")
			name_2::String: Name prefix for second network (default = "network_2")
			verbose::Bool: Print diagnostic messages
		Returns:
			Nothing
		Notes:
			Creates comparison tables and similarity scores with filenames
			that encode both network names, e.g.:
				name_1_name_2_comparison_raw.csv
		"""
		
		#	Create output directory if needed
			mkpath(output_dir)

		#	Build pair prefix for filenames
			pair_prefix = "$(name_1)_$(name_2)"
		
		#	Split features for raw and asinh comparisons
			is_raw_js = occursin.(r"^jsd_raw_", comparison_result.combined_features.measure)
			is_asinh_js = occursin.(r"^jsd_asinh_", comparison_result.combined_features.measure)
			is_non_js = .!(is_raw_js .| is_asinh_js)
		
		#	Create raw comparison table
			raw_features = comparison_result.combined_features[is_non_js .| is_raw_js, :]
			raw_path = joinpath(output_dir, "$(pair_prefix)_comparison_raw.csv")
			CSV.write(raw_path, raw_features)
			if verbose
				println("Wrote raw comparison to: $raw_path")
			end
		
		#	Create asinh comparison table
			asinh_features = comparison_result.combined_features[is_non_js .| is_asinh_js, :]
			asinh_path = joinpath(output_dir, "$(pair_prefix)_comparison_asinh.csv")
			CSV.write(asinh_path, asinh_features)
			if verbose
				println("Wrote asinh comparison to: $asinh_path")
			end
		
		#	Create similarity scores table
			scores = DataFrame(
				metric = ["overall_distance_raw",
						  "overall_similarity_raw",
						  "overall_distance_asinh",
						  "overall_similarity_asinh"],
				value = [comparison_result.overall_distance_raw,
						 comparison_result.overall_similarity_raw,
						 comparison_result.overall_distance_asinh,
						 comparison_result.overall_similarity_asinh]
			)
			scores_path = joinpath(output_dir, "$(pair_prefix)_similarity_scores.csv")
			CSV.write(scores_path, scores)
			if verbose
				println("Wrote similarity scores to: $scores_path")
			end
		
		#	Write type contributions
			contrib_raw_path = joinpath(output_dir, "$(pair_prefix)_type_contributions_raw.csv")
			CSV.write(contrib_raw_path, comparison_result.type_contributions_raw)
			
			contrib_asinh_path = joinpath(output_dir, "$(pair_prefix)_type_contributions_asinh.csv")
			CSV.write(contrib_asinh_path, comparison_result.type_contributions_asinh)
			
			if verbose
				println("Wrote type contributions to: $contrib_raw_path and $contrib_asinh_path")
			end
		
		#	Return nothing
			return nothing
	end

#	Helper Function for CLI: List Available Functions
	function _list_functions()
		"""
		Print list of available user-facing functions
		"""
		
		functions = [
			"adjusted_rand_index" => "Compare two partitions using Adjusted Rand Index",
			"load_ora_xml" => "Load ORA XML metanetwork file",
			"in_degree" => "Calculate in-degree for directed networks",
			"out_degree" => "Calculate out-degree for directed networks",
			"total_degree" => "Calculate total degree",
			"degree_ratio" => "Calculate degree ratios for nodes",
			"freeman_degree_normalization" => "Apply Freeman degree normalization",
			"local_clustering_coefficient" => "Calculate local clustering coefficients",
			"global_clustering_coefficient" => "Calculate global clustering coefficient",
			"weighted_clustering_coefficient" => "Calculate weighted clustering",
			"directed_clustering_cg" => "Directed clustering (Clemente-Grassi)",
			"local_weighted_reciprocity" => "Calculate local weighted reciprocity",
			"pagerank_local_ora" => "PageRank with ORA-style implementation",
			"pagerank_stitched" => "PageRank for disconnected components",
			"salsa_centrality" => "SALSA hub/authority scores",
			"calculate_modularity" => "Calculate network modularity",
			"leiden_community_detection" => "Detect communities using Leiden algorithm",
			"champ_community_detection" => "Multi-resolution community detection",
			"modularity_vitality" => "Node importance via modularity change",
			"core_decomposition" => "K-core and S-core decomposition",
			"hop_reach_k" => "K-hop reachability analysis",
			"group_statistics" => "Statistics for node groups",
			"recommend_L" => "Recommend L parameter for centrality",
			"triad_census" => "Count triad patterns",
			"component_statistics" => "Analyze connected components",
			"link_statistics" => "Edge weight statistics",
			"reciprocity" => "Calculate network reciprocity",
			"assortativity_degree" => "Degree assortativity coefficient",
			"network_comparator" => "Compare two networks comprehensively"
		]
		
		println("\nAvailable Functions:")
		println("=" ^ 80)
		
		for (func, desc) in functions
			println("  $func")
			println("    $desc")
			println()
		end
		
		println("Use --help <function_name> to see detailed documentation")
	end

#	Helper Function for CLI: Show Function Help
	function _show_function_help(function_name::String)
		"""
		Display docstring for specified function
		"""
		
		#	Map string to function object from parent module
			func_map = Dict(
				"adjusted_rand_index" => Large_Graph_Similarity.adjusted_rand_index,
				"load_ora_xml" => Large_Graph_Similarity.load_ora_xml,
				"in_degree" => Large_Graph_Similarity.in_degree,
				"out_degree" => Large_Graph_Similarity.out_degree,
				"total_degree" => Large_Graph_Similarity.total_degree,
				"degree_ratio" => Large_Graph_Similarity.degree_ratio,
				"freeman_degree_normalization" => Large_Graph_Similarity.freeman_degree_normalization,
				"local_clustering_coefficient" => Large_Graph_Similarity.local_clustering_coefficient,
				"global_clustering_coefficient" => Large_Graph_Similarity.global_clustering_coefficient,
				"weighted_clustering_coefficient" => Large_Graph_Similarity.weighted_clustering_coefficient,
				"directed_clustering_cg" => Large_Graph_Similarity.directed_clustering_cg,
				"local_weighted_reciprocity" => Large_Graph_Similarity.local_weighted_reciprocity,
				"pagerank_local_ora" => Large_Graph_Similarity.pagerank_local_ora,
				"pagerank_stitched" => Large_Graph_Similarity.pagerank_stitched,
				"salsa_centrality" => Large_Graph_Similarity.salsa_centrality,
				"calculate_modularity" => Large_Graph_Similarity.calculate_modularity,
				"leiden_community_detection" => Large_Graph_Similarity.leiden_community_detection,
				"champ_community_detection" => Large_Graph_Similarity.champ_community_detection,
				"modularity_vitality" => Large_Graph_Similarity.modularity_vitality,
				"core_decomposition" => Large_Graph_Similarity.core_decomposition,
				"hop_reach_k" => Large_Graph_Similarity.hop_reach_k,
				"group_statistics" => Large_Graph_Similarity.group_statistics,
				"recommend_L" => Large_Graph_Similarity.recommend_L,
				"triad_census" => Large_Graph_Similarity.triad_census,
				"component_statistics" => Large_Graph_Similarity.component_statistics,
				"link_statistics" => Large_Graph_Similarity.link_statistics,
				"reciprocity" => Large_Graph_Similarity.reciprocity,
				"assortativity_degree" => Large_Graph_Similarity.assortativity_degree,
				"network_comparator" => Large_Graph_Similarity.network_comparator
			)
		
			if !haskey(func_map, function_name)
				println("Function '$function_name' not found. Use --list-functions to see available functions.")
				return
			end
		
		#	Get the function
			func = func_map[function_name]
		
		#	Try to get and display docstring
			try
				# Method 1: Try using Docs.doc
				doc_obj = Docs.doc(func)
				doc_str = string(doc_obj)
				
				if !isempty(doc_str) && doc_str != "No documentation found."
					println("\nDocumentation for $function_name:")
					println("="^80)
					println(doc_str)
					println("="^80)
				else
					# Method 2: Try direct evaluation of @doc
					doc_expr = :(@doc $func)
					doc_result = eval(doc_expr)
					println("\nDocumentation for $function_name:")
					println("="^80)
					println(doc_result)
					println("="^80)
				end
			catch e
				# Fallback: Show basic function info
				println("\nFunction: $function_name")
				println("="^80)
				println("Function object: $func")
				methods_list = methods(func)
				println("\nAvailable methods:")
				for m in methods_list
					println("  ", m)
				end
				println("\nNote: Could not retrieve full documentation. Error: $e")
				println("="^80)
			end
	end	

#	Main CLI Entry Point
	function cli_main(args::Vector{String})
		"""
		Main entry point for CLI

		Args:
			args::Vector{String}: Command-line arguments (excluding julia and script name)
		Returns:
			Nothing
		Notes:
			Handles:
				- --list-functions (prints function catalog and exits)
				- --function-help <name> (prints docstring for a function and exits)
				- analysis / comparison modes for ORA or CSV inputs
		"""
		#	Parse arguments
			s = ArgParseSettings(
				prog = "Large Graph Similarity",
				description = "Network analysis and comparison tool",
				version = "1.0.0",
				add_version = true,
				add_help = true  # Let ArgParse handle --help
			)

			@add_arg_table s begin
				"--mode"
					help = "Operation mode: analysis or comparison"
					arg_type = String
					default = "analysis"
				
				"--ora-xml-1"
					help = "Path to first ORA metanetwork XML"
					arg_type = String
				
				"--ora-xml-2"
					help = "Path to second ORA metanetwork XML"
					arg_type = String
					
				"--network-name-1"
					help = "Network name to extract from first ORA file"
					arg_type = String
					
				"--network-name-2"
					help = "Network name to extract from second ORA file"
					arg_type = String
					
				"--ora-leiden"
					help = "ORA community attribute name"
					arg_type = String
					
				"--edgelist-1"
					help = "Path to first edge list CSV/TSV"
					arg_type = String
					
				"--edgelist-2"
					help = "Path to second edge list CSV/TSV"
					arg_type = String
					
				"--nodelist-1"
					help = "Path to first node list"
					arg_type = String
					
				"--nodelist-2"
					help = "Path to second node list"
					arg_type = String
					
				"--partition-1"
					help = "Path to first partition file"
					arg_type = String
					
				"--partition-2"
					help = "Path to second partition file"
					arg_type = String
					
				"--output-dir"
					help = "Output directory for results"
					arg_type = String
					default = "./output"
					
				"--name-1"
					help = "Name prefix for first network outputs"
					arg_type = String
					default = "network_1"
					
				"--name-2"
					help = "Name prefix for second network outputs"
					arg_type = String
					default = "network_2"
					
				"--directed"
					help = "Treat networks as directed"
					arg_type = Bool
					default = true
					
				"--weighted"
					help = "Use edge weights if present"
					arg_type = Bool
					default = true
					
				"--resolution"
					help = "Resolution parameter for community detection"
					arg_type = Float64
					default = 1.0
					
				"--resolution-sweep"
					help = "Use CHAMP multi-resolution community detection"
					arg_type = Bool
					default = false
					
				"--n-resolutions"
					help = "Number of resolutions for CHAMP"
					arg_type = Int
					default = 15
					
				"--n-runs"
					help = "Number of Leiden runs per resolution"
					arg_type = Int
					default = 5
					
				"--n-iterations"
					help = "Max iterations per Leiden run"
					arg_type = Int
					default = 10
					
				"--seed"
					help = "Random seed for reproducibility"
					arg_type = Int
					
				"--verbose"
					help = "Print diagnostic messages"
					arg_type = Bool
					default = true
					
				"--list-functions"
					help = "List available functions"
					action = :store_true
					
				"--function-help"
					help = "Show help for specific function"
					arg_type = String
			end

			parsed_args = parse_args(args, s)

		#	Handle special commands (meta modes that exit early)
			if parsed_args["list-functions"]
				_list_functions()
				return
			end

			if parsed_args["function-help"] !== nothing
				_show_function_help(parsed_args["function-help"])
				return
			end

		#	Extract common parameters
			verbose    = parsed_args["verbose"]
			output_dir = parsed_args["output-dir"]
			mode       = lowercase(parsed_args["mode"])

		#	Load first network
			edges_1     = nothing
			nodes_1     = nothing
			partition_1 = nothing
			
			if parsed_args["ora-xml-1"] !== nothing
				#	Load from ORA
					if parsed_args["network-name-1"] === nothing
						throw(ArgumentError("--network-name-1 required when using --ora-xml-1"))
					end
				
					ora_data = _process_ora_input(
						parsed_args["ora-xml-1"],
						parsed_args["network-name-1"],
						parsed_args["ora-leiden"];
						verbose = verbose
					)
					
					edges_1     = ora_data.edges
					nodes_1     = ora_data.nodes
					partition_1 = ora_data.partition
				
			elseif parsed_args["edgelist-1"] !== nothing
				#	Load from CSV/TSV edge list
					edges_1 = _load_edge_list(parsed_args["edgelist-1"]; verbose = verbose)
				
				#	Node list: use provided nodelist if present; else construct from edges
					if parsed_args["nodelist-1"] !== nothing
						nodes_1 = _load_node_list(parsed_args["nodelist-1"]; verbose = verbose)
					else
						#	Construct canonical node list from edge endpoints
							all_ids = union(edges_1.src, edges_1.dst)
							all_ids = string.(all_ids)
							
							if verbose
								println("No node list provided for network 1; constructing node list from edge endpoints")
								println("Constructed node list with $(length(all_ids)) unique nodes")
							end
							
							nodes_1 = DataFrame(
								id    = all_ids,
								label = all_ids
							)
					end
				
				#	Partition (if provided) uses the node list for ID mapping / validation
					if parsed_args["partition-1"] !== nothing
						partition_1 = _load_partition(parsed_args["partition-1"], node_list = nodes_1;
													 verbose = verbose)
					end
			else
				throw(ArgumentError("Must provide either --ora-xml-1 or --edgelist-1"))
			end
		
		#	Determine if comparison mode
			if mode == "comparison" || parsed_args["ora-xml-2"] !== nothing || 
			   parsed_args["edgelist-2"] !== nothing
				mode = "comparison"
			end
		
		#	Load second network if comparison mode
			edges_2     = nothing
			nodes_2     = nothing
			partition_2 = nothing
			
			if mode == "comparison"
				if parsed_args["ora-xml-2"] !== nothing
					#	Load from ORA
						if parsed_args["network-name-2"] === nothing
							throw(ArgumentError("--network-name-2 required when using --ora-xml-2"))
						end
					
						ora_data = _process_ora_input(
							parsed_args["ora-xml-2"],
							parsed_args["network-name-2"],
							parsed_args["ora-leiden"];
							verbose = verbose
						)
						
						edges_2     = ora_data.edges
						nodes_2     = ora_data.nodes
						partition_2 = ora_data.partition
					
				elseif parsed_args["edgelist-2"] !== nothing
					#	Load from CSV/TSV edge list
						edges_2 = _load_edge_list(parsed_args["edgelist-2"]; verbose = verbose)
					
					#	Node list: use provided nodelist if present; else construct from edges
						if parsed_args["nodelist-2"] !== nothing
							nodes_2 = _load_node_list(parsed_args["nodelist-2"]; verbose = verbose)
						else
							#	Construct canonical node list from edge endpoints
								all_ids_2 = union(edges_2.src, edges_2.dst)
								all_ids_2 = string.(all_ids_2)
								
								if verbose
									println("No node list provided for network 2; constructing node list from edge endpoints")
									println("Constructed node list with $(length(all_ids_2)) unique nodes")
								end
								
								nodes_2 = DataFrame(
									id    = all_ids_2,
									label = all_ids_2
								)
						end
					
					#	Partition (if provided) uses the node list for ID mapping / validation
						if parsed_args["partition-2"] !== nothing
							partition_2 = _load_partition(parsed_args["partition-2"], node_list = nodes_2;
														 verbose = verbose)
						end
				else
					throw(ArgumentError("Comparison mode requires second network"))
				end
			end
		
		#	Execute analysis or comparison
			if mode == "analysis"
				#	Announcing Analysis Mode
					if verbose
						println("\n=== Running Network Analysis ===")
					end
				
				#	Generating Data based on Network Type
					if !parsed_args["directed"] && !parsed_args["weighted"]
						#	Undirected / unweighted
							global_stats, triad_census_counts, node_measures = undirected_binary_constructor(
								edges_1, nodes_1;
								resolution_sweep     = parsed_args["resolution-sweep"],
								resolution           = parsed_args["resolution"],
								directed             = parsed_args["directed"],
								weighted             = parsed_args["weighted"],
								n_resolutions        = parsed_args["n-resolutions"],
								n_runs_per_gamma     = parsed_args["n-runs"],
								n_iterations_per_run = parsed_args["n-iterations"],
								seed                 = parsed_args["seed"],
								provided_membership  = partition_1
							)
							
							feature_vector = symmetric_binary_feature_builder(
								global_stats, triad_census_counts, node_measures
							)
					elseif !parsed_args["directed"] && parsed_args["weighted"]
						#	Undirected / weighted
							global_stats, triad_census_counts, node_measures = undirected_weighted_constructor(
								edges_1, nodes_1;
								resolution_sweep     = parsed_args["resolution-sweep"],
								resolution           = parsed_args["resolution"],
								directed             = parsed_args["directed"],
								weighted             = parsed_args["weighted"],
								n_resolutions        = parsed_args["n-resolutions"],
								n_runs_per_gamma     = parsed_args["n-runs"],
								n_iterations_per_run = parsed_args["n-iterations"],
								seed                 = parsed_args["seed"],
								provided_membership  = partition_1
							)
								
							feature_vector = symmetric_weighted_feature_builder(
								global_stats, triad_census_counts, node_measures
							)
					elseif parsed_args["directed"] && !parsed_args["weighted"]
						#	Directed / unweighted
							global_stats, triad_census_counts, node_measures = directed_binary_constructor(
								edges_1, nodes_1;
								resolution_sweep     = parsed_args["resolution-sweep"],
								resolution           = parsed_args["resolution"],
								directed             = parsed_args["directed"],
								weighted             = parsed_args["weighted"],
								n_resolutions        = parsed_args["n-resolutions"],
								n_runs_per_gamma     = parsed_args["n-runs"],
								n_iterations_per_run = parsed_args["n-iterations"],
								seed                 = parsed_args["seed"],
								provided_membership  = partition_1
							)
								
							feature_vector = directed_binary_feature_builder(
								global_stats, triad_census_counts, node_measures
							)
					else
						#	Directed / weighted
							global_stats, triad_census_counts, node_measures = directed_weighted_constructor(
								edges_1, nodes_1;
								resolution_sweep     = parsed_args["resolution-sweep"],
								resolution           = parsed_args["resolution"],
								directed             = parsed_args["directed"],
								weighted             = parsed_args["weighted"],
								n_resolutions        = parsed_args["n-resolutions"],
								n_runs_per_gamma     = parsed_args["n-runs"],
								n_iterations_per_run = parsed_args["n-iterations"],
								seed                 = parsed_args["seed"],
								provided_membership  = partition_1
							)
								
							feature_vector = directed_weighted_feature_builder(
								global_stats, triad_census_counts, node_measures
							)
					end
					
				#	Write results
					_write_analysis_results(
						global_stats, triad_census_counts, node_measures, feature_vector,
						output_dir, parsed_args["name-1"];
						verbose = verbose
					)
			else  # comparison mode
				#	Announcing Comparator Mode
					if verbose
						println("\n=== Running Network Comparison ===")
					end
				
				#	Run comparator (constructors + feature building handled internally)
					result = network_comparator(
						edges_1, nodes_1, edges_2, nodes_2;
						resolution_sweep      = parsed_args["resolution-sweep"],
						resolution            = parsed_args["resolution"],
						directed              = parsed_args["directed"],
						weighted              = parsed_args["weighted"],
						n_resolutions         = parsed_args["n-resolutions"],
						n_runs_per_gamma      = parsed_args["n-runs"],
						n_iterations_per_run  = parsed_args["n-iterations"],
						seed                  = parsed_args["seed"],
						provided_membership_1 = partition_1,
						provided_membership_2 = partition_2
					)

				#	Ensure output directory exists for per-network tables
					mkpath(output_dir)

				#	Write individual network analyses (no per-network feature builders here)
					name_1 = parsed_args["name-1"]
					name_2 = parsed_args["name-2"]

					global_path_1 = joinpath(output_dir, "$(name_1)_global_stats.csv")
					triad_path_1  = joinpath(output_dir, "$(name_1)_triad_census.csv")
					node_path_1   = joinpath(output_dir, "$(name_1)_node_measures.csv")

					CSV.write(global_path_1, result.global_stats_1)
					CSV.write(triad_path_1,  result.triad_census_counts_1)
					CSV.write(node_path_1,   result.node_measures_1)

					if verbose
						println("Wrote global statistics to: $global_path_1")
						println("Wrote triad census to: $triad_path_1")
						println("Wrote node measures to: $node_path_1")
					end

					global_path_2 = joinpath(output_dir, "$(name_2)_global_stats.csv")
					triad_path_2  = joinpath(output_dir, "$(name_2)_triad_census.csv")
					node_path_2   = joinpath(output_dir, "$(name_2)_node_measures.csv")

					CSV.write(global_path_2, result.global_stats_2)
					CSV.write(triad_path_2,  result.triad_census_counts_2)
					CSV.write(node_path_2,   result.node_measures_2)

					if verbose
						println("Wrote global statistics to: $global_path_2")
						println("Wrote triad census to: $triad_path_2")
						println("Wrote node measures to: $node_path_2")
					end
				
				#	Write comparison results (uses result.combined_features and similarity scores)
					_write_comparison_results(
						result, output_dir;
						name_1 = parsed_args["name-1"],
						name_2 = parsed_args["name-2"],
						verbose = verbose
					)
			end
		
			if verbose
				println("\nAnalysis complete. Results saved to: $output_dir")
			end
	end

end # module CLI