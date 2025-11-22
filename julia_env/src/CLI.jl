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
			verbose::Bool: Print diagnostic messages
		Returns:
			DataFrame: Edge list with :src, :dst, optional :weight columns
		Notes:
			Accepts variants: source/target, from/to
			Auto-detects CSV vs TSV format
		"""
		
		#	Check file exists
			if !isfile(filepath)
				throw(ArgumentError("Edge list file not found: $filepath"))
			end
		
		#	Detect delimiter
			first_line = readline(filepath)
			delimiter = occursin("\t", first_line) ? '\t' : ','
			
			if verbose
				println("Loading edge list from: $filepath")
				println("Detected delimiter: $(delimiter == '\t' ? "TAB" : "COMMA")")
			end
		
		#	Load DataFrame
			df = CSV.read(filepath, DataFrame; delim=delimiter)
		
		#	Standardize column names
			col_names = lowercase.(string.(names(df)))
			
			if "src" in col_names && "dst" in col_names
				#	Already correct
					rename!(df, Dict(names(df)[i] => Symbol(col) 
									for (i, col) in enumerate(col_names)))
			elseif "source" in col_names && "target" in col_names
				#	Transform source/target
					idx_map = Dict("source" => findfirst(x -> x == "source", col_names),
								  "target" => findfirst(x -> x == "target", col_names))
					rename!(df, names(df)[idx_map["source"]] => :src,
							   names(df)[idx_map["target"]] => :dst)
			elseif "from" in col_names && "to" in col_names
				#	Transform from/to
					idx_map = Dict("from" => findfirst(x -> x == "from", col_names),
								  "to" => findfirst(x -> x == "to", col_names))
					rename!(df, names(df)[idx_map["from"]] => :src,
							   names(df)[idx_map["to"]] => :dst)
			else
				throw(ArgumentError("Edge list must contain (src,dst), (source,target), or (from,to) columns. Found: $(join(names(df), ", "))"))
			end
		
		#	Handle weight column
			if "weight" in lowercase.(string.(names(df)))
				weight_idx = findfirst(col -> lowercase(string(col)) == "weight", names(df))
				rename!(df, names(df)[weight_idx] => :weight)
				df.weight = Float64.(df.weight)
			elseif verbose
				println("No weight column found - assuming unit weights")
			end
		
		#	Ensure string node IDs
			df.src = string.(df.src)
			df.dst = string.(df.dst)
		
		#	Select relevant columns
			result = select(df, :src, :dst)
			if hasproperty(df, :weight)
				result.weight = df.weight
			end
		
			if verbose
				n_edges = nrow(result)
				n_nodes = length(union(result.src, result.dst))
				println("Loaded $n_edges edges connecting $n_nodes unique nodes")
			end
		
		#	Return edge list
			return result
	end

#	Helper Function for CLI: Load Node List
	function _load_node_list(filepath::String; verbose::Bool = true)
		"""
		Args:
			filepath::String: Path to node list file
			verbose::Bool: Print diagnostic messages
		Returns:
			DataFrame: Node list with :id and :label columns
		Notes:
			Accepts CSV with id/label columns or text file with one node per line
		"""
		
		#	Check file exists
			if !isfile(filepath)
				throw(ArgumentError("Node list file not found: $filepath"))
			end
		
			if verbose
				println("Loading node list from: $filepath")
			end
		
		#	Try loading as CSV first
			try
				df = CSV.read(filepath, DataFrame)
				
				#	Check for id/label columns
					col_names = lowercase.(string.(names(df)))
					
					if "id" in col_names && "label" in col_names
						id_idx = findfirst(x -> x == "id", col_names)
						label_idx = findfirst(x -> x == "label", col_names)
						result = DataFrame(
							id = string.(df[!, names(df)[id_idx]]),
							label = string.(df[!, names(df)[label_idx]])
						)
					elseif "id" in col_names
						id_idx = findfirst(x -> x == "id", col_names)
						ids = string.(df[!, names(df)[id_idx]])
						result = DataFrame(id = ids, label = ids)
					elseif ncol(df) == 1
						#	Single column treated as node IDs
							ids = string.(df[!, 1])
							result = DataFrame(id = ids, label = ids)
					else
						throw(ArgumentError("Node list must contain 'id' column or be single column. Found: $(join(names(df), ", "))"))
					end
				
				if verbose
					println("Loaded $(nrow(result)) nodes from CSV")
				end
				
				return result
				
			catch e
				#	Try as plain text file
					if isa(e, ArgumentError)
						rethrow(e)
					end
				
					lines = readlines(filepath)
					ids = String[strip(line) for line in lines if !isempty(strip(line))]
					result = DataFrame(id = ids, label = ids)
				
					if verbose
						println("Loaded $(length(ids)) nodes from text file")
					end
				
					return result
			end
	end

#	Helper Function for CLI: Load Partition
	function _load_partition(filepath::String, node_list::Union{Nothing,DataFrame}; 
							 verbose::Bool = true)
		"""
		Args:
			filepath::String: Path to partition file
			node_list::Union{Nothing,DataFrame}: Reference node list for validation
			verbose::Bool: Print diagnostic messages
		Returns:
			DataFrame: Partition with :id and :community columns
		Notes:
			Accepts CSV with id/(community|node community) columns
			Warns if integer vector assumed to match node list order
		"""
		
		#	Check file exists
			if !isfile(filepath)
				throw(ArgumentError("Partition file not found: $filepath"))
			end
		
			if verbose
				println("Loading partition from: $filepath")
			end
		
		#	Load DataFrame
			df = CSV.read(filepath, DataFrame)
			col_names = lowercase.(string.(names(df)))
		
		#	Check for expected columns
			has_id = "id" in col_names
			has_community = "community" in col_names
			has_node_community = "node community" in col_names
		
			if has_id && (has_community || has_node_community)
				#	Standard format with node IDs
					id_idx = findfirst(x -> x == "id", col_names)
					
					if has_node_community
						comm_idx = findfirst(x -> x == "node community", col_names)
					else
						comm_idx = findfirst(x -> x == "community", col_names)
					end
				
					result = DataFrame(
						id = string.(df[!, names(df)[id_idx]]),
						community = Int.(df[!, names(df)[comm_idx]])
					)
				
			elseif ncol(df) == 1 && eltype(df[!, 1]) <: Number
				#	Integer vector - warn about assumption
					if verbose
						println("WARNING: Single column of integers found - assuming order matches node list")
					end
				
					if node_list === nothing
						throw(ArgumentError("Integer partition vector requires node list for ID mapping"))
					end
				
					result = DataFrame(
						id = node_list.id,
						community = Int.(df[!, 1])
					)
				
			else
				throw(ArgumentError("Partition must contain (id, community) or (id, 'node community') columns. Found: $(join(names(df), ", "))"))
			end
		
			if verbose
				n_nodes = nrow(result)
				n_comms = length(unique(result.community))
				println("Loaded partition with $n_nodes nodes in $n_comms communities")
			end
		
		#	Return partition
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
		
		#	Extract partition if specified
			partition = nothing
			
			if ora_leiden !== nothing
				if !(ora_leiden in names(agent_nodes))
					available_attrs = names(agent_nodes)[3:end]  # Skip ID and label
					error_msg = "Attribute '$ora_leiden' not found.\nAvailable attributes:\n"
					for attr in available_attrs
						error_msg *= "  - $attr\n"
					end
					throw(ArgumentError(error_msg))
				end
				
				partition = agent_nodes[:, ["Node ID", ora_leiden]]
				rename!(partition, ["id", "community"])
				partition.community = Int.(partition.community)
				
				if verbose
					n_comms = length(unique(partition.community))
					println("Extracted ORA partition '$ora_leiden' with $n_comms communities")
				end
			end
		
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
									   output_dir::String; verbose::Bool = true)
		"""
		Args:
			comparison_result::NamedTuple: Output from network_comparator
			output_dir::String: Output directory path
			verbose::Bool: Print diagnostic messages
		Returns:
			Nothing
		Notes:
			Creates comparison tables and similarity scores
		"""
		
		#	Create output directory if needed
			mkpath(output_dir)
		
		#	Split features for raw and asinh comparisons
			is_raw_js = occursin.(r"^jsd_raw_", comparison_result.combined_features.measure)
			is_asinh_js = occursin.(r"^jsd_asinh_", comparison_result.combined_features.measure)
			is_non_js = .!(is_raw_js .| is_asinh_js)
		
		#	Create raw comparison table
			raw_features = comparison_result.combined_features[is_non_js .| is_raw_js, :]
			raw_path = joinpath(output_dir, "comparison_raw.csv")
			CSV.write(raw_path, raw_features)
			if verbose
				println("Wrote raw comparison to: $raw_path")
			end
		
		#	Create asinh comparison table
			asinh_features = comparison_result.combined_features[is_non_js .| is_asinh_js, :]
			asinh_path = joinpath(output_dir, "comparison_asinh.csv")
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
			scores_path = joinpath(output_dir, "similarity_scores.csv")
			CSV.write(scores_path, scores)
			if verbose
				println("Wrote similarity scores to: $scores_path")
			end
		
		#	Write type contributions
			contrib_raw_path = joinpath(output_dir, "type_contributions_raw.csv")
			CSV.write(contrib_raw_path, comparison_result.type_contributions_raw)
			
			contrib_asinh_path = joinpath(output_dir, "type_contributions_asinh.csv")
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
			verbose = parsed_args["verbose"]
			output_dir = parsed_args["output-dir"]
			mode = lowercase(parsed_args["mode"])

		#	Load first network
			edges_1 = nothing
			nodes_1 = nothing
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
					
					edges_1 = ora_data.edges
					nodes_1 = ora_data.nodes
					partition_1 = ora_data.partition
				
			elseif parsed_args["edgelist-1"] !== nothing
				#	Load from CSV
					edges_1 = _load_edge_list(parsed_args["edgelist-1"]; verbose = verbose)
				
					if parsed_args["nodelist-1"] !== nothing
						nodes_1 = _load_node_list(parsed_args["nodelist-1"]; verbose = verbose)
					end
				
					if parsed_args["partition-1"] !== nothing
						partition_1 = _load_partition(parsed_args["partition-1"], nodes_1; 
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
			edges_2 = nothing
			nodes_2 = nothing
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
						
						edges_2 = ora_data.edges
						nodes_2 = ora_data.nodes
						partition_2 = ora_data.partition
					
				elseif parsed_args["edgelist-2"] !== nothing
					#	Load from CSV
						edges_2 = _load_edge_list(parsed_args["edgelist-2"]; verbose = verbose)
					
						if parsed_args["nodelist-2"] !== nothing
							nodes_2 = _load_node_list(parsed_args["nodelist-2"]; verbose = verbose)
						end
					
						if parsed_args["partition-2"] !== nothing
							partition_2 = _load_partition(parsed_args["partition-2"], nodes_2; 
														verbose = verbose)
						end
				else
					throw(ArgumentError("Comparison mode requires second network"))
				end
			end
		
		#	Execute analysis or comparison
			if mode == "analysis"
				if verbose
					println("\n=== Running Network Analysis ===")
				end
				
				#	Undirected / unweighted
				if !parsed_args["directed"] && !parsed_args["weighted"]
					global_stats, triad_census_counts, node_measures = undirected_binary_constructor(
						edges_1, nodes_1;
						resolution_sweep = parsed_args["resolution-sweep"],
						resolution = parsed_args["resolution"],
						directed = parsed_args["directed"],
						weighted = parsed_args["weighted"],
						n_resolutions = parsed_args["n-resolutions"],
						n_runs_per_gamma = parsed_args["n-runs"],
						n_iterations_per_run = parsed_args["n-iterations"],
						seed = parsed_args["seed"],
						provided_membership = partition_1
					)
					
					feature_vector = symmetric_binary_feature_builder(
						global_stats, triad_census_counts, node_measures
					)
				
				#	Undirected / weighted
				elseif !parsed_args["directed"] && parsed_args["weighted"]
					global_stats, triad_census_counts, node_measures = undirected_weighted_constructor(
						edges_1, nodes_1;
						resolution_sweep = parsed_args["resolution-sweep"],
						resolution = parsed_args["resolution"],
						directed = parsed_args["directed"],
						weighted = parsed_args["weighted"],
						n_resolutions = parsed_args["n-resolutions"],
						n_runs_per_gamma = parsed_args["n-runs"],
						n_iterations_per_run = parsed_args["n-iterations"],
						seed = parsed_args["seed"],
						provided_membership = partition_1
					)
					
					feature_vector = symmetric_weighted_feature_builder(
						global_stats, triad_census_counts, node_measures
					)
				
				#	Directed / unweighted
				elseif parsed_args["directed"] && !parsed_args["weighted"]
					global_stats, triad_census_counts, node_measures = directed_binary_constructor(
						edges_1, nodes_1;
						resolution_sweep = parsed_args["resolution-sweep"],
						resolution = parsed_args["resolution"],
						directed = parsed_args["directed"],
						weighted = parsed_args["weighted"],
						n_resolutions = parsed_args["n-resolutions"],
						n_runs_per_gamma = parsed_args["n-runs"],
						n_iterations_per_run = parsed_args["n-iterations"],
						seed = parsed_args["seed"],
						provided_membership = partition_1
					)
					
					feature_vector = directed_binary_feature_builder(
						global_stats, triad_census_counts, node_measures
					)
				
				#	Directed / weighted
				else
					global_stats, triad_census_counts, node_measures = directed_weighted_constructor(
						edges_1, nodes_1;
						resolution_sweep = parsed_args["resolution-sweep"],
						resolution = parsed_args["resolution"],
						directed = parsed_args["directed"],
						weighted = parsed_args["weighted"],
						n_resolutions = parsed_args["n-resolutions"],
						n_runs_per_gamma = parsed_args["n-runs"],
						n_iterations_per_run = parsed_args["n-iterations"],
						seed = parsed_args["seed"],
						provided_membership = partition_1
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
				if verbose
					println("\n=== Running Network Comparison ===")
				end
				
				#	Run comparator (constructors inside comparator handle partitions as needed)
				result = network_comparator(
					edges_1, nodes_1, edges_2, nodes_2;
					resolution_sweep = parsed_args["resolution-sweep"],
					resolution = parsed_args["resolution"],
					directed = parsed_args["directed"],
					weighted = parsed_args["weighted"],
					n_resolutions = parsed_args["n-resolutions"],
					n_runs_per_gamma = parsed_args["n-runs"],
					n_iterations_per_run = parsed_args["n-iterations"],
					seed = parsed_args["seed"],
					provided_membership_1 = partition_1,
					provided_membership_2 = partition_2
				)
				
				#	Also save individual network analyses
				
				#	Network 1
				if !parsed_args["directed"] && !parsed_args["weighted"]
					global_stats_1, triad_census_1, node_measures_1 = undirected_binary_constructor(
						edges_1, nodes_1;
						resolution_sweep = parsed_args["resolution-sweep"],
						resolution = parsed_args["resolution"],
						directed = parsed_args["directed"],
						weighted = parsed_args["weighted"],
						n_resolutions = parsed_args["n-resolutions"],
						n_runs_per_gamma = parsed_args["n-runs"],
						n_iterations_per_run = parsed_args["n-iterations"],
						seed = parsed_args["seed"],
						provided_membership = partition_1
					)
					
					feature_vector_1 = symmetric_binary_feature_builder(
						global_stats_1, triad_census_1, node_measures_1
					)
				
				elseif !parsed_args["directed"] && parsed_args["weighted"]
					global_stats_1, triad_census_1, node_measures_1 = undirected_weighted_constructor(
						edges_1, nodes_1;
						resolution_sweep = parsed_args["resolution-sweep"],
						resolution = parsed_args["resolution"],
						directed = parsed_args["directed"],
						weighted = parsed_args["weighted"],
						n_resolutions = parsed_args["n-resolutions"],
						n_runs_per_gamma = parsed_args["n-runs"],
						n_iterations_per_run = parsed_args["n-iterations"],
						seed = parsed_args["seed"],
						provided_membership = partition_1
					)
					
					feature_vector_1 = symmetric_weighted_feature_builder(
						global_stats_1, triad_census_1, node_measures_1
					)
				
				elseif parsed_args["directed"] && !parsed_args["weighted"]
					global_stats_1, triad_census_1, node_measures_1 = directed_binary_constructor(
						edges_1, nodes_1;
						resolution_sweep = parsed_args["resolution-sweep"],
						resolution = parsed_args["resolution"],
						directed = parsed_args["directed"],
						weighted = parsed_args["weighted"],
						n_resolutions = parsed_args["n-resolutions"],
						n_runs_per_gamma = parsed_args["n-runs"],
						n_iterations_per_run = parsed_args["n-iterations"],
						seed = parsed_args["seed"],
						provided_membership = partition_1
					)
					
					feature_vector_1 = directed_binary_feature_builder(
						global_stats_1, triad_census_1, node_measures_1
					)
				
				else
					global_stats_1, triad_census_1, node_measures_1 = directed_weighted_constructor(
						edges_1, nodes_1;
						resolution_sweep = parsed_args["resolution-sweep"],
						resolution = parsed_args["resolution"],
						directed = parsed_args["directed"],
						weighted = parsed_args["weighted"],
						n_resolutions = parsed_args["n-resolutions"],
						n_runs_per_gamma = parsed_args["n-runs"],
						n_iterations_per_run = parsed_args["n-iterations"],
						seed = parsed_args["seed"],
						provided_membership = partition_1
					)
					
					feature_vector_1 = directed_weighted_feature_builder(
						global_stats_1, triad_census_1, node_measures_1
					)
				end
				
				_write_analysis_results(
					global_stats_1, triad_census_1, node_measures_1, feature_vector_1,
					output_dir, parsed_args["name-1"];
					verbose = verbose
				)
				
				#	Network 2
				if !parsed_args["directed"] && !parsed_args["weighted"]
					global_stats_2, triad_census_2, node_measures_2 = undirected_binary_constructor(
						edges_2, nodes_2;
						resolution_sweep = parsed_args["resolution-sweep"],
						resolution = parsed_args["resolution"],
						directed = parsed_args["directed"],
						weighted = parsed_args["weighted"],
						n_resolutions = parsed_args["n-resolutions"],
						n_runs_per_gamma = parsed_args["n-runs"],
						n_iterations_per_run = parsed_args["n-iterations"],
						seed = parsed_args["seed"],
						provided_membership = partition_2
					)
					
					feature_vector_2 = symmetric_binary_feature_builder(
						global_stats_2, triad_census_2, node_measures_2
					)
				
				elseif !parsed_args["directed"] && parsed_args["weighted"]
					global_stats_2, triad_census_2, node_measures_2 = undirected_weighted_constructor(
						edges_2, nodes_2;
						resolution_sweep = parsed_args["resolution-sweep"],
						resolution = parsed_args["resolution"],
						directed = parsed_args["directed"],
						weighted = parsed_args["weighted"],
						n_resolutions = parsed_args["n-resolutions"],
						n_runs_per_gamma = parsed_args["n-runs"],
						n_iterations_per_run = parsed_args["n-iterations"],
						seed = parsed_args["seed"],
						provided_membership = partition_2
					)
					
					feature_vector_2 = symmetric_weighted_feature_builder(
						global_stats_2, triad_census_2, node_measures_2
					)
				
				elseif parsed_args["directed"] && !parsed_args["weighted"]
					global_stats_2, triad_census_2, node_measures_2 = directed_binary_constructor(
						edges_2, nodes_2;
						resolution_sweep = parsed_args["resolution-sweep"],
						resolution = parsed_args["resolution"],
						directed = parsed_args["directed"],
						weighted = parsed_args["weighted"],
						n_resolutions = parsed_args["n-resolutions"],
						n_runs_per_gamma = parsed_args["n-runs"],
						n_iterations_per_run = parsed_args["n-iterations"],
						seed = parsed_args["seed"],
						provided_membership = partition_2
					)
					
					feature_vector_2 = directed_binary_feature_builder(
						global_stats_2, triad_census_2, node_measures_2
					)
				
				else
					global_stats_2, triad_census_2, node_measures_2 = directed_weighted_constructor(
						edges_2, nodes_2;
						resolution_sweep = parsed_args["resolution-sweep"],
						resolution = parsed_args["resolution"],
						directed = parsed_args["directed"],
						weighted = parsed_args["weighted"],
						n_resolutions = parsed_args["n-resolutions"],
						n_runs_per_gamma = parsed_args["n-runs"],
						n_iterations_per_run = parsed_args["n-iterations"],
						seed = parsed_args["seed"],
						provided_membership = partition_2
					)
					
					feature_vector_2 = directed_weighted_feature_builder(
						global_stats_2, triad_census_2, node_measures_2
					)
				end
				
				_write_analysis_results(
					global_stats_2, triad_census_2, node_measures_2, feature_vector_2,
					output_dir, parsed_args["name-2"];
					verbose = verbose
				)
				
				#	Write comparison results
				_write_comparison_results(result, output_dir; verbose = verbose)
			end
		
			if verbose
				println("\nAnalysis complete. Results saved to: $output_dir")
			end
	end


end # module CLI