__precompile__(true)
module Large_Graph_Similarity
#   Packages
    using CSV
    using DataFrames
    using Dates
	using EzXML
	using LinearAlgebra
	using ProgressMeter
	using Random
	using SparseArrays
	using StatsFuns
	using Statistics

################
#   UTLITIES   #
################

#	DATA PROCESSING FUNCTIONS

#	Joiner for multi-valued string properties
	const MULTI_SEP = " | "

#	Standardized nodeset name for Url
	const URL_KEY = "URL"

#	Map ORA "type" → preferred nodeset key
	const TYPE_TO_STDKEY = Dict(
		"Agent"     => "Agent",
		"Event"     => "Tweet",
		"Knowledge" => "Hashtag",
		"Resource"  => URL_KEY,
	)

#	Map ORA network source/targetType → nodeset key when `source`/`target` attrs are missing
	const NETTYPE_TO_STDKEY = TYPE_TO_STDKEY

#	Permissive list of ORA property data types
	const ORA_DATA_TYPES = Set([
		"Text", "Text Category",
		"Number", "Number Category",
		"URI",
		"Date", "Datetime", "DateTime",
	])

#	Coalesce multi-valued string-like fields into one String using MULTI_SEP
	function _coalesce_str!(dict::Dict{String,Any}, key::String, val::AbstractString)
		existing = get(dict, key, nothing)
		if existing === nothing || existing === missing || isempty(String(existing))
			dict[key] = String(val)
		elseif !isempty(val)
			dict[key] = String(existing) * MULTI_SEP * String(val)
		end
		return nothing
	end

#	Coerce a raw string into the requested ORA data type (per your rules)
	function _coerce_value(raw::AbstractString, dtype::AbstractString)
		s = strip(String(raw))
		if dtype == "Text" || dtype == "Text Category" || dtype == "URI"
			return s
		elseif dtype == "Number"
			x = tryparse(Float64, s)
			return x === nothing ? missing : x
		elseif dtype == "Number Category"
			x = tryparse(Int, s)
			return x === nothing ? missing : x
		elseif dtype in ("Date", "Datetime", "DateTime")
			#	Try common ISO-8601 variants first
			formats = (dateformat"yyyy-mm-ddTHH:MM:SS.szzzz",
			           dateformat"yyyy-mm-ddTHH:MM:SSzzzz",
			           dateformat"yyyy-mm-ddTHH:MM:SS",
			           dateformat"yyyy-mm-dd")
			for fmt in formats
				dt = tryparse(DateTime, s, fmt)
				dt !== nothing && return dt
			end
			d = tryparse(Date, s, dateformat"yyyy-mm-dd")
			return d === nothing ? missing : DateTime(d)
		else
			return s
		end
	end

#	Parse <propertyIdentities> to map property id → ORA data type
	function _collect_nodeset_schema(nodeset::EzXML.Node)
		#	Schema map
			schema = Dict{String,String}()

		#	Walk <propertyIdentities>/<propertyIdentity>
			for child in eachelement(nodeset)
				if child.name == "propertyIdentities"
					for p in eachelement(child)
						if p.name == "propertyIdentity"
							id_attr = haskey(p, "id") ? p["id"] : nothing
							dt_attr = haskey(p, "dataType") ? p["dataType"] : nothing
							if id_attr !== nothing && dt_attr !== nothing
								dtype = String(dt_attr)
								schema[String(id_attr)] = dtype
							end
						end
					end
				end
			end

		#	Return
			return schema
	end

#	Extract one or more textual values from a <property> node
	function _extract_property_values(p::EzXML.Node)::Vector{String}
		#	Collect in file order, dedup later only if needed by callers
			vals = String[]

		#	1) value attribute
			if haskey(p, "value")
				v = String(p["value"])
				!isempty(strip(v)) && push!(vals, v)
			end

		#	2) <value> child elements
			for c in eachelement(p)
				if c.name == "value"
					v = String(nodecontent(c))
					!isempty(strip(v)) && push!(vals, v)
				end
			end

		#	3) fallback: direct text content (if nothing else found)
			if isempty(vals)
				v = String(nodecontent(p))
				!isempty(strip(v)) && push!(vals, v)
			end

		#	Return
			return vals
	end

#	Ingest a single <property> into `row` using schema rules (top-level helper)
	function _ingest_property!(
		row::Dict{String,Any},
		p::EzXML.Node,
		schema::Dict{String,String},
		prop_keys::Set{String}
	)
		#	Property id and dtype
			pid = haskey(p, "id") ? p["id"] : nothing
			pid === nothing && return
			key = String(pid)
			dtype = get(schema, key, "Text")

		#	Extract one or more values (from attribute/child/fallback)
			vals = _extract_property_values(p)

		#	Type-specific accumulation
			if dtype == "Text" || dtype == "Text Category" || dtype == "URI"
				if !isempty(vals)
					for v in vals
						_coalesce_str!(row, key, v)
					end
				end
			elseif dtype == "Number"
				if !isempty(vals)
					row[key] = _coerce_value(vals[end], "Number")
				end
			elseif dtype == "Number Category"
				if !isempty(vals)
					row[key] = _coerce_value(vals[end], "Number Category")
				end
			elseif dtype == "Date" || dtype == "Datetime" || dtype == "DateTime"
				if !isempty(vals)
					row[key] = _coerce_value(vals[end], "DateTime")
				end
			else
				#	Unknown dtype → safe string accumulation
				if !isempty(vals)
					for v in vals
						_coalesce_str!(row, key, v)
					end
				end
			end

		#	Track presence
			push!(prop_keys, key)
			return nothing
	end

#	Read one <nodeset> block into (standardized_key, DataFrame)
	function _parse_nodeset(nodeset::EzXML.Node)
		#	Attributes
			ns_type = haskey(nodeset, "type") ? nodeset["type"] : nothing
			ns_id   = haskey(nodeset, "id")   ? nodeset["id"]   : nothing
			ns_type === nothing && error("nodeset missing 'type' attribute")
			ns_id   === nothing && error("nodeset missing 'id' attribute")

		#	Standardized key
			stdkey = get(TYPE_TO_STDKEY, String(ns_type), String(ns_id))
			stdkey == "Url" && (stdkey = URL_KEY)

		#	Schema from <propertyIdentities>
			schema = _collect_nodeset_schema(nodeset)

		#	Collect rows as Dicts, then materialize (canonical keys first)
			rows = Vector{Dict{String,Any}}()
			prop_keys = Set{String}(["Node ID","Node Label"])

		#	Walk <node> children
			for child in eachelement(nodeset)
				if child.name == "node"
					row = Dict{String,Any}()

					#	Canonical Node ID
						node_id = haskey(child, "id") ? child["id"] : nothing
						node_id === nothing && error("node without 'id' in nodeset $(String(ns_id))")
						row["Node ID"] = String(node_id)

					#	Direct <property> elements
						for p in eachelement(child)
							if p.name == "property"
								_ingest_property!(row, p, schema, prop_keys)
							end
						end

					#	<properties>/<property> wrapper (common in ORA)
						for pwrap in eachelement(child)
							if pwrap.name == "properties"
								for p in eachelement(pwrap)
									if p.name == "property"
										_ingest_property!(row, p, schema, prop_keys)
									end
								end
							end
						end

					#	Guarantee "Node Label" field exists (empty string if absent)
						haskey(row, "Node Label") || (row["Node Label"] = "")

					push!(rows, row)
				end
			end

		#	Column order: "Node ID", "Node Label", then schema-declared (excluding the two), then extras seen
			ordered_keys = String["Node ID","Node Label"]
			for k in keys(schema)
				(k != "Node ID" && k != "Node Label") && push!(ordered_keys, k)
			end
			for k in prop_keys
				if !(k in ordered_keys)
					push!(ordered_keys, k)
				end
			end

		#	Allocate columns by dtype (canonical two are Strings)
			data = Dict{Symbol,Vector}()
			for k in ordered_keys
				if k == "Node ID" || k == "Node Label"
					data[Symbol(k)] = String[]
				else
					dt = get(schema, k, "Text")
					if dt == "Number"
						data[Symbol(k)] = Vector{Union{Missing,Float64}}()
					elseif dt == "Number Category"
						data[Symbol(k)] = Vector{Union{Missing,Int64}}()
					elseif dt == "Date" || dt == "Datetime" || dt == "DateTime"
						data[Symbol(k)] = Vector{Union{Missing,DateTime}}()
					else
						data[Symbol(k)] = String[]
					end
				end
			end

		#	Populate columns
			for r in rows
				push!(data[Symbol("Node ID")], String(get(r, "Node ID", "")))
				push!(data[Symbol("Node Label")], String(get(r, "Node Label", "")))
				for k in ordered_keys
					(k == "Node ID" || k == "Node Label") && continue
					col = data[Symbol(k)]
					if haskey(r, k)
						val = r[k]
						if isa(col, Vector{String})
							push!(col, String(val))
						else
							push!(col, val === nothing ? missing : val)
						end
					else
						if isa(col, Vector{String})
							push!(col, "")
						else
							push!(col, missing)
						end
					end
				end
			end

		#	Materialize DataFrame in the **exact** column order (avoid Dict constructor)
			pairs_ordered = [ Symbol(k) => data[Symbol(k)] for k in ordered_keys ]
			df = DataFrame(pairs_ordered)

		#	Return (key, df)
			return stdkey, df
	end

#	Parse one <network> block; strict on node existence; flag missing weights
	function _parse_network(netnode::EzXML.Node, nodesets_map::Dict{String,DataFrame})
		#	Attributes
			net_id = haskey(netnode, "id") ? netnode["id"] : nothing
			net_id === nothing && error("<network> missing 'id'")

			src_type   = haskey(netnode, "sourceType") ? netnode["sourceType"] : nothing
			tgt_type   = haskey(netnode, "targetType") ? netnode["targetType"] : nothing
			src_ns_att = haskey(netnode, "source")     ? netnode["source"]     : nothing
			tgt_ns_att = haskey(netnode, "target")     ? netnode["target"]     : nothing

		#	Resolve nodeset keys (prefer explicit source/target attrs)
			src_key = src_ns_att !== nothing ? String(src_ns_att) : (
				src_type === nothing ? nothing : get(NETTYPE_TO_STDKEY, String(src_type), nothing)
			)
			tgt_key = tgt_ns_att !== nothing ? String(tgt_ns_att) : (
				tgt_type === nothing ? nothing : get(NETTYPE_TO_STDKEY, String(tgt_type), nothing)
			)
			src_key === nothing && error("network $(String(net_id)) missing resolvable source nodeset")
			tgt_key === nothing && error("network $(String(net_id)) missing resolvable target nodeset")

		#	Standardize URL capitalization
			src_key == "Url" && (src_key = URL_KEY)
			tgt_key == "Url" && (tgt_key = URL_KEY)

		#	Nodeset presence
			haskey(nodesets_map, src_key) || error("network $(String(net_id)): source nodeset '$src_key' not found")
			haskey(nodesets_map, tgt_key) || error("network $(String(net_id)): target nodeset '$tgt_key' not found")

		#	ID sets for strict checking (use canonical "Node ID" column)
			src_df = nodesets_map[src_key]
			tgt_df = nodesets_map[tgt_key]
			hasproperty(src_df, Symbol("Node ID")) || error("network $(String(net_id)): nodeset '$src_key' missing 'Node ID' column")
			hasproperty(tgt_df, Symbol("Node ID")) || error("network $(String(net_id)): nodeset '$tgt_key' missing 'Node ID' column")
			src_ids = Set(String.(src_df[!, Symbol("Node ID")]))
			tgt_ids = Set(String.(tgt_df[!, Symbol("Node ID")]))

		#	Flags
			isDirected     = !(haskey(netnode,"isDirected")     && netnode["isDirected"] == "false")
			isBinary       =  (haskey(netnode,"isBinary")       && netnode["isBinary"]   == "true")
			allowSelfLoops =  (haskey(netnode,"allowSelfLoops") && netnode["allowSelfLoops"] == "true")

		#	Edge buffers
			src_col = String[]
			dst_col = String[]
			wgt_col = Float64[]
			hadMissingWeights = false

		#	Read <link> edges
			for lnk in eachelement(netnode)
				if lnk.name == "link"
					s = haskey(lnk, "source") ? lnk["source"] : nothing
					t = haskey(lnk, "target") ? lnk["target"] : nothing
					s === nothing && error("network $(String(net_id)): <link> missing 'source'")
					t === nothing && error("network $(String(net_id)): <link> missing 'target'")

					sid = String(s)
					tid = String(t)

					(sid in src_ids) || error("network $(String(net_id)): unknown source node '$sid' in '$src_key'")
					(tid in tgt_ids) || error("network $(String(net_id)): unknown target node '$tid' in '$tgt_key'")

					if haskey(lnk, "value")
						w = tryparse(Float64, String(lnk["value"]))
						push!(wgt_col, w === nothing ? 1.0 : w)
						hadMissingWeights |= (w === nothing)
					else
						push!(wgt_col, 1.0)
						hadMissingWeights = true
					end

					push!(src_col, sid)
					push!(dst_col, tid)
				end
			end

		#	Assemble meta
			meta = (;
				id                = String(net_id),
				sourceType        = src_type === nothing ? "" : String(src_type),
				targetType        = tgt_type === nothing ? "" : String(tgt_type),
				sourceNodeset     = src_key,
				targetNodeset     = tgt_key,
				isDirected        = isDirected,
				isBinary          = isBinary,
				allowSelfLoops    = allowSelfLoops,
				hadMissingWeights = hadMissingWeights,
				edges             = DataFrame(:src => src_col, :dst => dst_col, :weight => wgt_col),
			)

		#	Return
			return String(net_id), meta
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

#	NORMALIZATION FUNCTIONS

#	Helper Function for freeman_degree_normalization: symmetry check
	function _is_symmetric(adj::SparseMatrixCSC{<:Real,Int}; directed::Bool=true, atol::Float64=1e-12)
		#	Validation
			if size(adj, 1) != size(adj, 2)
				throw(ArgumentError("Adjacency must be square"))
			end

		#	Undirected => symmetric by convention
			if !directed
				return true
			end

		#	Numerical symmetry check
			delta = adj - adj'
			return LinearAlgebra.norm(delta, 1) <= atol
	end

#	Helper Function for freeman_degree_normalization: bipartite mode counts
	function _bipartite_counts(types::AbstractVector{Bool})
		"""
		Args:
			types::AbstractVector{Bool}: vertex modes; true = first mode, false = second
		Returns:
			Tuple{Int,Int}: (first_mode_count, second_mode_count)
		Notes:
			Aligns with Python reference where counts are derived from V(type).
		"""

		#	Validation
			if isempty(types)
				throw(ArgumentError("types vector must not be empty"))
			end

		#	Count modes
			first_mode = count(types)
			second_mode = length(types) - first_mode

		#	Return counts
			return (first_mode, second_mode)
	end

#	COMPARISON FUNCTIONS

#	Adjusted Rand Index Calculation
	function adjusted_rand_index(partition1::Vector{Int}, partition2::Vector{Int})
		"""
		Args:
			partition1::Vector{Int}: first partition/clustering
			partition2::Vector{Int}: second partition/clustering
		Returns:
			Float64: ARI score between -1 and 1 (1 = perfect agreement)
		Notes:
			Calculates Adjusted Rand Index between two partitions.
			Corrects for chance agreement in clustering comparisons.
		"""
		
		#	Validation
			n = length(partition1)
			if n != length(partition2)
				throw(ArgumentError("Partitions must have same length"))
			end
			if n == 0
				return 1.0
			end
		
		#	Build contingency table
			labels1 = unique(partition1)
			labels2 = unique(partition2)
			n_clusters1 = length(labels1)
			n_clusters2 = length(labels2)
			
		#	Create mapping for efficient indexing
			map1 = Dict(label => i for (i, label) in enumerate(labels1))
			map2 = Dict(label => i for (i, label) in enumerate(labels2))
			
		#	Build contingency matrix
			contingency = zeros(Int, n_clusters1, n_clusters2)
			for i in 1:n
				row = map1[partition1[i]]
				col = map2[partition2[i]]
				contingency[row, col] += 1
			end
		
		#	Calculate marginals
			sum_rows = sum(contingency, dims=2)
			sum_cols = sum(contingency, dims=1)
		
		#	Calculate index components
			sum_nij_2 = sum(contingency .^ 2)
			sum_ai_2 = sum(sum_rows .^ 2)
			sum_bj_2 = sum(sum_cols .^ 2)
			
		#	Calculate combinations
			comb_nij = (sum_nij_2 - n) / 2
			comb_ai = (sum_ai_2 - n) / 2
			comb_bj = (sum_bj_2 - n) / 2
			
		#	Total combinations
			total_comb = n * (n - 1) / 2
		
		#	Expected index
			expected_index = (comb_ai * comb_bj) / total_comb
		
		#	Maximum index
			max_index = (comb_ai + comb_bj) / 2
		
		#	Handle edge cases
			if max_index == expected_index
				if comb_nij == expected_index
					return 1.0
				else
					return 0.0
				end
			end
		
		#	Adjusted Rand Index
			ari = (comb_nij - expected_index) / (max_index - expected_index)
		
		return ari
	end

########################
#   IMPORT FUNCTIONS   #
########################

#   ORA Meta-Network Import Function
    function load_ora_xml(filepath::AbstractString)
		#	Developer Notes
			#	- Strict on network node references; permissive on attributes.
			#	- Multi-valued string-like properties are concatenated with MULTI_SEP.
			#	- Numbers → Float64, Number Categories → Int64, Date/Datetime → DateTime.
			#	- IDs remain Strings; Url nodeset standardized as "URL".

		#	Read XML
			doc = readxml(filepath)
			root = doc.root
			(root === nothing) && error("Empty XML document")

		#	Locate <MetaNetwork> (may be nested under <DynamicMetaNetwork>)
			meta = nothing
			if root.name == "MetaNetwork"
				meta = root
			elseif root.name == "DynamicMetaNetwork"
				for child in eachelement(root)
					if child.name == "MetaNetwork"
						meta = child
						break
					end
				end
			end
			meta === nothing && error("No <MetaNetwork> element found")

		#	Parse nodesets
			nodesets_map = Dict{String,DataFrame}()
			for child in eachelement(meta)
				if child.name == "nodes"
					for ns in eachelement(child)
						if ns.name == "nodeset"
							key, df = _parse_nodeset(ns)
							nodesets_map[key] = df
						end
					end
				end
			end

		#	Ensure expected sets exist (warn if missing)
			for must in ("Agent", "Tweet", "Hashtag", "URL")
				haskey(nodesets_map, must) || @warn "Nodeset '$must' not found in file"
			end

		#	Parse networks (strict)
			networks_map = Dict{String,NamedTuple}()
			for child in eachelement(meta)
				if child.name == "networks"
					for net in eachelement(child)
						if net.name == "network"
							id, meta_nt = _parse_network(net, nodesets_map)
							networks_map[id] = meta_nt
						end
					end
				end
			end

		#	Clean up
			EzXML.finalize(doc)

		#	Return structure
			return (;
				nodesets = nodesets_map,
				networks = networks_map,
			)
	end
	@doc """
		load_ora_xml(filepath::AbstractString) -> NamedTuple

		Read an ORA **MetaNetwork** XML export and return:
		- `nodesets::Dict{String,DataFrame}` with keys:
		  - `"Agent"`, `"Tweet"`, `"Hashtag"`, `"URL"` (standardized from `Url`)
		  - Each DataFrame has an `id::String` column plus one column per declared property.
		    * String-like (`Text`, `Text Category`, `URI`) are `String`. Multiple
		      occurrences are concatenated with the configured separator.
		    * `Number` → `Union{Missing,Float64}`
		    * `Number Category` → `Union{Missing,Int64}`
		    * `Date/Datetime/DateTime` → `Union{Missing,DateTime}`
		- `networks::Dict{String,NamedTuple}` keyed by the network `id` in the file.
		  Each value contains:
		  - `id::String`, `sourceType::String`, `targetType::String`
		  - `sourceNodeset::String`, `targetNodeset::String`
		  - `isDirected::Bool`, `isBinary::Bool`, `allowSelfLoops::Bool`
		  - `hadMissingWeights::Bool` (true if any `<link>` lacked a `value`)
		  - `edges::DataFrame` with columns `:src::String`, `:dst::String`, `:weight::Float64`

		Behavior
		--------
		- **Strict on networks**: throws if any `<link>` references an unknown node id.
		- **Permissive on attributes**: unknown/missing properties become `""` (strings)
		  or `missing` (numeric/date types).
		- IDs are preserved as **Strings**. The `Url` nodeset is exposed as **"URL"**.

		Example
		-------
		```julia
		out = load_ora_xml("/path/to/Balikatan_2022_Processed.xml")
		df_agents = out.nodesets["Agent"]
		nt = out.networks["Agent x Tweet - Sender"]
		first(nt.edges, 5)
		```
	""" load_ora_xml

################
#   MEASURES   #
################

#   DEGREE MEASURES   

#	In-Degree
	function in_degree(edges::DataFrame; 
	                   weighted::Bool=true, 
	                   normalize::Bool=false,
	                   agg_func::Function=sum)
		"""
		Args:
			edges::DataFrame: edge list with src, dst, and optionally weight columns
			weighted::Bool: use edge weights if available (default = true)
			normalize::Bool: if true, returns Freeman-normalized in-degree via freeman_degree_normalization (default = false)
			agg_func::Function: function to aggregate multi-edges (default = sum)
		Returns:
			DataFrame: columns [node, in_degree]
		Notes:
			When normalize=false: in-degree is the sum of weights of incoming edges (column sums).
			When normalize=true: uses freeman_degree_normalization(...; mode=:in, directed=true) and renames the score to :in_degree.
		"""

		#	Validation
			if !hasproperty(edges, :src) || !hasproperty(edges, :dst)
				throw(ArgumentError("edges DataFrame must have src and dst columns"))
			end

		#	Handle empty edge list
			if nrow(edges) == 0
				return DataFrame(node=[], in_degree=Float64[])
			end

		#	Aggregate multi-edges
			clean_edges = _aggregate_multi_edges(edges; agg_func=agg_func)

		#	Normalized path: delegate to Freeman with mode=:in
			if normalize
				df = freeman_degree_normalization(clean_edges; mode=:in, directed=true, bipartite=false, weighted=weighted, agg_func=agg_func)
				rename!(df, :freeman_degree => :in_degree)
				return df
			end

		#	Unnormalized path: build adjacency and sum columns
			adj, node_to_idx, idx_to_node = _edgelist_to_sparse_matrix(clean_edges; weighted=weighted)
			in_deg_values = vec(sum(adj, dims=1))

		#	Assembling Result
			return DataFrame(node = idx_to_node, in_degree = in_deg_values)
	end

#	Out-Degree
	function out_degree(edges::DataFrame; 
	                    weighted::Bool=true, 
	                    normalize::Bool=false,
	                    agg_func::Function=sum)
		"""
		Args:
			edges::DataFrame: edge list with src, dst, and optionally weight columns
			weighted::Bool: use edge weights if available (default = true)
			normalize::Bool: if true, returns Freeman-normalized out-degree via freeman_degree_normalization (default = false)
			agg_func::Function: function to aggregate multi-edges (default = sum)
		Returns:
			DataFrame: columns [node, out_degree]
		Notes:
			When normalize=false: out-degree is the sum of weights of outgoing edges (row sums).
			When normalize=true: uses freeman_degree_normalization(...; mode=:out, directed=true) and renames the score to :out_degree.
		"""

		#	Validation
			if !hasproperty(edges, :src) || !hasproperty(edges, :dst)
				throw(ArgumentError("edges DataFrame must have src and dst columns"))
			end

		#	Handle empty edge list
			if nrow(edges) == 0
				return DataFrame(node=[], out_degree=Float64[])
			end

		#	Aggregate multi-edges
			clean_edges = _aggregate_multi_edges(edges; agg_func=agg_func)

		#	Normalized path: delegate to Freeman with mode=:out
			if normalize
				df = freeman_degree_normalization(clean_edges; mode=:out, directed=true, bipartite=false, weighted=weighted, agg_func=agg_func)
				rename!(df, :freeman_degree => :out_degree)
				return df
			end

		#	Unnormalized path: build adjacency and sum rows
			adj, node_to_idx, idx_to_node = _edgelist_to_sparse_matrix(clean_edges; weighted=weighted)
			out_deg_values = vec(sum(adj, dims=2))

		#	Assembling Result
			return DataFrame(node = idx_to_node, out_degree = out_deg_values)
	end

#	Total Degree
	function total_degree(edges::DataFrame; 
	                      weighted::Bool=true, 
	                      normalize::Bool=false,
	                      agg_func::Function=sum,
	                      ignore_direction::Bool=false,
	                      drop_self_loops::Bool=false,
	                      count_self_loops_once::Bool=true,
	                      atol::Float64=1e-12)
		"""
		Args:
			edges::DataFrame: edge list with src, dst, and optionally weight columns
			weighted::Bool: use edge weights if available (default = true)
			normalize::Bool: if true, returns Freeman-normalized total-degree (default = false)
			agg_func::Function: function to aggregate multi-edges (default = sum)
			ignore_direction::Bool: if true, treat as undirected for the total metric
			drop_self_loops::Bool: if true, exclude self-loops (u→u) entirely (default = false)
			count_self_loops_once::Bool: when not dropping loops and directed, count each loop once (default = true)
			atol::Float64: tolerance for symmetry tests (default = 1e-12)
		Returns:
			DataFrame: columns [node, total_degree]
		Notes:
			Order of operations:
			1) Build sparse adjacency (weighted or unweighted).
			2) Symmetrize if `ignore_direction=true` (A ← max(A, A')).
			3) If `drop_self_loops=true`, zero the diagonal once.
			4) Compute totals.

			Self-loop behavior:
			- By default (`count_self_loops_once=true`), a self-loop contributes its weight **once**
			  to total-degree (consistent with Freeman's numerator `row + col − diag` and ORA outputs).
			- If `drop_self_loops=true`, loops contribute nothing.
			- If `count_self_loops_once=false`, each self-loop contributes twice (once to in-degree,
			  once to out-degree), matching pure graph-theoretic totals.

			When `normalize=true`, the function applies Freeman’s :all normalization on the prepared matrix.
		"""

		#	Validation
			if !hasproperty(edges, :src) || !hasproperty(edges, :dst)
				throw(ArgumentError("edges DataFrame must have src and dst columns"))
			end

		#	Handle empty edge list
			if nrow(edges) == 0
				return DataFrame(node=[], total_degree=Float64[])
			end

		#	Aggregate multi-edges
			clean_edges = _aggregate_multi_edges(edges; agg_func=agg_func)

		#	Build sparse adjacency + node order
			adj, node_to_idx, idx_to_node = _edgelist_to_sparse_matrix(clean_edges; weighted=weighted)
			n = size(adj, 1)

		#	Optional symmetrization
			if ignore_direction
				adj = max.(adj, adj')
			end

		#	Drop self-loops once
			if drop_self_loops
				adj = copy(adj)
				for i in 1:n
					adj[i, i] = 0.0
				end
				dropzeros!(adj)
			end

		#	Unnormalized path
			if !normalize
				if ignore_direction
					total_deg_values = vec(sum(adj, dims=1))
				else
					in_deg  = vec(sum(adj, dims=1))
					out_deg = vec(sum(adj, dims=2))
					if drop_self_loops
						total_deg_values = in_deg .+ out_deg
					elseif count_self_loops_once
						total_deg_values = (in_deg .+ out_deg) .- collect(diag(adj))
					else
						total_deg_values = in_deg .+ out_deg
					end
				end
				return DataFrame(node = idx_to_node, total_degree = total_deg_values)
			end

		#	Normalized path
			row_sums = vec(sum(adj, dims=2))
			col_sums = vec(sum(adj, dims=1))
			diag_vec = collect(diag(adj))
			numerator = row_sums .+ col_sums .- diag_vec

			is_sym = ignore_direction ? true : _is_symmetric(adj; directed=true, atol=atol)

			if weighted
				nz = nonzeros(adj)
				V = (length(nz) > 0) ? maximum(nz) : 1.0
			else
				V = 1.0
			end

			N = n
			denom = is_sym ? (V * (N - 1)) : (2 * V * (N - 1))
			if denom == 0.0
				return DataFrame(node = idx_to_node, total_degree = zeros(Float64, n))
			end

			total_deg_values = numerator ./ denom

			return DataFrame(node = idx_to_node, total_degree = total_deg_values)
	end

#   In/Out Degree Ratio
	function degree_ratio(edges::DataFrame; 
	                      weighted::Bool=true,
	                      epsilon::Float64=1e-10)
		"""
		Args:
			edges::DataFrame: edge list with src, dst, and optionally weight columns
			weighted::Bool: use edge weights if available (default = true)
			epsilon::Float64: small value to avoid division by zero (default = 1e-10)
		Returns:
			DataFrame: columns [node, in_degree, out_degree, in_out_ratio]
		Notes:
			Ratio of in-degree to out-degree.
			Indicates authority (>1) vs hub (<1) behavior.
		"""
		
		#	Calculate in and out degrees
			in_deg_df = in_degree(edges; weighted=weighted)
			out_deg_df = out_degree(edges; weighted=weighted)
		
		#	Merge on node
			result = innerjoin(in_deg_df, out_deg_df, on=:node)
		
		#	Calculate ratio with epsilon to avoid division by zero
			result.in_out_ratio = result.in_degree ./ (result.out_degree .+ epsilon)
		
		#	Handle pure sinks (out_degree = 0)
			pure_sinks = result.out_degree .== 0
			result.in_out_ratio[pure_sinks] .= Inf
		
		#	Handle pure sources (in_degree = 0, out_degree > 0)
			pure_sources = (result.in_degree .== 0) .& (result.out_degree .> 0)
			result.in_out_ratio[pure_sources] .= 0.0
		
		#	Assembling Result
			return result
	end

#	Freeman Degree Normalization (edges → sparse; uni/bipartite; directed/undirected)
	function freeman_degree_normalization(edges::DataFrame;
	                                      mode::Symbol = :all,
	                                      directed::Bool = true,
	                                      bipartite::Bool = false,
	                                      types::Union{Nothing,AbstractVector{Bool}} = nothing,
	                                      weighted::Bool = true,
	                                      agg_func::Function = sum,
	                                      atol::Float64 = 1e-12)
		"""
		Args:
			edges::DataFrame: edge list with :src, :dst, and optional :weight
			mode::Symbol: :all | :out | :in (default = :all)
			directed::Bool: treat network as directed (default = true)
			bipartite::Bool: indicate bipartite network; requires `types` (default = false)
			types::Union{Nothing,AbstractVector{Bool}}: node-mode flags aligned to node order (true = first mode)
			weighted::Bool: use edge weights if available (default = true)
			agg_func::Function: aggregation for parallel edges (default = sum)
			atol::Float64: tolerance for symmetry test when `directed=true` (default = 1e-12)
		Returns:
			DataFrame: columns [node, freeman_degree]
		Notes:
			Builds a (possibly weighted) sparse adjacency via existing helpers, then applies
			Freeman-style normalization *aligned to your R/Python reference*:
			- mode=:all → symmetric:   divide by V*(N−1); asymmetric: divide by 2*V*(N−1)
			- mode=:out → symmetric:   divide by V*(N−1); asymmetric: divide by V*N
			- mode=:in  → symmetric:   divide by V*(N−1); asymmetric: divide by V*N
			where N is the number of columns of A (second mode in bipartite; n in unimodal).
		"""

		#	Validation
			if !hasproperty(edges, :src) || !hasproperty(edges, :dst)
				throw(ArgumentError("edges must have :src and :dst columns"))
			end
			if !(mode in (:all, :out, :in))
				throw(ArgumentError("mode must be :all, :out, or :in"))
			end
			if nrow(edges) == 0
				return DataFrame(node=Any[], freeman_degree=Float64[])
			end

		#	Aggregate multi-edges via existing helper
			clean_edges = _aggregate_multi_edges(edges; agg_func=agg_func)

		#	Build sparse adjacency and node order via existing helper
			adj, node_to_idx, idx_to_node = _edgelist_to_sparse_matrix(clean_edges; weighted=weighted)

		#	Compute marginals and diagonal
			row_sums = vec(sum(adj, dims=2))               # out-strength
			col_sums = vec(sum(adj, dims=1))               # in-strength
			diagonal = collect(diag(adj))                  # self-loop weights as dense vector

		#	Determine V (max edge weight if weighted; else 1.0)
			V = (weighted && hasproperty(clean_edges, :weight) && !isempty(clean_edges.weight)) ?
			    maximum(clean_edges.weight) : 1.0

		#	Symmetry detection (undirected or directed-but-symmetric)
			is_sym = _is_symmetric(adj; directed=directed, atol=atol)

		#	Determine N & R (bipartite or unimodal)
			n = size(adj, 1)

		#	Initialize defaults (unimodal)
			N = n
			R = n

		#	Override for bipartite if requested
			if bipartite
				if types === nothing
					throw(ArgumentError("bipartite=true requires a `types::Vector{Bool}`"))
				end
				if length(types) != n
					throw(ArgumentError("length(types) must equal number of nodes ($n)"))
				end
				first_mode, second_mode = _bipartite_counts(types)
				R = first_mode
				N = second_mode
			end

		#	Edge cases: insufficient neighbors
			if N ≤ 1
				return DataFrame(node = idx_to_node, freeman_degree = zeros(Float64, n))
			end

		#	Apply denominators by mode (match R/Python reference exactly)
			numerator = zeros(Float64, n)
			denom = 0.0
			if mode == :all
				#	Total-degree numerator
					numerator .= row_sums .+ col_sums .- diagonal
				#	Denominator by symmetry
					denom = is_sym ? (V * (N - 1)) : (2 * V * (N - 1))
			elseif mode == :out
				#	Out-degree numerator
					numerator .= row_sums
				#	Symmetric → V*(N−1); Asymmetric (directed) → V*N
					denom = is_sym ? (V * (N - 1)) : (V * N)
			else
				#	In-degree numerator
					numerator .= col_sums
				#	Symmetric → V*(N−1); Asymmetric (directed) → V*N
					denom = is_sym ? (V * (N - 1)) : (V * N)
			end

		#	Protect against zero denominator
			if denom == 0.0
				return DataFrame(node = idx_to_node, freeman_degree = zeros(Float64, n))
			end

		#	Compute normalized scores
			scores = numerator ./ denom

		#	Assembling Result
			return DataFrame(node = idx_to_node, freeman_degree = scores)
	end
	@doc raw"""
	**Description**
	Compute Freeman-normalized degree centrality for an edge list (supports weighted/unweighted, directed/undirected, and optional bipartite via `types`), aligned to the R/Python reference you provided.

	**Usage**
	`freeman_degree_normalization(edges::DataFrame; mode::Symbol=:all, directed::Bool=true, bipartite::Bool=false, types::Union{Nothing,AbstractVector{Bool}}=nothing, weighted::Bool=true, agg_func::Function=sum, atol::Float64=1e-12)`

	**Details**
	Let `A` be the (possibly weighted) adjacency matrix. The function:
	- Builds `A` from `edges` (uses weights if present).
	- Determines whether the graph is **symmetric** (undirected or directed-but-symmetric).
	- Handles bipartite graphs via `types` and sets `N` and `R` as the sizes of the two modes; otherwise `N = R = ncol(A)`.
	- Sets `V` to the **maximum edge weight** if weighted, else `V = 1`.

	**Normalization denominators (Freeman convention, matching R/Python):**
	- **Mode `:all`**: numerator = `rowSums(A) + colSums(A) − diag(A)`.  
	Symmetric → divide by `V*(N−1)`; Asymmetric → divide by `2*V*(N−1)`.
	- **Mode `:out`**: numerator = `rowSums(A)`.  
	Symmetric → divide by `V*(N−1)`; Asymmetric → divide by `V*N`.
	- **Mode `:in`**: numerator = `colSums(A)`.  
	Symmetric → divide by `V*(N−1)`; Asymmetric → divide by `V*N`.

	For bipartite inputs, `N` is the **second-mode** size (number of columns of `A`); normalization uses this `N` for the target mode’s denominator.

	**Edge Cases**
	If `N ≤ 1` or the denominator is zero, returns zeros.

	**Value**
	A `DataFrame` with:
	- `node`: Node identifiers (same order as produced by `_edgelist_to_sparse_matrix`).
	- `freeman_degree::Vector{Float64}`: Normalized degree centrality scores.

	**Examples**
	```julia
	using DataFrames, SparseArrays

	#	Undirected, unweighted triangle
		edges = DataFrame(src=[1,2,3], dst=[2,3,1])
		scores = freeman_degree_normalization(edges; mode=:all, directed=false)
		@show scores

	#	Directed, weighted (in/out use V*(N−1) if symmetric, V*N if asymmetric)
		edges_w = DataFrame(src=[1,1,2], dst=[2,3,3], weight=[2.0, 1.0, 1.5])
		scores_out = freeman_degree_normalization(edges_w; mode=:out, directed=true, weighted=true)
		@show scores_out

	#	Bipartite example (types must align with node order from helper)
		edges_bi = DataFrame(src=["A","B","C","B"], dst=["D","D","E","E"])
		types = [true,true,true,false,false]
		scores_in = freeman_degree_normalization(edges_bi; mode=:in, directed=false, bipartite=true, types=types)
		@show scores_in

	**References**
	Freeman, L. C. (1978). Centrality in social networks conceptual clarification. Social Networks, 1(3), 215–239.
	""" freeman_degree_normalization

#   LOCAL STRUCTURE

#	Helper Function for clustering_coefficient: ego network extraction
	function _extract_ego_network(adj::SparseMatrixCSC{Float64,Int64}, node_idx::Int; directed::Bool=true)
		"""
		Args:
			adj::SparseMatrixCSC: adjacency matrix
			node_idx::Int: index of ego node (1-based)
			directed::Bool: whether graph is directed (default = true)
		Returns:
			Tuple{Vector{Int}, SparseMatrixCSC}: (neighbor_indices, ego_subnet_adjacency)
		Notes:
			Extracts ego network: node + neighbors + links between them.
			For directed graphs, neighbors are nodes with any edge to/from ego.
		"""
		
		#	Find all neighbors (in and out for directed)
			if directed
				#	Out-neighbors (ego -> neighbor)
					out_neighbors = findnz(adj[node_idx, :])[1]
				#	In-neighbors (neighbor -> ego)
					in_neighbors = findnz(adj[:, node_idx])[1]
				#	Combine and deduplicate
					neighbors = unique(vcat(out_neighbors, in_neighbors))
			else
				#	For undirected, just get neighbors from row or column
					neighbors = findnz(adj[node_idx, :])[1]
			end
		
		#	Remove self-loop if present
			neighbors = filter(n -> n != node_idx, neighbors)
		
		#	Include ego in the network
			ego_nodes = vcat(node_idx, neighbors)
		
		#	Extract submatrix for ego network
			ego_subnet = adj[ego_nodes, ego_nodes]
		
		#	Return neighbor indices and subnet
			return (neighbors, ego_subnet)
	end

#	Helper Function for clustering_coefficient: count triangles
	function _count_triangles_directed(adj::SparseMatrixCSC{Float64,Int64}, node_idx::Int)
		"""
		Args:
			adj::SparseMatrixCSC: adjacency matrix
			node_idx::Int: index of node (1-based)
		Returns:
			Tuple{Float64, Float64}: (num_triangles, max_possible_triangles)
		Notes:
			Counts directed triangles through node.
			Triangle exists if i->j and j->k and (i->k or k->i).
		"""
		
		#	Get out and in neighbors
			out_neighbors = findnz(adj[node_idx, :])[1]
			in_neighbors = findnz(adj[:, node_idx])[1]
		
		#	Remove self-loops
			out_neighbors = filter(n -> n != node_idx, out_neighbors)
			in_neighbors = filter(n -> n != node_idx, in_neighbors)
		
		#	All neighbors (for counting possible triangles)
			all_neighbors = unique(vcat(out_neighbors, in_neighbors))
			k = length(all_neighbors)
		
		#	Maximum possible directed triangles
			max_triangles = k * (k - 1)
		
		#	Count actual triangles (edges between neighbors)
			if k < 2
				return (0.0, Float64(max_triangles))
			end
		
		#	Count edges between neighbors
			triangle_count = 0.0
			for i in all_neighbors
				for j in all_neighbors
					if i != j && adj[i, j] > 0
						triangle_count += 1.0
					end
				end
			end
		
		#	Return counts
			return (triangle_count, Float64(max_triangles))
	end

#	Local Clustering Coefficient (Node Level)
	function local_clustering_coefficient(edges::DataFrame;
	                                     directed::Bool=true,
	                                     weighted::Bool=false,
	                                     method::Symbol=:density,
	                                     agg_func::Function=sum,
	                                     include_neighbor_selfloops::Bool=true)
		"""
		Args:
			edges::DataFrame: edge list with :src, :dst, and optional :weight columns
			directed::Bool: treat graph as directed (default = true)
			weighted::Bool: use edge weights if available (default = false, uses binary)
			method::Symbol: :density (ego network density) or :transitivity (triangle-based) (default = :density)
			agg_func::Function: aggregation for parallel edges (default = sum)
			include_neighbor_selfloops::Bool:
				- true  => include neighbor self-loops in numerator and denominator
				           (ORA parity; directed uses k*k, undirected k*(k+1)/2)
				- false => exclude neighbor self-loops in numerator and denominator
				           (directed uses k*(k-1), undirected k*(k-1)/2)
		Returns:
			DataFrame: columns [node, clustering_coefficient]
		Notes:
			:density computes the density of the ego's neighbor subgraph (ego excluded).
			For ORA parity on directed graphs, set include_neighbor_selfloops=true (default),
			which counts neighbor self-loops and divides by k*k where k = number of neighbors.
			:transitivity computes a standard triangle-based local coefficient.
		"""

		# 	Validation
			if !hasproperty(edges, :src) || !hasproperty(edges, :dst)
				throw(ArgumentError("edges DataFrame must have :src and :dst columns"))
			end
			if !(method in (:density, :transitivity))
				throw(ArgumentError("method must be :density or :transitivity"))
			end

		# 	Handle empty edge list
			if nrow(edges) == 0
				return DataFrame(node=[], clustering_coefficient=Float64[])
			end

		# 	Aggregate multi-edges
			clean_edges = _aggregate_multi_edges(edges; agg_func=agg_func)

		# 	Build adjacency matrix (binary unless weighted=true & :weight present)
			if weighted && hasproperty(clean_edges, :weight)
				adj, node_to_idx, idx_to_node = _edgelist_to_sparse_matrix(clean_edges; weighted=true)
			else
				adj, node_to_idx, idx_to_node = _edgelist_to_sparse_matrix(clean_edges; weighted=false)
			end

		# 	Initialize clustering coefficients
			n = length(idx_to_node)
			clustering_values = zeros(Float64, n)

		# 	Compute per node
			for i in 1:n
				if method == :density
					# 	Ego network: ego_subnet has ego at index 1; neighbors at 2:end
						_, ego_subnet = _extract_ego_network(adj, i; directed=directed)

					# 	No neighbors at all (ego_subnet is 1x1)
						if size(ego_subnet, 1) <= 1
							clustering_values[i] = 0.0
							continue
						end

					# 	Neighbor block (exclude ego row/col)
						neighbor_block_indices = 2:size(ego_subnet, 1)
						neighbor_subnet = ego_subnet[neighbor_block_indices, neighbor_block_indices]

					# 	k = number of neighbors
						k = size(neighbor_subnet, 1)

					# 	Numerator: edges among neighbors (match loop convention)
						if include_neighbor_selfloops
							edge_sum = weighted ? sum(neighbor_subnet) : nnz(neighbor_subnet)
						else
							neighbor_subnet_nodiag = copy(neighbor_subnet)
							for d in 1:k
								neighbor_subnet_nodiag[d, d] = 0
							end
							edge_sum = weighted ? sum(neighbor_subnet_nodiag) : nnz(neighbor_subnet_nodiag)
						end

					# 	Denominator: max possible edges among neighbors (aligned with numerator)
						max_edges = 0.0
						if directed
							max_edges = include_neighbor_selfloops ? (k * k) : (k * (k - 1))
						else
							max_edges = include_neighbor_selfloops ? (k * (k + 1) / 2) : (k * (k - 1) / 2)
						end

					# 	Density
						clustering_values[i] = (max_edges > 0) ? (edge_sum / max_edges) : 0.0

				else
					# 	Triangle-based local clustering (directed variant assumed by helper)
						triangles, max_triangles = _count_triangles_directed(adj, i)
						clustering_values[i] = (max_triangles > 0) ? (triangles / max_triangles) : 0.0
				end
			end

		# 	Result
			return DataFrame(
				node = idx_to_node,
				clustering_coefficient = clustering_values
			)
	end
	@doc raw"""
	**Description**
	Computes the local clustering coefficient for each node by measuring the density of its ego network. For directed graphs, this captures how tightly interconnected a node's immediate neighborhood is, indicating local information diffusion patterns and group cohesion.

	**Usage**
	`local_clustering_coefficient(edges::DataFrame; directed::Bool=true, weighted::Bool=false, method::Symbol=:density, agg_func::Function=sum)`

	**Arguments**
	- `edges::DataFrame`: Edge list with `:src`, `:dst`, and optional `:weight` columns.
	- `directed::Bool`: Treat graph as directed (default `true`).
	- `weighted::Bool`: Use edge weights if available (default `false`, uses binary).
	- `method::Symbol`: `:density` (ego network density, ORA approach) or `:transitivity` (triangle-based) (default `:density`).
	- `agg_func::Function`: Aggregation function for parallel edges (default `sum`).

	**Details**
	The clustering coefficient measures the degree to which nodes tend to cluster together. For each node, it calculates the density of connections in its ego network (the node, its neighbors, and edges between them).

	Method `:density` (ORA approach): Computes the ratio of existing edges to possible edges among a node's neighbors. For directed graphs, max possible = k*(k-1); for undirected, max = k*(k-1)/2.

	Method `:transitivity`: Counts triangles passing through the node relative to connected triples.

	Higher values indicate tighter local clustering, supporting local information diffusion and decentralized infrastructure.

	**Value**
	A `DataFrame` with columns:
	- `node`: Node identifiers
	- `clustering_coefficient::Float64`: Local clustering coefficient [0,1]

	**Examples**
	```julia
	# Simple directed network
	edges = DataFrame(src=["A","A","B","B","C"], dst=["B","C","C","D","A"])
	local_cc = local_clustering_coefficient(edges; directed=true)
	
	# Undirected with ego density method
	local_cc = local_clustering_coefficient(edges; directed=false, method=:density)
	```

	**See Also**
	`global_clustering_coefficient`, `weighted_clustering_coefficient`

	**References**
	Watts DJ, Strogatz SH (1998). "Collective dynamics of 'small-world' networks" Nature 393(6684): 440-442.
	""" local_clustering_coefficient

#	Global Clustering Coefficient (Network Level)
	function global_clustering_coefficient(edges::DataFrame;
										directed::Bool=true,
										weighted::Bool=false,
										method::Symbol=:average,
										agg_func::Function=sum,
										drop_self_loops::Bool=true)
		"""
		Args:
			edges::DataFrame: edge list with src, dst, and optionally weight columns
			directed::Bool: treat graph as directed (default = true)
			weighted::Bool: use edge weights (default = false, uses binary in :transitivity)
			method::Symbol: :average (mean of local) or :transitivity (global ratio) (default = :average)
			agg_func::Function: aggregation for parallel edges (default = sum)
			drop_self_loops::Bool: if true, remove self-loops before computing (default = true)
		Returns:
			Float64: global clustering coefficient
		Notes:
			- :average returns mean of local clustering (ego-density).
			- :transitivity returns the fraction of connected triples that are closed.
			When directed=false, this path follows the ORA/NetStat undirected, binary, loopless
			specification on a simple graph. When directed=true, it follows the directed
			Newman-style wedge denominator (your original behavior).
		"""

		#	Average of locals (ego-density), optionally weighted
			if method == :average
				local_cc = local_clustering_coefficient(edges;
														directed=directed,
														weighted=weighted,
														method=:density,
														agg_func=agg_func)
				return mean(local_cc.clustering_coefficient)
			end

		#	:transitivity path (global ratio of closed triples to connected triples)
			clean_edges = _aggregate_multi_edges(edges; agg_func=maximum)

		#	Optional: drop self-loops early
			if drop_self_loops
				if hasproperty(clean_edges, :weight)
					clean_edges = clean_edges[clean_edges.src .!= clean_edges.dst, [:src, :dst, :weight]]
				else
					clean_edges = clean_edges[clean_edges.src .!= clean_edges.dst, [:src, :dst]]
				end
			end

			if directed
				#	Directed global transitivity (Newman-style wedges) ----
				#	Binary adjacency for triangle/tuple counting
					adj, _, _ = _edgelist_to_sparse_matrix(clean_edges; weighted=false)

					total_triangles = 0.0
					total_triples   = 0.0
					n = size(adj, 1)

					for i in 1:n
						# Count closed directed wedges centered at i
						triangles, _ = _count_triangles_directed(adj, i)
						total_triangles += triangles

						# Connected triples centered at i (Newman-directed denominator)
						out_deg = nnz(adj[i, :])
						in_deg  = nnz(adj[:, i])
						triples = out_deg * in_deg + out_deg * (out_deg - 1) + in_deg * (in_deg - 1)
						total_triples += triples
					end

					return total_triples > 0 ? (total_triangles / total_triples) : 0.0

			else
				#	Undirected, binary, loopless global transitivity (ORA/NetStat) ----
				# 	Canonicalize endpoints (min, max) to collapse to simple undirected edges
					edges_canonical = DataFrame(
						src = min.(clean_edges.src, clean_edges.dst),
						dst = max.(clean_edges.src, clean_edges.dst)
					)
					edges_simple = unique(edges_canonical)

				#	Build an undirected edge list by duplicating both directions
					edges_bidirectional = vcat(
						edges_simple,
						DataFrame(src = edges_simple.dst, dst = edges_simple.src)
					)

				#	Binary adjacency (presence only)
					A, _, _ = _edgelist_to_sparse_matrix(edges_bidirectional; weighted=false)

				#	Ensure strictly binary, symmetric, zero-diagonal adjacency
					A = max.(A, A')
					if drop_self_loops
						A = A .- spdiagm(0 => diag(A))
					end
					A = spzeros(Float64, size(A)...) .+ (A .> 0)

				#	Denominator: sum_i k_i (k_i - 1) == 2 * (# connected triples)
					k   = vec(sum(A, dims=2))
					den = sum(k .* (k .- 1))
					if den == 0.0
						return 0.0
					end

				# 	Numerator: 6 * (#triangles) via sum((A*A) .* A)
					tri6 = sum((A * A) .* A)

				#	Classic Transitivity: tri6 / den == 3T / (# connected triples)
					return tri6 / den
			end
	end
	@doc raw"""
	**Description**  
	Computes the global (network-level) clustering coefficient. Two variants are supported:
	1) the **average of local clustering** (ego-neighborhood density), and  
	2) the **global transitivity** (fraction of connected triples that are closed).

	This function also provides an **ORA/NetStat-compatible** global transitivity when `method = :transitivity` and `directed = false`: the graph is treated as **undirected, binary, and loopless**, directions are collapsed to a simple graph, and the measure is computed as the fraction of 2-paths that participate in a triangle.

	**Usage**  
	`global_clustering_coefficient(edges::DataFrame; directed::Bool=true, weighted::Bool=false, method::Symbol=:average, agg_func::Function=sum, drop_self_loops::Bool=true)`

	**Arguments**
	- `edges::DataFrame`: Edge list with `:src`, `:dst`, and optional `:weight`.
	- `directed::Bool`: Interpret the network as directed (`true`) or undirected (`false`). Default `true`.
	- `weighted::Bool`: Whether local clustering (when `method=:average`) uses weights. The `:transitivity` variant is always computed on a binary graph. Default `false`.
	- `method::Symbol`:  
	- `:average` – mean of node-level local clustering coefficients (ego-density).  
	- `:transitivity` – global ratio of closed triples to connected triples.  
	Default `:average`.
	- `agg_func::Function`: Aggregation for parallel edges before building adjacency (default `sum`).  
	For undirected transitivity, directions are collapsed with presence logic (see Details).
	- `drop_self_loops::Bool`: Remove self-loops before computing the metric. Default `true`.  
	(Recommended for reproducibility with ORA/NetStat.)

	**Details**
	- **Average of locals (`method=:average`):**  
	Computes the ego-neighborhood density for each node (using `local_clustering_coefficient(...; method=:density)`) and returns the mean. If `weighted=true` and weights are present, the ego subgraph density sums weights; otherwise it is binary.

	- **Global transitivity (`method=:transitivity`):**
	- **Directed case (`directed=true`):**  
		Uses the directed wedge denominator per node  
		`out_i * in_i + out_i*(out_i-1) + in_i*(in_i-1)`,  
		and counts a wedge as closed if the third side is present in either direction.  
		The returned value is `total_closed_wedges / total_wedges`.
	- **Undirected case (`directed=false`) – ORA/NetStat-compatible:**  
		1) Drop self-loops if `drop_self_loops=true`.  
		2) Collapse directions to an undirected **simple** graph (presence only). Internally we duplicate reversed edges and aggregate with `maximum`, which is equivalent to “any direction implies presence”.  
		3) Build a **binary**, symmetric, zero-diagonal adjacency `A`.  
		4) Denominator: `∑_i k_i (k_i − 1)` (twice the number of connected triples).  
		5) Numerator: `sum((A*A) .* A)` equals `6 ×` the number of undirected triangles.  
		6) Return `tri6 / den`, which is identical to `3T / (#connected triples)` and matches the NetStat/ORA specification:
		\[
		\text{Transitivity} = \frac{|\{(i,j,k): (i,j)\in L, (j,k)\in L, (i,k)\in L\}|}{|\{(i,j,k): (i,j)\in L, (j,k)\in L\}|}.
		\]

	**Value**  
	`Float64`: global clustering coefficient in `[0, 1]`.

	**Examples**
	```julia
	using DataFrames

	# Example 1: ORA/NetStat transitivity on undirected/binary/loopless graph
	val_netstat = global_clustering_coefficient(edges;
		directed=false, method=:transitivity, drop_self_loops=true)

	# Example 2: Directed global transitivity (binary)
	val_dir = global_clustering_coefficient(edges;
		directed=true, method=:transitivity)

	# Example 3: Average of local clustering (ego-density), undirected
	val_avg = global_clustering_coefficient(edges;
		directed=false, method=:average)

	# Example 4: Average of local clustering with weights (if weights present)
	val_avg_w = global_clustering_coefficient(edges;
		directed=true, weighted=true, method=:average)

	**Notes**
	To closely reproduce ORA’s reported “Transitivity,” prefer:
	directed=false, method=:transitivity, drop_self_loops=true.

	The :transitivity path is always computed on a binary graph regardless of weighted.

	**References**
	Newman, M. E. J. (2003). “The structure and function of complex networks.” SIAM Review, 45(2), 167–256.

	NetStat/ORA “Transitivity” definition (fraction of 2-paths that are closed).
	""" global_clustering_coefficient

#	Weighted Clustering Coefficient (Barrat et al. 2004)
	function weighted_clustering_coefficient(edges::DataFrame;
	                                        directed::Bool=true,
	                                        agg_func::Function=sum)
		"""
		Args:
			edges::DataFrame: edge list with src, dst, and (recommended) weight columns
			directed::Bool: treat graph as directed (default = true)
			agg_func::Function: aggregation for parallel edges (default = sum)
		Returns:
			DataFrame:
				- if directed=true: columns [node, cg_cycle, cg_middleman, cg_in, cg_out, cg_total, barrat_local]
				- if directed=false: columns [node, weighted_clustering] (Barrat local)
		Notes:
			- Barrat local is computed on the undirected projection with sum symmetrization:
			  Wᵤ = W + Wᵀ, diag set to 0, A = 1{Wᵤ>0}. Formula:
			  Cᵢ = diag(Wᵤ * A * A)[i] / ( rowSums(Wᵤ)[i] * ( rowSums(A)[i] - 1 ) ), isolates→0.
			- For directed networks, Clemente & Grassi (2018) components are reported alongside
			  Barrat local from the undirected projection for comparison.
		"""

		#	Validation
			if !hasproperty(edges, :src) || !hasproperty(edges, :dst)
				throw(ArgumentError("edges DataFrame must have :src and :dst columns"))
			end

		#	Aggregate duplicates, build weighted adjacency
			clean_edges = _aggregate_multi_edges(edges; agg_func=agg_func)
			W, node_to_idx, idx_to_node = _edgelist_to_sparse_matrix(clean_edges; weighted=true)

		#	Remove self-loops from W
			n = size(W, 1)
			for i in 1:n
				W[i,i] = 0.0
			end
			dropzeros!(W)

		#	Helper: Barrat local on UNDIRECTED projection with SUM symmetrization
			function _barrat_local_sum_sym(Win::SparseMatrixCSC{Float64,Int64})
				#	Sum-symmetrize and zero diagonal
					Wu = Win .+ transpose(Win)
					for i in 1:size(Wu,1)
						Wu[i,i] = 0.0
					end
					dropzeros!(Wu)

				#	Binary adjacency A = 1{Wu>0}, enforce symmetry & zero diagonal
					IW, JW, _ = findnz(Wu)
					A = sparse(IW, JW, ones(Float64, length(IW)), size(Wu,1), size(Wu,2))
					A = max.(A, transpose(A))
					for i in 1:size(A,1)
						A[i,i] = 0.0
					end
					dropzeros!(A)

				#	Barrat (2004) local: num/den with isolates→0
					s   = vec(sum(Wu, dims=2))             # strength (row sums of Wu)
					k   = vec(sum(A,  dims=2))             # degree (binary)
					num = vec(diag(Wu * A * A))            # weighted 2-step closure through neighbors
					den = s .* (k .- 1)

					res = similar(num)
					@inbounds for t in eachindex(num)
						res[t] = den[t] > 0 ? (num[t] / den[t]) : 0.0
					end
					return res
			end

		#	Directed vs Undirected
			if directed
			#	Clemente & Grassi (2018): directed weighted components
				df_cg = directed_clustering_cg(edges; isolates=:zero, agg_func=agg_func)
				rename!(df_cg,
					:cycle_cc     => :cg_cycle,
					:middleman_cc => :cg_middleman,
					:in_cc        => :cg_in,
					:out_cc       => :cg_out,
					:total_cc     => :cg_total
				)

			#	Barrat local from undirected projection
				barrat_local = _barrat_local_sum_sym(W)

			#	Result (node order matches idx_to_node produced with the same mapping helper)
				return DataFrame(
					node          = idx_to_node,
					cg_cycle      = df_cg.cg_cycle,
					cg_middleman  = df_cg.cg_middleman,
					cg_in         = df_cg.cg_in,
					cg_out        = df_cg.cg_out,
					cg_total      = df_cg.cg_total,
					barrat_local  = barrat_local
				)
			else
			#	Undirected Barrat
				barrat_local = _barrat_local_sum_sym(W)
				return DataFrame(node = idx_to_node, weighted_clustering = barrat_local)
			end
	end
	@doc raw"""
	**Description**
	Weighted local clustering:
	- `directed=true`: **Clemente & Grassi (2018)** total directed weighted clustering.
	- `directed=false`: **Barrat et al. (2004)** undirected weighted clustering.

	**Usage**
	`weighted_clustering_coefficient(edges::DataFrame; directed::Bool=true, agg_func::Function=sum)`

	**Arguments**
	- `edges::DataFrame`: Edge list with `:src`, `:dst`, and optional `:weight`.
	- `directed::Bool`: If `true`, return C&G **total**; if `false`, Barrat (default `true`).
	- `agg_func::Function`: Aggregation for parallel edges.

	**Notes**
	- Loops removed; multi-edges aggregated.
	- Undirected weights are symmetrized via `max(W, W')` before Barrat.
	- No weight rescaling is performed.

	**Value**
	A `DataFrame`:
	- `node`: Node identifiers
	- `weighted_clustering::Float64`: Local clustering score

	**References**
	- Barrat, A., Barthélemy, M., Pastor-Satorras, R., & Vespignani, A. (2004).
	  *The architecture of complex weighted networks*. PNAS, 101(11), 3747–3752.
	- Clemente, G. P., & Grassi, R. (2018).
	  *Directed clustering in weighted networks: a new perspective*. Chaos, Solitons & Fractals, 107, 26–38.
	""" weighted_clustering_coefficient

#	Directed Weighted Clustering (Clemente & Grassi, 2018)
	function directed_clustering_cg(edges::DataFrame;
                                isolates::Symbol = :zero,
                                agg_func::Function = sum)
		"""
		Args:
			edges::DataFrame: edge list with :src, :dst, and optional :weight
			isolates::Symbol: :zero (set NaN denominators to 0) or :NaN (propagate NaN) (default = :zero)
			agg_func::Function: aggregation for parallel edges (default = sum)
		Returns:
			DataFrame: columns [node, cycle_cc, middleman_cc, in_cc, out_cc, total_cc]
		Notes:
			Implements Clemente & Grassi (2018) directed, weighted local clustering:
			- cycle, middleman, in, out, and total components
			- loops removed; multiedges aggregated via `agg_func`
			- weights used as provided (no rescaling)
			- denominators per C&G; entries with nonpositive denom are 0 if isolates=:zero, else NaN
		"""

		#	Validation
			if !hasproperty(edges, :src) || !hasproperty(edges, :dst)
				throw(ArgumentError("edges DataFrame must have :src and :dst columns"))
			end

		#	Aggregate duplicates
			clean_edges = _aggregate_multi_edges(edges; agg_func=agg_func)

		#	Build weighted and binary adjacencies (directed)
			W, _, idx_to_node = _edgelist_to_sparse_matrix(clean_edges; weighted=true)
			A, _, _ = _edgelist_to_sparse_matrix(clean_edges; weighted=false)

		#	Force binary (defensive; ensures 0/1 even if upstream changed)
			A .= (A .> 0)

		#	Drop self-loops explicitly
			n = size(W, 1)
			for i in 1:n
				W[i, i] = 0.0
				A[i, i] = 0.0
			end
			dropzeros!(W); dropzeros!(A)

		#	Common vectors/matrices
			one = ones(Float64, n)
			AT  = transpose(A)    # or A'
			WT  = transpose(W)    # or W'

		#	In/Out degree (binary), total degree, bilateral dyads count
			degin  = vec(AT * one)                 # k_in
			degout = vec(A  * one)                 # k_out
			dtot   = vec((AT .+ A) * one)          # k_in + k_out
			dbil   = vec(diag(A * A))              # sum_j a_ij * a_ji

		#	Strengths (weighted): s_in, s_out, s_total
			sin   = vec(diag(AT * W))              # s_in
			sout  = vec(diag(A * transpose(W)))    # s_out
			stot  = sin .+ sout                    # s_total

		#	Bilateral strength term (C&G): sbil = diag(W*A + A*W)/2
			sbil = vec(diag(W * A .+ A * W)) ./ 2

		#	Numerators (each is 0.5 * diag(...))
			num_cyc = 0.5 .* vec(diag( W  * A  * A  .+ WT * AT * AT ))
			num_mid = 0.5 .* vec(diag( WT * A  * AT .+ W  * AT * A  ))
			num_in  = 0.5 .* vec(diag( WT * (A .+ AT) * A  ))
			num_out = 0.5 .* vec(diag( W  * (A .+ AT) * AT ))
			num_tot = 0.5 .* vec(diag( (W .+ WT) * (A .+ AT) * (A .+ AT) ))

		#	Denominators (C&G)
			den_cymid = 0.5 .* (sin .* degout .+ sout .* degin) .- sbil
			den_in    = sin  .* (degin  .- 1)
			den_out   = sout .* (degout .- 1)
			den_tot   = stot .* (dtot   .- 1) .- 2 .* sbil

		#	Safe division helper
			function _safe_div(num::AbstractVector{<:Real}, den::AbstractVector{<:Real}; isolates::Symbol = :zero)
				#	Allocate a dense Float64 result; works for both dense & sparse inputs
					res = Vector{Float64}(undef, length(num))

				#	Safe elementwise division
					@inbounds for i in eachindex(num, den)
						d = float(den[i])
						if d > 0
							res[i] = float(num[i]) / d
						else
							res[i] = (isolates === :zero) ? 0.0 : NaN
						end
					end
					return res
			end

		#	Per-node coefficients
			cyc  = _safe_div(num_cyc, den_cymid; isolates=isolates)
			mid  = _safe_div(num_mid, den_cymid; isolates=isolates)
			incc = _safe_div(num_in,  den_in;    isolates=isolates)
			outc = _safe_div(num_out, den_out;   isolates=isolates)
			totc = _safe_div(num_tot, den_tot;   isolates=isolates)

		#	Result
			return DataFrame(
				node = idx_to_node,
				cycle_cc = cyc,
				middleman_cc = mid,
				in_cc = incc,
				out_cc = outc,
				total_cc = totc
			)
	end
	@doc raw"""
	**Description**
	Directed, weighted local clustering coefficients per Clemente & Grassi (2018), returning the five components:
	*cycle*, *middleman*, *in*, *out*, and *total*.

	**Usage**
	`directed_clustering_cg(edges::DataFrame; isolates::Symbol=:zero, agg_func::Function=sum)`

	**Arguments**
	- `edges::DataFrame`: Edge list with `:src`, `:dst`, and optional `:weight` (weights used as given).
	- `isolates::Symbol`: `:zero` (default) sets coefficients to 0.0 when denominators are nonpositive; `:NaN` preserves `NaN`.
	- `agg_func::Function`: Aggregation for parallel edges (default `sum`).

	**Details**
	- Loops are removed; multi-edges aggregated.
	- Binary adjacency `A` derives from the presence of an edge.
	- Strength and degree terms follow the directed in/out definitions in C&G (2018).
	- Denominators: see paper; we set coefficients to 0 when denominators ≤ 0 and `isolates=:zero`.

	**Value**
	A `DataFrame` with columns:
	- `node`, `cycle_cc`, `middleman_cc`, `in_cc`, `out_cc`, `total_cc`.

	**References**
	- Clemente, G. P., & Grassi, R. (2018). *Directed clustering in weighted networks: a new perspective*. Chaos, Solitons & Fractals, 107, 26–38.
	""" directed_clustering_cg

#	Local Weighted Reciprocity (Squartini, node/ego level) with Normalization
	function local_weighted_reciprocity(edges::DataFrame;
	                                    weighted::Bool=true,
	                                    agg_func::Union{Function,Nothing}=nothing,
	                                    normalize::Symbol=:none)
		"""
		Args:
			edges::DataFrame: edge list with :src, :dst, and optional :weight
			weighted::Bool: use weights if present (default = true)
			agg_func::Union{Function,Nothing}: aggregation for parallel edges (default = sum for weighted, maximum for binary)
			normalize::Symbol: :none, :zscore, or :rank (default = :none)
		Returns:
			DataFrame: columns [node, r, reciprocated, out_strength, r_norm, normalization]
		Notes:
			Implements Squartini et al. local weighted reciprocity.
			r_i = (Σ_j min(w_ij, w_ji)) / (Σ_j w_ij) for j≠i.
			Self-loops excluded. Zero out-strength gives r_i = 0.
		"""
		
		#	Validation
			if !hasproperty(edges, :src) || !hasproperty(edges, :dst)
				throw(ArgumentError("edges must have :src and :dst columns"))
			end
			if !(normalize in (:none, :zscore, :rank))
				throw(ArgumentError("normalize must be :none, :zscore, or :rank"))
			end
		
		#	Handle empty edge list
			if nrow(edges) == 0
				return DataFrame(
					node = String[],
					r = Float64[],
					reciprocated = Float64[],
					out_strength = Float64[],
					r_norm = Float64[],
					normalization = Symbol[]
				)
			end
		
		#	Set default aggregation function
			if isnothing(agg_func)
				agg_func = (weighted && hasproperty(edges, :weight)) ? sum : maximum
			end
		
		#	Aggregate parallel edges
			clean_edges = _aggregate_multi_edges(edges; agg_func=agg_func)
		
		#	Build adjacency matrix
			use_weights = weighted && hasproperty(clean_edges, :weight)
			adj, node_to_idx, idx_to_node = _edgelist_to_sparse_matrix(clean_edges; weighted=use_weights)
			n = size(adj, 1)
		
		#	Remove self-loops
			for i in 1:n
				adj[i, i] = 0
			end
			dropzeros!(adj)
		
		#	Compute out-strength and reciprocated weight for each node
			out_strength = Array{Float64}(undef, n)
			recip = Array{Float64}(undef, n)
			
			for i in 1:n
				#	Out-strength: sum of outgoing weights
					out_strength[i] = sum(adj[i, :])

				#	Reciprocated weight: sum of min(w_ij, w_ji) over nonzero out-neighbors
					acc = 0.0
					cols, vals = findnz(adj[i, :])   # SparseVector ⇒ (indices, values)
					for t in 1:length(cols)
						j   = cols[t]
						wij = vals[t]
						wji = adj[j, i]
						acc += min(wij, wji)
					end
					recip[i] = acc
			end
		
		#	Calculate raw reciprocity r_i
			r = similar(recip)
			for i in 1:n
				r[i] = out_strength[i] > 0 ? recip[i] / out_strength[i] : 0.0
			end
		
		#	Apply normalization
			r_norm = copy(r)
			
			if normalize == :zscore
				#	Z-score normalization
					μ = mean(r)
					σ = std(r)
					if σ > 0
						for i in 1:n
							r_norm[i] = (r[i] - μ) / σ
						end
					else
						fill!(r_norm, 0.0)
					end
					
			elseif normalize == :rank
    			#	Dense ranks: equal values share a rank; next distinct value gets +1.
    			# 	Then scale ranks to [0,1] so the highest tier maps to 1.0.
					vals = collect(r)
					uniq = sort(unique(vals))                    # distinct r values, ascending
					rankmap = Dict(v => i for (i, v) in enumerate(uniq))  # v -> 1..k (dense)
					ranks = [rankmap[v] for v in vals]
					k = length(uniq)
					if k > 1
						for i in 1:n
							r_norm[i] = (ranks[i] - 1) / (k - 1)
						end
					else
						fill!(r_norm, 0.0)
					end
			end
		
		#	Assembling Result
			result = DataFrame(
				node = [idx_to_node[i] for i in 1:n],
				r = r,
				reciprocated = recip,
				out_strength = out_strength,
				r_norm = r_norm,
				normalization = fill(normalize, n)
			)
			return result
	end
	@doc raw"""
	**Description**
	Computes local (node-level) weighted reciprocity following Squartini et al. (2013). For each node, measures the fraction of outgoing weight that is reciprocated by incoming connections.

	**Usage**
	`local_weighted_reciprocity(edges::DataFrame; weighted=true, agg_func=nothing, normalize=:none)`

	**Arguments**
	- `edges::DataFrame`: Edge list with `:src`, `:dst`, and optionally `:weight` columns
	- `weighted::Bool`: Use edge weights if present (default `true`)
	- `agg_func::Function`: Aggregation for parallel edges (default `sum` for weighted, `maximum` for binary)
	- `normalize::Symbol`: Post-processing normalization - `:none` (raw), `:zscore` (standardized), or `:rank` (percentile)

	**Details**
	For each node i, local reciprocity is:
	r_i = Σ_j min(w_ij, w_ji) / Σ_j w_ij

	Where the sums are over all neighbors j≠i. This measures what fraction of i's outgoing communication is reciprocated. Self-loops are excluded. Nodes with zero out-strength receive r_i = 0.

	Normalization options:
	- `:none`: Raw values [0,1]
	- `:zscore`: (r_i - μ)/σ across nodes
	- `:rank`: Average rank scaled to [0,1]

	**Value**
	A `DataFrame` with columns:
	- `node`: Node identifier
	- `r`: Raw local reciprocity [0,1]
	- `reciprocated`: Total reciprocated weight
	- `out_strength`: Total outgoing weight
	- `r_norm`: Normalized reciprocity
	- `normalization`: Method used

	**Examples**
```julia
	# Weighted network with varied reciprocity
	edges = DataFrame(
		src = ["A", "B", "C", "D", "E"],
		dst = ["B", "A", "D", "C", "F"],
		weight = [5, 4, 1, 1, 6]
	)
	
	# Raw local reciprocity
	local_rec = local_weighted_reciprocity(edges)
	
	# With rank normalization
	local_rec_ranked = local_weighted_reciprocity(edges; normalize=:rank)
```

	**See Also**
	`reciprocity` (global measure)

	**References**
	Squartini T, Picciolo F, Ruzzenenti F, Garlaschelli D (2013). "Reciprocity of weighted networks" Scientific Reports 3:2729.
	""" local_weighted_reciprocity

#   INFLUENCE CENTRALITY MEASURES

#	Helper for Page Rank: Weak Components from a Directed Sparse Adjacency
	function _component_indices_weak(adj::SparseMatrixCSC{<:Real,Int})
		"""
		Args:
			adj::SparseMatrixCSC{<:Real,Int}: directed adjacency (weights allowed)
		Returns:
			Vector{Vector{Int}}: 1-based index lists for each weakly connected component
		Notes:
			Builds an undirected reachability pattern (A ∨ Aᵀ) ignoring self-loops,
			then BFS to extract weak components.
		"""
		#	Specifying Parameters
			n = size(adj, 1)
			pat = spzeros(Bool, n, n)

		#	For the whole matrix, findnz returns (rows, cols, vals)
			rows, cols, _ = findnz(adj)
			@inbounds for k in eachindex(rows)
				i = rows[k]; j = cols[k]
				if i != j
					pat[i, j] = true
					pat[j, i] = true
				end
			end

		#	Isolating Components
			visited = falses(n)
			comps = Vector{Vector{Int}}()

			for s in 1:n
				visited[s] && continue
				q = Int[s]
				visited[s] = true
				comp = Int[]

				while !isempty(q)
					v = popfirst!(q)
					push!(comp, v)

					#	Row slice is a SparseVector ⇒ findnz returns (indices, values)
						idxs, _ = findnz(pat[v, :])
						@inbounds for w in idxs
							if !visited[w]
								visited[w] = true
								push!(q, w)
							end
						end
				end

				push!(comps, comp)
			end

		#	Return Identified Components
			return comps
	end

#	Helper Function for Page Rank Local and Component Scaled Page Rank: 
	function pagerank_local_ora_matrix(
		adj::SparseMatrixCSC{<:Real,Int},
		idx::Vector{Int};
		alpha::Float64 = 0.85,
		tol::Float64   = 1e-6,
		maxiter::Int   = 1000,
		final_norm::Symbol = :L1,   # :L1 or :sup
		mode::Symbol       = :in,   # :in or :out
		personalization::Union{Nothing,AbstractVector{<:Real}} = nothing,
		rng = Random.default_rng())
		"""
		Args:
			adj::SparseMatrixCSC{<:Real,Int}: full directed adjacency (weights allowed)
			idx::Vector{Int}: component node indices (1-based) to solve on
			alpha::Float64: damping factor in (0,1) (default = 0.85)
			tol::Float64: sup-norm stopping tolerance (default = 1e-6)
			maxiter::Int: maximum iterations (default = 1000)
			final_norm::Symbol: :L1 (sum 1) or :sup (max 1) cosmetic normalization
			mode::Symbol: :in (use Aᵀ) or :out (use A) before column-normalization
			personalization::Union{Nothing,AbstractVector{<:Real}}:
				optional component-local teleport vector (L1-normalized internally)
			rng: RNG for reproducible init (default = Random.default_rng())

		Returns:
			NamedTuple: (scores::Vector{Float64}, converged::Bool,
						iterations::Int, norm_used::Symbol)

		Notes:
			ORA conventions:
			- absolute weights, self-loops removed
			- build column-stochastic H (A or Aᵀ by mode)
			- dangling mass folded into teleport p
			- per-iteration sup-norm scaling; final L1/sup normalization
		"""
		#	Checks
			@assert 0.0 < alpha < 1.0 "alpha must be in (0,1)"
			@assert final_norm in (:L1, :sup)
			@assert mode in (:in, :out)

			n = length(idx)
			if n == 0
				return (scores=Float64[], converged=true, iterations=0, norm_used=final_norm)
			elseif n == 1
				return (scores=[1.0], converged=true, iterations=0, norm_used=final_norm)
			end

		#	Submatrix, drop self-loops, absolute weights
			A = adj[idx, idx]
			@inbounds for i in 1:n
				A[i,i] = 0
			end
			dropzeros!(A)
			A = SparseMatrixCSC{Float64,Int}(abs.(A))

		#	Column-stochastic H (A' for :in, A for :out)
			M = (mode == :in) ? transpose(A) : A
			colsum = vec(sum(M, dims=1))
			colsum[colsum .== 0.0] .= 1.0
			H = M * spdiagm(0 => (1.0 ./ colsum))

		# 	Teleport vector p
			p = if personalization === nothing
				fill(1.0 / n, n)
			else
				@assert length(personalization) == n "personalization length must match component size"
				pp = collect(float.(personalization))
				s = sum(pp); @assert s > 0 "personalization must have positive sum"
				pp ./ s
			end

		# 	Dangling columns in M (pre-normalization)
			dangling = vec(sum(M, dims=1) .== 0.0)

			# Initialize x ~ U(0,1), sup-normalized
			x = rand(rng, n)
			x ./= maximum(x)

		# 	Applying Iterative Power Method
			converged = false
			iters = 0
			for it in 1:maxiter
				x_prev = x
				y = H * x
				dang_mass = alpha * sum(x[dangling])
				y = alpha .* y .+ dang_mass .* p .+ (1.0 - alpha) .* p
				m = maximum(y)
				x = (m > 0) ? (y ./ m) : fill(1.0 / n, n)
				if maximum(abs.(x .- x_prev)) < tol
					converged = true
					iters = it
					break
				end
			end
			iters == 0 && (iters = maxiter)

		# 	Final cosmetic normalization
			if final_norm == :L1
				s = sum(x); if s > 0; x ./= s; end
			else
				m = maximum(x); if m > 0; x ./= m; end
			end

		#	Return Page Rank Scores
			return (scores=x, converged=converged, iterations=iters, norm_used=final_norm)
	end

#	Local ORA-Style PageRank on a Given Subgraph (indices)
	function pagerank_local_ora(edges::DataFrame;
		alpha::Float64 = 0.85,
		tol::Float64   = 1e-6,
		maxiter::Int   = 1000,
		final_norm::Symbol = :L1,      # :L1 or :sup
		mode::Symbol       = :in,      # :in or :out
		weighted::Bool     = true,
		agg_func::Union{Function,Nothing} = nothing,
		nodes::Union{Nothing,Vector{String}} = nothing,  # optional subset by name
		personalization::Union{Nothing,AbstractVector{<:Real}} = nothing,
		rng = Random.default_rng())
		"""
		Args:
			edges::DataFrame: :src, :dst, optional :weight
			alpha, tol, maxiter, final_norm, mode: same as matrix method
			weighted::Bool: use weights if present (default = true)
			agg_func::Union{Function,Nothing}: sum if weighted else maximum (default)
			nodes::Union{Nothing,Vector{String}}: optional subset of node names (component)
			personalization::Union{Nothing,AbstractVector}: optional teleport for that subset
			rng: RNG for reproducible init
		Returns:
			NamedTuple (scores, converged, iterations, norm_used) for the chosen nodes
		Notes:
			Uses your ecosystem helpers: _aggregate_multi_edges, _edgelist_to_sparse_matrix
		"""
		#	Basic Checks
			@assert mode in (:in, :out)
			if isnothing(agg_func)
				agg_func = (weighted && hasproperty(edges, :weight)) ? sum : maximum
			end

    	# 	Aggregate multi-edges
    		clean_edges = _aggregate_multi_edges(edges; agg_func=agg_func)

    	# 	Build adjacency (weighted iff requested and weights exist)
			use_weights = weighted && hasproperty(clean_edges, :weight)
			adj, node_to_idx, idx_to_node = _edgelist_to_sparse_matrix(clean_edges; weighted=use_weights)
			n = size(adj, 1)

			node_names=String[]
			node_names = [string(idx_to_node[i]) for i in 1:n]

    	# 	Pick the vertex set (default = all nodes)
			idx =
				if nodes === nothing
					collect(1:n)
				else
					# Map names -> indices, keep only those present
					[node_to_idx[name] for name in nodes if haskey(node_to_idx, name)]
				end
			if isempty(idx)
				return (scores=Float64[], converged=true, iterations=0, norm_used=final_norm)
			end

    	# 	Component-local personalization (in idx order) if provided
			p_local =
				if personalization === nothing
					nothing
				else
					@assert length(personalization) == length(idx) "personalization length must match #nodes in subset"
					personalization
				end

    	# 	Delegate to the core solver (this is NOT recursion; it calls the other method)
			res = pagerank_local_ora_matrix(adj, idx; alpha=alpha, tol=tol, maxiter=maxiter, final_norm=final_norm, mode=mode,
											personalization=p_local, rng=rng)
			
		#	Return Page Rank Scores
			return (scores=res.scores, node_names=node_names, converged=res.converged, iterations=res.iterations, norm_used=res.norm_used)
	end
	@doc raw"""
	**Description**
	Computes ORA-style PageRank on a selected vertex set from an edge list. This is a convenience
	wrapper that builds the adjacency, selects nodes (optionally by name), prepares an optional
	component-local teleport vector, and delegates to `pagerank_local_ora_matrix` (the core solver).

	**Usage**
	`pagerank_local_ora(edges::DataFrame;
						alpha=0.85, tol=1e-6, maxiter=1000,
						final_norm=:L1, mode=:in,
						weighted=true, agg_func=nothing,
						nodes=nothing, personalization=nothing,
						rng=Random.default_rng())`

	**Arguments**
	- `edges::DataFrame`: Edge list with `:src`, `:dst`, and optional `:weight`.
	- `alpha::Float64`: Damping in `(0,1)`. Default `0.85`.
	- `tol::Float64`: Sup-norm convergence tolerance. Default `1e-6`.
	- `maxiter::Int`: Maximum number of iterations. Default `1000`.
	- `final_norm::Symbol`: Final cosmetic scaling — `:L1` (scores sum to 1) or `:sup` (max=1).
	- `mode::Symbol`: `:in` (uses `A'` before column normalization) or `:out` (uses `A`).
	- `weighted::Bool`: Use edge weights if present. Default `true`.
	- `agg_func::Union{Function,Nothing}`: Aggregation for parallel edges; default `sum` when
	`weighted=true`, else `maximum`.
	- `nodes::Union{Nothing,Vector{String}}`: Optional subset of node names to solve on; defaults
	to all nodes found in `edges`.
	- `personalization::Union{Nothing,AbstractVector{<:Real}}`: Optional teleport vector for the
	chosen node set (auto L1-normalized internally). Length must equal `length(nodes)` (or the
	number of nodes selected by default).
	- `rng`: RNG for reproducible starts. Default `Random.default_rng()`.

	**Details**
	- Builds the adjacency with your helpers (`_aggregate_multi_edges`, `_edgelist_to_sparse_matrix`),
	strips self-loops, and (if `weighted=true`) uses absolute weights.
	- Selects a vertex set: by `nodes` (name → index mapping) or all nodes if `nodes=nothing`.
	- Prepares a component-local teleport vector if `personalization` is provided; otherwise uses
	uniform teleport within the selected set.
	- Calls `pagerank_local_ora_matrix(adj, idx; ...)`, which performs ORA-style PageRank:
	column-stochastic `H`, sup-norm stabilized iterations, and dangling handling. Final scores are
	`:L1` or `:sup` normalized per `final_norm`.

	**Value**
	A `NamedTuple` with:
	- `scores::Vector{Float64}` — PageRank scores on the selected nodes (order = selected indices).
	- `converged::Bool` — `true` iff the iteration met `tol` before `maxiter`.
	- `iterations::Int` — Number of iterations used.
	- `norm_used::Symbol` — `:L1` or `:sup` (final normalization applied).

	**Examples**
	```julia
	edges = DataFrame(
		src = ["A","B","B","C","X","Y"],
		dst = ["B","A","C","B","Y","X"],
		weight = [1,1,1,1,2,2]
	)

	# Solve on all nodes (in-PageRank), weighted, L1-normalized output
	res_all = pagerank_local_ora(edges; mode=:in, weighted=true)
	sum(res_all.scores) ≈ 1.0

	# Solve only on the {A,B,C} subset
	res_sub = pagerank_local_ora(edges; nodes=["A","B","C"], mode=:in)

	# With a custom teleport on the subset (must match subset length)
	p_sub = [0.2, 0.5, 0.3]
	res_sub_p = pagerank_local_ora(edges; nodes=["A","B","C"], personalization=p_sub)
	See Also
	pagerank_local_ora_matrix (core solver), pagerank_stitched (component-wise stitching)
	""" pagerank_local_ora

#	Component Scaled Page Rank
	function pagerank_stitched(
		edges::DataFrame;
		alpha::Float64 = 0.85,
		tol::Float64 = 1e-6,
		maxiter::Int = 1000,
		mode::Symbol = :in, # :in or :out
		final_norm::Symbol = :L1, # :L1 or :sup
		weighted::Bool = true,
		agg_func::Union{Function,Nothing} = nothing,
		stitch_by::Symbol = :nodes, # :nodes | :edges | :personalization
		personalization::Union{Nothing,AbstractVector{<:Real}} = nothing,
		rng::AbstractRNG = Random.default_rng() )
		"""
		Args:
			edges::DataFrame: Edge list with :src, :dst, and optional :weight
			alpha::Float64: Damping factor in (0,1) (default = 0.85)
			tol::Float64: Sup-norm convergence tolerance (default = 1e-6)
			maxiter::Int: Maximum number of iterations (default = 1000)
			mode::Symbol: :in (uses Aᵀ) or :out (uses A) before column normalization
			final_norm::Symbol: :L1 (sum to 1) or :sup (max = 1) final normalization
			weighted::Bool: Use edge weights if present (default = true)
			agg_func::Union{Function,Nothing}: Aggregation for parallel edges
				(default = sum if weighted, maximum if binary)
			stitch_by::Symbol: Component weighting rule:
				- :nodes → proportional to component size (|C_j| / N)
				- :edges → proportional to absolute edge mass within component
				- :personalization → proportional to global teleport mass p_i
			personalization::Union{Nothing,AbstractVector{<:Real}}:
				Global teleport vector of length N (L1-normalized internally);
				required if stitch_by = :personalization
			rng::AbstractRNG: Random number generator for reproducible starts
				(default = Random.default_rng())
		Returns:
			NamedTuple: (
				scores::Vector{Float64},
				node_names::Vector{String},
				converged::Bool,
				iterations_sum::Int,
				norm_used::Symbol,
				stitch_by::Symbol,
				component_weights::Dict{Int,Float64}
			)
		Notes:
			- Relies on helper functions `_aggregate_multi_edges` and `_edgelist_to_sparse_matrix`
			from this ecosystem.
			- ORA conventions within each component:
				* Absolute weights; self-loops removed
				* Column-stochastic transition matrix H
				* Sup-norm stabilized iteration with dangling handling
			- Stitching rules:
				* :nodes → component share by node count
				* :edges → component share by total absolute edge weight
				* :personalization → exact split by teleport mass, with per-component p normalization
		"""
		#	Basic Checks
			@assert 0.0 < alpha < 1.0 "alpha must be in (0,1)"
			@assert mode in (:in, :out)
			@assert final_norm in (:L1, :sup)
			@assert stitch_by in (:nodes, :edges, :personalization)
			if !(hasproperty(edges, :src) && hasproperty(edges, :dst))
    			throw(ArgumentError("edges must contain :src and :dst columns"))
			end
			if weighted && !hasproperty(edges, :weight)
				@warn "weighted=true but :weight column not found; falling back to binary."
				weighted = false
			end

		# 	Aggregation default
			if agg_func === nothing
				agg_func = (weighted && hasproperty(edges, :weight)) ? sum : maximum
			end

		#	Prepare adjacency via your helpers
			clean_edges = _aggregate_multi_edges(edges; agg_func=agg_func)
			use_weights = weighted && hasproperty(clean_edges, :weight)
			adj, node_to_idx, idx_to_node = _edgelist_to_sparse_matrix(clean_edges; weighted=use_weights)
			n = size(adj, 1)
			if n == 0
				return (scores=Float64[], node_names=String[], converged=true,
						iterations_sum=0, norm_used=final_norm,
						stitch_by=stitch_by, component_weights=Dict{Int,Float64}())
			end

		# 	Strip self-loops
			@inbounds for i in 1:n
				adj[i,i] = 0
			end
			dropzeros!(adj)

		# 	Weak components
			comps = _component_indices_weak(adj)
			node_names = [string(idx_to_node[i]) for i in 1:n]

		# 	Global personalization if needed
			p_global = nothing
			if stitch_by == :personalization
				personalization === nothing && throw(ArgumentError("Provide `personalization` when stitch_by=:personalization"))
				@assert length(personalization) == n "personalization length must equal #nodes"
				p_global = collect(float.(personalization))
				s = sum(p_global); @assert s > 0 "personalization must have positive sum"
				p_global ./= s
			end

		#	Component weights
			weights = Vector{Float64}(undef, length(comps))
			if stitch_by == :nodes
				@inbounds for (j, idx) in pairs(comps)
					weights[j] = length(idx) / n
				end
			elseif stitch_by == :edges
				masses = map(comps) do idx
					sub = adj[idx, idx]
					_, _, v = findnz(sub)
					sum(abs, v)
				end
				tot = sum(masses)
				if tot > 0
					weights .= masses ./ tot
				else
					@inbounds for (j, idx) in pairs(comps)
						weights[j] = length(idx) / n
					end
				end
			else
				#	Personalization mass per component
					@assert p_global !== nothing
					@inbounds for (j, idx) in pairs(comps)
						weights[j] = sum(p_global[idx])
					end
			end

		# 	Per-component solve + stitch
			scores = zeros(Float64, n)
			converged_all = true
			iter_sum = 0
			comp_weights = Dict{Int,Float64}()
			for (j, idx) in pairs(comps)
				p_local = if stitch_by == :personalization
					pj = p_global[idx]
					s = sum(pj)
					(s > 0) ? (pj ./ s) : fill(1.0 / length(idx), length(idx))
				else
					nothing
				end

				res = pagerank_local_ora_matrix(adj, idx; alpha=alpha, tol=tol, maxiter=maxiter,
												final_norm=:L1, mode=mode,
												personalization=p_local, rng=rng)

				scores[idx] .= weights[j] .* res.scores
				converged_all &= res.converged
				iter_sum += res.iterations
				comp_weights[j] = weights[j]
			end

		# 	Final cosmetic normalization
			if final_norm == :L1
				s = sum(scores); if s > 0; scores ./= s; end
			else
				m = maximum(scores); if m > 0; scores ./= m; end
			end

		#	Return Component Scaled Page Rank Scores
			return (scores=scores, node_names=node_names, converged=converged_all,
					iterations_sum=iter_sum, norm_used=final_norm,
					stitch_by=stitch_by, component_weights=comp_weights)
	end
	@doc raw"""
	**Description**
	Runs ORA-style PageRank **per weakly connected component** and stitches component
	score vectors into a single global vector using a chosen weighting rule
	(`:nodes`, `:edges`, or exact `:personalization`). Within each component:
	absolute weights are used, self-loops are removed, the transition matrix is
	column-stochastic, iterations are sup-norm stabilized, and dangling mass is handled.

	**Usage**
	`pagerank_stitched(edges::DataFrame;
	                   alpha=0.85, tol=1e-6, maxiter=1000,
	                   mode=:in, final_norm=:L1,
	                   weighted=true, agg_func=nothing,
	                   stitch_by=:nodes, personalization=nothing,
	                   rng=Random.default_rng())`

	**Arguments**
	- `edges::DataFrame`: Edge list with `:src`, `:dst`, and optional `:weight`.
	- `alpha::Float64`: Damping factor in `(0,1)`. Default `0.85`.
	- `tol::Float64`: Sup-norm convergence tolerance. Default `1e-6`.
	- `maxiter::Int`: Maximum number of iterations. Default `1000`.
	- `mode::Symbol`: `:in` (uses `A'` before column normalization) or `:out` (uses `A`).
	- `final_norm::Symbol`: Final cosmetic scaling — `:L1` (scores sum to 1) or `:sup` (max=1).
	- `weighted::Bool`: Use edge weights if present. Default `true`.
	- `agg_func`: Aggregation for parallel edges; default `sum` when `weighted=true`, else `maximum`.
	- `stitch_by::Symbol`: How to weight component contributions:
	  - `:nodes` → weight ∝ component size share (`|C_j| / N`).
	  - `:edges` → weight ∝ sum of absolute weights within the component.
	  - `:personalization` → exact split by the mass of a global teleport vector `p`
	    (**requires** `personalization`).
	- `personalization`: Global length-`N` teleport vector (L1-normalized internally) used only
	  when `stitch_by = :personalization`.
	- `rng`: RNG for reproducible starts. Default `Random.default_rng()`.

	**Details**
	Inside each component the solver is equivalent to `pagerank_local_ora`. The stitched
	global vector is a convex combination of component vectors using the rule specified
	by `stitch_by`. With `:personalization`, stitching implements the linear decomposition
	implied by the global teleport vector `p` (component mass equals the sum of `p` over
	the component; local solve uses `p` restricted to that component).

	**Value**
	A `NamedTuple` with:
	- `scores::Vector{Float64}` — Stitched PageRank scores in global node order.
	- `node_names::Vector{String}` — Node identifiers (from your edge list).
	- `converged::Bool` — `true` iff all component solves hit the tolerance.
	- `iterations_sum::Int` — Sum of per-component iteration counts.
	- `norm_used::Symbol` — `:L1` or `:sup` (final normalization applied).
	- `stitch_by::Symbol` — Stitching rule actually used.
	- `component_weights::Dict{Int,Float64}` — Component index → stitch weight.

	**Examples**
	```julia
	edges = DataFrame(
	    src = ["A","B","B","C","X","Y"],
	    dst = ["B","A","C","B","Y","X"],
	    weight = [1,1,1,1,2,2]
	)

	# Stitch by node share
	res_nodes = pagerank_stitched(edges; stitch_by=:nodes)
	sum(res_nodes.scores) ≈ 1.0  # final_norm=:L1

	# Stitch by component edge mass
	res_edges = pagerank_stitched(edges; stitch_by=:edges)

	# Exact personalization-based stitching
	p = fill(1.0 / length(res_nodes.node_names), length(res_nodes.node_names))
	res_pers = pagerank_stitched(edges; stitch_by=:personalization, personalization=p)
	```

	**See Also**
	`pagerank_local_ora` (component solver)
	""" pagerank_stitched

#   Hub & Authority Scores
	function salsa_centrality(edges::DataFrame;
	                          score::Symbol=:hub,
	                          weighted::Bool=false,
	                          include_self_loops::Bool=false,
	                          tol::Float64=1e-9,
	                          max_iter::Int=10_000,
	                          agg_func::Function=(weighted ? sum : maximum),
	                          init::Union{Nothing,AbstractVector{<:Real}}=nothing,
	                          normalize::Symbol=:l1)
		"""
		Args:
			edges::DataFrame: edge list with :src, :dst, optional :weight
			score::Symbol: :hub or :authority
			weighted::Bool: use weights if present (default = false, binary)
			include_self_loops::Bool: retain self-loops (default = false)
			tol::Float64: convergence tolerance on L1 norm (default = 1e-9)
			max_iter::Int: maximum iterations (default = 10_000)
			agg_func::Function: aggregate parallel edges (default = sum if weighted, maximum if binary)
			init::Union{Nothing,Vector}: initial distribution (default = uniform)
			normalize::Symbol: :l1 or :l2 normalization per iteration (default = :l1)
		Returns:
			DataFrame: columns [node, salsa_hub] or [node, salsa_authority]
		Notes:
			Implements SALSA centrality via sparse power method.
			Hub chain: M_h = D_out^{-1} * A * D_in^{-1} * A^T
			Auth chain: M_a = D_in^{-1} * A^T * D_out^{-1} * A
		"""
		#	Validation
			if !hasproperty(edges, :src) || !hasproperty(edges, :dst)
				throw(ArgumentError("edges must have :src and :dst columns"))
			end
			if !(score in (:hub, :authority))
				throw(ArgumentError("score must be :hub or :authority"))
			end

		#	Aggregate multi-edges (respecting weighted/binary mode)
			clean = _aggregate_multi_edges(edges; agg_func=agg_func)

		#	Build sparse adjacency (weighted iff requested and present)
			use_weights = weighted && hasproperty(clean, :weight)
			A, node_to_idx, idx_to_node = _edgelist_to_sparse_matrix(clean; weighted=use_weights)
			n = size(A, 1)

		#	Self-loops
			if !include_self_loops
				for i in 1:n
					A[i, i] = 0
				end
				dropzeros!(A)
			end

		#	Degrees (weighted if use_weights, else binary degrees)
			outdeg = Vector{Float64}(undef, n)
			indeg  = Vector{Float64}(undef, n)
			for i in 1:n
				outdeg[i] = sum(A[i, :])
				indeg[i]  = sum(A[:, i])
			end
			#	Guard zeros to avoid division by zero (dangling rows/cols)
			@inbounds for i in 1:n
				if outdeg[i] == 0.0; outdeg[i] = 1.0; end
				if indeg[i]  == 0.0; indeg[i]  = 1.0; end
			end

		#	Init vector
			x = isnothing(init) ? fill(1.0 / n, n) : Float64.(init)
			if length(x) != n
				throw(ArgumentError("init vector length $(length(x)) must equal number of nodes $n"))
			end

		#	Normalization helper
			normalize_vec!(v, how::Symbol) = begin
				if how === :l1
					s = sum(abs, v); if s > 0; v ./= s; end
				elseif how === :l2
					s = sqrt(sum(abs2, v)); if s > 0; v ./= s; end
				end
				v
			end
			normalize_vec!(x, normalize)

		#	Work buffers
			tmp1 = similar(x)
			tmp2 = similar(x)
			y    = similar(x)

		#	Power iteration: y = M * x (hub/authority matvec), normalize, test convergence
			for it in 1:max_iter
				if score === :hub
					#	y = D_out^{-1} * ( A * ( D_in^{-1} * ( Aᵀ * x ) ) )
						mul!(tmp1, transpose(A), x)						# tmp1 = Aᵀ * x
						@inbounds for i in 1:n; tmp1[i] /= indeg[i]; end	# tmp1 = D_in^{-1} * tmp1
						mul!(tmp2, A, tmp1)								# tmp2 = A * tmp1
						@inbounds for i in 1:n; y[i] = tmp2[i] / outdeg[i]; end
				else
					#	y = D_in^{-1} * ( Aᵀ * ( D_out^{-1} * ( A * x ) ) )
						mul!(tmp1, A, x)								# tmp1 = A * x
						@inbounds for i in 1:n; tmp1[i] /= outdeg[i]; end	# tmp1 = D_out^{-1} * tmp1
						mul!(tmp2, transpose(A), tmp1)					# tmp2 = Aᵀ * tmp1
						@inbounds for i in 1:n; y[i] = tmp2[i] / indeg[i]; end
				end

				normalize_vec!(y, normalize)

				#	convergence (L1)
				if sum(abs, y .- x) < tol
					x .= y
					break
				end
				x .= y
			end

		#	Result as DataFrame
			if score === :hub
				return DataFrame(node = idx_to_node, salsa_hub = x)
			else
				return DataFrame(node = idx_to_node, salsa_authority = x)
			end
	end
	@doc raw"""
	**Description**  
	SALSA centrality for directed networks using a scalable power method (no dense eigensolve).  
	Computes either **hub** or **authority** scores following Lempel & Moran (2001).

	- **Hub chain:** \(M_h = L_r L_c^\top\), with \(L_r = D_{\text{out}}^{-1}A\), \(L_c = A D_{\text{in}}^{-1}\).  
	Implemented as \(y = D_{\text{out}}^{-1}\,A\,D_{\text{in}}^{-1}\,A^\top x\).

	- **Authority chain:** \(M_a = L_c^\top L_r\).  
	Implemented as \(y = D_{\text{in}}^{-1}\,A^\top D_{\text{out}}^{-1}\,A\,x\).

	**Usage**  
	`salsa_centrality(edges; score=:hub, weighted=false, include_self_loops=false, tol=1e-9, max_iter=10_000, agg_func=..., init=nothing, normalize=:l1)`

	**Arguments**  
	- `edges::DataFrame`: `:src`, `:dst`, optional `:weight`.  
	- `score::Symbol`: `:hub` (default) or `:authority`.  
	- `weighted::Bool`: use weights if present; else binary.  
	- `include_self_loops::Bool`: keep i→i (default `false`).  
	- `tol`, `max_iter`: convergence controls.  
	- `agg_func::Function`: aggregate parallel edges (`sum` for weighted; `maximum` for binary).  
	- `init::Vector`: optional initial vector; defaults to uniform.  
	- `normalize::Symbol`: `:l1` or `:l2` normalization per iteration.

	**Details**  
	- Time per iteration \(O(m)\): two sparse multiplies and diagonal scaling.  
	- Zero in/out-degree rows/cols are guarded by setting the corresponding degree to 1 to avoid division by zero.  
	- Returns a probability-like vector if `normalize=:l1`.

	**Value**  
	`DataFrame` with columns `node` and `salsa_hub` (or `salsa_authority`).

	**References**  
	- Lempel, R., & Moran, S. (2001). *SALSA: The Stochastic Approach for Link-Structure Analysis.* ACM TOIS 19(2), 131–160.
	""" salsa_centrality

#	Helper Function: LogSumExp for numerical stability (Leiden)
	function logsumexp(x::Vector{Float64})
		"""
		Args:
			x::Vector{Float64}: log probabilities
		Returns:
			Float64: log(sum(exp(x)))
		Notes:
			Numerically stable computation of log-sum-exp.
			Prevents overflow/underflow in probability calculations.
		"""
		
		#	Handle empty vector
			if isempty(x)
				return -Inf
			end
		
		#	Extract maximum for stability
			max_x = maximum(x)
			if !isfinite(max_x)
				return max_x
			end
		
		#	Compute log-sum-exp with offset
			return max_x + log(sum(exp.(x .- max_x)))
	end


#	Helper Function: Calculate Modularity for Leiden, CHAMP, & Modularity Vitality Functions
	function calculate_modularity(adj::SparseMatrixCSC,
								membership::Vector{Int},
								resolution::Float64;
								min_group_size::Int = 5,
								filter_small_groups::Bool = true,
								loop_convention::Symbol = :as_is)
		"""
		Args:
			adj::SparseMatrixCSC{Float64,Int}: symmetric adjacency matrix
			membership::Vector{Int}: 1-based community labels aligned to adj rows (length = n)
			resolution::Float64: γ parameter (1.0 = Newman–Girvan)
			min_group_size::Int: groups with size < min_group_size are dropped before computing Q (default 5)
			filter_small_groups::Bool: if true, apply ORA-style node filtering (induced subgraph on kept groups)
			loop_convention::Symbol:
				:as_is  → use A as provided by getSparseA for example (Default)
				:igraph → double diagonal entries to match igraph's undirected self-loop convention

		Returns:
			Float64: modularity Q on the (possibly filtered) induced subgraph.

		Notes:
			- This implements the “identify small groups first → filter nodes → recompute Q” logic.
			- If all groups are filtered out, returns 0.0.
		"""

		#	Validation
			@assert issymmetric(adj) "calculate_modularity: A must be symmetric"
			n = size(adj, 1)
			@assert length(membership) == n "calculate_modularity: membership length mismatch"

		#	Optional: match igraph's loop convention (double diagonal)
			A = adj
			if loop_convention === :igraph
				d = diag(A)
				if any(d .!= 0.0)
					A = copy(A)
					for i in 1:n
						if d[i] != 0.0
							A[i, i] = 2.0 * d[i]
						end
					end
				end
			elseif loop_convention === :as_is
				# no-op
			else
				throw(ArgumentError("calculate_modularity: unknown loop_convention=$(loop_convention)"))
			end

		#	Identify nodes to keep (ORA-style): keep communities with size ≥ min_group_size
			keep_idx = collect(1:n)
			if filter_small_groups
				counts = Dict{Int,Int}()
				for c in membership
					counts[c] = get(counts, c, 0) + 1
				end
				keep_mask = [get(counts, membership[i], 0) >= min_group_size for i in 1:n]
				if !any(keep_mask)
					return 0.0
				end
				keep_idx = findall(keep_mask)
			end

		#	Induced subgraph & filtered membership
			Af = A[keep_idx, keep_idx]
			mf = membership[keep_idx]
			nf = length(mf)

		#	Remap labels to contiguous 1..C on the filtered set
			labels = sort(unique(mf))
			label_to_col = Dict{Int,Int}(lab => i for (i, lab) in enumerate(labels))
			mr = Vector{Int}(undef, nf)
			for i in 1:nf
				mr[i] = label_to_col[mf[i]]
			end
			C = length(labels)

		#	Mass and degrees on the filtered graph
			mass = sum(Af) / 2.0
			if mass == 0.0
				return 0.0
			end
			k = vec(sum(Af, dims=2))

		#	Indicator matrix on filtered graph
			S = sparse(collect(1:nf), mr, ones(Float64, nf), nf, C)

		#	Intra-community totals and null model expectations
			E_c = diag(S' * Af * S)
			K_c = S' * k
			Q_num = sum(E_c .- resolution .* (K_c .^ 2) ./ (2.0 * mass))

		#	Return modularity
			return Q_num / (2.0 * mass)
	end

#	Helper Function: Ensure Connectivity Within Communities
	function _refine_connectivity!(adj::SparseMatrixCSC, membership::Vector{Int})
		"""
		Args:
			adj::SparseMatrixCSC: symmetric adjacency matrix
			membership::Vector{Int}: community labels (modified in-place)
		Returns:
			Nothing (membership updated in-place)
		Notes:
			Splits disconnected communities into separate components.
			Ensures each community forms a connected subgraph.
		"""
		
		#	Build neighbor lists
			n = size(adj, 1)
			rows, cols, vals = findnz(adj)
			neighbors = [Int[] for _ in 1:n]
			
			for k in eachindex(vals)
				i, j = rows[k], cols[k]
				if i != j
					push!(neighbors[i], j)
					push!(neighbors[j], i)
				end
			end
		
		#	Process each community
			current_max = maximum(membership)
			comms = unique(membership)
			
			for c in comms
				nodes = findall(==(c), membership)
				if length(nodes) ≤ 1
					continue
				end
				
				#	BFS to find connected components
					unvisited = Set(nodes)
					first_component = true
					
					while !isempty(unvisited)
						start = first(unvisited)
						queue = [start]
						component = Int[]
						delete!(unvisited, start)
						
						while !isempty(queue)
							v = popfirst!(queue)
							push!(component, v)
							for nbr in neighbors[v]
								if nbr in unvisited && membership[nbr] == c
									delete!(unvisited, nbr)
									push!(queue, nbr)
								end
							end
						end
						
						#	Assign new label if not first component
							if !first_component
								current_max += 1
								for v in component
									membership[v] = current_max
								end
							end
							first_component = false
					end
			end
	end

#	Helper Function: Contract Graph by Community Structure
	function _contract_by_membership(adj::SparseMatrixCSC, membership::Vector{Int})
		"""
		Args:
			adj::SparseMatrixCSC: symmetric adjacency matrix
			membership::Vector{Int}: community labels for each node
		Returns:
			SparseMatrixCSC: contracted adjacency (communities as supernodes)
		Notes:
			Aggregates edges between communities.
			Self-loops represent internal community edges.
		"""
		
		#	Map communities to consecutive indices
			unique_comms = sort(unique(membership))
			label_map = Dict(old => new for (new, old) in enumerate(unique_comms))
			C = length(unique_comms)
		
		#	Aggregate inter-community edges
			rows, cols, vals = findnz(adj)
			I = Int[]
			J = Int[]
			V = Float64[]
			
			for k in eachindex(vals)
				ci = label_map[membership[rows[k]]]
				cj = label_map[membership[cols[k]]]
				push!(I, ci)
				push!(J, cj)
				push!(V, vals[k])
			end
		
		#	Build contracted adjacency
			S = sparse(I, J, V, C, C)
			S = max.(S, S')  # Ensure symmetry
		
		return S
	end

#	Helper Function: Single Leiden Run
	function _leiden_single_run(adj::SparseMatrixCSC,
	                            resolution::Float64,
	                            n_iterations::Int;
	                            seed::Union{Int,Nothing}=nothing)
		"""
		Args:
			adj::SparseMatrixCSC: original symmetric adjacency matrix
			resolution::Float64: resolution parameter γ
			n_iterations::Int: maximum iterations per run
			seed::Union{Int,Nothing}: random seed for this run
		Returns:
			NamedTuple: (membership, modularity, n_communities)
		Notes:
			Single Leiden run with local moves, refinement, and contraction.
			Preserves mapping to original nodes throughout multilevel hierarchy.
			Final modularity computed on original adjacency.
		"""
		
		#	Initialize run-specific RNG
			if seed !== nothing
				Random.seed!(seed)
			end
		
		#	Ensure symmetry and preserve original
			@assert issparse(adj) "_leiden_single_run: adj must be SparseMatrixCSC"
			adj = max.(adj, adj')
			adj_original = adj
		
		#	Track original node mapping
			n_original = size(adj, 1)
			orig_to_curr = collect(1:n_original)
		
		#	Initialize partition
			membership = collect(1:size(adj, 1))
			Q = calculate_modularity(adj, membership, resolution)
			iteration = 0
			improved = true
		
		#	Main Leiden loop
			while improved && iteration < n_iterations
				improved = false
				iteration += 1
				
				#	Build neighbor lists for current level
					n = size(adj, 1)
					rows, cols, vals = findnz(adj)
					neighbors = [Int[] for _ in 1:n]
					
					for k in eachindex(vals)
						i, j = rows[k], cols[k]
						if i != j
							push!(neighbors[i], j)
							push!(neighbors[j], i)
						end
					end
				
				#	Phase 1: Local moves
					node_order = randperm(n)
					
					for node in node_order
						current_comm = membership[node]
						
						#	Identify neighbor communities
							neighbor_comms = Set{Int}()
							for other in neighbors[node]
								push!(neighbor_comms, membership[other])
							end
						
						#	Evaluate best move
							best_comm = current_comm
							best_Q = Q
							
							for target in neighbor_comms
								if target == current_comm
									continue
								end
								
								#	Test move
									membership[node] = target
									new_Q = calculate_modularity(adj, membership, resolution)
									
									if new_Q > best_Q
										best_Q = new_Q
										best_comm = target
									end
							end
						
						#	Apply optimal move
							if best_comm != current_comm
								membership[node] = best_comm
								Q = best_Q
								improved = true
							else
								membership[node] = current_comm
							end
					end
				
				#	Phase 2: Connectivity refinement
					_refine_connectivity!(adj, membership)
				
				#	Phase 3: Graph contraction
					unique_comms = sort(unique(membership))
					label_map = Dict(old => new for (new, old) in enumerate(unique_comms))
					
					#	Update original-to-current mapping
						for i in 1:n_original
							orig_to_curr[i] = label_map[membership[orig_to_curr[i]]]
						end
					
					#	Contract and reset
						adj = _contract_by_membership(adj, membership)
						adj = max.(adj, adj')
						membership = collect(1:size(adj, 1))
						Q = calculate_modularity(adj, membership, resolution)
			end
		
		#	Map back to original nodes
			final_membership = [membership[orig_to_curr[i]] for i in 1:n_original]
			Q_final = calculate_modularity(adj_original, final_membership, resolution)
		
		#	Return Community Solution
			return (
				membership = final_membership,
				modularity = Q_final,
				n_communities = length(unique(final_membership))
			)
	end

#	Leiden Community Detection (Main Interface)
	function leiden_community_detection(edges::DataFrame;
	                                   resolution::Float64=1.0,
	                                   n_iterations::Int=10,
	                                   n_runs::Int=1,
	                                   weighted::Bool=false,
	                                   seed::Union{Int,Nothing}=nothing)
		"""
		Args:
			edges::DataFrame: edge list with :src, :dst, optional :weight
			resolution::Float64: resolution parameter γ (default = 1.0)
			n_iterations::Int: max iterations per run (default = 10)
			n_runs::Int: independent runs (default = 1)
			weighted::Bool: use edge weights (default = false)
			seed::Union{Int,Nothing}: base random seed
		Returns:
			NamedTuple: (membership, modularity, n_communities, node_names)
		Notes:
			Multi-run Leiden returning best modularity partition.
			Each run uses seed + run - 1 for reproducibility.
		"""
		
		#	Build adjacency matrix
			clean_edges = _aggregate_multi_edges(edges; agg_func=(weighted ? sum : maximum))
			use_weights = weighted && hasproperty(clean_edges, :weight)
			adj, node_to_idx, idx_to_node = _edgelist_to_sparse_matrix(clean_edges; weighted=use_weights)
			adj = max.(adj, adj')
		
		#	Track best result across runs
			best_membership = Vector{Int}()
			best_modularity = -Inf
			best_n_communities = 0
		
		#	Execute multiple runs
			for run in 1:n_runs
				seed_run = (seed === nothing) ? nothing : (seed + run - 1)
				res = _leiden_single_run(adj, resolution, n_iterations; seed=seed_run)
				
				if res.modularity > best_modularity
					best_modularity = res.modularity
					best_membership = res.membership
					best_n_communities = res.n_communities
				end
			end
		
		return (
			membership = best_membership,
			modularity = best_modularity,
			n_communities = best_n_communities,
			node_names = idx_to_node
		)
	end
	@doc raw"""
	**Description**
	Detects communities using the Leiden algorithm with guaranteed well-connected 
	communities through local moves, refinement, and multilevel optimization.

	**Usage**
	`leiden_community_detection(edges; resolution=1.0, n_iterations=10, n_runs=1, weighted=false, seed=nothing)`

	**Arguments**
	- `edges::DataFrame`: Edge list with `:src` and `:dst` columns, optionally `:weight`
	- `resolution::Float64`: Resolution parameter γ (default `1.0`)
	  - γ < 1.0: Larger communities
	  - γ = 1.0: Standard modularity
	  - γ > 1.0: Smaller communities
	- `n_iterations::Int`: Maximum iterations per run (default `10`)
	- `n_runs::Int`: Independent runs to perform (default `1`)
	- `weighted::Bool`: Use edge weights if present (default `false`)
	- `seed::Union{Int,Nothing}`: Random seed for reproducibility

	**Details**
	Three-phase optimization per iteration:
	1. **Local Moves**: Greedy node moves to neighboring communities
	2. **Refinement**: Ensures internal connectivity within communities
	3. **Contraction**: Hierarchical aggregation into supernodes
	
	Optimizes modularity:
```
	Q = (1/2m) Σ[A_ij - γ(k_i·k_j)/(2m)] δ(c_i, c_j)
```

	**Value**
	NamedTuple containing:
	- `membership::Vector{Int}`: Community assignments (1-based)
	- `modularity::Float64`: Best modularity score achieved
	- `n_communities::Int`: Number of communities detected
	- `node_names::Vector`: Original node identifiers

	**Examples**
```julia
	# Standard detection
	result = leiden_community_detection(edges)
	
	# Multiple runs for robustness
	result = leiden_community_detection(edges;
	                                   resolution=0.8,
	                                   n_runs=10,
	                                   weighted=true,
	                                   seed=42)

	**References**
	Traag VA, Waltman L, van Eck NJ (2019) Scientific Reports 9(1):5233
	""" leiden_community_detection

#	Helper Function: CHAMP Partition Coefficients
	function _calculate_partition_coefficients(adj::SparseMatrixCSC, membership::Vector{Int})
		"""
		Args:
			adj::SparseMatrixCSC: symmetric adjacency matrix
			membership::Vector{Int}: community assignments
		Returns:
			Tuple{Float64,Float64}: (A, P) coefficients
		Notes:
			A = internal edges (block-sum)
			P = expected edges under configuration model
			Optimized O(m + n) computation.
		"""
		
		#	Validation
			@assert issymmetric(adj) "CHAMP requires symmetric adjacency"
			@assert size(adj,1) == length(membership) "Membership length mismatch"
		
		#	Calculate edge totals
			two_m = sum(adj)
			if two_m == 0.0
				return (0.0, 0.0)
			end
			m = two_m / 2.0
			degrees = vec(sum(adj, dims=2))
		
		#	A: Internal edges via sparse iteration
			A_sum = 0.0
			rows, cols, vals = findnz(adj)
			
			@inbounds for k in eachindex(vals)
				i, j, w = rows[k], cols[k], vals[k]
				if i == j
					A_sum += w  # Self-loops counted once
				elseif membership[i] == membership[j]
					A_sum += w  # Off-diagonals counted for both (i,j) and (j,i)
				end
			end
		
		#	P: Expected edges via community degrees
			P_sum = 0.0
			for c in unique(membership)
				nodes = findall(==(c), membership)
				K_c = sum(degrees[nodes])
				P_sum += (K_c * K_c) / (2.0 * m)
			end
		
		return (A_sum, P_sum)
	end

#	CHAMP: Convex Hull of Admissible Modularity Partitions
	function champ_community_detection(edges::DataFrame;
	                                   resolution::Union{Float64,Nothing}=nothing,
	                                   resolution_range::Tuple{Float64,Float64}=(0.5, 1.8),
	                                   n_resolutions::Int=20,
	                                   weighted::Bool=false,
	                                   agg_func::Union{Function,Nothing}=nothing,
	                                   n_runs_per_gamma::Int=5,
	                                   n_iterations_per_run::Int=10,
	                                   seed::Union{Int,Nothing}=nothing,
	                                   show_progress::Bool=true)
		"""
		Args:
			edges::DataFrame: edge list with :src, :dst, optional :weight
			resolution::Union{Float64,Nothing}: single γ or nothing for sweep
			resolution_range::Tuple: γ range for sweep (default = (0.5, 1.8))
			n_resolutions::Int: number of γ values in sweep (default = 20)
			weighted::Bool: use edge weights (default = false)
			agg_func::Union{Function,Nothing}: edge aggregation function
			n_runs_per_gamma::Int: Leiden runs per γ (default = 5)
			n_iterations_per_run::Int: iterations per Leiden run (default = 10)
			seed::Union{Int,Nothing}: random seed
			show_progress::Bool: display progress bars (default = true)
		Returns:
			NamedTuple: (membership, resolution_used, modularity, n_communities, node_names)
		Notes:
			Identifies optimal partition via CHAMP dominance analysis.
			Linear coefficients (A,P) define modularity planes Q(γ) = (A - γP)/2m.
		"""
		
		#	Validation
			@assert hasproperty(edges, :src) && hasproperty(edges, :dst) "Missing required columns"
			if nrow(edges) == 0
				return (membership=Int[], resolution_used=0.0, modularity=0.0, 
				       n_communities=0, node_names=String[])
			end
			if seed !== nothing
				Random.seed!(seed)
			end
		
		#	Set aggregation strategy
			if isnothing(agg_func)
				agg_func = (weighted && hasproperty(edges, :weight)) ? sum : maximum
			end
		
		#	Define resolution grid
			gammas = if resolution === nothing
				collect(range(resolution_range[1], resolution_range[2]; length=n_resolutions))
			else
				[resolution]
			end
		
		#	Build adjacency matrix once
			clean_edges = _aggregate_multi_edges(edges; agg_func=agg_func)
			use_weights = weighted && hasproperty(clean_edges, :weight)
			adj, node_to_idx, idx_to_node = _edgelist_to_sparse_matrix(clean_edges; weighted=use_weights)
			adj = max.(adj, adj')
			node_names = [idx_to_node[i] for i in 1:size(adj,1)]
		
		#	Storage for partitions
			all_partitions = Vector{NamedTuple}(undef, length(gammas))
			all_coefficients = Vector{Vector{Float64}}(undef, length(gammas))
		
		#	Phase 1: Resolution sweep
			if show_progress
				try; ProgressMeter.ijulia_behavior(:append); catch; end
				@showprogress "CHAMP γ sweep" for (ix, γ) in enumerate(gammas)
					#	Run Leiden at this resolution
						res = leiden_community_detection(
							clean_edges;
							resolution = γ,
							n_iterations = n_iterations_per_run,
							n_runs = n_runs_per_gamma,
							weighted = use_weights,
							seed = seed
						)
					
					#	Calculate CHAMP coefficients
						A, P = _calculate_partition_coefficients(adj, res.membership)
					
					#	Store results
						all_partitions[ix] = (
							membership = res.membership,
							gamma = γ,
							modularity = res.modularity,
							n_communities = res.n_communities,
							A = A,
							P = P
						)
						all_coefficients[ix] = [A, P]
				end
			else
				for (ix, γ) in enumerate(gammas)
					res = leiden_community_detection(
						clean_edges;
						resolution = γ,
						n_iterations = n_iterations_per_run,
						n_runs = n_runs_per_gamma,
						weighted = use_weights,
						seed = seed
					)
					A, P = _calculate_partition_coefficients(adj, res.membership)
					all_partitions[ix] = (
						membership = res.membership,
						gamma = γ,
						modularity = res.modularity,
						n_communities = res.n_communities,
						A = A,
						P = P
					)
					all_coefficients[ix] = [A, P]
				end
			end
		
		#	Phase 2: Dominance analysis
			best_partition = all_partitions[1]
			
			if length(gammas) > 1
				n_partitions = length(all_partitions)
				dominant = trues(n_partitions)
				
				#	Check dominance relationships
					if show_progress
						@showprogress "CHAMP dominance scan" for i in 1:n_partitions
							A_i, P_i = all_coefficients[i]
							
							for j in 1:n_partitions
								i == j && continue
								A_j, P_j = all_coefficients[j]
								
								#	Test dominance
									if !isapprox(P_i, P_j; atol=1e-12)
										#	Find crossing point
											γ_cross = (A_j - A_i) / (P_i - P_j + 1e-12)
											
											if minimum(gammas) - 1e-6 < γ_cross < maximum(gammas) + 1e-6
												#	Test at midpoint
													γ_test = clamp((γ_cross + all_partitions[i].gamma)/2, 
													              minimum(gammas), maximum(gammas))
													              
													if (A_j - γ_test*P_j) > (A_i - γ_test*P_i) + 1e-12
														dominant[i] = false
														break
													end
											elseif (A_j - all_partitions[i].gamma*P_j) > 
											       (A_i - all_partitions[i].gamma*P_i) + 1e-12
												dominant[i] = false
												break
											end
									else
										#	Same P: compare A directly
											if A_j > A_i + 1e-12
												dominant[i] = false
												break
											end
									end
							end
						end
					else
						for i in 1:n_partitions
							A_i, P_i = all_coefficients[i]
							for j in 1:n_partitions
								i == j && continue
								A_j, P_j = all_coefficients[j]
								
								if !isapprox(P_i, P_j; atol=1e-12)
									γ_cross = (A_j - A_i) / (P_i - P_j + 1e-12)
									if minimum(gammas) - 1e-6 < γ_cross < maximum(gammas) + 1e-6
										γ_test = clamp((γ_cross + all_partitions[i].gamma)/2,
										              minimum(gammas), maximum(gammas))
										if (A_j - γ_test*P_j) > (A_i - γ_test*P_i) + 1e-12
											dominant[i] = false
											break
										end
									elseif (A_j - all_partitions[i].gamma*P_j) > 
									       (A_i - all_partitions[i].gamma*P_i) + 1e-12
										dominant[i] = false
										break
									end
								else
									if A_j > A_i + 1e-12
										dominant[i] = false
										break
									end
								end
							end
						end
					end
				
				#	Select best among dominant partitions
					if any(dominant)
						best_idx = findfirst(dominant)
						best_score = -Inf
						
						for (idx, is_dominant) in enumerate(dominant)
							is_dominant || continue
							part = all_partitions[idx]
							score = part.A - part.gamma * part.P
							
							if score > best_score
								best_score = score
								best_idx = idx
							end
						end
						
						best_partition = all_partitions[best_idx]
					else
						#	Fallback: highest modularity
							best_idx = argmax(getfield.(all_partitions, :modularity))
							best_partition = all_partitions[best_idx]
					end
			end
		
		#	Return optimal partition
			return (
				membership = best_partition.membership,
				resolution_used = best_partition.gamma,
				modularity = best_partition.modularity,
				n_communities = best_partition.n_communities,
				node_names = node_names
			)
	end
	@doc raw"""
	**Description**
	Implements CHAMP (Convex Hull of Admissible Modularity Partitions) to identify 
	optimal community structure across resolution parameters.

	**Usage**
	`champ_community_detection(edges; resolution=nothing, resolution_range=(0.5,1.8), 
	                          n_resolutions=20, weighted=false, n_runs_per_gamma=5, 
	                          n_iterations_per_run=10, seed=nothing, show_progress=true)`

	**Arguments**
	- `edges::DataFrame`: Edge list with `:src`, `:dst`, optional `:weight`
	- `resolution::Float64|nothing`: Single γ or `nothing` for sweep
	- `resolution_range::Tuple`: Range [min, max] for γ sweep (default `(0.5, 1.8)`)
	- `n_resolutions::Int`: Number of γ values in sweep (default `20`)
	- `weighted::Bool`: Use edge weights (default `false`)
	- `agg_func::Function`: Edge aggregation (default `sum` if weighted)
	- `n_runs_per_gamma::Int`: Leiden runs per γ (default `5`)
	- `n_iterations_per_run::Int`: Iterations per run (default `10`)
	- `seed::Int`: Random seed for reproducibility
	- `show_progress::Bool`: Display progress bars (default `true`)

	**Details**
	CHAMP identifies non-dominated partitions on the convex hull of modularity 
	optimization. The algorithm:
	
	1. **Sweep**: Run Leiden at multiple resolutions
	2. **Coefficients**: Calculate (A,P) defining Q(γ) = (A - γP)/2m
	3. **Dominance**: Find non-dominated partitions
	4. **Selection**: Return partition with highest quality score
	
	A partition dominates another if it achieves higher modularity for all γ 
	in the search range.

	**Value**
	NamedTuple containing:
	- `membership::Vector{Int}`: Optimal community assignments
	- `resolution_used::Float64`: Selected resolution parameter
	- `modularity::Float64`: Final modularity score
	- `n_communities::Int`: Number of communities
	- `node_names::Vector`: Original node identifiers

	**Examples**
```julia
	# Full parameter sweep
	result = champ_community_detection(edges;
	                                   resolution_range=(0.3, 2.0),
	                                   n_resolutions=30,
	                                   n_runs_per_gamma=10,
	                                   weighted=true)
	
	# Single resolution
	result = champ_community_detection(edges;
	                                   resolution=1.0,
	                                   n_runs_per_gamma=20)
	
	println("Optimal resolution: γ = $(result.resolution_used)")
	println("Communities: $(result.n_communities)")
	println("Modularity: Q = $(round(result.modularity, digits=4))")

	**References**
	1. Weir WH et al. (2017) Algorithms 10(3):93. doi:10.3390/a10030093
	2. github.com/wweir827/CHAMP
	""" champ_community_detection


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

#	Helper Function for modularity_vitality: getSparseA(edges) → A
	function getSparseA(edges::DataFrame;
						nodes::Union{Nothing,DataFrame,AbstractVector{<:AbstractString}}=nothing,
						weighted::Bool=true,
						test_flag::Bool=false,
						sentinel_node::AbstractString="828033366712688640",  # MyriadCsPhantom (ID)
						selfloop_node::AbstractString="INDOPACOM")
		"""
		Args:
			edges::DataFrame
				Required columns: :src, :dst
				Optional column:  :weight
				src/dst treated as String IDs (supports long IDs)

			nodes::Union{Nothing,DataFrame,Vector{<:AbstractString}}
				Nothing  → infer nodes from edges (isolates excluded)
				DataFrame: columns :id and :label (both strings). Uses :id as fixed universe (includes isolates)
				Vector   : string vector of node IDs forming fixed universe (includes isolates)

			weighted::Bool
				If true and edges has :weight, use it; otherwise use ones.
				If false, ignore any :weight column and use ones.
				(Self-loop halving is applied in all cases.)

			test_flag::Bool
				When true, run dataset-specific debug checks (sentinel and self-loop probes)

			sentinel_node::AbstractString
				Node key for targeted checks (may be an ID or, when nodes DataFrame provided, a label)

			selfloop_node::AbstractString
				Node key for self-loop checks (may be an ID or, when nodes DataFrame provided, a label)

		Returns:
			SparseMatrixCSC{Float64,Int}

		Notes:
			Builds a symmetric adjacency matrix A matching the established convention:
			- Aggregate duplicate edges by sum
			- Halve self-loops before symmetrization
			- Symmetrize by addition (A = A + A')
			- Diagonal equals the *original* (pre-halving) loop weight
			General invariants (symmetry, shape, non-negativity, NaNs) always run.
			Dataset-specific checks only run when `test_flag=true`.
		"""

		#	Aggregate duplicates by sum (work on a copy to avoid mutating caller’s DF)
			clean_edges = _aggregate_multi_edges(edges; agg_func=sum)

		#	Ensure weight semantics per `weighted`
			if weighted && hasproperty(clean_edges, :weight)
				clean_edges.weight = Float64.(clean_edges.weight)
			else
				clean_edges.weight = ones(Float64, nrow(clean_edges))
			end

		#	Record original self-loop weights (before halving)
			self_mask = clean_edges.src .== clean_edges.dst
			original_self_loops = Dict{Any,Float64}()
			if any(self_mask)
				for i in findall(self_mask)
					original_self_loops[clean_edges.src[i]] = clean_edges.weight[i]
				end
			end
			if test_flag && !isempty(original_self_loops)
				if haskey(original_self_loops, selfloop_node)
					println("DEBUG getSparseA: $selfloop_node original self-loop weight = $(original_self_loops[selfloop_node])")
				end
			end

		#	Halve self-loops prior to symmetrization
			clean_edges.weight[self_mask] ./= 2.0

		#	Expected sum after halving (used to verify symmetry sum later)
			sum_before_symmetry = sum(clean_edges.weight)
			if test_flag
				println("DEBUG getSparseA: Sum of edge weights after halving = $sum_before_symmetry")
			end

		#	Build directed adjacency using the fixed-node helper (includes isolates when nodes provided)
			adj_dir, node_to_idx, idx_to_node = _graph_to_sparse_matrix(clean_edges; nodes=nodes, weighted=true)

		#	Resolver for external key → row index
		#	- If idx_to_node is DataFrame, try :id first, then :label (both String)
		#	- Else, fall back to node_to_idx (Dict)
			_resolve_index = let node_to_idx=node_to_idx, idx_to_node=idx_to_node
				key::AbstractString -> begin
					if idx_to_node isa DataFrame
						ndf = idx_to_node::DataFrame
						# 	Try ID match
							if hasproperty(ndf, :id)
								pos = findfirst(==(key), String.(ndf.id))
								if pos !== nothing
									return pos
								end
							end

						# 	Try label match
							if hasproperty(ndf, :label)
								pos = findfirst(==(key), String.(ndf.label))
								if pos !== nothing
									return pos
								end
							end
							return nothing
					else
						return haskey(node_to_idx, key) ? node_to_idx[key] : nothing
					end
				end
			end

		#	Optional: Sentinel checks (pre-symmetrization row/col sums)
			if test_flag
				myriad_idx = _resolve_index(sentinel_node)
				if myriad_idx !== nothing
					println("DEBUG getSparseA: Sentinel '$sentinel_node' mapped to index $myriad_idx")
					row_sum = sum(adj_dir[myriad_idx, :])
					col_sum = sum(adj_dir[:, myriad_idx])
					println("DEBUG getSparseA: Sentinel out-sum (row) = $row_sum")
					println("DEBUG getSparseA: Sentinel in-sum  (col) = $col_sum")
				else
					println("DEBUG getSparseA: Sentinel '$sentinel_node' not present in mapping")
				end
			end

		#	Symmetrize by addition
			A = adj_dir + adj_dir'

		#	Optional: Verify diagonal equals original self-loop for designated node (if resolvable)
			if test_flag
				sl_idx = _resolve_index(selfloop_node)
				if sl_idx !== nothing
					# 	If we have the original loop by ID, verify exactness
						diag_val = A[sl_idx, sl_idx]
						println("DEBUG getSparseA: $selfloop_node diagonal after symmetrization = $diag_val")
					
						if haskey(original_self_loops, selfloop_node)
							@assert abs(diag_val - original_self_loops[selfloop_node]) < 1e-10 "Diagonal should equal original self-loop weight for $selfloop_node"
						end
				end
			end

		#	Total sum after symmetrization must match 2 * sum_after_halving
			total_sum = sum(A)
			expected_sum = 2 * sum_before_symmetry
			if test_flag
				println("DEBUG getSparseA: Total sum of A = $total_sum")
				println("DEBUG getSparseA: Expected sum (2 * halved) = $expected_sum")
			end
			@assert abs(total_sum - expected_sum) < 1e-10 "Sum mismatch after symmetrization"

		#	Optional: Sentinel degree and a few neighbor probes (if resolvable & present)
			if test_flag
				myriad_idx = _resolve_index(sentinel_node)
				if myriad_idx !== nothing
					#	Performing Degree Check
					# 	If this dataset expects degree 3 for the sentinel, keep the check; otherwise comment/remove as needed
					# 	@assert abs(myriad_degree - 3.0) < 1e-10 "Sentinel '$sentinel_node' should have degree 3 in this test dataset"
						myriad_degree = sum(A[myriad_idx, :])
						println("DEBUG getSparseA: Sentinel '$sentinel_node' total degree in A = $myriad_degree")

					#	Checking Neighbor Degree Totals
						for nbr in ("INDOPACOM", "PACAF", "US7thFleet")
							j = _resolve_index(nbr)
							if j !== nothing
								println("DEBUG getSparseA: A[sentinel, $nbr] = $(A[myriad_idx, j])")
							end
						end
				end
			end

		#	General invariants
			@assert issymmetric(A) "getSparseA: adjacency matrix must be symmetric"
			@assert sum(A .< 0.0) == 0 "getSparseA: adjacency matrix must not contain negative weights"
			@assert size(A,1) == size(A,2) "getSparseA: adjacency matrix must be square"
			@assert !any(isnan, A.nzval) "getSparseA: adjacency matrix contains NaN values"

		#	Optional: Summary
			if test_flag
				println("DEBUG getSparseA: Matrix size = $(size(A))")
				println("DEBUG getSparseA: Number of non-zeros = $(nnz(A))")
			end

		#	Return Self-Loop/Symmetrized Adjacency Matrix
			return A, idx_to_node
	end

#	Helper Function for modularity_vitality: getGroupIndicator
	function getGroupIndicator(A::SparseMatrixCSC, node_index::AbstractDataFrame,
							partition::DataFrame;
							node_col::Symbol = :node,
							community_col::Symbol = :community,
							expected_sizes::Union{Nothing,Dict{Int,Int}} = Dict(0=>388, 2=>193, 5=>137, 16=>118),
							perform_sanity_checks::Bool = true,
							test_flag::Bool = false)
		"""
		Args:
			A::SparseMatrixCSC:
				Symmetric adjacency; used to determine n (= size(A,1)) and ensure square shape.

			node_index::DataFrame:
				Node index aligned to A’s rows (length n). Typically the `idx_to_node` DataFrame
				returned by `getSparseA(...)`, containing at least an ID column. By default this
				function expects a column named by `node_col` (default `:node`). If `node_index`
				instead has `:id`, it will be renamed on-the-fly to `node_col` for the join.

			partition::DataFrame:
				Arbitrary ordering of node-community assignments with columns:
				- `node_col` (external node ID; e.g., string ID)
				- `community_col` (original community label; can be non-contiguous like 0,2,5,16)

			node_col::Symbol:
				Column name for node IDs in both `node_index` and `partition` (default `:node`).

			community_col::Symbol:
				Column name for community labels in `partition` (default `:community`).

			expected_sizes::Union{Nothing,Dict{Int,Int}}:
				Optional map of **original labels** → expected counts. Checked only when `test_flag=true`.

			perform_sanity_checks::Bool:
				Run general validations (one-hot rows, no empty columns) on the returned indicator.

			test_flag::Bool:
				When true, additionally runs dataset-specific checks (e.g., `expected_sizes`).

		Returns:
			SparseMatrixCSC{Float64,Int}:
				Indicator matrix `S` of size n×C (one-hot per node). Original labels are remapped
				internally to contiguous columns 1..C.

		Notes:
			- Row order strictly follows `node_index` (which must match A’s row order).
			- Community labels from `partition` may be arbitrary (e.g., {0,2,5,16}) and are remapped
			to contiguous columns internally.
			- Dataset-specific size assertions (e.g., specific community counts) are executed **only**
			when `test_flag=true`.
		"""

		#	Validation
			n = size(A, 1)
			if size(A,1) != size(A,2)
				throw(ArgumentError("get_group_indicator: A must be square"))
			end
			if !(hasproperty(partition, node_col) && hasproperty(partition, community_col))
				throw(ArgumentError("get_group_indicator: partition must have columns $(node_col) and $(community_col)"))
			end
			if nrow(node_index) != n
				throw(ArgumentError("get_group_indicator: node_index has $(nrow(node_index)) rows but A implies n=$(n). Provide a node_index aligned to A’s rows."))
			end

		#	Ensure node_index has the join key named node_col (support :id → node_col)
			if !hasproperty(node_index, node_col) && hasproperty(node_index, :id)
				rename!(node_index, :id => node_col)
			end
			if !hasproperty(node_index, node_col)
				throw(ArgumentError("get_group_indicator: node_index must have a '$(node_col)' column (or an ':id' column to be renamed)"))
			end

		#	Join partition labels onto node_index (A’s order defines the rows)
			ni = deepcopy(node_index)   # avoid mutating caller’s frame
			leftjoin!(ni, partition, on=node_col)

		#	Extract aligned vectors
			if !hasproperty(ni, community_col)
				throw(ArgumentError("get_group_indicator: join did not produce column $(community_col). Check node IDs and join key."))
			end
			nodes = ni[!, node_col]
			comms = ni[!, community_col]

			if length(nodes) != n || length(comms) != n
				throw(ArgumentError("get_group_indicator: partition (after join) must have exactly n=$(n) rows (one per node in A)"))
			end

		#	Ensure every node has a community (isolates included)
			if any(ismissing, comms)
				missing_nodes = collect(nodes[ismissing.(comms)])
				throw(ArgumentError("get_group_indicator: missing community assignments for $(length(missing_nodes)) node(s) present in A (examples: $(first(missing_nodes, min(5, length(missing_nodes)))))"))
			end

		#	Normalize community type to Int (after confirming no missings)
			ni[!, community_col] = convert.(Int, ni[!, community_col])
			comms = ni[!, community_col]

		#	Remap community labels to contiguous 1..C
			labels = sort(unique(comms))
			C = length(labels)
			label_to_col = Dict{eltype(labels),Int}(lab => i for (i, lab) in enumerate(labels))

		#	Build membership vector m (1..C) aligned to node_index/A
			m = Vector{Int}(undef, n)
			for i in 1:n
				m[i] = label_to_col[comms[i]]
			end

		#	Construct one-hot indicator S
			vals = ones(Float64, n)
			S = sparse(collect(1:n), m, vals, n, C)

		#	Sanity checks (general + optional dataset-specific)
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

				#	Dataset-specific assertions (original label sizes), only when test_flag
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

#	Modularity Vitality
	function modularity_vitality(edges::DataFrame;
                             resolution_sweep::Bool=false,
                             resolution::Float64=1.0,
                             weighted::Bool=false,
                             n_resolutions::Int=15,
                             n_runs_per_gamma::Int=5,
                             seed::Union{Int,Nothing}=nothing,
                             provided_membership::Union{Nothing,DataFrame,Vector{Int},Dict}=nothing)
		"""
		Args:
			edges::DataFrame: edge list with :src, :dst, optional :weight
			resolution_sweep::Bool: run CHAMP sweep (true) or fixed-γ Leiden (false)
			resolution::Float64: single γ for Leiden when not sweeping (default = 1.0)
			weighted::Bool: use edge weights during community detection (default = false)
			n_resolutions::Int: number of γ values for CHAMP sweep (default = 15)
			n_runs_per_gamma::Int: Leiden runs per γ (default = 5)
			seed::Union{Int,Nothing}: random seed for reproducibility
			provided_membership::Union{Nothing,DataFrame,Vector{Int},Dict}: optional user partition
				• DataFrame: must contain columns [:node, :community], left-joined on node id
				• Vector{Int}: community labels aligned to canonical node order
				• Dict(node⇒label): order-agnostic node-community mapping
		Returns:
			NamedTuple: (results_df, resolution_used, modularity, n_communities)
		Notes:
			Implements Matt Magelinski’s modularity vitality pipeline with strict order alignment:
			1) Canonical node order is derived from edges (first-appearance order).
			2) Community labels (detected or provided) are remapped to that order.
			3) Baseline modularity q₀ is computed on getSparseA(edges), matching newMods().
			4) Node-removal modularities q₁ = newMods(edges, membership).
			5) Vitality v = q₀ − q₁:
				• Hubs: v > 0 (removal decreases modularity)
				• Bridges: v < 0 (removal increases modularity)
		"""
		#	Validation
			cols = propertynames(edges)
			@assert (:src in cols) && (:dst in cols) "modularity_vitality: edges must have :src and :dst columns"

		#	Canonical edge-based node order (must match newMods/getSparseA)
			edge_order = unique(vcat(edges.src, edges.dst))
			n = length(edge_order)
			if n == 0
				df = DataFrame(node=String[], modularity_vitality=Float64[],
							modularity_vitality_hub=Float64[], modularity_vitality_bridge=Float64[], community=Int[])
				return (results_df=df, resolution_used=resolution, modularity=0.0, n_communities=0)
			end

		#	Membership + q0
			membership = Int[]
			resolution_used = resolution
			n_communities   = 0
			q0 = 0.0

			if provided_membership === nothing
				#	Detect partition
					if resolution_sweep
						det = champ_community_detection(
							edges;
							resolution         = nothing,
							resolution_range   = (0.5, 1.8),
							n_resolutions      = n_resolutions,
							weighted           = weighted,
							n_runs_per_gamma   = n_runs_per_gamma,
							seed               = seed,
							show_progress      = true
						)
						dmap = Dict(det.node_names .=> det.membership)
						@assert all(haskey.(Ref(dmap), edge_order)) "modularity_vitality: detector missing nodes for remap"
						membership       = [Int(dmap[v]) for v in edge_order]
						resolution_used  = det.resolution_used
						n_communities    = maximum(membership)
					else
						det = leiden_community_detection(
							edges;
							resolution   = resolution,
							n_iterations = 10,
							n_runs       = n_runs_per_gamma,
							weighted     = weighted,
							seed         = seed
						)
						dmap = Dict(det.node_names .=> det.membership)
						@assert all(haskey.(Ref(dmap), edge_order)) "modularity_vitality: detector missing nodes for remap"
						membership       = [Int(dmap[v]) for v in edge_order]
						resolution_used  = resolution
						n_communities    = maximum(membership)
					end

				#	q₀ on Matt-faithful A
					A0 = getSparseA(edges)
					@assert issymmetric(A0) "getSparseA: adjacency must be symmetric"
					@assert size(A0, 1) == length(membership) "q₀ check: A–membership mismatch"
					q0 = calculate_modularity(A0, membership, resolution_used)
			else
				#	User-provided partition
					if provided_membership isa DataFrame
						colsyms = Symbol.(names(provided_membership))
						@assert (:node in colsyms) && (:community in colsyms) "provided_membership must contain :node and :community"
						node_df = DataFrame(node=edge_order)
						mapped  = leftjoin(node_df, provided_membership, on=:node)
						@assert !any(ismissing, mapped.community) "modularity_vitality: missing community labels after join"
						membership = Int.(mapped.community)
					elseif provided_membership isa Dict
						@assert all(haskey.(Ref(provided_membership), edge_order)) "modularity_vitality: provided_membership Dict missing nodes"
						membership = [Int(provided_membership[v]) for v in edge_order]
					else
						@assert length(provided_membership) == n "provided_membership length must equal node count"
						membership = Int.(provided_membership)
					end
					resolution_used = resolution
					n_communities   = maximum(membership)

					A0 = getSparseA(edges)
					@assert issymmetric(A0) "getSparseA: adjacency must be symmetric"
					q0 = calculate_modularity(A0, membership, resolution_used)
			end

		#	Sanity
			@assert length(membership) == n "modularity_vitality: membership length mismatch"
			@assert all(membership .>= 1) "modularity_vitality: community labels must be positive"

		#	Per-node Q(G\i)
			q1s = newMods(edges, membership)

		#	Vitality & hub/bridge
			v = q0 .- q1s
			ϵ = 1e-12
			hub_scores    = map(x -> x > ϵ  ? x  : 0.0, v)
			bridge_scores = map(x -> x < -ϵ ? -x : 0.0, v)

		#	Results (include raw modularity_vitality as 2nd column for Matt parity)
			df = DataFrame(
				node                       = edge_order,
				modularity_vitality        = v,
				modularity_vitality_hub    = hub_scores,
				modularity_vitality_bridge = bridge_scores,
				community                  = membership
			)
			df.total_vitality = abs.(v)
			sort!(df, :total_vitality, rev=true)
			select!(df, Not(:total_vitality))

		return (
			results_df      = df,
			resolution_used = resolution_used,
			modularity      = q0,
			n_communities   = n_communities
		)
	end
	@doc raw"""
	**Description**  
	Identifies hub and bridge nodes using modularity vitality, following Matt Magelinski’s
	reference implementation. The function detects communities (Leiden or CHAMP) unless a
	partition is provided, then computes per-node modularity vitality via `newMods(edges, membership)`.

	**Usage**  
	`modularity_vitality(edges; resolution_sweep=false, resolution=1.0, weighted=false,
						n_resolutions=15, n_runs_per_gamma=5, seed=nothing,
						provided_membership=nothing)`

	**Arguments**
	- `edges::DataFrame`: Edge list with `:src`, `:dst`, optional `:weight`
	- `resolution_sweep::Bool`: Use CHAMP sweep (true) or fixed-γ Leiden (false)
	- `resolution::Float64`: Resolution γ for Leiden when not sweeping (default `1.0`)
	- `weighted::Bool`: Use edge weights during detection (default `false`)
	- `n_resolutions::Int`: Number of γ values for CHAMP sweep (default `15`)
	- `n_runs_per_gamma::Int`: Leiden runs per γ (default `5`)
	- `seed::Int`: Random seed for reproducibility
	- `provided_membership::Union{Nothing,DataFrame,Vector{Int},Dict}`:
	Optional community mapping.  
	*If DataFrame, must contain columns `[:node, :community]` and is left-joined on node id to preserve order.*

	**Details**  
	- If communities are detected, the detector’s modularity (`part.modularity`) is used as `q0`.  
	- If a membership is provided, `q0` is computed as `calculate_modularity(getSparseA(edges), membership, γ)`.  
	- Per-node removal modularities `q1s` are computed by `newMods(edges, membership)`.  
	- Vitality is `v = q0 - q1s`.  
	- **Hubs**: `v > 0` (removal decreases modularity)  
	- **Bridges**: `v < 0` (removal increases modularity)

	**Value**  
	NamedTuple with:
	- `results_df::DataFrame`: `[node, modularity_vitality_hub, modularity_vitality_bridge, community]`
	- `resolution_used::Float64`
	- `modularity::Float64` (baseline `q0`)
	- `n_communities::Int`

	**References**  
	Magelinski T, Bartulovic M, Carley KM (2021). Measuring node contribution to community structure with modularity vitality. IEEE TNSE 8(1):707–723.  
	GitHub: github.com/tmagelinski/modularity_vitality
	""" modularity_vitality

#   CORE DECOMPOSITION (Considering Using ORA K-Core Decomposition Here)

#   In-Core Number

#   Out-Core Number

#   Combined Core Number

#   LOCAL REACH

#   2-Hop In-Reach (How Many Can Reach this Node in 2 Steps)

#   2-Hop Out-Reach (How Many Nodes Can this Node Reach in 2 Steps)

#   GRAPH-LEVEL FEATURES

#   Sample Triads

#   SCC Size Distribution (Largest & Second Largest)

#   Bow-Ties Fractions (In, Out, SCC)

#   GLOBAL MEASURES

#   Global Reciprocity
	function reciprocity(edges::DataFrame;
	                     weighted::Bool=false,
	                     agg_func::Union{Function,Nothing}=nothing,
	                     mode::Symbol=:arc_based,
	                     weighted_method::Symbol=:squartini)
		"""
		Args:
			edges::DataFrame: must contain :src, :dst, optionally :weight
			weighted::Bool: enables weighted reciprocity if :weight exists (default = false)
			agg_func::Union{Function,Nothing}: aggregation for parallel edges (default = sum for weighted, maximum for binary)
			mode::Symbol: :arc_based or :dyad_based (default = :arc_based)
			weighted_method::Symbol: for dyad_based weighted only - :squartini or :ora_mutual (default = :squartini)
		Returns:
			Float64: reciprocity value based on selected mode
		Notes:
			Arc-based counts directed edges, dyad-based counts unordered pairs.
			Weighted dyad methods differ in how they handle weight asymmetry.
		"""
		
		#	Validation
			if !hasproperty(edges, :src) || !hasproperty(edges, :dst)
				throw(ArgumentError("edges DataFrame must have src and dst columns"))
			end
			if !(mode in (:arc_based, :dyad_based))
				throw(ArgumentError("mode must be :arc_based or :dyad_based"))
			end
			if mode == :dyad_based && weighted && !(weighted_method in (:squartini, :ora_mutual))
				throw(ArgumentError("weighted_method must be :squartini or :ora_mutual for weighted dyad_based"))
			end
			
		#	Handle empty edge list
			if nrow(edges) == 0
				return 0.0
			end
			
		#	Set default aggregation function
			if isnothing(agg_func)
				agg_func = weighted ? sum : maximum
			end
			
		#	Aggregate multi-edges
			clean_edges = _aggregate_multi_edges(edges; agg_func=agg_func)
			
		#	Build adjacency matrix
			use_weights = weighted && hasproperty(clean_edges, :weight)
			adj, node_to_idx, idx_to_node = _edgelist_to_sparse_matrix(clean_edges; weighted=use_weights)
			n = size(adj, 1)
			
		#	Remove self-loops
			for i in 1:n
				adj[i, i] = 0
			end
			dropzeros!(adj)
			
		#	Calculate based on mode
			if mode == :arc_based
				#	Arc-based: fraction of directed edges with reverse
					rows, cols, vals = findnz(adj)
					
					if use_weights
						#	Weighted: sum(w_ij * I{w_ji>0}) / sum(w_ij)
							reciprocal_weight = 0.0
							total_weight = 0.0
							
							for idx in 1:length(rows)
								i, j, w = rows[idx], cols[idx], vals[idx]
								total_weight += w
								if adj[j, i] > 0
									reciprocal_weight += w
								end
							end
							
							numerator = reciprocal_weight
							denominator = total_weight
					else
						#	Binary: count arcs with reverse / total arcs
							reciprocal_arcs = 0
							
							for idx in 1:length(rows)
								i, j = rows[idx], cols[idx]
								if adj[j, i] > 0
									reciprocal_arcs += 1
								end
							end
							
							numerator = Float64(reciprocal_arcs)
							denominator = Float64(length(rows))
					end
					
			else  # mode == :dyad_based
				#	Dyad-based: fraction of connected dyads that are mutual
					if use_weights
						#	Weighted dyad-based with method selection
							if weighted_method == :squartini
								#	Squartini: sum of reciprocated weights / total weight
									rows, cols, vals = findnz(adj)
									total_reciprocated = 0.0
									total_weight = 0.0
									
									for idx in 1:length(rows)
										i, j, w = rows[idx], cols[idx], vals[idx]
										total_weight += w
										total_reciprocated += min(w, adj[j, i])
									end
									
									numerator = total_reciprocated
									denominator = total_weight
									
							else  # weighted_method == :ora_mutual
								#	ORA mutual: exact weight matching required
									exact_match_dyads = 0
									total_connected_dyads = 0
									
									for i in 1:n
										for j in (i+1):n
											w_ij = adj[i, j]
											w_ji = adj[j, i]
											
											if w_ij > 0 || w_ji > 0
												total_connected_dyads += 1
												if w_ij > 0 && w_ji > 0 && w_ij == w_ji
													exact_match_dyads += 1
												end
											end
										end
									end
									
									numerator = Float64(exact_match_dyads)
									denominator = Float64(total_connected_dyads)
							end
					else
						#	Binary: mutual dyads / connected dyads
							mutual_dyads = 0
							connected_dyads = 0
							
							for i in 1:n
								for j in (i+1):n
									has_ij = adj[i, j] > 0
									has_ji = adj[j, i] > 0
									if has_ij || has_ji
										connected_dyads += 1
										if has_ij && has_ji
											mutual_dyads += 1
										end
									end
								end
							end
							
							numerator = Float64(mutual_dyads)
							denominator = Float64(connected_dyads)
					end
			end
			
		#	Calculate final reciprocity
			if denominator == 0
				return 0.0
			end
			return numerator / denominator
	end
	@doc raw"""
	**Description**  
	Computes reciprocity for directed networks using arc-based or dyad-based approaches.

	**Usage**  
	`reciprocity(edges::DataFrame; weighted=false, agg_func=nothing, mode=:arc_based, weighted_method=:squartini)`

	**Arguments**
	- `edges::DataFrame`: must contain `:src` and `:dst` columns, optionally `:weight`
	- `weighted::Bool`: enables weighted reciprocity if `:weight` exists (default `false`)
	- `agg_func`: aggregation for parallel edges; defaults to `sum` (weighted) or `maximum` (binary)
	- `mode::Symbol`: `:arc_based` (default) or `:dyad_based`
	- `weighted_method::Symbol`: for weighted dyad_based only - `:squartini` (default) or `:ora_mutual`

	**Details**
	
	**Arc-based** (counts directed edges):
	- Binary: Fraction of directed edges that have their reverse edge
	- Weighted: Σ w_ij * I{w_ji>0} / Σ w_ij (weight of edges with reverse / total weight)

	**Dyad-based** (counts unordered pairs):
	- Binary: Fraction of connected dyads that are mutual
	- Weighted with Squartini: r = Σ_ij min(w_ij, w_ji) / Σ_ij w_ij
	- Weighted with ORA mutual: Fraction of connected dyads with exactly matching weights

	Self-loops are always excluded from calculations.

	**Value**
	A `Float64` between 0 and 1 representing reciprocity.

	**Examples**
```julia
	# Arc-based (default)
	edges = DataFrame(src=["A","B","C"], dst=["B","A","D"])
	rec = reciprocity(edges)  # 2/3 (2 of 3 arcs have reverse)
	
	# Dyad-based with weighted network
	wedges = DataFrame(src=["A","B","C"], dst=["B","A","D"], weight=[3,3,2])
	rec_sq = reciprocity(wedges; weighted=true, mode=:dyad_based)  # Squartini
	rec_ora = reciprocity(wedges; weighted=true, mode=:dyad_based, weighted_method=:ora_mutual)
```

	**References**
	- Squartini T et al. (2013). "Reciprocity of weighted networks." Scientific Reports 3:2729.
	- Wasserman S & Faust K (1994). Social Network Analysis: Methods and Applications.
	- Carley KM (2002). Summary of Key Network Measures. CMU/CASOS.
	""" reciprocity

#   Density

#   Degree Assortativity

#   Note: Modularity is Reported When Modularity Vitality Statistics Are Calculated

############################
#   FEATURE CONSTRUCTORS   #
############################


############################
#   COMPARISON FUNCTIONS   #
############################



#   Exporting Objects
    export adjusted_rand_index,
		   load_ora_xml,
		   in_degree,
		   out_degree,
		   total_degree,
		   degree_ratio,
		   freeman_degree_normalization,
		   local_clustering_coefficient,
		   global_clustering_coefficient,
           weighted_clustering_coefficient,
           directed_clustering_cg,
		   local_weighted_reciprocity,
		   pagerank_local_ora,
		   pagerank_stitched,
		   salsa_centrality,
		   calculate_modularity,
		   leiden_community_detection,
		   champ_community_detection,
		   modularity_vitality,
		   reciprocity
		   
end # module julia_env
