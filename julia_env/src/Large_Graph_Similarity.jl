module Large_Graph_Similarity
#   Packages
    using CSV
    using DataFrames
    using Dates
	using EzXML
	using LinearAlgebra
	using SparseArrays

#   UTLITIES

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

#	Helper Function for degree calculations: aggregate duplicate edges
	function _aggregate_multi_edges(edges::DataFrame; agg_func::Function=sum)
		"""
		Args:
			edges::DataFrame: DataFrame with src, dst, and optionally weight columns
			agg_func::Function: aggregation function for duplicate edges (default = sum)
		Returns:
			DataFrame: edges with duplicates aggregated
		Notes:
			Handles multi-graphs by aggregating parallel edges.
		"""
		
		#	Check if weights exist
			has_weights = hasproperty(edges, :weight)
		
		#	Group and aggregate
			if has_weights
				#	Aggregate weights for duplicate edges
					grouped = combine(groupby(edges, [:src, :dst]), 
					                 :weight => agg_func => :weight)
			else
				#	Count duplicate edges
					grouped = combine(groupby(edges, [:src, :dst]), 
					                 nrow => :weight)
			end
		
		#	Return aggregated edges
			return grouped
	end

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

#   IMPORT FUNCTIONS

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

#   MEASURES

#   Degree Measures 

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
	                      ignore_direction::Bool=false)
		"""
		Args:
			edges::DataFrame: edge list with src, dst, and optionally weight columns
			weighted::Bool: use edge weights if available (default = true)
			normalize::Bool: if true, returns Freeman-normalized total-degree via freeman_degree_normalization (default = false)
			agg_func::Function: function to aggregate multi-edges (default = sum)
			ignore_direction::Bool: if true, treat as undirected for the total metric
		Returns:
			DataFrame: columns [node, total_degree]
		Notes:
			When normalize=true: delegates to freeman_degree_normalization(...; mode=:all, directed = !ignore_direction).
			When normalize=false:
				- ignore_direction=false: total = in-degree + out-degree.
				- ignore_direction=true: symmetrizes edges (max(A,A')) and returns degree from the symmetric matrix.
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

		#	Normalized path: delegate to Freeman with mode=:all
			if normalize
				#	Here we control the symmetry interpretation via `directed`
					df = freeman_degree_normalization(clean_edges; mode=:all, directed = !ignore_direction, bipartite=false, weighted=weighted, agg_func=agg_func)
				#	Freeman returns :freeman_degree; rename to :total_degree
					rename!(df, :freeman_degree => :total_degree)
					return df
			end

		#	Unnormalized path (original behavior)
			if ignore_direction
				#	Symmetrize edge list
					edges_reversed = DataFrame(src = clean_edges.dst, dst = clean_edges.src)
					if hasproperty(clean_edges, :weight)
						edges_reversed.weight = clean_edges.weight
					end
					combined_edges = vcat(clean_edges, edges_reversed)
					clean_edges = _aggregate_multi_edges(combined_edges; agg_func=maximum)
			end

		#	Build adjacency
			adj, node_to_idx, idx_to_node = _edgelist_to_sparse_matrix(clean_edges; weighted=weighted)

		#	Compute totals
			if ignore_direction
				#	Degree on symmetric matrix counts each undirected edge once
					adj_sym = max.(adj, adj')
					total_deg_values = vec(sum(adj_sym, dims=1))
			else
				#	Directed: in + out
					in_deg = vec(sum(adj, dims=1))
					out_deg = vec(sum(adj, dims=2))
					total_deg_values = in_deg .+ out_deg
			end

		#	Assembling Result
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
			Freeman's normalization. Matches the Python reference behavior for symmetry,
			denominators, and (optionally) bipartite handling.
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
			diagonal = collect(diag(adj))                  # ensure a dense Vector{<:Real}

		#	Determine V (max edge weight if weighted; else 1.0)
			V = (weighted && hasproperty(clean_edges, :weight) && !isempty(clean_edges.weight)) ?
			    maximum(clean_edges.weight) : 1.0

		#	Symmetry detection (directed-but-symmetric allowed)
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

		#	Apply Freeman denominators by mode
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
				#	Denominator by symmetry
					denom = is_sym ? (V * (N - 1)) : (V * N)
			else
				#	In-degree numerator
					numerator .= col_sums
				#	Denominator by symmetry
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
	Compute Freeman-normalized degree centrality from an edge list using your sparse-matrix helpers. Supports weighted/unweighted, directed/undirected, and optional bipartite mode counts.

	**Usage**
	`freeman_degree_normalization(edges::DataFrame; mode::Symbol=:all, directed::Bool=true, bipartite::Bool=false, types::Union{Nothing,AbstractVector{Bool}}=nothing, weighted::Bool=true, agg_func::Function=sum, atol::Float64=1e-12)`

	**Arguments**
	- `edges::DataFrame`: Edge list with `:src`, `:dst`, and optional `:weight`.
	- `mode::Symbol`: One of `:all`, `:out`, or `:in` (default `:all`).
	- `directed::Bool`: Treat the network as directed (default `true`).
	- `bipartite::Bool`: If `true`, use bipartite mode counts; requires `types` (default `false`).
	- `types::Union{Nothing,AbstractVector{Bool}}`: Node-mode flags (aligned to node order; `true` = first mode).
	- `weighted::Bool`: Use edge weights if available (default `true`).
	- `agg_func::Function`: Aggregation for parallel edges (default `sum`).
	- `atol::Float64`: Tolerance for symmetry when `directed=true` (default `1e-12`).

	**Details**
	The function:
	1. Aggregates parallel edges (`agg_func`) and builds a sparse adjacency with `_edgelist_to_sparse_matrix`.
	2. Computes row sums (out-strength), column sums (in-strength), and diagonal (self-loops).
	3. Sets `V` to the maximum edge weight if weighted, else `1.0`.
	4. Detects symmetry. Undirected graphs are treated as symmetric; directed-but-symmetric is allowed.
	5. Determines `N` and `R`. If `bipartite=true`, `types` provides first/second mode counts; otherwise `N=R=n`.
	6. Applies Freeman denominators:
	- `:all`: numerator = out + in − self; symmetric → `V*(N−1)`, asymmetric → `2*V*(N−1)`.
	- `:out`: numerator = out; symmetric → `V*(N−1)`, asymmetric → `V*N`.
	- `:in`: numerator = in; symmetric → `V*(N−1)`, asymmetric → `V*N`.

	Edge cases: If `N ≤ 1` or the denominator is zero, returns zeros of appropriate length.

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

	#	Directed with weights
		edges_w = DataFrame(src=[1,1,2], dst=[2,3,3], weight=[2.0, 1.0, 1.5])
		scores_out = freeman_degree_normalization(edges_w; mode=:out, directed=true, weighted=true)
		@show scores_out

	#	Bipartite (types must align with node order from helper)
		# Suppose nodes appear in order [A,B,C,D,E] with types [true,true,true,false,false]
		edges_bi = DataFrame(src=["A","B","C","B"], dst=["D","D","E","E"])
		types = [true,true,true,false,false]
		scores_in = freeman_degree_normalization(edges_bi; mode=:in, directed=false, bipartite=true, types=types)
		@show scores_in

	**References**
	Freeman, L. C. (1978). Centrality in social networks conceptual clarification. Social Networks, 1(3), 215–239.
	""" freeman_degree_normalization

#   Local structure

#   Local Clustering Coefficient (Directed)

#   Local Reciprocity (Fraction of Reciprocated Edges)

#   Influence Centrality Measures

#   Component Scaled Page Rank

#   Hub Scores

#   Authority Scores

#   Core Decomposition (Considering Using ORA K-Core Decomposition Here)

#   In-Core Number

#   Out-Core Number

#   Combined Core Number

#   Local Reach

#   2-Hop In-Reach (How Many Can Reach this Node in 2 Steps)

#   2-Hop Out-Reach (How Many Nodes Can this Node Reach in 2 Steps)

#   Graph-Level Features

#   Sample Triads

#   Component structure

#   SCC Size Distribution (Largest & Second Largest)

#   Bow-Ties Fractions (In, Out, SCC)

#   Global Measures

#   Global Reciprocity

#   Density

#   Degree Assortativity

#   Modularity

#   FEATURE CONSTRUCTORS

#   COMPARISON FUNCTIONS


#   Exporting Objects
    export load_ora_xml,
		   in_degree,
		   out_degree,
		   total_degree,
		   degree_ratio,
		   freeman_degree_normalization

end # module julia_env
