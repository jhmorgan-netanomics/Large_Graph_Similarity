module Large_Graph_Similarity
#   Packages
    using CSV
    using DataFrames
    using Dates
	using EzXML

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

#   In-Degree

#   Out-Degree

#   Total Degree

#   In/Out Degree Ratio

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
    export load_ora_xml
   

end # module julia_env
