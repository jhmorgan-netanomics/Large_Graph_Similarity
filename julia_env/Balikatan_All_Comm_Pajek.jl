#Creating a Pajek Network File for Triad Census Validation Tests
#Jonathan H. Morgan
#7 November 2025

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
using Large_Graph_Similarity

#################
#   FUNCTIONS   #
#################

#   sequential_network_maker
    function sequential_network_maker(person_i::Vector{Int64}, person_j::Vector{Int64},  
                                      tie_weight::Union{Vector{Int64}, Vector{Float64}, Nothing} = nothing)
         #  Creating a Node List 
            node_list = DataFrame(node_id = [1:1:length(sort(unique([person_i; person_j])));], 
                                  node_label = sort(unique([person_i; person_j])))

        #   Creating Sequential Edge IDs
            if (isnothing(tie_weight) == true)
                edge_list = DataFrame(Obs_ID = [1:1:length(person_i);], source = person_i, target = person_j)
            else
                edge_list = DataFrame(Obs_ID = [1:1:length(person_i);], source = person_i, target = person_j, 
                                      weight = tie_weight)
            end
            
            source_nodes = edge_list[:,(1:2)]
            DataFrames.rename!(source_nodes, :source => :node_label)
            DataFrames.leftjoin!(source_nodes, node_list, on=:node_label)
            DataFrames.rename!(source_nodes, ["Obs_ID", "source_label", "source_id"])
            source_nodes = source_nodes[:,[1,3,2]]
            source_nodes.source_id = convert.(Int64, source_nodes.source_id)
  
            target_nodes = edge_list[:,[1; 3:ncol(edge_list)]]
            DataFrames.rename!(target_nodes, :target => :node_label)
            DataFrames.leftjoin!(target_nodes, node_list, on=:node_label)
            cols_to_front = ["Obs_ID", "node_id", "node_label"]
            remaining_cols = setdiff(names(target_nodes), cols_to_front)
            target_nodes = target_nodes[:, [cols_to_front; remaining_cols]]
            rename!(target_nodes, names(target_nodes)[2] => "target_id", names(target_nodes)[3] => "target_label")
            target_nodes.target_id = convert.(Int64, target_nodes.target_id)

        #   Creating Sequential Edge List
            DataFrames.leftjoin!(source_nodes, target_nodes, on=:Obs_ID)
            source_nodes.target_id = convert.(Int64, source_nodes.target_id)
            source_nodes.target_label = convert.(Int64, source_nodes.target_label)
            if(length(names(source_nodes)) == 6)
                source_nodes.weight = convert.(Float64, source_nodes.weight)
            end

        #   Order by ID
            DataFrames.sort!(source_nodes, ["source_id", "target_id"])

        #   Returning node_list and source_nodes
            return node_list, source_nodes
    end

    function sequential_network_maker(person_i::Vector{BigInt}, person_j::Vector{BigInt},  
                                      tie_weight::Union{Vector{Int64}, Vector{Float64}, Nothing} = nothing)
        #   Creating a Node List 
            node_list = DataFrame(node_id = [1:1:length(sort(unique([person_i; person_j])));], 
                                  node_label = sort(unique([person_i; person_j])))

        #   Creating Sequential Edge IDs
            if (isnothing(tie_weight) == true)
                edge_list = DataFrame(Obs_ID = [1:1:length(person_i);], source = person_i, target = person_j)
            else
                edge_list = DataFrame(Obs_ID = [1:1:length(person_i);], source = person_i, target = person_j, 
                                      weight = tie_weight)
            end
            
            source_nodes = edge_list[:,(1:2)]
            DataFrames.rename!(source_nodes, :source => :node_label)
            DataFrames.leftjoin!(source_nodes, node_list, on=:node_label)
            DataFrames.rename!(source_nodes, ["Obs_ID", "source_label", "source_id"])
            source_nodes = source_nodes[:,[1,3,2]]
            source_nodes.source_id = convert.(Int64, source_nodes.source_id)
  
            target_nodes = edge_list[:,[1; 3:ncol(edge_list)]]
            DataFrames.rename!(target_nodes, :target => :node_label)
            DataFrames.leftjoin!(target_nodes, node_list, on=:node_label)
            cols_to_front = ["Obs_ID", "node_id", "node_label"]
            remaining_cols = setdiff(names(target_nodes), cols_to_front)
            target_nodes = target_nodes[:, [cols_to_front; remaining_cols]]
            rename!(target_nodes, names(target_nodes)[2] => "target_id", names(target_nodes)[3] => "target_label")
            target_nodes.target_id = convert.(Int64, target_nodes.target_id)

        #   Creating Sequential Edge List
            DataFrames.leftjoin!(source_nodes, target_nodes, on=:Obs_ID)
            source_nodes.target_id = convert.(Int64, source_nodes.target_id)
            source_nodes.target_label = convert.(BigInt, source_nodes.target_label)
            if(length(names(source_nodes)) == 6)
                source_nodes.weight = convert.(Float64, source_nodes.weight)
            end

        #   Order by ID
            DataFrames.sort!(source_nodes, ["source_id", "target_id"])

        #   Returning node_list and source_nodes
            return node_list, source_nodes
    end

#   PAJEK: write_net
    function write_net(tie_type::String, data_id::DataFrame, 
                   person_i::Vector{Int64}, person_j::Vector{Int64},
                   net_name::String, sort_simplify::Bool = true,
                   tie_weight::Union{Vector{Int64}, Vector{Float64}, Nothing} = nothing,
                   x_coord::Union{Vector{Float64}, Nothing} = nothing, 
                   y_coord::Union{Vector{Float64}, Nothing} = nothing, 
                   z_coord::Union{Vector{Float64}, Nothing} = nothing, 
                   node_color::Union{String, Nothing} = nothing, 
                   node_border::Union{String, Nothing} = nothing, 
                   tie_color::Union{String, Nothing} = nothing)

        #   Creating Meta Data
            if (lowercase(tie_type) == "edges")
                tie_type = "*Edges"
            else
                tie_type = "*Arcs"
            end
           
        #   Prepare coordinates
            x = isnothing(x_coord) ? fill("", nrow(data_id)) : string.(x_coord)
            y = isnothing(y_coord) ? fill("", nrow(data_id)) : string.(y_coord)
            z = isnothing(z_coord) ? fill("", nrow(data_id)) : string.(z_coord)

        #   Manage colors and uppercase first character
            node_color = isnothing(node_color) ? vec(fill("", 1, nrow(data_id))) : vec(fill("ic " * uppercasefirst(strip(node_color)), 1, nrow(data_id)))
            node_border = isnothing(node_border) ? vec(fill("", 1, nrow(data_id))) : vec(fill("bc " * uppercasefirst(strip(node_border)), 1, nrow(data_id)))
            tie_color = isnothing(tie_color) ? vec(fill("", 1, length(person_i))) : vec(fill("c " * uppercasefirst(strip(tie_color)), 1, length(person_i)))
    
        #   Prepare edge list and add default weight if not provided
            edge_list = DataFrame(sender = person_i, target = person_j, 
                                  weight = isnothing(tie_weight) ? fill(1.0, length(person_i)) : tie_weight)

        #   Conditional sorting and removing duplicates
            if sort_simplify
                #   First, sort the edge_list by the columns 'sender' and 'target'
                    edge_list = sort(edge_list, [:sender, :target])

                #   After sorting, remove any duplicate entries
                    edge_list = unique(edge_list)
            end    

        #   Pajek format assembly
            vertices = "*Vertices $(nrow(data_id))"
       
            nodelist_str = [string(data_id[i, 1], " \"", data_id[i, 2], "\" ", x[i], " ", y[i], " ", z[i], " ", node_color[i], " ", node_border[i]) for i in 1:nrow(data_id)]
            nodelist_str = [vertices; nodelist_str]

            edgelist_str = [string(edge_list[i, :sender], " ", edge_list[i, :target], " ", edge_list[i, :weight], " ", tie_color[i]) for i in 1:nrow(edge_list)]
            edgelist_str = [tie_type; edgelist_str]

        #   Write to file
            open(net_name * ".net", "w") do file
                write(file, join(nodelist_str, "\r\n"))
                write(file, "\r\n") 
                write(file, join(edgelist_str, "\r\n"))
            end
    end

###################
#   IMPORT DATA   #
###################

#   Loading Balikatan_2022_Processed
    import_directory = "/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data"
    ora_xml = "Balikatan_2022_Processed.xml"
    file_location = string(import_directory, "/", ora_xml)
    balikatan_2022 = load_ora_xml(file_location)

#   Isolating the Agent x Agent - All Communication Nodes
    agents = balikatan_2022.nodesets["Agent"]

#   Isolating the Agent x Agent - All Communication Network
    agent_agent_all_com = balikatan_2022.networks["Agent x Agent - All Communication"]

##############################
#   WRITING-OUT PAJEK FILE   #
##############################

#   Extracting Edges
    edgelist = agent_agent_all_com.edges

#   Creating Sequential Nodelist & Network 
    nodes, sequential_network = sequential_network_maker(parse.(Int64, edgelist.src), parse.(Int64, edgelist.dst), edgelist.weight)

#   Writing-Out Pajek File
    cd("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data")
    write_net("edges", nodes, sequential_network.source_id, sequential_network.target_id,
              "Balikatan_Agent_AllComm", true, sequential_network.weight)
