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
    Pkg.precompile()

#   Load Packages
    using CSV
    using DataFrames
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

#	Local Clustering Coefficient: Watts DJ, Strogatz SH (1998)

#	Global Clustering Coefficient

#	Weighted Global Clustering Coefficient: Barrat et al. (2004)

#	Directed Weighted Clustering (Clemente & Grassi, 2018)

#   Local Reciprocity (Fraction of Reciprocated Edges)

#   COMPARISON TESTS


####################################################
#   MEASURE TESTS: INFLUENCE CENTRALITY MEASURES   #
####################################################