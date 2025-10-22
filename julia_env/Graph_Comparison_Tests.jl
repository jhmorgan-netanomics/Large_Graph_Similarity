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

#####################
#   MEASURE TESTS   #
#####################




