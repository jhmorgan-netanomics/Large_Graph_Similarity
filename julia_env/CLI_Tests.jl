#Test Script of Large_Graph_Similarity's Client Script
#Jonathan H. Morgan
#21-25 November 2025; revised 13 November 2026 for version 0.5.0

#   Pulling-In BEND_2022 & Activating Local Environment
    cd("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity")
    using Pkg
    Pkg.activate("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/julia_env")
    Pkg.status()

################
#   PACKAGES   #
################
using ArgParse
using CSV
using DataFrames
using Dates
using SparseArrays
using Test
using Large_Graph_Similarity 

#	The helper functions of the command-line interface are exercised here through the package itself
#	(Large_Graph_Similarity.CLI.cli_main). A copy of those helpers that stood in this file, from before
#	version 0.2.0, was removed: it wrote the raw and asinh comparison files, which no longer exist.
#	Since version 0.2.0 a comparison writes one score and these tables for the pair:
#	_comparison, _similarity_scores, _type_contributions, _node_measure_diagnostics, _edge_agreement,
#	_weight_scale, _comparison_info, and _undefined_features.

#######################
#   CI CLIENT TESTS   #
#######################

#	Test Configuration
	const TEST_DATA_DIR = "/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Test_Data"
	const OUTPUT_DIR = "/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/Example_Outputs"

#	Test 1: Documentation Lookup for network_comparator
	function test_documentation_lookup()
		"""
		Test the function help system
		"""
		
		println("\n" * "="^80)
		println("TEST 1: Documentation Lookup")
		println("="^80)
		
		#	Test listing all functions
			args = ["--list-functions"]
			println("Running: --list-functions")
			Large_Graph_Similarity.CLI.cli_main(args)
		
		#	Test getting help for network_comparator
			args = ["--function-help", "network_comparator"]  # Changed from --help
			println("\nRunning: --function-help network_comparator")
			Large_Graph_Similarity.CLI.cli_main(args)
		
		println("\nTest 1 completed successfully")
	end

    test_documentation_lookup()

#	Test 2: Single Network Analysis - Balikatan 2022
	function test_balikatan_analysis()
		"""
		Test single network analysis mode with Balikatan 2022
		"""
		
		println("\n" * "="^80)
		println("TEST 2: Balikatan 2022 Analysis")
		println("="^80)
		
		#	Construct CLI arguments (removed --function-help line)
			args = [
				"--mode", "analysis",
				"--ora-xml-1", joinpath(TEST_DATA_DIR, "Balikatan_2022_Processed.xml"),
				"--network-name-1", "Agent x Agent - All Communication",
				"--output-dir", joinpath(OUTPUT_DIR, "test2_balikatan"),
				"--name-1", "Balikatan_2022",
				"--directed", "true",
				"--weighted", "false",
				"--resolution", "1.0",
				"--n-runs", "3",
				"--n-iterations", "5",
				"--verbose", "true",
			]
		
		println("Analyzing Balikatan 2022 network...")
		println("Command arguments:")
		for i in 1:2:length(args)
			println("  $(args[i]) $(args[i+1])")
		end
		
		#	Run analysis
			Large_Graph_Similarity.CLI.cli_main(args)
		
		#	Verify output files exist
			output_files = [
				"Balikatan_2022_global_stats.csv",
				"Balikatan_2022_triad_census.csv",
				"Balikatan_2022_node_measures.csv",
				"Balikatan_2022_feature_vector.csv"
			]
		
			for file in output_files
				filepath = joinpath(OUTPUT_DIR, "test2_balikatan", file)
				if isfile(filepath)
					println("  ✓ Created: $file")
				else
					println("  ✗ Missing: $file")
					error("Expected output file not found: $file")
				end
			end
		
		println("\nTest 2 completed successfully")
	end

    test_balikatan_analysis()

#	Test 3: Network Comparison - Balikatan vs Pac Rim
	function test_network_comparison()
		"""
		Test network comparison mode with two metanetworks
		"""
        #   Delcare Tests
            println("\n" * "="^80)
            println("TEST 3: Network Comparison - Balikatan vs Pac Rim")
            println("="^80)
            
		#	Construct CLI arguments
			args = [
				"--mode", "comparison",
				"--ora-xml-1", joinpath(TEST_DATA_DIR, "Balikatan_2022_Processed.xml"),
				"--network-name-1", "Agent x Agent - All Communication",
				"--ora-xml-2", joinpath(TEST_DATA_DIR, "Pac Rim Day 1.xml"),
				"--network-name-2", "Agent x Agent - All Communication",
				"--output-dir", joinpath(OUTPUT_DIR, "test3_comparison"),
				"--name-1", "Balikatan_2022",
				"--name-2", "PacRim_Day1",
				"--directed", "true",
				"--weighted", "false",
				"--resolution", "1.0",
				"--n-runs", "3",
				"--n-iterations", "5",
				"--verbose", "true"
			]
		
            println("Comparing Balikatan 2022 vs Pac Rim Day 1...")
            println("Command arguments:")
            for i in 1:2:length(args)
                println("  $(args[i]) $(args[i+1])")
            end
		
		#	Run comparison
			Large_Graph_Similarity.CLI.cli_main(args)
		
		#	Verify output files exist
			expected_files = [
				#   Network 1 analysis
                    "Balikatan_2022_global_stats.csv",
                    "Balikatan_2022_node_measures.csv",
                    "Balikatan_2022_triad_census.csv",

				#   Network 2 analysis
                    "PacRim_Day1_global_stats.csv",
                    "PacRim_Day1_node_measures.csv",
                    "PacRim_Day1_triad_census.csv",

				#   Comparison results
                    "Balikatan_2022_PacRim_Day1_comparison.csv",
                    "Balikatan_2022_PacRim_Day1_similarity_scores.csv",
                    "Balikatan_2022_PacRim_Day1_type_contributions.csv",
                    "Balikatan_2022_PacRim_Day1_node_measure_diagnostics.csv",
                    "Balikatan_2022_PacRim_Day1_edge_agreement.csv",
                    "Balikatan_2022_PacRim_Day1_weight_scale.csv",
                    "Balikatan_2022_PacRim_Day1_comparison_info.csv",
                    "Balikatan_2022_PacRim_Day1_undefined_features.csv"
			]
		
			for file in expected_files
				filepath = joinpath(OUTPUT_DIR, "test3_comparison", file)
				@test isfile(filepath) 
				println("  ✓ Created: $file")
			end
		
        #   Printing Results
		    println("\nTest 3 completed successfully")
	end

    test_network_comparison()

#	Test 4: Load Community Partition from ORA
	function test_ora_leiden_partition()
		"""
		Test loading a pre-computed community partition from ORA XML
		"""
        #   Announcing Test
            println("\n" * "="^80)
            println("TEST 4: ORA Leiden Partition - Pac Rim Day 1")
            println("="^80)
		
		#	First, let's check what attributes are available
			println("Checking available attributes in Pac Rim Day 1...")
			
		#	Quick check using load_ora_xml directly to list attributes
			ora_file = joinpath(TEST_DATA_DIR, "Pac Rim Day 1.xml")
			pac_rim = load_ora_xml(ora_file)
			
			if haskey(pac_rim.nodesets, "Agent")
				agent_attrs = names(pac_rim.nodesets["Agent"])
				println("Available Agent attributes:")
				for attr in agent_attrs
					println("  - $attr")
				end
			end
		
		#	Construct CLI arguments with ora-leiden
			args = [
				"--mode", "analysis",
				"--ora-xml-1", joinpath(TEST_DATA_DIR, "Pac Rim Day 1.xml"),
				"--network-name-1", "Agent x Agent - All Communication",
				"--ora-leiden", "leiden group",  # Attempting to load this attribute
				"--output-dir", joinpath(OUTPUT_DIR, "test4_ora_leiden"),
				"--name-1", "PacRim_Leiden",
				"--directed", "true",
				"--weighted", "false",
				"--verbose", "true"
			]
		
            println("\nAttempting to load 'leiden group' partition...")
            println("Command arguments:")
            for i in 1:2:length(args)
                println("  $(args[i]) $(args[i+1])")
            end
		
		#	Run analysis with ORA partition
			try
				Large_Graph_Similarity.CLI.cli_main(args)
				println("  ✓ Successfully loaded ORA Leiden partition")
			catch e
				if isa(e, ArgumentError) && occursin("Attribute 'leiden group' not found", e.msg)
					println("  ! 'leiden group' attribute not found")
					println("  ! Error message will list available attributes")
					println(e.msg)
				else
					rethrow(e)
				end
			end
		
		println("\nTest 4 completed")
	end

    test_ora_leiden_partition()

#   Test 5: Load a Nodelist and Edgelist as CSVs
	function test_csv_input_balikatan()
		"""
		Test loading edge list, node list, and partition from CSV files
		and running a single-network analysis on Balikatan 2022.
		"""

		#   Announcing Test
			println("\n" * "="^80)
			println("TEST 5: CSV Input - Balikatan 2022 with Partition")
			println("="^80)

		#   Construct CLI arguments for CSV-based analysis
			edgelist_path   = joinpath(TEST_DATA_DIR, "Balikatan_2022_All_Communication_Arcs.csv")
			nodelist_path   = joinpath(TEST_DATA_DIR, "Balikatan_2022_All_Communication_Nodes.csv")
			partition_path  = joinpath(TEST_DATA_DIR, "All_Comm_Lieden_Group_Assignments.csv")
			output_path     = joinpath(OUTPUT_DIR, "test5_csv_input")

			args = [
				"--mode", "analysis",
				"--edgelist-1", edgelist_path,
				"--nodelist-1", nodelist_path,
				"--partition-1", partition_path,
				"--output-dir", output_path,
				"--name-1", "Balikatan_2022_CSV",
				"--directed", "true",
				"--weighted", "false",
				"--verbose", "true"
			]

			println("Running Balikatan 2022 analysis from CSV inputs...")
			println("Command arguments:")
			for i in 1:2:length(args)
				println("  $(args[i]) $(args[i+1])")
			end

		#   Run analysis via CLI
			Large_Graph_Similarity.CLI.cli_main(args)

		#   Verify output files exist
			expected_files = [
				"Balikatan_2022_CSV_global_stats.csv",
				"Balikatan_2022_CSV_triad_census.csv",
				"Balikatan_2022_CSV_node_measures.csv",
				"Balikatan_2022_CSV_feature_vector.csv"
			]

			for file in expected_files
				filepath = joinpath(output_path, file)
				@test isfile(filepath)
				println("  ✓ Created: $file")
			end

		println("\nTest 5 completed successfully")
	end

	test_csv_input_balikatan()

#   Test 6: Load a Nodelist and Edgelist as CSVs with a 0-Indexed Partition
	function test_csv_input_balikatan_Zero()
		"""
		Test loading edge list, node list, and partition from CSV files
		and running a single-network analysis on Balikatan 2022.
		"""

		#   Announcing Test
			println("\n" * "="^80)
			println("TEST 6: CSV Input - Balikatan 2022 with 0-Indexed Partition")
			println("="^80)

		#   Construct CLI arguments for CSV-based analysis
			edgelist_path   = joinpath(TEST_DATA_DIR, "Balikatan_2022_All_Communication_Arcs.csv")
			nodelist_path   = joinpath(TEST_DATA_DIR, "Balikatan_2022_All_Communication_Nodes.csv")
			partition_path  = joinpath(TEST_DATA_DIR, "balikatan_all_comm_partition_leiden_gamma1.csv")
			output_path     = joinpath(OUTPUT_DIR, "test6_csv_input")

			args = [
				"--mode", "analysis",
				"--edgelist-1", edgelist_path,
				"--nodelist-1", nodelist_path,
				"--partition-1", partition_path,
				"--output-dir", output_path,
				"--name-1", "Balikatan_2022_CSV",
				"--directed", "true",
				"--weighted", "false",
				"--verbose", "true"
			]

			println("Running Balikatan 2022 analysis from CSV inputs...")
			println("Command arguments:")
			for i in 1:2:length(args)
				println("  $(args[i]) $(args[i+1])")
			end

		#   Run analysis via CLI
			Large_Graph_Similarity.CLI.cli_main(args)

		#   Verify output files exist
			expected_files = [
				"Balikatan_2022_CSV_global_stats.csv",
				"Balikatan_2022_CSV_triad_census.csv",
				"Balikatan_2022_CSV_node_measures.csv",
				"Balikatan_2022_CSV_feature_vector.csv"
			]

			for file in expected_files
				filepath = joinpath(output_path, file)
				@test isfile(filepath)
				println("  ✓ Created: $file")
			end

		println("\nTest 6 completed successfully")
	end

	test_csv_input_balikatan_Zero()

#   Test 7: Load an Edgelist without a Nodelist 
	function test_csv_input_balikatan_edges_only()
		"""
		Test loading edge list from a CSV file
		"""

		#   Announcing Test
			println("\n" * "="^80)
			println("TEST 7: CSV Input - Balikatan 2022 Edges Only")
			println("="^80)

		#   Construct CLI arguments for CSV-based analysis
			edgelist_path   = joinpath(TEST_DATA_DIR, "Balikatan_2022_All_Communication_Arcs.csv")
			output_path     = joinpath(OUTPUT_DIR, "test7_csv_input")

			args = [
				"--mode", "analysis",
				"--edgelist-1", edgelist_path,
				"--output-dir", output_path,
				"--name-1", "Balikatan_2022_CSV",
				"--directed", "true",
				"--weighted", "false",
				"--verbose", "true"
			]

			println("Running Balikatan 2022 analysis from CSV inputs...")
			println("Command arguments:")
			for i in 1:2:length(args)
				println("  $(args[i]) $(args[i+1])")
			end

		#   Run analysis via CLI
			Large_Graph_Similarity.CLI.cli_main(args)

		#   Verify output files exist
			expected_files = [
				"Balikatan_2022_CSV_global_stats.csv",
				"Balikatan_2022_CSV_triad_census.csv",
				"Balikatan_2022_CSV_node_measures.csv",
				"Balikatan_2022_CSV_feature_vector.csv"
			]

			for file in expected_files
				filepath = joinpath(output_path, file)
				@test isfile(filepath)
				println("  ✓ Created: $file")
			end

		println("\nTest 7 completed successfully")
	end
	
	test_csv_input_balikatan_edges_only()

#   Test 8: Compare an Edgelist and ORA Network 
    function test_mixed_comparison_pacrim_ora_vs_balikatan_csv()
		"""
		Test mixed comparison:
		- Network 1: Pac Rim Day 1 ORA metanetwork using ORA Leiden partition
		- Network 2: Balikatan 2022 loaded from CSV edge/node/partition files
		"""

		#	Announce Test
			println("\n" * "="^80)
			println("TEST 8: Mixed Comparison – Pac Rim ORA (ORA Leiden) vs Balikatan CSVs")
			println("="^80)

		#	Define input paths
			pacrim_ora_path   = joinpath(TEST_DATA_DIR, "Pac Rim Day 1.xml")
			balikatan_edges   = joinpath(TEST_DATA_DIR, "Balikatan_2022_All_Communication_Arcs.csv")
			balikatan_nodes   = joinpath(TEST_DATA_DIR, "Balikatan_2022_All_Communication_Nodes.csv")
			balikatan_part    = joinpath(TEST_DATA_DIR, "All_Comm_Lieden_Group_Assignments.csv")

		#	Define output directory for this test
			test_output_dir = joinpath(OUTPUT_DIR, "test8_mixed_comparison")

		#	Construct CLI arguments
			args = [
				"--mode", "comparison",

				#	Network 1: Pac Rim ORA with ORA Leiden partition
				"--ora-xml-1", pacrim_ora_path,
				"--network-name-1", "Agent x Agent - All Communication",
				"--ora-leiden", "leiden group",
				"--name-1", "PacRim_Day1_Leiden",

				#	Network 2: Balikatan from CSVs
				"--edgelist-2", balikatan_edges,
				"--nodelist-2", balikatan_nodes,
				"--partition-2", balikatan_part,
				"--name-2", "Balikatan_2022",

				#	Common options
				"--output-dir", test_output_dir,
				"--directed", "true",
				"--weighted", "true",
				"--resolution", "1.0",
				"--n-runs", "3",
				"--n-iterations", "5",
				"--verbose", "true"
			]

			println("Running mixed comparison with arguments:")
			for i in 1:2:length(args)
				println("  $(args[i]) $(args[i+1])")
			end

		#	Run comparison via CLI
			Large_Graph_Similarity.CLI.cli_main(args)

		#	Expected output files
			name_1 = "PacRim_Day1_Leiden"
			name_2 = "Balikatan_2022"

			expected_files = [
				#	Network 1 analysis outputs
				"$(name_1)_global_stats.csv",
				"$(name_1)_node_measures.csv",
				"$(name_1)_triad_census.csv",

				#	Network 2 analysis outputs
				"$(name_2)_global_stats.csv",
				"$(name_2)_node_measures.csv",
				"$(name_2)_triad_census.csv",

				#	Comparison outputs (name_1_name_2 prefix)
				"$(name_1)_$(name_2)_comparison.csv",
				"$(name_1)_$(name_2)_similarity_scores.csv",
				"$(name_1)_$(name_2)_type_contributions.csv",
				"$(name_1)_$(name_2)_node_measure_diagnostics.csv",
				"$(name_1)_$(name_2)_edge_agreement.csv",
				"$(name_1)_$(name_2)_weight_scale.csv",
				"$(name_1)_$(name_2)_comparison_info.csv",
				"$(name_1)_$(name_2)_undefined_features.csv"
			]

		#	Check that all expected files exist
			println("\nVerifying expected output files in: $test_output_dir")
			for file in expected_files
				filepath = joinpath(test_output_dir, file)
				@test isfile(filepath)
				println("  ✓ Created: $file")
			end

			println("\nTest 8 completed successfully")
	end

	test_mixed_comparison_pacrim_ora_vs_balikatan_csv()

#	Test 9: Network Comparison - Balikatan vs Pac Rim Hashtags
	function test_network_comparison_hashtags()
		"""
		Test network comparison mode with two metanetworks
		"""
        #   Delcare Tests
            println("\n" * "="^80)
            println("TEST 9: Network Comparison - Balikatan vs Pac Rim Hashtag x Hashtag - Co-Occurrence")
            println("="^80)
            
		#	Construct CLI arguments
			args = [
				"--mode", "comparison",
				"--ora-xml-1", joinpath(TEST_DATA_DIR, "Balikatan_2022_Processed.xml"),
				"--network-name-1", "Hashtag x Hashtag - Co-Occurrence",
				"--ora-xml-2", joinpath(TEST_DATA_DIR, "Pac Rim Day 1.xml"),
				"--network-name-2", "Hashtag x Hashtag - Co-Occurrence",
				"--output-dir", joinpath(OUTPUT_DIR, "test9_comparison"),
				"--name-1", "Balikatan_2022_Hashtags",
				"--name-2", "PacRim_Day1_Hashtags",
				"--directed", "false",
				"--weighted", "true",
				"--resolution", "1.0",
				"--n-runs", "3",
				"--n-iterations", "5",
				"--verbose", "true"
			]
		
            println("Comparing Balikatan 2022 vs Pac Rim Day 1...")
            println("Command arguments:")
            for i in 1:2:length(args)
                println("  $(args[i]) $(args[i+1])")
            end
		
		#	Run comparison
			Large_Graph_Similarity.CLI.cli_main(args)
		
		#	Verify output files exist
			expected_files = [
				#   Network 1 analysis
                    "Balikatan_2022_Hashtags_global_stats.csv",
                    "Balikatan_2022_Hashtags_node_measures.csv",
                    "Balikatan_2022_Hashtags_triad_census.csv",

				#   Network 2 analysis
                    "PacRim_Day1_Hashtags_global_stats.csv",
                    "PacRim_Day1_Hashtags_node_measures.csv",
                    "PacRim_Day1_Hashtags_triad_census.csv",

				#   Comparison results
                    "Balikatan_2022_Hashtags_PacRim_Day1_Hashtags_comparison.csv",
                    "Balikatan_2022_Hashtags_PacRim_Day1_Hashtags_similarity_scores.csv",
                    "Balikatan_2022_Hashtags_PacRim_Day1_Hashtags_type_contributions.csv",
                    "Balikatan_2022_Hashtags_PacRim_Day1_Hashtags_node_measure_diagnostics.csv",
                    "Balikatan_2022_Hashtags_PacRim_Day1_Hashtags_edge_agreement.csv",
                    "Balikatan_2022_Hashtags_PacRim_Day1_Hashtags_weight_scale.csv",
                    "Balikatan_2022_Hashtags_PacRim_Day1_Hashtags_comparison_info.csv",
                    "Balikatan_2022_Hashtags_PacRim_Day1_Hashtags_undefined_features.csv"
			]
		
			for file in expected_files
				filepath = joinpath(OUTPUT_DIR, "test9_comparison", file)
				@test isfile(filepath) 
				println("  ✓ Created: $file")
			end
		
        #   Printing Results
		    println("\nTest 9 completed successfully")
	end

    test_network_comparison_hashtags()