#   Building the Large_Graph_Similarity Executable
#   Jonathan H. Morgan, Ph.D.
#   25 November 2025

#   Activate Package
    cd("/mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity")
    using Pkg
    Pkg.activate(@__DIR__)
    Pkg.status()
    Pkg.resolve()
    Pkg.instantiate()

    ENV["JULIA_DEBUG"] = "PackageCompiler"
    ENV["JULIA_PKG_PRECOMPILE_AUTO"] = "0"

#   Loading Compiler
    using PackageCompiler

#   Check the Entry Point
    @info "Verifying module & entrypoint…"
    import Large_Graph_Similarity
    @assert isdefined(Large_Graph_Similarity, :julia_main) "Large_Graph_Similarity.julia_main is not defined"

#   Specify Parameters/Constants
    const PKGDIR          = normpath(@__DIR__)
    const APPDIR          = joinpath(@__DIR__, "build", "Large_Graph_Similarity_app")
    const PRECOMPILE_EXEC = joinpath(@__DIR__, "precompile_exec.jl")

    @info "Checking precompile script at $PRECOMPILE_EXEC"
    @assert isfile(PRECOMPILE_EXEC) "Missing precompile_exec.jl at $PRECOMPILE_EXEC"

#   Build the App
    create_app(
        PKGDIR,
        APPDIR;
        executables = ["large_graph_similarity" => "julia_main"],  # <-- entrypoint in Large_Graph_Similarity
        precompile_execution_file = PRECOMPILE_EXEC,
        incremental = false,
        force = true,
        filter_stdlibs = false,
        include_transitive_dependencies = true,
        include_preferences = true,
        cpu_target = "generic",
        sysimage_build_args = `--compile=all`
    )

#   Notices
    @info "Done. Run it like:"
    @info joinpath(APPDIR, "bin", "large_graph_similarity") * " --help"

    println("\n✅ Built app at: ", APPDIR)
    println("Binary:\n  ", joinpath(APPDIR, "bin", "large_graph_similarity"))
    println("\nTry:")
    println("  ", joinpath(APPDIR, "bin", "large_graph_similarity"), " --help")

# Platform-specific run instructions:
# Linux:
#   julia --project -e 'include("build_app_linux.jl")'
#   cd /mnt/d/Dropbox/Netanomics_Resources/Documents/SBP_BRIMS_2025/Large_Graph_Similarity/julia_env/build/Large_Graph_Similarity_app/bin
#   ./large_graph_similarity --help