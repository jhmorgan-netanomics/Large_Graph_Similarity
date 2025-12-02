#   Building the Large_Graph_Similarity Executable
#   Jonathan H. Morgan, Ph.D.
#   2 December 2025

#   Loading Packages
    using Pkg
    using PackageCompiler

#   Activate Package (relative to this script, not hard-coded)
    cd(@__DIR__)
    Pkg.activate(@__DIR__)
    Pkg.status()
    Pkg.resolve()
    Pkg.instantiate()

#   Environment for PackageCompiler
    ENV["JULIA_DEBUG"] = "PackageCompiler"
    ENV["JULIA_PKG_PRECOMPILE_AUTO"] = "0"

    @info "Verifying module & entrypoint…"
    import Large_Graph_Similarity
    @assert isdefined(Large_Graph_Similarity, :julia_main) "Large_Graph_Similarity.julia_main is not defined"

#   Paths / Constants
    const PKGDIR          = normpath(@__DIR__)
    const APPDIR          = joinpath(@__DIR__, "build", "Large_Graph_Similarity_app")
    const PRECOMPILE_EXEC = joinpath(@__DIR__, "precompile_exec.jl")

    @info "Checking precompile script at $PRECOMPILE_EXEC"
    @assert isfile(PRECOMPILE_EXEC) "Missing precompile_exec.jl at $PRECOMPILE_EXEC"

#   Executable name (Platform-aware)
    const EXE_BASENAME = "large_graph_similarity"
    const EXE_FILE = Sys.iswindows() ? EXE_BASENAME * ".exe" : EXE_BASENAME
    const EXE_PATH = joinpath(APPDIR, "bin", EXE_FILE)

#   Build the App
    create_app(
        PKGDIR,
        APPDIR;
        executables = [EXE_BASENAME => "julia_main"],  # entrypoint in Large_Graph_Similarity
        precompile_execution_file = PRECOMPILE_EXEC,
        incremental = false,
        force = true,
        filter_stdlibs = false,
        include_transitive_dependencies = true,
        include_preferences = true,
        cpu_target = "generic",
        sysimage_build_args = `--compile=all`
    )

#   Notices / Run Instructions
    @info "✅ Done. Built app at: $APPDIR"
    @info "Binary: $EXE_PATH"

    println("\n✅ Built app at: ", APPDIR)
    println("Binary:\n  ", EXE_PATH)
    println("\nTry:")

    if Sys.iswindows()
        # Windows command prompt / PowerShell
        println("  \"", EXE_PATH, "\" --help")
    else
        # Linux / macOS
        println("  ", EXE_PATH, " --help")
    end

# julia --project=. build_app_windows.jl
# build\Large_Graph_Similarity_app\bin\large_graph_similarity.exe
