using Pkg
# tempdir = mktempdir()
# Pkg.activate(tempdir)
# Pkg.develop(PackageSpec(path = joinpath(@__DIR__, "..")))
# Pkg.add(["BenchmarkTools", "PkgBenchmark", "MatrixDepot"])
# Pkg.resolve()

using Finch
using BenchmarkTools
using MatrixDepot
using SparseArrays

include(joinpath(@__DIR__, "../apps/apps.jl"))

SUITE = BenchmarkGroup()

SUITE["compile"] = BenchmarkGroup()

code = """
using Finch
A = @fiber d(sl(e(0.0)))
B = @fiber d(sl(e(0.0)))
C = @fiber d(sl(e(0.0)))

@finch (C .= 0; @loop i j k C[j, i] += A[k, i] * B[k, i])
"""
cmd = pipeline(`$(Base.julia_cmd()) --project=$(Base.active_project()) --eval $code`, stdout = IOBuffer())

SUITE["compile"]["time_to_first_SpGeMM"] = @benchmarkable run(cmd)

let
    A = @fiber d(sl(e(0.0)))
    B = @fiber d(sl(e(0.0)))
    C = @fiber d(sl(e(0.0)))

    SUITE["compile"]["compile_SpGeMM"] = @benchmarkable begin   
        A, B, C = ($A, $B, $C)
        Finch.execute_code(:ex, typeof(Finch.@finch_program_instance (C .= 0; @loop i j k C[j, i] += A[k, i] * B[k, j])))
    end
end

SUITE["embed"] = BenchmarkGroup()

libembedbenchmarks_file = joinpath(@__DIR__, "libembedbenchmarks.so")
if isfile(libembedbenchmarks_file)
    Base.Libc.Libdl.dlopen(libembedbenchmarks_file)

    ccall((:benchmarks_initialize, "libembedbenchmarks.so"), Cvoid, ())

    function sample_spmv_tiny((), params::BenchmarkTools.Parameters)
        evals = params.evals
        sample_time = ccall((:benchmark_spmv_tiny, "libembedbenchmarks.so"), Clong, (Cint,), evals)
        time = max((sample_time / evals) - params.overhead, 0.001)
        gctime = memory = allocs = return_val = 0
        return time, gctime, memory, allocs, return_val
    end

    SUITE["embed"]["spmv_tiny"] = BenchmarkTools.Benchmark(sample_spmv_tiny, (), BenchmarkTools.Parameters())

    #TODO how to call this at the right time?
    #println(ccall((:benchmarks_finalize, "libembedbenchmarks.so"), Cvoid, ()))
end

SUITE["graphs"] = BenchmarkGroup()

SUITE["graphs"]["pagerank"] = BenchmarkGroup()
for mtx in ["SNAP/soc-Epinions1", "SNAP/soc-LiveJournal1"]
    SUITE["graphs"]["pagerank"][mtx] = @benchmarkable FinchApps.pagerank($(pattern!(fiber(SparseMatrixCSC(matrixdepot(mtx)))))) 
end

SUITE["graphs"]["bfs"] = BenchmarkGroup()
for mtx in ["SNAP/soc-Epinions1", "SNAP/soc-LiveJournal1"]
    SUITE["graphs"]["bfs"][mtx] = @benchmarkable FinchApps.bfs($(fiber(SparseMatrixCSC(matrixdepot(mtx))))) 
end

SUITE["graphs"]["bellmanford"] = BenchmarkGroup()
for mtx in ["Newman/netscience", "SNAP/roadNet-CA"]
    A = redefault!(fiber(SparseMatrixCSC(matrixdepot(mtx))), Inf)
    SUITE["graphs"]["bellmanford"][mtx] = @benchmarkable FinchApps.bellmanford($A)
end

SUITE["matrices"] = BenchmarkGroup()

SUITE["matrices"]["ATA_spgemm_inner"] = BenchmarkGroup()
for mtx in []#"SNAP/soc-Epinions1", "SNAP/soc-LiveJournal1"]
    A = fiber(permutedims(SparseMatrixCSC(matrixdepot(mtx))))
    SUITE["matrices"]["ATA_spgemm_inner"][mtx] = @benchmarkable FinchApps.spgemm_inner($A, $A) 
end

SUITE["matrices"]["ATA_spgemm_gustavson"] = BenchmarkGroup()
for mtx in ["SNAP/soc-Epinions1"]#], "SNAP/soc-LiveJournal1"]
    A = fiber(SparseMatrixCSC(matrixdepot(mtx)))
    SUITE["matrices"]["ATA_spgemm_gustavson"][mtx] = @benchmarkable FinchApps.spgemm_gustavson($A, $A) 
end

SUITE["matrices"]["ATA_spgemm_outer"] = BenchmarkGroup()
for mtx in ["SNAP/soc-Epinions1"]#, "SNAP/soc-LiveJournal1"]
    A = fiber(SparseMatrixCSC(matrixdepot(mtx)))
    SUITE["matrices"]["ATA_spgemm_outer"][mtx] = @benchmarkable FinchApps.spgemm_outer($A, $A) 
end

SUITE["indices"] = BenchmarkGroup()

function spmv32(A, x)
    y = @fiber d{Int32}(e(0.0))
    @finch (y .= 0; @loop i j y[i] += A[j, i] * x[j])
    return y
end

SUITE["indices"]["SpMV_32"] = BenchmarkGroup()
for mtx in ["SNAP/soc-Epinions1"]#, "SNAP/soc-LiveJournal1"]
    A = fiber(SparseMatrixCSC(matrixdepot(mtx)))
    A = copyto!(@fiber(d{Int32}(sl{Int32, Int32}(e(0.0)))), A)
    x = copyto!(@fiber(d{Int32}(e(0.0))), rand(size(A)[2]))
    SUITE["indices"]["SpMV_32"][mtx] = @benchmarkable spmv32($A, $x) 
end

function spmv64(A, x)
    y = @fiber d{Int64}(e(0.0))
    @finch (y .= 0; @loop i j y[i] += A[j, i] * x[j])
    return y
end

SUITE["indices"]["SpMV_64"] = BenchmarkGroup()
for mtx in ["SNAP/soc-Epinions1"]#, "SNAP/soc-LiveJournal1"]
    A = fiber(SparseMatrixCSC(matrixdepot(mtx)))
    A = copyto!(@fiber(d{Int64}(sl{Int64, Int64}(e(0.0)))), A)
    x = copyto!(@fiber(d{Int64}(e(0.0))), rand(size(A)[2]))
    SUITE["indices"]["SpMV_64"][mtx] = @benchmarkable spmv64($A, $x) 
end

foreach(((k, v),) -> BenchmarkTools.warmup(v), SUITE)