include("simplerunlength.jl")
include("simplesparsevector.jl")
include("singlespike.jl")

@testset "simplevectors" begin
    println("run = run + run")
    A = SimpleRunLength{Float64, Int}([1, 3, 5, 7, 9, 10], [2.0, 3.0, 4.0, 5.0, 6.0, 7.0])
    B = SimpleRunLength{Float64, Int}([5, 8, 10], [1.0, 2.0, 3.0])
    C = SimpleRunLength{Float64, Int}([1, 10], [0.0])
    ex = @I @loop i C[i] += A[i] + B[i]

    display(lower_julia(virtualize(:ex, typeof(ex), LowerJuliaContext())))
    println()
    execute(ex)

    println(A)
    println(B)
    println(C)

    @test C.idx == [1, 3, 5, 7, 8, 9, 10]
    @test C.val == [3.0, 4.0, 5.0, 7.0, 8.0, 9.0, 10.0]
    println()

    println("dense = sparse + sparse")
    A = SimpleSparseVector{0.0, Float64, Int}([1, 3, 5, 7, 9, 11], [2.0, 3.0, 4.0, 5.0, 6.0])
    B = SimpleSparseVector{0.0, Float64, Int}([2, 5, 8, 11], [1.0, 1.0, 1.0])
    C = zeros(10)
    ex = @I @loop i C[i] += A[i] + B[i]

    display(lower_julia(virtualize(:ex, typeof(ex), LowerJuliaContext())))
    println()
    execute(ex)

    println(A)
    println(B)
    println(C)

    @test C == [2.0, 1.0, 3.0, 0.0, 5.0, 0.0, 5.0, 1.0, 6.0, 0.0]
    println()

    println("sparse = sparse + sparse")
    C = SimpleSparseVector{0.0, Float64, Int}([11], [])
    ex = @I @loop i C[i] += A[i] + B[i]

    display(lower_julia(virtualize(:ex, typeof(ex), LowerJuliaContext())))
    println()
    execute(ex)

    println(A)
    println(B)
    println(C)

    @test C.idx == [1, 2, 3, 5, 7, 8, 9, 11]
    @test C.val == [2.0, 1.0, 3.0, 5.0, 5.0, 1.0, 6.0]
    println()

    println("run = run + sparse")
    A = SimpleRunLength{Float64, Int}([5, 9, 10], [2.0, 6.0, 9.0])
    B = SimpleSparseVector{0.0, Float64, Int}([3, 5, 8, 11], [1.0, 1.0, 1.0])
    C = SimpleRunLength{Float64, Int}([1, 10], [0.0])
    ex = @I @loop i C[i] += A[i] + B[i]

    display(lower_julia(virtualize(:ex, typeof(ex), LowerJuliaContext())))
    println()
    execute(ex)

    println(A)
    println(B)
    println(C)

    @test C.idx == [2, 3, 4, 5, 7, 8, 9, 10]
    @test C.val == [2.0, 3.0, 2.0, 3.0, 6.0, 7.0, 6.0, 9.0]
    println()

    println("run = run + sparse")
    A = SimpleRunLength{Float64, Int}([5, 7, 10], [2.0, 6.0, 9.0])
    B = SimpleSparseVector{0.0, Float64, Int}([3, 5, 8, 11], [1.0, 1.0, 1.0])
    C = SimpleRunLength{Float64, Int}([1, 10], [0.0])
    ex = @I @loop i C[i] += A[i] + B[i]

    display(lower_julia(virtualize(:ex, typeof(ex), LowerJuliaContext())))
    println()
    execute(ex)

    println(A)
    println(B)
    println(C)

    @test C.idx == [2, 3, 4, 5, 7, 7, 8, 10]
    @test C.val == [2.0, 3.0, 2.0, 3.0, 6.0, 9.0, 10.0, 9.0]
    println()

    println("run = run + dense")
    A = SimpleRunLength{Float64, Int}([1, 3, 5, 7, 9, 10], [2.0, 3.0, 4.0, 5.0, 6.0, 7.0])
    B = ones(10)
    C = SimpleRunLength{Float64, Int}([1, 10], [0.0])
    ex = @I @loop i C[i] += A[i] + B[i]

    display(lower_julia(virtualize(:ex, typeof(ex), LowerJuliaContext())))
    println()
    execute(ex)

    println(A)
    println(B)
    println(C)

    @test C.idx == [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    @test C.val == [3.0, 4.0, 4.0, 5.0, 5.0, 6.0, 6.0, 7.0, 7.0, 8.0]
    println()

    println("run = run + singlespike")
    A = SimpleRunLength{Float64, Int}([1, 3, 5, 7, 9, 10], [2.0, 3.0, 4.0, 5.0, 6.0, 7.0])
    B = SingleSpike{0.0}(10, 2.0)
    C = SimpleRunLength{Float64, Int}([1, 10], [0.0])
    ex = @I @loop i C[i] += A[i] + B[i]

    display(lower_julia(virtualize(:ex, typeof(ex), LowerJuliaContext())))
    println()
    execute(ex)

    println(A)
    println(B)
    println(C)

    #Not checking for empty runs lol.
    @test C.idx == [1, 3, 5, 7, 9, 9, 10]
    @test C.val == [2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 9.0]
    println()
end