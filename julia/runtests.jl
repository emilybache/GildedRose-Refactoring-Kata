using Test

# To run the tests, include this file into your REPL
# julia> include("runtests.jl")

include("gilded_rose.jl")

@testset "gilded_rose.jl" begin

    # Test foo
    items = [Item("foo", 0, 0)]
    gildedrose = GildedRose(items)
    update_quality!(gildedrose)
    @test items[1].name == "fixme"

end
