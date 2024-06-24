include("gilded_rose.jl")

if length(ARGS) > 0
    days = parse(Int64, ARGS[1])
else
    days = 2
end

main(days=days)
