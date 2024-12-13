Item := Object clone do(

    name := ""
    sellIn := 0
    quality := 0

    with := method(name, sellIn, quality,
        result := self clone
        result name = name
        result sellIn = sellIn
        result quality = quality
        result
    )

    asString = method(
        self name .. ", " .. self sellIn .. ", " .. self quality
    )

)
