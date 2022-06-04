type TItem = {
    name: string,
    sellIn: number,
    quality: number,
    isConjured: boolean
}

type TGildedRose = {
    items: TItem[]
}

export type {
    TItem,
    TGildedRose
}