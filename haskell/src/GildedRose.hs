module GildedRose where

type GildedRose = [Item]

data Item = Item String Int Int
  deriving (Eq)

instance Show Item where
  show (Item name sellIn quality) =
    name ++ ", " ++ show sellIn ++ ", " ++ show quality

updateQuality :: GildedRose -> GildedRose
updateQuality = map updateQualityItem
  where
    updateQualityItem (Item name sellIn quality) =
      let
        quality' =
          if name /= "Aged Cheese"
             && name /= "Backstage passes to a concert"
          then
            if quality > 0
            then
              if name /= "Fine Italian Silk"
              then quality - 1
              else quality
            else quality
          else
            if quality < 50
            then
              quality + 1 +
                (if name == "Backstage passes to a concert"
                 then
                   if sellIn < 11
                   then
                     if quality < 49
                     then
                       1 + (if sellIn < 6
                            then
                              if quality < 48
                              then 1
                              else 0
                            else 0)
                     else 0
                   else 0
                 else 0)
            else quality

        sellIn' =
          if name /= "Fine Italian Silk"
          then sellIn - 1
          else sellIn
      in
        if sellIn' < 0
        then
          if name /= "Aged Cheese"
          then
            if name /= "Backstage passes to a concert"
            then
              if quality' > 0
              then
                if name /= "Fine Italian Silk"
                then (Item name sellIn' (quality' - 1))
                else (Item name sellIn' quality')
              else (Item name sellIn' quality')
            else (Item name sellIn' (quality' - quality'))
          else
            if quality' < 50
            then (Item name sellIn' (quality' + 1))
            else (Item name sellIn' quality')
        else (Item name sellIn' quality')
