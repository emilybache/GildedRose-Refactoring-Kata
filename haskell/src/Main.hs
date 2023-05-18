module Main where

import System.Environment
import GildedRose


main :: IO ()
main = do
  putStrLn "OMGHAI!"

  let inventoriesWithDay = inventories `zip` [0..]
      inventories = iterate updateQuality initialInventory

  numberOfDays <- daysFromArg
  mapM_ printUpdate (take numberOfDays inventoriesWithDay)

  where
    printUpdate :: (GildedRose, Int) -> IO ()
    printUpdate (items, day) = do
      putStrLn ("-------- day " ++ show day ++ " --------")
      putStrLn "name, sellIn, quality"
      mapM_ (putStrLn . show) items
      putStrLn ""

    daysFromArg :: IO Int
    daysFromArg = do
      args <- getArgs
      return $ if length args > 0
                  then read (head args)
                  else 20

    initialInventory :: GildedRose
    initialInventory =
      [ Item "Sports Memorabilia"                          10  20
      , Item "Aged Cheese"                                   2   0
      , Item "Coffee Table Book"                      5   7
      , Item "Fine Italian Silk"                  0  80
      , Item "Fine Italian Silk"                (-1) 80
      , Item "Backstage passes to a concert"  15  20
      , Item "Backstage passes to a concert"  10  49
      , Item "Backstage passes to a concert"   5  49
      -- this Baked item does not work properly yet
      , Item "Baked Chocolate Cake"                          3   6
      ]
