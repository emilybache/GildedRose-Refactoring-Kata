package cmd

import (
    "fmt"
    "os"
    "strconv"

    "github.com/emilybache/gildedrose-refactoring-kata/lib"
    "github.com/emilybache/gildedrose-refactoring-kata/models"
    "github.com/emilybache/gildedrose-refactoring-kata/domains"
    "github.com/spf13/cobra"
)

type TextTestFixtureCommand struct{}

func (this *TextTestFixtureCommand) Short() string {
    return "execute texttest fixture"
}

func (this *TextTestFixtureCommand) Setup(command *cobra.Command) {}

func (this *TextTestFixtureCommand) Run() lib.CommandRunner {
    return func(
        env                          lib.Env,
        logger                       lib.Logger,
        itemUpdateServiceProvider    domains.ItemUpdateServiceProvider,
    ) {
        fmt.Println("OMGHAI!")

        var itemModels = []*models.ItemModel{
            {"+5 Dexterity Vest", 10, 20},
            {"Aged Brie", 2, 0},
            {"Elixir of the Mongoose", 5, 7},
            {"Sulfuras, Hand of Ragnaros", 0, 80},
            {"Sulfuras, Hand of Ragnaros", -1, 80},
            {"Backstage passes to a TAFKAL80ETC concert", 15, 20},
            {"Backstage passes to a TAFKAL80ETC concert", 10, 49},
            {"Backstage passes to a TAFKAL80ETC concert", 5, 49},
            {"Conjured Mana Cake", 3, 6}, // <-- :O
        }

        var items = make([]*models.Item, len(itemModels))
        for i, item := range itemModels {
            items[i] = models.NewItem(item)
        }

        days := 2
        var err error
        if len(os.Args) > 2 {
            days, err = strconv.Atoi(os.Args[2])
            if err != nil {
                fmt.Println(err.Error())
                os.Exit(1)
            }
            days++
        }

        for day := 0; day < days; day++ {
            fmt.Printf("-------- day %d --------\n", day)
            fmt.Println("name, sellIn, quality")
            for i := 0; i < len(items); i++ {
                fmt.Println(items[i].Model)
            }
            fmt.Println("")
            for i := 0; i < len(items); i++ {
                itemUpdateService := itemUpdateServiceProvider.GetUpdateService(items[i])
                itemUpdateService.UpdateQuality(items[i])
            }
        }
    }
}

func NewTextTestFixtureCommand() *TextTestFixtureCommand {
    return &TextTestFixtureCommand{}
}
