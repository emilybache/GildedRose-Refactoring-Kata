package com.gildedrose;

import static java.lang.Integer.*;
import static java.lang.Integer.MIN_VALUE;
import static java.util.function.Function.identity;
import static org.junit.Assert.*;

import org.approvaltests.legacycode.LegacyApprovals;
import org.approvaltests.reporters.DiffReporter;
import org.approvaltests.reporters.JunitReporter;
import org.approvaltests.reporters.UseReporter;
import org.junit.Test;

import java.util.Arrays;
import java.util.function.Function;
import java.util.stream.Stream;

@UseReporter(JunitReporter.class)
public class GildedRoseTest {

    Stream<String> ODD_ITEM_NAMES = Σ("", "pie", "foo");
    Stream<String> KNOWN_ITEM_NAMES =
         Σ("Aged Brie",
           "Elixir of the Mongoose",
           "Sulfuras, Hand of Ragnaros",
           "Backstage passes to a TAFKAL80ETC concert");
    Stream<String> UNKNOWN_ITEM_NAMES = Σ("+5 Dexterity Vest", "Conjured Mana Cake");

    Stream<Integer> MIN_DAYS =                Σ(MIN_VALUE);
    Stream<Integer> DAYS_NEAR_ZERO =          Σ(-1,0,1);
    Stream<Integer> SINGLE_DIGIT_DAYS =       Σ(4,5,6);
    Stream<Integer> LOW_DOUBLE_DIGIT_DAYS =   Σ(10,11,12);
    Stream<Integer> HIGH_DOUBLE_DIGIT_DAYS =  Σ(99_999,100_000,100_001);
    Stream<Integer> MAX_DAYS =                Σ(MAX_VALUE);

    Stream<Integer> MINIMUM_QUALITY =         Σ(0);
    Stream<Integer> SINGLE_DIGIT_QUALITIES =  Σ(1,2,3,4,5);
    Stream<Integer> DOUBLE_DIGIT_QUALITIES =  Σ(12,13,15);
    Stream<Integer> QUALITIES_NEAR_MAXIMUM =  Σ(47,48,49);
    Stream<Integer> MAXIMUM_QUALITY =         Σ(50);
    Stream<Integer> LEGENDARY_ITEM_QUALITY =  Σ(80);

    Stream<String>  GOOD_MIX_OF_ITEM_NAMES =
            ⁀(ODD_ITEM_NAMES, KNOWN_ITEM_NAMES, UNKNOWN_ITEM_NAMES);
    Stream<Integer> GOOD_MIX_OF_NUMBER_OF_DAYS_BY_WHICH_TO_SELL_ITEM =
            ⁀(MIN_DAYS, DAYS_NEAR_ZERO, SINGLE_DIGIT_DAYS, LOW_DOUBLE_DIGIT_DAYS, HIGH_DOUBLE_DIGIT_DAYS, MAX_DAYS);
    Stream<Integer> GOOD_MIX_OF_ITEM_QUALITY_LEVELS =
            ⁀(MINIMUM_QUALITY, SINGLE_DIGIT_QUALITIES, DOUBLE_DIGIT_QUALITIES, QUALITIES_NEAR_MAXIMUM, MAXIMUM_QUALITY, LEGENDARY_ITEM_QUALITY);

    @Test
    public void foo() {
        String name = "foo";
        int sellIn = 0;
        int quality = 0;
        GildedRose app = updateQuality(name, sellIn, quality);
        assertEquals("fixme", app.items[0].name);
    }

    @Test
    public void regression_test() throws Exception
    {
        String[] names = GOOD_MIX_OF_ITEM_NAMES.toArray(String[]::new);
        Integer[] sellIn = GOOD_MIX_OF_NUMBER_OF_DAYS_BY_WHICH_TO_SELL_ITEM.toArray(Integer[]::new);
        Integer[] quality = GOOD_MIX_OF_ITEM_QUALITY_LEVELS.toArray(Integer[]::new);
        LegacyApprovals.LockDown(this, "updateQuality", names, sellIn, quality);
    }

    public GildedRose updateQuality(String name, Integer sellIn, Integer quality)
    {
        Item[] items = new Item[] { new Item(name, sellIn, quality) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        return app;
    }

    // Create a stream of As
    private <A> Stream<A> Σ(A... As){ return Arrays.stream(As); }

    // concatenate streams of As
    private <A> Stream<A> ⁀(Stream<A>... streams) { return Arrays.stream(streams).flatMap( Function.identity() ); }

}
