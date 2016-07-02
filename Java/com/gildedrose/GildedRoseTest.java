package com.gildedrose;

import static org.junit.Assert.*;

import org.approvaltests.Approvals;
import org.approvaltests.reporters.JunitReporter;
import org.approvaltests.reporters.UseReporter;
import org.junit.Test;

import java.io.File;

@UseReporter(JunitReporter.class)
public class GildedRoseTest {

    public static final File OUTPUT_FILE = new File("com/gildedrose/GildedRoseTest.regression_test.received.txt");

    @Test
    public void foo() {
        Item[] items = new Item[] { new Item("foo", 0, 0) };
        GildedRose app = new GildedRose(items);
        app.updateQuality();
        assertEquals("fixme", app.items[0].name);
    }

    @Test
    public void regression_test() throws Exception {
        TexttestFixture.updateQualityOverTenDays(OUTPUT_FILE);
        Approvals.verify(OUTPUT_FILE);
    }
}
