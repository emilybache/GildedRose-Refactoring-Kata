package com.gildedrose;

import static org.junit.Assert.*;

import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class StepDefinitions {
    private Item[] items = new Item[1];
    private GildedRose app;
    private int[] qualityValues;
    private int[] sellInValues;

    @Given("For article {string} with initial quality {int} and sellIn {int}")
    public void initial_sellin_is_and_quality_is(String name, int quality, int sellIn) {
        items = new Item[] { new Item(name, sellIn, quality) };
        app = new GildedRose(items);

    }

    @When("The quality is updated the next {int} days")
    public void i_update_the_quality(int days) {
        qualityValues = new int[days];
        sellInValues = new int[days];
        for (int i = 0; i < days; i++) {
            app.updateQuality();
            qualityValues[i] = items[0].quality;
            sellInValues[i] = items[0].sellIn;
        }
    }

    @Then("I should get the following quality values each day:")
    public void i_should_get_sellin_as_and_quality_as(io.cucumber.datatable.DataTable dataTable) {
        List<Map<String, String>> rows = dataTable.asMaps(String.class, String.class);
        for (int i = 0; i < rows.size(); i++) {
            Map<String, String> row = rows.get(i);
            int expectedQuality = Integer.parseInt(row.get("quality"));
            int expectedSellIn = Integer.parseInt(row.get("sellIn"));
            assertEquals(expectedQuality, qualityValues[i]);
            assertEquals(expectedSellIn, sellInValues[i]);
        }
    }
}

