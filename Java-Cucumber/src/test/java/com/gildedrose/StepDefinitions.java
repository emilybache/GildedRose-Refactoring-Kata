package com.gildedrose;

import static org.junit.Assert.*;

import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;

public class StepDefinitions {
    private String savedName;
    private Item[] items = new Item[1];
    private GildedRose app;

    @Given("for {string} initial sellin is {int} and quality is {int}")
    public void initial_sellin_is_and_quality_is(String name, int sellin, int quality) {
        savedName = name;
        items[0] = new Item(name, sellin, quality);
        app = new GildedRose(items);
    }

    @When("I update the quality")
    public void i_update_the_quality() {
        app.updateQuality();
    }

    @Then("I should get sellin as {int} and quality as {int}")
    public void i_should_get_sellin_as_and_quality_as(int sellin, int quality) {
        assertEquals(savedName, app.items[0].name);
        assertEquals(sellin, app.items[0].sellIn);
        assertEquals(quality, app.items[0].quality);
    }
}

