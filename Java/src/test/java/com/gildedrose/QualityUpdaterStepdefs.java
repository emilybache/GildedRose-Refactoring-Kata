package com.gildedrose;

import static org.assertj.core.api.Assertions.assertThat;

import io.cucumber.java.en.Given;
import io.cucumber.java.en.Then;
import io.cucumber.java.en.When;
import org.assertj.core.util.Arrays;

public class QualityUpdaterStepdefs {

   private GildedRose gildedRose;
   private Item item;

   @Given("^name is \"([^\"]*)\", quality is \"([^\"]*)\" and sellIn is \"([^\"]*)\"$")
   public void nameIsQualityIsAndSellInIs(String name, String quality, String sellIn) {
      item = new Item(name, Integer.parseInt(sellIn), Integer.parseInt(quality));
      gildedRose = new GildedRose(Arrays.array(item));
   }

   @When("I calculateQuality")
   public void iCalculateQuality() {
      gildedRose.updateQuality();
   }

   @Then("I should have new quality {string} and new sellIn {string}")
   public void iShouldHaveNewQualityAndNewSellIn(String newQuality, String newSellIn) {
      assertThat(item.quality).isEqualTo(Integer.parseInt(newQuality));
      assertThat(item.sellIn).isEqualTo(Integer.parseInt(newSellIn));
   }
}
