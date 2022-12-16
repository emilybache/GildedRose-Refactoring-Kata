package com.gildedrose;

import lombok.EqualsAndHashCode;
import lombok.ToString;

@ToString
@EqualsAndHashCode
public class SellIn {

  private int daysLeftToSell;

  private SellIn(int days) {
    this.daysLeftToSell = days;
  }

  public static SellIn create(int daysLeftToSell) {
    return new SellIn(daysLeftToSell);
  }

  public void decrement() {
    this.daysLeftToSell--;
  }

  public boolean daysLeftUntilIsOverAreLessThan(int days) {
    return this.daysLeftToSell < days;
  }

  public boolean isOver() {
    return this.daysLeftToSell <= 0;
  }
}
