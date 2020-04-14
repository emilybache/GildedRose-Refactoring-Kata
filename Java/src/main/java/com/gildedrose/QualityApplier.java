package com.gildedrose;

import java.util.EnumSet;

public interface QualityApplier {
  boolean matches(Item item, Category category);

  int apply(Item item);
}

abstract class BaseQualityApplier implements QualityApplier {
  protected final EnumSet<Category> matchingCategories;

  public BaseQualityApplier(EnumSet<Category> matchingCategories) {
    this.matchingCategories = matchingCategories;
  }

  @Override
  public boolean matches(Item item, Category category) {
    return matchingCategories.contains(category);
  }
}

class DegradingQualityApplier extends BaseQualityApplier {

  private final int positiveSellInDegradeBy;
  private final int negativeSellInDegradeBy;

  DegradingQualityApplier(
      int positiveSellInDegradeBy,
      int negativeSellInDegradeBy,
      EnumSet<Category> matchingCategories) {
    super(matchingCategories);
    this.positiveSellInDegradeBy = positiveSellInDegradeBy;
    this.negativeSellInDegradeBy = negativeSellInDegradeBy;
  }

  @Override
  public int apply(Item item) {
    int degradeBy = item.sellIn >= 0 ? this.positiveSellInDegradeBy : negativeSellInDegradeBy;
    int newQuality = item.quality - degradeBy;
    if (newQuality < 0) {
      newQuality = 0;
    }
    return newQuality;
  }
}

class IncreasingQualityApplier extends BaseQualityApplier {

  private final int increaseBy;
  private final int max;

  IncreasingQualityApplier(int increaseBy, int max, EnumSet<Category> matchingCategories) {
    super(matchingCategories);
    this.increaseBy = increaseBy;
    this.max = max;
  }

  @Override
  public int apply(Item item) {
    int newQuality = item.quality + increaseBy;
    if (newQuality >= max) {
      newQuality = max;
    }
    return newQuality;
  }
}

class BackstagePassQualityApplier extends BaseQualityApplier {

  private final int max;

  BackstagePassQualityApplier(int max, EnumSet<Category> matchingCategories) {
    super(matchingCategories);
    this.max = max;
  }

  @Override
  public int apply(Item item) {
    int newQuality;

    if (item.sellIn < 0) {
      newQuality = 0;
    } else if (item.sellIn < 5) {
      newQuality = item.quality + 3;
    } else if (item.sellIn < 10) {
      newQuality = item.quality + 2;
    } else {
      newQuality = item.quality + 1;
    }
    if (newQuality >= max) {
      newQuality = max;
    }
    return newQuality;
  }
}
