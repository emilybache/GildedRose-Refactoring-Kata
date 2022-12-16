package com.gildedrose.items;

import static org.assertj.core.api.Assertions.assertThat;

import com.gildedrose.Quality;
import com.gildedrose.SellIn;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class AgedBrieItemTest {
  private final AgedBrieItem item =
      new AgedBrieItem(SellIn.create(2), Quality.create(0), Boolean.FALSE);

  @Nested
  class nextQuality {
    @Test
    void should_increment_quality_in_one() {
      Quality nextQuality = item.nextQuality(Quality.create(0));

      assertThat(nextQuality).isEqualTo(Quality.create(1));
    }
  }
}
