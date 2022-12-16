package com.gildedrose.items;

import static org.assertj.core.api.Assertions.assertThat;

import com.gildedrose.Quality;
import com.gildedrose.SellIn;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class NormalItemTest {

  @Nested
  class nextQuality {
    private final NormalItem item =
        new NormalItem("+5 Dexterity Vest", SellIn.create(10), Quality.create(1), Boolean.FALSE);

    @Test
    void should_decrement_quality_in_one() {
      Quality nextQuality = item.nextQuality(Quality.create(1));

      assertThat(nextQuality.getValue()).isZero();
    }
  }
}
