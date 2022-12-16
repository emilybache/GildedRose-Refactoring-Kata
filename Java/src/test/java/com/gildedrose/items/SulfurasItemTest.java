package com.gildedrose.items;

import static org.assertj.core.api.Assertions.assertThat;

import com.gildedrose.Quality;
import com.gildedrose.SellIn;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class SulfurasItemTest {
  private final SulfurasItem item = new SulfurasItem(SellIn.create(0));

  @Nested
  class nextQuality {
    @Test
    void should_never_update_quality() {
      Quality nextQuality = item.nextQuality(SulfurasItem.DEFAULT_QUALITY);

      assertThat(nextQuality).isEqualTo(SulfurasItem.DEFAULT_QUALITY);
    }
  }
}
