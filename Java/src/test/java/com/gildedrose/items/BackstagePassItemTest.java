package com.gildedrose.items;

import static org.assertj.core.api.Assertions.assertThat;

import com.gildedrose.Quality;
import com.gildedrose.SellIn;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class BackstagePassItemTest {

  @Nested
  class nextQuality {
    @Test
    void should_increment_quality_by_one() {
      BackstagePassItem item =
          new BackstagePassItem(SellIn.create(20), Quality.create(0), Boolean.FALSE);

      Quality nextQuality = item.nextQuality(item.getQuality());

      assertThat(nextQuality).isEqualTo(Quality.create(1));
    }

    @Test
    void should_increment_quality_by_two_if_less_than_ten_days_to_sell() {
      BackstagePassItem item =
          new BackstagePassItem(SellIn.create(10), Quality.create(0), Boolean.FALSE);

      Quality nextQuality = item.nextQuality(item.getQuality());

      assertThat(nextQuality).isEqualTo(Quality.create(2));
    }

    @Test
    void should_increment_quality_by_three_if_less_than_five_days_to_sell() {
      BackstagePassItem item =
          new BackstagePassItem(SellIn.create(5), Quality.create(0), Boolean.FALSE);

      Quality nextQuality = item.nextQuality(item.getQuality());

      assertThat(nextQuality).isEqualTo(Quality.create(3));
    }

    @Test
    void should_set_quality_to_zero_if_sell_is_over() {
      BackstagePassItem item =
          new BackstagePassItem(SellIn.create(0), Quality.create(10), Boolean.FALSE);

      Quality nextQuality = item.nextQuality(item.getQuality());

      assertThat(nextQuality).isEqualTo(Quality.create(0));
    }
  }
}
