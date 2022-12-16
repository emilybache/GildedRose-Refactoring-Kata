package com.gildedrose.items;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.gildedrose.Quality;
import com.gildedrose.SellIn;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class GildedRoseItemTest {

  @Nested
  class refreshState {
    GildedRoseItem item;

    @BeforeEach
    void setUp() {
      item = mock(GildedRoseItem.class);
      when(item.getSellIn()).thenReturn(SellIn.create(0));
      when(item.getQuality()).thenReturn(Quality.create(0));
      when(item.nextQuality(Quality.create(0))).thenReturn(Quality.create(10));
      when(item.nextQuality(Quality.create(10))).thenReturn(Quality.create(5));
      doCallRealMethod().when(item).refreshState();
    }

    @Test
    void should_decrement_sellIn_in_one() {
      item.refreshState();

      assertThat(item.getSellIn()).isEqualTo(SellIn.create(-1));
    }

    @Test
    void should_update_quality_using_nextQuality() {
      item.refreshState();

      assertThat(item.getQuality()).isEqualTo(Quality.create(10));
    }

    @Test
    void should_update_quality_twice_when_is_conjured() {
      when(item.isConjured()).thenReturn(Boolean.TRUE);

      item.refreshState();

      assertThat(item.getQuality()).isEqualTo(Quality.create(5));
    }
  }
}
