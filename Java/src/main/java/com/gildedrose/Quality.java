package com.gildedrose;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@ToString
@EqualsAndHashCode
public class Quality {
  static final int DEFAULT_MAX_QUALITY = 50;
  static final int DEFAULT_MIN_QUALITY = 0;

  private final int maximumQuality;
  private final int minimumValue;
  @Getter @Setter private int value;

  private Quality(int maximumQuality, int minimumValue, int value) {
    this.maximumQuality = maximumQuality;
    this.minimumValue = minimumValue;
    this.value = value;
  }

  public static Quality create(int value) {
    return Quality.create(value, DEFAULT_MAX_QUALITY);
  }

  public static Quality create(int value, int maximumQuality) {
    return new Quality(maximumQuality, DEFAULT_MIN_QUALITY, value);
  }

  public Quality copy() {
    return new Quality(maximumQuality, minimumValue, value);
  }

  public void decrement() {
    if (this.value - 1 >= this.minimumValue) {
      this.value--;
    } else {
      this.value = this.minimumValue;
    }
  }

  public void increment() {
    if (this.value + 1 <= this.maximumQuality) {
      this.value++;
    } else {
      this.value = this.maximumQuality;
    }
  }
}
