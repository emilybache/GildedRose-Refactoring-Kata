package com.gildedrose.itemsorts;

import com.gildedrose.Item;
import lombok.AllArgsConstructor;
import lombok.Getter;

public interface QualityItem {
    String getName();

    int getSellIn();

    int getQuality();

    /**
     * Skeleton class intended for subclassing
     */
    abstract class QualityItemSkeleton implements QualityItem {
        private final Item item;

        public QualityItemSkeleton(String name, int sellIn, int quality) {
            this.item = new Item(name, sellIn, quality);
        }

        @Override
        public String getName() {
            return item.name;
        }

        @Override
        public String toString() {
            return item.toString();
        }

        @Override
        public int getQuality() {
            return item.quality;
        }

        @Override
        public int getSellIn() {
            return item.sellIn;
        }

        @Override
        public boolean equals(Object other) {
            if (!(other instanceof QualityItem) || other.getClass() != this.getClass()) {
                return false;
            }
            QualityItem qualityItem = (QualityItem) other;
            return (this.getName().equals(qualityItem.getName())) &&
                    (this.getQuality() == qualityItem.getQuality()) &&
                    (this.getSellIn() == qualityItem.getSellIn());
        }


        @Override
        public int hashCode() {
            int result = getName() == null ? 0 : getName().hashCode();
            result += 31 * result + getQuality();
            result += 31 * result + getSellIn();
            result += 31 * result + this.getClass().getName().hashCode();
            return result;
        }
    }
}
