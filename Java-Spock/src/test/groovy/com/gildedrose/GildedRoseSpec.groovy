package com.gildedrose

import spock.lang.Specification

/**
 * Spock unit tests.
 */
class GildedRoseSpec extends Specification {

    def "should update quality correctly"() {

        given: "some items"
        Item[] items = [new Item("foo", 0, 0)];

        and: "the application with these items"
        GildedRose app = new GildedRose(items);

        when: "updating quality"
        app.updateQuality();

        then: "the quality is correct"
        app.items[0].name == "fixme"
    }

}
