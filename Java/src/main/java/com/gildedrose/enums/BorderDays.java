package com.gildedrose.enums;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum BorderDays {

    INCREASE_QUALITY_BY_TWO_WHEN_LESS_THEN_OR_EQUAL_TEN_DAYS(10),
    INCREASE_QUALITY_BY_THREE_WHEN_LESS_THEN_OR_EQUAL_FIVE_DAYS(5),
    INCREASE_BY_TWO_DAYS(2),
    INCREASE_BY_THREE_DAYS(3),
    DATE_HAS_PASSED(0);

    private final int days;

}
