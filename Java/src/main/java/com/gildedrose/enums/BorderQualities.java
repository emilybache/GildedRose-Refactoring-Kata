package com.gildedrose.enums;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public enum BorderQualities {

    MIN_QUALITY(0),
    MAX_QUALITY(50);

    private final int quality;

}
