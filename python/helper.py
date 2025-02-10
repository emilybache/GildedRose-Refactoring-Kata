def decrease_quality(quality, sell_in, decrease_quality_by=None) -> int:
    if not decrease_quality_by:
        if sell_in <= 0:
            decrease_quality_by = 2
        else:
            decrease_quality_by = 1

    return max(quality - decrease_quality_by, 0)


def increase_quality(quality, increase_quality_by=1) -> int:
    return min(quality + increase_quality_by, 50)
