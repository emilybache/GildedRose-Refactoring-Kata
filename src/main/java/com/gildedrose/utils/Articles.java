package com.gildedrose.utils;


/**
 * The {@link Articles} enum defines types of a Articles.
 * The following types are defined:
 * <ul>
 *     <li>Aged Brie</li>
 *     <li>Backstage passes to a TAFKAL80ETC concert</li>
 *     <li>Sulfuras, Hand of Ragnaros</li>
 *     <li>Conjured Mana Cake</li>
 *     <li>Default</li>
 * </ul>
 */
public enum Articles {

    /**
     * Aged Brie
     */
    AGED_BRIE ("Aged Brie"),

    /**
     * the Backstage passes
     */
    TAFKAL_80ETC ("Backstage passes to a TAFKAL80ETC concert"),

    /**
     * Sulfuras, Hand of Ragnaros
     */
    SULFURAS ("Sulfuras, Hand of Ragnaros"),
    /**
     * Conjured Mana Cake
     */
    CONJURED ("Conjured Mana Cake"),

    /**
     * Unknown article.
     */
    DEFAULT ("DEFAULT");

    /**
     * The article name.
     */
    private final String article;

    Articles(final String article) {
        this.article = article;
    }

    /**
     * Gets the article name.
     *
     * @return the article.
     */
    public String getArticle() {
        return article;
    }

    /**
     * Gets the article name given an article.
     *
     * @param article the article String to match
     * @return the article given a {@code}. {@code DEFAULT} if not found, can be null
     */
    public static Articles getArticle(final String article) {
        for (final Articles a : values()) {
            if (a.getArticle().equals(article)) {
                return a;
            }
        }
        return DEFAULT;
    }




}
