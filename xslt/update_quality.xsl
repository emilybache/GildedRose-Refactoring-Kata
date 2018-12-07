<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
    <xsl:output method="xml" encoding="utf-8" indent="yes" />

    <xsl:template match="/gildedrose">
        <xsl:text disable-output-escaping='yes'>
&lt;!DOCTYPE gildedrose SYSTEM "gilded_rose.dtd"&gt;
</xsl:text>
        <xsl:element name="gildedrose">

            <xsl:element name="greeting">
                <xsl:text>OMGHAI!</xsl:text>
            </xsl:element>

            <xsl:choose>
                <xsl:when test="day">
                    <xsl:apply-templates select="day" />
                </xsl:when>
                <xsl:otherwise>
                    <xsl:element name="day">
                        <xsl:text>0</xsl:text>
                    </xsl:element>
                </xsl:otherwise>
            </xsl:choose>

            <xsl:element name="items">
                <xsl:for-each select="items/item">
                    <xsl:element name="item">

                        <xsl:attribute name="name">
                            <xsl:value-of select="@name" />
                        </xsl:attribute>

                        <xsl:variable name="qualityDelta1">
                            <xsl:choose>
                                <xsl:when test="not(@name = 'Aged Brie') and not(@name = 'Backstage passes to a TAFKAL80ETC concert')">
                                    <xsl:if test="@quality &gt; 0">
                                        <xsl:if test="not(@name = 'Sulfuras, Hand of Ragnaros')">
                                            <xsl:text>-</xsl:text>
                                        </xsl:if>
                                    </xsl:if>
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:if test="@quality &lt; 50">
                                        <xsl:text>-0+</xsl:text>

                                        <xsl:if test="@name = 'Backstage passes to a TAFKAL80ETC concert'">
                                            <xsl:if test="@sellin &lt; 11">
                                                <xsl:if test="@quality &lt; 49"> <!-- hack for XSLT -->
                                                    <xsl:text>+</xsl:text>
                                                </xsl:if>
                                            </xsl:if>

                                            <xsl:if test="@sellin &lt; 6">
                                                <xsl:if test="@quality &lt; 48"> <!-- hack for XSLT -->
                                                    <xsl:text>+</xsl:text>
                                                </xsl:if>
                                            </xsl:if>
                                        </xsl:if>
                                    </xsl:if>
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:variable>
                        <xsl:variable name="quality1">
                            <xsl:choose>
                                <xsl:when test="string-length($qualityDelta1)">
                                    <xsl:value-of select="@quality + string-length($qualityDelta1) - 2" /> <!-- hack for XSLT -->
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:value-of select="@quality" />
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:variable>

                        <xsl:variable name="sellin">
                            <xsl:choose>
                                <xsl:when test="not(@name = 'Sulfuras, Hand of Ragnaros')">
                                    <xsl:value-of select="@sellin - 1" />
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:value-of select="@sellin" />
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:variable>
                        <xsl:attribute name="sellin">
                            <xsl:value-of select="$sellin" />
                        </xsl:attribute>

                        <xsl:variable name="qualityDelta2">
                            <xsl:if test="$sellin &lt; 0">
                                <xsl:choose>
                                    <xsl:when test="not(@name = 'Aged Brie')">
                                        <xsl:choose>
                                            <xsl:when test="not(@name = 'Backstage passes to a TAFKAL80ETC concert')">
                                                <xsl:if test="$quality1 &gt; 0">
                                                    <xsl:if test="not(@name = 'Sulfuras, Hand of Ragnaros')">
                                                        <xsl:text>-1</xsl:text>
                                                    </xsl:if>
                                                </xsl:if>
                                            </xsl:when>
                                            <xsl:otherwise>
                                                <xsl:value-of select="-$quality1" />
                                            </xsl:otherwise>
                                        </xsl:choose>
                                    </xsl:when>
                                    <xsl:otherwise>
                                        <xsl:if test="$quality1 &lt; 50">
                                            <xsl:text>1</xsl:text>
                                        </xsl:if>
                                    </xsl:otherwise>
                                </xsl:choose>
                            </xsl:if>
                        </xsl:variable>

                        <xsl:attribute name="quality">
                            <xsl:choose>
                                <xsl:when test="string-length($qualityDelta2)">
                                    <xsl:value-of select="$quality1 + $qualityDelta2" />
                                </xsl:when>
                                <xsl:otherwise>
                                    <xsl:value-of select="$quality1" />
                                </xsl:otherwise>
                            </xsl:choose>
                        </xsl:attribute>

                    </xsl:element>
                </xsl:for-each>
            </xsl:element>
        </xsl:element>
    </xsl:template>

    <xsl:template match="day">
        <xsl:element name="day">
            <xsl:variable name="day-before">
                <xsl:value-of select="." />
            </xsl:variable>
            <xsl:value-of select="$day-before + 1" />
        </xsl:element>
    </xsl:template>

</xsl:stylesheet>
