<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:exsl="http://exslt.org/common"
    extension-element-prefixes="exsl" xmlns:xsltu="http://xsltunit.org/0/" exclude-result-prefixes="exsl" xmlns:xalan="http://xml.apache.org/xalan">
    <xsl:import href="update_quality.xsl" />
    <xsl:import href="xsltunit-0.2/xsltunit.xsl" />
    <xsl:output method="xml" version="1.0" encoding="UTF-8" indent="yes" xalan:indent-amount="4" />
    <xsl:template match="/">
        <xsltu:tests>

            <xsltu:test id="test-top-level-adds-greeting">
                <xsl:variable name="source">
                    <gildedrose><items></items></gildedrose>
                </xsl:variable>
                <xsl:variable name="applied">
                    <xsl:apply-templates select="exsl:node-set($source)/gildedrose" />
                </xsl:variable>
                <xsl:call-template name="xsltu:assertEqual">
                    <xsl:with-param name="id" select="'template shows greeting'" />
                    <xsl:with-param name="nodes1">
                        <xsl:value-of select="exsl:node-set($applied)/gildedrose/greeting" />
                    </xsl:with-param>
                    <xsl:with-param name="nodes2">OMGHAI!</xsl:with-param>
                </xsl:call-template>
            </xsltu:test>

            <xsltu:test id="test-top-level-keeps-greeting">
                <xsl:variable name="source">
                    <gildedrose><greeting>OMGHAI!</greeting><items></items></gildedrose>
                </xsl:variable>
                <xsl:variable name="applied">
                    <xsl:apply-templates select="exsl:node-set($source)/gildedrose" />
                </xsl:variable>
                <xsl:call-template name="xsltu:assertEqual">
                    <xsl:with-param name="id" select="'template shows greeting'" />
                    <xsl:with-param name="nodes1">
                        <xsl:value-of select="exsl:node-set($applied)/gildedrose/greeting" />
                    </xsl:with-param>
                    <xsl:with-param name="nodes2">OMGHAI!</xsl:with-param>
                </xsl:call-template>
            </xsltu:test>

            <xsltu:test id="test-day-increases-day">
                <xsl:variable name="source">
                    <gildedrose><day>12</day><items></items></gildedrose>
                </xsl:variable>
                <xsl:variable name="applied">
                    <xsl:apply-templates select="exsl:node-set($source)/gildedrose" />
                </xsl:variable>
                <xsl:call-template name="xsltu:assertEqual">
                    <xsl:with-param name="id" select="'template shows day'" />
                    <xsl:with-param name="nodes1">
                        <xsl:value-of select="exsl:node-set($applied)/gildedrose/day" />
                    </xsl:with-param>
                    <xsl:with-param name="nodes2">13</xsl:with-param>
                </xsl:call-template>
            </xsltu:test>

            <xsltu:test id="test-top-level-resets-day">
                <xsl:variable name="source">
                    <gildedrose><items></items></gildedrose>
                </xsl:variable>
                <xsl:variable name="applied">
                    <xsl:apply-templates select="exsl:node-set($source)/gildedrose" />
                </xsl:variable>
                <xsl:call-template name="xsltu:assertEqual">
                    <xsl:with-param name="id" select="'template shows day'" />
                    <xsl:with-param name="nodes1">
                        <xsl:value-of select="exsl:node-set($applied)/gildedrose/day" />
                    </xsl:with-param>
                    <xsl:with-param name="nodes2">0</xsl:with-param>
                </xsl:call-template>
            </xsltu:test>

            <xsltu:test id="test-item-foo">
                <xsl:variable name="source">
                    <gildedrose><items><item name="foo" sellin="0" quality="0" /></items></gildedrose>
                </xsl:variable>
                <xsl:variable name="applied">
                    <xsl:apply-templates select="exsl:node-set($source)/gildedrose" />
                </xsl:variable>
                <xsl:call-template name="xsltu:assertEqual">
                    <xsl:with-param name="id" select="'name'" />
                    <xsl:with-param name="nodes1">
                        <xsl:value-of select="exsl:node-set($applied)/gildedrose/items/item[1]/@name" />
                    </xsl:with-param>
                    <xsl:with-param name="nodes2">fixme</xsl:with-param>
                </xsl:call-template>
            </xsltu:test>

        </xsltu:tests>
    </xsl:template>
</xsl:stylesheet>
