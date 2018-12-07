<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" 
	xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
	xmlns:exsl="http://exslt.org/common" 
	extension-element-prefixes="exsl"
	xmlns:xsltu="http://xsltunit.org/0/" 
	exclude-result-prefixes="exsl">

  <xsl:template name="xsltu:assertEqual">
    <xsl:param name="id"/>
    <xsl:param name="nodes1"/>
    <xsl:param name="nodes2"/>
    <xsl:variable name="result">
      <xsl:call-template name="xsltu:diff">
        <xsl:with-param name="nodes1" select="exsl:node-set($nodes1)"/>
        <xsl:with-param name="nodes2" select="exsl:node-set($nodes2)"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:call-template name="xsltu:assert">
      <xsl:with-param name="id" select="$id"/>
      <xsl:with-param name="test" select="not(exsl:node-set($result)//xsltu:no-match)"/>
      <xsl:with-param name="message" select="exsl:node-set($result)"/>
    </xsl:call-template>
  </xsl:template>
  <xsl:template name="xsltu:assertNotEqual">
    <xsl:param name="id"/>
    <xsl:param name="nodes1"/>
    <xsl:param name="nodes2"/>
    <xsl:variable name="result">
      <xsl:call-template name="xsltu:diff">
        <xsl:with-param name="nodes1" select="exsl:node-set($nodes1)"/>
        <xsl:with-param name="nodes2" select="exsl:node-set($nodes2)"/>
      </xsl:call-template>
    </xsl:variable>
    <xsl:call-template name="xsltu:assert">
      <xsl:with-param name="id" select="$id"/>
      <xsl:with-param name="test" select="exsl:node-set($result)//xsltu:no-match"/>
      <xsl:with-param name="message">Should have been different!</xsl:with-param>
    </xsl:call-template>
  </xsl:template>
  <xsl:template name="xsltu:assert">
    <xsl:param name="id"/>
    <xsl:param name="test"/>
    <xsl:param name="message"/>
    <xsltu:assert id="{$id}">
      <xsl:choose>
        <xsl:when test="$test">
          <xsl:attribute name="outcome">passed</xsl:attribute>
        </xsl:when>
        <xsl:otherwise>
          <xsl:attribute name="outcome">failed</xsl:attribute>
          <xsltu:message>
            <xsl:copy-of select="$message"/>
          </xsltu:message>
        </xsl:otherwise>
      </xsl:choose>
    </xsltu:assert>
  </xsl:template>
  <xsl:template name="xsltu:diff">
    <xsl:param name="nodes1"/>
    <xsl:param name="nodes2"/>
    <xsltu:diff name="{name($nodes1)}">
      <xsl:choose>
        <xsl:when test="self::* and (local-name($nodes1) != local-name($nodes2) or namespace-uri($nodes1) != namespace-uri($nodes2))">
          <xsltu:no-match diff="names">
            <xsltu:node>
              <xsl:copy-of select="$nodes1"/>
            </xsltu:node>
            <xsltu:node>
              <xsl:copy-of select="$nodes2"/>
            </xsltu:node>
          </xsltu:no-match>
        </xsl:when>
        <xsl:when test="count($nodes1/@*) != count($nodes2/@*)">
          <xsltu:no-match diff="number of children attributes ({count($nodes1/@*)} versus {count($nodes2/@*)} )">
            <xsltu:node>
              <xsl:copy-of select="$nodes1"/>
            </xsltu:node>
            <xsltu:node>
              <xsl:copy-of select="$nodes2"/>
            </xsltu:node>
          </xsltu:no-match>
        </xsl:when>
        <xsl:when test="count($nodes1/*) != count($nodes2/*)">
          <xsltu:no-match diff="number of children elements ({count($nodes1/*)} versus {count($nodes2/*)} )">
            <xsltu:node>
              <xsl:copy-of select="$nodes1"/>
            </xsltu:node>
            <xsltu:node>
              <xsl:copy-of select="$nodes2"/>
            </xsltu:node>
          </xsltu:no-match>
        </xsl:when>
        <xsl:when test="count($nodes1/text()) != count($nodes2/text())">
          <xsltu:no-match diff="number of children text nodes ({count($nodes1/text())} versus {count($nodes2/text())} )">
            <xsltu:node>
              <xsl:copy-of select="$nodes1"/>
            </xsltu:node>
            <xsltu:node>
              <xsl:copy-of select="$nodes2"/>
            </xsltu:node>
          </xsltu:no-match>
        </xsl:when>
        <xsl:otherwise>
          <xsl:apply-templates select="$nodes1/@*" mode="xsltu:diff">
            <xsl:with-param name="nodes2" select="$nodes2"/>
          </xsl:apply-templates>
          <xsl:apply-templates select="$nodes1/*" mode="xsltu:diff">
            <xsl:with-param name="nodes2" select="$nodes2"/>
          </xsl:apply-templates>
          <xsl:apply-templates select="$nodes1/text()" mode="xsltu:diff">
            <xsl:with-param name="nodes2" select="$nodes2"/>
          </xsl:apply-templates>
        </xsl:otherwise>
      </xsl:choose>
    </xsltu:diff>
  </xsl:template>
  <xsl:template match="*" mode="xsltu:diff">
    <xsl:param name="pos" select="position()"/>
    <xsl:param name="nodes2"/>
    <xsl:param name="node2" select="$nodes2/*[position()=$pos]"/>
    <xsl:call-template name="xsltu:diff">
      <xsl:with-param name="nodes1" select="."/>
      <xsl:with-param name="nodes2" select="$node2"/>
    </xsl:call-template>
  </xsl:template>
  <xsl:template match="text()" mode="xsltu:diff">
    <xsl:param name="current" select="."/>
    <xsl:param name="pos" select="position()"/>
    <xsl:param name="nodes2"/>
    <xsl:param name="node2" select="$nodes2/text()[position()=$pos]"/>
    <xsl:if test="not(. = $node2)">
      <xsltu:no-match>
        <xsltu:node>
          <xsl:copy-of select="."/>
        </xsltu:node>
        <xsltu:node>
          <xsl:copy-of select="$node2"/>
        </xsltu:node>
      </xsltu:no-match>
    </xsl:if>
  </xsl:template>
  <xsl:template match="@*" mode="xsltu:diff">
    <xsl:param name="current" select="."/>
    <xsl:param name="nodes2"/>
    <xsl:param name="node2" select="$nodes2/@*[local-name() = local-name(current()) and namespace-uri() = namespace-uri(current())]"/>
    <xsl:if test="not(. = $node2)">
      <xsltu:no-match>
        <xsltu:node>
          <xsl:copy-of select="."/>
        </xsltu:node>
        <xsltu:node>
          <xsl:copy-of select="$node2"/>
        </xsltu:node>
      </xsltu:no-match>
    </xsl:if>
  </xsl:template>
</xsl:stylesheet>
