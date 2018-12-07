<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:xsltu="http://xsltunit.org/0/" version="1.0">

    <xsl:param name="testname" />
    
    <xsl:template match="xsltu:tests">
        <html>
            <head>
                <title>All Tests</title>
            </head>
            <body>
                <h2>Tests Stylesheet <code><xsl:value-of select="$testname" /></code></h2>
                <xsl:apply-templates />
            </body>
        </html>
    </xsl:template>

    <xsl:template match="xsltu:test">
        <h3>Test <code><xsl:value-of select="@id" /></code></h3>
        <ul>
            <xsl:apply-templates />
        </ul>
    </xsl:template>

    <xsl:template match="xsltu:assert">
        <li>
            Assert <code><xsl:value-of select="@id" /></code><xsl:text> ... </xsl:text>
            <xsl:choose>
                <xsl:when test="@outcome = 'passed'">
                    <span style="color:green">Passed</span>
                </xsl:when>
                <xsl:otherwise>
                    <span style="color:red">Failed</span>
                    <xsl:apply-templates />
                </xsl:otherwise>
            </xsl:choose>
        </li>
    </xsl:template>

    <xsl:template match="text()">
        <xsl:value-of select="normalize-space(translate(., '&#9;&#10;&#13;', ''))" />
    </xsl:template>

    <xsl:template match="xsltu:no-match">
        <table>
            <tr>
                <td>Expected:</td><td><code><xsl:value-of select="node[2]" /></code></td>
            </tr>
            <tr>
                <td>Actual:</td><td><code><xsl:value-of select="node[1]" /></code></td>
            </tr>
        </table>
    </xsl:template>

</xsl:stylesheet>
