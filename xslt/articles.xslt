<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" doctype-public="-//W3C//DTD HTML 4.01//EN" doctype-system="http://www.w3.org/TR/html4/strict.dtd"/>
  <xsl:include href="page-template.xslt"/>
  <xsl:variable name="context"/>
  <xsl:variable name="articles" select="document('articles.xml', .)"/>
  <xsl:template match="/">
    <xsl:variable name="selector" select="selector"/>
    <html>
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <title>Article Index</title>
        <link rel="stylesheet" href="css/base.css"/>
        <link rel="stylesheet" href="css/articles.css"/>
      </head>
      <body>
        <div id="box0">
          <xsl:call-template name="header"/>
          <div id="box1">
            <div id="box2">
              <div id="contents">
                <h2 class="title">Article Index</h2>
                <xsl:choose>
                  <xsl:when test="$selector='ALL'">
                    <xsl:for-each select="$articles/article-list/article[category!='Info']">
                      <xsl:apply-templates select="."/>
                    </xsl:for-each>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:for-each select="$articles/article-list/article[category=$selector]">
                      <xsl:apply-templates select="."/>
                    </xsl:for-each>
                  </xsl:otherwise>
                </xsl:choose>
              </div>
              <xsl:call-template name="navigation"/>
            </div>
          </div>
          <xsl:call-template name="footer"/>
        </div>
      </body>
    </html>
  </xsl:template>
  <xsl:template match="article-list/article">
    <xsl:variable name="article-uri" select="concat(filename, 'index.html')"/>
    <h3 class="article-title">
      <a>
        <xsl:attribute name="href">
          <xsl:value-of select="$article-uri"/>
        </xsl:attribute>
        <xsl:value-of select="title"/>
      </a>
    </h3>
    <div class="abstract">
      <xsl:apply-templates select="abstract/*"/>
    </div>
    <div class="date-and-category">
      <xsl:text>Date: </xsl:text>
      <xsl:variable name="d" select="date"/>
      <xsl:choose>
        <xsl:when test="substring($d, 6, 1) = '0'">
          <xsl:value-of select="substring($d, 7, 10)"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="substring($d, 6, 11)"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:text> • Category: </xsl:text>
      <xsl:for-each select="category">
        <xsl:apply-templates/>
        <xsl:if test="not(position()=last())">, </xsl:if>
      </xsl:for-each>
    </div>
  </xsl:template>
<!-- Identity transformation -->
  <xsl:template match="@*|*">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
