<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" doctype-public="-//W3C//DTD HTML 4.01//EN" doctype-system="http://www.w3.org/TR/html4/strict.dtd"/>
  <xsl:include href="page-template.xslt"/>
  <xsl:variable name="context">../../</xsl:variable>
  <xsl:template match="/article">
    <html>
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <title>
          <xsl:value-of select="title"/>
        </title>
        <link rel="stylesheet" href="../../css/base.css"/>
        <link rel="stylesheet" href="../../css/article.css"/>
        <link rel="stylesheet" href="../../css/highlight.css"/>
      </head>
      <body>
        <div id="box0">
          <xsl:call-template name="header"/>
          <div id="box1">
            <div id="box2">
              <div id="contents">
                <h2 class="title">
                  <xsl:value-of select="title"/>
                </h2>
                <xsl:call-template name="gen-date-and-abstract">
                  <xsl:with-param name="title">
                    <xsl:value-of select="title"/>
                  </xsl:with-param>
                </xsl:call-template>
                <div class="contents">
                  <xsl:apply-templates select="contents/*"/>
                </div>
                <xsl:call-template name="gen-category">
                  <xsl:with-param name="title">
                    <xsl:value-of select="title"/>
                  </xsl:with-param>
                </xsl:call-template>
              </div>
              <xsl:call-template name="navigation"/>
            </div>
          </div>
          <xsl:call-template name="footer"/>
        </div>
      </body>
    </html>
  </xsl:template>
  <xsl:template name="gen-date-and-abstract">
    <xsl:param name="title"/>
    <xsl:variable name="articles" select="document(concat($context, 'articles.xml'), .)"/>
    <xsl:for-each select="$articles//article">
      <xsl:if test="title = $title">
        <div class="date">
          <xsl:choose>
            <xsl:when test="substring(date, 6, 1) = '0'">
              <xsl:value-of select="substring(date, 7, 10)"/>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="substring(date, 6, 11)"/>
            </xsl:otherwise>
          </xsl:choose>
        </div>
        <div class="abstract">
          <xsl:apply-templates select="abstract/*"/>
        </div>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
  <xsl:template name="gen-category">
    <xsl:param name="title"/>
    <xsl:variable name="articles" select="document(concat($context, 'articles.xml'), .)"/>
    <xsl:for-each select="$articles//article">
      <xsl:if test="title = $title">
        <div class="category">
          <p>
            <xsl:text>Category: </xsl:text>
            <xsl:for-each select="category">
              <xsl:apply-templates/>
              <xsl:if test="not(position()=last())">, </xsl:if>
            </xsl:for-each>
          </p>
        </div>
      </xsl:if>
    </xsl:for-each>
  </xsl:template>
  <xsl:template match="include">
    <xsl:variable name="pathname" select="."/>
    <xsl:variable name="contents" select="document($pathname)"/>
    <xsl:copy-of select="$contents"/>
  </xsl:template>
<!-- Identity transformation -->
  <xsl:template match="@*|*">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
