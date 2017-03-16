<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="xml" encoding="utf-8"/>
  <xsl:variable name="base-uri">http://members.shaw.ca/akochoi/</xsl:variable>
  <xsl:template match="/">
    <xsl:variable name="articles" select="document('articles.xml', .)"/>
    <xsl:element name="rss">
      <xsl:attribute name="version">2.0</xsl:attribute>
      <xsl:element name="channel">
        <xsl:element name="title">
          <xsl:text>My OS X Programming Blog</xsl:text>
        </xsl:element>
        <xsl:element name="link">
          <xsl:value-of select="concat($base-uri, 'index.html')"/>
        </xsl:element>
        <xsl:element name="description">
          <xsl:text>Mac OS X Cocoa, Carbon, and CoreMIDI Programming</xsl:text>
        </xsl:element>
        <xsl:element name="language">
          <xsl:text>en-ca</xsl:text>
        </xsl:element>
        <xsl:element name="copyright">
          <xsl:text>Copyright 2007 Andrew Choi</xsl:text>
        </xsl:element>
        <xsl:apply-templates select="$articles/article-list"/>
      </xsl:element>
    </xsl:element>
  </xsl:template>
  <xsl:template match="article-list">
    <xsl:apply-templates/>
  </xsl:template>
  <xsl:template match="article">
    <item>
      <title>
        <xsl:value-of select="title"/>
      </title>
      <link>
        <xsl:value-of select="concat($base-uri, filename, 'index.html')"/>
      </link>
      <description>
        <xsl:value-of select="abstract"/>
      </description>
      <guid>
        <xsl:value-of select="concat($base-uri, filename, 'index.html')"/>
      </guid>
      <pubDate>
        <xsl:value-of select="date"/>
      </pubDate>
	  <xsl:apply-templates select="category"/>
    </item>
  </xsl:template>
<!-- Identity transformation -->
  <xsl:template match="@*|*">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
