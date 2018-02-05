<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:template name="header">
    <div id="header">
      <h1 class="blog-title">
        <a>
          <xsl:attribute name="href">
            <xsl:value-of select="concat($context, 'index.html')"/>
          </xsl:attribute>
          <xsl:text>My OS X Programming Blog</xsl:text>
        </a>
      </h1>
    </div>
  </xsl:template>
  <xsl:template name="navigation">
    <xsl:variable name="articles" select="document(concat($context, 'articles.xml'), .)"/>
    <div id="navigation">
      <h2 class="title">Articles</h2>
      <xsl:apply-templates select="$articles/article-list/article[1]" mode="nav-article"/>
      <xsl:apply-templates select="$articles/article-list/article[2]" mode="nav-article"/>
      <xsl:apply-templates select="$articles/article-list/article[3]" mode="nav-article"/>
      <xsl:apply-templates select="$articles/article-list/article[4]" mode="nav-article"/>
      <xsl:apply-templates select="$articles/article-list/article[5]" mode="nav-article"/>
      <h3 class="article-title">
        <a>
          <xsl:attribute name="href">
            <xsl:value-of select="concat($context, 'all.html')"/>
          </xsl:attribute>
          <xsl:text>[Complete Article List...]</xsl:text>
        </a>
      </h3>
      <h3 class="article-title">
        <a>
          <xsl:attribute name="href">
            <xsl:value-of select="concat($context, 'rss.xml')"/>
          </xsl:attribute>
          <xsl:text>[RSS]</xsl:text>
        </a>
      </h3>
      <h2 class="title">Categories</h2>
      <xsl:call-template name="category">
        <xsl:with-param name="category-name">General</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="category">
        <xsl:with-param name="category-name">Jazz</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="category">
        <xsl:with-param name="category-name">Programming</xsl:with-param>
      </xsl:call-template>
      <xsl:call-template name="category">
        <xsl:with-param name="category-name">T2</xsl:with-param>
      </xsl:call-template>
      <h2 class="title">Information</h2>
      <xsl:for-each select="$articles/article-list/article[category='Info']">
        <xsl:apply-templates select="." mode="nav-info"/>
      </xsl:for-each>
      <xsl:variable name="links" select="document(concat($context, 'links.xml'), .)"/>
      <xsl:for-each select="$links/links/group">
        <xsl:apply-templates select="."/>
      </xsl:for-each>
    </div>
  </xsl:template>
  <xsl:template name="footer">
    <div id="footer">Copyright © 2007, 2008, 2009, 2010, 2011, 2012 Andrew Choi. All rights reserved. Do not aggregate.</div>
  </xsl:template>
  <xsl:template match="article" mode="nav-article">
    <h3 class="article-title">
      <a>
        <xsl:attribute name="href">
          <xsl:value-of select="concat($context, filename, 'index.html')"/>
        </xsl:attribute>
        <xsl:value-of select="title"/>
      </a>
    </h3>
    <div class="date">
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
    </div>
  </xsl:template>
  <xsl:template match="article" mode="nav-info">
    <h3 class="article-title">
      <a>
        <xsl:attribute name="href">
          <xsl:value-of select="concat($context, filename, 'index.html')"/>
        </xsl:attribute>
        <xsl:value-of select="title"/>
      </a>
    </h3>
  </xsl:template>
  <xsl:template name="category">
    <xsl:param name="category-name"/>
    <h3 class="article-title">
      <a>
        <xsl:attribute name="href">
          <xsl:value-of select="concat($context, $category-name, '.html')"/>
        </xsl:attribute>
        <xsl:value-of select="$category-name"/>
      </a>
    </h3>
  </xsl:template>
  <xsl:template match="links/group">
    <h2 class="title">
      <xsl:value-of select="@name"/>
    </h2>
    <xsl:for-each select="link">
	  <h3 class="article-title">
        <a>
          <xsl:attribute name="href">
    	  <xsl:value-of select="@href"/>
          </xsl:attribute>
          <xsl:value-of select="@name"/>
        </a>
      </h3>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
