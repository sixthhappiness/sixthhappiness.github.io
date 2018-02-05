<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="html" doctype-public="-//W3C//DTD HTML 4.01//EN" doctype-system="http://www.w3.org/TR/html4/strict.dtd"/>
  <xsl:include href="page-template.xslt"/>
  <xsl:variable name="context"/>
  <xsl:template match="/">
    <html>
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <title>My OS X Programming Blog</title>
        <link rel="stylesheet" href="css/base.css"/>
        <link rel="stylesheet" href="css/index.css"/>
      </head>
      <body>
        <div id="box0">
          <xsl:call-template name="header"/>
          <div id="box1">
            <div id="box2">
              <div id="contents">
                <xsl:apply-templates select="contents/*"/>
              </div>
              <xsl:call-template name="navigation"/>
            </div>
          </div>
          <xsl:call-template name="footer"/>
        </div>
      </body>
    </html>
  </xsl:template>
<!-- Identity transformation -->
  <xsl:template match="@*|*">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()"/>
    </xsl:copy>
  </xsl:template>
</xsl:stylesheet>
