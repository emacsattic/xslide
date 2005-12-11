<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  exclude-result-prefixes="xs xsi"
  version="2.0">

  <xsl:output method="xml" indent="yes" encoding="utf-8"/>

  <xsl:template name="*">
    <xsl:message>
*** Failed to process <xsl:value-of select="name()"/> ***
</xsl:message>
  </xsl:template>

</xsl:stylesheet>
