<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template name="step1">
    <xsl:param name="mode" />
    <xsl:param name="digit1" />
    <xsl:variable name="everything">
      <xsl:choose>
        <xsl:when test="$digit1 &gt; 1">
          <xsl:call-template name="step1">
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="digit1" select="$digit1 - 1" />
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
          1
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="current">
      <xsl:call-template name="step2">
        <xsl:with-param name="mode" select="$mode" />
        <xsl:with-param name="digit1" select="$digit1" />
        <xsl:with-param name="digit2" select="9" />
      </xsl:call-template>
    </xsl:variable>
    <xsl:value-of select="$everything * $current" />
  </xsl:template>

  <xsl:template name="step2">
    <xsl:param name="mode" />
    <xsl:param name="digit1" />
    <xsl:param name="digit2" />
    <xsl:variable name="everything">
      <xsl:choose>
        <xsl:when test="$digit2 &gt; 1">
          <xsl:call-template name="step2">
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="digit1" select="$digit1" />
            <xsl:with-param name="digit2" select="$digit2 - 1" />
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
          1
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="current">
      <xsl:call-template name="step3">
        <xsl:with-param name="mode" select="$mode" />
        <xsl:with-param name="digit1" select="$digit1" />
        <xsl:with-param name="digit2" select="$digit2" />
        <xsl:with-param name="digit3" select="9" />
      </xsl:call-template>
    </xsl:variable>
    <xsl:value-of select="$everything * $current" />
  </xsl:template>

  <xsl:template name="step3">
    <xsl:param name="mode" />
    <xsl:param name="digit1" />
    <xsl:param name="digit2" />
    <xsl:param name="digit3" />
    <xsl:variable name="everything">
      <xsl:choose>
        <xsl:when test="$digit3 &gt; 1">
          <xsl:call-template name="step3">
            <xsl:with-param name="mode" select="$mode" />
            <xsl:with-param name="digit1" select="$digit1" />
            <xsl:with-param name="digit2" select="$digit2" />
            <xsl:with-param name="digit3" select="$digit3 - 1" />
          </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
          1
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="current">
      <xsl:call-template name="magic">
        <xsl:with-param name="mode" select="$mode" />
        <xsl:with-param name="digit1" select="$digit1" />
        <xsl:with-param name="digit2" select="$digit2" />
        <xsl:with-param name="digit3" select="$digit3 - 1" />
      </xsl:call-template>
    </xsl:variable>
    <xsl:value-of select="$everything * $current" />
  </xsl:template>

  <xsl:template name="magic">
    <xsl:param name="mode" />
    <xsl:param name="digit1" />
    <xsl:param name="digit2" />
    <xsl:param name="digit3" />
    <xsl:variable name="first">
      <xsl:call-template name="take1">
        <xsl:with-param name="mode" select="$mode" />
        <xsl:with-param name="digit1" select="$digit1" />
        <xsl:with-param name="digit2" select="$digit2" />
        <xsl:with-param name="digit3" select="$digit3" />
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="second">
      <xsl:call-template name="take2">
        <xsl:with-param name="mode" select="$mode" />
        <xsl:with-param name="digit1" select="$digit1" />
        <xsl:with-param name="digit2" select="$digit2" />
        <xsl:with-param name="digit3" select="$digit3" />
      </xsl:call-template>
    </xsl:variable>
    <xsl:value-of select="$first * $second" />
  </xsl:template>

  <xsl:template name="take1">
    <xsl:param name="mode" />
    <xsl:param name="digit1" />
    <xsl:param name="digit2" />
    <xsl:param name="digit3" />
    <xsl:variable name="bignumer">
      <xsl:value-of select="$digit1" /><xsl:value-of select="$digit2" />
    </xsl:variable>
    <xsl:variable name="bigdenom">
      <xsl:value-of select="$digit2" /><xsl:value-of select="$digit3" />
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$bignumer &gt;= $bigdenom">
        1
      </xsl:when>
      <xsl:when test="($bignumer div $bigdenom) != ($digit1 div $digit3)">
        1
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="$mode = 0">
          <xsl:value-of select="$bignumer" />
        </xsl:if>
        <xsl:if test="$mode = 1">
          <xsl:value-of select="$bigdenom" />
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="take2">
    <xsl:param name="mode" />
    <xsl:param name="digit1" />
    <xsl:param name="digit2" />
    <xsl:param name="digit3" />
    <xsl:variable name="bignumer">
      <xsl:value-of select="$digit2" /><xsl:value-of select="$digit1" />
    </xsl:variable>
    <xsl:variable name="bigdenom">
      <xsl:value-of select="$digit3" /><xsl:value-of select="$digit2" />
    </xsl:variable>
    <xsl:choose>
      <xsl:when test="$bignumer &gt;= $bigdenom">
        1
      </xsl:when>
      <xsl:when test="($bignumer div $bigdenom) != ($digit1 div $digit3)">
        1
      </xsl:when>
      <xsl:otherwise>
        <xsl:if test="$mode = 0">
          <xsl:value-of select="$bignumer" />
        </xsl:if>
        <xsl:if test="$mode = 1">
          <xsl:value-of select="$bigdenom" />
        </xsl:if>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="simplify">
    <xsl:param name="mode" />
    <xsl:param name="numer" />
    <xsl:param name="denom" />
    <xsl:param name="elem" />
    <xsl:choose>
      <xsl:when test="($numer mod $elem = 0) and ($denom mod $elem = 0)">
        <xsl:if test="$mode = 0">
          <xsl:value-of select="$numer div $elem" />
        </xsl:if>
        <xsl:if test="$mode = 1">
          <xsl:value-of select="$denom div $elem" />
        </xsl:if>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="simplify">
          <xsl:with-param name="mode">
            <xsl:value-of select="$mode" />
          </xsl:with-param>
          <xsl:with-param name="numer" select="$numer" />
          <xsl:with-param name="denom" select="$denom" />
          <xsl:with-param name="elem" select="$elem - 1" />
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template match="/">
    <xsl:variable name="numer">
      <xsl:call-template name="step1">
        <xsl:with-param name="mode" select="0" />
        <xsl:with-param name="digit1" select="9" />
      </xsl:call-template>
    </xsl:variable>
    <xsl:variable name="denom">
      <xsl:call-template name="step1">
        <xsl:with-param name="mode" select="1" />
        <xsl:with-param name="digit1" select="9" />
      </xsl:call-template>
    </xsl:variable>
    <xsl:call-template name="simplify">
      <xsl:with-param name="mode" select="1" />
      <xsl:with-param name="numer" select="$numer" />
      <xsl:with-param name="denom" select="$denom" />
      <xsl:with-param name="elem" select="$numer" />
    </xsl:call-template>
  </xsl:template>

</xsl:stylesheet>
