''*****************************************
''* Floating-Point <-> Strings v 1.2      *
''* Single-precision IEEE-754             *
''* Authors: Chip Gracey and Cam Thompson *
''* (C) 2006 Parallax, Inc.               *
''* See end of file for terms of use.     *       
''*****************************************

'' v1.0 - 01 May 2006 - original version
'' v1.1 - 12 Jul 2006 - added FloatToFormat routine
'' v1.2 - 06 Mar 2009 - added StringToFloat [mpark]
VAR

  long  p, digits, exponent, integer, tens, zeros,  precision
  long  positive_chr, decimal_chr, thousands_chr, thousandths_chr
  byte  float_string[20]

  
OBJ
  ' The F object can be FloatMath, Float32 or Float32Full depending on the application
  F : "FloatMath"

PUB FloatToString(Single) : StringPtr

''Convert floating-point number to string
''
''  entry:
''      Single = floating-point number
''
''  exit:
''      StringPtr = pointer to resultant z-string
''
''  Magnitudes below 1e+12 and within 1e-12 will be expressed directly;
''  otherwise, scientific notation will be used.
''
''  examples                 results
''  -----------------------------------------
''  FloatToString(0.0)       "0"
''  FloatToString(1.0)       "1"
''  FloatToString(-1.0)      "-1"
''  FloatToString(^^2.0)     "1.414214"
''  FloatToString(2.34e-3)   "0.00234"
''  FloatToString(-1.5e-5)   "-0.000015"
''  FloatToString(2.7e+6)    "2700000"
''  FloatToString(1e11)      "100000000000"
''  FloatToString(1e12)      "1.000000e+12"
''  FloatToString(1e-12)     "0.000000000001"
''  FloatToString(1e-13)     "1.000000e-13"

  'perform initial setup
  StringPtr := Setup(Single)

  'eliminate trailing zeros
  if integer
    repeat until integer // 10
      integer /= 10
      tens /= 10
      digits--
  else
    digits~

  'express number according to exponent
  case exponent
    'in range left of decimal
    11..0:
      AddDigits(exponent + 1)
    'in range right of decimal
    -1..digits - 13:
      zeros := -exponent
      AddDigits(1)
    'out of range, do scientific notation
    other:
      DoScientific

  'terminate z-string
  byte[p]~


PUB FloatToScientific(Single) : StringPtr

''Convert floating-point number to scientific-notation string
''
''  entry:
''      Single = floating-point number
''
''  exit:
''      StringPtr = pointer to resultant z-string
''
''  examples                           results
''  -------------------------------------------------
''  FloatToScientific(1e-9)            "1.000000e-9"
''  FloatToScientific(^^2.0)           "1.414214e+0"
''  FloatToScientific(0.00251)         "2.510000e-3"
''  FloatToScientific(-0.0000150043)   "-1.500430e-5"

  'perform initial setup
  StringPtr := Setup(Single)

  'do scientific notation
  DoScientific
  
  'terminate z-string
  byte[p]~

  
PUB FloatToMetric(Single, SuffixChr) : StringPtr | x, y

''Convert floating-point number to metric string
''
''  entry:
''      Single = floating-point number
''      SuffixChr = optional ending character (0=none)
''
''  exit:
''      StringPtr = pointer to resultant z-string
''
''  Magnitudes within the metric ranges will be expressed in metric
''  terms; otherwise, scientific notation will be used.
''
''  range   name     symbol
''  -----------------------
''  1e24    yotta    Y
''  1e21    zetta    Z
''  1e18    exa      E
''  1e15    peta     P
''  1e12    tera     T
''  1e9     giga     G
''  1e6     mega     M
''  1e3     kilo     k
''  1e0     -        -
''  1e-3    milli    m
''  1e-6    micro    u
''  1e-9    nano     n
''  1e-12   pico     p
''  1e-15   femto    f
''  1e-18   atto     a
''  1e-21   zepto    z
''  1e-24   yocto    y
''
''  examples               results
''  ------------------------------------
''  metric(2000.0, "m")    "2.000000km"
''  metric(-4.5e-5, "A")   "-45.00000uA"
''  metric(2.7e6, 0)       "2.700000M"
''  metric(39e31, "W")     "3.9000e+32W"

  'perform initial setup
  StringPtr := Setup(Single)

  'determine thousands exponent and relative tens exponent
  x := (exponent + 45) / 3 - 15
  y := (exponent + 45) // 3

  'if in metric range, do metric
  if ||x =< 8
    'add digits with possible decimal
    AddDigits(y + 1)
    'if thousands exponent not 0, add metric indicator
    if x
      byte[p++] := " "
      byte[p++] := metric[x]
  'if out of metric range, do scientific notation
  else
    DoScientific
    
  'if SuffixChr not 0, add SuffixChr
  if SuffixChr
    byte[p++] := SuffixChr
    
  'terminate z-string
  byte[p]~


PUB FloatToFormat(single, width, dp) : stringptr | n, w2

''Convert floating-point number to formatted string
''
''  entry:
''      Single = floating-point number
''      width = width of field
''      dp = number of decimal points
''
''  exit:
''      StringPtr = pointer to resultant z-string
''
''  asterisks are displayed for format errors 
''  leading blank fill is used

  ' get string pointer
  stringptr := p := @float_string

  ' width must be 1 to 9, dp must be 0 to width-1
  w2 := width  :=  width #> 1 <# 9
  dp := dp #> 0 <# (width - 2)
  if dp > 0
    w2--
  if single & $8000_0000 or positive_chr
    w2--

  ' get positive scaled integer value
  n := F.FRound(F.FMul(single & $7FFF_FFFF , F.FFloat(teni[dp])))

  if n => teni[w2]
    ' if format error, display asterisks
    repeat while width
      if --width == dp
        if decimal_chr
          byte[p++] := decimal_chr
        else
          byte[p++] := "."
      else
        byte[p++] := "*" 
    byte[p]~

  else
    ' store formatted number
    p += width
    byte[p]~

    repeat width
      byte[--p] := n // 10 + "0"
      n /= 10
      if --dp == 0
        if decimal_chr
          byte[--p] := decimal_chr
        else
          byte[--p] := "."
      if n == 0 and dp < 0
        quit

    ' store sign      
    if single & $80000000
      byte[--p] := "-"
    elseif positive_chr
      byte[--p] := positive_chr
    ' leading blank fill
    repeat while p <> stringptr
      byte[--p] := " "
      
PUB SetPrecision(NumberOfDigits)

''Set precision to express floating-point numbers in
''
''  NumberOfDigits = Number of digits to round to, limited to 1..7 (7=default)
''
''  examples          results
''  -------------------------------
''  SetPrecision(1)   "1e+0"
''  SetPrecision(4)   "1.000e+0"
''  SetPrecision(7)   "1.000000e+0"

  precision := NumberOfDigits
  

PUB SetPositiveChr(PositiveChr)

''Set lead character for positive numbers
''
''  PositiveChr = 0: no character will lead positive numbers (default)
''            non-0: PositiveChr will lead positive numbers (ie " " or "+")
''
''  examples              results
''  ----------------------------------------
''  SetPositiveChr(0)     "20.07"   "-20.07"
''  SetPositiveChr(" ")   " 20.07"  "-20.07"
''  SetPositiveChr("+")   "+20.07"  "-20.07"

  positive_chr := PositiveChr
  

PUB SetDecimalChr(DecimalChr)

''Set decimal point character
''
''  DecimalChr = 0: "." will be used (default)
''           non-0: DecimalChr will be used (ie "," for Europe)
''
''  examples             results
''  ----------------------------
''  SetDecimalChr(0)     "20.49"
''  SetDecimalChr(",")   "20,49"

  decimal_chr := DecimalChr
  

PUB SetSeparatorChrs(ThousandsChr, ThousandthsChr)

''Set thousands and thousandths separator characters
''
''  ThousandsChr =
''        0: no character will separate thousands (default)
''    non-0: ThousandsChr will separate thousands
''
''  ThousandthsChr =
''        0: no character will separate thousandths (default)
''    non-0: ThousandthsChr will separate thousandths
''
''  examples                     results
''  -----------------------------------------------------------
''  SetSeparatorChrs(0, 0)       "200000000"    "0.000729345"
''  SetSeparatorChrs(0, "_")     "200000000"    "0.000_729_345"
''  SetSeparatorChrs(",", 0)     "200,000,000"  "0.000729345"
''  SetSeparatorChrs(",", "_")   "200,000,000"  "0.000_729_345"

  thousands_chr := ThousandsChr
  thousandths_chr := ThousandthsChr
  

PUB StringToFloat(strptr) : flt | significand, ssign, places, exp, esign 
{{
  Converts string to floating-point number
  entry:
      strptr = pointer to z-string

  exit:
      flt = floating-point number


  Assumes the following floating-point syntax: [-] [0-9]* [ . [0-9]* ] [ e|E [-|+] [0-9]* ]
                                               ┌── ┌───── ┌─────────── ┌───────────────────
                                               │   │      │            │     ┌──── ┌─────
    Optional negative sign ────────────────────┘   │      │            │     │     │
    Digits ────────────────────────────────────────┘      │            │     │     │
    Optional decimal point followed by digits ────────────┘            │     │     │
    Optional exponent ─────────────────────────────────────────────────┘     │     │
      optional exponent sign ────────────────────────────────────────────────┘     │
      exponent digits ─────────────────────────────────────────────────────────────┘

  Examples of recognized floating-point numbers:
  "123", "-123", "123.456", "123.456e+09"
  Conversion stops as soon as an invalid character is encountered. No error-checking.
    
  Based on Ariba's StrToFloat in http://forums.parallax.com/forums/default.aspx?f=25&m=280607
  Expanded by Michael Park
}}
  significand~
  ssign~
  exp~
  esign~
  places~
  repeat
    case byte[strptr]
      "-":
        ssign~~
      ".":
        places := 1
      "0".."9":
        significand := significand * 10 + byte[strptr] - "0"
        if places
          ++places                    'count decimal places
      "e", "E":
        ++strptr ' skip over the e or E
        repeat
          case byte[strptr]
            "+":
              ' ignore
            "-":
              esign~~
            "0".."9":
              exp := exp * 10 + byte[strptr] - "0"
            other:
              quit
          ++strptr
        quit              
      other:
        quit
    ++strptr
    
  if ssign
    -significand
  flt := f.FFloat(significand)

  ifnot esign  ' tenf table is in decreasing order, so the sign of exp is reversed
    -exp
    
  if places
    exp += places - 1
        
  flt := f.FMul(flt, tenf[exp])              'adjust flt's decimal point
    

PRI Setup(single) : stringptr

 'limit digits to 1..7
  if precision
    digits := precision #> 1 <# 7
  else
    digits := 7

  'initialize string pointer
  p := @float_string

  'add "-" if negative
  if single & $80000000
    byte[p++] := "-"
  'otherwise, add any positive lead character
  elseif positive_chr
    byte[p++] := positive_chr

  'clear sign and check for 0
  if single &= $7FFFFFFF

    'not 0, estimate exponent
    exponent := ((single << 1 >> 24 - 127) * 77) ~> 8
    
    'if very small, bias up
    if exponent < -32
      single := F.FMul(single, 1e13)
      exponent += result := 13
      
    'determine exact exponent and integer
    repeat
      integer := F.FRound(F.FMul(single, tenf[exponent - digits + 1]))
      if integer < teni[digits - 1]
        exponent--
      elseif integer => teni[digits]
        exponent++
      else
        exponent -= result
        quit

  'if 0, reset exponent and integer
  else
    exponent~
    integer~

  'set initial tens and clear zeros
  tens := teni[digits - 1]
  zeros~

  'return pointer to string
  stringptr := @float_string


PRI DoScientific

  'add digits with possible decimal
  AddDigits(1)
  'add exponent indicator
  byte[p++] := "e"
  'add exponent sign
  if exponent => 0
    byte[p++] := "+"
  else
    byte[p++] := "-"
    ||exponent
  'add exponent digits
  if exponent => 10
    byte[p++] := exponent / 10 + "0"
    exponent //= 10
  byte[p++] := exponent + "0"


PRI AddDigits(leading) | i

  'add leading digits
  repeat i := leading
    AddDigit
    'add any thousands separator between thousands
    if thousands_chr
      i--
      if i and not i // 3
        byte[p++] := thousands_chr
  'if trailing digits, add decimal character
  if digits
    AddDecimal
    'then add trailing digits
    repeat while digits
      'add any thousandths separator between thousandths
      if thousandths_chr
        if i and not i // 3
          byte[p++] := thousandths_chr
      i++
      AddDigit


PRI AddDigit

  'if leading zeros, add "0"
  if zeros
    byte[p++] := "0"
    zeros--
  'if more digits, add current digit and prepare next
  elseif digits
    byte[p++] := integer / tens + "0"
    integer //= tens
    tens /= 10
    digits--
  'if no more digits, add "0"
  else
    byte[p++] := "0"


PRI AddDecimal

  if decimal_chr
    byte[p++] := decimal_chr
  else
    byte[p++] := "."
                    

DAT
        long                1e+38, 1e+37, 1e+36, 1e+35, 1e+34, 1e+33, 1e+32, 1e+31
        long  1e+30, 1e+29, 1e+28, 1e+27, 1e+26, 1e+25, 1e+24, 1e+23, 1e+22, 1e+21
        long  1e+20, 1e+19, 1e+18, 1e+17, 1e+16, 1e+15, 1e+14, 1e+13, 1e+12, 1e+11
        long  1e+10, 1e+09, 1e+08, 1e+07, 1e+06, 1e+05, 1e+04, 1e+03, 1e+02, 1e+01
tenf    long  1e+00, 1e-01, 1e-02, 1e-03, 1e-04, 1e-05, 1e-06, 1e-07, 1e-08, 1e-09
        long  1e-10, 1e-11, 1e-12, 1e-13, 1e-14, 1e-15, 1e-16, 1e-17, 1e-18, 1e-19
        long  1e-20, 1e-21, 1e-22, 1e-23, 1e-24, 1e-25, 1e-26, 1e-27, 1e-28, 1e-29
        long  1e-30, 1e-31, 1e-32, 1e-33, 1e-34, 1e-35, 1e-36, 1e-37, 1e-38

teni    long  1, 10, 100, 1_000, 10_000, 100_000, 1_000_000, 10_000_000, 100_000_000, 1_000_000_000

        byte "yzafpnum"
metric  byte 0
        byte "kMGTPEZY"

{{
┌──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│                                                   TERMS OF USE: MIT License                                                  │                                                            
├──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┤
│Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation    │ 
│files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,    │
│modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software│
│is furnished to do so, subject to the following conditions:                                                                   │
│                                                                                                                              │
│The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.│
│                                                                                                                              │
│THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE          │
│WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR         │
│COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   │
│ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                         │
└──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
}}        