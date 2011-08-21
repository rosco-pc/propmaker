{{ Object to work with coordinates in 4D

   Each coordinate consists of an array with 5 coordinates as floats:
    point[0] - X coordinate
    point[1] - Y coordinate
    point[2] - Z coordinate
    point[3] - Y coordinate
    point[5] - Feedrate
   The feedrate is not included in these calculations

   Implement some simple matrix routines
    point_add - Add 2 points
    point_sub - Substract 2 points
    point_mul - Multiply 2 points
    point_div - Divide 2 points
    point_abs - Absolute value of a point
}}

CON
  _clkmode = xtal1+pll16x
  _clkfreq = 80_000_000

OBJ
  f     : "F32"

PUB point_add(p1, p2, r)
  LONG[r][0] := f.FAdd(LONG[p1][0], LONG[p2][0])
  LONG[r][1] := f.FAdd(LONG[p1][1], LONG[p2][1])
  LONG[r][2] := f.FAdd(LONG[p1][2], LONG[p2][2])
  LONG[r][3] := f.FAdd(LONG[p1][3], LONG[p2][3])
  'LONG[r][4] := f.FAdd(LONG[p1][4], LONG[p2][4])

PUB point_sub(p1, p2, r)
  LONG[r][0] := f.FSub(LONG[p1][0], LONG[p2][0])
  LONG[r][1] := f.FSub(LONG[p1][1], LONG[p2][1])
  LONG[r][2] := f.FSub(LONG[p1][2], LONG[p2][2])
  LONG[r][3] := f.FSub(LONG[p1][3], LONG[p2][3])
  'LONG[r][4] := f.FSub(LONG[p1][4], LONG[p2][4])

PUB point_mul(p1, p2, r)
  LONG[r][0] := f.FMul(LONG[p1][0], LONG[p2][0])
  LONG[r][1] := f.FMul(LONG[p1][1], LONG[p2][1])
  LONG[r][2] := f.FMul(LONG[p1][2], LONG[p2][2])
  LONG[r][3] := f.FMul(LONG[p1][3], LONG[p2][3])
  'LONG[r][4] := f.FMul(LONG[p1][4], LONG[p2][4])

PUB point_div(p1, p2, r)
  LONG[r][0] := f.FDiv(LONG[p1][0], LONG[p2][0])
  LONG[r][1] := f.FDiv(LONG[p1][1], LONG[p2][1])
  LONG[r][2] := f.FDiv(LONG[p1][2], LONG[p2][2])
  LONG[r][3] := f.FDiv(LONG[p1][3], LONG[p2][3])
  'LONG[r][4] := f.FDiv(LONG[p1][4], LONG[p2][4])

PUB point_abs(p1, r)
  LONG[r][0] := f.FAbs(LONG[p1][0])
  LONG[r][1] := f.FAbs(LONG[p1][1])
  LONG[r][2] := f.FAbs(LONG[p1][2])
  LONG[r][3] := f.FAbs(LONG[p1][3])
  'LONG[r][4] := f.FAbs(LONG[p1][4])


{{
+------------------------------------------------------------------------------------------------------------------------------+
|                                                   TERMS OF USE: MIT License                                                  |
+------------------------------------------------------------------------------------------------------------------------------+
|Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation    |
|files (the "Software"), to deal in the Software without rtriction, including without limitation the rights to use, copy,    |
|modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software|
|is furnished to do so, subject to the following conditions:                                                                   |
|                                                                                                                              |
|The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.|
|                                                                                                                              |
|THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPrS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE          |
|WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR         |
|COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   |
|ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                         |
+------------------------------------------------------------------------------------------------------------------------------+
}}