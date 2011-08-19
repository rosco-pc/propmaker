'******************************************************************************
' gCode parser demo, loosely based on ultimaker reprap firmware
' as found here: http://www.guthub.com/ultimaker/
'
' Version 0.2
' Changes
'
' Copyright (c) Rob Schmersel
'
' See end of file for terms of use.
'******************************************************************************
{{
  This object is used to test a gCode parser.
}}
CON
  _clkmode = xtal1+pll16x
  _clkfreq = 80_000_000

         '*ZY_XWVU_TSRQ_PONM_LKJI_HGFE_DCBA
  type = %011_1111_0111_1000_0111_1011_1111         ' Gcode type mask:
                                                    ' set bit indicates float,
                                                    ' unset bit integer/long
  ' Define bits in seen
  GCODE_A = (1<<0)                                  ' Rotary axis around X
  GCODE_B = (1<<1)                                  ' Rotary axis around Y
  GCODE_C = (1<<2)                                  ' Rotary axis around Z
  GCODE_D = (1<<3)                                  ' Cutter diameter
  GCODE_E = (1<<4)                                  '
  GCODE_F = (1<<5)                                  ' Feed rate
  GCODE_G = (1<<6)                                  ' G code
  GCODE_H = (1<<7)                                  ' Tool length offset
  GCODE_I = (1<<8)                                  ' Arc X axis
  GCODE_J = (1<<9)                                  ' Arc Y axis
  GCODE_K = (1<<10)                                 ' Arc Z axis
  GCODE_L = (1<<11)                                 '
  GCODE_M = (1<<12)                                 ' Machine code
  GCODE_N = (1<<13)                                 ' Line number
  GCODE_O = (1<<14)                                 '
  GCODE_P = (1<<15)                                 ' Dwell time
  GCODE_Q = (1<<16)                                 '
  GCODE_R = (1<<17)                                 ' Arc radius
  GCODE_S = (1<<18)                                 ' Spindle speed
  GCODE_T = (1<<19)                                 ' Tool head
  GCODE_U = (1<<20)                                 ' Relative axis parallel to X
  GCODE_V = (1<<21)                                 ' Relative axis parallel to Y
  GCODE_W = (1<<22)                                 ' Relative axis parallel to Z
  GCODE_X = (1<<23)                                 ' X position
  GCODE_Y = (1<<24)                                 ' Y position
  GCODE_Z = (1<<25)                                 ' Z position
  GCODE_Cs = (1<<26)                                ' Checksum (indicated with a '*')

  ' index in code array
  _A = 0                                            ' Rotary axis around X
  _B = 1                                            ' Rotary axis around Y
  _C = 2                                            ' Rotary axis around Z
  _D = 3                                            ' Cutter diameter
  _E = 4                                            '
  _F = 5                                            ' Feed rate
  _G = 6                                            ' G code
  _H = 7                                            ' Tool length offset
  _I = 8                                            ' Arc X axis
  _J = 9                                            ' Arc Y axis
  _K = 10                                           ' Arc Z axis
  _L = 11                                           '
  _M = 12                                           ' M code
  _N = 13                                           ' Line number
  _O = 14                                           '
  _P = 15                                           ' Dwell time
  _Q = 16                                           '
  _R = 17                                           ' Arc radius
  _S = 18                                           ' Spindle speed
  _T = 19                                           ' Tool head
  _U = 20                                           ' Relative axis parallel to X
  _V = 21                                           ' Relative axis parallel to Y
  _W = 22                                           ' Relative axis parallel to Z
  _X = 23                                           ' X position
  _Y = 24                                           ' Y position
  _Z = 25                                           ' Z position
  _Cs = 26                                          ' Checksum

  ' index into for cp and np
  xpos = 0		                                      ' X coordinate
  ypos = 1                                          ' Y coordinate
  zpos = 2                                          ' Z coordinate
  epos = 3                                          ' E ??
  fpos = 4                                          ' Feedrate

  SLOW_XY_FEEDRATE = 0                              ' Define save feedrate

OBJ
  conio  : "conio"                                  ' spinsim027 copy
  fileio : "fileio"                                 ' spinsim027 copy
  f      : "F32"                                    ' F32 v1.3
  fs     : "FloatString"                            '
  motor  : "motor"                                  ' placeholder

VAR
  BYTE buffer[100]                                  ' string buffer
  LONG seen                                         ' seen gCode mask, set bit indicates gCode seen by parser
  LONG code[27]                                     ' G code value
  LONG cp[5], np[5]                                 ' head coordinates Current Position (cp) and New Position (np)
  BYTE unit                                         ' FALSE = inches,     TRUE = mm
  BYTE mode                                         ' FALSE = Relative,   TRUE = Absolute
  BYTE debug                                        ' FALSE = do nothing, TRUE = print debug messages
  LONG lastline, lastcode

PUB main | tokens[10], numtokens, cmd, start, stop
  fileio.mount(16)                                  ' spinsim version: Mount SD card
  conio.start(31, 30, 0, 115200)                    ' spinsim version: Start serial connection
  f.start                                           ' Start F32 cog
  motor.start                                       ' PLACEHOLDER: start motor cog


  help                                              ' Show menu
  REPEAT                                            ' Main loop
    conio.tx(13)                                    ' Show prompt
    conio.tx(">")
    gets(@buffer)                                   ' get input
    numtokens := tokenize(@buffer, @tokens)         ' Tokenize input
    IFNOT numtokens                                 ' empty string, start over
      NEXT

    cmd := tokens[0]                                ' get command
    case BYTE[cmd]
      "e":                                          ' Exit
        quit
      "h":                                          ' Help
        help
      "p":                                          ' Parse gCode
        debug := FALSE
        lastline := 0
        lastcode := 0
        IF numtokens == 2
          start := cnt
          parsefile(tokens[1])                      ' read file
          stop := cnt
          conio.str(STRING("Time to complete: "))
          conio.dec((stop-start)/(CLKFREQ/1000000))
          conio.tx(13)
        ELSE
          parse                                     ' Get user input
      "d":                                          ' List directory
        printdir
      OTHER:                                        ' Unknown command
        conio.str(string("Unknown command", 13))    ' print error message
        help                                        ' and help

PUB help
  conio.str(string(13, "Avaliable Commands", 13))
  conio.str(string("h        - Print the available commands", 13))
  conio.str(string("p <file> - Parse gCode, if file is given read file from SD", 13))
  conio.str(string("d        - Directory listing", 13))
  conio.str(string("e        - Exit the program", 13))

PUB parsefile(fname) | char, prev, line, comment, r
  char~
  prev~
  comment := FALSE                        ' No comments
  line := @buffer                         ' Set line pointer
  IFNOT fileio.popen(fname, "R")
    REPEAT WHILE fileio.pread(@char, 1) > 0  ' Read character until EOF
      CASE char
        10,13:                            ' End of Line
          BYTE[line++]~                   ' Mark end of string
          IF line > @buffer + 1           ' skip empty lines
            r := parse_gcode(@buffer)     ' Parse code
                                          '   1         - END program
                                          '   0 (FALSE) - RESEND line
                                          '  -1 (TRUE)  - OK, Continue
            IF r == 1
              conio.str(STRING("END", 13))
              QUIT
            ELSEIF r
              conio.str(STRING("OK "))
              conio.dec(lastline)
              process_code
            ELSE
              conio.str(STRING("RESEND "))
              conio.dec(lastline+1)
            conio.tx(13)
          line := @buffer                 ' Reset line pointer
          NEXT                            ' Next character
        0..31:                            ' Don't store control characters
          NEXT
      BYTE[line++] := char                ' Store character
      'conio.tx(char)

  ELSE
    conio.str(string("Could not open "))
    conio.str(fname)
    conio.tx(13)

PUB parse
  REPEAT
    conio.str(STRING(13, "gcode> "))      ' prompt
    gets(@buffer)                         ' get gcode
    IFNOT parse_gcode(@buffer)            ' Parse code untill END or error
      conio.str(STRING("RESEND "))
      conio.dec(lastline+1)
    ELSE
      conio.str(STRING("OK "))
      conio.dec(lastline)
    conio.tx(13)

DAT
  end_prog     BYTE "END",0
  error_code   BYTE "Error: Invalid code: ",0
  error_check  BYTE "Error: Check sum mismatch",0
  error_line   BYTE "Error: Checksum without Linenumber",0
  error_check1 BYTE "Error: Linenumber without Checksum",0
  line_rec     BYTE ", Line received: ",0

PUB parse_gcode(str) | start, end, pos, char, prev, checksum, end_check, key, keyCode, comment

  IF LONG[str]&$FFF == LONG[@end_prog]
    RETURN 1

  comment~
  seen~
  checksum~

  start := str
  end := str + STRSIZE(str)

  REPEAT WHILE str < end
    char := BYTE[str++]
    case char
      "A".."Z","*":                       ' G-code, * = checksum
        IFNOT comment
          key := char -"A"                ' get index
          if char == "*"                  ' * is special
            key := _Cs
            end_check := str -1           ' Save pointer to wher checksum should be calculated
          keyCode := 1<<key               '
          seen |= keyCode
          IFNOT type & keyCode
            code[key] := StringToInt(str, @pos)
          ELSE
            code[key] := StringToFloat(str, @pos)
          str := pos                      ' Update pointer
      "/", ";":                           ' Comment?
        QUIT                              ' Comment until end of line, so stop parsing
      "(":                                ' Comment
        IFNOT comment
          comment := TRUE
          prev := char
      ")":
        IF comment AND prev == "("
          comment := FALSE
      " ":
      OTHER:
        IFNOT comment
          IF debug
            conio.str(@error_code)
            conio.tx(char)
            conio.str(@line_rec)
            conio.str(start)
            conio.tx(13)
          RETURN 0

  IFNOT seen                              ' Empty line
    RETURN -1

  ' checksum check
  str := start
  IF (seen & (GCODE_Cs | GCODE_N)) == (GCODE_Cs | GCODE_N)
    ' Calculate checksum (xor al characters until teh checksum marker *
    str := start
    REPEAT WHILE str < end_check
      checksum ^= BYTE[str++]
    IF checksum <> code[_Cs]
      ' Checksums are not equal
      IF debug
        conio.str(@error_check)
        conio.str(@line_rec)
        conio.str(start)
        conio.tx(13)
      RETURN 0
  ELSEIF (seen & (GCODE_Cs | GCODE_N)) == GCODE_Cs
    ' Checksum wihtout Linenumber
    IF debug
      conio.str(@error_check1)
      conio.str(@line_rec)
      conio.str(start)
      conio.tx(13)
    RETURN 0
  ELSEIF  (seen & (GCODE_Cs | GCODE_N)) == GCODE_N
    ' Linenumber without Checksum
    IF debug
      conio.str(@error_line)
      conio.str(@line_rec)
      conio.str(start)
      conio.tx(13)
    RETURN 0

  IFNOT (seen & (GCODE_G | GCODE_M | GCODE_T))
    code[_G] := lastcode
    seen |= GCODE_G

  IF debug
    conio.str(@line_rec)
    conio.str(start)
    conio.tx(13)
    conio.str(STRING("Code seen: "))

    conio.bin(seen,32)
    conio.tx(13)

  lastline++
  RETURN -1

PUB process_code
  '*************************************************************
  ' CODE BELOW COMPILES, BUT IS NOT YET CHECKED FOR CORRECTNESS
  ' ATLEAST THE CODE CONCERNING COORDINATES IS NOT CORRECT
  '*************************************************************
  {{
  IF ((seen & GCODE_M)>0) AND (code[_M] == 112) ' Deal with emergency stop as No 1 priority
      shutdown

  IF (seen & GCODE_G)>0                     ' Got a gcode?
    lastcode := code[_G]                    ' Remember it for future instructions
    IF (mode)
      cp[xpos] := f.Float(0)
      cp[ypos] := f.Float(0)
      cp[zpos] := f.Float(0)
      cp[epos] := f.Float(0)
    IF (seen & GCODE_X)>0                 ' Set X to new coordinate
      np[xpos] := f.FAdd(cp[xpos], code[_X])
    IF (seen & GCODE_Y)>0                 ' Set Y to new coordinate
      np[ypos] := f.FAdd(cp[ypos], code[_Y])
    IF (seen & GCODE_Z)>0                 ' Set Z to new coordinate
      np[zpos] := f.FAdd(cp[zpos], code[_Z])
    IF (seen & GCODE_E)>0                 ' Set E to new coordinate
      np[epos] := f.FAdd(cp[epos], code[_E])


  IF (seen & GCODE_F)>0                   ' Get feedrate if supplied -
    np[fpos] := code[_F]

  ' Process the buffered move commands first
  ' If we get one, return immediately

  case code[_G]
    0:                                    'Rapid move
      motor.move(@np)
      RETURN
    1:                                    ' Controlled move -ve coordinate means zero the axis
      motor.move(@np)
      RETURN
    28: 'go home. If we send coordinates (regardless of their value) only zero those axes
      axis := FALSE
      if seen & GCODE_X
        zeroX
        axis := TRUE
      if seen & GCODE_Y
        zeroY
        axis := TRUE
      if seen & GCODE_Z
        zeroZ
        axis := TRUE
      IFNOT axis
        zeroX
        zeroY
        zeroZ
      cp[fpos] := SLOW_XY_FEEDRATE ' Most sensible feedrate to leave it in
      RETURN

  ' Non-buffered G commands
    4:                                    ' Dwell
      delay(f.FAdd(code[_P], 0.5))
    20:                                   ' Use Inches for Units
      unit := FALSE
    21:                                   ' Use mm for Units
      unit := TRUE
    90:                                   'Absolute Positioning
      mode := TRUE
    91:                                   'Incremental Positioning
      mode := FALSE
    92:                                   'Set new position
      setPosition(@np)
    OTHER:
      if debug
        conio.str(STRING("Not implemented: G"))
        conio.dec(code[_G])
        conio.tx(13)
        resend(lastline+1)


  if (seen & GCODE_M)>0                   ' Handle M code
    case code[_M]
      0:                                  ' Shutdown
        shutdown
      1:                                  ' todo: optional stop
      2:                                  ' todo: program end
      104:                                ' custom code for temperature control
        if (seen & GCODE_S)
          motor.setTemperature(code[_S])
      105:                                ' custom code for temperature reading
        i := motor.getTemperature
        conio.dec(i)
        conio.tx(STRING(","))
        i := motor.getBedTemperature
        conio.dec(i)
        conio.tx(13)
      106:                                ' turn fan on
        if (seen & GCODE_S)>0
          motor.setCooler(code[_S])
        else
          motor.setCooler(255)
      107:                                ' turn fan off
        motor.setCooler(0)
      109:                                ' Set the temperature and wait for it to get there
        motor.setTemperature(code[_S])
        motor.waitForTemperature
      110:                                ' Starting a new print, reset the LastLineNrRecieved counter
        if (seen & GCODE_N)
          lastline := code[_N]
        else
          lastline := 0
      111:
        debug := code[_S]
      112:                                ' STOP!
        shutdown
      113:                                ' If there's an S field, use that to set the PWM, otherwise use the pot.
      114:                                ' custom code for returning current coordinates
        sendPosition(cp)
      116:                                ' TODO: make this work properly
        motor.waitForTemperature

      ' The valve (real, or virtual...) is now the way to control any extruder (such as
      ' a pressurised paste extruder) that cannot move using E codes.
      126:          ' Open the valve
        motor.valveSet(TRUE, f.FAdd(code[_P], 0.5))
      127:          ' Close the valve
        motor.valveSet(FAlSE, f.FAdd(code[_P], 0.5))
      140:
        if (seen & GCODE_S)>0
          motor.setBedTemperature(code[_S])

      141:                                'TODO: set chamber temperature
      142:                                'TODO: set holding pressure
      OTHER:
        IF debug
          conio.str(STRING("Not implemented: M"))
          conio.dec(code[_M])
          conio.tx(13)
          resend(lastline+1)

  IF (seen & GCODE_T)>0                   ' Handle T code
    motor.newExtruder(code[_T])
  }}
  lastline++
  RETURN TRUE

'*****************************************************************************
' SOME STUBS, TO FILLED IN LATTER
'*****************************************************************************
PUB resend(line)


PUB delay(time)


PUB setPosition(pos)


PUB sendPosition(pos)


PUB zeroX


PUB zeroY


PUB zeroZ


PUB shutdown


PUB printdir | pdir, dirent, num
  pdir := fileio.hopendir
  REPEAT WHILE dirent := fileio.hreaddir(pdir)
    IF long[dirent][1] & 16
      conio.str(string("<DIR>   "))
    ELSE
      num := long[dirent]
      IF num < 1
        num := 1
      REPEAT WHILE num < 1000000
        conio.tx(" ")
        num *= 10
    conio.dec(LONG[dirent])
    conio.tx(" ")
    conio.str(@LONG[dirent][2])
    conio.tx(13)
  fileio.hclosedir(pdir)

PUB gets(str) | char, prev, start, comment
  start := str
  comment := FALSE
  prev := 0
  REPEAT
    char := conio.rx
    case char
      8:                                  ' Backspace
        IF str > start                    ' Make sure we do not remove past the start of the buffer
          char := BYTE[str--]             ' Remove character from buffer
          conio.tx(8)                     ' Remove character from screen
          conio.tx(" ")
          conio.tx(8)
        NEXT
      10,13:                              ' End of input
        conio.tx(char)
        BYTE[str++]~                      ' Mark end of string
        RETURN
      0..31:                              ' Don't show or store control characters
        NEXT
    conio.tx(char)                        ' Echo character
    BYTE[str++] := char                   ' Store character if it is not part of a comment

PUB tokenize(str, tokens)
  REPEAT 10
    str := skipblanks(str)              ' Find start of token
    IFNOT BYTE[str]                     '
      RETURN
    LONG[tokens][result++] := str       ' Store token position
    str := skipchars(str)               ' Find end of token
    IF byte[str] == 0                   ' end of input, let's get back
      RETURN
    BYTE[str++]~                        ' mark end of token

PUB skipchars(str)
  REPEAT WHILE BYTE[str] AND BYTE[str] <> " "
    str++
  RETURN str

PUB skipblanks(str)
  REPEAT WHILE BYTE[str] AND BYTE[str] == " "
    str++
  RETURN str

PUB StringToInt(str, pos) : int | ssign
  int~
  ssign~
  repeat
    case byte[str]
      "+":
      "-":
        ssign~~
      "0".."9":
        int := int * 10 + byte[str] - "0"
      other:
        quit
    ++str

  LONG[pos] := str

PUB StringToFloat(str, pos) : flt | significand, ssign, places, exp
  significand~
  ssign~
  exp~
  places~
  repeat
    case byte[str]
      "+":
      "-":
        ssign~~
      ".":
        places := 1
      "0".."9":
        significand := significand * 10 + byte[str] - "0"
        if places
          ++places                    'count decimal places
      other:
        quit
    ++str

  LONG[pos] := str

  if ssign
    -significand
  flt := f.FFloat(significand)

  if places
    exp += places - 1

  flt := f.FMul(flt, tenf[exp])              'adjust flt's decimal point

DAT

tenf    long  1e+00, 1e-01, 1e-02, 1e-03, 1e-04, 1e-05, 1e-06, 1e-07, 1e-08, 1e-09
        long  1e-10, 1e-11, 1e-12, 1e-13, 1e-14, 1e-15, 1e-16, 1e-17, 1e-18, 1e-19
        long  1e-20, 1e-21, 1e-22, 1e-23, 1e-24, 1e-25, 1e-26, 1e-27, 1e-28, 1e-29
        long  1e-30, 1e-31, 1e-32, 1e-33, 1e-34, 1e-35, 1e-36, 1e-37, 1e-38





{{
+------------------------------------------------------------------------------------------------------------------------------+
|                                                   TERMS OF USE: MIT License                                                  |
+------------------------------------------------------------------------------------------------------------------------------+
|Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation    |
|files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy,    |
|modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software|
|is furnished to do so, subject to the following conditions:                                                                   |
|                                                                                                                              |
|The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.|
|                                                                                                                              |
|THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE          |
|WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR         |
|COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,   |
|ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                         |
+------------------------------------------------------------------------------------------------------------------------------+
}}