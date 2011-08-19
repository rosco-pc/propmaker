CON
  _clkmode = xtal1+pll16x
  _clkfreq = 80_000_000

OBJ
  conio : "conio"
  f      : "F32"                                    ' F32 v1.3
  fs     : "FloatString"                            ' Replace F32 FloatString by FloatString v1.2

PUB start

PUB move(pos)

PUB setTemperature(S)
  conio.str(STRING("Set extruder temperature to "))
  conio.str(fs.FloatToString(S))
  conio.tx(13)

PUB getTemperature
  RETURN 0

PUB setBedTemperature(S)
  conio.str(STRING("Set bed temperature to "))
  conio.str(fs.FloatToString(S))
  conio.tx(13)

PUB getBedTemperature
  RETURN 128

PUB  setCooler(S)
  conio.str(STRING("Set cooler temperature to "))
  conio.str(fs.FloatToString(S))
  conio.tx(13)

PUB waitForTemperature

PUB valveSet(S, T)
  conio.str(STRING("Open extruder valve to "))
  conio.str(fs.FloatToString(S))
  conio.str(STRING(" for "))
  conio.dec(T)
  conio.tx(13)

PUB newExtruder(T)
  conio.str(STRING("Change extruder head to "))
  conio.dec(T)
  conio.tx(13)





