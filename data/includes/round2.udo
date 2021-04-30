opcode round2, a, a
  aval xin

  if (frac(k(aval)) >= 0.5) then
    around = ceil(aval)
  else
    around = floor(aval)
  endif

  xout around
endop

opcode round2, k, k
  kval xin

  if (frac(kval) >= 0.5) then
    kround = ceil(kval)
  else
    kround = floor(kval)
  endif

  xout kround
endop

opcode round2, i, i
  ival xin

  if (frac(ival) >= 0.5) then
    iround = ceil(ival)
  else
    iround = floor(ival)
  endif

  xout iround
endop
