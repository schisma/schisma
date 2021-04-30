opcode round2, a, a
  aval xin

  kval = k(aval)

  if (frac(kval) == 0.5) then
    if (kval < 0) then
      around = floor(aval)
    else
      around = ceil(aval)
    endif
  else
    around = round(aval)
  endif

  xout around
endop

opcode round2, k, k
  kval xin

  if (frac(kval) == 0.5) then
    if (kval < 0) then
      kround = floor(kval)
    else
      kround = ceil(kval)
    endif
  else
    kround = round(kval)
  endif

  xout kround
endop

opcode round2, i, i
  ival xin

  if (frac(ival) == 0.5) then
    if (ival < 0) then
      iround = floor(ival)
    else
      iround = ceil(ival)
    endif
  else
    iround = round(ival)
  endif

  xout iround
endop
