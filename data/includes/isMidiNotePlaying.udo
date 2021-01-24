opcode isMidiNotePlaying, kk, k
  kchannelToCheck xin

  kstatus, kchannel, kdata1, kdata2 midiin

  if (kchannel == kchannelToCheck) then
    if (kstatus == 144) then
      if (kdata2 > 0) then
        kplaying = 1
        knotenumber = kdata1
      else
        kplaying = -1
        knotenumber = kdata1
      endif
    elseif (kstatus == 128) then
      kplaying = -1
      knotenumber = kdata1
    else
      kplaying = 0
      knotenumber = -1
    endif
  else
    kplaying = 0
    knotenumber = -1
  endif

  xout kplaying, knotenumber
endop
