opcode midiAmplitudeAndFrequency, kk, k
  kchannelToCheck xin

  kstatus, kchannel, kdata1, kdata2 midiin

  if (kchannel == kchannelToCheck) then
    if (kstatus == 144) then
      kfrequency = cpsmidinn(kdata1)

      if (kdata2 > 0) then
        kamplitude scale kdata2, 1, 0, 127, 0
      else
        kamplitude = 0
      endif
    elseif (kstatus == 128) then
      kamplitude = 0
      kfrequency = cpsmidinn(kdata1)
    else
      kamplitude = 0
      kfrequency = 0
    endif
  else
    kamplitude = 0
    kfrequency = 0
  endif

  xout kamplitude, kfrequency
endop
