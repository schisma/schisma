opcode midiChannelMatches, k, k
  kchannelToCheck xin

  kstatus, kchannel, kdata1, kdata2 midiin

  if (kchannel == kchannelToCheck) then
    kmatch = 1
  else
    kmatch = 0
  endif

  xout kmatch
endop
