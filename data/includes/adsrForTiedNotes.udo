opcode adsrForTiedNotes, k, iiii
  iattack, idecay, isustain, irelease xin

  xtratim irelease

  tigoto skipInit

  kenvelope linsegr 0, iattack, 1, idecay, isustain, irelease, 0

  xout kenvelope

  skipInit:
endop
