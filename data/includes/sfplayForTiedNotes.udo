opcode sfplayForTiedNotes, aa, iiaaiooo
  ivel, inotenum, aamp, afreq, ipreindex, iflag, ioffset, ienv xin

  tigoto skipInit

  aleft, aright sfplay ivel, inotenum, aamp, afreq, ipreindex, iflag, ioffset, ienv

  xout aleft, aright

  skipInit:
endop

opcode sfplayForTiedNotes, aa, iikkiooo
  ivel, inotenum, kamp, kfreq, ipreindex, iflag, ioffset, ienv xin

  tigoto skipInit

  aleft, aright sfplay ivel, inotenum, kamp, kfreq, ipreindex, iflag, ioffset, ienv

  xout aleft, aright

  skipInit:
endop

opcode sfplayForTiedNotes, aa, iiiiiooo
  ivel, inotenum, iamp, ifreq, ipreindex, iflag, ioffset, ienv xin

  tigoto skipInit

  aleft, aright sfplay ivel, inotenum, iamp, ifreq, ipreindex, iflag, ioffset, ienv

  xout aleft, aright

  skipInit:
endop
