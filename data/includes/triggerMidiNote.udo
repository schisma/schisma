opcode triggerMidiNote, 0, kii
  kchannelToCheck, ieventParametersFtn, inotesStateFtn xin

  kstatus, kchannel, kdata1, kdata2 midiin

  ksustainedPedalStateIndex init 257

  if (kchannel == kchannelToCheck) then
    if (kstatus == 144) then
      if (kdata2 > 0) then
        tabw 1, kdata1 + 1, inotesStateFtn, 0

        ksustainPedalActive tab ksustainedPedalStateIndex, inotesStateFtn, 0
        if (ksustainPedalActive > 0) then
          tabw 1, kdata1 + 129, inotesStateFtn, 0
        endif

        kparams[] tab2array ieventParametersFtn
        ktargetInstrumentNumber = kparams[0] + (kdata1 * 0.000001)
        kparams[0] = ktargetInstrumentNumber

        turnoff2 ktargetInstrumentNumber, 12, 1
        schedulek kparams
      else
        tabw 0, kdata1 + 1, inotesStateFtn, 0

        kNoteSustained tab kdata1 + 129, inotesStateFtn, 0
        if (kNoteSustained == 0) then
          ktargetInstrumentNumber tab 0, inotesStateFtn, 0
          ktargetInstrumentNumber += (kdata1 * 0.000001)

          turnoff2 ktargetInstrumentNumber, 12, 1
        endif
      endif
    elseif (kstatus == 128) then
      tabw 0, kdata1 + 1, inotesStateFtn, 0

      kNoteSustained tab kdata1 + 129, inotesStateFtn, 0
      if (kNoteSustained == 0) then
        ktargetInstrumentNumber tab 0, inotesStateFtn, 0
        ktargetInstrumentNumber += (kdata1 * 0.000001)

        turnoff2 ktargetInstrumentNumber, 12, 1
      endif
    elseif (kstatus == 176 && kdata1 == 64) then
      if (kdata2 > 0) then
        tabw 1, ksustainedPedalStateIndex, inotesStateFtn, 0

        kplayedNoteIndex = 1
        while (kplayedNoteIndex <= 128) do
          kplayedNote tab kplayedNoteIndex, inotesStateFtn, 0
          ksustainedNoteIndex = kplayedNoteIndex + 128
          tabw kplayedNote, ksustainedNoteIndex, inotesStateFtn, 0

          kplayedNoteIndex += 1
        od
      else
        tabw 0, ksustainedPedalStateIndex, inotesStateFtn, 0

        ksustainedNoteIndex = 129
        while (ksustainedNoteIndex <= 256) do
          tabw 0, ksustainedNoteIndex, inotesStateFtn, 0
          ksustainedNoteIndex += 1
        od


        ktargetInstrumentNumber tab 0, inotesStateFtn, 0

        kplayedNoteIndex = 1
        while (kplayedNoteIndex <= 128) do
          kplayedNote tab kplayedNoteIndex, inotesStateFtn, 0
          if (kplayedNote == 0) then
            kfractionalInstrumentNumber = ktargetInstrumentNumber + ((kplayedNoteIndex - 1) * 0.000001)
            turnoff2 kfractionalInstrumentNumber, 12, 1
          endif
          kplayedNoteIndex += 1
        od
      endif
    endif
  endif
endop
