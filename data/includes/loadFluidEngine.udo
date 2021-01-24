opcode loadFluidEngine, i, Siiii
  SsoundFontPath, ienableChorus, ienableReverb, ichannels, ivoices xin

  SengineLoadedIdentifier strcat "__engine_loaded_", SsoundFontPath
  SengineNumberIdentifier strcat "__engine_number_", SsoundFontPath

  iengineLoaded chnget SengineLoadedIdentifier

  if (iengineLoaded == 0) then
    iengine fluidEngine ienableChorus, ienableReverb, ichannels, ivoices

    chnset 1, SengineLoadedIdentifier
    chnset iengine, SengineNumberIdentifier
  endif

  iengineNumber chnget SengineNumberIdentifier

  xout iengineNumber
endop
