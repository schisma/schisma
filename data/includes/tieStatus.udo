opcode tieStatus, i, 0

  itie tival

  if (p3 = -1000000000) ithen
      ; this note is a standalone note of indeterminate duration
      itiestatus = -1

  elseif (p3 < 0 && itie == 0) ithen
      ; this is an initial note within a group of tied notes
      itiestatus = 0

  elseif (p3 < 0 && itie == 1) ithen
      ; this is a middle note within a group of tied notes
      itiestatus = 1

  elseif (p3 > 0 && itie == 1) ithen
      ; this is an end note out of a group of tied notes
      itiestatus = 2

  elseif (p3 > 0 && itie == 0) ithen
      ; this note is a standalone note
      itiestatus = -1

  endif

  xout itiestatus

endop
