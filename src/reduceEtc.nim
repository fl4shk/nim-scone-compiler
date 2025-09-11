proc sconcat*(
  strSeq: seq[string]
): string =
  for idx in 0 ..< strSeq.len():
    result.add strSeq[idx]

proc orR*(
  condSeq: seq[bool]
): bool =
  result = false
  for idx in 0 ..< condSeq.len():
    result = result or condSeq[idx]
proc andR*(
  condSeq: seq[bool]
): bool =
  result = false
  for idx in 0 ..< condSeq.len():
    result = result and condSeq[idx]
