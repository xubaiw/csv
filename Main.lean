import CSV

def main : IO Unit := do
  IO.println <| CSV.parse "a,\"b\nc\"\r\n1,2"
  IO.println <| CSV.compress #[#["a\r\n", "b,", "c"], #["1", "2", "3"]]
