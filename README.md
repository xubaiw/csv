# CSV

Parse and write CSV string, based on the grammer of [RFC 4180](http://tools.ietf.org/html/rfc4180).

```lean
import CSV

#eval CSV.parse "a,\"b\nc\"\r\n1,2"
-- Except.ok #[#["a", "b\nc"], #["1", "2"]]

#eval CSV.compress #[#["a\r\n", "b,", "c"], #["1", "2", "3"]]
-- "\"a\x0d\n\",\"b,\",c\x0d\n1,2,3"
```

## Todo
- Customizable separators and other configs
- Typed reader
- Read by row