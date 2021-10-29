# CSV

Parse CSV file using the parser combinator, based on the grammer of [RFC 4180](http://tools.ietf.org/html/rfc4180).

```lean
import CSV

#eval CSV.parse "a,\"b\nc\"\r\n1,2"
-- Except.ok #[#["a", "b\nc"], #["1", "2"]]
```

## Todo
- Customizable separators and other configs
- Typed reader
- Read by row