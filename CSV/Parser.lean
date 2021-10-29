/-
  Copyright (c) 2021 Xubai Wang. All rights reserved.
  Released under Apache 2.0 license as described in the file LICENSE.
  Authors: Xubai Wang
-/

import Lean.Data.Parsec
import CSV.Basic

open Lean Parsec

namespace CSV

/--
  Many arrays of `p` with the same size.
-/
partial def manyHomoCore (p : Parsec $ Array α) (acc : Array $ Array α) : Parsec $ Array $ Array α :=
  (do 
    let first ← p
    if acc.size = 0 then
      manyCore p (acc.push first)
    else
      if acc.back.size = first.size then 
        manyCore p (acc.push first)
      else 
        fail "expect same size"
  )
  <|> pure acc

/--
  Many `p` separated by `s`.
-/
def manySep (p : Parsec α) (s : Parsec β) : Parsec $ Array α := do
  manyCore (attempt (s *> p)) #[←p]

/--
  Many arrays of `p` with the same size separated by `s`.
-/
def manySepHomo (p : Parsec $ Array α) (s : Parsec β) : Parsec $  Array $ Array α := do
  manyHomoCore (attempt (s *> p)) #[←p]

/-
  The following definitions are adapted from https://datatracker.ietf.org/doc/html/rfc4180
-/

def TEXTDATA : Parsec Char := satisfy fun c =>
  0x20 ≤ c.val ∧ c.val ≤ 0x21 ∨
  0x23 ≤ c.val ∧ c.val ≤ 0x2B ∨ 
  0x2D ≤ c.val ∧ c.val ≤ 0x7E

def CR : Parsec Char := pchar '\r'
def LF : Parsec Char := pchar '\n'
def CRLF : Parsec String := pstring "\r\n"
def COMMA : Parsec Char := pchar ','
def DQUOTE : Parsec Char := pchar '\"'
def «2DQUOTE»  : Parsec Char := pstring "\"\"" *> pure '\"'

def escaped : Parsec String := 
  DQUOTE *>
  manyChars (TEXTDATA <|> COMMA <|> CR <|> LF <|> «2DQUOTE»)
  <* DQUOTE

def «non-escaped» : Parsec String := manyChars TEXTDATA

def field : Parsec Field := escaped <|> «non-escaped»

def record : Parsec Record := manySep field COMMA

def file : Parsec $ Array Record := manySepHomo record (CRLF <* notFollowedBy eof) <* (optional CRLF) <* eof

def parse (s : String) : Except String $ Array $ Array $ String :=
  match file s.mkIterator with
  | Parsec.ParseResult.success _ res => Except.ok res
  | Parsec.ParseResult.error it err  => Except.error s!"offset {it.i.repr}: {err}"

end CSV