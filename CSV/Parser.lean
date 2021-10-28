/-
  Copyright (c) 2021 Xubai Wang. All rights reserved.
  Released under Apache 2.0 license as described in the file LICENSE.
  Authors: Xubai Wang
-/

import Lean.Data.Parsec
import CSV.Basic

open Lean Parsec

namespace CSV

/-
  file = [header CRLF] record *(CRLF record) [CRLF]

  header = name *(COMMA name)

  record = field *(COMMA field)

  name = field

  field = (escaped / non-escaped)

  escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE

  non-escaped = *TEXTDATA

  COMMA = %x2C

  CR = %x0D ;as per section 6.1 of RFC 2234 [2]

  DQUOTE =  %x22 ;as per section 6.1 of RFC 2234 [2]

  LF = %x0A ;as per section 6.1 of RFC 2234 [2]

  CRLF = CR LF ;as per section 6.1 of RFC 2234 [2]

  TEXTDATA =  %x20-21 / %x23-2B / %x2D-7E
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
def «2DQUOTE» : Parsec Char := DQUOTE *> DQUOTE

def escaped : Parsec String := 
  DQUOTE *>
  manyChars (TEXTDATA <|> COMMA <|> CR <|> LF <|> «2DQUOTE»)
  <* DQUOTE

def «non-escaped» : Parsec String := manyChars TEXTDATA

def field : Parsec String := escaped <|> «non-escaped»

def record : Parsec $ Array String := do 
  let first ← field
  manyCore (COMMA *> field) #[first]

def file : Parsec $ Array $ Array String := do
  let first ← record
  manyCore (CRLF *> record) #[first]
  <* optional CRLF
  <* eof

def parse (s : String) : Except String (Array $ Array String) :=
  match file s.mkIterator with
  | Parsec.ParseResult.success _ res => Except.ok res
  | Parsec.ParseResult.error it err  => Except.error s!"offset {it.i.repr}: {err}" 

end CSV