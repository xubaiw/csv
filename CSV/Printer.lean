/-
  Copyright (c) 2021 Xubai Wang. All rights reserved.
  Released under Apache 2.0 license as described in the file LICENSE.
  Authors: Xubai Wang
-/

import Lean.Data.Parsec
import CSV.Basic

namespace CSV

def String.joinSep (a : Array String) (d : String) : String := do
  if a.size = 0 then "" else
    let mut res := a.get! 0
    for s in a.toSubarray (start := 1) do
      res := res ++ d ++ s
    res

def isTEXTDATA (c : Char) : Bool := 
  0x20 ≤ c.val ∧ c.val ≤ 0x21 ∨
  0x23 ≤ c.val ∧ c.val ≤ 0x2B ∨ 
  0x2D ≤ c.val ∧ c.val ≤ 0x7E

def escapeChar (c : Char) : String := 
  if c = '"' then "\"\"" else String.mk [c]

def escape (s : String) : String :=
  match s.foldl (fun (acc, flag) char => if isTEXTDATA char then (acc.push char, flag) else (acc ++ (escapeChar char), true)) ("", false) with
  | (s, false) => s
  | (s, true)  => "\"" ++ s ++ "\""

def compress (c : Array Record) : String := String.joinSep (c.map (fun record => String.joinSep (record.map escape) ",")) "\r\n"

end CSV