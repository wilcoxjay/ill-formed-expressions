{

open Lexing
open ILLPLParser

}

let numlit =
  ['0'-'9']*

let var =
  ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*

let comment =
  "//"[^'\n' '\r']*

let white =
  [' ' '\t']+

let line =
  '\r' | '\n' | "\r\n"


rule token = parse
  | "true"     { TRUE     }
  | "false"    { FALSE    }
  | "&&"       { CONJ     }
  | "||"       { DISJ     }
  | "==>"      { IMPLIES  }
  | "+"        { ADD      }
  | "-"        { SUB      }
  | "*"        { MUL      }
  | "/"        { DIV      }
  | "=="       { EQEQ     }
  | "!="       { NEQ      }
  | "<"        { LT       }
  | "<="       { LE       }
  | ">"        { GT       }
  | ">="       { GE       }

  | "="        { EQ       }
  | ","        { COMMA    }
  | ";"        { SEMI     }
  | "."        { DOT      }
  | ".."       { DOTDOT   }
  | "!"        { BANG     }
  | "|"        { PIPE     }

  | "("        { LPAREN   }
  | ")"        { RPAREN   }
  | "["        { LSQUARE  }
  | "]"        { RSQUARE  }

  | "fun"      { FUN      }
  | "val"      { VAL      }
  | "let"      { LET      }
  | "in"       { IN       }
  | "if"       { IF       }
  | "then"     { THEN     }
  | "else"     { ELSE     }
  | "forall"   { FORALL   }
  | "exists"   { EXISTS   }
  | "assert"   { ASSERT   }
  | "requires" { REQUIRES }
  | "ensures"  { ENSURES  }

  | numlit as x { NUMLIT x }
  | var    as x { VAR x    }

  (* ignore *)
  | comment { token lexbuf }
  | white   { token lexbuf }
  | line    { new_line lexbuf; token lexbuf }

  | eof { EOF }

  (* error *)
  | _ as c
    { raise (Parser.LexicalError
        (Printf.sprintf "Unexpected char: %c" c)) }
