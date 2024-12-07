package misc_asm

import "core:testing"
import "core:unicode/utf8"

@(test)
test_scan :: proc(t: ^testing.T) {
    input := "LDA $4f"
    l := lexer_init(input)

    res := lex_scan(&l)

    testing.expect_value(t, res.type, TokenType.Instruction)
    testing.expect_value(t, res.text, "LDA")
    
    res = lex_scan(&l)

    testing.expect_value(t, res.type, TokenType.HexNumber)
    testing.expect_value(t, res.text, "4f")
}
