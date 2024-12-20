package misc_asm

import "core:fmt"
import "core:strings"
import "core:unicode"
import "core:unicode/utf8"
import vmem "core:mem/virtual"

HexLetters: bit_set['A'..='f'] = {'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f'}

TokenType :: enum {
    Invalid,
    Instruction,
    Register,
    DecimalNumber,
    HexNumber,
    Symbol,
    Comma,
    Hash,
    Colon,
    Comment,
    EOF,
}

Token :: struct {
    type: TokenType,
    text: string,
    line: int,
    col: int,
}

Lexer :: struct {
    input: []rune,
    pos: int,
    line: int,
    col: int,
    arena: vmem.Arena,
}

lexer_init :: proc(input: string) -> Lexer {
    l := Lexer{}
    l.arena = vmem.Arena{}
    alloc := vmem.arena_allocator(&l.arena)
    l.input = utf8.string_to_runes(input, allocator = alloc)
    l.line = 1
    l.col = 1

    return l
}

lexer_peek :: proc(l: ^Lexer) -> rune {
    if l.pos >= len(l.input) {
        return 0
    }

    return l.input[l.pos]
}

lexer_consume :: proc(l: ^Lexer) -> rune {
    if l.pos >= len(l.input) {
        return 0
    }

    r := l.input[l.pos]
    l.pos += 1

    if r == '\n' {
        l.col = 0
        l.line += 1
    }

    l.col += 1
    return r
}

lex_scan_all :: proc(l: ^Lexer) -> []Token {
    context.allocator = vmem.arena_allocator(&l.arena)
    toks := make([dynamic]Token)

    for {
        tok := lex_scan(l)
        append(&toks, tok)
        if tok.type == .EOF do break
    }

    return toks[:]
}

lex_scan :: proc(l: ^Lexer) -> (tok: Token) {
    context.allocator = vmem.arena_allocator(&l.arena)

    for unicode.is_space(lexer_peek(l)) {
        lexer_consume(l)
    }

    tok.line = l.line
    tok.col = l.col

    r := lexer_peek(l)
    switch r {
    case 0:
        tok.type = .EOF

    case ',':
        lexer_consume(l)
        tok.type = .Comma
        tok.text = ","

    case '#':
        lexer_consume(l)
        tok.type = .Hash
        tok.text = "#"

    case ':':
        lexer_consume(l)
        tok.type = .Colon
        tok.text = ":"

    case ';':
        tok.type = .Comment
        text: strings.Builder
        strings.builder_init(&text)
        for lexer_peek(l) != '\n' && lexer_peek(l) != 0 {
            ch := lexer_consume(l)
            strings.write_rune(&text, ch)
        }
        tok.text = strings.to_string(text)

    case '0'..='9':
        tok.type = .DecimalNumber
        text: strings.Builder
        strings.builder_init(&text)
        for unicode.is_digit(lexer_peek(l)) {
            ch := lexer_consume(l)
            strings.write_rune(&text, ch)
        }
        tok.text = strings.to_string(text)

    case '$':
        lexer_consume(l) // consume $
        tok.type = .HexNumber
        text: strings.Builder
        strings.builder_init(&text)
        for unicode.is_digit(lexer_peek(l)) || (lexer_peek(l) in HexLetters) {
            ch := lexer_consume(l)
            strings.write_rune(&text, ch)
        }
        tok.text = strings.to_string(text)

    case 'x', 'X':
        lexer_consume(l)
        tok.type = .Register
        tok.text = "x"

    case 'y', 'Y':
        lexer_consume(l)
        tok.type = .Register
        tok.text = "y"

    case 'z', 'Z':
        lexer_consume(l)
        tok.type = .Register
        tok.text = "z"

    case 'w', 'W':
        lexer_consume(l)
        tok.type = .Register
        tok.text = "w"

    case:
        tok.type = .Symbol
        text: strings.Builder
        strings.builder_init(&text)
        for unicode.is_alpha(lexer_peek(l)) || unicode.is_digit(lexer_peek(l)) || lexer_peek(l) == '_' {
            ch := lexer_consume(l)
            strings.write_rune(&text, ch)
        }
        tok.text = strings.to_upper(strings.to_string(text))

        if _, ok := Instructions[tok.text]; ok {
            tok.type = .Instruction
        }
    }

    return
}
