package misc_asm

import "core:fmt"
import vmem "core:mem/virtual"
import "core:strconv"
import "core:strings"

AddrMode :: enum u8 {
    Implied,
    Immediate,
    Register,
    Absolute,
    ZeroPage,
    IndexedZeroPage,
    IndexedAbsolute,
    Relative,
}

@(private = "package")
Instructions := map[string]u8 {
    "NOP" = 0x00, "LDA" = 0x01, "LDR" = 0x02, "CLR" = 0x03,
    "SWP" = 0x04, "CMP" = 0x05, "ADD" = 0x06, "SUB" = 0x07,
    "MUL" = 0x08, "HLT" = 0x09, "AND" = 0x0A, "OR"  = 0x0B,
    "XOR" = 0x0C, "STA" = 0x0D, "JLT" = 0x0E, "JGT" = 0x0F,
    "JLE" = 0x10, "JGE" = 0x11, "JEQ" = 0x12, "JNE" = 0x13,
    "PSH" = 0x14, "POP" = 0x15, "TST" = 0x16, "BIT" = 0x17,
    "ASL" = 0x18, "ADR" = 0x19, "DBG" = 0x1A, "NIL" = 0x1B,
    "JMP" = 0x1C, "STR" = 0x1D, "INC" = 0x1E, "DEC" = 0x1F,
}

RegisterByte := map[rune]u8 {
    'X' = 0x00, 'Y' = 0x01, 'Z' = 0x02, 'W' = 0x03,
    'x' = 0x00, 'y' = 0x01, 'z' = 0x02, 'w' = 0x03,
}

Program :: []Instruction

Instruction :: struct {
    tokens: []Token,
    operands: []Operand,
    addr_mode: AddrMode,
}

Operand :: union #no_nil {
    ByteLiteral,
    AddrLiteral,
    RegLiteral
}

ByteLiteral :: distinct byte
AddrLiteral :: distinct u16
RegLiteral  :: distinct rune

Parser :: struct {
    tokens: []Token,
    pos: int,
    errors: [dynamic]ParseError,
    arena: vmem.Arena,
}

ParseError :: struct {
    kind: ParseErrorKind,
    token: Token,
}

ParseErrorKind :: enum {
    UnexpectedToken,
    StrConvFailed,
    TooManyOperands,
    InvalidOperand,
}

@(private = "file")
error :: proc(p: ^Parser, token: Token, kind: ParseErrorKind = .UnexpectedToken) {
    append(&p.errors, ParseError{kind, token})
}

parser_init :: proc(parser: ^Parser, tokens: []Token) {
    alloc := vmem.arena_allocator(&parser.arena)
    parser.tokens = tokens
    parser.errors = make([dynamic]ParseError)
}

// look at current token
@(private = "file")
peek :: proc(p: ^Parser) -> Token {
    if p.pos >= len(p.tokens) {
        return Token { type = .EOF }
    }

    return p.tokens[p.pos]
}

// look at next token
@(private = "file")
peek_next :: proc(p: ^Parser) -> Token {
    if p.pos >= len(p.tokens) - 1 {
        return Token { type = .EOF }
    }

    return p.tokens[p.pos + 1]
}

// "soft" assert current token's type
match :: proc(p: ^Parser, t: TokenType) -> bool {
    return peek(p).type == t
}

// "soft" assert current token's type against a set
match_set :: proc(p: ^Parser, t: bit_set[TokenType]) -> bool {
    return peek(p).type in t
}

// assert current token's type - error on mismatch
expect :: proc(p: ^Parser, t: TokenType) -> bool {
    if peek(p).type != t {
        error(p, peek(p))
        return false
    }

    return true
}

// assert current token's type against a set - error on mismatch
expect_set ::  proc(p: ^Parser, t: bit_set[TokenType]) -> bool {
    if peek(p).type not_in t {
        error(p, peek(p))
        return false
    }

    return true
}

// assert next token's type - error on mismatch
expect_next :: proc(p: ^Parser, t: TokenType) -> bool {
    if peek_next(p).type != t {
        error(p, peek_next(p))
        return false
    }

    return true
}

// return current token and advance past it
@(private = "file")
consume :: proc(p: ^Parser) -> Token {
    t := peek(p)
    p.pos += 1
    return t
}

parse_register_operand :: proc(parser: ^Parser, tokens: ^[dynamic]Token, operands: ^[dynamic]Operand) -> (ok: bool) {
    expect(parser, .Register) or_return

    register_token := consume(parser)
    register: RegLiteral

    switch register_token.text {
    case "x", "X": register = 0x00
    case "y", "Y": register = 0x01
    case "z", "Z": register = 0x02
    case "w", "W": register = 0x03
    case:
        error(parser, register_token, .InvalidOperand)
        return
    }

    append(tokens, register_token)
    append(operands, register)
    return true
}

parse_value_operand :: proc(parser: ^Parser, tokens: ^[dynamic]Token, operands: ^[dynamic]Operand) -> (mode: AddrMode) {
    #partial switch peek(parser).type {
    case .Hash:
        mode = .Immediate
        consume(parser) // discard #
        expect_set(parser, {.Number, .HexNumber})

        value_token := consume(parser)
        append(tokens, value_token)

        value: ByteLiteral
        if value_token.type == .Number {
            raw_value, conversion_ok := strconv.parse_int(value_token.text, 10)
            if !conversion_ok {
                error(parser, value_token, .StrConvFailed)
            }
            value = ByteLiteral(raw_value)
        }

        if value_token.type == .HexNumber {
            raw_value, conversion_ok := strconv.parse_int(value_token.text, 16)
            if !conversion_ok {
                error(parser, value_token, .StrConvFailed)
            }
            value = ByteLiteral(raw_value)
        }

        append(operands, value)
        mode = .Immediate

    case .HexNumber:
        address_token := consume(parser)
        append(tokens, address_token)

        raw_value, conversion_ok := strconv.parse_int(address_token.text, 16)
        if !conversion_ok {
            error(parser, address_token, .StrConvFailed)
        }

        address := AddrLiteral(raw_value)
        append(operands, address)

        if address > 255 {
            mode = .Absolute
        } else {
            mode = .ZeroPage
        }

        // indexed modes
        if match(parser, .Comma) {
            consume(parser) // discard comma
            parse_register_operand(parser, tokens, operands)

            if mode == .ZeroPage {
                mode = .IndexedZeroPage
            } else {
                mode = .IndexedAbsolute
            }
        }

    case .Register:
        parse_register_operand(parser, tokens, operands)
        mode = .Register

    case:
        error(parser, peek(parser))
    }

    return
}

parse_instruction :: proc(parser: ^Parser) -> (instruction: Instruction) {
    expect(parser, .Instruction)
    context.allocator = vmem.arena_allocator(&parser.arena)
    tokens := make([dynamic]Token)
    operands := make([dynamic]Operand)

    instruction_token := consume(parser)
    append(&tokens, instruction_token)

    raw_inst := strings.to_upper(instruction_token.text)
    switch raw_inst {
    case "LDR":
        instruction = parse_ldr(parser, &tokens, &operands)

    case "CLR":
        instruction = parse_clr(parser, &tokens, &operands)

    case "SWP":
        instruction = parse_swp(parser, &tokens, &operands)

    case:
        instruction.addr_mode = parse_value_operand(parser, &tokens, &operands)

    }

    instruction.tokens = tokens[:]
    instruction.operands = operands[:]
    return
}

parse_lda :: proc(parser: ^Parser, tokens: ^[dynamic]Token, operands: ^[dynamic]Operand) -> (instruction: Instruction) {
    instruction.addr_mode = parse_value_operand(parser, tokens, operands)
    return
}

parse_ldr :: proc(parser: ^Parser, tokens: ^[dynamic]Token, operands: ^[dynamic]Operand) -> (instruction: Instruction) {
    instruction.addr_mode = .Implied
    parse_register_operand(parser, tokens, operands)

    if !match_set(parser, {.EOF, .Instruction}) {
        instruction.addr_mode = parse_value_operand(parser, tokens, operands)
    }

    if match(parser, .Comma) {
        consume(parser) // discard comma
        instruction.addr_mode = .Register
        parse_register_operand(parser, tokens, operands)
    }

    return
}

parse_clr :: proc(parser: ^Parser, tokens: ^[dynamic]Token, operands: ^[dynamic]Operand) -> (instruction: Instruction) {
    if !match(parser, .Register) {
        instruction.addr_mode = .Implied
        return instruction
    }

    for _ in 0..<4 {
        match(parser, .Register) or_break
        instruction.addr_mode = .Register
        parse_register_operand(parser, tokens, operands)
    }

    return
}

parse_swp :: proc(parser: ^Parser, tokens: ^[dynamic]Token, operands: ^[dynamic]Operand) -> (instruction: Instruction) {
    instruction.addr_mode = .Implied
    parse_register_operand(parser, tokens, operands)

    // a single operand swaps with ACC, two swap with each other
    if match(parser, .Register) {
        instruction.addr_mode = .Register
        parse_register_operand(parser, tokens, operands)
    }

    return
}

