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

ParseError :: union {
    UnexpectedTokenErr,
    ParseNumberErr,
    TooManyOperandsErr,
    InvalidValueErr,
}

UnexpectedTokenErr :: struct {
    token: Token,
    expected: bit_set[TokenType],
}

ParseNumberErr :: struct {
    token: Token,
    target_type: TokenType,
}

TooManyOperandsErr :: struct {
    token: Token,
    max_allowed: int,
}

InvalidValueErr :: struct {
    token: Token,
    value: int,
    max_allowed: int,
}

parse_error :: proc(p: ^Parser, err: ParseError) {
    append(&p.errors, err)
}

parser_init :: proc(parser: ^Parser, tokens: []Token) {
    alloc := vmem.arena_allocator(&parser.arena)
    parser.tokens = tokens
    parser.errors = make([dynamic]ParseError, alloc)
}

parse_all :: proc(parser: ^Parser) -> []Instruction {
    context.allocator = vmem.arena_allocator(&parser.arena)
    res := make([dynamic]Instruction)

    for !match(parser, .EOF) {
        append(&res, parse_instruction(parser))
    }

    return res[:]
}

// look at current token
parser_peek :: proc(p: ^Parser) -> Token {
    if p.pos >= len(p.tokens) {
        return Token { type = .EOF }
    }

    return p.tokens[p.pos]
}

// look at next token
parser_peek_next :: proc(p: ^Parser) -> Token {
    if p.pos >= len(p.tokens) - 1 {
        return Token { type = .EOF }
    }

    return p.tokens[p.pos + 1]
}

// "soft" assert current token's type
match :: proc(p: ^Parser, t: TokenType) -> bool {
    return parser_peek(p).type == t
}

// "soft" assert current token's type against a set
match_set :: proc(p: ^Parser, t: bit_set[TokenType]) -> bool {
    return parser_peek(p).type in t
}

// assert current token's type - error on mismatch
expect :: proc(parser: ^Parser, type: TokenType) -> bool {
    token := parser_peek(parser)
    if token.type != type {
        parse_error(parser, UnexpectedTokenErr{token, {type}})

        for !match_set(parser, {.EOF, .Instruction}) {
            consume(parser)
        }

        return false
    }

    return true
}

// assert current token's type against a set - error on mismatch
expect_set ::  proc(parser: ^Parser, type: bit_set[TokenType]) -> bool {
    token := parser_peek(parser)
    if token.type not_in type {
        parse_error(parser, UnexpectedTokenErr{token, type})

        for !match_set(parser, {.EOF, .Instruction}) {
            consume(parser) // reach EOF or next instruction
        }

        return false
    }

    return true
}

// assert next token's type - error on mismatch
expect_next :: proc(parser: ^Parser, type: TokenType) -> bool {
    token := parser_peek_next(parser)
    if token.type != type {
        parse_error(parser, UnexpectedTokenErr{token, {type}})

        for !match_set(parser, {.EOF, .Instruction}) {
            consume(parser)
        }

        return false
    }

    return true
}

// return current token and advance past it
@(private = "file")
consume :: proc(p: ^Parser) -> Token {
    t := parser_peek(p)
    p.pos += 1
    return t
}

parse_register_operand :: proc(parser: ^Parser, tokens: ^[dynamic]Token, operands: ^[dynamic]Operand) {
    register_token := consume(parser)
    register: RegLiteral

    switch register_token.text {
    case "x", "X": register = 0x00
    case "y", "Y": register = 0x01
    case "z", "Z": register = 0x02
    case "w", "W": register = 0x03
    case: return
    }

    append(tokens, register_token)
    append(operands, register)
}

parse_addr_operand :: proc(parser: ^Parser, tokens: ^[dynamic]Token, operands: ^[dynamic]Operand) -> (mode: AddrMode) {
    address_token := consume(parser)
    append(tokens, address_token)

    raw_value, ok := strconv.parse_int(address_token.text, 16)
    if !ok {
        parse_error(parser, ParseNumberErr{address_token, .HexNumber})
        return
    }

    address := AddrLiteral(raw_value)
    append(operands, address)

    if address > 255 {
        mode = .Absolute
    } else {
        mode = .ZeroPage
    }

    if match(parser, .Comma) {
        consume(parser) // discard comma
        if !expect(parser, .Register) do return
        parse_register_operand(parser, tokens, operands)

        if mode == .ZeroPage {
            mode = .IndexedZeroPage
        } else {
            mode = .IndexedAbsolute
        }
    }

    return
}

parse_value_operand :: proc(parser: ^Parser, tokens: ^[dynamic]Token, operands: ^[dynamic]Operand) -> (mode: AddrMode) {
    token := parser_peek(parser)
    #partial switch token.type {
    case .Hash:
        mode = .Immediate
        consume(parser) // discard #
        expect_set(parser, {.DecimalNumber, .HexNumber})

        value_token := consume(parser)
        append(tokens, value_token)

        value: ByteLiteral
        if value_token.type == .DecimalNumber {
            raw_value, conversion_ok := strconv.parse_int(value_token.text, 10)
            if !conversion_ok {
                parse_error(parser, ParseNumberErr{value_token, .DecimalNumber})
            }
            value = ByteLiteral(raw_value)
        }

        if value_token.type == .HexNumber {
            raw_value, conversion_ok := strconv.parse_int(value_token.text, 16)
            if !conversion_ok {
                parse_error(parser, ParseNumberErr{value_token, .HexNumber})
            }
            value = ByteLiteral(raw_value)
        }

        append(operands, value)
        mode = .Immediate

    case .HexNumber:
        mode = parse_addr_operand(parser, tokens, operands)

    case .Register:
        parse_register_operand(parser, tokens, operands)
        mode = .Register

    case:
        parse_error(parser, UnexpectedTokenErr{token, {.Hash, .HexNumber, .Register}})
    }

    return
}

parse_jump_operand :: proc(parser: ^Parser, tokens: ^[dynamic]Token, operands: ^[dynamic]Operand) -> (mode: AddrMode) {
    address_token := consume(parser)
    append(tokens, address_token)

    value, conversion_ok := strconv.parse_int(address_token.text, 16)
    if !conversion_ok {
        parse_error(parser, ParseNumberErr{address_token, .HexNumber})
        return
    }

    if value <= 0xFF {
        mode = .Relative
    } else {
        mode = .Absolute
    }

    append(operands, AddrLiteral(value))
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

    case "JMP", "JLT", "JGT", "JLE", "JGE", "JEQ", "JNE":
        instruction = parse_jmp(parser, &tokens, &operands)

    case "STA":
        instruction = parse_sta(parser, &tokens, &operands)

    case "STR":
        instruction = parse_str(parser, &tokens, &operands)

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
    if !expect(parser, .Register) do return

    instruction.addr_mode = .Implied
    parse_register_operand(parser, tokens, operands)

    if !match_set(parser, {.EOF, .Instruction, .Comma}) {
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
    if !expect(parser, .Register) do return

    instruction.addr_mode = .Implied
    parse_register_operand(parser, tokens, operands)

    // a single operand swaps with ACC, two swap with each other
    if match(parser, .Register) {
        instruction.addr_mode = .Register
        parse_register_operand(parser, tokens, operands)
    }

    return
}

parse_jmp :: proc(parser: ^Parser, tokens: ^[dynamic]Token, operands: ^[dynamic]Operand) -> (instruction: Instruction) {
    if !expect(parser, .HexNumber) do return
    instruction.addr_mode = parse_jump_operand(parser, tokens, operands)
    return
}

parse_sta :: proc(parser: ^Parser, tokens: ^[dynamic]Token, operands: ^[dynamic]Operand) -> (instruction: Instruction) {
    if !expect(parser, .HexNumber) do return
    instruction.addr_mode = parse_addr_operand(parser, tokens, operands)
    return
}

parse_str :: proc(parser: ^Parser, tokens: ^[dynamic]Token, operands: ^[dynamic]Operand) -> (instruction: Instruction) {
    if !expect(parser, .Register) do return
    if !expect_next(parser, .HexNumber) do return

    parse_register_operand(parser, tokens, operands)
    instruction.addr_mode = parse_addr_operand(parser, tokens, operands)

    return
}
