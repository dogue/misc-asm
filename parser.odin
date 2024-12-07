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
    arena: vmem.Arena,
}

parser_init :: proc(t: []Token) -> Parser {
    return Parser { tokens = t }
}

@(private = "file")
peek :: proc(p: ^Parser) -> Token {
    if p.pos >= len(p.tokens) {
        return Token { type = .EOF }
    }

    return p.tokens[p.pos]
}

@(private = "file")
peek_next :: proc(p: ^Parser) -> Token {
    if p.pos >= len(p.tokens) - 1 {
        return Token { type = .EOF }
    }

    return p.tokens[p.pos + 1]
}

match :: proc(p: ^Parser, t: TokenType) -> bool {
    return peek(p).type == t
}

match_set :: proc(p: ^Parser, t: bit_set[TokenType]) -> bool {
    return peek(p).type in t
}

expect :: proc(p: ^Parser, t: TokenType) -> bool {
    return peek_next(p).type == t
}

@(private = "file")
consume :: proc(p: ^Parser) -> Token {
    t := peek(p)
    p.pos += 1
    return t
}

parse_ivalue :: proc(p: ^Parser, toks: ^[dynamic]Token, oper: ^[dynamic]Operand) -> (ok: bool) {
    consume(p) // discard #
    match_set(p, {.Number, .HexNumber}) or_return
    value_tok := consume(p)
    append(toks, value_tok)

    value: ByteLiteral
    if value_tok.type == .Number {
        value = ByteLiteral(strconv.parse_int(value_tok.text, 10) or_return)
    }

    if value_tok.type == .HexNumber {
        value = ByteLiteral(strconv.parse_int(value_tok.text, 16) or_return)
    }

    append(oper, value)
    return true
}

parse_addr :: proc(p: ^Parser, toks: ^[dynamic]Token, oper: ^[dynamic]Operand) -> (mode: AddrMode, ok: bool) {
    addr_tok := consume(p)
    append(toks, addr_tok)
    addr := AddrLiteral(strconv.parse_int(addr_tok.text, 16) or_return)
    append(oper, addr)

    if addr > 255 {
        mode = .Absolute
    } else {
        mode = .ZeroPage
    }

    if match(p, .Comma) {
        consume(p) // discard comma
        match(p, .Register) or_return
        reg_tok := consume(p)
        reg := parse_reg_id(reg_tok) or_return
        append(toks, reg_tok)
        append(oper, reg)

        if mode == .ZeroPage {
            mode = .IndexedZeroPage
        } else {
            mode = .IndexedAbsolute
        }
    }

    return mode, true
}

parse_reg_id :: proc(t: Token) -> (rl: RegLiteral, ok: bool) {
    if t.type != .Register do return

    switch t.text {
    case "x", "X": return RegLiteral(0x00), true
    case "y", "Y": return RegLiteral(0x01), true
    case "z", "Z": return RegLiteral(0x02), true
    case "w", "W": return RegLiteral(0x03), true
    }

    return
}

parse_instruction :: proc(p: ^Parser) -> (inst: Instruction, ok: bool) {
    match(p, .Instruction) or_return
    context.allocator = vmem.arena_allocator(&p.arena)

    raw_inst := strings.to_upper(peek(p).text)
    switch raw_inst {
    case "LDA":
        return parse_lda(p)
    case "LDR":
    }

    return
}

parse_lda :: proc(p: ^Parser) -> (inst: Instruction, ok: bool) {
    toks := make([dynamic]Token)
    oper := make([dynamic]Operand)

    append(&toks, consume(p)) // consume instruction token

    #partial switch peek(p).type {
    case .Hash:
        inst.addr_mode = .Immediate
        parse_ivalue(p, &toks, &oper) or_return

    case .HexNumber:
        inst.addr_mode = parse_addr(p, &toks, &oper) or_return

    case .Register:
        inst.addr_mode = .Register
        tok := consume(p) // consume reg token
        reg := parse_reg_id(tok) or_return
        append(&toks, tok)
        append(&oper, reg)

    // invalid modes
    case:
        return

    }

    inst.tokens = toks[:]
    inst.operands = oper[:]
    return inst, true
}

parse_ldr :: proc(p: ^Parser) -> (inst: Instruction, ok: bool) {
    toks := make([dynamic]Token)
    oper := make([dynamic]Operand)

    append(&toks, consume(p)) // consume instruction token

    #partial switch peek(p).type {
    case .Hash:
        inst.addr_mode = .Immediate
        parse_ivalue(p, &toks, &oper) or_return

    case .HexNumber:
        inst.addr_mode = parse_addr(p, &toks, &oper) or_return

    case .Register:
        inst.addr_mode = .Register
        reg_tok := consume(p)
        reg := parse_reg_id(reg_tok) or_return
        append(&toks, reg_tok)
        append(&oper, reg)

    case:
        return
    }

    inst.tokens = toks[:]
    inst.operands = oper[:]
    return
}