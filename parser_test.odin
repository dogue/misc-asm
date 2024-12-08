package misc_asm

import "core:testing"
import "core:fmt"
import vmem "core:mem/virtual"

ta: vmem.Arena

@(test)
test_parse_lda_immediate :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        {type = .Instruction, text = "LDA"},
        {type = .Hash, text = "#"},
        {type = .HexNumber, text = "69"},
    }

    p := parser_init(input)
    res, ok := parse_lda(&p)

    testing.expect(t, ok)
    testing.expect_value(t, res.addr_mode, AddrMode.Immediate)
    testing.expect_value(t, len(res.tokens), 2)
    testing.expect_value(t, len(res.operands), 1)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.HexNumber)
    testing.expect_value(t, res.operands[0], ByteLiteral(0x69))
}

@(test)
test_parse_lda_zeropage :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        {type = .Instruction, text = "LDA"},
        {type = .HexNumber, text = "42"},
    }

    p := parser_init(input)
    res, ok := parse_lda(&p)

    testing.expect(t, ok)
    testing.expect_value(t, res.addr_mode, AddrMode.ZeroPage)
    testing.expect_value(t, len(res.tokens), 2)
    testing.expect_value(t, len(res.operands), 1)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.HexNumber)
    testing.expect_value(t, res.operands[0], AddrLiteral(0x42))
}

@(test)
test_parse_lda_absolute :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        {type = .Instruction, text = "LDA"},
        {type = .HexNumber, text = "6942"},
    }

    p := parser_init(input)
    res, ok := parse_lda(&p)

    testing.expect(t, ok)
    testing.expect_value(t, res.addr_mode, AddrMode.Absolute)
    testing.expect_value(t, len(res.tokens), 2)
    testing.expect_value(t, len(res.operands), 1)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.HexNumber)
    testing.expect_value(t, res.operands[0], AddrLiteral(0x6942))
}

@(test)
test_parse_lda_indexed_zeropage :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        {type = .Instruction, text = "LDA"},
        {type = .HexNumber, text = "42"},
        {type = .Comma, text = ","},
        {type = .Register, text = "X"},
    }

    p := parser_init(input)
    res, ok := parse_lda(&p)

    testing.expect(t, ok)
    testing.expect_value(t, res.addr_mode, AddrMode.IndexedZeroPage)
    testing.expect_value(t, len(res.tokens), 3)
    testing.expect_value(t, len(res.operands), 2)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.HexNumber)
    testing.expect_value(t, res.tokens[2].type, TokenType.Register)
    testing.expect_value(t, res.operands[0], AddrLiteral(0x42))
    testing.expect_value(t, res.operands[1], RegLiteral(0x00))
}

@(test)
test_parse_lda_indexed_absolute :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        {type = .Instruction, text = "LDA"},
        {type = .HexNumber, text = "6942"},
        {type = .Comma, text = ","},
        {type = .Register, text = "X"},
    }

    p := parser_init(input)
    res, ok := parse_lda(&p)

    testing.expect(t, ok)
    testing.expect_value(t, res.addr_mode, AddrMode.IndexedAbsolute)
    testing.expect_value(t, len(res.tokens), 3)
    testing.expect_value(t, len(res.operands), 2)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.HexNumber)
    testing.expect_value(t, res.tokens[2].type, TokenType.Register)
    testing.expect_value(t, res.operands[0], AddrLiteral(0x6942))
    testing.expect_value(t, res.operands[1], RegLiteral(0x00))
}

@(test)
test_parse_lda_register :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        {type = .Instruction, text = "LDA"},
        {type = .Register, text = "Z"},
    }

    p := parser_init(input)
    res, ok := parse_lda(&p)

    testing.expect(t, ok)
    testing.expect_value(t, res.addr_mode, AddrMode.Register)
    testing.expect_value(t, len(res.tokens), 2)
    testing.expect_value(t, len(res.operands), 1)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.Register)
    testing.expect_value(t, res.operands[0], RegLiteral(0x02))
}

@(test)
test_parse_ldr_implied :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token {
        {type = .Instruction, text = "LDR"},
        {type = .Register, text = "X"},
    }

    p := parser_init(input)
    res, ok := parse_ldr(&p)

    testing.expect(t, ok)
    testing.expect_value(t, res.addr_mode, AddrMode.Implied)
    testing.expect_value(t, len(res.tokens), 2)
    testing.expect_value(t, len(res.operands), 1)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.Register)
    testing.expect_value(t, res.operands[0], RegLiteral(0x00))
}

@(test)
test_parse_ldr_immediate :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token {
        {type = .Instruction, text = "LDR"},
        {type = .Register, text = "X"},
        {type = .Hash, text = "#"},
        {type = .HexNumber, text = "69"},
    }

    p := parser_init(input)
    res, ok := parse_ldr(&p)

    testing.expect(t, ok)
    testing.expect_value(t, res.addr_mode, AddrMode.Immediate)
    testing.expect_value(t, len(res.tokens), 3)
    testing.expect_value(t, len(res.operands), 2)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.Register)
    testing.expect_value(t, res.tokens[2].type, TokenType.HexNumber)
    testing.expect_value(t, res.operands[0], RegLiteral(0x00))
    testing.expect_value(t, res.operands[1], ByteLiteral(0x69))
}

@(test)
test_parse_ldr_zeropage :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token {
        {type = .Instruction, text = "LDR"},
        {type = .Register, text = "X"},
        {type = .HexNumber, text = "69"},
    }

    p := parser_init(input)
    res, ok := parse_ldr(&p)

    testing.expect(t, ok)
    testing.expect_value(t, res.addr_mode, AddrMode.ZeroPage)
    testing.expect_value(t, len(res.tokens), 3)
    testing.expect_value(t, len(res.operands), 2)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.Register)
    testing.expect_value(t, res.tokens[2].type, TokenType.HexNumber)
    testing.expect_value(t, res.operands[0], RegLiteral(0x00))
    testing.expect_value(t, res.operands[1], AddrLiteral(0x69))
}

@(test)
test_parse_ldr_absolute :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token {
        {type = .Instruction, text = "LDR"},
        {type = .Register, text = "X"},
        {type = .HexNumber, text = "6942"},
    }

    p := parser_init(input)
    res, ok := parse_ldr(&p)

    testing.expect(t, ok)
    testing.expect_value(t, res.addr_mode, AddrMode.Absolute)
    testing.expect_value(t, len(res.tokens), 3)
    testing.expect_value(t, len(res.operands), 2)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.Register)
    testing.expect_value(t, res.tokens[2].type, TokenType.HexNumber)
    testing.expect_value(t, res.operands[0], RegLiteral(0x00))
    testing.expect_value(t, res.operands[1], AddrLiteral(0x6942))
}

@(test)
test_parse_ldr_indexed_zeropage :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token {
        {type = .Instruction, text = "LDR"},
        {type = .Register, text = "Y"},
        {type = .HexNumber, text = "69"},
        {type = .Comma, text = ","},
        {type = .Register, text = "X"},
    }

    p := parser_init(input)
    res, ok := parse_ldr(&p)

    testing.expect(t, ok)
    testing.expect_value(t, res.addr_mode, AddrMode.IndexedZeroPage)
    testing.expect_value(t, len(res.tokens), 4)
    testing.expect_value(t, len(res.operands), 3)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.Register)
    testing.expect_value(t, res.tokens[2].type, TokenType.HexNumber)
    testing.expect_value(t, res.tokens[3].type, TokenType.Register)
    testing.expect_value(t, res.operands[0], RegLiteral(0x01))
    testing.expect_value(t, res.operands[1], AddrLiteral(0x69))
    testing.expect_value(t, res.operands[2], RegLiteral(0x00))
}

@(test)
test_parse_ldr_index_absolute :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token {
        {type = .Instruction, text = "LDR"},
        {type = .Register, text = "Y"},
        {type = .HexNumber, text = "6942"},
        {type = .Comma, text = ","},
        {type = .Register, text = "X"},
    }

    p := parser_init(input)
    res, ok := parse_ldr(&p)

    testing.expect(t, ok)
    testing.expect_value(t, res.addr_mode, AddrMode.IndexedAbsolute)
    testing.expect_value(t, len(res.tokens), 4)
    testing.expect_value(t, len(res.operands), 3)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.Register)
    testing.expect_value(t, res.tokens[2].type, TokenType.HexNumber)
    testing.expect_value(t, res.tokens[3].type, TokenType.Register)
    testing.expect_value(t, res.operands[0], RegLiteral(0x01))
    testing.expect_value(t, res.operands[1], AddrLiteral(0x6942))
    testing.expect_value(t, res.operands[2], RegLiteral(0x00))
}

@(test)
test_parse_ldr_register :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token {
        {type = .Instruction, text = "LDR"},
        {type = .Register, text = "X"},
        {type = .Register, text = "Y"},
    }

    p := parser_init(input)
    res, ok := parse_ldr(&p)

    testing.expect(t, ok)
    testing.expect_value(t, res.addr_mode, AddrMode.Register)
    testing.expect_value(t, len(res.tokens), 3)
    testing.expect_value(t, len(res.operands), 2)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.Register)
    testing.expect_value(t, res.tokens[2].type, TokenType.Register)
    testing.expect_value(t, res.operands[0], RegLiteral(0x00))
    testing.expect_value(t, res.operands[1], RegLiteral(0x01))
}
