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
