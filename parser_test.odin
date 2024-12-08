package misc_asm

import "core:testing"
import "core:fmt"
import vmem "core:mem/virtual"

ta: vmem.Arena

@(test)
test_error_parse_lda_bad_immediate_value :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        {type = .Instruction, text = "LDA"},
        {type = .Number, text = "42"},
    }

    p := Parser{}
    parser_init(&p, input)
    res, ok := parse_lda(&p)

    testing.expect(t, !ok)
    testing.expect_value(t, len(p.errors), 1)
    testing.expect_value(t, p.errors[0].kind, ParseErrorKind.UnexpectedToken)
    testing.expect_value(t, p.errors[0].token, Token{type = .Number, text = "42"})
}

@(test)
test_parse_lda_immediate :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        {type = .Instruction, text = "LDA"},
        {type = .Hash, text = "#"},
        {type = .HexNumber, text = "69"},
    }

    p := Parser{}
	parser_init(&p, input)
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

    p := Parser{}
	parser_init(&p, input)
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

    p := Parser{}
	parser_init(&p, input)
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

    p := Parser{}
	parser_init(&p, input)
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

    p := Parser{}
	parser_init(&p, input)
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

    p := Parser{}
	parser_init(&p, input)
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

    p := Parser{}
	parser_init(&p, input)
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

    p := Parser{}
	parser_init(&p, input)
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

    p := Parser{}
	parser_init(&p, input)
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

    p := Parser{}
	parser_init(&p, input)
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

    p := Parser{}
	parser_init(&p, input)
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

    p := Parser{}
	parser_init(&p, input)
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

    p := Parser{}
	parser_init(&p, input)
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

@(test)
test_parse_clr_one_reg :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        {type = .Instruction, text = "CLR"},
        {type = .Register, text = "X"},
    }

    p: Parser
    parser_init(&p, input)
    res, ok := parse_clr(&p)

    testing.expect(t, ok)
    testing.expect_value(t, len(res.tokens), 2)
    testing.expect_value(t, len(res.operands), 1)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.Register)
    testing.expect_value(t, res.operands[0], RegLiteral(0x00))
}

@(test)
test_parse_clr_two_regs :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        {type = .Instruction, text = "CLR"},
        {type = .Register, text = "X"},
        {type = .Register, text = "Y"},
    }

    p: Parser
    parser_init(&p, input)
    res, ok := parse_clr(&p)

    testing.expect(t, ok)
    testing.expect_value(t, len(res.tokens), 3)
    testing.expect_value(t, len(res.operands), 2)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.Register)
    testing.expect_value(t, res.tokens[2].type, TokenType.Register)
    testing.expect_value(t, res.operands[0], RegLiteral(0x00))
    testing.expect_value(t, res.operands[1], RegLiteral(0x01))
}

@(test)
test_parse_clr_three_regs :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        {type = .Instruction, text = "CLR"},
        {type = .Register, text = "X"},
        {type = .Register, text = "Y"},
        {type = .Register, text = "Z"},
    }

    p: Parser
    parser_init(&p, input)
    res, ok := parse_clr(&p)

    testing.expect(t, ok)
    testing.expect_value(t, len(res.tokens), 4)
    testing.expect_value(t, len(res.operands), 3)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.Register)
    testing.expect_value(t, res.tokens[2].type, TokenType.Register)
    testing.expect_value(t, res.tokens[3].type, TokenType.Register)
    testing.expect_value(t, res.operands[0], RegLiteral(0x00))
    testing.expect_value(t, res.operands[1], RegLiteral(0x01))
    testing.expect_value(t, res.operands[2], RegLiteral(0x02))
}

@(test)
test_parse_clr_four_regs :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)
    
    input := []Token{
        {type = .Instruction, text = "CLR"},
        {type = .Register, text = "X"},
        {type = .Register, text = "Y"},
        {type = .Register, text = "Z"},
        {type = .Register, text = "W"},
    }

    p: Parser
    parser_init(&p, input)
    res, ok := parse_clr(&p)

    testing.expect(t, ok)
    testing.expect_value(t, len(res.tokens), 5)
    testing.expect_value(t, len(res.operands), 4)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.Register)
    testing.expect_value(t, res.tokens[2].type, TokenType.Register)
    testing.expect_value(t, res.tokens[3].type, TokenType.Register)
    testing.expect_value(t, res.tokens[4].type, TokenType.Register)
    testing.expect_value(t, res.operands[0], RegLiteral(0x00))
    testing.expect_value(t, res.operands[1], RegLiteral(0x01))
    testing.expect_value(t, res.operands[2], RegLiteral(0x02))
    testing.expect_value(t, res.operands[3], RegLiteral(0x03))
}

@(test)
test_error_parse_clr_five_regs :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        {type = .Instruction, text = "CLR"},
        {type = .Register, text = "X"},
        {type = .Register, text = "X"},
        {type = .Register, text = "X"},
        {type = .Register, text = "X"},
        {type = .Register, text = "X"},
    }

    p: Parser
    parser_init(&p, input)
    res, ok := parse_clr(&p)

    testing.expect(t, !ok)
    testing.expect_value(t, len(p.errors), 1)
    testing.expect_value(t, p.errors[0].kind, ParseErrorKind.TooManyOperands)
}
