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

    p := Parser{}
	parser_init(&p, input)
    res := parse_instruction(&p)

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
    res := parse_instruction(&p)

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
    res := parse_instruction(&p)

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
    res := parse_instruction(&p)

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
    res := parse_instruction(&p)

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
    res := parse_instruction(&p)

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
    res := parse_instruction(&p)

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
    res := parse_instruction(&p)

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
    res := parse_instruction(&p)

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
    res := parse_instruction(&p)

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
    res := parse_instruction(&p)

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
    res := parse_instruction(&p)

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
    res := parse_instruction(&p)

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
    res := parse_instruction(&p)

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
    res := parse_instruction(&p)

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
    res := parse_instruction(&p)

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
    res := parse_instruction(&p)

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
test_parse_swp_implied :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        {type = .Instruction, text = "SWP"},
        {type = .Register, text = "X"},
    }

    p: Parser
    parser_init(&p, input)
    res := parse_instruction(&p)

    testing.expect_value(t, res.addr_mode, AddrMode.Implied)
    testing.expect_value(t, len(res.tokens), 2)
    testing.expect_value(t, len(res.operands), 1)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.Register)
    testing.expect_value(t, res.operands[0], RegLiteral(0x00))
}

@(test)
test_parse_swp_reg :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)
    
    input := []Token{
        {type = .Instruction, text = "SWP"},
        {type = .Register, text = "X"},
        {type = .Register, text = "Y"},
    }

    p: Parser
    parser_init(&p, input)
    res := parse_instruction(&p)

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
test_parse_multiple_instructions_with_errors :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        // Valid LDA immediate
        {type = .Instruction, text = "LDA"},
        {type = .Hash, text = "#"},
        {type = .HexNumber, text = "42"},

        // Invalid LDR (missing register)
        {type = .Instruction, text = "LDR"},
        {type = .HexNumber, text = "69"},

        // Valid CLR register
        {type = .Instruction, text = "CLR"},
        {type = .Register, text = "X"},

        // Invalid SWP (invalid register name)
        {type = .Instruction, text = "SWP"},
        {type = .Symbol, text = "Q"},
    }

    p: Parser
    parser_init(&p, input)

    instructions := make([dynamic]Instruction)
    for parser_peek(&p).type != .EOF {
        append(&instructions, parse_instruction(&p))
    }

    testing.expect_value(t, len(instructions), 4)
    testing.expect_value(t, len(p.errors), 2)

    // first
    testing.expect_value(t, instructions[0].addr_mode, AddrMode.Immediate)
    testing.expect_value(t, len(instructions[0].operands), 1)
    testing.expect_value(t, instructions[0].operands[0], ByteLiteral(0x42))

    // third
    testing.expect_value(t, instructions[2].addr_mode, AddrMode.Register)
    testing.expect_value(t, len(instructions[2].operands), 1)
    testing.expect_value(t, instructions[2].operands[0], RegLiteral(0))

    // errors
    first_err := p.errors[0].(UnexpectedTokenErr)
    testing.expect_value(t, first_err.token.type, TokenType.HexNumber)
    testing.expect_value(t, first_err.expected, bit_set[TokenType]{.Register})

    second_err := p.errors[1].(UnexpectedTokenErr)
    testing.expect_value(t, second_err.token.type, TokenType.Symbol)
    testing.expect_value(t, second_err.expected, bit_set[TokenType]{.Register})
}

@(test)
test_parse_jmp_absolute :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        {type = .Instruction, text = "JMP"},
        {type = .HexNumber, text = "4269"},
    }

    p: Parser
    parser_init(&p, input)
    res := parse_instruction(&p)

    testing.expect_value(t, res.addr_mode, AddrMode.Absolute)
    testing.expect_value(t, len(res.tokens), 2)
    testing.expect_value(t, len(res.operands), 1)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.HexNumber)
    testing.expect_value(t, res.operands[0], AddrLiteral(0x4269))
}

@(test)
test_parse_jmp_relative :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        {type = .Instruction, text = "JMP"},
        {type = .HexNumber, text = "42"},
    }

    p: Parser
    parser_init(&p, input)
    res := parse_instruction(&p)

    testing.expect_value(t, res.addr_mode, AddrMode.Relative)
    testing.expect_value(t, len(res.tokens), 2)
    testing.expect_value(t, len(res.operands), 1)
    testing.expect_value(t, res.tokens[0].type, TokenType.Instruction)
    testing.expect_value(t, res.tokens[1].type, TokenType.HexNumber)
    testing.expect_value(t, res.operands[0], AddrLiteral(0x42))
}

@(test)
test_parse_sta_absolute :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        {type = .Instruction, text = "STA"},
        {type = .HexNumber, text = "4269"},
    }

    p: Parser
    parser_init(&p, input)
    res := parse_instruction(&p)

    testing.expect_value(t, res.addr_mode, AddrMode.Absolute)
    testing.expect_value(t, len(res.tokens), 2)
    testing.expect_value(t, len(res.operands), 1)
    testing.expect_value(t, res.operands[0], AddrLiteral(0x4269))
}

@(test)
test_parse_sta_zeropage_indexed :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        {type = .Instruction, text = "STA"},
        {type = .HexNumber, text = "42"},
        {type = .Comma, text = ","},
        {type = .Register, text = "X"},
    }

    p: Parser
    parser_init(&p, input)
    res := parse_instruction(&p)

    testing.expect_value(t, res.addr_mode, AddrMode.IndexedZeroPage)
    testing.expect_value(t, len(res.tokens), 3)
    testing.expect_value(t, len(res.operands), 2)
    testing.expect_value(t, res.operands[0], AddrLiteral(0x42))
    testing.expect_value(t, res.operands[1], RegLiteral(0x00))
}

@(test)
test_parse_str :: proc(t: ^testing.T) {
    context.allocator = vmem.arena_allocator(&ta)

    input := []Token{
        {type = .Instruction, text = "STR"},
        {type = .Register, text = "Y"},
        {type = .HexNumber, text = "4269"},
    }

    p: Parser
    parser_init(&p, input)
    res := parse_instruction(&p)

    testing.expect_value(t, res.addr_mode, AddrMode.Absolute)
    testing.expect_value(t, len(res.tokens), 3)
    testing.expect_value(t, len(res.operands), 2)
    testing.expect_value(t, res.operands[0], RegLiteral(0x01))
    testing.expect_value(t, res.operands[1], AddrLiteral(0x4269))
}
