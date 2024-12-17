package misc_asm

assemble_instruction :: proc(instruction: Instruction) -> []byte {
    b := make([dynamic]byte)
    inst := instruction.tokens[0]

    if ib, ok := Instructions[inst.text]; ok {
        op := (byte(instruction.addr_mode) << 5) | ib
        append(&b, op)
    }

    for op in instruction.operands {
        switch t in op {
        case RegLiteral:
            append(&b, byte(t))

        case ByteLiteral:
            append(&b, byte(t))

        case AddrLiteral:
            if instruction.addr_mode == .ZeroPage {
                al := byte(t)
                append(&b, al)
            } else {
                al := byte(t)
                ah := byte(t >> 8)
                append(&b, al)
                append(&b, ah)
            }
        }
    }

    return b[:]
}
