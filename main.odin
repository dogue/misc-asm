package misc_asm

import "core:fmt"
import "core:flags"
import "core:os"

Cli :: struct {
    input_file: string `args:"pos=0,required"`,
    output_file: string `args:"pos=1"`
}

main :: proc() {
    cli: Cli
    if err := flags.parse(&cli, os.args[1:]); err != nil {
        fmt.eprintf("Error: %#v\n", err)
        return
    }

    src, err := os.read_entire_file(cli.input_file)

    lexer := lexer_init(string(src))
    tokens := lex_scan_all(&lexer)
    
    p: Parser
    parser_init(&p, tokens)

    instructions := parse_all(&p)

    program := make([dynamic]byte)

    for i in instructions {
        as := assemble_instruction(i)
        for b in as {
            append(&program, b)
        }
    }

    if cli.output_file == "" {
        for b, i in program {
            fmt.printf("%4X: %2X\n", i, b)
        }
        return
    }

    os.write_entire_file(cli.output_file, program[:])
}
