/*
 *
 *  This simple compiler is a syntax directed translator that directly emits x64 (IA-64, AMD64) machine code
 *  while recursive descent parsing. 
 *
 *  by Kuter Dinel. 08/2023
 *
 */


#include <stddef.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdbool.h>

#include <dlfcn.h> // used for dynamic linking.
#include <sys/mman.h> // for allocating executable memory.

// Buffer managment code.
#define BUFFER_SIZE 100000

uint8_t result_buffer[BUFFER_SIZE];
size_t result_size = 0;

// used for dynamic linking.
void *dlHandle = NULL;

#define critical_check_msg(cond, msg) if (!(cond)) { printf(__FILE__ ":%d\n" msg "\nCondition failed: " #cond, __LINE__); exit(1);}
#define critical_check(cond) critical_check_msg(cond, "Compilation aborted !")

// Push various sized values into the result buffer.
void push_int8(uint8_t w) {
    critical_check_msg(result_size != BUFFER_SIZE, "Buffer Overflow ! Try increasing BUFFER_SIZE"); 
    result_buffer[result_size++] = w;
}

void push_int16(uint16_t w) {
    critical_check_msg(result_size + 2 <= BUFFER_SIZE, "Buffer Overflow ! Try increasing BUFFER_SIZE");
    result_buffer[result_size++] = (uint8_t)(w & 0xFF);
    result_buffer[result_size++] = (uint8_t)(w >> 8 & 0xFF);
}

void push_int32(uint32_t w) {
    critical_check_msg(result_size + 4 <= BUFFER_SIZE, "Buffer Overflow ! Try increasing BUFFER_SIZE");

    result_buffer[result_size++] = (uint8_t)(w & 0xFF);
    result_buffer[result_size++] = (uint8_t)(w >> 8 & 0xFF);
    result_buffer[result_size++] = (uint8_t)(w >> 16 & 0xFF);
    result_buffer[result_size++] = (uint8_t)(w >> 24 & 0xFF);
} 

void push_int64(uint64_t w) {
    push_int32(w & 0xFFFFFFFF);
    push_int32((w >> 32) & 0xFFFFFFFF);
} 

typedef int32_t slot_t;

int peak_stack_size = 0;

// Allocate a stack slot.
slot_t slot_alloc() {
    int32_t result = peak_stack_size;
    peak_stack_size += 8;

    return result;
}

// Positive integers are assumed to be unsigned and if the integer is spoused to be signed
// it's users responsibility to ensure that the integer is small enough.
//
// Negative integers are treated as signed integers.
// TODO: Hex parsing.
uint64_t read_int(char **current) {
    uint64_t result = 0;
    bool isNegative = false; 
    if (**current == '-') {
        isNegative = true;
        (*current)++;
    }

    while (**current >= '0' && **current <= '9') {
        result *= 10;
        uint64_t d = (**current - '0');
        // Make sure the integer does not overflow.
        critical_check_msg(result <= UINT64_MAX - d, "Int too big") 
        result += d;
        (*current)++;
    }

    if (isNegative) {
        //FIXME: add assertion here.
        return -result;
    }
  
    return result;
}

char translate_escape(char c) {
    switch (c) {
        case 'n':
            return '\n';
        case 'r':
            return '\r';
        case '"':
        case '\'':
        case '\\':
        default:
            return c;
    }
}

char parse_char(char **current) {
    critical_check(**current == '\'');
    (*current)++;

    char c = **current;
    if (c == '\\') {
        (*current)++;
        c = translate_escape(**current);
    }
    critical_check(**current == '\'');
    (*current)++;
   
    return c;
}

char* parse_string(char **current) {
    critical_check(**current == '"');
    (*current)++;

    char *start = *current;

    size_t length = 0;
    int is_escaped = 0;
    while (**current != '"' || is_escaped) {
        is_escaped = 0;
        if (**current == '\\')
            is_escaped = 1;
        (*current)++;
        length++;
    }

    (*current)++;

    char *resultString = malloc(length + 1);
    is_escaped = 0;
    size_t true_length = 0;
    for (size_t i = 0; start[i] != '"' || is_escaped; i++) {
        char c = start[i];
        if (c == '\\') {
            is_escaped = 1;
            continue;
        }
        if (!is_escaped) {
            resultString[true_length++] = c;
            continue;
        }

        resultString[true_length++] = translate_escape(c);
        is_escaped = 0;
    }

    resultString[true_length + 1] = 0; // Zero termination.
    return resultString;
}

void skip_whitespace(char **current) {
    while (1) {
        char c = **current;
        if (c != ' ' && c != '\n' && c != '\r')
            break;
        (*current)++;
    }
}

bool skip_comment(char **current) {
   if (**current == '/') {
        (*current)++;
        if (**current == '/') {
            // single line comment
            while (**current && **current != '\n')
                (*current)++;
            critical_check(**current == '\n');
            (*current)++;
            return true;
        } else if (**current == '*') {
            (*current)++;

            // Multiline comment.
            while (1) {
                while (**current && **current != '*')
                    (*current)++;

                critical_check_msg(**current != 0, "unterminated comment");
                (*current)++;
                if (**current == '/') {
                    (*current)++;
                    return true; 
                }
                (*current)++;

            }
        } else { 
            (*current)--;        
        }
    }
    return false;
}

// Skip whitespace or comments.
void skip_gap(char **current) {
    // We need to do it like this to be able to handle alternating whitespace and comments.
    while (1) {
        skip_whitespace(current);
        if (!skip_comment(current))
            return;
    }
}

// Variable management code starts here.
#define MAX_VAR_NAME_LENGTH 20

// local variable or symbol.
typedef struct  {
    char name[MAX_VAR_NAME_LENGTH];
    union {
        uint32_t slot; // Slot offset of local variable.
        uint64_t value; // Sym value.
    };
    bool is_function; // Function symbols need special handling.
} sym_t;

typedef struct {
    sym_t *global_sym;
    bool is_relative; // is this a rel32 or imm64.
    void *result_point;
} reloc_t;

#define MAX_RELOC_COUNT 5000
reloc_t relocations[MAX_RELOC_COUNT];
size_t reloc_count = 0;

#define MAX_VARIABLE 100
sym_t local_syms[MAX_VARIABLE] = {};
size_t local_sym_count = 0;

sym_t global_syms[MAX_VARIABLE] = {};
size_t global_sym_count = 0;

size_t slotCount = 0;

// The buffer position of the current loop header.
// Used to implement `continue` statements.
size_t current_loop_header = 0;
sym_t *current_loop_exit = NULL;

sym_t *function_exit = NULL;

sym_t* resolve_sym(sym_t *variables, size_t variable_count, char *name) {
    size_t len = strlen(name);
    critical_check(len < MAX_VAR_NAME_LENGTH);

    for (int i = 0; i < variable_count; i++) {
        sym_t *var = &variables[i];
        size_t varLen = strlen(var->name);
        if (len != varLen)
            continue;
        if (memcmp(var->name, name, len) == 0)
            return var;
    }
    return NULL;
}

sym_t* insert_sym(sym_t *syms, size_t *sym_count, char *name, uint32_t slot) {
    critical_check_msg(MAX_VARIABLE > *sym_count, "try increasing MAX_VARIABLE");

    sym_t *result = &syms[(*sym_count)++];
    *result->name = 0;

    if (name != NULL) {
        size_t len = strlen(name);
        critical_check(len < MAX_VAR_NAME_LENGTH);
        memcpy(result->name, name, len + 1);
    }

    result->slot = slot;
    return result;
}

sym_t* define_sym(sym_t *syms, size_t *sym_count, char *name) {
    critical_check(name != NULL);
    sym_t *var = resolve_sym(syms, *sym_count, name);
    if (var != NULL) 
        return var;
    
    slot_t slot = slot_alloc();
    // variable has not been defined.
    return insert_sym(syms, sym_count, name, slot);
}


void push_reloc_at(sym_t *global, void *point, bool is_relative) {
    critical_check_msg(MAX_RELOC_COUNT > reloc_count, "try increasing MAX_RELOC_COUNT");
    reloc_t *reloc = &relocations[reloc_count++];
    reloc->global_sym = global;
    reloc->result_point = point;
    reloc->is_relative = is_relative;
}

void push_reloc(sym_t *global) {
    push_reloc_at(global, (void*)(result_buffer + result_size), false);
}

// Called at the end of compilation.
void apply_relocs() {
    for (int i = 0; i < reloc_count; i++) {
        reloc_t *reloc = &relocations[i];
        // Apply relocation.
        if (!reloc->is_relative)
            *(uint64_t*)(reloc->result_point) = reloc->global_sym->value;
        else
            *(uint32_t*)(reloc->result_point) = reloc->global_sym->value -
                (size_t)((uint8_t*)reloc->result_point - result_buffer) - 4;
        
    }
}

char ident_buffer[MAX_VAR_NAME_LENGTH];
size_t read_ident(char **current) {
    size_t length = 0;

    while (1) {
        char c = **current;
        if (!isalnum(c) && c != '_')
            break;
        (*current)++;
        
        critical_check(length < MAX_VAR_NAME_LENGTH);

        ident_buffer[length++] = c;
    }
    ident_buffer[length] = 0;
    return length;
}

typedef enum {
    RAX = 0,
    RCX = 1,
    RDX = 2,
    RBX = 3,
    RSP = 4,
    RBP = 5,
    RSI = 6,
    RDI = 7,
    R8  = 8,
    R9  = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15
} reg64;

// @W Make the addressing 64bit.
// @R Extension for the ModR/M reg field.
// @X Extension of the SIB index field.
// @B Extension for the R/M field or opcode reg field.
void emit_rex(uint8_t w, uint8_t r, uint8_t x, uint8_t b) {
    uint8_t result = (0b0100 << 4) | (w << 3) | (r << 2) | (x << 1) | b;
    push_int8(result);
}

// Store a constant value in a register.
void mov_reg_const64(reg64 reg, uint64_t cons) {
    uint8_t regNumber = (uint8_t)reg;
    emit_rex(1, 0, 0, regNumber > 7);
    regNumber &= 0b111;

    // MOV REG64, CONST64
    push_int8(0xB8 | regNumber);
    push_int64(cons);
}

void mov_reg_global(reg64 reg, sym_t *global) {
    uint8_t regNumber = (uint8_t)reg;
    emit_rex(1, 0, 0, regNumber > 7);
    regNumber &= 0b111;

    // MOV REG64, CONST64
    push_int8(0xB8 | regNumber);

    push_reloc(global);
    push_int64(0);
}

// mod
// 0 [rm]
// 1 [rm + disp8]
// 2 [rm + disp32]
// 3 rm
void emit_modrm(uint8_t mod, uint8_t regop, uint8_t rm) {
    uint8_t result = (mod << 6) | (regop << 3) | rm;
    push_int8(result);
}

// Emit modrm that represents slot value from stack.
void modrm_slot(slot_t slot, uint8_t regop) {
    emit_modrm(2, regop, RBP);
    push_int32(-slot - 0x8);
}

/// mov [rbp + slot], reg
void mov_slot_reg(slot_t slot, uint8_t reg) {
    emit_rex(1, reg > 7, 0, 0);
    reg &= 0b111;

    push_int8(0x89);

    modrm_slot(slot, reg);
}

void mov_reg_reg(uint8_t target, uint8_t source) {
    emit_rex(1, source > 7, 0, target > 7);
    source &= 0b111;
    target  &= 0b111;
    push_int8(0x89);
    emit_modrm(3, source, target);
}

// Load a constant value into an allocated stack slot.
slot_t mov_slot_const(uint64_t value) {
    slot_t slot = slot_alloc();
    // There is no instruction in x86 to store a 64 bit value in modrm,
    // we have to use multiple instructions here.

    // mov rax, int64
    mov_reg_const64(RAX, value);
    // mov [rbp + slot], rax
    mov_slot_reg(slot, RAX);

    return slot;
}

void mov_reg_slot(slot_t slot, uint8_t reg) {
    emit_rex(1, 0, 0, reg > 7);
    reg &= 0b111;

    push_int8(0x8B);
    modrm_slot(slot, reg);
}

slot_t copy_slot(slot_t a) {
    slot_t result = slot_alloc();
    mov_reg_slot(a, RAX); // mov RAX, [rbp + slot_a]
    mov_slot_reg(result, RAX); // mov [rbp + result], RAX
    return result;
}

slot_t add_slots(slot_t slot_a, slot_t slot_b) {
    slot_t result = copy_slot(slot_a);
    mov_reg_slot(slot_b, RAX); // mov, RAX, [rbp + slot_b]

    // add [rbp + result], RAX
    emit_rex(1, 0, 0, 0);
    push_int8(0x01);  //opcode
    modrm_slot(result, RAX);

    return result;
}

// Compare slots and set flags.
void cmp_slots(slot_t slot_a, slot_t slot_b) {
    mov_reg_slot(slot_b, RAX); // mov RAX, [rbp + slot_a]

    // cmp [rbp + slot_a], RAX
    emit_rex(1, 0, 0, 0);
    push_int8(0x39);  // opcode
    modrm_slot(slot_a, RAX);
}

void cmp_const(slot_t a, uint32_t imm) {
    //REX.W + 81 /7 id
    emit_rex(1, 0, 0, 0);
    push_int8(0x81);
    modrm_slot(a, 7);
    push_int32(imm);
}

enum condition {
    EQUAL,
    NOT_EQUAL,
    GREATER,
    LESS,
    GREATER_EQUAL,
    LESS_EQUAL
}; 

// Set the value a of a slot based on the value of a comparision.
int get_compare(enum condition cond) {
    slot_t result = mov_slot_const(0);

    push_int8(0x0F);
    uint8_t secOp;
    switch (cond) {
        case EQUAL:
            secOp = 0x94;
            break;
        case NOT_EQUAL:
            secOp = 0x95;
            break;
        case GREATER:
            secOp = 0x9f;
            break;
        case LESS:
            secOp = 0x9C;
            break;
        case GREATER_EQUAL:
            secOp = 0x9D;
            break;
        case LESS_EQUAL:
            secOp = 0x9E;
            break;
    }
    push_int8(secOp);
    modrm_slot(result, 0);
    return result;
}

slot_t compare_slots(slot_t slot_a, slot_t slot_b, enum condition cond) {
    cmp_slots(slot_a, slot_b);
    return get_compare(cond);
}

slot_t sub_slots(slot_t slot_a, slot_t slot_b) {
    slot_t result = copy_slot(slot_a);
    mov_reg_slot(slot_b, RAX); // mov, RAX, [rbp + slot_b]

    // sub [rbp + result], RAX
    emit_rex(1, 0, 0, 0);
    push_int8(0x29); //opcode
    modrm_slot(result, RAX);

    return result;
}

slot_t and_slots(slot_t slot_a, slot_t slot_b) {
    slot_t result = copy_slot(slot_a);
    mov_reg_slot(slot_b, RAX); // mov, RAX, [rbp + slot_b]

    // and [rbp + result], RAX
    emit_rex(1, 0, 0, 0);
    push_int8(0x21); //opcode
    modrm_slot(result, RAX);

    return result;
}

slot_t or_slots(slot_t slot_a, slot_t slot_b) {
    slot_t result = copy_slot(slot_a);
    mov_reg_slot(slot_b, RAX); // mov, RAX, [rbp + slot_b]

    // or [rbp + result], RAX
    emit_rex(1, 0, 0, 0);
    push_int8(0x09); //opcode
    modrm_slot(result, RAX);

    return result;
}

slot_t not_slot(slot_t slot_a) {
    slot_t result = copy_slot(slot_a);
    emit_rex(1, 0, 0, 0);
    push_int8(0xf7); //opcode
    modrm_slot(result, 2);
    
    return result;
} 

// Division and multiplication instruction are special in that they require one argument to be in the RAX register.
slot_t div_slots(int slot_a, int slot_b) {
    slot_t result = slot_alloc();
    mov_reg_slot(slot_a, RAX); // mov RAX, [rbp + slot_a]
    mov_reg_const64(RDX, 0);
    emit_rex(1, 0, 0, 0);
    
    push_int8(0xF7); //idiv opcode
    modrm_slot(slot_b, 7);

    mov_slot_reg(result, RAX);
    return result;
}

slot_t mul_slots(int slot_a, int slot_b) {
    slot_t result = slot_alloc();
    mov_reg_slot(slot_a, RAX); // mov RAX, [rbp + slot_a]
   
    emit_rex(1, 0, 0, 0);
   
    push_int8(0xF7); // imul opcode
    modrm_slot(slot_b, 4);
    mov_slot_reg(result, RAX);
    return result;
}

void push_stack(uint8_t reg) {
    emit_rex(1, 0, 0, reg > 7);
    reg &= 0b111;
    push_int8(0xFF);
    emit_modrm(3, 6, reg);
}

void pop_stack(uint8_t reg) {
    emit_rex(1, 0, 0, reg > 7);
    reg &= 0b111;
    push_int8(0x8F);
    emit_modrm(3, 0, reg);
}

uint32_t* emit_prolog() {
    push_stack(RBP); // save rbp.
    mov_reg_reg(RBP, RSP);

    // sub RSP, <const_value>
    emit_rex(1, 0, 0, 0);
    push_int8(0x81);
    emit_modrm(3, 5, RSP);

    // Result is the offset of the stack size.
    uint32_t *result = (uint32_t*)((void*)result_buffer + result_size);
    push_int32(0); // place holder.
    return result;
}

void emit_epilog() {
    mov_reg_reg(RSP, RBP); // restore rsp.
    pop_stack(RBP); // restore rbp
    push_int8(0xC3); // ret
}

slot_t compile_expression(char **current);

slot_t compile_exp_function_call(char **current, slot_t function) {
    critical_check(**current == '(');
    (*current)++;

    
    slot_t arguments[6];
    size_t argCount = 0;

    reg64 registers[] = { RDI, RSI, RDX, RCX, R8, R9};

    // Parse arguments.
    while (1) {
        skip_gap(current);
        critical_check(argCount <= 6 && "FIXME: Support more than 6 args for function calls.");
        if (**current == ')')
            break;
        
        arguments[argCount++] = compile_expression(current);
        skip_gap(current);

        if (**current == ')')
            break;
        critical_check(**current == ',');
        (*current)++;
    }
    (*current)++;
    
    // Move argument slots to call argument registers.
    for (size_t i = 0; i < argCount; i++) {
        mov_reg_slot(arguments[i], registers[i]);
    }

    push_int8(0xFF); // call opcode.
    modrm_slot(function, 2);

    // Store the function return value.
    slot_t result = slot_alloc();
    mov_slot_reg(result, RAX);

    return result;
}

// Load the address of the slot.
slot_t load_slot_address(slot_t slot) {
    emit_rex(1, 0, 0, 0);
    push_int8(0x8D); // LEA
    modrm_slot(slot, RAX);

    slot_t result = slot_alloc();
    mov_slot_reg(result, RAX);

    return result;
}

// Returns the stack slot where the result is stored.
slot_t compile_exp_atom_(char **current) {


    skip_gap(current);

    char c = **current;

    if (c == '&') { // Reference.
        (*current)++;
        slot_t atom = compile_exp_atom_(current);
        return load_slot_address(atom);
    }

    if (c == '~') { // logical not .
        (*current)++;
        slot_t atom = compile_exp_atom_(current);
        return not_slot(atom);
    }

    if (c == '*') { // Dereference
        (*current)++;
        slot_t atom = compile_exp_atom_(current);
        mov_reg_slot(atom, RAX);
        
        //TODO: Move this to a separate function.
        emit_rex(1, 0, 0, 0);
        push_int8(0x8B);
        emit_modrm(0, RAX, RAX);

        slot_t result = slot_alloc();
        mov_slot_reg(result, RAX);
        return result;
    }

    // The atom is a constant 
    if (c >= '0' && c <= '9'){
        int value = read_int(current);
        return mov_slot_const(value);
    }

    if (c == '-') {
        char nc = *(*current + 1);
        if (nc >= '0' && nc <= '9') {
            int value = read_int(current);
            return mov_slot_const(value);
        }
        // Analytical negation.
        critical_check_msg(false, "unimplemented");
    }

    // Parenthesis expression.
    // (123 * 123 *  ...)
    if (c == '(') {
        (*current)++;
        slot_t result = compile_expression(current);
        critical_check(**current == ')');
        (*current)++;
        return result;
    }

    // function call or variable reference.
    if (isalpha(c)) {
        read_ident(current);

        sym_t *var = resolve_sym(local_syms, local_sym_count, ident_buffer);
        if (var)
            return var->slot;

        sym_t *global_sym = resolve_sym(global_syms, global_sym_count, ident_buffer);
        if (global_sym) {
            mov_reg_global(RAX, global_sym);
            slot_t result = slot_alloc();
            mov_slot_reg(result, RAX);

            return result;
        }

        uint64_t handle = (uint64_t)dlsym(dlHandle, ident_buffer);
        critical_check_msg(handle != 0, "Variable not found");
    
        return mov_slot_const(handle);
    }

    if (c == '"') {
        char *string = parse_string(current);
        return mov_slot_const((uint64_t)(void*)string);
    }

    if (c == '\'') {
        char c = parse_char(current);
        return mov_slot_const((uint64_t)c);
    }

    return -1;
}

slot_t compile_exp_atom(char **current) {
    slot_t result = compile_exp_atom_(current);

    skip_gap(current);
    if (**current == '(')
        return compile_exp_function_call(current, result);

    return result;
}


typedef slot_t (*compile_operator_t)(slot_t a, slot_t b);

typedef struct  {
    char *op_string;
    size_t precedence;
    compile_operator_t compile_op;
} bin_operator_t;

slot_t equals_slots(slot_t a, slot_t b) {
    return compare_slots(a, b, EQUAL);
}

slot_t not_equals_slots(slot_t a, slot_t b) {
    return compare_slots(a, b, NOT_EQUAL);
}

slot_t greater_slots(slot_t a, slot_t b) {
    return compare_slots(a, b, GREATER);
}

slot_t greater_equal_slots(slot_t a, slot_t b) {
    return compare_slots(a, b, GREATER_EQUAL);
}

slot_t less_slots(slot_t a, slot_t b) {
    return compare_slots(a, b, LESS);
}

slot_t less_equal_slots(slot_t a, slot_t b) {
    return compare_slots(a, b, LESS_EQUAL);
}

slot_t right_shift_slots(slot_t a, slot_t b) {
    slot_t result = copy_slot(a);

    mov_reg_slot(b, RCX);

    //SAR [rbp + a], CL
    emit_rex(1, 0, 0, 0);
    push_int8(0xD3);
    modrm_slot(result, 7);

    return result;
}

slot_t left_shift_slots(slot_t a, slot_t b) {
    slot_t result = copy_slot(a);

    mov_reg_slot(b, RCX);

    //SAL [rbp + a], CL
    emit_rex(1, 0, 0, 0);
    push_int8(0xD3);
    modrm_slot(result, 4);

    return result;

}

#define MAX_PRECEDENCE 5

#define IS_OPERATOR_CONTINUATION(c) ((c) == '<' || (c) == '>' || (c) == '=')

bin_operator_t bin_operators[] = {
    {.op_string="*", .precedence=4, .compile_op=mul_slots},
    {.op_string="/", .precedence=4, .compile_op=div_slots},
    {.op_string="+", .precedence=3, .compile_op=add_slots},
    {.op_string="-", .precedence=3, .compile_op=sub_slots},
    
    {.op_string=">>", .precedence=2, .compile_op=right_shift_slots},
    {.op_string="<<", .precedence=2, .compile_op=left_shift_slots},
    
    {.op_string="==",.precedence=1, .compile_op=equals_slots},
    {.op_string="!=", .precedence=1, .compile_op=not_equals_slots},

    {.op_string=">=", .precedence=1, .compile_op=greater_equal_slots},
    {.op_string="<=", .precedence=1, .compile_op=less_equal_slots},

    {.op_string=">", .precedence=1, .compile_op=greater_slots},
    
    {.op_string="<", .precedence=1, .compile_op=less_slots},
   
    {.op_string="&", .precedence=0, .compile_op=and_slots},
    {.op_string="|", .precedence=0, .compile_op=or_slots},
}; 


slot_t compile_expression_(char **current, int precedence) {
    skip_gap(current);
    if (precedence == MAX_PRECEDENCE)
        return compile_exp_atom(current);
    
    int64_t result = compile_expression_(current, precedence + 1);
    
    while (1) {
        skip_gap(current);
        char op = **current;
        if (op == 0)
            return result;
        char nOP = *(*current + 1);

        compile_operator_t compile_op_fun = NULL;
        // Go over each operator and check if the current operator
        for (size_t i = 0; i < sizeof(bin_operators) / sizeof(bin_operator_t); i++) {
            bin_operator_t *operator = &bin_operators[i];
            if (operator->op_string[0] != op || (IS_OPERATOR_CONTINUATION(nOP) && operator->op_string[1] != nOP))
                continue;

            // Operator found, check precedence.
            if (operator->precedence != precedence)
                break;

            // Operator and precedence matches.
            compile_op_fun = operator->compile_op;
            (*current)++;
            if (operator->op_string[1] != 0)
                (*current)++;
            break;
        }

        if (compile_op_fun == NULL)
            return result;

        slot_t right = compile_expression_(current, precedence + 1);
        result = compile_op_fun(result, right);
    }
}

slot_t compile_expression(char **current) {
    return compile_expression_(current, 0);
}

void* allocate_executable(size_t size) {
    void* result = mmap(
       NULL,
       size,
       PROT_READ | PROT_WRITE | PROT_EXEC,
       MAP_ANONYMOUS | MAP_PRIVATE,
       -1,
       0
    );

    critical_check(result != MAP_FAILED);
    return result;
}

slot_t compile_statement(char **current);
slot_t compile_exp_block(char **current);

slot_t compile_exp_st_block(char **current) {
    skip_gap(current);
    if (**current == '{') {
        (*current)++;
        slot_t result = compile_exp_block(current);
        critical_check(**current == '}');
        (*current)++;
        return result;
    }
    return compile_statement(current);
}

// Jump if zero.
uint32_t* jump_zero_offset() {
    push_int8(0x0f);
    push_int8(0x84);
    uint32_t *jump_offset_point = (uint32_t*)(result_buffer + result_size);
    push_int32(0); // will be replaced later.
    return jump_offset_point;
}

uint32_t* jump_offset() {
    push_int8(0xE9);
    uint32_t *jump_offset_point = (uint32_t*)(result_buffer + result_size);
    push_int32(0); // will be replaced later. 
    return jump_offset_point;
}

void compile_if(char **current) {
    skip_gap(current);

    critical_check(**current == '(');
    (*current)++;
    // Evaluate the condition.
    slot_t condition = compile_expression(current);
    
    critical_check(**current == ')');
    (*current)++;

    cmp_const(condition, 0);
    // Jump to end of block if the condition is not met.
    uint32_t *jump_offset_point = jump_zero_offset();
    /*
    push_int8(0x0f);
    push_int8(0x84);

    uint32_t *jump_offset_point = (uint32_t*)(result_buffer + result_size);
    push_int32(0); // will be replaced later.
    */
    uint32_t body_begin = result_size;

    compile_exp_st_block(current);
    skip_gap(current);
    char *backup = *current; 
    read_ident(current);
    if (strcmp(ident_buffer, "else") == 0) { // Compile else.
        uint32_t *jump_over_else = jump_offset(); // jump over the else if we are in the if block.
        uint32_t else_body_begin = result_size;
        *jump_offset_point = result_size - body_begin; // set the jump over offset.
        compile_exp_st_block(current);
        *jump_over_else = result_size - else_body_begin;
    } else {
        *current = backup; // next token is not else, restore the reader point. 
        *jump_offset_point = result_size - body_begin; // set the jump over offset.
    } 
}

void compile_while(char **current) {
    skip_gap(current);

    critical_check(**current == '(');
    (*current)++;
    size_t loop_start = result_size;

    // Push new loop header.
    size_t old_loop_header = current_loop_header;
    current_loop_header = result_size;

    // Push new loop exit symbol.
    sym_t * loop_exit = insert_sym(global_syms, &global_sym_count, NULL, -1);
    sym_t *old_loop_exit = current_loop_exit;
    current_loop_exit = loop_exit;

    // Evaluate the condition.
    slot_t condition = compile_expression(current);

    critical_check(**current == ')');
    (*current)++;
    cmp_const(condition, 0);

    uint32_t *jump_offset_point = jump_zero_offset();
    uint32_t body_begin = result_size;

    compile_exp_st_block(current);
    
    // Return to the start of the loop.
    uint32_t *return_jump_offset_point = jump_offset();
    *return_jump_offset_point = loop_start - result_size;
    *jump_offset_point = result_size - body_begin;

    current_loop_exit->value = result_size;

    current_loop_header = old_loop_header; // restore loop header position.
    current_loop_exit = old_loop_exit; // restore loop exit symbol. 
}

slot_t compile_statement(char **current) {
    skip_gap(current);

    char *begin = *current;
    char c = **current;
    if (c >= 'a' && c <= 'z') {
        read_ident(current);
        if (strcmp(ident_buffer, "if") == 0) {
            compile_if(current);
            return -1;
        } else if (strcmp(ident_buffer, "while") == 0) {
            compile_while(current);
            return -1;
        } else if (strcmp(ident_buffer, "continue") == 0) {
            uint32_t *loop_continue = jump_offset();
            *loop_continue = current_loop_header - result_size;
            critical_check(**current == ';');
            (*current)++;
 
            return -1;
        } else if (strcmp(ident_buffer, "break") == 0) {
            // insert a jump to the end of the loop.
            jump_offset();
            push_reloc_at(current_loop_exit, ((void*)(result_buffer + result_size - 4)), true);

            critical_check(**current == ';');
            (*current)++;
 
            return -1;
        } else if (strcmp(ident_buffer, "return") == 0) {
            skip_gap(current);

            if (**current == ';') {
                (*current)++;
            } else {
                slot_t returnValue = compile_expression(current);
                mov_reg_slot(returnValue, RAX); // load the return value to the return register. 
            
                critical_check(**current == ';');
                (*current)++;
            }

            jump_offset();
            push_reloc_at(function_exit, ((void*)(result_buffer + result_size - 4)), true);
            return -1;
        } else {
            skip_gap(current);
            
            // try to parse variable assignment.
            if (**current == '=') {
                (*current)++;
                sym_t *var = define_sym(local_syms, &local_sym_count, ident_buffer);
                slot_t result = compile_expression(current);
                mov_reg_slot(result, RAX);
                mov_slot_reg(var->slot, RAX);
                critical_check(**current == ';');
                (*current)++;
                return -1;
            }

            // This is a small hack, since our tokenizer does not support look ahead.
            *current = begin;
        }
    }

    if (c == '*') { // Memory assignment.
        // *asd = ...
        //
        (*current)++;
        slot_t address = compile_exp_atom(current);
        skip_gap(current);

        critical_check(**current == '=');
        (*current)++;
        
        slot_t value = compile_expression(current);
        mov_reg_slot(address, RAX);
        mov_reg_slot(value, RDI);

        //TODO: Move this to a seperate function.
        // mov [rax], rdi
        emit_rex(1, 0, 0, 0);
        push_int8(0x89);
        emit_modrm(0, RDI, RAX);
        critical_check_msg(**current == ';', "Expected semicolon after statement.");
        (*current)++;
        return -1;
    }


    slot_t result = compile_expression(current);
    critical_check_msg(**current == ';', "Expected semicolon after statement.");
    (*current)++;
    return result;
}

slot_t compile_exp_block(char **current) {
    slot_t result;
    while (**current != 0 && **current != '}') {
        result = compile_statement(current);
        skip_gap(current);
    }
    return result;
}

void compile_exp_function_dec(char **current) {
    skip_gap(current);
    critical_check_msg(isalpha(**current), "expected identifier"); 
    read_ident(current);
    //define function.
    sym_t *function_sym = define_sym(global_syms, &global_sym_count, ident_buffer);
    function_exit = insert_sym(global_syms, &global_sym_count, NULL, 0);
    
    function_sym->is_function = true;
    function_sym->value = result_size;
    slotCount = 0; // reset stack size.
    local_sym_count = 0; // reset variables.
    
    uint32_t *stack_size_location = emit_prolog();

    skip_gap(current);

    critical_check_msg(**current == '(', "Was expecting argument list");
    (*current)++;

    reg64 registers[] = { RDI, RSI, RDX, RCX, R8, R9};
    
    size_t arg_count = 0;
    // Parse and compile function arguments.
    while (**current != ')') {
        if (arg_count != 0) {
            skip_gap(current);
            critical_check(**current == ',');
            (*current)++;
        }
        skip_gap(current);
        read_ident(current);
        sym_t *arg = define_sym(local_syms, &local_sym_count, ident_buffer);
        mov_slot_reg(arg->slot, registers[arg_count++]);
    }

    critical_check_msg(**current == ')', "Was expecting argument list to be closed.");
    (*current)++;

    
    slot_t result_slot = compile_exp_st_block(current);

    if (result_slot != -1)
        mov_reg_slot(result_slot, RAX); // result value is stored in RAX 

    function_exit->value = result_size;

    emit_epilog();
    *stack_size_location = (uint32_t)((peak_stack_size + 15) / 16 * 16);
}

char* read_file(char *filename) {
    FILE *file = fopen(filename, "r");
    critical_check_msg(file != NULL, "Couldn't open file");
    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *buffer = (char *)malloc(file_size + 1);
    fread(buffer, 1, file_size, file);
    buffer[file_size] = 0;

    return buffer;
}

void compile_program(char **current) {
    while (**current != 0) {
        compile_exp_function_dec(current);
        skip_gap(current);
    }
}

int main(int argc, char *args[]) {
    critical_check_msg(argc >= 2, "Usage: ./simplest_compiler <file_name>");

    dlHandle = dlopen(0, RTLD_NOW); // needed for dynamically linking.

    char *contents = read_file(args[1]);

    compile_program(&contents);

    // Allocate executable memory, we need to know the memory address before we link the program.
    void *executable = allocate_executable(result_size);

    // Compute absolute addresses for locally defined functions.
    for (int i = 0; i < global_sym_count; i++) {
        if (global_syms[i].is_function)
            global_syms[i].value += (uint64_t)executable;
    }

    apply_relocs();
    memcpy((void*)executable, result_buffer, result_size);

    FILE *file = fopen("assembly_output", "wb");
    fwrite(result_buffer, result_size, 1, file);
    fclose(file);
    

    // Execute the main function.    
    sym_t *funMain = resolve_sym(global_syms, global_sym_count, "main");
    void *fun = (void*)funMain->value;

    puts("executing code");
    ((int (*)(uint64_t, char**))(fun))(argc - 1, args + 1);

    return 0;
}
