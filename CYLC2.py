registers = [0] * 8 # A, B, C, X, Y, Z, W, MAR
flags = [0] * 3 # Z, C, I
ram = [0] * (2**12)
stack = [0]
pc = 0

def fix_register(register):
    global pc, flags, ram, stack, registers
    registers[register] %= 255
    if (registers[register] < 0):
        registers[register] = 255 - registers[register]

def update_flags():
    global pc, flags, ram, stack, registers
    if (registers[0] < 0) or (registers[0] > 0xFF):
        flags[1] = 1

    if (registers[0] == 0):
        flags[0] = 1
    else:
        flags[0] = 0

def execute(instruction):
    global pc, flags, ram, stack, registers
    opcode = (instruction >> 11)
    ignore_update = False

    if (opcode == 0x00): # HLT
        exit(0)
    elif (opcode == 0x01): # ADD
        operand0 = (instruction >> 8) & 0x7
        registers[0] += registers[operand0]
        if not ignore_update:
            update_flags()
    elif (opcode == 0x02): # SUB
        operand0 = (instruction >> 8) & 0x7
        registers[0] -= registers[operand0]
        if not ignore_update:
            update_flags()
    elif (opcode == 0x03): # XOR
        operand0 = (instruction >> 8) & 0x7
        registers[0] ^= registers[operand0]
        if not ignore_update:
            update_flags()
    elif (opcode == 0x04): # AND
        operand0 = (instruction >> 8) & 0x7
        registers[0] &= registers[operand0]
        if not ignore_update:
            update_flags()
    elif (opcode == 0x05): # OR
        operand0 = (instruction >> 8) & 0x7
        registers[0] |= registers[operand0]
        if not ignore_update:
            update_flags()
    elif (opcode == 0x06): # LIM
        operand0 = (instruction >> 8) & 0x7
        operand1 = (instruction) & 0xFF
        registers[operand0] = operand1
    elif (opcode == 0x07): # STR
        operand0 = (instruction >> 8) & 0x7
        ram[registers[7]] = registers[operand0]
    elif (opcode == 0x08): # LDR
        operand0 = (instruction >> 8) & 0x7
        registers[operand0] = ram[registers[7]]
    elif (opcode == 0x09): # TRS
        operand0 = (instruction >> 8) & 0x7
        operand1 = (instruction >> 5) & 0x7
        tmp = registers[operand0]
        registers[operand0] = registers[operand1]
        registers[operand1] = tmp
    elif (opcode == 0x0A): # JMP
        pc = registers[7]
    elif (opcode == 0x0B): # JIF
        operand0 = (instruction >> 9) & 0x3
        if (operand0 == 0) and (flags[0] == 1):
            pc = registers[7]
        elif (operand0 == 1) and (flags[1] == 1):
            pc = registers[7]
        elif (operand0 == 2) and (flags[0] != 1):
            pc = registers[7]
        elif (operand0 == 3) and (flags[1] != 1):
            pc = registers[7]
        ignore_update = True
    elif (opcode == 0x0C): # STF
        operand0 = (instruction >> 9) & 0x3
        if (operand0 == 0):
            flags[0] = 1
        elif (operand0 == 1):
            flags[1] = 1
        elif (operand0 == 2):
            flags[0] = 0
        elif (operand0 == 3):
            flags[1] = 0
        ignore_update = True
    elif (opcode == 0x0D): # PUSH
        operand0 = (instruction >> 8) & 0x7
        stack.append(registers[operand0])
    elif (opcode == 0x0E): # POP
        operand0 = (instruction >> 8) & 0x7
        registers[operand0] = stack.pop()
    elif (opcode == 0x0F): # SHR
        operand0 = (instruction >> 8) & 0x7
        registers[operand0] >>= 1
        if not ignore_update:
            update_flags()
    elif (opcode == 0x10): # CMP
        operand0 = (instruction >> 8) & 0x7
        tmp = registers[0]
        registers[0] -= registers[operand0]
        update_flags()
        registers[0] = tmp
        ignore_update = True
    elif (opcode == 0x11): # ADDI
        operand0 = (instruction >> 3) & 0xFF
        registers[0] += operand0
        if not ignore_update:
            update_flags()
    elif (opcode == 0x12): # SUBI
        operand0 = (instruction >> 3) & 0xFF
        registers[0] -= operand0
        if not ignore_update:
            update_flags()
    elif (opcode == 0x13): # XORI
        operand0 = (instruction >> 3) & 0xFF
        registers[0] ^= operand0
        if not ignore_update:
            update_flags()
    elif (opcode == 0x14): # ANDI
        operand0 = (instruction >> 3) & 0xFF
        registers[0] &= operand0
        if not ignore_update:
            update_flags()
    elif (opcode == 0x15): # ORI
        operand0 = (instruction >> 3) & 0xFF
        registers[0] |= operand0
        if not ignore_update:
            update_flags()
    elif (opcode == 0x16): # SYS
        pass # not implemented yet
    elif (opcode == 0x17): # AITA
        operand0 = (instruction >> 3) & 0xFF
        registers[7] += operand0
    elif (opcode == 0x18): # RET
        pc = stack.pop()
    elif (opcode == 0x19): # INC
        operand0 = (instruction >> 8) & 0x7
        registers[operand0] += 1
        if not ignore_update:
            update_flags()
    elif (opcode == 0x1A): # DEC
        operand0 = (instruction >> 8) & 0x7
        registers[operand0] -= 1
        if not ignore_update:
            update_flags()
    elif (opcode == 0x1B): # IE
        flags[2] = 1
    elif (opcode == 0x1C): # ID
        flags[2] = 1
    elif (opcode == 0x1D): # REL
        registers[7] += pc
    elif (opcode == 0x1E): # ARTA
        operand0 = (instruction >> 8) & 0x7
        registers[7] += registers[operand0]
    elif (opcode == 0x1F): # NOP
        pass

    for reg in range(len(registers)):
        fix_register(reg)

def decode(line):
    arguments = line.split()
    instruction = -1
    operand0 = -1
    operand1 = -1

    try:
        if arguments[1] == "A":
            operand0 = 0
        elif arguments[1] == "B":
            operand0 = 1
        elif arguments[1] == "C":
            operand0 = 2
        elif arguments[1] == "X":
            operand0 = 3
        elif arguments[1] == "Y":
            operand0 = 4
        elif arguments[1] == "Z":
            operand0 = 5
        elif arguments[1] == "W":
            operand0 = 6
        elif arguments[1] == "M":
            operand0 = 7
        elif arguments[1] == "ZR":
            operand0 = 0
        elif arguments[1] == "CR":
            operand0 = 1
        elif arguments[1] == "NZ":
            operand0 = 2
        elif arguments[1] == "NC":
            operand0 = 3
        else:
            try:
                operand0 = int(arguments[1], 16)
            except ValueError:
                print("Invalid operand: ", arguments[1])
                exit(1)
    except IndexError:
        pass

    try:
        if arguments[2] == "A":
            operand1 = 0
        elif arguments[2] == "B":
            operand1 = 1
        elif arguments[2] == "C":
            operand1 = 2
        elif arguments[2] == "X":
            operand1 = 3
        elif arguments[2] == "Y":
            operand1 = 4
        elif arguments[2] == "Z":
            operand1 = 5
        elif arguments[2] == "W":
            operand1 = 6
        elif arguments[2] == "M":
            operand1 = 7
        else:
            try:
                operand1 = int(arguments[2], 16)
            except ValueError:
                print("Invalid operand: ", arguments[2])
                exit(1)
    except IndexError:
        pass

    if arguments[0] == "HLT":
        instruction = 0
    elif arguments[0] == "ADD":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x01 
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "SUB":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x02
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "XOR":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x03
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "AND":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x04 
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "OR":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x05 
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "LIM":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        if (operand1 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x06 
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
        instruction |= operand1
    elif arguments[0] == "STR":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x07
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "LDR":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x08
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "TRS":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        if (operand1 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x09
        instruction <<= 11 
        operand0 <<= 8
        operand1 <<= 5
        instruction |= operand0
        instruction |= operand1
    elif arguments[0] == "JMP":
        instruction = 0x0A
        instruction <<= 11 
    elif arguments[0] == "JIF":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x0B
        instruction <<= 11 
        operand0 <<= 9
        instruction |= operand0
    elif arguments[0] == "STF":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x0C
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "PUSH":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x0D
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "POP":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x0E
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "SHR":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x0F
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "CMP":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x10
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "ADDI":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x11
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "SUBI":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x12
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "XORI":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x13
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "ANDI":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x14
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "ORI":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x15
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "SYS":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x16
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "AITA":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x17
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "RET":
        instruction = 0x18
        instruction <<= 11 
    elif arguments[0] == "INC":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x19
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "DEC":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x1A
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0
    elif arguments[0] == "IE":
        instruction = 0x1B
        instruction <<= 11
    elif arguments[0] == "ID":
        instruction = 0x1C
        instruction <<= 11
    elif arguments[0] == "REL":
        instruction = 0x1D
        instruction <<= 11
    elif arguments[0] == "ARTA":
        if (operand0 == -1):
            print("Missing argument for: ", arguments[0])
            exit(2)
        instruction = 0x1E
        instruction <<= 11 
        operand0 <<= 8
        instruction |= operand0 
    elif arguments[0] == "NOP":
        instruction = 0x1F
        instruction <<= 11
    else:
        print("Unknown instruction: ", arguments[0])
        exit(3) 

    return instruction
        

def print_info():
    global pc, flags, ram, stack, registers
    for reg in registers:
        print(hex(reg), "  ", end='')
    print()
    for flag in flags:
        print(bin(flag)[2:], "  ", end='')
    print()
    print("PC: ", hex(pc))
    print()
    print()

with open("program.txt") as file:
    for line in file:
        instruction = decode(line)
        ram[pc] = instruction
        pc += 1
pc = 0

while True:
    update_flags()
    print_info()
    instruction = ram[pc]
    pc += 1
    execute(instruction)
    #input("Press key to continue... ")
            
