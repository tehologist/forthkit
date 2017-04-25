#include <stdio.h>
#define VM_SIZE (1024 * 64) // Size in bytes of vm
#define NUM_FILE_HANDLES 8  // Number of simultaneous open files allowed
#define CELL_SIZE 2
#define SP0      6 * CELL_SIZE // Top of data stack
#define RP0      7 * CELL_SIZE // Top of return stack
#define SPP     33 * CELL_SIZE // Data stack pointer
#define RPP     34 * CELL_SIZE // Return stack pointer
#define TRUE    -1
#define FALSE    0
#define FORTH    4 * CELL_SIZE // Forth Vocabulary
#define TO_IN   17 * CELL_SIZE // >IN
#define NUM_TIB 18 * CELL_SIZE // #TIB
#define TIB     19 * CELL_SIZE // Text input buffer
#define CONTEXT 25 * CELL_SIZE // Current search vocabulary
#define CURRENT 27 * CELL_SIZE // Current vocabulary
#define CP      29 * CELL_SIZE // Code Pointer
#define NP      30 * CELL_SIZE // Next cell in name dictionary
#define LAST    31 * CELL_SIZE // Last name in dictionary
#define STATE   32 * CELL_SIZE // State of compiler
#define LIT         2
#define EXIT        3
#define INTERPRETER 0
#define WORDLIST    2
#define IMMEDIATE   3
struct FILE_HANDLE {
    int handle_set;
    FILE* handle;
};
struct IMAGE {
    char memory[VM_SIZE];
    struct FILE_HANDLE file_handles[NUM_FILE_HANDLES];
};
unsigned short create_handle(struct FILE_HANDLE * fh, FILE* handle) {
    int index = 0;
    while(index++ < NUM_FILE_HANDLES) {
        if(fh[index].handle_set == FALSE) {
			fh[index].handle_set = TRUE;
            fh[index].handle = handle;
            return index;
        }
    }
    return 0;
}
void vm_pokeByte(struct IMAGE * image, int position, unsigned char value) {
    image->memory[position] = value;
}
unsigned char vm_peekByte(struct IMAGE * image, int position) {
    return image->memory[position];
}
#define GET(X) *(unsigned short*)(image->memory + X)
#define SET(X,Y) *(unsigned short *)(image->memory + X) = Y
#define PUSHDS(X) *(unsigned short *)(image->memory + GET(SPP)) = (X & 0xFFFF);SET(SPP,GET(SPP) + CELL_SIZE)
#define PUSHRS(X) *(unsigned short *)(image->memory + GET(RPP)) = (X & 0xFFFF);SET(RPP,GET(RPP) - CELL_SIZE)
#define POPDS(X) SET(SPP,GET(SPP) - CELL_SIZE);X = *(unsigned short *)(image->memory + GET(SPP))
#define POPRS(X) SET(RPP,GET(RPP) + CELL_SIZE);X = *(unsigned short *)(image->memory + GET(RPP))
#define NEXT  PC = PC + CELL_SIZE
int vm_tostring(struct IMAGE * image, int start, char * text) {
    int ndx = 0;
    int pos = start;
    int size = image->memory[pos];
    while(ndx < (size)) {
        text[ndx] = image->memory[pos + ndx + 1];
        ndx = ndx + 1;
    }
    text[ndx] = '\0';
    return size;
}
int vm_run(struct IMAGE * image, int start) {
    int PC = start;
    long int regA, regB, regC = 0;
    PUSHRS(VM_SIZE - 1);
    while(PC < VM_SIZE - 1) {
        switch(GET(PC)) {
        case 0: // NOP
            NEXT;
            break;
        case 1: // HALT
            return PC;
        case 2: // LIT
            NEXT;
            PUSHDS(GET(PC));
            NEXT;
            break;
        case 3: // EXIT
            regA = PC;
            POPRS(PC);
            break;
        case 4: // @
            POPDS(regA);
            PUSHDS(GET(regA));
            NEXT;
            break;
        case 5: // !
            POPDS(regA);
            POPDS(regB);
            SET(regA, regB);
            NEXT;
            break;
        case 6: // DROP
            POPDS(regA);
            NEXT;
            break;
        case 7: // OVER
            POPDS(regA);
            POPDS(regB);
            PUSHDS(regB);
            PUSHDS(regA);
            PUSHDS(regB);
            NEXT;
            break;
        case 8: // SWAP
            POPDS(regA);
            POPDS(regB);
            PUSHDS(regA);
            PUSHDS(regB);
            NEXT;
            break;
        case 9: // DUP
            POPDS(regA);
            PUSHDS(regA);
            PUSHDS(regA);
            NEXT;
            break;
        case 10: // UM+
            POPDS(regA);
            POPDS(regB);
            regC = regA + regB;
            if(regC > 0xFFFF) {
                PUSHDS(regC & 0xFFFF);
                PUSHDS(1);
            }
            else {
                PUSHDS(regC);
                PUSHDS(0);
            }
            NEXT;
            break;
        case 11: // NAND
            POPDS(regA);
            POPDS(regB);
            PUSHDS(~(regA & regB) & 0xFFFF);
            NEXT;
            break;
        case 12: // 0<
            POPDS(regA);
            PUSHDS(regA > 0x8000 ? TRUE : FALSE);
            NEXT;
            break;
        case 13: // >R
            POPDS(regA);
            PUSHRS(regA);
            NEXT;
            break;
        case 14: // R>
            POPRS(regA);
            PUSHDS(regA);
            NEXT;
            break;
        case 15: // next
            NEXT;
            regA = GET(PC);
            POPRS(regB);
            regB = regB - 1;
            if(regB < 0) NEXT;
            else {
                PUSHRS(regB);
                PC = regA;
            }
            break;
        case 16: // BRANCH
            NEXT;
            PC = GET(PC);
            break;
        case 17: // 0BRANCH
            POPDS(regA);
            NEXT;
            regB = GET(PC);
            if(regA == FALSE) PC = regB;
            else NEXT;
            break;
        case 18: // TX!
            POPDS(regA);
            POPDS(regB);
			fputc(regB, image->file_handles[regA].handle);
            NEXT;
            break;
        case 19: // ?RX
            POPDS(regA);
			regA = fgetc(image->file_handles[regA].handle);
            PUSHDS(regA);
            PUSHDS(TRUE);
            NEXT;
            break;
        case 20: // F_OPEN
            {
                char filename[31];
                char options[31];
                POPDS(regB);
                POPDS(regA);
                vm_tostring(image, regA, filename);
                vm_tostring(image, regB, options);
				PUSHDS(create_handle(image->file_handles, fopen(filename,options)));
            }
            NEXT;
            break;
        case 21: // F_CLOSE
            POPDS(regA);
            fclose(image->file_handles[regA].handle);
            image->file_handles[regA].handle_set = FALSE;
            NEXT;
            break;
        case 22: // C@
            POPDS(regA);
			regB  = image->memory[regA];
            PUSHDS(regB);
            NEXT;
            break;
        case 23: // C!
            POPDS(regA);
            POPDS(regB);
			image->memory[regA] = regB;
            NEXT;
            break;
        default: // CALL
            regA = PC;
            PUSHDS(GET(PC));
            NEXT;
            PUSHRS(PC);
            POPDS(PC);
            break;
        }
    }
    return PC;
}
void vm_init(struct IMAGE * image, FILE * fi, int size) {
    int x = 0;
    while(x < NUM_FILE_HANDLES) {
        image->file_handles[x].handle_set = FALSE;
		x++;
    }
    image->file_handles[0].handle_set = TRUE;
    image->file_handles[0].handle = stdin;
    image->file_handles[1].handle_set = TRUE;
    image->file_handles[1].handle = stdout;
    x = 0;
    while(x < VM_SIZE) {
		image->memory[x++] = (fi == NULL ? 0 : fgetc(fi));
    }
    SET(SP0, 35 * CELL_SIZE);
    SET(SPP, 35 * CELL_SIZE);
    SET(RP0, 99 * CELL_SIZE);
    SET(RPP, 99 * CELL_SIZE);
}
void int_poke(struct IMAGE * image, int position, unsigned short value) {
	vm_pokeByte(image, position + 1, (value >> 8) & 0xFF);
	vm_pokeByte(image, position, (value & 0xFF));
}
unsigned short int_peek(struct IMAGE * image, int position) {
		unsigned char byte1 = vm_peekByte(image, position);
		unsigned char byte2 = vm_peekByte(image, position + 1);
		return ((byte2 << 8) | byte1);
}
int  int_getCompiler(struct IMAGE * image) { return int_peek(image, STATE); }
void int_setcompiler(struct IMAGE * image, int val) {
    int_poke(image, STATE, val); }
void int_create(struct IMAGE * image, char * word, int size,
    unsigned short type, unsigned short data) {
    int x = 0;
    int charstart;
    int position = int_peek(image, NP);
    position = position - size;
    charstart = position + 1;
    vm_pokeByte(image, position, size);
    int_poke(image, position - (3 * CELL_SIZE), int_peek(image, LAST));
    int_poke(image, LAST, position);
    while(x < size) {
		vm_pokeByte(image, charstart + x, word[x]);
		x = x + 1;
    }
    if((size) % 2 == 1) vm_pokeByte(image, charstart + size, '\0');
    int_poke(image, position - (1 * CELL_SIZE), type);
    int_poke(image, position - (2 * CELL_SIZE), data);
    int_poke(image, NP, position - (4 * CELL_SIZE));
}
void int_init(struct IMAGE * image) {
    int_poke(image, NP,      (VM_SIZE / 4) - 1);
    int_poke(image, CP,      (182 * CELL_SIZE));
    int_poke(image, NUM_TIB, (100 * CELL_SIZE));
    int_poke(image, TIB,     ((100 * CELL_SIZE) + 1));
    int_poke(image, CURRENT, FORTH);
    int_poke(image, CONTEXT, FORTH);
    int_create(image, ":",       1, INTERPRETER, 41);
    int_create(image, ";",       1, INTERPRETER, 42);
    int_create(image, "NOP",     3, WORDLIST,     0);
    int_create(image, "HALT",    4, WORDLIST,     1);
    int_create(image, "LIT",     3, WORDLIST,     2);
    int_create(image, "EXIT",    4, WORDLIST,     3);
    int_create(image, "@",       1, WORDLIST,     4);
    int_create(image, "!",       1, WORDLIST,     5);
    int_create(image, "DROP",    4, WORDLIST,     6);
    int_create(image, "OVER",    4, WORDLIST,     7);
    int_create(image, "SWAP",    4, WORDLIST,     8);
    int_create(image, "DUP",     3, WORDLIST,     9);
    int_create(image, "UM+",     3, WORDLIST,    10);
    int_create(image, "NAND",    4, WORDLIST,    11);
    int_create(image, "0<",      2, WORDLIST,    12);
    int_create(image, ">R",      2, WORDLIST,    13);
    int_create(image, "R>",      2, WORDLIST,    14);
    int_create(image, "next",    4, WORDLIST,    15);
    int_create(image, "BRANCH",  6, WORDLIST,    16);
    int_create(image, "0BRANCH", 7, WORDLIST,    17);
    int_create(image, "PUTC",    4, WORDLIST,    18);
    int_create(image, "GETC",    4, WORDLIST,    19);
    int_create(image, "F_OPEN",  6, WORDLIST,    20);
    int_create(image, "F_CLOSE", 7, WORDLIST,    21);
    int_create(image, "C@",      2, WORDLIST,    22);
    int_create(image, "C!",      2, WORDLIST,    23);
    int_poke(image, int_peek(image, CURRENT), int_peek(image,LAST));
}
int int_word(struct IMAGE * image, char * word) {
    int numIn = int_peek(image,TO_IN);
    int length = vm_peekByte(image,int_peek(image, NUM_TIB));
    int start = numIn;
    int count = 0;
    while(numIn < length) {
        char c = (char)vm_peekByte(image, int_peek(image, TIB) + numIn);
        numIn = numIn + 1;
        if(c == ' ') {
            word[count] = '\0';
            break;
        }
        else word[count] = c;
        count = count + 1;
    }
    int_poke(image, TO_IN, numIn);
    return numIn - start;
}
int int_find(struct IMAGE * image, char * word, int size) {
    int start = int_peek(image, int_peek(image, CONTEXT));
    int len   = vm_peekByte(image, start);
    while(start != 0) {
        if(len == size) {
            int match = TRUE;
            int x = 0;
			while ( x < len) {
				match &= (word[x] == vm_peekByte(image, start + 1 + x));
				x = x + 1;
			}
            if(match > FALSE)
                return start;
        }
        start = int_peek(image, start - (3 * CELL_SIZE));
        len   = vm_peekByte(image, start);
    }
    return 0;
}
int int_number(int * val, char * word, int size) {
    int ndx = 0;
    int negative = 0;
    if(size == 0)
        return 0;
    if(word[ndx] == '-') {
        negative = 1;
        ndx = ndx + 1;
    }
    while(ndx < size) {
        if(word[ndx] >= '0' && word[ndx] <= '9')
            *val = ((*val * 10) + (word[ndx] - '0'));
        else
            return 0;
        ndx = ndx + 1;
    }
    if(negative == 1)
        *val = ~*val + 1;
    return 1;
}
#define HERE int_peek(image, CP)
void int_comma(struct IMAGE * image, unsigned short val) {
    int_poke(image, HERE, val);
    int_poke(image, CP, HERE + CELL_SIZE);
}
int int_interpret(struct IMAGE * image) {
    int sz, found, num, type, data;
    char word[128];
    while(int_peek(image,TO_IN) < vm_peekByte(image,int_peek(image, NUM_TIB))) {
        sz = int_word(image, word);
        found = int_find(image, word, sz-1);
        if(found != FALSE) {
            type = int_peek(image, found - (1 * CELL_SIZE));
            data = int_peek(image, found - (2 * CELL_SIZE));
            if(type == INTERPRETER) {
                switch(data) {
                case 41: // :
                    sz = int_word(image, word);
                    int_create(image, word, sz - 1, WORDLIST, HERE);
                    int_setcompiler(image, TRUE);
                    break;
                case 42: // ;
                    int_comma(image, EXIT);
                    int_setcompiler(image, FALSE);
                    int_poke(image, int_peek(image, CURRENT), int_peek(image, LAST));
                    break;
                default:
                    break;
                }
            }
            else if(type == IMMEDIATE ||
                (type == WORDLIST && int_getCompiler(image) == FALSE)) {
                if(vm_run(image, data) == -1)
                    return -1;
            }
            else {
                int_comma(image, data);
            }
        }
        else {
            num = 0;
            if(int_number(&num,word,sz - 1)) {
                if(int_getCompiler(image) != FALSE) {
                    int_comma(image, LIT);
                    int_comma(image, num);
                }
            }
            else {
                if(word[0] != 0 && word[0] != '\n' && word[0] != '\r') {
                    word[sz] = '\0';
                }
            }
        }
    }
    return 1;
}
int int_readfile(FILE * fp, struct IMAGE * image) {
	char c = 1;
	int i;
	while(TRUE) {
		i = 0;
		while(c != '\n') {
			c = fgetc(fp);
			vm_pokeByte(image, int_peek(image, TIB) + i, c);
			i++;
			if(c == EOF) return -1;
		}
		vm_pokeByte(image, int_peek(image, NUM_TIB),i);
	    int_poke(image, TO_IN, 0);
		return i - 1;
	}
}
int int_eval(struct IMAGE * image, FILE * fi) {
    while(int_readfile(fi, image) != -1) {
        if(int_interpret(image) == -1) {
            return -1;
        }
    }
    return 0;
}
struct IMAGE image;
int main(int argc, char *argv[]) {
    FILE * f = NULL;
    if(argc > 1) {
        int x = 0;
        while(x < argc) {
            if(argv[x][0] == '-') {
                if((x + 1) <= argc) {
                    f = fopen(argv[x+1],"rb");
                    if(f != NULL) {
                        if(argv[x][1] == 'c'){
                            vm_init(&image, NULL, VM_SIZE);
                            int_init(&image);
                            int_eval(&image, f);
                        }
                        else if(argv[x][1] == 'i'){
                            vm_init(&image, f, VM_SIZE);
                        }
                        else return -1;
                        fclose(f);
                        vm_run(&image, 0);
                    }
                    break;
                }
            }
            x++;
        }
    }
    return 1;
}
