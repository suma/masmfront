
{
open Parser
open Parserhelper
open Type
open Printf

let delete_front str rm =
  let len = (String.length str) - rm in
  String.sub str rm len

let delete_back str rm =
  let len = (String.length str) - rm in
  String.sub str 0 len

let string_cut str front back =
  let len = (String.length str) - front - back in
  String.sub str front len

let symbol_of s = List.assoc s
  [
    ",", COMMA;
    ":", COLON;
    "=", EQUAL;
    "[", BR_OPEN;
    "]", BR_CLOSE;
    "(", PAR_OPEN;
    ")", PAR_CLOSE;
    "+", PLUS_OP;
    "-", MINUS_OP;
    "*", MUL_OP;
    "/", DIV_OP;
    "?", QUESTION_OP;
  ]

let cpu_of s = List.assoc s
  [
    ".186",    CPU_TYPE(CPU_186);
    ".286",    CPU_TYPE(CPU_286);
    ".286P",   CPU_TYPE(CPU_286P);
    ".287",    CPU_TYPE(CPU_287);
    ".386",    CPU_TYPE(CPU_386);
    ".386P",   CPU_TYPE(CPU_386P);
    ".387",    CPU_TYPE(CPU_387);
    ".486",    CPU_TYPE(CPU_486);
    ".486P",   CPU_TYPE(CPU_486P);
    ".586",    CPU_TYPE(CPU_586);
    ".586P",   CPU_TYPE(CPU_586P);
    ".686",    CPU_TYPE(CPU_686);
    ".686P",   CPU_TYPE(CPU_686P);
    ".K3D",    CPU_TYPE(CPU_K3D);
    ".MMX",    CPU_TYPE(CPU_MMX);
    ".XMM",    CPU_TYPE(CPU_XMM);
    ".8086",   CPU_TYPE(CPU_8080);
    ".8087",   CPU_TYPE(CPU_8087);
    ".NO87",   CPU_TYPE(CPU_NO87);

    (* not cpu *)
    ".model",     MODEL;
  ]

let keyword_of s = List.assoc s
  [
    "include",    INCLUDE;
    "INCLUDE",    INCLUDE;
    "INCLUDELIB", INCLUDELIB;
    "SEGMENT",    SEGMENT_START;
    "ENDS",       SEGMENT_END;
    "PROC",       PROC;
    "ENDP",       PROC_END;
    "PUBLIC",     PUBLIC;
    "END",        XEND;
    "EXTRN",      EXTERN;
    "EXTERN",     EXTERN;
    "ORG",        ORG;
    "npad",       NPAD;

    "tiny",       TINY;
    "small",      SMALL;
    "compact",    COMPACT;
    "medium",     MEDIUM;
    "large",      LARGE;
    "huge",       HUGE;
    "flat",       FLAT;

    "TINY",       TINY;
    "SMALL",      SMALL;
    "COMPACT",    COMPACT;
    "MEDIUM",     MEDIUM;
    "LARGE",      LARGE;
    "HUGE",       HUGE;
    "FLAT",       FLAT;
    "FLAT",       FLAT;

    "OFFSET",     OFFSET;
    "COMM",       COMMON;
    "ALIGN",      ALIGN;
    "SHORT",      SHORT;

    "DUP",        DUP;
    "ST",         ST;

    "BYTE",       BYTE;
    "SBYTE",      SBYTE;
    "WORD",       WORD;
    "SWORD",      SWORD;
    "DWORD",      DWORD;
    "SDWORD",     SDWORD;
    "FWORD",      FWORD;
    "QWORD",      QWORD;
    "TBYTE",      TBYTE;
    "REAL4",      REAL4;
    "REAL8",      REAL8;
    "REAL10",     REAL10;
    "MMWORD",     MMWORD;
    "XMMWORD",    XMMWORD;
    "PTR",        PTR;
    "DB",         DB;
    "DW",         DW;
    "DD",         DD;
    "DF",         DF;
    "DQ",         DQ;
    "DT",         DT;

    "NEAR",       NEAR;
    "FAR",        FAR;
    "NEAR16",     NEAR16;
    "NEAR32",     NEAR32;
    "FAR16",      FAR16;
    "FAR32",      FAR32;

    "ax", REGISTER(REG_AX);
    "bx", REGISTER(REG_BX);
    "cx", REGISTER(REG_CX);
    "dx", REGISTER(REG_DX);
    "bp", REGISTER(REG_BP);
    "sp", REGISTER(REG_SP);
    "di", REGISTER(REG_DI);
    "si", REGISTER(REG_SI);

    "al", REGISTER(REG_AL);
    "ah", REGISTER(REG_AH);
    "bl", REGISTER(REG_BL);
    "bh", REGISTER(REG_BH);
    "cl", REGISTER(REG_CL);
    "ch", REGISTER(REG_CH);
    "dl", REGISTER(REG_DL);
    "dh", REGISTER(REG_DH);

    "eax",  REGISTER(REG_EAX);
    "ebx",  REGISTER(REG_EBX);
    "ecx",  REGISTER(REG_ECX);
    "edx",  REGISTER(REG_EDX);
    "ebp",  REGISTER(REG_EBP);
    "esp",  REGISTER(REG_ESP);
    "edi",  REGISTER(REG_EDI);
    "esi",  REGISTER(REG_ESI);

    "cr0",  REGISTER(REG_CR0);
    "cr1",  REGISTER(REG_CR1);
    "cr2",  REGISTER(REG_CR2);
    "cr3",  REGISTER(REG_CR3);
    "dr0",  REGISTER(REG_DR0);
    "dr1",  REGISTER(REG_DR1);
    "dr2",  REGISTER(REG_DR2);
    "dr3",  REGISTER(REG_DR3);
    "dr4",  REGISTER(REG_DR4);
    "dr5",  REGISTER(REG_DR5);
    "dr6",  REGISTER(REG_DR6);
    "dr7",  REGISTER(REG_DR7);
    "tr3",  REGISTER(REG_TR3);
    "tr4",  REGISTER(REG_TR4);
    "tr5",  REGISTER(REG_TR5);
    "tr6",  REGISTER(REG_TR6);
    "tr7",  REGISTER(REG_TR7);

    "mm0",  REGISTER(REG_MM0);
    "mm1",  REGISTER(REG_MM1);
    "mm2",  REGISTER(REG_MM2);
    "mm3",  REGISTER(REG_MM3);
    "mm4",  REGISTER(REG_MM4);
    "mm5",  REGISTER(REG_MM5);
    "mm6",  REGISTER(REG_MM6);
    "mm7",  REGISTER(REG_MM7);

    "xmm0", REGISTER(REG_XMM0);
    "xmm1", REGISTER(REG_XMM1);
    "xmm2", REGISTER(REG_XMM2);
    "xmm3", REGISTER(REG_XMM3);
    "xmm4", REGISTER(REG_XMM4);
    "xmm5", REGISTER(REG_XMM5);
    "xmm6", REGISTER(REG_XMM6);
    "xmm7", REGISTER(REG_XMM7);

    "cs",   SEGMENT_REGISTER(REG_CS);
    "ds",   SEGMENT_REGISTER(REG_DS);
    "es",   SEGMENT_REGISTER(REG_ES);
    "fs",   SEGMENT_REGISTER(REG_FS);
    "gs",   SEGMENT_REGISTER(REG_GS);
    "ss",   SEGMENT_REGISTER(REG_SS);


    "rep",   INSTRUCTION_PREFIX(IST_REP);
    "repe",  INSTRUCTION_PREFIX(IST_REPE);
    "repz",  INSTRUCTION_PREFIX(IST_REPZ);
    "repne", INSTRUCTION_PREFIX(IST_REPNE);
    "repnz", INSTRUCTION_PREFIX(IST_REPNZ);
    "lock",  INSTRUCTION_PREFIX(IST_LOCK);

    "in",		INSTRUCTION(IST_IN);
    "ja",		INSTRUCTION(IST_JA);
    "jb",		INSTRUCTION(IST_JB);
    "jc",		INSTRUCTION(IST_JC);
    "je",		INSTRUCTION(IST_JE);
    "jg",		INSTRUCTION(IST_JG);
    "jl",		INSTRUCTION(IST_JL);
    "jo",		INSTRUCTION(IST_JO);
    "jp",		INSTRUCTION(IST_JP);
    "js",		INSTRUCTION(IST_JS);
    "jz",		INSTRUCTION(IST_JZ);
    "or",		INSTRUCTION(IST_OR);
    "bt",		INSTRUCTION(IST_BT);
    "aaa",  INSTRUCTION(IST_AAA);
    "aad",  INSTRUCTION(IST_AAD);
    "aam",  INSTRUCTION(IST_AAM);
    "aas",  INSTRUCTION(IST_AAS);
    "adc",  INSTRUCTION(IST_ADC);
    "add",  INSTRUCTION(IST_ADD);
    "and",  INSTRUCTION(IST_AND);
    "cbw",  INSTRUCTION(IST_CBW);
    "clc",  INSTRUCTION(IST_CLC);
    "cld",  INSTRUCTION(IST_CLD);
    "cli",  INSTRUCTION(IST_CLI);
    "cmc",  INSTRUCTION(IST_CMC);
    "cmp",  INSTRUCTION(IST_CMP);
    "cwd",  INSTRUCTION(IST_CWD);
    "daa",  INSTRUCTION(IST_DAA);
    "das",  INSTRUCTION(IST_DAS);
    "dec",  INSTRUCTION(IST_DEC);
    "div",  INSTRUCTION(IST_DIV);
    "esc",  INSTRUCTION(IST_ESC);
    "hlt",  INSTRUCTION(IST_HLT);
    "inc",  INSTRUCTION(IST_INC);
    "int",  INSTRUCTION(IST_INT);
    "jae",  INSTRUCTION(IST_JAE);
    "jbe",  INSTRUCTION(IST_JBE);
    "jge",  INSTRUCTION(IST_JGE);
    "jle",  INSTRUCTION(IST_JLE);
    "jna",  INSTRUCTION(IST_JNA);
    "jnb",  INSTRUCTION(IST_JNB);
    "jnc",  INSTRUCTION(IST_JNC);
    "jne",  INSTRUCTION(IST_JNE);
    "jng",  INSTRUCTION(IST_JNG);
    "jnl",  INSTRUCTION(IST_JNL);
    "jno",  INSTRUCTION(IST_JNO);
    "jnp",  INSTRUCTION(IST_JNP);
    "jns",  INSTRUCTION(IST_JNS);
    "jnz",  INSTRUCTION(IST_JNZ);
    "jpe",  INSTRUCTION(IST_JPE);
    "jpo",  INSTRUCTION(IST_JPO);
    "jmp",  INSTRUCTION(IST_JMP);
    "lds",  INSTRUCTION(IST_LDS);
    "lea",  INSTRUCTION(IST_LEA);
    "les",  INSTRUCTION(IST_LES);
    "mov",  INSTRUCTION(IST_MOV);
    "mul",  INSTRUCTION(IST_MUL);
    "neg",  INSTRUCTION(IST_NEG);
    "nop",  INSTRUCTION(IST_NOP);
    "not",  INSTRUCTION(IST_NOT);
    "out",  INSTRUCTION(IST_OUT);
    "pop",  INSTRUCTION(IST_POP);
    "rcl",  INSTRUCTION(IST_RCL);
    "rcr",  INSTRUCTION(IST_RCR);
    "ret",  INSTRUCTION(IST_RET);
    "rol",  INSTRUCTION(IST_ROL);
    "ror",  INSTRUCTION(IST_ROR);
    "sal",  INSTRUCTION(IST_SAL);
    "sar",  INSTRUCTION(IST_SAR);
    "sbb",  INSTRUCTION(IST_SBB);
    "shl",  INSTRUCTION(IST_SHL);
    "shr",  INSTRUCTION(IST_SHR);
    "stc",  INSTRUCTION(IST_STC);
    "std",  INSTRUCTION(IST_STD);
    "sti",  INSTRUCTION(IST_STI);
    "sub",  INSTRUCTION(IST_SUB);
    "xor",  INSTRUCTION(IST_XOR);
    "ins",  INSTRUCTION(IST_INS);
    "lar",  INSTRUCTION(IST_LAR);
    "lsl",  INSTRUCTION(IST_LSL);
    "ltr",  INSTRUCTION(IST_LTR);
    "str",  INSTRUCTION(IST_STR);
    "bsf",  INSTRUCTION(IST_BSF);
    "bsr",  INSTRUCTION(IST_BSR);
    "btc",  INSTRUCTION(IST_BTC);
    "btr",  INSTRUCTION(IST_BTR);
    "bts",  INSTRUCTION(IST_BTS);
    "cdq",  INSTRUCTION(IST_CDQ);
    "lfs",  INSTRUCTION(IST_LFS);
    "lgs",  INSTRUCTION(IST_LGS);
    "lss",  INSTRUCTION(IST_LSS);
    "ud2",  INSTRUCTION(IST_UD2);
    "por",  INSTRUCTION(IST_POR);
    "call", INSTRUCTION(IST_CALL);
    "idiv", INSTRUCTION(IST_IDIV);
    "imul", INSTRUCTION(IST_IMUL);
    "into", INSTRUCTION(IST_INTO);
    "iret", INSTRUCTION(IST_IRET);
    "jcxz", INSTRUCTION(IST_JCXZ);
    "jnae", INSTRUCTION(IST_JNAE);
    "jnbe", INSTRUCTION(IST_JNBE);
    "jnge", INSTRUCTION(IST_JNGE);
    "jnle", INSTRUCTION(IST_JNLE);
    "lahf", INSTRUCTION(IST_LAHF);
    "loop", INSTRUCTION(IST_LOOP);
    "popf", INSTRUCTION(IST_POPF);
    "push", INSTRUCTION(IST_PUSH);
    "retn", INSTRUCTION(IST_RETN);
    "retf", INSTRUCTION(IST_RETF);
    "sahf", INSTRUCTION(IST_SAHF);
    "test", INSTRUCTION(IST_TEST);
    "wait", INSTRUCTION(IST_WAIT);
    "xchg", INSTRUCTION(IST_XCHG);
    "xlat", INSTRUCTION(IST_XLAT);
    "outs", INSTRUCTION(IST_OUTS);
    "popa", INSTRUCTION(IST_POPA);
    "arpl", INSTRUCTION(IST_ARPL);
    "clts", INSTRUCTION(IST_CLTS);
    "lgdt", INSTRUCTION(IST_LGDT);
    "lidt", INSTRUCTION(IST_LIDT);
    "lldt", INSTRUCTION(IST_LLDT);
    "lmsw", INSTRUCTION(IST_LMSW);
    "sgdt", INSTRUCTION(IST_SGDT);
    "sidt", INSTRUCTION(IST_SIDT);
    "sldt", INSTRUCTION(IST_SLDT);
    "smsw", INSTRUCTION(IST_SMSW);
    "verr", INSTRUCTION(IST_VERR);
    "verw", INSTRUCTION(IST_VERW);
    "cwde", INSTRUCTION(IST_CWDE);
    "insb", INSTRUCTION(IST_INSB);
    "insw", INSTRUCTION(IST_INSW);
    "insd", INSTRUCTION(IST_INSD);
    "insq", INSTRUCTION(IST_INSQ);
    "seta", INSTRUCTION(IST_SETA);
    "setb", INSTRUCTION(IST_SETB);
    "setc", INSTRUCTION(IST_SETC);
    "sete", INSTRUCTION(IST_SETE);
    "setg", INSTRUCTION(IST_SETG);
    "setl", INSTRUCTION(IST_SETL);
    "seto", INSTRUCTION(IST_SETO);
    "setp", INSTRUCTION(IST_SETP);
    "sets", INSTRUCTION(IST_SETS);
    "setz", INSTRUCTION(IST_SETZ);
    "shld", INSTRUCTION(IST_SHLD);
    "shrd", INSTRUCTION(IST_SHRD);
    "invd", INSTRUCTION(IST_INVD);
    "xadd", INSTRUCTION(IST_XADD);
    "pand", INSTRUCTION(IST_PAND);
    "pxor", INSTRUCTION(IST_PXOR);
    "movd", INSTRUCTION(IST_MOVD);
    "movq", INSTRUCTION(IST_MOVQ);
    "emms", INSTRUCTION(IST_EMMS);
    "orps", INSTRUCTION(IST_ORPS);
    "clgi", INSTRUCTION(IST_CLGI);
    "stgi", INSTRUCTION(IST_STGI);
    "dpps", INSTRUCTION(IST_DPPS);
    "dppd", INSTRUCTION(IST_DPPD);
    "cmpsb",  INSTRUCTION(IST_CMPSB);
    "cmpsw",  INSTRUCTION(IST_CMPSW);
    "jecxz",  INSTRUCTION(IST_JECXZ);
    "lodsb",  INSTRUCTION(IST_LODSB);
    "lodsw",  INSTRUCTION(IST_LODSW);
    "lodsd",  INSTRUCTION(IST_LODSD);
    "loopx",  INSTRUCTION(IST_LOOPX);
    "movsb",  INSTRUCTION(IST_MOVSB);
    "movsw",  INSTRUCTION(IST_MOVSW);
    "pushf",  INSTRUCTION(IST_PUSHF);
    "scasb",  INSTRUCTION(IST_SCASB);
    "scasw",  INSTRUCTION(IST_SCASW);
    "stosb",  INSTRUCTION(IST_STOSB);
    "stosw",  INSTRUCTION(IST_STOSW);
    "bound",  INSTRUCTION(IST_BOUND);
    "enter",  INSTRUCTION(IST_ENTER);
    "leave",  INSTRUCTION(IST_LEAVE);
    "pusha",  INSTRUCTION(IST_PUSHA);
    "cmpsd",  INSTRUCTION(IST_CMPSD);
    "iretb",  INSTRUCTION(IST_IRETB);
    "iretw",  INSTRUCTION(IST_IRETW);
    "iretd",  INSTRUCTION(IST_IRETD);
    "iretq",  INSTRUCTION(IST_IRETQ);
    "loope",  INSTRUCTION(IST_LOOPE);
    "loopz",  INSTRUCTION(IST_LOOPZ);
    "movsx",  INSTRUCTION(IST_MOVSX);
    "movsd",  INSTRUCTION(IST_MOVSD);
    "movzx",  INSTRUCTION(IST_MOVZX);
    "popad",  INSTRUCTION(IST_POPAD);
    "popfd",  INSTRUCTION(IST_POPFD);
    "scasd",  INSTRUCTION(IST_SCASD);
    "setae",  INSTRUCTION(IST_SETAE);
    "setbe",  INSTRUCTION(IST_SETBE);
    "setge",  INSTRUCTION(IST_SETGE);
    "setle",  INSTRUCTION(IST_SETLE);
    "setna",  INSTRUCTION(IST_SETNA);
    "setnb",  INSTRUCTION(IST_SETNB);
    "setnc",  INSTRUCTION(IST_SETNC);
    "setne",  INSTRUCTION(IST_SETNE);
    "setng",  INSTRUCTION(IST_SETNG);
    "setnl",  INSTRUCTION(IST_SETNL);
    "setno",  INSTRUCTION(IST_SETNO);
    "setnp",  INSTRUCTION(IST_SETNP);
    "setns",  INSTRUCTION(IST_SETNS);
    "setnz",  INSTRUCTION(IST_SETNZ);
    "setpe",  INSTRUCTION(IST_SETPE);
    "setpo",  INSTRUCTION(IST_SETPO);
    "stosd",  INSTRUCTION(IST_STOSD);
    "stosq",  INSTRUCTION(IST_STOSQ);
    "bswap",  INSTRUCTION(IST_BSWAP);
    "cpuid",  INSTRUCTION(IST_CPUID);
    "rdmsr",  INSTRUCTION(IST_RDMSR);
    "rdtsc",  INSTRUCTION(IST_RDTSC);
    "wrmsr",  INSTRUCTION(IST_WRMSR);
    "cmova",  INSTRUCTION(IST_CMOVA);
    "cmovb",  INSTRUCTION(IST_CMOVB);
    "cmovc",  INSTRUCTION(IST_CMOVC);
    "cmove",  INSTRUCTION(IST_CMOVE);
    "cmovg",  INSTRUCTION(IST_CMOVG);
    "cmovl",  INSTRUCTION(IST_CMOVL);
    "cmovo",  INSTRUCTION(IST_CMOVO);
    "cmovp",  INSTRUCTION(IST_CMOVP);
    "cmovs",  INSTRUCTION(IST_CMOVS);
    "cmovz",  INSTRUCTION(IST_CMOVZ);
    "rdpmc",  INSTRUCTION(IST_RDPMC);
    "paddb",  INSTRUCTION(IST_PADDB);
    "paddw",  INSTRUCTION(IST_PADDW);
    "paddd",  INSTRUCTION(IST_PADDD);
    "psubb",  INSTRUCTION(IST_PSUBB);
    "psubw",  INSTRUCTION(IST_PSUBW);
    "psubd",  INSTRUCTION(IST_PSUBD);
    "pmull",  INSTRUCTION(IST_PMULL);
    "pmulh",  INSTRUCTION(IST_PMULH);
    "pmadd",  INSTRUCTION(IST_PMADD);
    "pandn",  INSTRUCTION(IST_PANDN);
    "psllw",  INSTRUCTION(IST_PSLLW);
    "pslld",  INSTRUCTION(IST_PSLLD);
    "psrlw",  INSTRUCTION(IST_PSRLW);
    "psrld",  INSTRUCTION(IST_PSRLD);
    "psraw",  INSTRUCTION(IST_PSRAW);
    "psrad",  INSTRUCTION(IST_PSRAD);
    "psllq",  INSTRUCTION(IST_PSLLQ);
    "psrlq",  INSTRUCTION(IST_PSRLQ);
    "movss",  INSTRUCTION(IST_MOVSS);
    "addss",  INSTRUCTION(IST_ADDSS);
    "subss",  INSTRUCTION(IST_SUBSS);
    "mulss",  INSTRUCTION(IST_MULSS);
    "divss",  INSTRUCTION(IST_DIVSS);
    "rcpss",  INSTRUCTION(IST_RCPSS);
    "maxss",  INSTRUCTION(IST_MAXSS);
    "minss",  INSTRUCTION(IST_MINSS);
    "addps",  INSTRUCTION(IST_ADDPS);
    "subps",  INSTRUCTION(IST_SUBPS);
    "mulps",  INSTRUCTION(IST_MULPS);
    "divps",  INSTRUCTION(IST_DIVPS);
    "rcpps",  INSTRUCTION(IST_RCPPS);
    "maxps",  INSTRUCTION(IST_MAXPS);
    "minps",  INSTRUCTION(IST_MINPS);
    "cmpss",  INSTRUCTION(IST_CMPSS);
    "cmpps",  INSTRUCTION(IST_CMPPS);
    "andps",  INSTRUCTION(IST_ANDPS);
    "xorps",  INSTRUCTION(IST_XORPS);
    "pavgb",  INSTRUCTION(IST_PAVGB);
    "pavgw",  INSTRUCTION(IST_PAVGW);
    "pause",  INSTRUCTION(IST_PAUSE);
    "lddqu",  INSTRUCTION(IST_LDDQU);
    "mwait",  INSTRUCTION(IST_MWAIT);
    "pabsb",  INSTRUCTION(IST_PABSB);
    "pabsw",  INSTRUCTION(IST_PABSW);
    "pabsd",  INSTRUCTION(IST_PABSD);
    "vmxon",  INSTRUCTION(IST_VMXON);
    "vmrun",  INSTRUCTION(IST_VMRUN);
    "ptest",  INSTRUCTION(IST_PTEST);
    "crc32",  INSTRUCTION(IST_CRC32);
    "lzcnt",  INSTRUCTION(IST_LZCNT);
    "loopne",		INSTRUCTION(IST_LOOPNE);
    "loopnz",		INSTRUCTION(IST_LOOPNZ);
    "pushad",		INSTRUCTION(IST_PUSHAD);
    "pushfd",		INSTRUCTION(IST_PUSHFD);
    "setnae",		INSTRUCTION(IST_SETNAE);
    "setnbe",		INSTRUCTION(IST_SETNBE);
    "setnge",		INSTRUCTION(IST_SETNGE);
    "setnle",		INSTRUCTION(IST_SETNLE);
    "invlpg",		INSTRUCTION(IST_INVLPG);
    "wbinvd",		INSTRUCTION(IST_WBINVD);
    "cmovae",		INSTRUCTION(IST_CMOVAE);
    "cmovbe",		INSTRUCTION(IST_CMOVBE);
    "cmovge",		INSTRUCTION(IST_CMOVGE);
    "cmovle",		INSTRUCTION(IST_CMOVLE);
    "cmovna",		INSTRUCTION(IST_CMOVNA);
    "cmovnb",		INSTRUCTION(IST_CMOVNB);
    "cmovnc",		INSTRUCTION(IST_CMOVNC);
    "cmovne",		INSTRUCTION(IST_CMOVNE);
    "cmovng",		INSTRUCTION(IST_CMOVNG);
    "cmovnl",		INSTRUCTION(IST_CMOVNL);
    "cmovno",		INSTRUCTION(IST_CMOVNO);
    "cmovnp",		INSTRUCTION(IST_CMOVNP);
    "cmovns",		INSTRUCTION(IST_CMOVNS);
    "cmovnz",		INSTRUCTION(IST_CMOVNZ);
    "cmovpe",		INSTRUCTION(IST_CMOVPE);
    "cmovpo",		INSTRUCTION(IST_CMOVPO);
    "sysret",		INSTRUCTION(IST_SYSRET);
    "paddsb",		INSTRUCTION(IST_PADDSB);
    "paddsw",		INSTRUCTION(IST_PADDSW);
    "psubsb",		INSTRUCTION(IST_PSUBSB);
    "psubsw",		INSTRUCTION(IST_PSUBSW);
    "movaps",		INSTRUCTION(IST_MOVAPS);
    "movups",		INSTRUCTION(IST_MOVUPS);
    "movlps",		INSTRUCTION(IST_MOVLPS);
    "movhps",		INSTRUCTION(IST_MOVHPS);
    "sqrtss",		INSTRUCTION(IST_SQRTSS);
    "sqrtps",		INSTRUCTION(IST_SQRTPS);
    "comiss",		INSTRUCTION(IST_COMISS);
    "shufps",		INSTRUCTION(IST_SHUFPS);
    "andnps",		INSTRUCTION(IST_ANDNPS);
    "psadbw",		INSTRUCTION(IST_PSADBW);
    "pmaxub",		INSTRUCTION(IST_PMAXUB);
    "pminub",		INSTRUCTION(IST_PMINUB);
    "pmaxsw",		INSTRUCTION(IST_PMAXSW);
    "pminsw",		INSTRUCTION(IST_PMINSW);
    "pextrw",		INSTRUCTION(IST_PEXTRW);
    "pinsrw",		INSTRUCTION(IST_PINSRW);
    "pshufw",		INSTRUCTION(IST_PSHUFW);
    "movntq",		INSTRUCTION(IST_MOVNTQ);
    "sfence",		INSTRUCTION(IST_SFENCE);
    "lfence",		INSTRUCTION(IST_LFENCE);
    "mfence",		INSTRUCTION(IST_MFENCE);
    "movnti",		INSTRUCTION(IST_MOVNTI);
    "haddpd",		INSTRUCTION(IST_HADDPD);
    "haddps",		INSTRUCTION(IST_HADDPS);
    "hsubpd",		INSTRUCTION(IST_HSUBPD);
    "hsubps",		INSTRUCTION(IST_HSUBPS);
    "psignb",		INSTRUCTION(IST_PSIGNB);
    "psignw",		INSTRUCTION(IST_PSIGNW);
    "psignd",		INSTRUCTION(IST_PSIGND);
    "pshufb",		INSTRUCTION(IST_PSHUFB);
    "phsubw",		INSTRUCTION(IST_PHSUBW);
    "phsubd",		INSTRUCTION(IST_PHSUBD);
    "phaddw",		INSTRUCTION(IST_PHADDW);
    "phaddd",		INSTRUCTION(IST_PHADDD);
    "vmread",		INSTRUCTION(IST_VMREAD);
    "vmcall",		INSTRUCTION(IST_VMCALL);
    "vmxoff",		INSTRUCTION(IST_VMXOFF);
    "skinit",		INSTRUCTION(IST_SKINIT);
    "vmload",		INSTRUCTION(IST_VMLOAD);
    "vmsave",		INSTRUCTION(IST_VMSAVE);
    "rdtscp",		INSTRUCTION(IST_RDTSCP);
    "pmuldq",		INSTRUCTION(IST_PMULDQ);
    "pmulld",		INSTRUCTION(IST_PMULLD);
    "pminsb",		INSTRUCTION(IST_PMINSB);
    "pmaxsb",		INSTRUCTION(IST_PMAXSB);
    "pminuw",		INSTRUCTION(IST_PMINUW);
    "pmaxuw",		INSTRUCTION(IST_PMAXUW);
    "pminud",		INSTRUCTION(IST_PMINUD);
    "pmaxud",		INSTRUCTION(IST_PMAXUD);
    "pminsd",		INSTRUCTION(IST_PMINSD);
    "pmaxsd",		INSTRUCTION(IST_PMAXSD);
    "pinsrb",		INSTRUCTION(IST_PINSRB);
    "pinsrd",		INSTRUCTION(IST_PINSRD);
    "pinsrq",		INSTRUCTION(IST_PINSRQ);
    "pextrb",		INSTRUCTION(IST_PEXTRB);
    "pextrd",		INSTRUCTION(IST_PEXTRD);
    "pextrq",		INSTRUCTION(IST_PEXTRQ);
    "popcnt",		INSTRUCTION(IST_POPCNT);
    "loadall",  INSTRUCTION(IST_LOADALL);
    "cmpxchg",  INSTRUCTION(IST_CMPXCHG);
    "cmovnae",  INSTRUCTION(IST_CMOVNAE);
    "cmovnbe",  INSTRUCTION(IST_CMOVNBE);
    "cmovnge",  INSTRUCTION(IST_CMOVNGE);
    "cmovnle",  INSTRUCTION(IST_CMOVNLE);
    "sysexit",  INSTRUCTION(IST_SYSEXIT);
    "syscall",  INSTRUCTION(IST_SYSCALL);
    "paddusb",  INSTRUCTION(IST_PADDUSB);
    "paddusw",  INSTRUCTION(IST_PADDUSW);
    "psubusb",  INSTRUCTION(IST_PSUBUSB);
    "psubusw",  INSTRUCTION(IST_PSUBUSW);
    "pcmpeqb",  INSTRUCTION(IST_PCMPEQB);
    "pcmpeqw",  INSTRUCTION(IST_PCMPEQW);
    "pcmpeqd",  INSTRUCTION(IST_PCMPEQD);
    "movlhps",  INSTRUCTION(IST_MOVLHPS);
    "movhlps",  INSTRUCTION(IST_MOVHLPS);
    "rsqrtss",  INSTRUCTION(IST_RSQRTSS);
    "rsqrtps",  INSTRUCTION(IST_RSQRTPS);
    "ucomiss",  INSTRUCTION(IST_UCOMISS);
    "pmulhuw",  INSTRUCTION(IST_PMULHUW);
    "ldmxcsr",  INSTRUCTION(IST_LDMXCSR);
    "stmxcsr",  INSTRUCTION(IST_STMXCSR);
    "movntps",  INSTRUCTION(IST_MOVNTPS);
    "clflush",  INSTRUCTION(IST_CLFLUSH);
    "movntdq",  INSTRUCTION(IST_MOVNTDQ);
    "movntpd",  INSTRUCTION(IST_MOVNTPD);
    "movddup",  INSTRUCTION(IST_MOVDDUP);
    "fisfttp",  INSTRUCTION(IST_FISFTTP);
    "monitor",  INSTRUCTION(IST_MONITOR);
    "palignr",  INSTRUCTION(IST_PALIGNR);
    "phaddsw",  INSTRUCTION(IST_PHADDSW);
    "vmptrld",  INSTRUCTION(IST_VMPTRLD);
    "vmptrst",  INSTRUCTION(IST_VMPTRST);
    "vmclear",  INSTRUCTION(IST_VMCLEAR);
    "vmwrite",  INSTRUCTION(IST_VMWRITE);
    "vmmcall",  INSTRUCTION(IST_VMMCALL);
    "mpsadbw",  INSTRUCTION(IST_MPSADBW);
    "blendps",  INSTRUCTION(IST_BLENDPS);
    "blendpd",  INSTRUCTION(IST_BLENDPD);
    "pblendw",  INSTRUCTION(IST_PBLENDW);
    "roundps",  INSTRUCTION(IST_ROUNDPS);
    "roundss",  INSTRUCTION(IST_ROUNDSS);
    "roundpd",  INSTRUCTION(IST_ROUNDPD);
    "roundsd",  INSTRUCTION(IST_ROUNDSD);
    "pcmpeqq",  INSTRUCTION(IST_PCMPEQQ);
    "pcmpgtq",  INSTRUCTION(IST_PCMPGTQ);
    "sysenter", INSTRUCTION(IST_SYSENTER);
    "pcmpgtpb", INSTRUCTION(IST_PCMPGTPB);
    "pcmpgtpw", INSTRUCTION(IST_PCMPGTPW);
    "pcmpgtpd", INSTRUCTION(IST_PCMPGTPD);
    "packsswb", INSTRUCTION(IST_PACKSSWB);
    "packssdw", INSTRUCTION(IST_PACKSSDW);
    "packuswb", INSTRUCTION(IST_PACKUSWB);
    "unpckhps", INSTRUCTION(IST_UNPCKHPS);
    "unpcklps", INSTRUCTION(IST_UNPCKLPS);
    "cvtsi2ss", INSTRUCTION(IST_CVTSI2SS);
    "cvtss2si", INSTRUCTION(IST_CVTSS2SI);
    "cvtpi2ps", INSTRUCTION(IST_CVTPI2PS);
    "cvtps2pi", INSTRUCTION(IST_CVTPS2PI);
    "pmovmskb", INSTRUCTION(IST_PMOVMSKB);
    "maskmovq", INSTRUCTION(IST_MASKMOVQ);
    "addsubpd", INSTRUCTION(IST_ADDSUBPD);
    "addsubps", INSTRUCTION(IST_ADDSUBPS);
    "movshdup", INSTRUCTION(IST_MOVSHDUP);
    "movsldup", INSTRUCTION(IST_MOVSLDUP);
    "pmulhrsw", INSTRUCTION(IST_PMULHRSW);
    "vmlaunch", INSTRUCTION(IST_VMLAUNCH);
    "vmresume", INSTRUCTION(IST_VMRESUME);
    "blendvps", INSTRUCTION(IST_BLENDVPS);
    "blendvpd", INSTRUCTION(IST_BLENDVPD);
    "pblendvb", INSTRUCTION(IST_PBLENDVB);
    "insertps", INSTRUCTION(IST_INSERTPS);
    "pmovsxbw", INSTRUCTION(IST_PMOVSXBW);
    "pmovzxbw", INSTRUCTION(IST_PMOVZXBW);
    "pmovsxbd", INSTRUCTION(IST_PMOVSXBD);
    "pmovzxbd", INSTRUCTION(IST_PMOVZXBD);
    "pmovsxbq", INSTRUCTION(IST_PMOVSXBQ);
    "pmovzxbq", INSTRUCTION(IST_PMOVZXBQ);
    "pmovsxwd", INSTRUCTION(IST_PMOVSXWD);
    "pmovzxwd", INSTRUCTION(IST_PMOVZXWD);
    "pmovsxwq", INSTRUCTION(IST_PMOVSXWQ);
    "pmovzxwq", INSTRUCTION(IST_PMOVZXWQ);
    "pmovsxdq", INSTRUCTION(IST_PMOVSXDQ);
    "pmovzxdq", INSTRUCTION(IST_PMOVZXDQ);
    "packusdw", INSTRUCTION(IST_PACKUSDW);
    "movntdqa", INSTRUCTION(IST_MOVNTDQA);
    "cmpxchg8b",  INSTRUCTION(IST_CMPXCHG8B);
    "punpckhbw",  INSTRUCTION(IST_PUNPCKHBW);
    "punpckhwd",  INSTRUCTION(IST_PUNPCKHWD);
    "punpckhdq",  INSTRUCTION(IST_PUNPCKHDQ);
    "punpcklbw",  INSTRUCTION(IST_PUNPCKLBW);
    "punpcklwd",  INSTRUCTION(IST_PUNPCKLWD);
    "punpckldq",  INSTRUCTION(IST_PUNPCKLDQ);
    "cvttss2si",  INSTRUCTION(IST_CVTTSS2SI);
    "cvttps2pi",  INSTRUCTION(IST_CVTTPS2PI);
    "prefetch0",  INSTRUCTION(IST_PREFETCH0);
    "prefetch1",  INSTRUCTION(IST_PREFETCH1);
    "prefetch2",  INSTRUCTION(IST_PREFETCH2);
    "pmaddubsw",  INSTRUCTION(IST_PMADDUBSW);
    "extractps",  INSTRUCTION(IST_EXTRACTPS);
    "pcmpestri",  INSTRUCTION(IST_PCMPESTRI);
    "pcmpestrm",  INSTRUCTION(IST_PCMPESTRM);
    "pcmpistri",  INSTRUCTION(IST_PCMPISTRI);
    "pcmpistrm",  INSTRUCTION(IST_PCMPISTRM);
    "maskmovdqu", INSTRUCTION(IST_MASKMOVDQU);
    "cmpxchg16b", INSTRUCTION(IST_CMPXCHG16B);
    "phminposuw", INSTRUCTION(IST_PHMINPOSUW);
    "prefetchnta", INSTRUCTION(IST_PREFETCHNTA);

    (* FPU *)
    "f2xm1",  INSTRUCTION(IST_F2XM1);
    "FABS", INSTRUCTION(IST_FABS);
    "fadd", INSTRUCTION(IST_FADD);
    "faddp",  INSTRUCTION(IST_FADDP);
    "fbld", INSTRUCTION(IST_FBLD);
    "fbstp",  INSTRUCTION(IST_FBSTP);
    "fchs", INSTRUCTION(IST_FCHS);
    "fclex",  INSTRUCTION(IST_FCLEX);
    "fcom", INSTRUCTION(IST_FCOM);
    "fcomp",  INSTRUCTION(IST_FCOMP);
    "fcompp", INSTRUCTION(IST_FCOMPP);
    "fdecstp",  INSTRUCTION(IST_FDECSTP);
    "fdisi",  INSTRUCTION(IST_FDISI);
    "fdiv", INSTRUCTION(IST_FDIV);
    "fdivp",  INSTRUCTION(IST_FDIVP);
    "fdivr",  INSTRUCTION(IST_FDIVR);
    "fdivrp", INSTRUCTION(IST_FDIVRP);
    "feni", INSTRUCTION(IST_FENI);
    "ffree",  INSTRUCTION(IST_FFREE);
    "fiadd",  INSTRUCTION(IST_FIADD);
    "ficom",  INSTRUCTION(IST_FICOM);
    "ficomp", INSTRUCTION(IST_FICOMP);
    "fidiv",  INSTRUCTION(IST_FIDIV);
    "fidivr", INSTRUCTION(IST_FIDIVR);
    "fild", INSTRUCTION(IST_FILD);
    "fimul",  INSTRUCTION(IST_FIMUL);
    "fincstp",  INSTRUCTION(IST_FINCSTP);
    "finit",  INSTRUCTION(IST_FINIT);
    "fist", INSTRUCTION(IST_FIST);
    "fistp",  INSTRUCTION(IST_FISTP);
    "fisub",  INSTRUCTION(IST_FISUB);
    "fisubr", INSTRUCTION(IST_FISUBR);
    "fld",  INSTRUCTION(IST_FLD);
    "fld1", INSTRUCTION(IST_FLD1);
    "fldcw",  INSTRUCTION(IST_FLDCW);
    "fldenv", INSTRUCTION(IST_FLDENV);
    "fldenvw",  INSTRUCTION(IST_FLDENVW);
    "fldl2e", INSTRUCTION(IST_FLDL2E);
    "fldl2t", INSTRUCTION(IST_FLDL2T);
    "fldlg2", INSTRUCTION(IST_FLDLG2);
    "fldln2", INSTRUCTION(IST_FLDLN2);
    "fldpi",  INSTRUCTION(IST_FLDPI);
    "fldz", INSTRUCTION(IST_FLDZ);
    "fmul", INSTRUCTION(IST_FMUL);
    "fmulp",  INSTRUCTION(IST_FMULP);
    "fnclex", INSTRUCTION(IST_FNCLEX);
    "fndisi", INSTRUCTION(IST_FNDISI);
    "fneni",  INSTRUCTION(IST_FNENI);
    "fninit", INSTRUCTION(IST_FNINIT);
    "fnop", INSTRUCTION(IST_FNOP);
    "fnsave", INSTRUCTION(IST_FNSAVE);
    "fnsavew",  INSTRUCTION(IST_FNSAVEW);
    "fnstcw", INSTRUCTION(IST_FNSTCW);
    "fnstenv",  INSTRUCTION(IST_FNSTENV);
    "fnstenvw", INSTRUCTION(IST_FNSTENVW);
    "fnstsw", INSTRUCTION(IST_FNSTSW);
    "fpatan", INSTRUCTION(IST_FPATAN);
    "fprem",  INSTRUCTION(IST_FPREM);
    "fptan",  INSTRUCTION(IST_FPTAN);
    "frndint",  INSTRUCTION(IST_FRNDINT);
    "frstor", INSTRUCTION(IST_FRSTOR);
    "frstorw",  INSTRUCTION(IST_FRSTORW);
    "fsave",  INSTRUCTION(IST_FSAVE);
    "fsavew", INSTRUCTION(IST_FSAVEW);
    "fscale", INSTRUCTION(IST_FSCALE);
    "fsqrt",  INSTRUCTION(IST_FSQRT);
    "fst",  INSTRUCTION(IST_FST);
    "fstcw",  INSTRUCTION(IST_FSTCW);
    "fstenv", INSTRUCTION(IST_FSTENV);
    "fstenvw",  INSTRUCTION(IST_FSTENVW);
    "fstp", INSTRUCTION(IST_FSTP);
    "fstsw",  INSTRUCTION(IST_FSTSW);
    "fsub", INSTRUCTION(IST_FSUB);
    "fsubp",  INSTRUCTION(IST_FSUBP);
    "fsubr",  INSTRUCTION(IST_FSUBR);
    "fsubrp", INSTRUCTION(IST_FSUBRP);
    "ftst", INSTRUCTION(IST_FTST);
    "fwait",  INSTRUCTION(IST_FWAIT);
    "fxam", INSTRUCTION(IST_FXAM);
    "fxch", INSTRUCTION(IST_FXCH);
    "fxtract",  INSTRUCTION(IST_FXTRACT);
    "fyl2x",  INSTRUCTION(IST_FYL2X);
    "fyl2xp1",  INSTRUCTION(IST_FYL2XP1);
    "fsetpm", INSTRUCTION(IST_FSETPM);
    "fcos", INSTRUCTION(IST_FCOS);
    "fldenvd",  INSTRUCTION(IST_FLDENVD);
    "fnsaved",  INSTRUCTION(IST_FNSAVED);
    "fnstenvd", INSTRUCTION(IST_FNSTENVD);
    "fprem1", INSTRUCTION(IST_FPREM1);
    "frstord",  INSTRUCTION(IST_FRSTORD);
    "fsaved", INSTRUCTION(IST_FSAVED);
    "fsin", INSTRUCTION(IST_FSIN);
    "fsincos",  INSTRUCTION(IST_FSINCOS);
    "fstenvd",  INSTRUCTION(IST_FSTENVD);
    "fucom",  INSTRUCTION(IST_FUCOM);
    "fucomp", INSTRUCTION(IST_FUCOMP);
    "fucompp",  INSTRUCTION(IST_FUCOMPP);
    "fcmovbe",  INSTRUCTION(IST_FCMOVBE);
    "fcmove", INSTRUCTION(IST_FCMOVE);
    "fcmovnb",  INSTRUCTION(IST_FCMOVNB);
    "fcmovnbe", INSTRUCTION(IST_FCMOVNBE);
    "fcmovne",  INSTRUCTION(IST_FCMOVNE);
    "fcmovnu",  INSTRUCTION(IST_FCMOVNU);
    "fcmovu", INSTRUCTION(IST_FCMOVU);
    "fcomip", INSTRUCTION(IST_FCOMIP);
    "fucomi", INSTRUCTION(IST_FUCOMI);
    "fucomip",  INSTRUCTION(IST_FUCOMIP);
    "fxrstor",  INSTRUCTION(IST_FXRSTOR);
    "fxrstorb", INSTRUCTION(IST_FXRSTORB);
    "fxrstorw", INSTRUCTION(IST_FXRSTORW);
    "fxrstorq", INSTRUCTION(IST_FXRSTORQ);
    "fxsave", INSTRUCTION(IST_FXSAVE);
    "fxsaveb",  INSTRUCTION(IST_FXSAVEB);
    "fxsavew",  INSTRUCTION(IST_FXSAVEW);
    "fxsaveq",  INSTRUCTION(IST_FXSAVEQ);
    "ffreep", INSTRUCTION(IST_FFREEP);


    "movdqa", INSTRUCTION(IST_MOVDQA);
    "movdqu", INSTRUCTION(IST_MOVDQU);
    "movapd", INSTRUCTION(IST_MOVAPD);
    "movupd", INSTRUCTION(IST_MOVUPD);

    "psrldq", INSTRUCTION(IST_PSRLDQ);
    "pslldq", INSTRUCTION(IST_PSLLDQ);
    "pshufd", INSTRUCTION(IST_PSHUFD);
    "pshuflw", INSTRUCTION(IST_PSHUFLW);
    "shufpd",		INSTRUCTION(IST_SHUFPD);

    "pmullw", INSTRUCTION(IST_PMULLW);
    "pmulhw", INSTRUCTION(IST_PMULHW);
    "pmuludq",  INSTRUCTION(IST_PMULUDQ);
    "punpcklqdq", INSTRUCTION(IST_PUNPCKLQDQ);
    "punpckhqdq", INSTRUCTION(IST_PUNPCKHQDQ);
    "paddq",  INSTRUCTION(IST_PADDQ);
    "pcmpgtd",  INSTRUCTION(IST_PCMPGTD);
    "psubq",  INSTRUCTION(IST_PSUBQ);
    "movdq2q",  INSTRUCTION(IST_MOVDQ2Q);
  ]

}


let space = [' ' '\t']
let digit	= ['0'-'9']
let alphanum = ['0'-'9' 'a'-'z' 'A'-'Z']
let alpha = ['a'-'z' 'A'-'Z']
let hexchar	= ['0'-'9' 'a'-'f' 'A'-'F']


let symbolchar = [',' ':' '=' '[' ']' '(' ')' '+' '-' '*' '/' '?']

rule token = parse
 | space+
    { token lexbuf }
 | ("\r\n"|"\n"|"\r")
 {
   incr lineno;
   EOL
 }

 | eof { EOF }
 | ";" { eat_line_comment lexbuf }

 | "TITLE" [^ '\n' '\r']*
 {
   token lexbuf;
 }

 | symbolchar { symbol_of(Lexing.lexeme lexbuf) }

 | "0" ['X' 'x'] hexchar+
 {
   NUMBER(NumHexInt(delete_front (Lexing.lexeme lexbuf) 2))
 }
 | "0" hexchar+ ['H' 'h']
 {
   NUMBER(NumHexInt(string_cut (Lexing.lexeme lexbuf) 1 1))
 }
 | hexchar+ ['H' 'h']
 {
   NUMBER(NumHexInt(delete_back (Lexing.lexeme lexbuf) 1))
 }
 | "0" hexchar+ ['R' 'r']
 {
   NUMBER(NumHexReal(string_cut (Lexing.lexeme lexbuf) 1 1))
 }

 | digit+
 {
   NUMBER(NumInt(Int64.of_string(Lexing.lexeme lexbuf)))
 }

 | "'" [^ '\n' '\r' '\'']* "'"
 {
   STRING(string_cut(Lexing.lexeme lexbuf) 1 1)
 }

 | "\"" [^ '\n' '\r' '\'']* "\""
 {
   STRING(string_cut(Lexing.lexeme lexbuf) 1 1)
 }

 | "." alphanum+
 {
   let s = Lexing.lexeme lexbuf in
     try cpu_of(s) with _ -> IDENTIFIER(s)
 }

(*
 | alpha+
 {
   let s = Lexing.lexeme lexbuf in
     try keyword_of(s) with _ -> IDENTIFIER(s)
 }
 | ['a'-'z' 'A'-'Z' '@' '$' '_' '?']+ [^ '\n' '\r' ' ' '\t' ';']*
*)
 | ['a'-'z' 'A'-'Z' '@' '$' '_' '?']+ ['0'-'9' 'a'-'z' 'A'-'Z' '@' '$' '_' '?' '.' '<' '>']*
 {
   let s = Lexing.lexeme lexbuf in
     try keyword_of(s) with _ -> IDENTIFIER(s)
 }
 | _
 {
  failwith
    (Printf.sprintf "unknown token %s near characters %d-%d, line %d"
      (Lexing.lexeme lexbuf)
      (Lexing.lexeme_start lexbuf)
      (Lexing.lexeme_end lexbuf)
      (!lineno))
 }

and eat_line_comment = parse
 | ("\r\n"|"\n"|"\r")
 {
   incr lineno;
   token lexbuf;
 }
 | [^ '\n' '\r']*
 {
   COMMENT(Lexing.lexeme lexbuf)
 }

 | eof { EOF }


