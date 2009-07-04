

open Printf
open Lexer
open Parser
open Type
open Parserhelper

let cpu_type_s s = List.assoc s
  [
    CPU_186,  ".186";
    CPU_286,  ".286";
    CPU_286P, ".286P";
    CPU_287,  ".287";
    CPU_386,  ".386";
    CPU_386P, ".386P";
    CPU_387,  ".387";
    CPU_486,  ".486";
    CPU_486P, ".486P";
    CPU_586,  ".586";
    CPU_586P, ".586P";
    CPU_686,  ".686";
    CPU_686P, ".686P";
    CPU_K3D,  ".K3D";
    CPU_MMX,  ".MMX";
    CPU_XMM,  ".XMM";
    CPU_8080, ".8080";
    CPU_8087, ".8087";
    CPU_NO87, ".NO87";
  ]

let data_store_s s = List.assoc s
  [
    DataFlat,  "flat";
    DataTiny,  "tiny";
    DataSmall,  "small";
    DataCompact,  "compact";
    DataMedium,  "medium";
    DataLarge,  "large";
    DataHuge,  "huge";
  ]

let rec data_store_list_to_string dlist =
  match dlist with
    [h] -> sprintf "%s" (data_store_s(h))
  | h :: t -> sprintf "%s, %s" (data_store_s(h)) (data_store_list_to_string t)
  | [] -> ""

let data_type_s s = List.assoc s
  [
    DataBYTE,     "BYTE";
    DataSBYTE,    "SBYTE";
    DataWORD,     "WORD";
    DataSWORD,    "SWORD";
    DataDWORD,    "DWORD";
    DataSDWORD,   "SDWORD";
    DataFWORD,    "FWORD";
    DataQWORD,    "QWORD";
    DataTBYTE,    "TBYTE";
    DataREAL4,    "REAL4";
    DataREAL8,    "REAL8";
    DataREAL10,   "REAL10";
    DataMMWORD,   "MMWORD";
    DataXMMWORD,  "XMMWORD";
    DataProc,     "PROC";
  ]

let binop_to_string t =
  match t with
    BinPlus -> "+"
  | BinMinus -> "-"
  | BinMul -> "*"
  | BinDiv -> "/"

let number_to_string n =
  match n with
    NumInt i -> Int64.to_string i
  | NumHexInt s -> sprintf "0%sh" s
  | NumHexReal s -> sprintf "0%sRh" s

let register_s s = List.assoc s
  [
    REG_AX,  "ax";
    REG_BX,  "bx";
    REG_CX,  "cx";
    REG_DX,  "dx";
    REG_BP,  "bp";
    REG_SP,  "sp";
    REG_DI,  "di";
    REG_SI,  "si";
    REG_EAX,  "eax";
    REG_EBX,  "ebx";
    REG_ECX,  "ecx";
    REG_EDX,  "edx";
    REG_EBP,  "ebp";
    REG_ESP,  "esp";
    REG_EDI,  "edi";
    REG_ESI,  "esi";
    REG_CR0,  "cr0";
    REG_CR1,  "cr1";
    REG_CR2,  "cr2";
    REG_CR3,  "cr3";
    REG_DR0,  "dr0";
    REG_DR1,  "dr1";
    REG_DR2,  "dr2";
    REG_DR3,  "dr3";
    REG_DR4,  "dr4";
    REG_DR5,  "dr5";
    REG_DR6,  "dr6";
    REG_DR7,  "dr7";
    REG_TR3,  "tr3";
    REG_TR4,  "tr4";
    REG_TR5,  "tr5";
    REG_TR6,  "tr6";
    REG_TR7,  "tr7";
    REG_AL,  "al";
    REG_AH,  "ah";
    REG_BL,  "bl";
    REG_BH,  "bh";
    REG_CL,  "cl";
    REG_CH,  "ch";
    REG_DL,  "dl";
    REG_DH,  "dh";
    REG_MM0,  "mm0";
    REG_MM1,  "mm1";
    REG_MM2,  "mm2";
    REG_MM3,  "mm3";
    REG_MM4,  "mm4";
    REG_MM5,  "mm5";
    REG_MM6,  "mm6";
    REG_MM7,  "mm7";
    REG_XMM0,  "xmm0";
    REG_XMM1,  "xmm1";
    REG_XMM2,  "xmm2";
    REG_XMM3,  "xmm3";
    REG_XMM4,  "xmm4";
    REG_XMM5,  "xmm5";
    REG_XMM6,  "xmm6";
    REG_XMM7,  "xmm7";
  ]

let segment_register_s s = List.assoc s
  [
    REG_CS,  "cs";
    REG_DS,  "ds";
    REG_ES,  "es";
    REG_FS,  "fs";
    REG_GS,  "gs";
    REG_SS,  "ss";
  ]

let rec expr_to_string e = 
  match e with
    Expr(bin,left,right) -> sprintf "%s %s %s" (expr_to_string left) (binop_to_string bin) (expr_to_string right)
  | ExprWithPrefix(e1, e2) -> sprintf "%s %s" (expr_to_string e1) (expr_to_string e2)
  | TermShort -> "SHORT"
  | TermNumber n -> number_to_string n
  | TermIdentifier s -> s
  | TermRegister r -> register_s(r)
  | TermSegmentRegister r -> segment_register_s(r)

let rec data_list_to_string dl =
  let data_to_string d =
    match d with
      DataNumber n -> sprintf "%s" (number_to_string n)
    | DataString s -> sprintf "%s" s
    | DataIdentifier s -> sprintf "%s" s
    | DataDUPUndefined -> sprintf "DUP()"
    | DataDUP i -> sprintf "DUP(%s)" (Int64.to_string i)
    | DataExpression(d,e) -> sprintf "%s:%s" (data_store_s(d)) (expr_to_string e)
  in match dl with
      [hd] -> sprintf "%s" (data_to_string hd)
    | hd :: bk -> sprintf "%s, %s" (data_to_string hd) (data_list_to_string bk)
    | [] -> ""

let rec common_list_to_string comms =
  let common_to_string com =
    if Int64.compare com.count (Int64.of_int 0) == 0 then
      sprintf "%s:%s" com.name (data_type_s(com.dtype))
    else
      sprintf "%s:%s[%s]" com.name (data_type_s(com.dtype)) (Int64.to_string com.count)
  in match comms with
       [hd] -> sprintf "%s" (common_to_string hd)
     | hd :: bk -> sprintf "%s, %s" (common_to_string hd) (common_list_to_string bk)
     | [] -> ""

let data_distance_s s = List.assoc s
  [
    DistanceNear,   "NEAR";
    DistanceFar,    "FAR";
    DistanceNear16, "NEAR16";
    DistanceNear32, "NEAR32";
    DistanceFar16,  "FAR16";
    DistanceFar32,  "FAR32";
  ]


let rec operand_list_to_string olist =
  let rec operand_to_string op =
    match op with
      OperandExpression e -> sprintf "%s" (expr_to_string e)
    | OperandOffset -> "OFFSET"
    | OperandPtr -> "PTR"
    | OperandColon -> ":"
    | OperandBr e -> sprintf "[%s]" (operand_to_string e)
    | OperandType t -> data_type_s(t)
    | OperandDistance t -> data_distance_s(t)
    | OperandST e -> sprintf "ST(%s)" (expr_to_string e)
  in let rec operand_element_to_string op =
    match op with
      hd :: bk -> sprintf "%s %s" (operand_to_string hd) (operand_element_to_string bk)
    | [] -> ""
  in match olist with
      [hd] -> sprintf "%s" (operand_element_to_string hd)
    | hd :: bk -> sprintf "%s, %s" (operand_element_to_string hd) (operand_list_to_string bk)
    | [] -> ""


let instruction_s s = List.assoc s
  [
    IST_JA,  "ja";
    IST_JB,  "jb";
    IST_JC,  "jc";
    IST_JE,  "je";
    IST_JG,  "jg";
    IST_JL,  "jl";
    IST_JO,  "jo";
    IST_JP,  "jp";
    IST_JS,  "js";
    IST_JZ,  "jz";
    IST_JAE,  "jae";
    IST_JBE,  "jbe";
    IST_JGE,  "jge";
    IST_JLE,  "jle";
    IST_JNA,  "jna";
    IST_JNB,  "jnb";
    IST_JNC,  "jnc";
    IST_JNE,  "jne";
    IST_JNG,  "jng";
    IST_JNL,  "jnl";
    IST_JNO,  "jno";
    IST_JNP,  "jnp";
    IST_JNS,  "jns";
    IST_JNZ,  "jnz";
    IST_JPE,  "jpe";
    IST_JPO,  "jpo";
    IST_JMP,  "jmp";
    IST_MOV,  "mov";
    IST_MOVD,  "movd";
    IST_MOVQ,  "movq";
    IST_MOVSB,  "movsb";
    IST_MOVSW,  "movsw";
    IST_PUSH,  "push";
    IST_POP,  "pop";
    IST_POPF,  "popf";
    IST_POPA,  "popa";
    IST_POPAD,  "popad";
    IST_POPFD,  "popfd";
    IST_PUSHF,  "pushf";
    IST_PUSHA,  "pusha";
    IST_PUSHAD,  "pushad";
    IST_PUSHFD,  "pushfd";
    IST_IN,  "in";
    IST_OR,  "or";
    IST_BT,  "bt";
    IST_AAA,  "aaa";
    IST_AAD,  "aad";
    IST_AAM,  "aam";
    IST_AAS,  "aas";
    IST_ADC,  "adc";
    IST_ADD,  "add";
    IST_AND,  "and";
    IST_CBW,  "cbw";
    IST_CLC,  "clc";
    IST_CLD,  "cld";
    IST_CLI,  "cli";
    IST_CMC,  "cmc";
    IST_CMP,  "cmp";
    IST_CWD,  "cwd";
    IST_DAA,  "daa";
    IST_DAS,  "das";
    IST_DEC,  "dec";
    IST_DIV,  "div";
    IST_ESC,  "esc";
    IST_HLT,  "hlt";
    IST_INC,  "inc";
    IST_INT,  "int";
    IST_LDS,  "lds";
    IST_LEA,  "lea";
    IST_LES,  "les";
    IST_MUL,  "mul";
    IST_NEG,  "neg";
    IST_NOP,  "nop";
    IST_NOT,  "not";
    IST_OUT,  "out";
    IST_RCL,  "rcl";
    IST_RCR,  "rcr";
    IST_RET,  "ret";
    IST_ROL,  "rol";
    IST_ROR,  "ror";
    IST_SAL,  "sal";
    IST_SAR,  "sar";
    IST_SBB,  "sbb";
    IST_SHL,  "shl";
    IST_SHR,  "shr";
    IST_STC,  "stc";
    IST_STD,  "std";
    IST_STI,  "sti";
    IST_SUB,  "sub";
    IST_XOR,  "xor";
    IST_INS,  "ins";
    IST_LAR,  "lar";
    IST_LSL,  "lsl";
    IST_LTR,  "ltr";
    IST_STR,  "str";
    IST_BSF,  "bsf";
    IST_BSR,  "bsr";
    IST_BTC,  "btc";
    IST_BTR,  "btr";
    IST_BTS,  "bts";
    IST_CDQ,  "cdq";
    IST_LFS,  "lfs";
    IST_LGS,  "lgs";
    IST_LSS,  "lss";
    IST_UD2,  "ud2";
    IST_POR,  "por";
    IST_CALL,  "call";
    IST_IDIV,  "idiv";
    IST_IMUL,  "imul";
    IST_INTO,  "into";
    IST_IRET,  "iret";
    IST_JCXZ,  "jcxz";
    IST_JNAE,  "jnae";
    IST_JNBE,  "jnbe";
    IST_JNGE,  "jnge";
    IST_JNLE,  "jnle";
    IST_LAHF,  "lahf";
    IST_LOOP,  "loop";
    IST_RETN,  "retn";
    IST_RETF,  "retf";
    IST_SAHF,  "sahf";
    IST_TEST,  "test";
    IST_WAIT,  "wait";
    IST_XCHG,  "xchg";
    IST_XLAT,  "xlat";
    IST_OUTS,  "outs";
    IST_ARPL,  "arpl";
    IST_CLTS,  "clts";
    IST_LGDT,  "lgdt";
    IST_LIDT,  "lidt";
    IST_LLDT,  "lldt";
    IST_LMSW,  "lmsw";
    IST_SGDT,  "sgdt";
    IST_SIDT,  "sidt";
    IST_SLDT,  "sldt";
    IST_SMSW,  "smsw";
    IST_VERR,  "verr";
    IST_VERW,  "verw";
    IST_CWDE,  "cwde";
    IST_INSB,  "insb";
    IST_INSW,  "insw";
    IST_INSD,  "insd";
    IST_INSQ,  "insq";
    IST_SETA,  "seta";
    IST_SETB,  "setb";
    IST_SETC,  "setc";
    IST_SETE,  "sete";
    IST_SETG,  "setg";
    IST_SETL,  "setl";
    IST_SETO,  "seto";
    IST_SETP,  "setp";
    IST_SETS,  "sets";
    IST_SETZ,  "setz";
    IST_SHLD,  "shld";
    IST_SHRD,  "shrd";
    IST_INVD,  "invd";
    IST_XADD,  "xadd";
    IST_PAND,  "pand";
    IST_PXOR,  "pxor";
    IST_EMMS,  "emms";
    IST_ORPS,  "orps";
    IST_CLGI,  "clgi";
    IST_STGI,  "stgi";
    IST_DPPS,  "dpps";
    IST_DPPD,  "dppd";
    IST_CMPSB,  "cmpsb";
    IST_CMPSW,  "cmpsw";
    IST_JECXZ,  "jecxz";
    IST_LODSB,  "lodsb";
    IST_LODSW,  "lodsw";
    IST_LODSD,  "lodsd";
    IST_LOOPX,  "loopx";
    IST_SCASB,  "scasb";
    IST_SCASW,  "scasw";
    IST_STOSB,  "stosb";
    IST_STOSW,  "stosw";
    IST_BOUND,  "bound";
    IST_ENTER,  "enter";
    IST_LEAVE,  "leave";
    IST_CMPSD,  "cmpsd";
    IST_IRETB,  "iretb";
    IST_IRETW,  "iretw";
    IST_IRETD,  "iretd";
    IST_IRETQ,  "iretq";
    IST_LOOPE,  "loope";
    IST_LOOPZ,  "loopz";
    IST_MOVSX,  "movsx";
    IST_MOVSD,  "movsd";
    IST_MOVZX,  "movzx";
    IST_SCASD,  "scasd";
    IST_SETAE,  "setae";
    IST_SETBE,  "setbe";
    IST_SETGE,  "setge";
    IST_SETLE,  "setle";
    IST_SETNA,  "setna";
    IST_SETNB,  "setnb";
    IST_SETNC,  "setnc";
    IST_SETNE,  "setne";
    IST_SETNG,  "setng";
    IST_SETNL,  "setnl";
    IST_SETNO,  "setno";
    IST_SETNP,  "setnp";
    IST_SETNS,  "setns";
    IST_SETNZ,  "setnz";
    IST_SETPE,  "setpe";
    IST_SETPO,  "setpo";
    IST_STOSD,  "stosd";
    IST_STOSQ,  "stosq";
    IST_BSWAP,  "bswap";
    IST_CPUID,  "cpuid";
    IST_RDMSR,  "rdmsr";
    IST_RDTSC,  "rdtsc";
    IST_WRMSR,  "wrmsr";
    IST_CMOVA,  "cmova";
    IST_CMOVB,  "cmovb";
    IST_CMOVC,  "cmovc";
    IST_CMOVE,  "cmove";
    IST_CMOVG,  "cmovg";
    IST_CMOVL,  "cmovl";
    IST_CMOVO,  "cmovo";
    IST_CMOVP,  "cmovp";
    IST_CMOVS,  "cmovs";
    IST_CMOVZ,  "cmovz";
    IST_RDPMC,  "rdpmc";
    IST_PADDB,  "paddb";
    IST_PADDW,  "paddw";
    IST_PADDD,  "paddd";
    IST_PSUBB,  "psubb";
    IST_PSUBW,  "psubw";
    IST_PSUBD,  "psubd";
    IST_PMULL,  "pmull";
    IST_PMULH,  "pmulh";
    IST_PMADD,  "pmadd";
    IST_PANDN,  "pandn";
    IST_PSLLW,  "psllw";
    IST_PSLLD,  "pslld";
    IST_PSRLW,  "psrlw";
    IST_PSRLD,  "psrld";
    IST_PSRAW,  "psraw";
    IST_PSRAD,  "psrad";
    IST_PSLLQ,  "psllq";
    IST_PSRLQ,  "psrlq";
    IST_MOVSS,  "movss";
    IST_ADDSS,  "addss";
    IST_SUBSS,  "subss";
    IST_MULSS,  "mulss";
    IST_DIVSS,  "divss";
    IST_RCPSS,  "rcpss";
    IST_MAXSS,  "maxss";
    IST_MINSS,  "minss";
    IST_ADDPS,  "addps";
    IST_SUBPS,  "subps";
    IST_MULPS,  "mulps";
    IST_DIVPS,  "divps";
    IST_RCPPS,  "rcpps";
    IST_MAXPS,  "maxps";
    IST_MINPS,  "minps";
    IST_CMPSS,  "cmpss";
    IST_CMPPS,  "cmpps";
    IST_ANDPS,  "andps";
    IST_XORPS,  "xorps";
    IST_PAVGB,  "pavgb";
    IST_PAVGW,  "pavgw";
    IST_PAUSE,  "pause";
    IST_LDDQU,  "lddqu";
    IST_MWAIT,  "mwait";
    IST_PABSB,  "pabsb";
    IST_PABSW,  "pabsw";
    IST_PABSD,  "pabsd";
    IST_VMXON,  "vmxon";
    IST_VMRUN,  "vmrun";
    IST_PTEST,  "ptest";
    IST_CRC32,  "crc32";
    IST_LZCNT,  "lzcnt";
    IST_LOOPNE,  "loopne";
    IST_LOOPNZ,  "loopnz";
    IST_SETNAE,  "setnae";
    IST_SETNBE,  "setnbe";
    IST_SETNGE,  "setnge";
    IST_SETNLE,  "setnle";
    IST_INVLPG,  "invlpg";
    IST_WBINVD,  "wbinvd";
    IST_CMOVAE,  "cmovae";
    IST_CMOVBE,  "cmovbe";
    IST_CMOVGE,  "cmovge";
    IST_CMOVLE,  "cmovle";
    IST_CMOVNA,  "cmovna";
    IST_CMOVNB,  "cmovnb";
    IST_CMOVNC,  "cmovnc";
    IST_CMOVNE,  "cmovne";
    IST_CMOVNG,  "cmovng";
    IST_CMOVNL,  "cmovnl";
    IST_CMOVNO,  "cmovno";
    IST_CMOVNP,  "cmovnp";
    IST_CMOVNS,  "cmovns";
    IST_CMOVNZ,  "cmovnz";
    IST_CMOVPE,  "cmovpe";
    IST_CMOVPO,  "cmovpo";
    IST_SYSRET,  "sysret";
    IST_PADDSB,  "paddsb";
    IST_PADDSW,  "paddsw";
    IST_PSUBSB,  "psubsb";
    IST_PSUBSW,  "psubsw";
    IST_MOVAPS,  "movaps";
    IST_MOVUPS,  "movups";
    IST_MOVLPS,  "movlps";
    IST_MOVHPS,  "movhps";
    IST_SQRTSS,  "sqrtss";
    IST_SQRTPS,  "sqrtps";
    IST_COMISS,  "comiss";
    IST_SHUFPS,  "shufps";
    IST_ANDNPS,  "andnps";
    IST_PSADBW,  "psadbw";
    IST_PMAXUB,  "pmaxub";
    IST_PMINUB,  "pminub";
    IST_PMAXSW,  "pmaxsw";
    IST_PMINSW,  "pminsw";
    IST_PEXTRW,  "pextrw";
    IST_PINSRW,  "pinsrw";
    IST_PSHUFW,  "pshufw";
    IST_MOVNTQ,  "movntq";
    IST_SFENCE,  "sfence";
    IST_LFENCE,  "lfence";
    IST_MFENCE,  "mfence";
    IST_MOVNTI,  "movnti";
    IST_HADDPD,  "haddpd";
    IST_HADDPS,  "haddps";
    IST_HSUBPD,  "hsubpd";
    IST_HSUBPS,  "hsubps";
    IST_PSIGNB,  "psignb";
    IST_PSIGNW,  "psignw";
    IST_PSIGND,  "psignd";
    IST_PSHUFB,  "pshufb";
    IST_PHSUBW,  "phsubw";
    IST_PHSUBD,  "phsubd";
    IST_PHADDW,  "phaddw";
    IST_PHADDD,  "phaddd";
    IST_VMREAD,  "vmread";
    IST_VMCALL,  "vmcall";
    IST_VMXOFF,  "vmxoff";
    IST_SKINIT,  "skinit";
    IST_VMLOAD,  "vmload";
    IST_VMSAVE,  "vmsave";
    IST_RDTSCP,  "rdtscp";
    IST_PMULDQ,  "pmuldq";
    IST_PMULLD,  "pmulld";
    IST_PMINSB,  "pminsb";
    IST_PMAXSB,  "pmaxsb";
    IST_PMINUW,  "pminuw";
    IST_PMAXUW,  "pmaxuw";
    IST_PMINUD,  "pminud";
    IST_PMAXUD,  "pmaxud";
    IST_PMINSD,  "pminsd";
    IST_PMAXSD,  "pmaxsd";
    IST_PINSRB,  "pinsrb";
    IST_PINSRD,  "pinsrd";
    IST_PINSRQ,  "pinsrq";
    IST_PEXTRB,  "pextrb";
    IST_PEXTRD,  "pextrd";
    IST_PEXTRQ,  "pextrq";
    IST_POPCNT,  "popcnt";
    IST_LOADALL,  "loadall";
    IST_CMPXCHG,  "cmpxchg";
    IST_CMOVNAE,  "cmovnae";
    IST_CMOVNBE,  "cmovnbe";
    IST_CMOVNGE,  "cmovnge";
    IST_CMOVNLE,  "cmovnle";
    IST_SYSEXIT,  "sysexit";
    IST_SYSCALL,  "syscall";
    IST_PADDUSB,  "paddusb";
    IST_PADDUSW,  "paddusw";
    IST_PSUBUSB,  "psubusb";
    IST_PSUBUSW,  "psubusw";
    IST_PCMPEQB,  "pcmpeqb";
    IST_PCMPEQW,  "pcmpeqw";
    IST_PCMPEQD,  "pcmpeqd";
    IST_MOVLHPS,  "movlhps";
    IST_MOVHLPS,  "movhlps";
    IST_RSQRTSS,  "rsqrtss";
    IST_RSQRTPS,  "rsqrtps";
    IST_UCOMISS,  "ucomiss";
    IST_PMULHUW,  "pmulhuw";
    IST_LDMXCSR,  "ldmxcsr";
    IST_STMXCSR,  "stmxcsr";
    IST_MOVNTPS,  "movntps";
    IST_CLFLUSH,  "clflush";
    IST_MOVNTDQ,  "movntdq";
    IST_MOVNTPD,  "movntpd";
    IST_MOVDDUP,  "movddup";
    IST_FISFTTP,  "fisfttp";
    IST_MONITOR,  "monitor";
    IST_PALIGNR,  "palignr";
    IST_PHADDSW,  "phaddsw";
    IST_VMPTRLD,  "vmptrld";
    IST_VMPTRST,  "vmptrst";
    IST_VMCLEAR,  "vmclear";
    IST_VMWRITE,  "vmwrite";
    IST_VMMCALL,  "vmmcall";
    IST_MPSADBW,  "mpsadbw";
    IST_BLENDPS,  "blendps";
    IST_BLENDPD,  "blendpd";
    IST_PBLENDW,  "pblendw";
    IST_ROUNDPS,  "roundps";
    IST_ROUNDSS,  "roundss";
    IST_ROUNDPD,  "roundpd";
    IST_ROUNDSD,  "roundsd";
    IST_PCMPEQQ,  "pcmpeqq";
    IST_PCMPGTQ,  "pcmpgtq";
    IST_SYSENTER,  "sysenter";
    IST_PCMPGTPB,  "pcmpgtpb";
    IST_PCMPGTPW,  "pcmpgtpw";
    IST_PCMPGTPD,  "pcmpgtpd";
    IST_PACKSSWB,  "packsswb";
    IST_PACKSSDW,  "packssdw";
    IST_PACKUSWB,  "packuswb";
    IST_UNPCKHPS,  "unpckhps";
    IST_UNPCKLPS,  "unpcklps";
    IST_CVTSI2SS,  "cvtsi2ss";
    IST_CVTSS2SI,  "cvtss2si";
    IST_CVTPI2PS,  "cvtpi2ps";
    IST_CVTPS2PI,  "cvtps2pi";
    IST_PMOVMSKB,  "pmovmskb";
    IST_MASKMOVQ,  "maskmovq";
    IST_ADDSUBPD,  "addsubpd";
    IST_ADDSUBPS,  "addsubps";
    IST_MOVSHDUP,  "movshdup";
    IST_MOVSLDUP,  "movsldup";
    IST_PMULHRSW,  "pmulhrsw";
    IST_VMLAUNCH,  "vmlaunch";
    IST_VMRESUME,  "vmresume";
    IST_BLENDVPS,  "blendvps";
    IST_BLENDVPD,  "blendvpd";
    IST_PBLENDVB,  "pblendvb";
    IST_INSERTPS,  "insertps";
    IST_PMOVSXBW,  "pmovsxbw";
    IST_PMOVZXBW,  "pmovzxbw";
    IST_PMOVSXBD,  "pmovsxbd";
    IST_PMOVZXBD,  "pmovzxbd";
    IST_PMOVSXBQ,  "pmovsxbq";
    IST_PMOVZXBQ,  "pmovzxbq";
    IST_PMOVSXWD,  "pmovsxwd";
    IST_PMOVZXWD,  "pmovzxwd";
    IST_PMOVSXWQ,  "pmovsxwq";
    IST_PMOVZXWQ,  "pmovzxwq";
    IST_PMOVSXDQ,  "pmovsxdq";
    IST_PMOVZXDQ,  "pmovzxdq";
    IST_PACKUSDW,  "packusdw";
    IST_MOVNTDQA,  "movntdqa";
    IST_CMPXCHG8B,  "cmpxchg8b";
    IST_PUNPCKHBW,  "punpckhbw";
    IST_PUNPCKHWD,  "punpckhwd";
    IST_PUNPCKHDQ,  "punpckhdq";
    IST_PUNPCKLBW,  "punpcklbw";
    IST_PUNPCKLWD,  "punpcklwd";
    IST_PUNPCKLDQ,  "punpckldq";
    IST_CVTTSS2SI,  "cvttss2si";
    IST_CVTTPS2PI,  "cvttps2pi";
    IST_PREFETCH0,  "prefetch0";
    IST_PREFETCH1,  "prefetch1";
    IST_PREFETCH2,  "prefetch2";
    IST_PMADDUBSW,  "pmaddubsw";
    IST_EXTRACTPS,  "extractps";
    IST_PCMPESTRI,  "pcmpestri";
    IST_PCMPESTRM,  "pcmpestrm";
    IST_PCMPISTRI,  "pcmpistri";
    IST_PCMPISTRM,  "pcmpistrm";
    IST_MASKMOVDQU,  "maskmovdqu";
    IST_CMPXCHG16B,  "cmpxchg16b";
    IST_PHMINPOSUW,  "phminposuw";
    IST_PREFETCHNTA,  "prefetchnta";
    IST_F2XM1,  "f2xm1";
    IST_FABS,  "fabs";
    IST_FADD,  "fadd";
    IST_FADDP,  "faddp";
    IST_FBLD,  "fbld";
    IST_FBSTP,  "fbstp";
    IST_FCHS,  "fchs";
    IST_FCLEX,  "fclex";
    IST_FCOM,  "fcom";
    IST_FCOMP,  "fcomp";
    IST_FCOMPP,  "fcompp";
    IST_FDECSTP,  "fdecstp";
    IST_FDISI,  "fdisi";
    IST_FDIV,  "fdiv";
    IST_FDIVP,  "fdivp";
    IST_FDIVR,  "fdivr";
    IST_FDIVRP,  "fdivrp";
    IST_FENI,  "feni";
    IST_FFREE,  "ffree";
    IST_FIADD,  "fiadd";
    IST_FICOM,  "ficom";
    IST_FICOMP,  "ficomp";
    IST_FIDIV,  "fidiv";
    IST_FIDIVR,  "fidivr";
    IST_FILD,  "fild";
    IST_FIMUL,  "fimul";
    IST_FINCSTP,  "fincstp";
    IST_FINIT,  "finit";
    IST_FIST,  "fist";
    IST_FISTP,  "fistp";
    IST_FISUB,  "fisub";
    IST_FISUBR,  "fisubr";
    IST_FLD,  "fld";
    IST_FLD1,  "fld1";
    IST_FLDCW,  "fldcw";
    IST_FLDENV,  "fldenv";
    IST_FLDENVW,  "fldenvw";
    IST_FLDL2E,  "fldl2e";
    IST_FLDL2T,  "fldl2t";
    IST_FLDLG2,  "fldlg2";
    IST_FLDLN2,  "fldln2";
    IST_FLDPI,  "fldpi";
    IST_FLDZ,  "fldz";
    IST_FMUL,  "fmul";
    IST_FMULP,  "fmulp";
    IST_FNCLEX,  "fnclex";
    IST_FNDISI,  "fndisi";
    IST_FNENI,  "fneni";
    IST_FNINIT,  "fninit";
    IST_FNOP,  "fnop";
    IST_FNSAVE,  "fnsave";
    IST_FNSAVEW,  "fnsavew";
    IST_FNSTCW,  "fnstcw";
    IST_FNSTENV,  "fnstenv";
    IST_FNSTENVW,  "fnstenvw";
    IST_FNSTSW,  "fnstsw";
    IST_FPATAN,  "fpatan";
    IST_FPREM,  "fprem";
    IST_FPTAN,  "fptan";
    IST_FRNDINT,  "frndint";
    IST_FRSTOR,  "frstor";
    IST_FRSTORW,  "frstorw";
    IST_FSAVE,  "fsave";
    IST_FSAVEW,  "fsavew";
    IST_FSCALE,  "fscale";
    IST_FSQRT,  "fsqrt";
    IST_FST,  "fst";
    IST_FSTCW,  "fstcw";
    IST_FSTENV,  "fstenv";
    IST_FSTENVW,  "fstenvw";
    IST_FSTP,  "fstp";
    IST_FSTSW,  "fstsw";
    IST_FSUB,  "fsub";
    IST_FSUBP,  "fsubp";
    IST_FSUBR,  "fsubr";
    IST_FSUBRP,  "fsubrp";
    IST_FTST,  "ftst";
    IST_FWAIT,  "fwait";
    IST_FXAM,  "fxam";
    IST_FXCH,  "fxch";
    IST_FXTRACT,  "fxtract";
    IST_FYL2X,  "fyl2x";
    IST_FYL2XP1,  "fyl2xp1";
    IST_FSETPM,  "fsetpm";
    IST_FCOS,  "fcos";
    IST_FLDENVD,  "fldenvd";
    IST_FNSAVED,  "fnsaved";
    IST_FNSTENVD,  "fnstenvd";
    IST_FPREM1,  "fprem1";
    IST_FRSTORD,  "frstord";
    IST_FSAVED,  "fsaved";
    IST_FSIN,  "fsin";
    IST_FSINCOS,  "fsincos";
    IST_FSTENVD,  "fstenvd";
    IST_FUCOM,  "fucom";
    IST_FUCOMP,  "fucomp";
    IST_FUCOMPP,  "fucompp";
    IST_FCMOVBE,  "fcmovbe";
    IST_FCMOVE,  "fcmove";
    IST_FCMOVNB,  "fcmovnb";
    IST_FCMOVNBE,  "fcmovnbe";
    IST_FCMOVNE,  "fcmovne";
    IST_FCMOVNU,  "fcmovnu";
    IST_FCMOVU,  "fcmovu";
    IST_FCOMIP,  "fcomip";
    IST_FUCOMI,  "fucomi";
    IST_FUCOMIP,  "fucomip";
    IST_FXRSTOR,  "fxrstor";
    IST_FXRSTORB,  "fxrstorb";
    IST_FXRSTORW,  "fxrstorw";
    IST_FXRSTORQ,  "fxrstorq";
    IST_FXSAVE,  "fxsave";
    IST_FXSAVEB,  "fxsaveb";
    IST_FXSAVEW,  "fxsavew";
    IST_FXSAVEQ,  "fxsaveq";
    IST_FFREEP,  "ffreep";
    IST_MOVDQA,  "movdqa";
    IST_MOVDQU,  "movdqu";
    IST_MOVAPD,  "movapd";
    IST_MOVUPD,  "movupd";
    IST_PSRLDQ,  "psrldq";
    IST_PSLLDQ,  "pslldq";
    IST_PSHUFD,  "pshufd";
    IST_PSHUFLW, "pshuflw";
    IST_PUNPCKHQDQ,  "punpckhqdq";
    IST_PMULLW,  "pmullw";
    IST_PMULHW,  "pmulhw";
    IST_PMULUDQ,  "pmuludq";
    IST_PADDQ,  "paddq";
    IST_PCMPGTD,  "pcmpgtd";
    IST_PSUBQ,  "psubq";
    IST_MOVDQ2Q,  "movdq2q";
    IST_SHUFPD,  "shufpd";
  ]


let instruction_prefix_s s = List.assoc s
  [
    IST_REP,  "rep";
    IST_REPE,  "repe";
    IST_REPZ,  "repz";
    IST_REPNE,  "repne";
    IST_REPNZ,  "repnz";
    IST_LOCK,  "lock";
  ]

let ir_to_string code =
  match code with
    EndAsm -> "END"
  | Cpu t -> sprintf "\t%s" (cpu_type_s(t))
  | Include t -> sprintf "\tINCLUDE %s" t
  | IncludeLib t -> sprintf "\tINCLUDELIB %s" t
  | Model t -> sprintf "\t.model %s" (data_store_list_to_string t)
  | Public t -> sprintf "PUBLIC %s" t
  | ProcedureStart t -> sprintf "PROC %s" t
  | ProcedureEnd t -> sprintf "ENDP %s" t
  | SegmentStart t -> sprintf "SEGMENT %s" t
  | SegmentEnd t -> sprintf "ENDS %s" t
  | Extern(s,t) -> sprintf "EXTRN %s:%s" s (data_type_s(t))
  | Align t -> sprintf "ALIGN %s" (number_to_string t)
  | Label t -> sprintf "%s:" t
  | Npad t -> sprintf "NPAD %s" (number_to_string t)
  | Org t -> sprintf "ORG %s" (expr_to_string t)
  | Data(t, d_list) -> sprintf "\t%s %s" (data_type_s(t)) (data_list_to_string d_list)
  | DataWithName(n, t, d_list) -> sprintf "%s %s %s" n (data_type_s(t)) (data_list_to_string d_list)
  | Let(n, i) -> sprintf "%s = %s" n (Int64.to_string i)
  | Common cl -> sprintf "COMM %s" (common_list_to_string cl)
  | Instruction(i, op) -> sprintf "\t%s %s" (instruction_s(i)) (operand_list_to_string op)
  | InstructionWithPrefix(ip, i, op) -> sprintf "\t%s %s %s" (instruction_prefix_s(ip)) (instruction_s(i)) (operand_list_to_string op)
  | Comment s -> ""



let rec print_parser masmlist  =
  let print masmlist = 
    let str_ir = ir_to_string masmlist in
    if String.compare str_ir "" != 0 then Printf.printf "%s\n" str_ir else () in
  match masmlist with
    t :: a ->  print t; print_parser a
  | [] -> printf "";;

let main =
    (* let lexbuf = Lexing.from_channel stdin in *)
  Printf.printf "file: %s\n" Sys.argv.(1); flush stdout;
    let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
    let result = Parser.main Lexer.token lexbuf in
    print_parser result


