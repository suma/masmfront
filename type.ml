

type masm =
    EndAsm
  | Cpu of cpu_type
  | Include of string
  | IncludeLib of string
  | Model of data_store_type list
  | Public of string
  | ProcedureStart of string
  | ProcedureEnd of string
  | SegmentStart of string
  | SegmentEnd of string
  | Extern of string * data_type
  | Align of number
  | Label of string
  | Npad of number
  | Org of expr
  | Data of data_type * data list
  | DataWithName of string * data_type * data list
  | Let of string * int64
  | Common of common_definition list

  | Instruction of instruction * operand list list
  | InstructionWithPrefix of instruction_prefix * instruction * operand list list

  | Comment of string

and program =
  masm list


and operand =
    OperandExpression of expr
  | OperandOffset
  | OperandPtr
  | OperandColon
  | OperandBr of operand
  | OperandType of data_type
  | OperandDistance of data_distance
  | OperandST of expr

and data_distance =
    DistanceNear
  | DistanceFar
  | DistanceNear16
  | DistanceNear32
  | DistanceFar16
  | DistanceFar32

and data =
    DataNumber of number
  | DataString of string
  | DataIdentifier of string
  | DataDUPUndefined
  | DataDUP of int64
  | DataExpression of data_store_type * expr

and data_store_type =
    DataFlat
  | DataTiny
  | DataSmall
  | DataCompact
  | DataMedium
  | DataLarge
  | DataHuge

and expr =
    Expr of binop * expr * expr
  | ExprWithPrefix of expr * expr
  | TermShort
  | TermNumber of number
  | TermIdentifier of string
  | TermRegister of register
  | TermSegmentRegister of segment_register

and binop =
    BinPlus
  | BinMinus
  | BinMul
  | BinDiv

and data_type =
    DataBYTE
  | DataSBYTE
  | DataWORD
  | DataSWORD
  | DataDWORD
  | DataSDWORD
  | DataFWORD
  | DataQWORD
  | DataTBYTE
  | DataREAL4
  | DataREAL8
  | DataREAL10
  | DataMMWORD
  | DataXMMWORD
  | DataProc

and common_definition = {
  name : string;
  dtype : data_type;
  count : int64;
}

and number =
    NumInt of int64
  | NumHexInt of string
  | NumHexReal of string

and cpu_type =
    CPU_186
  | CPU_286
  | CPU_286P
  | CPU_287
  | CPU_386
  | CPU_386P
  | CPU_387
  | CPU_486
  | CPU_486P
  | CPU_586
  | CPU_586P
  | CPU_686
  | CPU_686P
  | CPU_K3D
  | CPU_MMX
  | CPU_XMM
  | CPU_8080
  | CPU_8087
  | CPU_NO87

and register =
    REG_AX
  | REG_BX
  | REG_CX
  | REG_DX
  | REG_BP
  | REG_SP
  | REG_DI
  | REG_SI
  | REG_EAX
  | REG_EBX
  | REG_ECX
  | REG_EDX
  | REG_EBP
  | REG_ESP
  | REG_EDI
  | REG_ESI
  | REG_CR0
  | REG_CR1
  | REG_CR2
  | REG_CR3
  | REG_DR0
  | REG_DR1
  | REG_DR2
  | REG_DR3
  | REG_DR4
  | REG_DR5
  | REG_DR6
  | REG_DR7
  | REG_TR3
  | REG_TR4
  | REG_TR5
  | REG_TR6
  | REG_TR7
  | REG_AL
  | REG_AH
  | REG_BL
  | REG_BH
  | REG_CL
  | REG_CH
  | REG_DL
  | REG_DH
  | REG_MM0
  | REG_MM1
  | REG_MM2
  | REG_MM3
  | REG_MM4
  | REG_MM5
  | REG_MM6
  | REG_MM7
  | REG_XMM0
  | REG_XMM1
  | REG_XMM2
  | REG_XMM3
  | REG_XMM4
  | REG_XMM5
  | REG_XMM6
  | REG_XMM7

and segment_register =
    REG_CS
  | REG_DS
  | REG_ES
  | REG_FS
  | REG_GS
  | REG_SS

and instruction_prefix =
    IST_REP
  | IST_REPE
  | IST_REPZ
  | IST_REPNE
  | IST_REPNZ
  | IST_LOCK

and instruction =
  (* Branch *)
    IST_JA
  | IST_JB
  | IST_JC
  | IST_JE
  | IST_JG
  | IST_JL
  | IST_JO
  | IST_JP
  | IST_JS
  | IST_JZ
  | IST_JAE
  | IST_JBE
  | IST_JGE
  | IST_JLE
  | IST_JNA
  | IST_JNB
  | IST_JNC
  | IST_JNE
  | IST_JNG
  | IST_JNL
  | IST_JNO
  | IST_JNP
  | IST_JNS
  | IST_JNZ
  | IST_JPE
  | IST_JPO
  | IST_JMP

  (* Data transfer *)
  | IST_MOV
  | IST_MOVD
  | IST_MOVQ
  | IST_MOVSB
  | IST_MOVSW

  (* Stack *)
  | IST_PUSH
  | IST_POP
  | IST_POPF
  | IST_POPA
  | IST_POPAD
  | IST_POPFD
  | IST_PUSHF
  | IST_PUSHA
  | IST_PUSHAD
  | IST_PUSHFD

  (* *)
  | IST_IN
  | IST_OR
  | IST_BT
  | IST_AAA
  | IST_AAD
  | IST_AAM
  | IST_AAS
  | IST_ADC
  | IST_ADD
  | IST_AND
  | IST_CBW
  | IST_CLC
  | IST_CLD
  | IST_CLI
  | IST_CMC
  | IST_CMP
  | IST_CWD
  | IST_DAA
  | IST_DAS
  | IST_DEC
  | IST_DIV
  | IST_ESC
  | IST_HLT
  | IST_INC
  | IST_INT
  | IST_LDS
  | IST_LEA
  | IST_LES
  | IST_MUL
  | IST_NEG
  | IST_NOP
  | IST_NOT
  | IST_OUT
  | IST_RCL
  | IST_RCR
  | IST_RET
  | IST_ROL
  | IST_ROR
  | IST_SAL
  | IST_SAR
  | IST_SBB
  | IST_SHL
  | IST_SHR
  | IST_STC
  | IST_STD
  | IST_STI
  | IST_SUB
  | IST_XOR
  | IST_INS
  | IST_LAR
  | IST_LSL
  | IST_LTR
  | IST_STR
  | IST_BSF
  | IST_BSR
  | IST_BTC
  | IST_BTR
  | IST_BTS
  | IST_CDQ
  | IST_LFS
  | IST_LGS
  | IST_LSS
  | IST_UD2
  | IST_POR
  | IST_CALL
  | IST_IDIV
  | IST_IMUL
  | IST_INTO
  | IST_IRET
  | IST_JCXZ
  | IST_JNAE
  | IST_JNBE
  | IST_JNGE
  | IST_JNLE
  | IST_LAHF
  | IST_LOOP
  | IST_RETN
  | IST_RETF
  | IST_SAHF
  | IST_TEST
  | IST_WAIT
  | IST_XCHG
  | IST_XLAT
  | IST_OUTS
  | IST_ARPL
  | IST_CLTS
  | IST_LGDT
  | IST_LIDT
  | IST_LLDT
  | IST_LMSW
  | IST_SGDT
  | IST_SIDT
  | IST_SLDT
  | IST_SMSW
  | IST_VERR
  | IST_VERW
  | IST_CWDE
  | IST_INSB
  | IST_INSW
  | IST_INSD
  | IST_INSQ
  | IST_SETA
  | IST_SETB
  | IST_SETC
  | IST_SETE
  | IST_SETG
  | IST_SETL
  | IST_SETO
  | IST_SETP
  | IST_SETS
  | IST_SETZ
  | IST_SHLD
  | IST_SHRD
  | IST_INVD
  | IST_XADD
  | IST_PAND
  | IST_PXOR
  | IST_EMMS
  | IST_ORPS
  | IST_CLGI
  | IST_STGI
  | IST_DPPS
  | IST_DPPD
  | IST_CMPSB
  | IST_CMPSW
  | IST_JECXZ
  | IST_LODSB
  | IST_LODSW
  | IST_LODSD
  | IST_LOOPX
  | IST_SCASB
  | IST_SCASW
  | IST_STOSB
  | IST_STOSW
  | IST_BOUND
  | IST_ENTER
  | IST_LEAVE
  | IST_CMPSD
  | IST_IRETB
  | IST_IRETW
  | IST_IRETD
  | IST_IRETQ
  | IST_LOOPE
  | IST_LOOPZ
  | IST_MOVSX
  | IST_MOVSD
  | IST_MOVZX
  | IST_SCASD
  | IST_SETAE
  | IST_SETBE
  | IST_SETGE
  | IST_SETLE
  | IST_SETNA
  | IST_SETNB
  | IST_SETNC
  | IST_SETNE
  | IST_SETNG
  | IST_SETNL
  | IST_SETNO
  | IST_SETNP
  | IST_SETNS
  | IST_SETNZ
  | IST_SETPE
  | IST_SETPO
  | IST_STOSD
  | IST_STOSQ
  | IST_BSWAP
  | IST_CPUID
  | IST_RDMSR
  | IST_RDTSC
  | IST_WRMSR
  | IST_CMOVA
  | IST_CMOVB
  | IST_CMOVC
  | IST_CMOVE
  | IST_CMOVG
  | IST_CMOVL
  | IST_CMOVO
  | IST_CMOVP
  | IST_CMOVS
  | IST_CMOVZ
  | IST_RDPMC
  | IST_PADDB
  | IST_PADDW
  | IST_PADDD
  | IST_PSUBB
  | IST_PSUBW
  | IST_PSUBD
  | IST_PMULL
  | IST_PMULH
  | IST_PMADD
  | IST_PANDN
  | IST_PSLLW
  | IST_PSLLD
  | IST_PSRLW
  | IST_PSRLD
  | IST_PSRAW
  | IST_PSRAD
  | IST_PSLLQ
  | IST_PSRLQ
  | IST_MOVSS
  | IST_ADDSS
  | IST_SUBSS
  | IST_MULSS
  | IST_DIVSS
  | IST_RCPSS
  | IST_MAXSS
  | IST_MINSS
  | IST_ADDPS
  | IST_SUBPS
  | IST_MULPS
  | IST_DIVPS
  | IST_RCPPS
  | IST_MAXPS
  | IST_MINPS
  | IST_CMPSS
  | IST_CMPPS
  | IST_ANDPS
  | IST_XORPS
  | IST_PAVGB
  | IST_PAVGW
  | IST_PAUSE
  | IST_LDDQU
  | IST_MWAIT
  | IST_PABSB
  | IST_PABSW
  | IST_PABSD
  | IST_VMXON
  | IST_VMRUN
  | IST_PTEST
  | IST_CRC32
  | IST_LZCNT
  | IST_LOOPNE
  | IST_LOOPNZ
  | IST_SETNAE
  | IST_SETNBE
  | IST_SETNGE
  | IST_SETNLE
  | IST_INVLPG
  | IST_WBINVD
  | IST_CMOVAE
  | IST_CMOVBE
  | IST_CMOVGE
  | IST_CMOVLE
  | IST_CMOVNA
  | IST_CMOVNB
  | IST_CMOVNC
  | IST_CMOVNE
  | IST_CMOVNG
  | IST_CMOVNL
  | IST_CMOVNO
  | IST_CMOVNP
  | IST_CMOVNS
  | IST_CMOVNZ
  | IST_CMOVPE
  | IST_CMOVPO
  | IST_SYSRET
  | IST_PADDSB
  | IST_PADDSW
  | IST_PSUBSB
  | IST_PSUBSW
  | IST_MOVAPS
  | IST_MOVUPS
  | IST_MOVLPS
  | IST_MOVHPS
  | IST_SQRTSS
  | IST_SQRTPS
  | IST_COMISS
  | IST_SHUFPS
  | IST_ANDNPS
  | IST_PSADBW
  | IST_PMAXUB
  | IST_PMINUB
  | IST_PMAXSW
  | IST_PMINSW
  | IST_PEXTRW
  | IST_PINSRW
  | IST_PSHUFW
  | IST_MOVNTQ
  | IST_SFENCE
  | IST_LFENCE
  | IST_MFENCE
  | IST_MOVNTI
  | IST_HADDPD
  | IST_HADDPS
  | IST_HSUBPD
  | IST_HSUBPS
  | IST_PSIGNB
  | IST_PSIGNW
  | IST_PSIGND
  | IST_PSHUFB
  | IST_PHSUBW
  | IST_PHSUBD
  | IST_PHADDW
  | IST_PHADDD
  | IST_VMREAD
  | IST_VMCALL
  | IST_VMXOFF
  | IST_SKINIT
  | IST_VMLOAD
  | IST_VMSAVE
  | IST_RDTSCP
  | IST_PMULDQ
  | IST_PMULLD
  | IST_PMINSB
  | IST_PMAXSB
  | IST_PMINUW
  | IST_PMAXUW
  | IST_PMINUD
  | IST_PMAXUD
  | IST_PMINSD
  | IST_PMAXSD
  | IST_PINSRB
  | IST_PINSRD
  | IST_PINSRQ
  | IST_PEXTRB
  | IST_PEXTRD
  | IST_PEXTRQ
  | IST_POPCNT
  | IST_LOADALL
  | IST_CMPXCHG
  | IST_CMOVNAE
  | IST_CMOVNBE
  | IST_CMOVNGE
  | IST_CMOVNLE
  | IST_SYSEXIT
  | IST_SYSCALL
  | IST_PADDUSB
  | IST_PADDUSW
  | IST_PSUBUSB
  | IST_PSUBUSW
  | IST_PCMPEQB
  | IST_PCMPEQW
  | IST_PCMPEQD
  | IST_MOVLHPS
  | IST_MOVHLPS
  | IST_RSQRTSS
  | IST_RSQRTPS
  | IST_UCOMISS
  | IST_PMULHUW
  | IST_LDMXCSR
  | IST_STMXCSR
  | IST_MOVNTPS
  | IST_CLFLUSH
  | IST_MOVNTDQ
  | IST_MOVNTPD
  | IST_MOVDDUP
  | IST_FISFTTP
  | IST_MONITOR
  | IST_PALIGNR
  | IST_PHADDSW
  | IST_VMPTRLD
  | IST_VMPTRST
  | IST_VMCLEAR
  | IST_VMWRITE
  | IST_VMMCALL
  | IST_MPSADBW
  | IST_BLENDPS
  | IST_BLENDPD
  | IST_PBLENDW
  | IST_ROUNDPS
  | IST_ROUNDSS
  | IST_ROUNDPD
  | IST_ROUNDSD
  | IST_PCMPEQQ
  | IST_PCMPGTQ
  | IST_SYSENTER
  | IST_PCMPGTPB
  | IST_PCMPGTPW
  | IST_PCMPGTPD
  | IST_PACKSSWB
  | IST_PACKSSDW
  | IST_PACKUSWB
  | IST_UNPCKHPS
  | IST_UNPCKLPS
  | IST_CVTSI2SS
  | IST_CVTSS2SI
  | IST_CVTPI2PS
  | IST_CVTPS2PI
  | IST_PMOVMSKB
  | IST_MASKMOVQ
  | IST_ADDSUBPD
  | IST_ADDSUBPS
  | IST_MOVSHDUP
  | IST_MOVSLDUP
  | IST_PMULHRSW
  | IST_VMLAUNCH
  | IST_VMRESUME
  | IST_BLENDVPS
  | IST_BLENDVPD
  | IST_PBLENDVB
  | IST_INSERTPS
  | IST_PMOVSXBW
  | IST_PMOVZXBW
  | IST_PMOVSXBD
  | IST_PMOVZXBD
  | IST_PMOVSXBQ
  | IST_PMOVZXBQ
  | IST_PMOVSXWD
  | IST_PMOVZXWD
  | IST_PMOVSXWQ
  | IST_PMOVZXWQ
  | IST_PMOVSXDQ
  | IST_PMOVZXDQ
  | IST_PACKUSDW
  | IST_MOVNTDQA
  | IST_CMPXCHG8B
  | IST_PUNPCKHBW
  | IST_PUNPCKHWD
  | IST_PUNPCKHDQ
  | IST_PUNPCKLBW
  | IST_PUNPCKLWD
  | IST_PUNPCKLDQ
  | IST_CVTTSS2SI
  | IST_CVTTPS2PI
  | IST_PREFETCH0
  | IST_PREFETCH1
  | IST_PREFETCH2
  | IST_PMADDUBSW
  | IST_EXTRACTPS
  | IST_PCMPESTRI
  | IST_PCMPESTRM
  | IST_PCMPISTRI
  | IST_PCMPISTRM
  | IST_MASKMOVDQU
  | IST_CMPXCHG16B
  | IST_PHMINPOSUW
  | IST_PREFETCHNTA

  (* FPU *)
  | IST_F2XM1
  | IST_FABS
  | IST_FADD
  | IST_FADDP
  | IST_FBLD
  | IST_FBSTP
  | IST_FCHS
  | IST_FCLEX
  | IST_FCOM
  | IST_FCOMP
  | IST_FCOMPP
  | IST_FDECSTP
  | IST_FDISI
  | IST_FDIV
  | IST_FDIVP
  | IST_FDIVR
  | IST_FDIVRP
  | IST_FENI
  | IST_FFREE
  | IST_FIADD
  | IST_FICOM
  | IST_FICOMP
  | IST_FIDIV
  | IST_FIDIVR
  | IST_FILD
  | IST_FIMUL
  | IST_FINCSTP
  | IST_FINIT
  | IST_FIST
  | IST_FISTP
  | IST_FISUB
  | IST_FISUBR
  | IST_FLD
  | IST_FLD1
  | IST_FLDCW
  | IST_FLDENV
  | IST_FLDENVW
  | IST_FLDL2E
  | IST_FLDL2T
  | IST_FLDLG2
  | IST_FLDLN2
  | IST_FLDPI
  | IST_FLDZ
  | IST_FMUL
  | IST_FMULP
  | IST_FNCLEX
  | IST_FNDISI
  | IST_FNENI
  | IST_FNINIT
  | IST_FNOP
  | IST_FNSAVE
  | IST_FNSAVEW
  | IST_FNSTCW
  | IST_FNSTENV
  | IST_FNSTENVW
  | IST_FNSTSW
  | IST_FPATAN
  | IST_FPREM
  | IST_FPTAN
  | IST_FRNDINT
  | IST_FRSTOR
  | IST_FRSTORW
  | IST_FSAVE
  | IST_FSAVEW
  | IST_FSCALE
  | IST_FSQRT
  | IST_FST
  | IST_FSTCW
  | IST_FSTENV
  | IST_FSTENVW
  | IST_FSTP
  | IST_FSTSW
  | IST_FSUB
  | IST_FSUBP
  | IST_FSUBR
  | IST_FSUBRP
  | IST_FTST
  | IST_FWAIT
  | IST_FXAM
  | IST_FXCH
  | IST_FXTRACT
  | IST_FYL2X
  | IST_FYL2XP1
  | IST_FSETPM
  | IST_FCOS
  | IST_FLDENVD
  | IST_FNSAVED
  | IST_FNSTENVD
  | IST_FPREM1
  | IST_FRSTORD
  | IST_FSAVED
  | IST_FSIN
  | IST_FSINCOS
  | IST_FSTENVD
  | IST_FUCOM
  | IST_FUCOMP
  | IST_FUCOMPP
  | IST_FCMOVBE
  | IST_FCMOVE
  | IST_FCMOVNB
  | IST_FCMOVNBE
  | IST_FCMOVNE
  | IST_FCMOVNU
  | IST_FCMOVU
  | IST_FCOMIP
  | IST_FUCOMI
  | IST_FUCOMIP
  | IST_FXRSTOR
  | IST_FXRSTORB
  | IST_FXRSTORW
  | IST_FXRSTORQ
  | IST_FXSAVE
  | IST_FXSAVEB
  | IST_FXSAVEW
  | IST_FXSAVEQ
  | IST_FFREEP

  (* SSE ? *)
  | IST_MOVDQA
  | IST_MOVDQU
  | IST_MOVAPD
  | IST_MOVUPD

  | IST_PSRLDQ
  | IST_PSLLDQ
  | IST_PSHUFD
  | IST_PSHUFLW

  | IST_PMULLW
  | IST_PMULHW
  | IST_PMULUDQ

  | IST_SHUFPD

  | IST_PUNPCKLQDQ
  | IST_PUNPCKHQDQ
  | IST_PADDQ
  | IST_PCMPGTD
  | IST_PSUBQ
  | IST_MOVDQ2Q


