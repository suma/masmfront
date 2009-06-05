
%{
open Type
open Parserhelper
%}


%token INCLUDE INCLUDELIB
%token MODEL SEGMENT_START SEGMENT_END PROC PROC_END
%token PUBLIC XEND
%token EXTERN NPAD ORG SHORT
%token TINY SMALL COMPACT MEDIUM LARGE HUGE FLAT
%token OFFSET
%token COMMON
%token DUP DUPQ_OP ST ALIGN
%token NEAR FAR NEAR16 NEAR32 FAR16 FAR32

%token <Type.cpu_type> CPU_TYPE

%token DB DW DD DF DQ DT
%token BYTE SBYTE WORD SWORD DWORD SDWORD FWORD QWORD
%token TBYTE REAL4 REAL8 REAL10 MMWORD XMMWORD


%token PTR

%token <Type.number> NUMBER
%token <string> IDENTIFIER
%token <string> STRING

%token COLON COMMA

%token <Type.instruction_prefix> INSTRUCTION_PREFIX
%token <Type.instruction> INSTRUCTION
%token <Type.register> REGISTER
%token <Type.segment_register> SEGMENT_REGISTER
%token BR_OPEN BR_CLOSE
%token PAR_OPEN PAR_CLOSE
%token <string> COMMENT

%token EQUAL
%token PLUS_OP MINUS_OP MUL_OP DIV_OP
%token QUESTION_OP

%token EOL EOF

%right EQUAL
%left PLUS_OP MINUS_OP
%left MUL_OP DIV_OP

%type <Type.program> main
%start main

%%

main:
  initialize line_list { $2 }

initialize:
  { initialize_helper () }

line_list:
    line { $1 }
  | line EOL line_list { $1 @ $3 }

line:
    line_impl { [$1] }
  | line_impl comment { [$1; $2] }
  | comment { [$1] }
  | /*empty*/ { [] }

comment:
    COMMENT { Comment($1) }

line_impl:
    XEND                  { EndAsm }
  | CPU_TYPE              { Cpu($1) }
  | INCLUDE IDENTIFIER        { Include($2) }
  | INCLUDELIB IDENTIFIER     { IncludeLib($2) }
  | MODEL model_list          { Model(List.rev $2) }
  | IDENTIFIER PROC           { ProcedureStart($1) }
  | IDENTIFIER PROC_END       { ProcedureEnd($1) }
  | IDENTIFIER SEGMENT_START  { SegmentStart($1) }
  | IDENTIFIER SEGMENT_END    { SegmentEnd($1) }
  | IDENTIFIER COLON          { Label($1) }
  | IDENTIFIER COLON COLON    { Label($1) }
  | PUBLIC IDENTIFIER         { Public($2) }
  | NPAD NUMBER           { Npad($2) }
  | ALIGN NUMBER          { Align($2) }
  | COMMON common_list    { Common(List.rev $2) }
  | EXTERN IDENTIFIER COLON extern_type { Extern($2, $4) }
  | ORG expression        { Org($2) }
  | IDENTIFIER EQUAL number
  {
    let size = match $3 with
      NumInt i -> i
    | _ -> Int64.of_int(0)
    in Let($1, size)
  }

  | IDENTIFIER data_type data_list { DataWithName($1, $2, $3) }
  | data_type data_list { Data($1, $2) }
  | INSTRUCTION instruction_operand { Instruction($1, (List.rev $2)) }
  | INSTRUCTION_PREFIX INSTRUCTION instruction_operand { InstructionWithPrefix($1, $2, (List.rev $3)) }
  | error
    { failwith
      (Printf.sprintf
        "parse error near characters %d-%d, line %d"
        (Parsing.symbol_start ())
        (Parsing.symbol_end ())
        (!Parserhelper.lineno)) }

;;

model_list:
    model_list COMMA model_element { $3 :: $1 }
  | model_element { [$1] }

model_element:
  data_model { $1 }

data_model:
    TINY    { DataTiny }
  | SMALL   { DataSmall }
  | COMPACT { DataCompact }
  | MEDIUM  { DataMedium }
  | LARGE   { DataLarge }
  | HUGE    { DataHuge }
  | FLAT    { DataFlat }

/* strictly: [[langtype]] [[NEAR | FAR]] label:type[[:count]] */
common_list:
    common_list COMMA common_element { $3 :: $1 }
  | common_element { [$1] }

common_element:
    IDENTIFIER COLON data_type { {name = $1; dtype = $3; count = Int64.of_int(1)} }
  | IDENTIFIER COLON data_type COLON NUMBER
  {
    let num_count = match $5 with
        NumInt i -> i
      | _ -> Int64.of_int(1)
    in {name = $1; dtype = $3; count = num_count}
  }

data_list:
    data_list COMMA data_initializer { $1 @ $3 }
  | data_initializer { $1 }

data_initializer:
    number { [DataNumber($1)] }
  | number DUP PAR_OPEN QUESTION_OP PAR_CLOSE
  {
    [DataDUPUndefined]
  }
  | number DUP PAR_OPEN NUMBER PAR_CLOSE
  {
    match $4 with
      NumInt i -> [DataDUP(i)]
    | _ -> [DataDUPUndefined]
  }
  | IDENTIFIER { [DataIdentifier($1)] }
  | data_initializer_string { $1 }
  | FLAT COLON expression
  {
    [DataExpression(DataFlat, $3)]
  }

data_initializer_string:
    STRING { [DataString($1)] }
  | data_initializer_string STRING
  {
    $1 @ [DataString($2)]
  }


expression:
    term { $1 }
  | SHORT expression { ExprWithPrefix(TermShort, $2) }
  | expression PLUS_OP  term { Expr(BinPlus, $1, $3) }
  | expression MINUS_OP term { Expr(BinMinus, $1, $3) }
  | expression MUL_OP   term { Expr(BinMul, $1, $3) }
  | expression DIV_OP   term { Expr(BinDiv, $1, $3) }

term:
    IDENTIFIER { TermIdentifier($1) }
  | number { TermNumber($1) }
  | REGISTER { TermRegister($1) }
  | SEGMENT_REGISTER { TermSegmentRegister($1) }

number:
    NUMBER { $1 }
  | MINUS_OP NUMBER
  {
    match $2 with
      NumInt i -> NumInt(Int64.mul Int64.minus_one i)
    | _ -> $2
  }

extern_type:
    PROC { DataProc }
  | data_type { $1 }

data_type:
    DB { DataBYTE }
  | DW { DataWORD }
  | DD { DataDWORD }
  | DF { DataFWORD }
  | DQ { DataQWORD }
  | DT { DataTBYTE }
  | BYTE    { DataBYTE }
  | SBYTE   { DataSBYTE }
  | WORD    { DataWORD }
  | SWORD   { DataSWORD }
  | DWORD   { DataDWORD }
  | SDWORD  { DataSDWORD }
  | FWORD   { DataFWORD }
  | QWORD   { DataQWORD }
  | TBYTE   { DataTBYTE }
  | REAL4   { DataREAL4 }
  | REAL8   { DataREAL8 }
  | REAL10  { DataREAL10 }

data_type_specifier:
    BYTE    { DataBYTE }
  | SBYTE   { DataSBYTE }
  | WORD    { DataWORD }
  | SWORD   { DataSWORD }
  | DWORD   { DataDWORD }
  | SDWORD  { DataSDWORD }
  | TBYTE   { DataTBYTE }
  | FWORD   { DataFWORD }
  | QWORD   { DataQWORD }
  | MMWORD  { DataMMWORD }
  | XMMWORD { DataXMMWORD }

instruction_operand:
    instruction_operand COMMA instruction_operand_impl { List.rev($3) :: $1 }
  | instruction_operand_impl { [List.rev $1] }
  | /* empty */ { [] }

instruction_operand_impl:
    OFFSET instruction_operand_impl2 { $2 :: [OperandOffset] }
  | instruction_operand_impl PTR instruction_operand_impl2 { $3 :: (OperandPtr :: $1) }
  | instruction_operand_impl COLON instruction_operand_impl2 { $3 :: (OperandColon :: $1) }
  | instruction_operand_impl BR_OPEN instruction_operand_impl2 BR_CLOSE { (OperandBr($3) :: $1) }
  | instruction_operand_impl2 { [$1] }

instruction_operand_impl2:
    expression { OperandExpression($1) }
  | type_ { $1 }
  | BR_OPEN expression BR_CLOSE { (OperandBr(OperandExpression($2))) }
  | ST PAR_OPEN expression PAR_CLOSE { OperandST($3) }

type_:
    data_type_specifier { OperandType($1) }
  | distance { OperandDistance($1) }

distance:
   NEAR16 { DistanceNear16 }
 | NEAR32 { DistanceNear32 }
 | FAR16  { DistanceFar16 }
 | FAR32  { DistanceFar32 }
 | nearfar { $1 }

nearfar:
    NEAR { DistanceNear }
  | FAR { DistanceFar }


