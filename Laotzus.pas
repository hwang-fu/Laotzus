(*-----------------------------------------*)
program Laotzus;
(*-----------------------------------------*)
{$mode objfpc}
(*-----------------------------------------*)
uses
    SysUtils,
    Classes;
(*-----------------------------------------*)
const
    TAB  = ^I;
    CR   = ^M;
    LF   = ^J;
    BELL = ^G;
(*-----------------------------------------*)
type
    bool   = boolean;
    Symbol = string;
    SymTable = array of Symbol;
    TablePtr = ^SymTable;
    TokenTypes = (
        TokenNone,          // 0
        TokenProgram,       // 1
        TokenUnit,
        TokenProject,
        TokenPackage,
        TokenModule,
        TokenIf,
        TokenElse,
        TokenThen,
        TokenDo,
        TokenWhile,
        TokenLoop,
        TokenRepeat, TokenUntil,
        TokenFor, TokenTo, TokenDownTo, TokenBy,
        TokenBreak, TokenContinue,
        TokenVar,  TokenVal,
        TokenBegin, TokenEnd,
        TokenType,
        TokenNull, TokenNil,
        TokenAnd, TokenOr, TokenXor,
        TokenNot,
        TokenAdd, TokenSub,
        TokenMul, TokenDiv, TokenMod,
        TokenChar, TokenWChar,
        TokenI8, TokenI16, TokenU8, TokenU16,
        TokenEqual1, TokenEqual2,
        TokenNotEqual1, TokenNotEqual2,
        TokenLess, TokenLessEq,
        TokenGreater, TokenGreaterEq,
        TokenEq, TokenNEQ,
        TokenGt, TokenLt,
        TokenGteq, TokenLteq,
        TokenArrow, TokenThickArrow,
        TokenExclamation,
        TokenBar, TokenCaret, TokenAmpersand,
        TokenDoubleBar, TokenDoubleCaret, TokenDoubleAmpersand,
        TokenComment,
        TokenPeriod,
        TokenRange,
        TokenColon,
        TokenNamespace,
        TokenAssign,
        TokenPlus, TokenMinus,
        TokenAsterisk, TokenSlash, TokenPercentage,
        TokenPlusEq, TokenMinusEq,
        TokenBackSlash,
        TokenArray, TokenTuple, TokenOf,
        TokenInc, TokenDec,
        TokenEnum, TokenStructure,
        TokenProcedure, TokenFunction, TokenReturn,
        TokenOrd,
        TokenTrue, TokenFalse,

        TokenSimpleIdent,
        TokenInteger,
        TokenCharacter,
        TokenString,
        TokenTypeName,

        TokenAddrAt,
        TokenComma,
        TokenTerminator,

        TokenLParenth, TokenRParenth,
        TokenLBracket, TokenRBracket,
        TokenLBrace, TokenRBrace,
        TokenLGuillemet, TokenRGuillemet,

        TokenCount
    );
(*-----------------------------------------*)
const
    // dynamical array are zero-indexed...
    KWlist: SymTable = (
        '',                 // index 0
        'program',          // index 1
        'unit',
        'project',
        'package',
        'module',
        'if',
        'else',
        'then',
        'do',
        'while',
        'loop',
        'repeat', 'until',
        'for', 'to', 'downto', 'by',
        'break', 'continue',
        'var', 'val',
        'begin', 'end',
        'type',
        'null', 'nil',
        'and', 'or', 'xor', 'not',
        'add', 'sub', 'mul', 'div', 'mod',
        'char', 'wchar', 'i8', 'i16', 'u8', 'u16',
        '=', '==', '!=', '<>', '<', '<=', '>', '>=',
        '->', '=>',
        '!', '|', '^', '&', '||', '^^', '&&',
        'eq', 'neq', 'gt', 'lt', 'gteq', 'lteq',
        '//',
        '.',
        '..',
        ':',
        '::',
        ':=',
        '+', '-', '*', '/', '%',
        '+=', '-=',
        '\',
        'array', 'tuple',
        'of',
        'inc', 'dec',
        'enum', 'structure',
        'procedure', 'function', 'return',
        'ord',
        'True', 'False'
    );

    MaxEntry = 500;
(*-----------------------------------------*)
var
    LineNr  : Uint64;
    ColumnNr: Uint64;

    look: char;
    buf : char;

    Token       : TokenTypes;
    TokenValue  : string;

    Buffer: string;
    BufferIndex: integer;

    ProgramName: string;

    FileNameIn: string;
    FileNameTemp: string;
    FileNameOut: string;

    LaotzusIn:   Text;
    LaotzusTemp: Text;
    LaotzusOut:  Text;

    SP: integer;
(*-----------------------------------------*)
procedure abort(s: string);             forward;
procedure expect(s: string);            forward;
procedure skipwhite;                    forward;
function isnewline(c: char): bool;      forward;
function isterminator(c: char): bool;   forward;
function isaddrat(c: char): bool;       forward;
function isnumbersign(c: char): bool;   forward;
function isoperator(c: char): bool;     forward;
function isperiod(c: char): bool;       forward;
function iscomma(c: char): bool;        forward;
function iscolon(c: char): bool;        forward;
(*-----------------------------------------*)
function isglobaldecl(t: TokenTypes): bool; forward;
function islocaldecl(t: TokenTypes) : bool; forward;
function validPtr(ptrstr: string)   : bool; forward;
function validType(typestr: string) : bool; forward;
function retrieveTypeSize(typestr: string): integer; forward;
(*-----------------------------------------*)
type
    StructureMember = object
        Name: string;
        TypeName: string;
        Offset: integer;

        procedure init(n: string; tn: string; o: integer);
        procedure display;
    end;

    EnumMember = object
        Name: string;
        Index: integer;

        procedure init(n: string; i: integer);
        procedure display;
    end;

    TupleMember = object
        Name: string;
        NTypes: integer;
        TupleTypes: array of string;

        procedure display;
    end;

    TStructureMembers = array of StructureMember;
    PStructureMembers = ^TStructureMembers;
    TEnumMembers = array of EnumMember;
    PEnumMembers = ^TEnumMembers;
    TTupleMembers = array of TupleMember;
    PTupleMembers = ^TTupleMembers;

    SpecialType = record
        case Tag: integer of
            0: (
                // nothing...
            );
            1: (
                StructureField: record
                    NEntry: integer;
                    Entries: PStructureMembers;
                end;
            );
            2: (
                EnumField: record
                    NEntry: integer;
                    Entries: PEnumMembers;
                end;
            );
            3: (
                ArrayField: record
                    ArrayType: string;
                    BeginIndex: integer;
                    EndIndex: integer;
                end;
            );
            4: (
                TupleField: record
                    NEntry: integer;
                    Entries: PTupleMembers;
                end;
            );
    end;

    TypeEntry = object
        Name: string;
        Size: integer;
        Special: SpecialType;

        procedure init(n: string; s: integer; spe: integer);
        procedure setArrayType(t: string);
        procedure setArraySize(s: integer);
        procedure setArrayBeginIndex(i: integer);
        procedure setArrayEndIndex(i: integer);
        procedure setArrayIndices(b: integer; e: integer);
        procedure setArray(t: string; s: integer);
        procedure addStructMember(MemberName: string; MemberType: string; MemberOffset: integer);
        procedure addEnumMember(MemberName: string; MemberIndex: integer);

        procedure setName(n: string);
        procedure setSize(s: integer);
        procedure display;

        function matches(n: string): bool;

        function retrieve_struct_offset(n: string): integer;
        function retrieve_struct_member_type(n: string): string;
        function retrieve_enum_member_index(n: string): integer;
        function retrieve_array_beginIndex: integer;
        function retrieve_array_endIndex: integer;
        function retrieve_array_size: integer;
        function retrieve_array_type: string;

        function is_common: bool;
        function is_structure: bool;
        function is_enum: bool;
        function is_array: bool;
        function is_tuple: bool;
    end;

    TypeTable = object
        NEntry: integer;
        Entries: array of TypeEntry;

        procedure init;
        procedure add(Name: string; Size: integer; Special: integer);
        procedure addEntry(Entry: TypeEntry);
        procedure addStructMember(n: string; tn: string; o: integer);
        procedure display;

        function have_type(Name: string): bool;
        function retrieve_type(Name: string): TypeEntry;
        function retrieve_size(Name: string): integer;
        function retrieve_struct_offset(Name: string; MemberName: string): integer;

        function is_common(Name: string): bool;
        function is_structure(Name: string): bool;
        function is_enum(Name: string): bool;
        function is_array(Name: string): bool;
        function is_tuple(Name: string): bool;
    end;

    TypeEntryPtr = ^TypeEntry;
(*-----------------------------------------*)
procedure TypeTable.init;
begin
    self.NEntry := 0;
end;
(*-----------------------------------------*)
procedure TypeEntry.setName(n: string);
begin
    self.Name := n;
end;
(*-----------------------------------------*)
procedure TypeEntry.setSize(s: integer);
begin
    self.Size := s;
end;
(*-----------------------------------------*)
procedure TypeEntry.display;
var
    index: integer;
begin
    writeln('Type Name: ', self.Name);
    writeln('Type Size: ', self.Size);
    case self.Special.Tag of
        0:
            ;
        1:
            for index := 1 to self.Special.StructureField.NEntry do
                self.Special.StructureField.Entries^[index].display;
        2:
            for index := 1 to self.Special.EnumField.NEntry do
                self.Special.EnumField.Entries^[index].display;
        3:
        begin
            writeln('Array Type: ', self.Special.ArrayField.ArrayType);
            writeln('Array BeginIndex: ', self.Special.ArrayField.BeginIndex);
            writeln('Array EndIndex: ', self.Special.ArrayField.EndIndex);
        end;
        4:
            for index := 1 to self.Special.TupleField.NEntry do
                self.Special.TupleField.Entries^[index].display;
    else
        abort('Unknown type!');
    end;
end;
(*-----------------------------------------*)
procedure TypeEntry.init(n: string; s: integer; spe: integer);
begin
    self.Name := n;
    self.Size := s;
    self.Special.Tag := spe;
    case self.Special.Tag of
        0:
            ; // nothing...
        1:
            self.Special.StructureField.NEntry := 0;
        2:
            self.Special.EnumField.NEntry := 0;
        3:
        begin
            self.Special.ArrayField.BeginIndex := 0;
            self.Special.ArrayField.EndIndex := 0;
        end;
        4:
            self.Special.TupleField.NEntry := 0;
    else
        abort('Invalid special identifier!');
    end;
end;
(*-----------------------------------------*)
procedure TypeEntry.addStructMember(MemberName: string; MemberType: string; MemberOffset: integer);
var
    index: integer;
begin
    if self.Special.Tag <> 1 then
        abort('''' + self.Name + '''' + ' is not a structure!');

    if self.Special.StructureField.NEntry = 0 then
        New(self.Special.StructureField.Entries);
    inc(self.Special.StructureField.NEntry);
    index := self.Special.StructureField.NEntry;
    Setlength(self.Special.StructureField.Entries^, index + 1);
    self.Special.StructureField.Entries^[index].init(MemberName, MemberType, MemberOffset);
end;
(*-----------------------------------------*)
procedure TypeEntry.addEnumMember(MemberName: string; MemberIndex: integer);
var
    index: integer;
begin
    if self.Special.Tag <> 2 then
        abort('''' + self.Name + '''' + ' is not an enumeration!');

    if self.Special.EnumField.NEntry = 0 then
        New(self.Special.EnumField.Entries);
    inc(self.Special.EnumField.NEntry);
    index := self.Special.EnumField.NEntry;
    Setlength(self.Special.EnumField.Entries^, index + 1);
    self.Special.EnumField.Entries^[index].init(MemberName, MemberIndex);
end;
(*-----------------------------------------*)
procedure TypeEntry.setArrayType(t: string);
begin
    if self.Special.Tag <> 3 then
        abort('''' + self.Name + '''' + ' is not an array!');
    if not validType(t) then
        abort('Invalid type ' + '''' + t + '''' + ' !!');
    self.Special.ArrayField.ArrayType := t;
end;
(*-----------------------------------------*)
procedure TypeEntry.setArraySize(s: integer);
begin
    if self.Special.Tag <> 3 then
        abort('''' + self.Name + '''' + ' is not an array!');
    if s <= 0 then
        abort('Size for array ' + '''' + self.Name + '''' + ' must be >= 1 !!');
    self.Special.ArrayField.BeginIndex := 0;
    self.Special.ArrayField.EndIndex := s - 1;
end;
(*-----------------------------------------*)
procedure TypeEntry.setArray(t: string; s: integer);
begin
    if self.Special.Tag <> 3 then
        abort('''' + self.Name + '''' + ' is not an array!');
    if not validType(t) then
        abort('Invalid type ' + '''' + t + '''' + ' !!');
    if s <= 0 then
        abort('Size for array ' + '''' + self.Name + '''' + ' must be >= 1 !!');
    self.Special.ArrayField.ArrayType := t;
    self.Special.ArrayField.BeginIndex := 0;
    self.Special.ArrayField.EndIndex := s - 1;
end;
(*-----------------------------------------*)
procedure TypeEntry.setArrayBeginIndex(i: integer);
begin
    if self.Special.Tag <> 3 then
        abort('''' + self.Name + '''' + ' is not an array!');
    self.Special.ArrayField.BeginIndex := i;
end;
(*-----------------------------------------*)
procedure TypeEntry.setArrayEndIndex(i: integer);
begin
    if self.Special.Tag <> 3 then
        abort('''' + self.Name + '''' + ' is not an array!');
    self.Special.ArrayField.EndIndex := i;
end;
(*-----------------------------------------*)
procedure TypeEntry.setArrayIndices(b: integer; e: integer);
begin
    if self.Special.Tag <> 3 then
        abort('''' + self.Name + '''' + ' is not an array!');
    if b > e then
        abort('Array indices must be increasing!');
    self.Special.ArrayField.BeginIndex := b;
    self.Special.ArrayField.EndIndex := e;
end;
(*-----------------------------------------*)
function TypeEntry.retrieve_struct_offset(n: string): integer;
var
    index: integer;
begin
    if self.Special.Tag <> 1 then
        abort('''' + self.Name + '''' + ' is not a structure!');
    if self.Special.StructureField.NEntry = 0 then
        abort('''' + self.Name + '''' + ' has no structure member!');
    for index := 1 to self.Special.StructureField.NEntry do begin
        if self.Special.StructureField.Entries^[index].Name = n then begin
            retrieve_struct_offset := self.Special.StructureField.Entries^[index].Offset;
            exit;
        end;
    end;
    abort('''' + n + '''' + ' not found in structure ' + '''' + self.Name + '''' + ' !!');
end;
(*-----------------------------------------*)
function TypeEntry.retrieve_struct_member_type(n: string): string;
var
    index: integer;
begin
    if self.Special.Tag <> 1 then
        abort('''' + self.Name + '''' + ' is not a structure!');
    if self.Special.StructureField.NEntry = 0 then
        abort('''' + self.Name + '''' + ' has no structure member!');
    for index := 1 to self.Special.StructureField.NEntry do begin
        if self.Special.StructureField.Entries^[index].Name = n then begin
            retrieve_struct_member_type := self.Special.StructureField.Entries^[index].TypeName;
            exit;
        end;
    end;
    abort('''' + n + '''' + ' not found in structure ' + '''' + self.Name + '''' + ' !!');
end;
(*-----------------------------------------*)
function TypeEntry.retrieve_enum_member_index(n: string): integer;
var
    index: integer;
begin
    if self.Special.Tag <> 2 then
        abort('''' + self.Name + '''' + ' is not a enumeration!');
    if self.Special.EnumField.NEntry = 0 then
        abort('''' + self.Name + '''' + ' has no enumeration member!');
    for index := 1 to self.Special.EnumField.NEntry do begin
        if self.Special.EnumField.Entries^[index].Name = n then begin
            retrieve_enum_member_index := self.Special.EnumField.Entries^[index].Index;
        end;
    end;
    abort('''' + n + '''' + ' not found in enumeration ' + '''' + self.Name + '''' + ' !!');
end;
(*-----------------------------------------*)
function TypeEntry.retrieve_array_beginIndex: integer;
begin
    if self.Special.Tag <> 3 then
        abort('''' + self.Name + '''' + ' is not an array!');
    retrieve_array_beginIndex := self.Special.ArrayField.BeginIndex;
end;
(*-----------------------------------------*)
function TypeEntry.retrieve_array_endIndex: integer;
begin
    if self.Special.Tag <> 3 then
        abort('''' + self.Name + '''' + ' is not an array!');
    retrieve_array_endIndex := self.Special.ArrayField.EndIndex;
end;
(*-----------------------------------------*)
function TypeEntry.retrieve_array_size: integer;
begin
    if self.Special.Tag <> 3 then
        abort('''' + self.Name + '''' + ' is not an array!');
    retrieve_array_size := self.Special.ArrayField.EndIndex - self.Special.ArrayField.BeginIndex + 1;
end;
(*-----------------------------------------*)
function TypeEntry.retrieve_array_type: string;
begin
    if self.Special.Tag <> 3 then
        abort('''' + self.Name + '''' + ' is not an array!');
    retrieve_array_type := self.Special.ArrayField.ArrayType;
end;
(*-----------------------------------------*)
function TypeEntry.is_common: bool;
begin
    is_common := self.Special.Tag = 0;
end;
(*-----------------------------------------*)
function TypeEntry.is_structure: bool;
begin
    is_structure := self.Special.Tag = 1;
end;
(*-----------------------------------------*)
function TypeEntry.is_enum: bool;
begin
    is_enum := self.Special.Tag = 2;
end;
(*-----------------------------------------*)
function TypeEntry.is_array: bool;
begin
    is_array := self.Special.Tag = 3;
end;
(*-----------------------------------------*)
function TypeEntry.is_tuple: bool;
begin
    is_tuple := self.Special.Tag = 4;
end;
(*-----------------------------------------*)
function TypeEntry.matches(n: string): bool;
begin
    matches := self.Name = n;
end;
(*-----------------------------------------*)
procedure StructureMember.init(n: string; tn: string; o: integer);
begin
    self.Name := n;
    self.TypeName := tn;
    self.Offset := o;
end;
(*-----------------------------------------*)
procedure StructureMember.display;
begin
    writeln(TAB, 'member name: ', self.Name);
    writeln(TAB, 'member type: ', self.TypeName);
    writeln(TAb, 'member offset: ', self.Offset);
end;
(*-----------------------------------------*)
procedure EnumMember.init(n: string; i: integer);
begin
    self.Name  := n;
    self.Index := i;
end;
(*-----------------------------------------*)
procedure EnumMember.display;
begin
    writeln(TAB, 'enum member name: ', self.Name);
    writeln(TAB, 'enum member index: ', self.Index);
end;
(*-----------------------------------------*)
procedure TupleMember.display;
begin
end;
(*-----------------------------------------*)
procedure TypeTable.add(Name: string; Size: integer; Special: integer);
begin
    inc(self.NEntry);
    Setlength(self.Entries, self.NEntry + 1);
    self.Entries[self.NEntry].init(Name, Size, Special);
end;
(*-----------------------------------------*)
procedure TypeTable.addEntry(Entry: TypeEntry);
begin
    inc(self.NEntry);
    Setlength(self.Entries, self.NEntry + 1);
    self.Entries[self.NEntry] := Entry;
end;
(*-----------------------------------------*)
procedure TypeTable.addStructMember(n: string; tn: string; o: integer);
begin
    self.Entries[self.NEntry].addStructMember(n, tn, o);
end;
(*-----------------------------------------*)
procedure TypeTable.display;
var
    index: integer;
begin
    if self.NEntry = 0 then exit;
    for index := 1 to self.NEntry do self.Entries[index].display;
end;
(*-----------------------------------------*)
function TypeTable.have_type(Name: string): bool;
var
    index: integer;
begin
    have_type := False;
    if self.NEntry = 0 then exit;

    if isaddrat(Name[1]) then begin
        have_type := validPtr(Name);
        exit;
    end;

    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then have_type := True;
    end;
end;
(*-----------------------------------------*)
function TypeTable.retrieve_type(Name: string): TypeEntry;
var
    index: integer;
begin
    if self.NEntry = 0 then abort('Type table is empty!');
    for index := 0 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            retrieve_type := self.Entries[index];
            exit;
        end;
    end;
    abort('Failed to retrieve type ' + '''' + Name + '''');
end;
(*-----------------------------------------*)
function TypeTable.retrieve_size(Name: string): integer;
var
    index: integer;
begin
    if self.NEntry = 0 then abort('Type table is empty!');

    if Name = '' then begin
        retrieve_size := 0;
        exit;
    end;

    if isaddrat(Name[1]) then begin
        // it's potentially a pointer
        if validPtr(Name) then begin
            // ptr size is always 2 bytes on our CPU machine...
            retrieve_size := 2;
            exit;
        end
        else abort('ptr type ' + '''' + Name + '''' + ' does not exist!');
    end;

    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            retrieve_size := self.Entries[index].Size;
            exit;
        end;
    end;
    abort('Failed to retrieve type size of ' + '''' + Name + '''');
end;
(*-----------------------------------------*)
function TypeTable.retrieve_struct_offset(Name: string; MemberName: string): integer;
var
    index: integer;
begin
    if self.NEntry = 0 then abort('Type table is empty!');
    for index := 0 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            retrieve_struct_offset := self.Entries[index].retrieve_struct_offset(MemberName);
            exit;
        end;
    end;
    abort('Failed to retrieve the offset of member ' + '''' + MemberName + '''' + ' of type ' + '''' + Name + '''');
end;
(*-----------------------------------------*)
function TypeTable.is_common(Name: string): bool;
var
    index: integer;
begin
    if self.NEntry = 0 then abort('Type table is empty!');
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            is_common := self.Entries[index].is_common;
            exit;
        end;
    end;
    abort('Could not find type named ' + '''' + Name + '''');
end;
(*-----------------------------------------*)
function TypeTable.is_structure(Name: string): bool;
var
    index: integer;
begin
    is_structure := False;
    if self.NEntry = 0 then exit;
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            is_structure := self.Entries[index].is_structure;
            exit;
        end;
    end;
end;
(*-----------------------------------------*)
function TypeTable.is_enum(Name: string): bool;
var
    index: integer;
begin
    is_enum := False;
    if self.NEntry = 0 then exit;
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            is_enum := self.Entries[index].is_enum;
            exit;
        end;
    end;
end;
(*-----------------------------------------*)
function TypeTable.is_array(Name: string): bool;
var
    index: integer;
begin
    is_array := False;
    if self.NEntry = 0 then exit;
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            is_array := self.Entries[index].is_array;
            exit;
        end;
    end;
end;
(*-----------------------------------------*)
function TypeTable.is_tuple(Name: string): bool;
var
    index: integer;
begin
    is_tuple := False;
    if self.NEntry = 0 then exit;
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            is_tuple := self.Entries[index].is_tuple;
            exit;
        end;
    end;
end;
(*-----------------------------------------*)
type
    VariableEntry = object
        Name: string;
        TypeName: string;
        Fixed: bool;

        procedure init(n: string; t: string; f:bool);
        procedure del;
        procedure display;

        function matches(n: string): bool;
    end;

    VariableTable = object
        NEntry: integer;
        Entries: array of VariableEntry;

        procedure init;
        procedure add(Name: string; TypeName: string; Fixed: bool);
        procedure del(Name: string);
        procedure display;

        function have(Name: string): bool;
        function retrieve_type(Name: string): string;
        function is_val(Name: string): bool;
        function is_var(Name: string): bool;
    end;

    VariableTablePtr = ^VariableTable;
(*-----------------------------------------*)
procedure VariableEntry.init(n: string; t: string; f:bool);
begin
    self.Name := n;
    self.TypeName := t;
    self.Fixed := f;
end;
(*-----------------------------------------*)
procedure VariableEntry.del;
begin
    self.Name := '';
end;
(*-----------------------------------------*)
procedure VariableEntry.display;
begin
    writeln('Varaible Name: ', self.Name);
    writeln('Varaible Type: ', self.TypeName);
    writeln('Varaible Constant: ', BoolToStr(self.Fixed, True));
end;
(*-----------------------------------------*)
function VariableEntry.matches(n: string): bool;
begin
    matches := self.Name = n;
end;
(*-----------------------------------------*)
procedure VariableTable.init;
begin
    self.NEntry := 0;
end;
(*-----------------------------------------*)
procedure VariableTable.add(Name: string; TypeName: string; Fixed: bool);
var
    index: integer;
begin
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches('') then begin
            self.Entries[index].init(Name, TypeName, Fixed);
            exit;
        end;
    end;

    self.NEntry := self.NEntry + 1;
    Setlength(self.Entries, self.NEntry + 1);
    self.Entries[self.NEntry].init(Name, TypeName, Fixed);
end;
(*-----------------------------------------*)
procedure VariableTable.del(Name: string);
var
    index: integer;
begin
    if self.NEntry = 0 then abort('variable table is empty!');
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            self.Entries[index].del;
            exit;
        end;
    end;
    abort('''' + Name + '''' + ' does not exist!');
end;
(*-----------------------------------------*)
procedure VariableTable.display;
var
    index: integer;
begin
    if self.NEntry = 0 then exit;
    for index := 1 to self.NEntry do self.Entries[index].display;
end;
(*-----------------------------------------*)
function VariableTable.have(Name: string): bool;
var
    index: integer;
begin
    have := False;
    if self.NEntry = 0 then exit;
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            have := True;
            exit;
        end;
    end;
end;
(*-----------------------------------------*)
function VariableTable.retrieve_type(Name: string): string;
var
    index: integer;
begin
    if self.NEntry = 0 then abort('Variable table is empty!');
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            retrieve_type := self.Entries[index].TypeName;
            exit;
        end;
    end;
    abort('Variable ' + '''' + Name + '''' + ' does not exist!');
end;
(*-----------------------------------------*)
function VariableTable.is_val(Name: string): bool;
var
    index: integer;
begin
    is_val := False;
    if self.NEntry = 0 then exit;
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            is_val := self.Entries[index].Fixed = True;
        end;
    end;
end;
(*-----------------------------------------*)
function VariableTable.is_var(Name: string): bool;
var
    index: integer;
begin
    is_var := False;
    if self.NEntry = 0 then exit;
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            is_var := self.Entries[index].Fixed <> True;
        end;
    end;
end;
(*-----------------------------------------*)
type
    LocalEntry = object
        Name: string;
        TypeName: string;
        Fixed: bool;
        BP: integer;

        procedure init(n: string; tn: string; f: bool; stackbackup: integer);
        procedure display;

        function matches(n: string): bool;
    end;

    LocalTable = object
        NEntry: integer;
        Entries: array of LocalEntry;

        procedure init;
        procedure add(Name: string; TypeName: string; Fixed: bool; BP: integer);
        procedure display;

        function have(Name: string): bool;
        function retrieve_type(Name: string): string;
        function retrieve_bp(Name: string): integer;
        function is_val(Name: string): bool;
        function is_var(Name: string): bool;
    end;

    LocalTablePtr = ^LocalTable;
(*-----------------------------------------*)
procedure LocalEntry.init(n: string; tn: string; f: bool; stackbackup: integer);
begin
    self.Name := n;
    self.TypeName := tn;
    self.Fixed := f;
    self.BP := stackbackup;
end;
(*-----------------------------------------*)
procedure LocalEntry.display;
begin
    writeln('Local Name: ', self.Name);
    writeln('Local Type: ', self.TypeName);
    writeln('Local Constant: ', BoolToStr(self.Fixed, True));
    writeln('Local BP: ', self.BP);
end;
(*-----------------------------------------*)
function LocalEntry.matches(n: string): bool;
begin
    matches := self.Name = n;
end;
(*-----------------------------------------*)
procedure LocalTable.init;
begin
    self.NEntry := 0;
end;
(*-----------------------------------------*)
procedure LocalTable.add(Name: string; TypeName: string; Fixed: bool; BP: integer);
begin
    inc(self.NEntry);
    Setlength(self.Entries, self.NEntry + 1);

    self.Entries[self.NEntry].init(Name, TypeName, Fixed, BP);
end;
(*-----------------------------------------*)
procedure LocalTable.display;
var
    index: integer;
begin
    if self.NEntry = 0 then exit;
    for index := 1 to self.NEntry do self.Entries[index].display;
end;
(*-----------------------------------------*)
function LocalTable.have(Name: string): bool;
var
    index: integer;
begin
    have := False;
    if self.NEntry = 0 then exit;
    for index := 0 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            have := True;
            exit;
        end;
    end;
end;
(*-----------------------------------------*)
function LocalTable.retrieve_type(Name: string): string;
var
    index: integer;
begin
    if self.NEntry = 0 then abort('Local table is empty!');
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            retrieve_type := self.Entries[index].TypeName;
            exit;
        end;
    end;
    abort('''' + Name + '''' + ' does not exist in local table!');
end;
(*-----------------------------------------*)
function LocalTable.retrieve_bp(Name: string): integer;
var
    index: integer;
begin
    if self.NEntry = 0 then abort('Local table is empty!');
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            retrieve_bp := self.Entries[index].BP;
            exit;
        end;
    end;
    abort('''' + Name + '''' + ' does not exist in local table!');
end;
(*-----------------------------------------*)
function LocalTable.is_val(Name: string): bool;
var
    index: integer;
begin
    is_val := False;
    if self.NEntry = 0 then exit;
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            is_val := self.Entries[index].Fixed = True;
        end;
    end;
end;
(*-----------------------------------------*)
function LocalTable.is_var(Name: string): bool;
var
    index: integer;
begin
    is_var := False;
    if self.NEntry = 0 then exit;
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            is_var := self.Entries[index].Fixed <> True;
        end;
    end;
end;
(*-----------------------------------------*)
type
    RoutineEntry = object
        Name: string;
        NParam: integer;
        Params: array of string; // parameter types
        Return: string; // '' for procedure
        BP: integer;

        procedure init(n: string; r: string);
        procedure addParam(TypeName: string);
        procedure setReturn(ReturnType: string);

        function matches(n: string): bool;
        function retrieve_nparam: integer;
        function retrieve_return: string;
        function retrieve_topBP: integer;
        function is_function: bool;
        function is_procedure: bool;
    end;

    RoutineTable = object
        NEntry: integer;
        Entries: array of RoutineEntry;

        procedure init;
        procedure add(Name: string; Return: string);
        procedure addParam(TypeName: string);
        procedure setReturn(Name: string; Return: string);

        function have_routine(Name: string): bool;
        function have_function(Name: string): bool;
        function have_procedure(Name: string): bool;
        function is_function(Name: string): bool;
        function is_procedure(Name: string): bool;
        function retrieve_nparam(Name: string): integer;
        function retrieve_return(Name: string): string;
        function retrieve_topBP: integer;
        function get_topBP(Name: string): integer;
    end;

    RoutineTablePtr = ^RoutineTable;
(*-----------------------------------------*)
procedure RoutineEntry.init(n: string; r: string);
begin
    self.Name := n;
    self.Return := r;
    self.NParam := 0;
    self.BP := 4;
end;
(*-----------------------------------------*)
procedure RoutineEntry.addParam(TypeName: string);
var
    TypeSize: integer;
begin
    inc(self.NParam);
    Setlength(self.Params, self.NParam + 1);

    TypeSize := retrieveTypeSize(TypeName);
    self.BP := self.BP + TypeSize;

    self.Params[self.NParam] := TypeName;
end;
(*-----------------------------------------*)
procedure RoutineEntry.setReturn(ReturnType: string);
begin
    self.Return := ReturnType;
end;
(*-----------------------------------------*)
function RoutineEntry.matches(n: string): bool;
begin
    matches := self.Name = n;
end;
(*-----------------------------------------*)
function RoutineEntry.retrieve_nparam: integer;
begin
    retrieve_nparam := self.NParam;
end;
(*-----------------------------------------*)
function RoutineEntry.retrieve_return: string;
begin
    retrieve_return := self.Return;
end;
(*-----------------------------------------*)
function RoutineEntry.retrieve_topBP: integer;
begin
    retrieve_topBP := self.BP;
end;
(*-----------------------------------------*)
function RoutineEntry.is_function: bool;
begin
    is_function := self.Return <> '';
end;
(*-----------------------------------------*)
function RoutineEntry.is_procedure: bool;
begin
    is_procedure := self.Return = '';
end;
(*-----------------------------------------*)
procedure RoutineTable.init;
begin
    self.NEntry := 0;
end;
(*-----------------------------------------*)
procedure RoutineTable.add(Name: string; Return: string);
begin
    inc(self.NEntry);
    Setlength(self.Entries, self.NEntry + 1);

    self.Entries[self.NEntry].init(Name, Return);
end;
(*-----------------------------------------*)
procedure RoutineTable.addParam(TypeName: string);
begin
    self.Entries[self.NEntry].addParam(TypeName);
end;
(*-----------------------------------------*)
procedure RoutineTable.setReturn(Name: string; Return: string);
var
    index: integer;
begin
    if self.NEntry = 0 then abort('routine table is empty!');
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            self.Entries[index].setReturn(Return);
            exit;
        end;
    end;
    abort('routine ' + '''' + Name + '''' + ' does not exist!');
end;
(*-----------------------------------------*)
function RoutineTable.have_routine(Name: string): bool;
var
    index: integer;
begin
    have_routine := False;
    if self.NEntry = 0 then exit;
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            have_routine := True;
            exit;
        end;
    end;
end;
(*-----------------------------------------*)
function RoutineTable.have_function(Name: string): bool;
var
    index: integer;
begin
    have_function := False;
    if self.NEntry = 0 then exit;
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            have_function := True;
            exit;
        end;
    end;
end;
(*-----------------------------------------*)
function RoutineTable.have_procedure(Name: string): bool;
var
    index: integer;
begin
    have_procedure := False;
    if self.NEntry = 0 then exit;
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            have_procedure := True;
            exit;
        end;
    end;
end;
(*-----------------------------------------*)
function RoutineTable.is_function(Name: string): bool;
var
    index: integer;
begin
    is_function := False;
    if self.NEntry = 0 then exit;
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            is_function := self.Entries[index].is_function;
            exit;
        end;
    end;
end;
(*-----------------------------------------*)
function RoutineTable.is_procedure(Name: string): bool;
var
    index: integer;
begin
    is_procedure := False;
    if self.NEntry = 0 then exit;
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            is_procedure := self.Entries[index].is_procedure;
            exit;
        end;
    end;
end;
(*-----------------------------------------*)
function RoutineTable.retrieve_nparam(Name: string): integer;
var
    index: integer;
begin
    if self.NEntry = 0 then abort('routine table is empty!');
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            retrieve_nparam := self.Entries[index].retrieve_nparam;
            exit;
        end;
    end;
    abort('routine ' + '''' + Name + '''' + ' does not exist!');
end;
(*-----------------------------------------*)
function RoutineTable.retrieve_return(Name: string): string;
var
    index: integer;
begin
    if self.NEntry = 0 then abort('routine table is empty!');
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            retrieve_return := self.Entries[index].retrieve_return;
            exit;
        end;
    end;
    abort('routine ' + '''' + Name + '''' + ' does not exist!');
end;
(*-----------------------------------------*)
function RoutineTable.retrieve_topBP: integer;
begin
    if self.NEntry = 0 then abort('routine table is empty!');
    retrieve_topBP := self.Entries[self.NEntry].retrieve_topBP;
end;
(*-----------------------------------------*)
function RoutineTable.get_topBP(Name: string): integer;
var
    index: integer;
begin
    if self.NEntry = 0 then abort('routine table is empty!');
    for index := 1 to self.NEntry do begin
        if self.Entries[index].matches(Name) then begin
            get_topBP := self.Entries[index].retrieve_topBP;
            exit;
        end;
    end;
    abort('routine ' + '''' + Name + '''' + ' does not exist!');
end;
(*-----------------------------------------*)
var
    variables: VariableTable;
    routines: RoutineTable;
    types: TypeTable;
(*-----------------------------------------*)
procedure analyzer;                                 forward;
procedure AnalyzeExpression(locals: LocalTablePtr); forward;
procedure AnalyzeTerm(locals: LocalTablePtr);       forward;
procedure AnalyzeFactor(locals: LocalTablePtr);     forward;
procedure AnalyzeRoutineCall(locals: LocalTablePtr);forward;
(*-----------------------------------------*)
function InterpExpression: integer;                 forward;
function InterpTerm: integer;                       forward;
function InterpFactor: integer;                     forward;
(*-----------------------------------------*)
procedure lookahead;
begin
    read(LaotzusIn, look);

    columnNr := columnNr + 1;
    if isnewline(look) then
    begin
        lineNr := lineNr + 1;
        columnNr := 0;
    end;
end;
(*-----------------------------------------*)
procedure readline;
begin
    Buffer := TokenValue;
    while not isterminator(look) do Buffer := Buffer + look;
end;
(*-----------------------------------------*)
procedure LineCol;
begin
    write('(line: ' + IntToStr(lineNr) + ', column: ' + IntToStr(columnNr) + ') ');
end;
(*-----------------------------------------*)
procedure Debug;
begin
    write(TAB, 'Debug:');
    LineCol;
    writeln;
    write(TAB);
    writeln('TokenValue: ', TokenValue);
    write(TAB);
    writeln('TokenTypes: ', Token);
end;
(*-----------------------------------------*)
procedure error(s: string);
begin
    writeln;
    LineCol;
    writeln(BELL, 'Error: ', s);
end;
(*-----------------------------------------*)
procedure abort(s: string);
begin
    error(s);
    halt;
end;
(*-----------------------------------------*)
procedure expect(s: string);
begin
    abort('Expecting ' + s + ', but received ' + '''' + TokenValue + '''' + '!');
end;
(*-----------------------------------------*)
function queryTable(table: TablePtr; s: string; n: integer): integer;
var
    found: bool;
    i: integer;
begin
    found := False;
    i := n;
    while (i > 0) and (not found) do
    begin
        if table^[i] = s then
            found := True
        else
            i := i - 1;
    end;
    queryTable := i;
end;
(*-----------------------------------------*)
function iscolon(c: char): bool;
begin
    iscolon := c = ':';
end;
(*-----------------------------------------*)
function iscomma(c: char): bool;
begin
    iscomma := c = ',';
end;
(*-----------------------------------------*)
function isperiod(c: char): bool;
begin
    isperiod := c = '.';
end;
(*-----------------------------------------*)
function isaddrat(c: char): bool;
begin
    isaddrat := c = '@';
end;
(*-----------------------------------------*)
function isnumbersign(c: char): bool;
begin
    isnumbersign := c in ['+', '-'];
end;
(*-----------------------------------------*)
function isoperator(c: char): bool;
begin
    isoperator := c in [
        '-', '+', '*', '/', '%',
        '<', '>', '=', '!',
        ':', '.'
    ];
end;
(*-----------------------------------------*)
function isupper(c: char): bool;
begin
    isupper := c in ['A'..'Z'];
end;
(*-----------------------------------------*)
function islower(c: char): bool;
begin
    islower := c in ['a'..'z'];
end;
(*-----------------------------------------*)
function isletter(c: char): bool;
begin
    isletter := isupper(c) or islower(c);
end;
(*-----------------------------------------*)
function isunderline(c: char): bool;
begin
    isunderline := c = '_';
end;
(*-----------------------------------------*)
function isalpha(c: char): bool;
begin
    isalpha := isletter(c) or isunderline(c);
end;
(*-----------------------------------------*)
function isdigit(c: char): bool;
begin
    isdigit := c in ['0'..'9'];
end;
(*-----------------------------------------*)
function isalnum(c: char): bool;
begin
    isalnum := isalpha(c) or isdigit(c);
end;
(*-----------------------------------------*)
function isptrname(c: char): bool;
begin
    isptrname := isalnum(c) or isaddrat(c);
end;
(*-----------------------------------------*)
function iswhitespace(c: char): bool;
begin
    iswhitespace := c in [' ', TAB];
end;
(*-----------------------------------------*)
function isnewline(c: char): bool;
begin
    isnewline := c in [CR, LF];
end;
(*-----------------------------------------*)
procedure skipwhite;
begin
    while iswhitespace(look) or isnewline(look) do lookahead;
end;
(*-----------------------------------------*)
function isterminator(c: char): bool;
begin
    isterminator := c = ';';
end;
(*-----------------------------------------*)
procedure skipterminator;
begin
    while isterminator(look) or iswhitespace(look) do lookahead;
end;
(*-----------------------------------------*)
function lineContains(s: string): bool;
begin
    lineContains := pos(s, Buffer) > 0;
end;
(*-----------------------------------------*)
function isControlToken(t: TokenTypes): bool;
begin
    isControlToken := False;
    if (t = TokenIf)
    or (t = TokenDo)
    or (t = TokenWhile)
    or (t = TokenLoop)
    or (t = TokenRepeat)
    then
        isControlToken := True;
end;
(*-----------------------------------------*)
function validPtr(ptrstr: string): bool;
var
    index: integer;
begin
    validPtr := False;

    index := 1;
    while index <= high(ptrstr) do
    begin
        if not isaddrat(ptrstr[index]) then break;
        inc(index);
    end;

    if index > high(ptrstr) then exit;

    validPtr := types.retrieve_size(copy(ptrstr, index, length(ptrstr) - index + 1)) > 0;
end;
(*-----------------------------------------*)
function validType(typestr: string): bool;
begin
    validType := types.have_type(typestr);
end;
(*-----------------------------------------*)
function retrieveTypeSize(typestr: string): integer;
begin
    if not types.have_type(typestr) then
        abort('''' + typestr + '''' + ' type does not exist!');
    retrieveTypeSize := types.retrieve_size(typestr);
end;
(*-----------------------------------------*)
procedure analyze_character;
begin
    if look <> '''' then expect('<single-quote>');
    lookahead;

    TokenValue := TokenValue + look;
    if Token = TokenBackSlash then begin
        lookahead;
        TokenValue := TokenValue + look;
    end;

    lookahead;
    if look <> '''' then expect('<single-quote>');
    Token := TokenCharacter;
    lookahead;
end;
(*-----------------------------------------*)
procedure analyze_string;
begin
    if look <> '«' then expect('«');
    lookahead;
    while not (look = '»') do begin
        TokenValue := TokenValue + look;
        lookahead;
    end;
    Token := TokenString;
    lookahead;
end;
(*-----------------------------------------*)
procedure analyze_operator;
var
    index: integer;
begin
    while isoperator(look) do begin
        TokenValue := TokenValue + look;
        lookahead;
    end;
    index := queryTable(@KWlist, TokenValue, high(KWlist));
    if index = 0 then
        abort('Invalid operator ' + '''' + TokenValue + '''')
    else begin
        Token := TokenTypes(index);
        if Token = TokenComment then begin
            while not isnewline(look) do lookahead;
            lookahead;
            analyzer;
        end;
    end;
end;
(*-----------------------------------------*)
procedure analyze_number;
begin
    while isdigit(look) do begin
        TokenValue := TokenValue + look;
        lookahead;
    end;
    Token := TokenInteger;
end;
(*-----------------------------------------*)
procedure analyze_ident;
var
    index: integer;
begin
    while isalnum(look) do begin
        TokenValue := TokenValue + look;
        lookahead;
    end;
    index := queryTable(@KWlist, TokenValue, high(KWlist));
    if index = 0 then
        Token := TokenSimpleIdent
    else
        Token := TokenTypes(index);
end;
(*-----------------------------------------*)
procedure analyzer;
begin
    skipwhite;
    Token := TokenNone;
    TokenValue := '';
    if isalpha(look) then
        analyze_ident
    else if isdigit(look) then
        analyze_number
    else if isoperator(look) then
        analyze_operator
    else if look = '(' then
    begin
        Token := TokenLParenth;
        TokenValue := TokenValue + look;
        lookahead;
        if look = '*' then
        begin
            while True do
            begin
                lookahead;
                if look = '*' then begin
                    lookahead;
                    if look = ')' then break;
                end;
            end;
            lookahead;
            analyzer;
        end;
    end
    else if look = '[' then
    begin
        Token := TokenLBracket;
        TokenValue := TokenValue + look;
        lookahead;
    end
    else if look = '{' then
    begin
        Token := TokenLBrace;
        TokenValue := TokenValue + look;
        lookahead;
    end
    else if look = ')' then
    begin
        Token := TokenRParenth;
        TokenValue := TokenValue + look;
        lookahead;
    end
    else if look = ']' then
    begin
        Token := TokenRBracket;
        TokenValue := TokenValue + look;
        lookahead;
    end
    else if look = '}' then
    begin
        Token := TokenRBrace;
        TokenValue := TokenValue + look;
        lookahead;
    end
    else if look = '''' then
        analyze_character
    else if look = '«' then
        analyze_string
    else if iscomma(look) then
    begin
        Token := TokenComma;
        TokenValue := TokenValue + look;
        lookahead;
    end
    else if isaddrat(look) then
    begin
        Token := TokenAddrAt;
        TokenValue := TokenValue + look;
        lookahead;
    end
    else if isterminator(look) then
    begin
        TokenValue := TokenValue + look;
        Token := TokenTerminator;
        skipterminator;
    end
    else
        abort('strange token: ' + TokenValue);
end;
(*-----------------------------------------*)
procedure AnalyzeAdd(locals: LocalTablePtr);
begin
    writeln(LaotzusTemp, TAB, 'push ax');
    analyzer;
    AnalyzeTerm(locals);
    writeln(LaotzusTemp, TAB, 'pop cx');
    writeln(LaotzusTemp, TAB, 'add ax, cx');
end;
(*-----------------------------------------*)
procedure AnalyzeSub(locals: LocalTablePtr);
begin
    writeln(LaotzusTemp, TAB, 'push ax');
    analyzer;
    AnalyzeTerm(locals);
    writeln(LaotzusTemp, TAB, 'pop cx');
    writeln(LaotzusTemp, TAB, 'sub ax, cx');
    writeln(LaotzusTemp, TAB, 'neg ax');
end;
(*-----------------------------------------*)
procedure AnalyzeMul(locals: LocalTablePtr);
begin
    writeln(LaotzusTemp, TAB, 'push ax');
    analyzer;
    AnalyzeFactor(locals);
    writeln(LaotzusTemp, TAB, 'pop cx');
    writeln(LaotzusTemp, TAB, 'mul ax, cx');
end;
(*-----------------------------------------*)
procedure AnalyzeDiv(locals: LocalTablePtr);
begin
    writeln(LaotzusTemp, TAB, 'push ax');
    analyzer;
    AnalyzeFactor(locals);
    writeln(LaotzusTemp, TAB, 'pop cx');
    writeln(LaotzusTemp, TAB, 'div cx, ax');
    writeln(LaotzusTemp, TAB, 'mov ax, cx');
end;
(*-----------------------------------------*)
procedure AnalyzeMod(locals: LocalTablePtr);
begin
    writeln(LaotzusTemp, TAB, 'push ax');
    analyzer;
    AnalyzeFactor(locals);
    writeln(LaotzusTemp, TAB, 'pop cx');
    writeln(LaotzusTemp, TAB, 'mod cx, ax');
    writeln(LaotzusTemp, TAB, 'mov ax, cx');
end;
(*-----------------------------------------*)
procedure AnalyzeExpression(locals: LocalTablePtr);
begin
    AnalyzeTerm(locals);
    while  (Token = TokenPlus)
        or (Token = TokenMinus) do
    begin
        if Token = TokenPlus then
            AnalyzeAdd(locals)
        else if Token = TokenMinus then
            AnalyzeSub(locals)
    end;
end;
(*-----------------------------------------*)
procedure AnalyzeTerm(locals: LocalTablePtr);
begin
    AnalyzeFactor(locals);
    while  (Token = TokenAsterisk)
        or (Token = TokenSlash)
        or (Token = TokenPercentage)
    do begin
        if Token = TokenAsterisk then
            AnalyzeMul(locals)
        else if Token = TokenSlash then
            AnalyzeDiv(locals)
        else if Token = TokenPercentage then
            AnalyzeMod(locals);
    end;
end;
(*-----------------------------------------*)
procedure AnalyzeFactor(locals: LocalTablePtr);
var
    BP: integer;
    SizeDirective: string;
begin
    if Token = TokenLParenth then
    begin
        analyzer;
        AnalyzeExpression(locals);
        if Token <> TokenRParenth then expect(')');
        analyzer;
    end
    else if Token = TokenInteger then
    begin
        writeln(LaotzusTemp, TAB, 'mov ax, ', TokenValue);
        analyzer;
    end
    else if Token = TokenPlus then
    begin
        analyzer;
        AnalyzeFactor(locals);
    end
    else if Token = TokenMinus then
    begin
        analyzer;
        AnalyzeFactor(locals);
        writeln(LaotzusTemp, TAB, 'neg ax');
    end
    else if Token = TokenAddrAt then
    begin
        analyzer;
        if Token <> TokenSimpleIdent then expect('<simple-ident>');
        if locals^.have(TokenValue) then
        begin
            BP := locals^.retrieve_bp(TokenValue);
            if BP < 0 then
                writeln(LaotzusTemp, TAB, 'lea ax, [bp - ', -BP, ']')
            else if BP > 0 then
                writeln(LaotzusTemp, TAB, 'lea ax, [bp + ', BP, ']')
            else
                writeln(LaotzusTemp, TAB, 'lea ax, [bp]');
            analyzer;
        end
        else if variables.have(TokenValue) then
        begin
            writeln(LaotzusTemp, TAB, 'lea ax, [', TokenValue, ']');
            analyzer;
        end
        else if routines.have_routine(TokenValue) then
        begin
            writeln(LaotzusTemp, TAB, 'lea ax, [', TokenValue, ']');
            analyzer;
        end
        else
            abort('''' + TokenValue + '''' + ' is an invlid ident!');
    end
    else if Token = TokenSimpleIdent then
    begin
        if locals^.have(TokenValue) then
        begin
            BP := locals^.retrieve_bp(TokenValue);
            if 2 = types.retrieve_size(locals^.retrieve_type(TokenValue)) then
                SizeDirective := 'word'
            else
                SizeDirective := 'byte';
            if BP < 0 then
                writeln(LaotzusTemp, TAB, 'mov ax, ', SizeDirective, ' [bp - ', -BP, ']')
            else if BP > 0 then
                writeln(LaotzusTemp, TAB, 'mov ax, ', SizeDirective, ' [bp + ', BP, ']')
            else
                writeln(LaotzusTemp, TAB, 'mov ax, ', SizeDirective, ' [bp]');
            analyzer;
        end
        else if variables.have(TokenValue) then
        begin
            if 2 = types.retrieve_size(variables.retrieve_type(TokenValue))
            then
                SizeDirective := 'word'
            else
                SizeDirective := 'byte';
            writeln(LaotzusTemp, TAB, 'mov ax, ', SizeDirective, ' [', TokenValue, ']');
            analyzer;
        end
        else if routines.have_routine(TokenValue) then
        begin
            SP := SP + types.retrieve_size(routines.retrieve_return(TokenValue));
            locals^.add(TokenValue, routines.retrieve_return(TokenValue), False, -SP);
            AnalyzeRoutineCall(locals);
        end
        else
            abort('''' + TokenValue + '''' + ' is an invlid ident!');
    end
    else
    begin
    end;
end;
(*-----------------------------------------*)
 procedure AnalyzeControlFlow(locals: LocalTablePtr);
 begin
 end;
(*-----------------------------------------*)
procedure AnalyzeParamsCall(locals: LocalTablePtr; var NParam: integer);
begin
    analyzer;
    if Token = TokenRParenth then exit;
    AnalyzeExpression(locals);
    writeln(LaotzusTemp, TAB, 'push ax');
    inc(NParam);
    while Token = TokenComma do begin
        analyzer;
        AnalyzeExpression(locals);
        writeln(LaotzusTemp, TAB, 'push ax');
        inc(NParam);
    end;
end;
(*-----------------------------------------*)
procedure AnalyzeRoutineCall(locals: LocalTablePtr);
var
    RoutineName: string;
    NParam: integer;
    BP: integer;
    SizeDirective: string;
    ReturnSize: integer;
begin
    NParam := 0;
    RoutineName := TokenValue;
    SizeDirective := '';

    ReturnSize := types.retrieve_size(routines.retrieve_return(RoutineName));
    if ReturnSize > 0 then
    begin
        BP := locals^.retrieve_bp(RoutineName);
        if BP < 0 then
            writeln(LaotzusTemp, TAB, 'lea ax, [bp - ', -BP, ']')
        else if BP > 0 then
            writeln(LaotzusTemp, TAB, 'lea ax, [bp + ', BP, ']')
        else begin
            writeln(LaotzusTemp, TAB, 'lea ax, [bp]');
        end;
        writeln(LaotzusTemp, TAB, 'push ax');
        inc(NParam);
    end;


    analyzer;
    if Token = TokenLParenth then begin
        AnalyzeParamsCall(locals, NParam);
        if Token <> TokenRParenth then expect(')');
        analyzer; { to jump through the <terminator> }
    end;
    writeln(LaotzusTemp, TAB, 'call ', RoutineName);
    if NParam <> routines.retrieve_nparam(RoutineName) then
        abort(
            'Parameter passed number wrong for routine ' + '''' + RoutineName + ''''
            + ', should be ' + IntToStr(routines.retrieve_nparam(RoutineName))
            + ' but only passed ' + IntToStr(NParam)
        );
    if NParam > 0 then
        writeln(LaotzusTemp, TAB, 'add sp, ', routines.get_topBP(RoutineName));
end;
(*-----------------------------------------*)
procedure AnalyzeAssignment(locals: LocalTablePtr);
var
    LeftValue: string;
    LeftType: string;
    SP: integer;
    SizeDirective: string;
begin
    LeftValue := TokenValue;
    if
        (locals^.have(LeftValue) and locals^.is_val(LeftValue))
    or
        (variables.have(LeftValue) and variables.is_val(LeftValue))
    then abort('''' + LeftValue + '''' + ' is a constant!');

    analyzer;
    if Token <> TokenAssign then expect('<assignment>');

    analyzer;
    AnalyzeExpression(locals);

    if locals^.have(LeftValue) then
    begin
        LeftType := locals^.retrieve_type(LeftValue);
        SP := locals^.retrieve_bp(LeftValue);

        if 2 = types.retrieve_size(LeftType) then
            SizeDirective := 'word'
        else
            SizeDirective := 'byte';
        if SP < 0 then
            writeln(LaotzusTemp, TAB, 'mov ', SizeDirective, ' [bp - ', -SP, '], ax')
        else if SP > 0 then
            writeln(LaotzusTemp, TAB, 'mov ', SizeDirective, ' [bp + ', SP, '], ax')
        else
            writeln(LaotzusTemp, TAB, 'mov ', SizeDirective, ' [bp], ax');
    end
    else if variables.have(LeftValue) then
    begin
        LeftType := variables.retrieve_type(LeftValue);
        if 2 = types.retrieve_size(LeftType)
        then
            SizeDirective := 'word'
        else
            SizeDirective := 'byte';
        writeln(LaotzusTemp, TAB, 'mov ', SizeDirective, ' [', LeftValue, '], ax');
    end;
end;
(*-----------------------------------------*)
procedure AnalyzeGeneralExpression(RoutineName: string; locals: LocalTablePtr);
var
    ReturnSize: integer;
begin
    ReturnSize := 0;
    analyzer;
    if isControlToken(Token) then begin
        AnalyzeControlFlow(locals);
    end
    else if Token = TokenReturn then begin
        analyzer;
        if routines.is_procedure(RoutineName) then
        begin
            if Token <> TokenTerminator then expect('<terminator>');
            writeln(LaotzusTemp, TAB, 'jmp .exit');
            analyzer;
        end
        else if routines.is_function(RoutineName) then
        begin
            AnalyzeExpression(locals);
            writeln(LaotzusTemp, TAB, 'push ax');
            writeln(LaotzusTemp, TAB, 'lea ax, [bp + ', routines.get_topBP(RoutineName) - 2, ']');
            writeln(LaotzusTemp, TAB, 'pop cx');
            writeln(LaotzusTemp, TAB, 'mov word [ax], cx');
            writeln(LaotzusTemp, TAB, 'jmp .exit');
        end
        else
        begin
            abort('''' + RoutineName + '''' + ' is not a routine!');
        end;
    end
    else if Token = TokenSimpleIdent then begin
        if routines.have_routine(TokenValue) then begin
            ReturnSize := types.retrieve_size(routines.retrieve_return(TokenValue));
            SP := SP + ReturnSize;
            locals^.add(TokenValue, routines.retrieve_return(TokenValue), False, -SP);
            AnalyzeRoutineCall(locals);
        end
        else if locals^.have(TokenValue) or variables.have(TokenValue) then begin
            AnalyzeAssignment(locals);
        end
        else abort('''' + TokenValue + '''' + ' is an invalid token!');
    end
end;
(*-----------------------------------------*)
function InterpAdd(value: integer): integer;
begin
    analyzer;
    InterpAdd := value + InterpTerm;
end;
(*-----------------------------------------*)
function InterpSub(value: integer): integer;
begin
    analyzer;
    InterpSub := value - InterpTerm;
end;
(*-----------------------------------------*)
function InterpMul(value: integer): integer;
begin
    analyzer;
    InterpMul := value * InterpFactor;
end;
(*-----------------------------------------*)
function InterpDiv(value: integer): integer;
begin
    analyzer;
    InterpDiv := value div InterpFactor;
end;
(*-----------------------------------------*)
function InterpMod(value: integer): integer;
begin
    analyzer;
    InterpMod := value mod InterpFactor;
end;
(*-----------------------------------------*)
function InterpExpression: integer;
begin
    InterpExpression := InterpTerm;
    while  (Token = TokenPlus)
        or (Token = TokenMinus)
    do begin
        case Token of
            TokenPlus:
                InterpExpression := InterpAdd(InterpExpression);
            TokenMinus:
                InterpExpression := InterpSub(InterpExpression);
        end;
    end;
end;
(*-----------------------------------------*)
function InterpTerm: integer;
begin
    InterpTerm := InterpFactor;
    while  (Token = TokenAsterisk)
        or (Token = TokenSlash)
        or (Token = TokenPercentage)
    do begin
        case Token of
            TokenAsterisk:
                InterpTerm := InterpMul(InterpTerm);
            TokenSlash:
                InterpTerm := InterpDiv(InterpTerm);
            TokenPercentage:
                InterpTerm := InterpMod(InterpTerm);
        end;
    end;
end;
(*-----------------------------------------*)
function InterpFactor: integer;
begin
    InterpFactor := 0;
    if Token = TokenLParenth then begin
        analyzer;
        InterpFactor := InterpExpression;
        if Token <> TokenRParenth then expect(')');
        analyzer;
    end
    else if Token = TokenInteger then begin
        InterpFactor := StrToInt(TokenValue);
        analyzer;
    end
    else if Token = TokenPlus then begin
        analyzer;
        InterpFactor := InterpFactor()
    end
    else if Token = TokenMinus then begin
        analyzer;
        InterpFactor := -InterpFactor()
    end
    else
        abort('Only accepting integer expression for constant expression by far!');
end;
(*-----------------------------------------*)
procedure AnalyzeBody;
var
    locals: LocalTable;

    TempRead: Text;
    TempWrite: Text;
    FileLine: string;
    LineCount: integer;
begin

    locals.init;
    SP := 0;

    if Token <> TokenBegin then expect('<begin>');
    writeln(LaotzusTemp, ProgramName, ':');

    writeln(LaotzusTemp, TAB, 'push bp');
    writeln(LaotzusTemp, TAB, 'mov bp, sp');
    writeln(LaotzusTemp, TAB, 'nop');

    AnalyzeGeneralExpression(ProgramName, @locals);
    while Token = TokenTerminator do AnalyzeGeneralExpression(ProgramName, @locals);

    if Token <> TokenEnd then expect('<end>');
    analyzer;
    if Token <> TokenPeriod then expect('<period>');
    writeln(LaotzusTemp, '.exit:');
    writeln(LaotzusTemp, TAB, 'mov sp, bp');
    writeln(LaotzusTemp, TAB, 'pop bp');

    close(LaotzusTemp);

    assign(TempRead, FileNameTemp);
    reset(TempRead);
    if IOResult <> 0 then begin
        close(TempRead);
        writeln('Failed to open the file', FileNameTemp);
        exit;
    end;

    assign(TempWrite, FileNameOut);
    rewrite(TempWrite);
    if IOResult <> 0 then begin
        close(TempWrite);
        writeln('Failed to open the file', FileNameOut);
        exit;
    end;

    while not eof(TempRead) do
    begin
        readln(TempRead, FileLine);
        if pos(ProgramName, FileLine) = 1 then
        begin
            writeln(TempWrite, FileLine);
            for LineCount := 1 to 2 do
            begin
                readln(TempRead, FileLine);
                writeln(TempWrite, FileLine);
            end;
            readln(TempRead, FileLine);
            writeln(TempWrite, TAB, 'sub sp, ', SP)
        end
        else
        begin
            writeln(TempWrite, FileLine);
        end;
    end;

    close(TempRead);
    close(TempWrite);

    DeleteFile(FileNameTemp);
    Rename(TempWrite, FileNameTemp);

    assign(LaotzusTemp, FileNameTemp);
    append(LaotzusTemp);

    RenameFile(FileNameTemp, FileNameOut)
end;
(*-----------------------------------------*)
function isglobaldecl(t: TokenTypes): bool;
begin
    isglobaldecl :=
        (t = TokenVar)
    or  (t = TokenVal)
    or  (t = TokenType)
    or  (t = TokenProcedure)
    or  (t = TokenFunction)
end;
(*-----------------------------------------*)
function islocaldecl(t: TokenTypes): bool;
begin
    islocaldecl :=
        (t = TokenVar)
    or  (t = TokenVal)
end;
(*-----------------------------------------*)
procedure AnalyzeGlobalVarIter;
var
    VarName: string;
    VarType: string;
begin
    VarName := '';
    VarType := '';
    ///
    // analyzing varaible name...
    analyzer;
    if Token <> TokenSimpleIdent then expect('<simple-ident>');
    VarName := TokenValue;
    if variables.have(VarName) then abort('Duplicated variable name ' + '''' + VarName + '''' + '!');
    ///

    /// analyzing variable type...
    analyzer;
    if Token <> TokenColon then expect('<colon>');

    analyzer;
    while Token = TokenAddrAt do begin
        VarType := VarType + TokenValue;
        analyzer;
    end;
    VarType := VarType + TokenValue;
    if not types.have_type(VarType) then expect('<type-name>');
    /// end analyzing variable type...

    variables.add(VarName, VarType, False);
    writeln(LaotzusTemp, TAB, VarName, ':');
    writeln(LaotzusTemp, TAB, '    resb ', types.retrieve_size(VarType));
end;
(*-----------------------------------------*)
 procedure AnalyzeGlobalVarDecl;
 begin
    AnalyzeGlobalVarIter;
    analyzer;
    while Token = TokenComma do begin
        AnalyzeGlobalVarIter;
        analyzer;
    end;

    if Token <> TokenTerminator then expect('<terminator>');
    analyzer;
end;
(*-----------------------------------------*)
procedure AnalyzeLocalVarIter(locals: LocalTablePtr; var BP: integer);
var
    VarName: string;
    VarType: string;
begin
    VarName := '';
    VarType := '';

    analyzer;
    if Token <> TokenSimpleIdent then expect('<simple-ident>');
    VarName := TokenValue;
    if locals^.have(VarName) then
        abort('Duplicated variable name ' + '''' + VarName + '''' + '!');

    analyzer;
    if Token <> TokenColon then expect('<colon>');

    analyzer;
    while Token = TokenAddrAt do begin
        VarType := VarType + TokenValue;
        analyzer;
    end;
    VarType := VarType + TokenValue;
    if not types.have_type(VarType) then expect('<type-name>');

    BP := BP - types.retrieve_size(VarType);
    locals^.add(VarName, VarType, False, BP);
end;
(*-----------------------------------------*)
procedure AnalyzeLocalVarDecl(locals: LocalTablePtr; var BP: integer);
begin
    AnalyzeLocalVarIter(locals, BP);
    analyzer;
    while Token = TokenComma do begin
        AnalyzeLocalVarIter(locals, BP);
        analyzer;
    end;

    if Token <> TokenTerminator then expect('<terminator>');
    analyzer;
end;
(*-----------------------------------------*)
procedure AnalyzeLocalValIter(locals: LocalTablePtr; var BP: integer);
var
    ValName: string;
    ValType: string;
    SizeDirective: string;
begin
    ValName := '';
    ValType := '';

    analyzer;
    if Token <> TokenSimpleIdent then expect('<simple-ident>');
    ValName := TokenValue;
    if locals^.have(ValName) then
        abort('Duplicated variable name ' + '''' + ValName + '''' + '!');

    analyzer;
    if Token <> TokenColon then expect('<colon>');

    analyzer;
    while Token = TokenAddrAt do begin
        ValType := ValType + TokenValue;
        analyzer;
    end;
    ValType := ValType + TokenValue;
    if not types.have_type(ValType) then expect('<type-name>');

    BP := BP - types.retrieve_size(ValType);
    locals^.add(ValName, ValType, True, BP);

    if 2 = types.retrieve_size(locals^.retrieve_type(ValName)) then
        SizeDirective := 'word'
    else if 1 = types.retrieve_size(locals^.retrieve_type(ValName)) then
        SizeDirective := 'byte';

    analyzer;
    if Token <> TokenEqual1 then expect('=');

    analyzer;
    AnalyzeExpression(locals);
    writeln(LaotzusTemp, TAB, 'mov ', SizeDirective, ' [bp - ', -BP, '], ax');

end;
(*-----------------------------------------*)
procedure AnalyzeLocalValDecl(locals: LocalTablePtr; var BP: integer);
begin
    AnalyzeLocalValIter(locals, BP);
    while Token = TokenComma do begin
        AnalyzeLocalValIter(locals, BP);
    end;

    if Token <> TokenTerminator then expect('<terminator>');
    analyzer;
end;
(*-----------------------------------------*)
procedure AnalyzeLocalDecls(locals: LocalTablePtr);
var
    BP: integer;
begin
    BP := 0;
    while islocaldecl(Token) do
    begin
        if Token = TokenVar then begin
            AnalyzeLocalVarDecl(locals, BP);
        end
        else if Token = TokenVal then begin
            AnalyzeLocalValDecl(locals, BP);
        end;
    end;

    SP := SP - BP;
    // writeln(LaotzusTemp, TAB, 'sub sp, ', -SP);

end;
(*-----------------------------------------*)
procedure AnalyzeParamsDecl(locals: LocalTablePtr);
var
    ParamNames: array of string;
    TypeNames: array of string;
    NParam: integer;
    index: integer;

    BP: integer;
begin
    NParam := 0;

    analyzer;
    if Token = TokenLParenth then begin
        analyzer;
        if Token = TokenSimpleIdent then
        begin
            NParam := NParam + 1;
            Setlength(ParamNames, NParam + 1);
            Setlength(TypeNames, NParam + 1);

            ParamNames[NParam] := TokenValue;

            analyzer;
            if Token <> TokenColon then expect('<colon>');

            analyzer;
            if not types.have_type(TokenValue) then expect('<type-name>');
            TypeNames[NParam] := TokenValue;

            analyzer;
        end;
        while Token = TokenComma do
        begin
            analyzer;
            if Token <> TokenSimpleIdent then expect('<simple-ident>');
            NParam := NParam + 1;
            Setlength(ParamNames, NParam + 1);
            Setlength(TypeNames, NParam + 1);

            ParamNames[NParam] := TokenValue;

            analyzer;
            if Token <> TokenColon then expect('<colon>');

            analyzer;
            if not types.have_type(TokenValue) then expect('<type-name>');
            TypeNames[NParam] := TokenValue;

            analyzer;
        end;

        for index := NParam downto 1 do begin
            if locals^.have(ParamNames[index]) then
                abort('Duplicated ' + '''' + ParamNames[index] + '''');
            locals^.add(ParamNames[index], TypeNames[index], False, routines.retrieve_topBP);
            routines.addParam(TypeNames[index]);
        end;

        if Token <> TokenRParenth then expect(')');
        analyzer;

    end;

end;
(*-----------------------------------------*)
 procedure AnalyzeProcedureDecl;
 var
    ProcedureName: string;
    locals: LocalTable;

    TempRead: Text;
    TempWrite: Text;
    FileLine: string;
    LineCount: integer;
 begin
    locals.init;
    SP := 0;

    analyzer;
    if Token <> TokenSimpleIdent then expect('<simple-ident>');
    ProcedureName := TokenValue;
    routines.add(ProcedureName, '');
    writeln(LaotzusTemp, ProcedureName, ':');
    writeln(LaotzusTemp, TAB, 'push bp');
    writeln(LaotzusTemp, TAB, 'mov bp, sp');
    writeln(LaotzusTemp, TAB, 'nop');

    AnalyzeParamsDecl(@locals);
    if Token <> TokenTerminator then expect('<terminator>');
    analyzer;

    if islocaldecl(Token) then AnalyzeLocalDecls(@locals);

    if Token <> TokenBegin then expect('<begin>');

    AnalyzeGeneralExpression(ProcedureName, @locals);
    while Token = TokenTerminator do AnalyzeGeneralExpression(ProcedureName, @locals);

    if Token <> TokenEnd then expect('<end>');
    analyzer;
    if Token <> TokenTerminator then expect('<terminator>');
    analyzer;

    writeln(LaotzusTemp, '.exit');
    writeln(LaotzusTemp, TAB, 'mov sp, bp');
    writeln(LaotzusTemp, TAB, 'pop bp');
    writeln(LaotzusTemp, TAB, 'ret');

    close(LaotzusTemp);

    assign(TempRead, FileNameTemp);
    reset(TempRead);
    if IOResult <> 0 then begin
        close(TempRead);
        writeln('Failed to open the file', FileNameTemp);
        exit;
    end;
    assign(TempWrite, FileNameOut);
    rewrite(TempWrite);
    if IOResult <> 0 then begin
        close(TempWrite);
        writeln('Failed to open the file', FileNameOut);
        exit;
    end;

    while not eof(TempRead) do
    begin
        readln(TempRead, FileLine);
        if pos(ProcedureName, FileLine) > 0 then
        begin
            writeln(TempWrite, FileLine);
            for LineCount := 1 to 2 do
            begin
                readln(TempRead, FileLine);
                writeln(TempWrite, FileLine);
            end;
            readln(TempRead, FileLine);
            writeln(TempWrite, TAB, 'sub sp, ', SP)
        end
        else
        begin
            writeln(TempWrite, FileLine);
        end;
    end;

    close(TempRead);
    close(TempWrite);

    DeleteFile(FileNameTemp);
    Rename(TempWrite, FileNameTemp);

    assign(LaotzusTemp, FileNameTemp);
    append(LaotzusTemp);

 end;
(*-----------------------------------------*)
procedure AnalyzeGlobalValIter;
var
    ValName: string;
    ValType: string;
    ValValue: string;
begin
    ValName := '';
    ValType := '';
    ValValue := '';

    analyzer;
    if Token <> TokenSimpleIdent then expect('<simple-ident>');
    ValName := TokenValue;
    if variables.have(ValName) then abort('Duplicated variable name ' + '''' + ValName + '''' + '!');

    analyzer;
    if Token = TokenColon then begin
        analyzer;
        while Token = TokenAddrAt do begin
            ValType := ValType + TokenValue;
            analyzer;
        end;
        ValType := ValType + TokenValue;
        if not types.have_type(ValType) then expect('<type-name>');
        analyzer;
    end;

    if Token <> TokenEqual1 then expect('=');
    analyzer;
    ValValue := IntToHex(InterpExpression, sizeof(integer * 2));

    variables.add(ValName, ValType, True);
    writeln(LaotzusTemp, TAB, ValName, ':');
    writeln(LaotzusTemp, TAB, '    db 0x', ValValue);
end;
(*-----------------------------------------*)
 procedure AnalyzeGlobalValDecl;
 begin
    AnalyzeGlobalValIter;
    while Token = TokenComma do begin
        AnalyzeGlobalValIter;
    end;

    if Token <> TokenTerminator then expect('<terminator>');
    analyzer;
 end;
(*-----------------------------------------*)
procedure AnalyzeStructureGlobalMemberDecl(Entry: TypeEntryPtr; var StructureSize: integer);
var
    MemberName: string;
    MemberType: string;
begin
    MemberName := '';
    MemberType := '';

    analyzer;
    if Token = TokenEnd then exit;
    if Token <> TokenSimpleIdent then expect('<simple-ident>');
    MemberName := TokenValue;

    analyzer;
    if Token <> TokenColon then expect('<colon>');

    analyzer;
    while Token = TokenAddrAt do begin
        MemberType := MemberType + TokenValue;
        analyzer;
    end;
    MemberType := MemberType + TokenValue;
    if not types.have_type(MemberType) then expect('<type-name>');
    Entry^.addStructMember(MemberName, MemberType, StructureSize);
    StructureSize := StructureSize + types.retrieve_size(MemberType);
    Entry^.setSize(StructureSize);
    analyzer;
end;
(*-----------------------------------------*)
procedure AnalyzeGlobalTypeIter;
var
    TypeName: string;
    AliasName: string;
    Entry: TypeEntry;
    StructureSize: integer;
begin
    TypeName := '';
    AliasName := '';
    StructureSize := 0;

    analyzer;
    if Token <> TokenSimpleIdent then expect('<simple-ident>');
    AliasName := TokenValue;
    if types.have_type(AliasName) then abort('Duplicated type name ' + '''' + AliasName + '''');

    analyzer;
    if Token <> TokenEqual1 then expect('=');

    analyzer;
    if Token = TokenStructure then begin
        analyzer;
        if Token <> TokenBegin then expect('<begin>');
        Entry.init(AliasName, 0, 1);

        AnalyzeStructureGlobalMemberDecl(@Entry, StructureSize);
        while Token = TokenComma do begin
            AnalyzeStructureGlobalMemberDecl(@Entry, StructureSize);
        end;

        if Token <> TokenEnd then expect('<end>');
        types.addEntry(Entry);
    end
    else begin
        while Token = TokenAddrAt do begin
            TypeName := TypeName + TokenValue;
            analyzer;
        end;
        TypeName := TypeName + TokenValue;
        if not types.have_type(TypeName) then
            abort('Type name ' + '''' + TypeName + '''' + ' does not exist!');
        if isaddrat(TypeName[1]) then begin
            types.add(AliasName, 2, 0);
            exit;
        end;
        Entry := types.retrieve_type(TypeName);
        Entry.setName(AliasName);
        types.addEntry(Entry);
    end;
end;
(*-----------------------------------------*)
procedure AnalyzeGlobalTypeDecl;
begin
    AnalyzeGlobalTypeIter;
    analyzer;
    while Token = TokenComma do begin
        AnalyzeGlobalTypeIter;
        analyzer;
    end;

    if Token <> TokenTerminator then expect('<terminator>');
    analyzer;
end;
(*-----------------------------------------*)
procedure AnalyzeFunctionDecl;
var
    FunctionName: string;
    locals: LocalTable;
    ReturnType: string;
    ReturnSize: integer;

    TempRead: Text;
    TempWrite: Text;
    FileLine: string;
    LineCount: integer;
begin
    locals.init;
    SP := 0;

    analyzer;
    if Token <> TokenSimpleIdent then expect('<simple-ident>');
    FunctionName := TokenValue;

    writeln(LaotzusTemp, FunctionName, ':');
    writeln(LaotzusTemp, TAB, 'push bp');
    writeln(LaotzusTemp, TAB, 'mov bp, sp');
    writeln(LaotzusTemp, TAB, 'nop');

    routines.add(FunctionName, '');
    AnalyzeParamsDecl(@locals);
    if Token <> TokenColon then expect('<colon>');

    analyzer;
    ReturnType := TokenValue;
    if not types.have_type(ReturnType) then expect('type-name');
    routines.setReturn(FunctionName, ReturnType);

    ReturnSize := retrieveTypeSize(ReturnType);

    // consider before the parameter decl
    // that we've already pushed a pointer which points to the return value.
    locals.add(FunctionName, '@' + ReturnType, False, routines.retrieve_topBP);
    routines.addParam('@' + ReturnType);

    analyzer;
    if Token <> TokenTerminator then expect('<terminator>');

    analyzer;
    if islocaldecl(Token) then AnalyzeLocalDecls(@locals);

    if Token <> TokenBegin then expect('<begin>');

    AnalyzeGeneralExpression(FunctionName, @locals);
    while Token = TokenTerminator do AnalyzeGeneralExpression(FunctionName, @locals);

    if Token <> TokenEnd then expect('<end>');
    analyzer;
    if Token <> TokenTerminator then expect('<terminator>');
    analyzer;

    writeln(LaotzusTemp, '.exit');
    writeln(LaotzusTemp, TAB, 'lea ax, [bp + ', routines.retrieve_topBP - 2, ']');
    writeln(LaotzusTemp, TAB, 'mov sp, bp');
    writeln(LaotzusTemp, TAB, 'pop bp');
    writeln(LaotzusTemp, TAB, 'ret');

    close(LaotzusTemp);

    assign(TempRead, FileNameTemp);
    reset(TempRead);
    if IOResult <> 0 then begin
        close(TempRead);
        writeln('Failed to open the file', FileNameTemp);
        exit;
    end;
    assign(TempWrite, FileNameOut);
    rewrite(TempWrite);
    if IOResult <> 0 then begin
        close(TempWrite);
        writeln('Failed to open the file', FileNameOut);
        exit;
    end;

    while not eof(TempRead) do
    begin
        readln(TempRead, FileLine);
        if pos(FunctionName, FileLine) > 0 then
        begin
            writeln(TempWrite, FileLine);
            for LineCount := 1 to 2 do
            begin
                readln(TempRead, FileLine);
                writeln(TempWrite, FileLine);
            end;
            readln(TempRead, FileLine);
            writeln(TempWrite, TAB, 'sub sp, ', SP)
        end
        else
        begin
            writeln(TempWrite, FileLine);
        end;
    end;

    close(TempRead);
    close(TempWrite);

    DeleteFile(FileNameTemp);
    Rename(TempWrite, FileNameTemp);

    assign(LaotzusTemp, FileNameTemp);
    append(LaotzusTemp);


end;
(*-----------------------------------------*)
procedure AnalyzeGlobalDecls;
begin
    analyzer;
    while isglobaldecl(Token) do
    begin
        case Token of
            TokenVar:
            begin
                AnalyzeGlobalVarDecl;
            end;
            TokenVal:
            begin
                AnalyzeGlobalValDecl;
            end;
            TokenType:
            begin
                AnalyzeGlobalTypeDecl;
            end;
            TokenProcedure:
            begin
                AnalyzeProcedureDecl;
            end;
            TokenFunction:
            begin
                AnalyzeFunctionDecl;
            end;
        end;
    end;
end;
(*-----------------------------------------*)
procedure AnalyzeProgramName;
begin
    analyzer; { retrieve the program name... }
    if Token <> TokenSimpleIdent then expect('valid program name token');
    ProgramName := TokenValue;
    writeln(LaotzusTemp, TAB, 'jmp ', ProgramName);
    routines.add(ProgramName, '');

    analyzer; { jump through the terminators... }
    if Token <> TokenTerminator then expect('<terminator>');
end;
(*-----------------------------------------*)
procedure AnalyzeHeader;
begin
    case Token of
        TokenProgram: begin
            AnalyzeProgramName;
            AnalyzeGlobalDecls;
        end;
        TokenUnit: begin
        end;
        TokenProject: begin
        end;
        TokenPackage: begin
        end;
        TokenModule: begin
        end;
    else begin
        abort('Expecting a program header token!');
    end;
    end;
end;
(*-----------------------------------------*)
procedure Init;
begin
    variables.init;

    routines.init;

    types.init;
    types.add('i8', 1, 0);
    types.add('i16', 2, 0);
    types.add('u8', 1, 0);
    types.add('u16', 2, 0);
    types.add('char', 1, 0);

    ProgramName := '';

    LineNr      := 1;
    ColumnNr    := 0;

    if ParamCount <> 1 then begin
        writeln('Usage: ', ParamStr(0), '<filename>');
        halt(1);
    end;

    FileNameIn := ParamStr(1);
    if LowerCase(ExtractFileExt(FileNameIn)) ='.dao' then begin
        FileNameTemp := ChangeFileExt(FileNameIn, '.tmp');
        FileNameOut := ChangeFileExt(FileNameIn, '.asm');
    end
    else begin
        writeln('Error: input file name should be ended with ''.dao''!');
        halt(1);
    end;

    assign(LaotzusIn, FileNameIn);
    reset(LaotzusIn);

    assign(LaotzusTemp, FileNameTemp);
    rewrite(LaotzusTemp);

    lookahead;
    analyzer;
end;
(*-----------------------------------------*)
procedure Fin;
begin
    close(LaotzusIn);
    close(LaotzusTemp);
end;
(*-----------------------------------------*)
begin
    Init;
    AnalyzeHeader;
    AnalyzeBody;
    Fin;
end.
(*-----------------------------------------*)
