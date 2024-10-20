# 利用clang前端让basilisk扩展到C++调研

>本调研旨在评估basilisk的词法语法的分析过程。将其与clang的词法语法分析代码进行对比，从而对basilisk的迁移进行评估。

- [利用clang前端让basilisk扩展到C++调研](#利用clang前端让basilisk扩展到c调研)
  - [1. 词法分析](#1-词法分析)
    - [1.1 对数字识别的差别（如int类型和float类型）](#11-对数字识别的差别如int类型和float类型)
    - [1.2 开头相同的运算符解析的差别（如"%="和"%"）](#12-开头相同的运算符解析的差别如和)
    - [1.3 对普通的Identify的处理](#13-对普通的identify的处理)
    - [1.4 对特殊Identify的处理（如int,float等）](#14-对特殊identify的处理如intfloat等)
    - [1.5 对于字符串的分析方式](#15-对于字符串的分析方式)
    - [1.6 OMP(OpenMP)](#16-ompopenmp)
    - [1.7 file\_line（文件行号指示器，例如 `#line 42 "example.c"`）](#17-file_line文件行号指示器例如-line-42-examplec)
    - [1.8 宏定义（@def）](#18-宏定义def)
    - [1.9 foreach与foreach\_（basilisk中的iterators）](#19-foreach与foreach_basilisk中的iterators)
    - [1.10 部分多个词的词法分析( new vertex scalar , new face vertor , new symmetric tensor , vertex scalar , face vertor , symmetric tensor)](#110-部分多个词的词法分析-new-vertex-scalar--new-face-vertor--new-symmetric-tensor--vertex-scalar--face-vertor--symmetric-tensor)
  - [2. 语法分析](#2-语法分析)
    - [2.1 external\_declaration](#21-external_declaration)
      - [2.1.1 basilisk的扩展](#211-basilisk的扩展)
      - [2.1.2 处理function\_definition和declaration的差别](#212-处理function_definition和declaration的差别)
    - [2.2 declaration(static\_assert\_declaration)](#22-declarationstatic_assert_declaration)
    - [2.3 function\_definition](#23-function_definition)
    - [2.4 basilisk 扩展](#24-basilisk-扩展)
      - [2.4.1 关系图](#241-关系图)


## 1. 词法分析
**参考：basilisk参考basilisk/src/ast/tokens.lex，clang参考[lexer.cpp](https://clang.llvm.org/doxygen/Lexer_8cpp_source.html)**
### 1.1 对数字识别的差别（如int类型和float类型）

basilisk中的数字识别分为float和integer，而clang中的数字识别是对所有的数字进行识别\
**basilisk:**
```lex
{HP}{H}+{IS}?				{ SAST(I_CONSTANT); }
{NZ}{D}*{IS}?				{ SAST(I_CONSTANT); }
"0"{O}*{IS}?				{ SAST(I_CONSTANT); }
{CP}?"'"([^'\\\n]|{ES})+"'"		{ SAST(I_CONSTANT); }

{D}+{E}{FS}?				{ SAST(F_CONSTANT); }
{D}*"."{D}+{E}?{FS}?			{ SAST(F_CONSTANT); }
{D}+"."{E}?{FS}?			{ SAST(F_CONSTANT); }
{HP}{H}+{P}{FS}?			{ SAST(F_CONSTANT); }
{HP}{H}*"."{H}+{P}{FS}?			{ SAST(F_CONSTANT); }
{HP}{H}+"."{P}{FS}?			{ SAST(F_CONSTANT); }
```
**clang:**
```cpp
  // C99 6.4.4.1: Integer Constants.
  // C99 6.4.4.2: Floating Constants.
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    // Notify MIOpt that we read a non-whitespace/non-comment token.
    MIOpt.ReadToken();
    return LexNumericConstant(Result, CurPtr);
```

### 1.2 开头相同的运算符解析的差别（如"%="和"%"）

basilisk中对单个字符的运算符与多个字符的运算符采用穷举法进行词法解析，而clang是对所有第一个字符相同的运算符相同进行词法分析。\
（eg.如下图所示，basilisk的词法解析对"%="和"%"分为两种情况进行考虑，而clang对所有以"%"为开头的字符进行词法分析。其余也同理）\
**basilisk:**
```lex
"%="					{ SAST(MOD_ASSIGN); }
"%"					{ CAST(); }
```
**clang:**
```cpp
case '%':
    Char = getCharAndSize(CurPtr, SizeTmp);
    if (Char == '=') {
      Kind = tok::percentequal;
      CurPtr = ConsumeChar(CurPtr, SizeTmp, Result);
    } else if (LangOpts.Digraphs && Char == '>') {
      Kind = tok::r_brace;                             // '%>' -> '}'
      CurPtr = ConsumeChar(CurPtr, SizeTmp, Result);
    } else if (LangOpts.Digraphs && Char == ':') {
      CurPtr = ConsumeChar(CurPtr, SizeTmp, Result);
      Char = getCharAndSize(CurPtr, SizeTmp);
      if (Char == '%' && getCharAndSize(CurPtr+SizeTmp, SizeTmp2) == ':') {
        Kind = tok::hashhash;                          // '%:%:' -> '##'
        CurPtr = ConsumeChar(ConsumeChar(CurPtr, SizeTmp, Result),
                             SizeTmp2, Result);
      } else if (Char == '@' && LangOpts.MicrosoftExt) {// %:@ -> #@ -> Charize
        CurPtr = ConsumeChar(CurPtr, SizeTmp, Result);
        if (!isLexingRawMode())
          Diag(BufferPtr, diag::ext_charize_microsoft);
        Kind = tok::hashat;
      } else {                                         // '%:' -> '#'
        // We parsed a # character.  If this occurs at the start of the line,
        // it's actually the start of a preprocessing directive.  Callback to
        // the preprocessor to handle it.
        // TODO: -fpreprocessed mode??
        if (TokAtPhysicalStartOfLine && !LexingRawMode && !Is_PragmaLexer)
          goto HandleDirective;
 
        Kind = tok::hash;
      }
    } else {
      Kind = tok::percent;
    }
    break;
```

### 1.3 对普通的Identify的处理

basilisk是直接对已经构成的AST进行检索，而clang是对维护的表进行检索

**basilisk:**
basilisk对于特殊字符直接进行特殊处理。
```cpp
static int check_type (AstRoot * parse)
{
  if (parse->type_already_specified)
    return IDENTIFIER;
  
  Ast * declaration = ast_identifier_declaration (parse->stack, yytext);
  if (declaration) {
    if (ast_is_typedef (declaration))
      return TYPEDEF_NAME;
    return IDENTIFIER;
  }

  return IDENTIFIER;
}
```
**clang:**
clang通过一个符号表（symbol table）进行查询
```cpp
case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
  case 'H': case 'I': case 'J': case 'K':    /*'L'*/case 'M': case 'N':
  case 'O': case 'P': case 'Q':    /*'R'*/case 'S': case 'T':    /*'U'*/
  case 'V': case 'W': case 'X': case 'Y': case 'Z':
  case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
  case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
  case 'o': case 'p': case 'q': case 'r': case 's': case 't':    /*'u'*/
  case 'v': case 'w': case 'x': case 'y': case 'z':
  case '_':
    // Notify MIOpt that we read a non-whitespace/non-comment token.
    MIOpt.ReadToken();
    return LexIdentifierContinue(Result, CurPtr);
```
Clang 中的标识符解析函数 LexIdentifierContinue 解析出标识符的整个内容后，会将该标识符交给预处理器中的 标识符表（identifier table） 进行查找：`const IdentifierInfo *II = PP->LookUpIdentifierInfo(Result)`;\
`LookUpIdentifierInfo`(Result) 会查找当前标识符是否是关键字或者是是否为已经定义的字符。

### 1.4 对特殊Identify的处理（如int,float等）
basilisk对于每个特殊字符有单独匹配的词法分析，而clang直接对Identify进行处理，对于特殊字符（如int,float）这些标准 C/C++ 关键字会被预先加入到符号表中。Clang 通过一个称为 IdentifierTable 的结构来管理所有的标识符。这个表不仅包含变量名、函数名，还包含所有的关键字，如 int、float 等。

**basilisk:**
这些特殊的字符都有单独匹配的词法分析。
```lex
"auto"					{ SAST(AUTO); }
"break"					{ SAST(BREAK); }
"case"					{ SAST(CASE); }
"char"					{ SAST(CHAR); }
"const"					{ SAST(CONST); }
"continue"				{ SAST(CONTINUE); }
"default"				{ SAST(DEFAULT); }
"do"					{ SAST(DO); }
"double"				{ SAST(DOUBLE); }
"else"					{ SAST(ELSE); }
"enum"					{ SAST(ENUM); }
"extern"				{ SAST(EXTERN); }
"float"					{ SAST(FLOAT); }
"for"					{ SAST(FOR); }
"goto"					{ SAST(GOTO); }
"if"					{ SAST(IF); }
"inline"				{ SAST(INLINE); }
"int"					{ SAST(INT); }
"long"					{ SAST(LONG); }
"register"				{ SAST(REGISTER); }
"restrict"				{ SAST(RESTRICT); }
"return"				{ SAST(RETURN); }
"short"					{ SAST(SHORT); }
"signed"				{ SAST(SIGNED); }
"sizeof"				{ SAST(SIZEOF); }
"static"				{ SAST(STATIC); }
"struct"				{ SAST(STRUCT); }
"switch"				{ SAST(SWITCH); }
"typedef"				{ SAST(TYPEDEF); }
"union"					{ SAST(UNION); }
"unsigned"				{ SAST(UNSIGNED); }
"void"					{ SAST(VOID); }
"volatile"				{ SAST(VOLATILE); }
"while"					{ SAST(WHILE); }
......
```
**clang:**\
当 Clang 的词法分析器遇到类似 int、float 这样的关键字时，它会调用 LookUpIdentifierInfo() 函数，查询符号表中的条目。这个函数会返回一个 IdentifierInfo 对象，该对象包含标识符的相关信息，如它是否是一个关键字、是否是 typedef、是否是宏等。(见[1.3 对普通的Identify的处理](#13-对普通的identify的处理))

### 1.5 对于字符串的分析方式
总体来说，在字符串分析方面两者分析步骤差异不大\
但是basilisk通过在字符串中标注u/U/L等来表示字符编码方式，对整体的字符串进行分析，而clang是读入前缀如u/U/L，再根据下一个字符是否为"进行分析\
**basilisk:**\
前缀种类 包括 u8、u、U 和 L，它们分别表示 UTF-8、UTF-16、UTF-32 和宽字符字符串字面量。
```lex
SP  (u8|u|U|L)
({SP}?\"([^"\\\n]|{ES})*\"{WS}*)+	{ SAST(STRING_LITERAL); }
```
**clang:**\
clang只对以"开头的字符串进行单独举例，其余都是在以前缀为开头的字符串的case里进行分析,作为一个参数在`bool Lexer::LexStringLiteral(Token &Result, const char *CurPtr,
                             tok::TokenKind Kind)`的最后一个参数中显示。\
eg:
```cpp
  // Identifier (e.g., uber), or
  // UTF-8 (C23/C++17) or UTF-16 (C11/C++11) character literal, or
  // UTF-8 or UTF-16 string literal (C11/C++11).
  case 'u':
    // Notify MIOpt that we read a non-whitespace/non-comment token.
    MIOpt.ReadToken();
 
    if (LangOpts.CPlusPlus11 || LangOpts.C11) {
      Char = getCharAndSize(CurPtr, SizeTmp);
 
      // UTF-16 string literal
      if (Char == '"')
        return LexStringLiteral(Result, ConsumeChar(CurPtr, SizeTmp, Result),
                                tok::utf16_string_literal);

 
      if (Char == '8') {
        char Char2 = getCharAndSize(CurPtr + SizeTmp, SizeTmp2);
 
        // UTF-8 string literal
        if (Char2 == '"')
          return LexStringLiteral(Result,
                               ConsumeChar(ConsumeChar(CurPtr, SizeTmp, Result),
                                           SizeTmp2, Result),
                               tok::utf8_string_literal);

  case 'U': // Identifier (e.g. Uber) or C11/C++11 UTF-32 string literal
    // Notify MIOpt that we read a non-whitespace/non-comment token.
    MIOpt.ReadToken();
 
    if (LangOpts.CPlusPlus11 || LangOpts.C11) {
      Char = getCharAndSize(CurPtr, SizeTmp);
 
      // UTF-32 string literal
      if (Char == '"')
        return LexStringLiteral(Result, ConsumeChar(CurPtr, SizeTmp, Result),
                                tok::utf32_string_literal);
 
      // UTF-32 character constant
      if (Char == '\'')
        return LexCharConstant(Result, ConsumeChar(CurPtr, SizeTmp, Result),
                               tok::utf32_char_constant);
 
      // UTF-32 raw string literal
      if (Char == 'R' && LangOpts.RawStringLiterals &&
          getCharAndSize(CurPtr + SizeTmp, SizeTmp2) == '"')
        return LexRawStringLiteral(Result,
                               ConsumeChar(ConsumeChar(CurPtr, SizeTmp, Result),
                                           SizeTmp2, Result),
                               tok::utf32_string_literal);
    }
```

### 1.6 OMP(OpenMP)
在basilisk的GPU中找到下面一段话：\
`Note that this could even be implemented just as a new definition of the macros OMP_PARALLEL(), OMP() and OMP_END_PARALLEL(). However, as pointed out in the blog, without some control of data layout, performance would probably be terrible.`\
猜测OMP是用于并行计算的函数，因此对于OMP的解析只需要检查括号的配对，但是不能用正规式来表达括号配对，
于是在Lex中写了一个函数进行检验，这样就实现了对以OMP为开头的这些函数的分析。
```lex
^[ \t]*OMP[ \t]*\(	                { ompreproc(); }
"__attribute__"{WS}*\(                  { ompreproc(); }

static void ompreproc (void)
{
  int c, scope = 1;
  while ((c = input()) != 0) {
    if (c == '(')
      scope++;
    else if (c == ')') {
      scope--;
      if (scope == 0)
	return;
    }
  }
  //  yyerror ("unterminated OMP");
}
```
相比于 Clang 对 OpenMP 的复杂处理，Basilisk C 采用了一种更简化的方式，可能basilisk只有这样的一种写法。


### 1.7 file_line（文件行号指示器，例如 `#line 42 "example.c"`）
basilisk中对行号指示器处理的比较简单，file_line() 函数解析这些行号指示器，并将行号和文件名信息存储到相应的结构中。这个机制在处理预处理器生成的文件时非常有用，因为它允许跟踪文件的源代码位置，特别是在宏展开或文件包含的情况下。\
```lex
^[ \t]*#[ \t]+[0-9]+[ \t]+{STRING}.*    { file_line (parse, yytext); }

static void file_line (AstRoot * parse, const char * text)
{
  char * s = strchr (text, '#') + 1;
  yylineno = atoi(s) - 1;
  s = strchr (s, '"') + 1;
  char * end = strchr (s, '"');
  parse->file = allocate (parse->alloc, end - s + 1);
  strncpy ((char *) parse->file, s, end - s);
  //  fprintf (stderr, "%s: \"%s\" %d\n", text, file, yylineno);
}
```

### 1.8 宏定义（@def）
在 Basilisk C 中，使用 @def 来定义复杂的宏结构。最后需要检测`@def ... @`。\
因此Lex中的检测部分是这样的：
```lex
^[ \t]*@[ \t]*def[ \t].*                { bpreproc(); }

static void bpreproc (void)
{
  int c;
  while ((c = input()) != 0)
    if (c == '@')
      return;
  //  yyerror ("unterminated @def");
}
```
这与C语言中定义宏的方式不同，Clang 的预处理器处理 #define、#include、#pragma 等标准预处理指令，而不像 Basilisk C 通过 @def 这样的自定义语法来处理预处理指令。

### 1.9 foreach与foreach_（basilisk中的iterators）
Basilisk C 的 foreach 是一种特定于该语言的控制结构，用于遍历网格中的单元、维度或邻居。在 Basilisk C 的词法分析器中，通过正则表达式识别不同形式的 foreach，然后根据不同的遍历需求生成相应的 Token。\
这与clang的词法分析不同，clang对所有首字母相同的进行分析，再进行分类，而且clang没有以foreach_{L}{A}*(foreach_+Identify)的用法。倘若有也只有类似`foreach Identify`的用法，clang不会特意为特定前缀（如 foreach_）做专门的处理。\
**basilisk:**
```lex
"foreach_blockf" |
"foreach_block" |
"foreach_child" |
"foreach_neighbor"                      { SAST(FOREACH_INNER); }

"foreach_dimension"			{ SAST(FOREACH_DIMENSION); }

"foreach" |
"foreach_"{L}{A}*                       { SAST(FOREACH); }
```
**clang:**\
clang中只有对以f开头的标识符进行整体分析。同时并没有同一前缀进行专门处理。
clang的Indentify处理函数`bool Lexer::LexIdentifierContinue(Token &Result, const char *CurPtr)`中只有对特殊的Identify进行识别。
```cpp
// C99 6.4.2: Identifiers.
  case 'A': case 'B': case 'C': case 'D': case 'E': case 'F': case 'G':
  case 'H': case 'I': case 'J': case 'K':    /*'L'*/case 'M': case 'N':
  case 'O': case 'P': case 'Q':    /*'R'*/case 'S': case 'T':    /*'U'*/
  case 'V': case 'W': case 'X': case 'Y': case 'Z':
  case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g':
  case 'h': case 'i': case 'j': case 'k': case 'l': case 'm': case 'n':
  case 'o': case 'p': case 'q': case 'r': case 's': case 't':    /*'u'*/
  case 'v': case 'w': case 'x': case 'y': case 'z':
  case '_':
    // Notify MIOpt that we read a non-whitespace/non-comment token.
    MIOpt.ReadToken();
    return LexIdentifierContinue(Result, CurPtr);
```

### 1.10 部分多个词的词法分析( new vertex scalar , new face vertor , new symmetric tensor , vertex scalar , face vertor , symmetric tensor)
basilisk通过正则表达式对这些进行识别，而clang在词法分析只是单独处理每个词(利用switch case对每个词的首字母进行分别处理)，在语法分析部分再整体处理这些词。\
这与basilisk的组合少，clang的new组合多有关。\
**basilisk:**
```lex
"new"{WS}+("vertex"{WS}+)?"scalar"      { SAST(NEW_FIELD); }
"new"{WS}+("face"{WS}+)?"vector"        { SAST(NEW_FIELD); }
"new"{WS}+("symmetric"{WS}+)?"tensor"   { SAST(NEW_FIELD); }
"vertex"{WS}+"scalar"                   { SAST(TYPEDEF_NAME); }
"face"{WS}+"vector"                     { SAST(TYPEDEF_NAME); }
"symmetric"{WS}+"tensor"                { SAST(TYPEDEF_NAME); }
```

## 2. 语法分析
对basilisk主要参考文件为basilisk/src/ast/yacc，对clang主要参考文件为[Parse.cpp](https://clang.llvm.org/doxygen/Parse_2Parser_8cpp_source.html)

### 2.1 external_declaration
#### 2.1.1 basilisk的扩展
相比于clang的external_declaration,basilisk的一些解析更加简单，同时也加上了一些basilisk c特殊的部分。
**basilisk:**
```yacc
  external_declaration
	: function_definition
	| declaration
	| macro_statement /* Basilisk C extension */
	| event_definition /* Basilisk C extension */
	| boundary_definition /* Basilisk C extension */
	| external_foreach_dimension /* Basilisk C extension */
	| attribute /* Basilisk C extension */
	| error compound_statement              { $1->sym = YYSYMBOL_YYerror; }
	;
```
#### 2.1.2 处理function_definition和declaration的差别
在external_declaration中，clang并没有把二者分开处理，而basilisk把二者分开识别处理。\
最后在`Parser::DeclGroupPtrTy Parser::ParseDeclOrFunctionDefInternal`中通过识别tokens来识别不同。\
**basilisk:**
```yacc
external_declaration
	: function_definition
	| declaration
```
**clang:**
```cpp
// We can't tell whether this is a function-definition or declaration yet.
    if (!SingleDecl)
      return ParseDeclarationOrFunctionDefinition(Attrs, DeclSpecAttrs, DS);
  }
```

### 2.2 declaration(static_assert_declaration)
在declaration中，basilisk把static_assert_declaration加入语法分析，而clang没有。\
clang对static_assert_declaration的定义在`Parser::ParseExternalDeclaration(ParsedAttributes &Attrs,
                                 ParsedAttributes &DeclSpecAttrs,
                                 ParsingDeclSpec *DS)`中。\
**basilisk:**
```yacc
declaration
        : declaration_specifiers ';' type_not_specified                        { ast_push_declaration (parse->stack, $$); }
	| declaration_specifiers init_declarator_list ';' type_not_specified   { ast_push_declaration (parse->stack, $$); }
	| static_assert_declaration
	;
```
**clang:**
```cpp
///       declaration: [C99 6.7]
///         declaration-specifiers init-declarator-list[opt] ';'
```
```cpp
Parser::ParseExternalDeclaration(ParsedAttributes &Attrs,
                                 ParsedAttributes &DeclSpecAttrs,
                                 ParsingDeclSpec *DS) {
  DestroyTemplateIdAnnotationsRAIIObj CleanupRAII(*this);
  ParenBraceBracketBalancer BalancerRAIIObj(*this);
 
  if (PP.isCodeCompletionReached()) {
    cutOffParsing();
    return nullptr;
  }
 
  Decl *SingleDecl = nullptr;
  switch (Tok.getKind()) {
  ......
  case tok::kw_static_assert:
  case tok::kw__Static_assert:
    // A function definition cannot start with any of these keywords.
    {
      SourceLocation DeclEnd;
      return ParseDeclaration(DeclaratorContext::File, DeclEnd, Attrs,
                              DeclSpecAttrs);
    }
```

### 2.3 function_definition
basilisk和clang的语法分析不同，见下：\
**basilisk:**
```yacc
function_declaration
        : declaration_specifiers declarator { ast_push_function_definition (parse->stack, $2);  }
	;
	
function_definition
        : function_declaration declaration_list compound_statement
```
**clang:**
```cpp
///       function-definition: [C99 6.9.1]
///         decl-specs      declarator declaration-list[opt] compound-statement
```
可见，basilisk把declaration_specifiers declarator合并成function_declaration处理，是为了方便对AST栈进行操作。\
而clang并没有把这两个语法集中处理，而是直接使用decl-specs declarator这一语法。


### 2.4 basilisk 扩展

#### 2.4.1 关系图
下面是Basilisk C grammar extensions 的语法关系图。
```plantuml
@startuml
class basilisk_statements {
    +macro_statement
    +foreach_statement
    +foreach_inner_statement
    +foreach_dimension_statement
    +forin_declaration_statement
    +forin_statement
}

class macro_statement {
    +function_call
    +compound_statement
}

class foreach_statement {
    +FOREACH '(' ')'
    +FOREACH '(' foreach_parameters ')'
}

class foreach_parameters {
    +foreach_parameter
    +foreach_parameters ',' foreach_parameter
}

class foreach_parameter {
    +assignment_expression
    +reduction_list
}

class reduction_list {
    +reduction
    +reduction_list
}

class reduction {
    +REDUCTION '(' reduction_operator ':' reduction_array ')'
}

class reduction_operator {
    +generic_identifier
    +'+'
    +OR_OP
}

class reduction_array {
    +generic_identifier
    +generic_identifier '[' ':' expression ']'
}

class foreach_inner_statement {
    +FOREACH_INNER '(' ')'
    +FOREACH_INNER '(' expression ')'
}

class foreach_dimension_statement {
    +FOREACH_DIMENSION '(' ')'
    +FOREACH_DIMENSION '(' I_CONSTANT ')'
}

class forin_declaration_statement {
    +for_scope '(' declaration_specifiers declarator IDENTIFIER forin_arguments ')'
}

class forin_statement {
    +for_scope '(' expression IDENTIFIER forin_arguments ')'
}

class forin_arguments {
    +expression
    +postfix_initializer
}

class event_definition {
    +generic_identifier generic_identifier '(' event_parameters ')' statement
}

class event_parameters {
    +event_parameter
    +event_parameters
    +event_parameters ';' event_parameter
}

class event_parameter {
    +conditional_expression
    +unary_expression assignment_operator conditional_expression
    +unary_expression assignment_operator postfix_initializer
}

class boundary_definition {
    +assignment_expression ';'
}

class external_foreach_dimension {
    +FOREACH_DIMENSION '(' ')' function_definition
    +FOREACH_DIMENSION '(' I_CONSTANT ')' function_definition
}

class attribute {
    +generic_identifier '{' struct_declaration_list '}'
}

class new_field {
    +NEW_FIELD
    +NEW_FIELD '[' postfix_expression ']'
}

basilisk_statements --> macro_statement
basilisk_statements --> foreach_statement
basilisk_statements --> foreach_inner_statement
basilisk_statements --> foreach_dimension_statement
basilisk_statements --> forin_declaration_statement
basilisk_statements --> forin_statement

macro_statement --> function_call
macro_statement --> compound_statement

foreach_statement --> FOREACH
foreach_statement --> foreach_parameters

foreach_parameters --> foreach_parameter
foreach_parameters --> foreach_parameters

foreach_parameter --> assignment_expression
foreach_parameter --> reduction_list

reduction_list --> reduction
reduction_list --> reduction_list

reduction --> reduction_operator
reduction --> reduction_array

reduction_operator --> generic_identifier
reduction_operator --> "+"
reduction_operator --> OR_OP

reduction_array --> generic_identifier
reduction_array --> generic_identifier

foreach_inner_statement --> FOREACH_INNER
foreach_inner_statement --> expression

foreach_dimension_statement --> FOREACH_DIMENSION
foreach_dimension_statement --> I_CONSTANT

forin_declaration_statement --> for_scope
forin_statement --> for_scope

forin_arguments --> expression
forin_arguments --> postfix_initializer

event_definition --> generic_identifier
event_definition --> event_parameters

event_parameters --> event_parameter
event_parameters --> event_parameters

event_parameter --> conditional_expression
event_parameter --> unary_expression
event_parameter --> postfix_initializer

external_foreach_dimension --> function_definition
@enduml

```

<!-- Gitalk 评论 start -->
<link rel="stylesheet" href="https://unpkg.com/gitalk/dist/gitalk.css">
<script src="https://unpkg.com/gitalk@latest/dist/gitalk.min.js"></script> 
<div id="gitalk-container"></div>     
<script type="text/javascript">
    var gitalk = new Gitalk({
        clientID: `Ov23lihfIHMzkOPtIzWI`,
        clientSecret: `a7478f71d01fa9bd0bb585ebbd31e3d085ddbecb`,
        repo: `basilisk_and_clang`,
        owner: 'YueqiangHe',
        admin: ['YueqiangHe'], 
        id: location.pathname, 
        distractionFreeMode: false  
    });
    gitalk.render('gitalk-container');
</script> 
<!-- Gitalk end -->

