# 利用clang前端让basilisk扩展到C++调研

>本调研旨在评估basilisk的词法语法的分析过程。将其与clang的词法语法分析代码进行对比，从而对basilisk的迁移进行评估。

- [利用clang前端让basilisk扩展到C++调研](#利用clang前端让basilisk扩展到c调研)
  - [1. 词法分析](#1-词法分析)
    - [1.1 对数字识别的差别（如int类型和float类型）](#11-对数字识别的差别如int类型和float类型)
    - [1.2 开头相同的运算符解析的差别（如"%="和"%"）](#12-开头相同的运算符解析的差别如和)
    - [1.3 对普通的Identify的处理](#13-对普通的identify的处理)
    - [1.4 对特殊字符串的处理（如int,float等）](#14-对特殊字符串的处理如intfloat等)


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

### 1.4 对特殊字符串的处理（如int,float等）
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

