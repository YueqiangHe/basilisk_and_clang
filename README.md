# 利用clang前端让basilisk扩展到C++调研

>本调研旨在评估basilisk的词法语法的分析过程。将其与clang的词法语法分析代码进行对比，从而对basilisk的迁移进行评估。

- [利用clang前端让basilisk扩展到C++调研](#利用clang前端让basilisk扩展到c调研)
  - [1. 词法分析](#1-词法分析)
      - [1.1. 对数字识别的差别](#11-对数字识别的差别)
      - [1.2. 开头相同的运算符解析的差别](#12-开头相同的运算符解析的差别)


## 1. 词法分析
**参考：basilisk参考basilisk/src/ast/tokens.lex，clang参考[lexer.cpp](https://clang.llvm.org/doxygen/Lexer_8cpp_source.html)**
#### 1.1. 对数字识别的差别

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
```c
  // C99 6.4.4.1: Integer Constants.
  // C99 6.4.4.2: Floating Constants.
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    // Notify MIOpt that we read a non-whitespace/non-comment token.
    MIOpt.ReadToken();
    return LexNumericConstant(Result, CurPtr);
```

#### 1.2. 开头相同的运算符解析的差别

basilisk中对单个字符的运算符与多个字符的运算符采用穷举法进行词法解析，而clang是对所有第一个字符相同的运算符相同进行词法分析。\（eg.如下图所示，basilisk的词法解析对"%="和"%"分为两种情况进行考虑，而clang对所有以"%"为开头的字符进行词法分析。其余也同理）\
**basilisk:**
```lex
"%="					{ SAST(MOD_ASSIGN); }
"%"					{ CAST(); }
```
**clang:**
```c
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

