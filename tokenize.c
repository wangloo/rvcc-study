#include "rvcc.h"

// 输入的文件名
char *CurrentFilename;
// 输入的字符串
char *CurrentInput;

void error(char *fmt, ...)
{
  va_list va;
  va_start(va, fmt);
  vfprintf(stderr, fmt, va);
  fprintf(stderr, "\n");
  va_end(va);
  // 终止程序
  exit(1);
}

// 输出错误出现的位置，并退出
// foo.c:10: x = y + 1;
//               ^ <错误信息>
static void verrorAt(int lineno, char *Loc, char *Fmt, va_list VA) {

  // 查找包含loc的行
  char *line = Loc;
  // line 递减到当前行的最开始位置
  // line<CurrentInput，判断是否读取到文件最开始的位置
  // line[-1] != '\n', line字符串前一个字符是否为换行符（上一行末尾）
  while (CurrentInput < line && line[-1] != '\n')
    line--;

  // end 递增到行尾的换行符
  char *end = Loc;
  while (*end != '\n')
    end++;

  // 输出 文件名:错误行
  // Indent记录输出了多少个字符
  int Indent = fprintf(stderr, "%s:%d: ", CurrentFilename, lineno);
  // 输出Line的行内所有字符（不含换行符）
  fprintf(stderr, "%.*s\n", (int)(end - line), line);
  // 计算错误信息位置，在当前行内的偏移量+前面输出了多少个字符
  int Pos = Loc - line + Indent;

  // 将字符串补齐为Pos位，因为是空字符串，所以填充Pos个空格。
  fprintf(stderr, "%*s", Pos, "");
  fprintf(stderr, "^ ");
  vfprintf(stderr, Fmt, VA);
  fprintf(stderr, "\n");
  va_end(VA);
}

// 字符解析出错
void errorAt(char *Loc, char *Fmt, ...) {
  // 获取行号
  int lineno = 1;
  for (char *p = CurrentInput; p < Loc; p++) {
    // 遇到换行符则行号+1
    if (*p == '\n')
      lineno++;
  }

  va_list VA;
  va_start(VA, Fmt);
  verrorAt(lineno, Loc, Fmt, VA);
  exit(1);
}

// Tok解析出错
void errorTok(Token *Tok, char *Fmt, ...) {
  va_list VA;
  va_start(VA, Fmt);
  verrorAt(Tok->lineno, Tok->loc, Fmt, VA);
  exit(1);
}

static long getnumber(Token *tok)
{
  if (tok->kind != TK_NUM)
    error("expect a number");
  return tok->val;
}
bool equal(Token *tok, char *str)
{
  return memcmp(tok->loc, str, tok->len) == 0 && str[tok->len] == 0;
}

// 跳过指定的Str
Token *skip(Token *tok, char *str)
{
  if (!equal(tok, str))
    errorTok(tok, "expect: %s\n", str);
  return tok->next;
}

// 消耗掉指定的Token
// 和skip差不多，但是对不存在的情况处理不同
bool consume(Token **rest, Token *tok, char *str)
{
  // 存在
  if (equal(tok, str)) {
    *rest = tok->next;
    return true;
  }
  // 不存在
  *rest = tok;
  return false;
}

static bool starts_with(char *str, char *substr) {
  return 0 == strncmp(str, substr, strlen(substr));
}

// 判断标记符首字母规则
// [a-zA-Z_]
static bool isident1(char c)
{
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c == '_');
}

// 判断标记符的非首字母的规则
// [a-zA-z0-9_]
static bool isident2(char c)
{
  return isident1(c) || (c >= '0' && c <= '9');
}

static bool iskeyword(Token *tok)
{
  char *KW[] = {"void",    "int",   "long", "char",  "struct", "union",
                "return",  "if",    "else", "for",   "while",  "sizeof",
                "typedef", "_Bool", "enum", "static"};
  for (int i = 0; i < sizeof(KW)/sizeof(*KW); i++) {
    if (equal(tok, KW[i]))
      return true;
  }
  return false;
}
// 将名为“return”的终结符转为KEYWORD
static void convert_keywords(Token *tok)
{
  for (Token *t = tok; t->kind != TK_EOF; t = t->next) {
    if (iskeyword(t)) {
      t->kind = TK_KEYWORD;
    }
  }
}

static void add_linenumbers(Token *tok) {
  char *p = CurrentInput;
  int n = 1;

  do {
    if (p == tok->loc) {
      tok->lineno = n;
      tok = tok->next;
    }
    if (*p == '\n')
      n++;
  } while (*p++);
}


static Token *newtoken(TokenKind kind, char *start)
{
  Token *tok = calloc(1, sizeof(Token));
  tok->kind = kind;
  tok->loc = start;
  return tok;
}

// 返回一位十六进制转十进制的结果
static int fromhex(char c)
{
  if ('0' <= c && c <= '9')
    return c - '0';
  if ('a' <= c && c <= 'f')
    return c - 'a' + 10;
  return c - 'A' + 10;
}

// 读取转义字符
static int read_escaped_char(char **newpos, char *p)
{
  if ('0' <= *p && *p <= '7') {
    // 读取一个八进制数字，不能长于三位
    // \abc = (a*8+b)*8+c
    int c = *p++ - '0';
    if ('0' <= *p && *p <= '7') {
      c = (c << 3) + (*p++ - '0');
      if ('0' <= *p && *p <= '7')
        c = (c << 3) + (*p++ - '0');
    }
    *newpos = p;
    return c;
  }

  if (*p == 'x') {
    p++;
    // 判断是否为十六进制数字
    if (!isxdigit(*p))
      errorAt(p, "invalid hex escape sequence");

    int c = 0;
    // 读取一位或多位十六进制数字
    // \xWXYZ = ((W*16+X)*16)+Y)*16+Z
    for (; isxdigit(*p); p++)
      c = (c << 4) + fromhex(*p);
    *newpos = p;
    return c;
  }

  *newpos = p+1;
  switch (*p) {
  case 'a': // 响铃（警报）
    return '\a';
  case 'b': // 退格
    return '\b';
  case 't': // 水平制表符, tab
    return '\t';
  case 'n': // 换行
    return '\n';
  case 'v': // 垂直制表符
    return '\v';
  case 'f': // 换页
    return '\f';
  case 'r': // 回车
    return '\r';
  // 属于GNU C拓展
  case 'e': // 转义符
    return 27;
  default:  // 默认将原字符返回
    return *p;
  }
}



// 读取到字符串字面量结尾
static char *string_literal_end(char *p)
{
  char *start = p;

  // 识别字符串内的所有非"字符
  for (; *p != '"'; ++p) {
    if (*p == '\n' || *p == '\0')
      errorAt(start, "unclosed string literal");
    if (*p == '\\')
      p++;
  }
  return p;
}

// 读取字符串字面量
static Token *read_string_literal(char *start)
{
  // 读取到字符串字面量的右引号
  char *end = string_literal_end(start+1);
  // 定义一个与字符串字面量内字符数+1的buf
  // 用来存储最大位数的字符串字面量
  char *buf = calloc(1, end - start);
  // 实际的字符数位，一个转义字符为1位
  int len = 0;

  // 将读取后的结果写入 buf
  for (char *p = start+1; p < end;) {
    if (*p == '\\') {
      buf[len++] = read_escaped_char(&p, p+1);
    } else {
      buf[len++] = *p++;
    }
  }

  Token *tok = newtoken(TK_STR, start);
  // tok->len 的作用是词法解析跳过，所以len包含两个双引号
  tok->len = end - start + 1;
  // 长度比 tok->str 多一个，存储\0
  tok->ty = arrayof(TyChar, len + 1);
  tok->str = buf;
  return tok;
}

static Token *read_char_literal(char *start) {
  char *p = start + 1;
  // 解析字符为 \0 的情况
  if (*p == '\0')
    errorAt(start, "unclosed char literal");

  // 解析字符
  char c;
  // 转义
  if (*p == '\\')
    c = read_escaped_char(&p, p + 1);
  else
    c = *p++;

  char *end = strchr(p, '\'');
  if (!end)
    errorAt(p, "unclosed char literal");

  // 构造一个NUM的终结符，值为C的数值
  Token *tok = newtoken(TK_NUM, start);
  // 长度多一个，存储\0
  tok->len = end - start + 1;
  tok->val = c;
  return tok;
}

static char *readfile(char *path)
{
  FILE *fp;

  if (strcmp(path, "-") == 0) {
    // 如果文件名是"-", 那么就从输入中读取
    fp = stdin;
  } else {
    fp = fopen(path, "r");
    if (!fp)
      // errno 为系统最后一次的错误代码
      // stderror以字符串的形式输出错误代码
      error("cannot open %s: %s", path, strerror(errno));
  }

  // 要返回的字符串
  char *buf;
  size_t buflen;
  FILE *out = open_memstream(&buf, &buflen);

  // 读取整个文件
  while (true) {
    char buf2[4096];
    // fread 从文件流中读取数据到数组中
    // 数组指针buf2，数组元素大小1，数组元素个数4096，文件流指针
    int n = fread(buf2, 1, sizeof(buf2), fp);
    if (n == 0)
      break;
    // 数组指针buf2，数组元素大小1，实际元素个数N，文件流指针
    fwrite(buf2, 1, n, out);
  }
  if (fp != stdin)
    fclose(fp);

  // 刷新流的输出缓冲区，确保内容都被输出到流中
  fflush(out);
  // 确保最后一行以'\n'结尾
  if (buflen == 0 || buf[buflen - 1] != '\n')
    // 将字符输出到流中
    fputc('\n', out);
  fputc('\0', out);
  fclose(out);
  return buf;
}

// 词法分析
Token *tokenize(char *filename, char *p)
{
  Token head = {};
  Token *cur = &head;

  CurrentFilename = filename;
  CurrentInput = p;
  while (*p) {
    // 跳过行注释
    if (starts_with(p, "//")) {
      p += 2;
      while (*p != '\n')
        p++;
      continue;
    }

    // 跳过块注释
    if (starts_with(p, "/*")) {
      char *q = strstr(p+2, "*/");
      if (!q)
        errorAt(p, "unclosing block comment");
      p = q + 2;
      continue;
    }

    if (isspace(*p)) {
      p++;
      continue;
    }
    if (isdigit(*p)) {
      cur->next = newtoken(TK_NUM, p);
      cur = cur->next;
      const char *oldp = p;
      cur->val = strtol(p, &p, 10);
      cur->len = p - oldp;
      continue;
    }

    // 解析字符串字面量
    if (*p == '"') {
      cur->next = read_string_literal(p);
      cur = cur->next;
      p += cur->len;
      continue;
    }

    // 解析字符字面量
    if (*p == '\'') {
      cur->next = read_char_literal(p);
      cur = cur->next;
      p += cur->len;
      continue;
    }

    // 解析标记符
    // [a-zA-Z_][a-zA-Z0-9_]*
    if (isident1(*p)) {
      char *start = p;
      do {
        p++;
      } while (isident2(*p));
      cur->next = newtoken(TK_IDENT,start);
      cur = cur->next;
      cur->len = p - start;
      continue;
    }

    if (*p == '=' && *(p+1) == '=') {
      cur->next = newtoken(TK_PUNCT, p);
      cur = cur->next;
      cur->len = 2;
      p += 2;
      continue;
    }
    if (*p == '-' && *(p+1) == '>') {
      cur->next = newtoken(TK_PUNCT, p);
      cur = cur->next;
      cur->len = 2;
      p += 2;
      continue;
    }
    if (*p == '!' && *(p+1) == '=') {
      cur->next = newtoken(TK_PUNCT, p);
      cur = cur->next;
      cur->len = 2;
      p += 2;
      continue;
    }
    if (*p == '<' && *(p+1) == '=') {
      cur->next = newtoken(TK_PUNCT, p);
      cur = cur->next;
      cur->len = 2;
      p += 2;
      continue;
    }
    if (*p == '>' && *(p+1) == '=') {
      cur->next = newtoken(TK_PUNCT, p);
      cur = cur->next;
      cur->len = 2;
      p += 2;
      continue;
    }
    if (ispunct(*p) || *p == ';') {
      cur->next = newtoken(TK_PUNCT, p);
      cur = cur->next;
      cur->len = 1;
      p++;
      continue;
    }

    // 处理无法识别的字符
    error("unexcepted character: '%c'\n", *p);
  }

  // 解析结束，增加一个EOF，表示终止符
  cur->next = newtoken(TK_EOF, p);

  // 为所有的Token添加行号
  add_linenumbers(head.next);
  // 将所有关键字的终结符，都标记为KEYWORD
  convert_keywords(head.next);
  return head.next;
}

// 对文件进行词法分析
Token *tokenize_file(char *path)
{
  return tokenize(path, readfile(path));
}