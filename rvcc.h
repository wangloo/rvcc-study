// 使用POSIX.1标准
// 使用了strndup函数
#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>
#include <stdbool.h>
#include <stdarg.h>
#include <errno.h>
#include <assert.h>

//
// 字符串
//
char *format(char *Fmt, ...);


typedef enum {
  TK_IDENT, // 标记符，可以为变量名、函数名等
  TK_PUNCT, // 操作符：如+-
  TK_KEYWORD, // 关键字
  TK_STR,     // 字符串字面量
  TK_NUM,   // 数字
  TK_EOF,   // 文件终止符，即文件的最后
} TokenKind;

typedef struct Type Type;
typedef struct Token {
  TokenKind kind;
  struct Token *next;
  int64_t val;
  char *loc;
  int len;

  Type *ty;  // TK_STR 使用
  char *str; // 字符串字面量，包括'\0'

  int lineno; // 行号
} Token;


typedef enum {
  ND_ADD, // +
  ND_SUB, // -
  ND_MUL, // *
  ND_DIV, // /
  ND_NEG, // 负号-
  ND_LT, // <
  ND_LE, // <=
  ND_NE, // !=
  ND_EQ, // ==
  ND_EXPR_STMT, // 表达式语句
  ND_STMT_EXPR, // 语句表达式
  ND_ASSIGN, // 赋值
  ND_COMMA,  // 逗号
  ND_MEMBER, // 结构体成员访问
  ND_ADDR,      // 取地址 &
  ND_DEREF,     // 解引用 *
  ND_RETURN, // 返回
  ND_IF,     // "if" 条件判断
  ND_FOR,    // "for" 循环
  ND_FUNCALL, // 函数调用
  ND_BLOCK,  // 代码块（花括号）
  ND_VAR, // 变量
  ND_NUM, // INT NUMBER
  ND_CAST, // 类型转换
} NodeKind;

typedef struct Obj Obj;
typedef struct Type Type;
typedef struct Member Member;

// AST中二叉树节点
// AST: 语法树
// 越往下，优先级越高
typedef struct Node {
  NodeKind kind;
  struct Node *next; // 下一节点，指代下一语句
  Token *tok;        // 节点对应的终结符
  Type *ty;          // 节点中数据的类型
  struct Node *left;
  struct Node *right;
  Obj *var;          // 存储ND_VAL种类的变量
  struct Node *body; // 代码块 或 语句表达式
  int64_t val;       // 存储ND_NUM种类的值

  // 结构体成员访问
  Member *mem;

  // 函数调用
  char *func_name;    // 函数名
  Type *func_type;    // 函数类型
  struct Node *args;  // 函数参数

  // if 语句 或者 "for" 语句
  struct Node *cond;  // 条件内的表达式
  struct Node *then;  // 符合条件后的语句
  struct Node *els;   // 不符合条件后的语句
  struct Node *init;  // 初始化语句
  struct Node *inc;   // 递增语句
} Node;


// 变量 或 函数
typedef struct Obj {
  struct Obj *next; // 指向下一个对象
  char *name;       // 变量名
  Type *ty;         // 变量类型
  bool is_local;    // 是局部或者全局 变量

  // 局部变量
  int offset;       // fp的偏移量

  // 函数 或者 全局变量
  bool is_function;
  bool is_definition; // 函数定义/声明

  // 全局变量
  char *initdata;

  // 函数
  struct Obj *params;  // 形参
  Node *body;          // 函数体
  struct Obj *locals;  // 本地变量
  int stacksize;       // 栈大小

} Obj;

//
// 类型系统
//

// 类型种类
typedef enum {
  TY_VOID, // void 类型
  TY_CHAR, // char 字符类型
  TY_INT, // int 整型
  TY_LONG, // long 长整型
  TY_SHORT, // short 短整型
  TY_PTR, // 指针
  TY_FUNC, // 函数
  TY_ARRAY, // 数组
  TY_STRUCT, // 结构体
  TY_UNION,  // 联合体
} TypeKind;

typedef struct Type {
  TypeKind kind;     // 种类
  int size;          // 大小，sizeof返回的值
  int align;         // 对齐
  struct Type *base; // 指向的类型

  // 变量名？
  Token *name;

  // 结构体
  Member *mems;

  // 函数类型
  Type *returnty; // 函数返回的类型
  Type *params;   // 形参
  Type *next;     // 下一类型（目前仅用于形参）

  // 数组
  int arraylen; // 数组长度，元素总个数
} Type;

typedef struct Member {
	Member *next; // 下个成员
	Type *ty;     // 成员的类型
	Token *name;  // 名称
	int offset;   // 偏移量
} Member;

// 声明全局变量，定义在type.c中
extern Type *TyVoid;
extern Type *TyChar;
extern Type *TyInt;
extern Type *TyLong;
extern Type *TyShort;

// 类型转换，将表达式的值转换为另一种类型
Node *newcast(Node *expr, Type *ty);
// 构建一个指针类型，并指向基类
Type *pointerto(Type *Base);
// 函数类型
Type *functype(Type *ReturnTy);

// 判断是否为整型
bool is_integer(Type *ty);
// 为节点内部的所有节点添加类型
void add_type(Node *nd);
// 复制类型
Type *copytype(Type *ty);
// 构造数据类型，传入数组基类，元素个数
Type *arrayof(Type *base, int len);


int align_to(int n, int align);
// 指rvcc源文件的某个文件的某一行出了问题，打印出文件名和行号
#define unreachable() error("internal error at %s:%d", __FILE__, __LINE__)
void error(char *fmt, ...);
void errorAt(char *Loc, char *Fmt, ...);
void errorTok(Token *Tok, char *Fmt, ...);
// 判断Token与Str的关系
bool equal(Token *Tok, char *Str);
Token *skip(Token *Tok, char *Str);
bool consume(Token **Rest, Token *Tok, char *Str);
// 词法分析
Token *tokenize_file(char *path);
void codegen(Obj *prog, FILE *out);
