#include "rvcc.h"

// (Type) {...} 构造了一个复合字面量，相当于Type的匿名变量。
Type *TyVoid = &(Type){TY_VOID, 1, 1};
Type *TyBool = &(Type){TY_BOOL, 1, 1};
Type *TyChar = &(Type){TY_CHAR, 1, 1};
Type *TyShort = &(Type){TY_SHORT, 2, 2};
Type *TyInt = &(Type){TY_INT, 4, 4};
Type *TyLong = &(Type){TY_LONG, 8, 8};

static Type *new_type(TypeKind kind, int size, int align) {
  Type *ty = calloc(1, sizeof(Type));
  ty->kind = kind;
  ty->size = size;
  ty->align = align;
  return ty;
}

// 判断Type是否为整数类型
bool is_integer(Type *ty) {
  return ty->kind == TY_BOOL || ty->kind == TY_CHAR || ty->kind == TY_SHORT ||
         ty->kind == TY_INT || ty->kind == TY_LONG || ty->kind == TY_ENUM;
}


// 构造一个指针类型，并且指向基类
Type *pointerto(Type *base)
{
  Type *ty = new_type(TY_PTR, 8, 8);
  ty->base = base;
  return ty;
}

// 构造数据类型，传入数组基类，元素个数
Type *arrayof(Type *base, int len)
{
  // 数组大小为所有元素大小之和
  Type *ty = new_type(TY_ARRAY, base->size * len, base->align);
  ty->base = base;
  ty->arraylen = len;
  return ty;
}

Type *functype(Type *returnty)
{
  Type *ty = calloc(1, sizeof(Type));
  ty->kind = TY_FUNC;
  ty->returnty = returnty;
  return ty;
}

// 构造枚举类型
Type *enumtype(void) {
  return new_type(TY_ENUM, 4, 4);
}

Type *copytype(Type *ty)
{
  Type *ret = calloc(1, sizeof(Type));
  *ret = *ty;
  return ret;
}

// 获取容纳左右部的类型
static Type *get_common_type(Type *ty1, Type *ty2) {
  if (ty1->base)
    return pointerto(ty1->base);
  if (ty1->size == 8 || ty2->size == 8)
    return TyLong;
  return TyInt;
}

// 进行常规的算术转换
static void usual_arith_conv(Node **lhs, Node **rhs) {
  Type *ty = get_common_type((*lhs)->ty, (*rhs)->ty);
  // 将左右部转换为兼容的类型
  *lhs = newcast(*lhs, ty);
  *rhs = newcast(*rhs, ty);
}

// 为节点内的所有节点添加类型
void add_type(Node *nd)
{
  if (!nd || nd->ty)
    return;

  // 递归访问所有节点以增加类型
  add_type(nd->left);
  add_type(nd->right);
  add_type(nd->cond);
  add_type(nd->then);
  add_type(nd->els);
  add_type(nd->init);
  add_type(nd->inc);

  // 访问链表内的所有节点以增加类型
  for (Node *n = nd->body; n; n = n->next) {
    add_type(n);
  }
  // 访问链表内的所有参数节点以增加类型
  for (Node *n = nd->args; n; n = n->next)
    add_type(n);

  switch (nd->kind) {
  // 判断是否val强制转换为int后依然完整，完整用int否则用long
  case ND_NUM:
    nd->ty = (nd->val == (int)nd->val) ? TyInt : TyLong;
    return;
  // 将节点类型设为 节点左部的类型
  case ND_ADD:
  case ND_SUB:
    // 左右部转换
    usual_arith_conv(&nd->left, &nd->right);
    // ADD 和 SUB 都调整为 ptr +- num 的形式了，ptr永远在左边
    // 所以要返回左边的类型。
    nd->ty = nd->left->ty;
    return;
  case ND_MUL:
  case ND_DIV:
  case ND_MOD:
    // 左右部转换
    usual_arith_conv(&nd->left, &nd->right);
    nd->ty = nd->right->ty;
    return;
  case ND_NOT:
    //  将类型设置为int
    nd->ty = TyInt;
    return;
  case ND_BITNO:
    // 将节点类型设置为 右部的类型
    nd->ty = nd->right->ty;
    return;
  case ND_NEG: {
    // 对右部进行转换
    Type *ty = get_common_type(TyInt, nd->right->ty);
    nd->right = newcast(nd->right, ty);
    nd->ty = ty;
    return;
  }
  // 将节点类型设为 节点右部的类型
  // 右部不能是数组节点
  case ND_ASSIGN:
    if (nd->left->ty->kind == TY_ARRAY)
      errorTok(nd->left->tok, "not an lvalue");
    if (nd->left->ty->kind != TY_STRUCT)
      // 对右部转换
      nd->right = newcast(nd->right, nd->left->ty);
    nd->ty = nd->left->ty;
    return;
  // 将节点类型设为 右部的类型
  case ND_COMMA:
    nd->ty = nd->right->ty;
    return;
  // 将节点类型设为 long，没有实际含义
  case ND_EQ:
  case ND_NE:
  case ND_LT:
  case ND_LE:
    // 对左右部进行转换
    usual_arith_conv(&nd->left, &nd->right);
    nd->ty = TyInt;
    return;
  case ND_FUNCALL:
    nd->ty = TyLong;
    return;
  // 将节点类型设为 变量的类型
  case ND_VAR:
    nd->ty = nd->var->ty;
    return;
  // 将节点类型设为 成员的类型
  case ND_MEMBER:
    nd->ty = nd->mem->ty;
    return;
  // 将节点类型设为指针，并指向左部的类型
  case ND_ADDR:
    // 右部如果是数组，则为指向数组基类的指针
    if (nd->right->ty->kind == TY_ARRAY)
      nd->ty = pointerto(nd->right->ty->base);
    else
      nd->ty = pointerto(nd->right->ty);
    return;
  // 节点类型：如果解引用指向的是指针，则为指针指向的类型，否则为int
  case ND_DEREF:
    // 如果不存在基类，则无法解引用
    if (!nd->right->ty->base)
      errorTok(nd->tok, "invalid pointer dereference");
    if (nd->right->ty->base->kind == TY_VOID)
      errorTok(nd->tok, "deferencing a void pointer");
    nd->ty = nd->right->ty->base;
    return;
  // 节点类型为 最后的表达式语句的类型
  case ND_STMT_EXPR:
    if (nd->body) {
      Node *stmt = nd->body;
      while (stmt->next)
        stmt = stmt->next;
      if (stmt->kind == ND_EXPR_STMT) {
        nd->ty = stmt->right->ty;
        return;
      }
    }
    errorTok(nd->tok, "statement expression returning void is not support");
    return;
  default:
    break;
  }
}
