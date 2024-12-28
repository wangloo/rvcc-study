#include "rvcc.h"

// 输出文件
static FILE *OutputFile;
// 存储栈的深度
static int depth;
// 用于函数参数的寄存器们
static char *ArgReg[] = {"a0", "a1", "a2", "a3", "a4", "a5"};
// 当前的函数
static Obj *current_fn;

int align_to(int n, int align)
{
  // 向上对齐 (0, align] 返回 align
  return (n+align-1) & ~(align-1);
}
static void assign_lvar_offset(Obj *prog)
{
  // 为每个函数计算其所用的栈空间
  for (Obj *fn = prog; fn; fn = fn->next) {
    int offset = 0;
    for (Obj *var = fn->locals; var; var = var->next) {
      // 为每个变量分配空间
      offset += var->ty->size;
      // 对齐变量
      offset = align_to(offset, var->ty->align);
      var->offset = -offset;
    }
    // 将栈固定对齐到16字节
    fn->stacksize = align_to(offset, 16);
  }
}

// 输出字符串并换行
static void println(char *Fmt, ...) {
  va_list VA;
  va_start(VA, Fmt);
  vfprintf(OutputFile, Fmt, VA);
  va_end(VA);
  fprintf(OutputFile, "\n");
}



static void gen_expr(Node *nd);
static void gen_stmt(Node *nd);

// 计算给定节点的绝对地址
// 如果报错，说明节点不在栈中
static void gen_addr(Node *nd)
{
  if (nd->kind == ND_VAR) {
    if (nd->var->is_local) { // 偏移量是相对fp的
      println("  # 获取变量%s的栈内地址为%d(fp)", nd->var->name,
           nd->var->offset);
      println("  addi a0, fp, %d", nd->var->offset);
    } else {
      println("  # 获取全局变量%s的地址", nd->var->name);
      println("  la a0, %s", nd->var->name);
    }
    return;
  }
  // &* expr == expr
  if (nd->kind == ND_DEREF) {
    gen_expr(nd->right);
    return;
  }
  // 逗号
  if (nd->kind == ND_COMMA) {
    gen_expr(nd->left);
    gen_addr(nd->right);
    return;
  }
  // 结构体成员
  if (nd->kind == ND_MEMBER) {
    gen_addr(nd->right);
    println("  # 计算成员变量的地址偏移量");
    println("  li t0, %d", nd->mem->offset);
    println("  add a0, a0, t0");
    return;
  }
  errorTok(nd->tok, "not an lvalue");
}

// 压栈，将结果临时存入栈中备用。
// 不实用寄存器存储的原因是需要存储变量的个数是变化的
static void push(void)
{
  println("  # 压栈，将a0的值存入栈顶");
  println("  addi sp, sp, -8");
  println("  sd a0, 0(sp)");
  depth++;
}

// 弹栈，弹出到reg名称的寄存器中
static void pop(const char *reg)
{
  println("  # 弹栈，将栈顶的值存入%s", reg);
  println("  ld %s, 0(sp)", reg);
  println("  addi sp, sp, 8");
  depth--;
}

// 加载a0指向的值
static void load(Type *ty) {
  // 使用数组名or结构体or联合体访问，得到的结果就是地址，不用再加载
  if (ty->kind == TY_ARRAY || ty->kind == TY_STRUCT || ty->kind == TY_UNION)
    return;
  // 访问a0地址中存储的数据，存入到a0当中
  println("  # 读取a0中存放的地址，得到的值存入a0");
  if (ty->size == 1)
    println("  lb a0, 0(a0)");
  else if (ty->size == 2)
    println("  lh a0, 0(a0)");
  else if (ty->size == 4)
    println("  lw a0, 0(a0)");
  else
    println("  ld a0, 0(a0)");
}

static void store(Type *ty)
{
  pop("a1");

  if (ty->kind == TY_STRUCT || ty->kind == TY_UNION) {
    println("  # 对%s进行赋值", ty->kind == TY_STRUCT ? "结构体" : "联合体");
    for (int i = 0; i < ty->size; i++) {
      println("  li t0, %d", i);
      println("  add t0, a0, t0");
      println("  lb t1, 0(t0)");
      println("  li t0, %d", i);
      println("  add t0, a1, t0");
      println("  sb t1, 0(t0)");
    }
    return;
  }
  println("  # 将a0的值，写入到a1中存放的地址");
  if (ty->size == 1)
    println("  sb a0, 0(a1)");
  else if (ty->size == 2)
    println("  sh a0, 0(a1)");
  else if (ty->size == 4)
    println("  sw a0, 0(a1)");
  else
    println("  sd a0, 0(a1)");
}

// 代码段计数
static int count(void)
{
  static int I = 1;
  return I++;
}

static void gen_expr(Node *nd)
{
  //.loc 文件编号 行号
  println(" .loc 1 %d", nd->tok->lineno);
  if (nd->kind == ND_NUM) {
    println("  # 将%d加载到a0中", nd->val);
    println("  li a0, %ld", nd->val);
    return;
  }
  if (nd->kind == ND_NEG) {
    gen_expr(nd->right);
    println("  # 对a0值进行取反");
    println("  neg a0, a0");
    return;
  }
  // 解引用
  if (nd->kind == ND_DEREF) {
    gen_expr(nd->right);
    load(nd->ty);
    return;
  }
  // 逗号
  if (nd->kind == ND_COMMA) {
    gen_expr(nd->left);
    gen_expr(nd->right);
    return;
  }
  // 取地址
  if (nd->kind == ND_ADDR) {
    gen_addr(nd->right);
    return;
  }
  if (nd->kind == ND_ASSIGN) {
    // 左部是左值，保存值到的地址
    gen_addr(nd->left);
    push();
    // 右部是右值，为表达式的值
    gen_expr(nd->right);
    store(nd->ty);
    return;
  }
  // 语句表达式
  if (nd->kind == ND_STMT_EXPR) {
    for (Node *n = nd->body; n; n = n->next)
      gen_stmt(n);
    return;
  }
  if (nd->kind == ND_FUNCALL) {
    // 记录参数个数
    int nargs = 0;
    // 计算所有参数的值，正向压栈
    for (Node *arg = nd->args; arg; arg = arg->next) {
      gen_expr(arg);
      push();
      nargs++;
    }
    // 反向弹栈, a0->参数1, a1->参数2
    for (int i = nargs-1; i >= 0; i--)
      pop(ArgReg[i]);

    println("\n # 调用函数%s", nd->func_name);
    println("  call %s", nd->func_name);
    return;
  }
  if (nd->kind == ND_VAR ||
      nd->kind == ND_MEMBER) {
    // 计算出变量的地址，然后存入a0
    gen_addr(nd);
    load(nd->ty);
    return;
  }


  // 递归到最右下节点
  gen_expr(nd->right);
  // 将结果压入栈
  push();
  // 递归到左节点
  gen_expr(nd->left);
  // 将结果弹栈到a1
  pop("a1");

  switch (nd->kind) {
  case ND_ADD:
    println("  # a0+a1，结果写入a0");
    println("  add a0, a0, a1");
    return;
  case ND_SUB:
    println("  # a0-a1，结果写入a0");
    println("  sub a0, a0, a1");
    return;
  case ND_MUL:
    println("  # a0×a1，结果写入a0");
    println("  mul a0, a0, a1");
    return;
  case ND_DIV:
    println("  # a0÷a1，结果写入a0");
    println("  div a0, a0, a1");
    return;
  case ND_EQ:
  case ND_NE:
    // a0=a0^a1，异或指令
    println("  # 判断是否a0%sa1", nd->kind == ND_EQ ? "=" : "≠");
    println("  xor a0, a0, a1");
    if (nd->kind == ND_EQ)
      // a0==a1
      // a0=a0^a1, sltiu a0, a0, 1
      // 等于0则置1
      println("  seqz a0, a0");
    else
      // a0!=a1
      // a0=a0^a1, sltu a0, x0, a0
      // 不等于0则置1
      println("  snez a0, a0");
    return;
  case ND_LT:
    println("  # 判断a0<a1");
    println("  slt a0, a0, a1");
    return;
  case ND_LE:
    // a0<=a1等价于
    // a0=a1<a0, a0=a1^1
    println("  # 判断是否a0≤a1");
    println("  slt a0, a1, a0");
    println("  xori a0, a0, 1");
    return;
  default:
    break;
  }

  errorTok(nd->tok, "invalid expression\n");
}

static void gen_stmt(Node *nd)
{
  // .loc 文件编号 行号
  println(" .loc 1 %d", nd->tok->lineno);

  if (nd->kind == ND_FOR) {
    // 代码段技术
    int c = count();
    println("\n# =====循环语句%d===============", c);
    // 生成初始化语句
    if (nd->init) {
      println("\n# Init语句%d", c);
      gen_stmt(nd->init);
    }
    // 输出循环头部标签
    println("\n# 循环%d的.L.begin.%d段标签", c, c);
    println(".L.begin.%d:", c);
    // 处理循环条件语句
    println("# Cond表达式%d", c);
    if (nd->cond) {
      // 生成条件循环语句
      gen_expr(nd->cond);
      // 判断结果是否为0，为0则跳转到结束部分
      println("  # 若a0为0，则跳转到循环%d的.L.end.%d段", c, c);
      println("  beqz a0, .L.end.%d", c);
    }
    // 生成循环体语句
    println("\n# Then语句%d", c);
    gen_stmt(nd->then);
    // 处理循环递增语句
    if (nd->inc) {
      // 生成循环递增语句
      gen_expr(nd->inc);
    }
    // 跳转到循环头部
    println("  j .L.begin.%d", c);
    // 输出循环尾部标签
    println(".L.end.%d:", c);
    return;
  }
  if (nd->kind == ND_IF) {
    // 代码段计数
    int c = count();
    println("\n# =====分支语句%d==============", c);
    // 生成条件内语句
    println("\n# Cond表达式%d", c);
    gen_expr(nd->cond);
    // 判断结果是否为0，为0则跳转到else标签
    println("  # 若a0为0，则跳转到分支%d的.L.else.%d段", c, c);
    println("  beqz a0, .L.else.%d", c);
    // 生成复合条件后的语句
    println("\n# Then语句%d", c);
    gen_stmt(nd->then);
    // 执行完后跳转到if语句后面的语句
    println("  # 跳转到分支%d的.L.end.%d段", c, c);
    println("  j .L.end.%d", c);
    // else代码块，else可能为空，故输出标签
    println("\n# Else语句%d", c);
    println("# 分支%d的.L.else.%d段标签", c, c);
    println(".L.else.%d:", c);
    // 生成不符合条件后的语句
    if (nd->els)
      gen_stmt(nd->els);
    // 结束if语句，继续执行后面的语句
    println("\n# 分支%d的.L.end.%d段标签", c, c);
    println(".L.end.%d:", c);

    return;
  }
  if (nd->kind == ND_BLOCK) {
    for (Node *n = nd->body; n; n = n->next) {
      gen_stmt(n);
    }
    return;
  }
  if (nd->kind == ND_RETURN) {
    println("# 返回语句");
    gen_expr(nd->right);
    // 无条件跳转语句，跳转到.L.return段
    // j offset是 jal x0, offset的别名指令
    println("  # 跳转到.L.return.%s段", current_fn->name);
    println("  j .L.return.%s", current_fn->name);
    return;
  }

  if (nd->kind == ND_EXPR_STMT) {
    gen_expr(nd->right);
    return;
  }
  errorTok(nd->tok, "invalid statement\n");
}

static void emit_data(Obj *prog) {
  for (Obj *var = prog; var; var = var->next) {
    if (var->is_function)
      continue;

    println("  # 数据段标签");
    println("  .data");
    // 判断是否有初始值
    if (var->initdata) {
      println("%s:", var->name);
      // 打印出字符串的内容，包括转义字符
      for (int i = 0; i < var->ty->size; ++i) {
        char c = var->initdata[i];
        if (isprint(c))
          println("  .byte %d\t# 字符：%c", c, c);
        else
          println("  .byte %d", c);
      }
    } else {
      println("  .globl %s", var->name);
      println("  # 全局变量%s", var->name);
      println("%s:", var->name);
      println("  # 零填充%d位", var->ty->size);
      println("  .zero %d", var->ty->size);
    }
  }
}

// 生成代码
static void emit_text(Obj *prog) {
  // 为每个函数单独生成代码
  for (Obj *fn = prog; fn; fn = fn->next) {
    if (!fn->is_function)
      continue;

    println("  # 定义全局%s段", fn->name);
    println("  .globl %s", fn->name);
    println("  # 代码段标签");
    println("  .text");
    println("\n# =====程序开始===============");
    println("# %s段标签，也是程序入口段", fn->name);
    println("%s:", fn->name);
    current_fn = fn;
    // 栈布局
    //-------------------------------// sp
    //              ra
    //-------------------------------// ra = sp-8
    //              fp
    //-------------------------------// fp = sp-16
    //              变量
    //-------------------------------// sp=sp-16-stacksize
    //           表达式计算
    //-------------------------------//

    // Prologue, 前言
    // 将ra寄存器压栈，保存ra的值
    println("  # 将ra寄存器压栈,保存ra的值");
    println("  addi sp, sp, -16");
    println("  sd ra, 8(sp)");
    // 将fp压入栈中，保存fp的值
    println("  # 将fp压栈，fp属于“被调用者保存”的寄存器，需要恢复原值");
    println("  sd fp, 0(sp)");
    // 将sp写入fp
    println("  # 将sp的值写入fp");
    println("  mv fp, sp");
    // sp偏移量为实际占用的栈大小
    println("  # sp腾出StackSize大小的栈空间");
    println("  addi sp, sp, -%d", fn->stacksize);

    int I = 0;
    for (Obj *var=fn->params; var; var = var->next) {
      println("  # 将%s寄存器的值存入%s的栈地址", ArgReg[I], var->name);
      if (var->ty->size == 1)
        println("  sb %s, %d(fp)", ArgReg[I++], var->offset);
      else if (var->ty->size == 2)
        println("  sh %s, %d(fp)", ArgReg[I++], var->offset);
      else if (var->ty->size == 4)
        println("  sw %s, %d(fp)", ArgReg[I++], var->offset);
      else if (var->ty->size == 8)
        println("  sd %s, %d(fp)", ArgReg[I++], var->offset);
      else
        unreachable();
    }


    // 使用语法树，生成表达式
    println("\n# =====%s主体===============", fn->name);
    gen_stmt(fn->body);
    assert(depth == 0);

    // Epilogue, 后语
    // 输出return段标签
    println("\n# =====%s结束===============", fn->name);
    println("# return段标签");
    println(".L.return.%s:", fn->name);
    // 将fp的值改写回sp
    println("  # 将fp的值写回sp");
    println("  mv sp, fp");
    // 将最早fp保存的值弹栈，恢复fp
    println("  # 将最早fp保存的值弹栈，恢复fp和sp");
    println("  ld fp, 0(sp)");
    // 将ra寄存器弹栈,恢复ra的值
    println("  # 将ra寄存器弹栈,恢复ra的值");
    println("  ld ra, 8(sp)");
    println("  addi sp, sp, 16");

    // ret为jalr x0, x1, 0别名指令，用于返回子程序
    println("  # 返回a0值给系统调用");
    println("  ret");
  }
}

void codegen(Obj *prog, FILE *out)
{
  // 设置目标文件的文件流指针
  OutputFile = out;

  // 分配函数内部变量的栈空间
  assign_lvar_offset(prog);
  // 生成数据
  emit_data(prog);
  // 生成代码
  emit_text(prog);
}