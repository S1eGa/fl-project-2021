from pyparsing import (
    Word,
    nums,
    alphas,
    Combine,
    oneOf,
    opAssoc,
    infixNotation,
    Literal,
    ParserElement,
    Keyword,
    Forward,
    Optional,
    Suppress,
    Group, QuotedString
)

ParserElement.enablePackrat()

VarKW = Keyword("Var")

ReturnKW = Keyword("Return")

IfKW = Keyword("If")

ElseKW = Keyword("Else")

WhileKW = Keyword("While")

FunctionKW = Keyword("Function")

StringT = Keyword("String")

BoolT = Keyword("Bool")

IntegerT = Keyword("Integer")


class EvalConstant:
    vars_ = {}

    def __init__(self, tokens):
        self.value = tokens[0]
        print("EvalConstant value is ", end='')
        print(self.value)

    def eval(self):
        if self.value in EvalConstant.vars_:
            return EvalConstant.vars_[self.value]
        else:
            return float(self.value)


class IntegerLiteral:

    def __init__(self, tokens):
        self.value = int(tokens[0])
        print("IntegerLiteral ", self.value)


class BoolLiteral:

    def __init__(self, tokens):
        self.value = bool(tokens[0])
        print("BoolLiteral ", self.value)


class StringLiteral:

    def __init__(self, tokens):
        self.value = tokens[0]
        print("StringLiteral ", self.value)


class EvalSignOp:
    def __init__(self, tokens):
        print("EvalSignOp ", end='')
        print(tokens[0])
        self.sign, self.value = tokens[0]

    def eval(self):
        mult = {"+": 1, "-": -1}[self.sign]
        return mult * self.value.eval()


def operatorOperands(tokenlist):
    it = iter(tokenlist)
    while 1:
        try:
            yield next(it), next(it)
        except StopIteration:
            break


class EvalPowerOp:
    def __init__(self, tokens):
        self.value = tokens[0]
        print("EvalPowerOp ", end='')
        print(self.value)

    def eval(self):
        res = self.value[-1].eval()
        for val in self.value[-3::-2]:
            res = val.eval() ** res
        return res


class EvalMultOp:
    def __init__(self, tokens):
        self.value = tokens[0]
        print("EvalMultOp ", end='')
        print(self.value)

    def eval(self):
        prod = self.value[0].eval()
        for op, val in operatorOperands(self.value[1:]):
            if op == "*":
                prod *= val.eval()
            if op == "/":
                prod /= val.eval()
        return prod


class EvalAddOp:
    def __init__(self, tokens):
        self.value = tokens[0]
        print("EvalAddOp ", end='')
        print(self.value)

    def eval(self):
        sum = self.value[0].eval()
        for op, val in operatorOperands(self.value[1:]):
            if op == "+":
                sum += val.eval()
            if op == "-":
                sum -= val.eval()
        return sum


class EvalComparisonOp:
    opMap = {
        "<": lambda a, b: a < b,
        "<=": lambda a, b: a <= b,
        ">": lambda a, b: a > b,
        ">=": lambda a, b: a >= b,
        "!=": lambda a, b: a != b,
        "==": lambda a, b: a == b,
    }

    def __init__(self, tokens):
        self.value = tokens[0]
        print("EvalComparisonOp ", end='')
        print(self.value)

    def eval(self):
        val1 = self.value[0].eval()
        for op, val in operatorOperands(self.value[1:]):
            fn = EvalComparisonOp.opMap[op]
            val2 = val.eval()
            if not fn(val1, val2):
                break
            val1 = val2
        else:
            return True
        return False


class EvalNegOp:

    def __init__(self, tokens):
        self.neg, self.value = tokens[0]
        print("EvalNegOp ", end='')
        print(self.value)

    def eval(self):
        return not self.value.eval()


class EvalAndOp:
    def __init__(self, tokens):
        self.value = tokens[0]
        print("EvalAndOp ", end='')
        print(self.value)

    def eval(self):
        val1 = self.value[0].eval()
        if not val1:
            return False
        for op, val in operatorOperands(self.value[1:]):
            val2 = val.eval()
            if not (val1 and val2):
                break
            val1 = val2
        return True


class EvalOrOp:
    def __init__(self, tokens):
        self.value = tokens[0]
        print("EvalOrOp ", end='')
        print(self.value)

    def eval(self):
        if self.value[0].eval():
            return True
        for op, val in operatorOperands(self.value[1:]):
            if val.eval():
                return True
        return False


class EvalFunctionCall:
    def __init__(self, tokens):
        self.name = tokens[0]
        self.arguments = tokens[1:]
        print("EvalFunctionCall ", end='')
        print(self.name, ' ', self.arguments)


class VariableDeclaration:
    def __init__(self, tokens):
        self.type = tokens[0]
        self.name = tokens[1]
        self.value = tokens[2]
        print("VariableDeclaration ", end='')
        print(self.name, ' ', self.value)


class FunctionDeclaration:
    def __init__(self, tokens):
        print(tokens)
        self.name = tokens[0]
        # TODO: change index of end
        self.args = tokens[1]
        self.return_type = tokens[2]
        self.body = tokens[3]


class ReturnStatement:
    def __init__(self, tokens):
        self.value = tokens[0]
        print("ReturnStatement ", self.value)


class IfStatement:
    def __init__(self, tokens):
        self.t = len(tokens) >= 3
        self.cond = tokens[0]
        self.then_br = tokens[1]
        print("IfStatement ", self.cond, ' ', self.then_br, end='')
        if self.t:
            self.else_br = tokens[2]
            print(' ', self.else_br)


class WhileStatement:
    def __init__(self, tokens):
        self.cond = tokens[0]
        self.while_body = tokens[1]
        print("WhileStatement ", self.cond, ' ', self.while_body)


class LProgram:
    def __init__(self, tokens):
        self.funcs = tokens
        print("LProgram ", self.funcs)


expr = Forward()
integer = Word(nums)
real = Combine(Word(nums) + "." + Word(nums))
variable = Word(alphas)
TYPE = StringT | BoolT | IntegerT
StringL = QuotedString('"', endQuoteChar='"')
BoolL = Literal("True") | Literal("False")
IntegerL = integer
Lit = StringL | BoolL | IntegerL
LP = Literal('(')
LB = Literal('{')
RP = Literal(')')
RB = Literal('}')
ASSIGN = Literal(':=')
ARROW = Literal('->')
COMMA = Literal(',')
EOS = Literal(';')
return_statement = Suppress(ReturnKW) + expr + EOS
operand = real | integer | variable
function_call = variable + Suppress(LP) + Optional(expr + (Suppress(COMMA) + expr)[...]) + Suppress(RP)

signop = oneOf("+ -")
multop = oneOf("* /")
plusop = oneOf("+ -")
expop = Literal("**")
negop = Literal("!")
logic_or_op = Literal("||")
logic_and_op = Literal("&&")

operand.setParseAction(EvalConstant)
IntegerL.setParseAction(IntegerLiteral)
BoolL.setParseAction(BoolLiteral)
StringL.setParseAction(StringLiteral)
function_call.setParseAction(EvalFunctionCall)
arith_expr = infixNotation(
    function_call | StringL | BoolL | IntegerL | variable,
    [
        (signop, 1, opAssoc.RIGHT, EvalSignOp),
        (expop, 2, opAssoc.LEFT, EvalPowerOp),
        (multop, 2, opAssoc.LEFT, EvalMultOp),
        (plusop, 2, opAssoc.LEFT, EvalAddOp),
    ],
)

comparisonop = oneOf("< <= > >= != ==")
comp_expr = infixNotation(
    arith_expr,
    [
        (comparisonop, 2, opAssoc.LEFT, EvalComparisonOp),
    ],
)

logic_expr = infixNotation(
    comp_expr,
    [
        (negop, 1, opAssoc.RIGHT, EvalNegOp),
        (logic_and_op, 2, opAssoc.LEFT, EvalAndOp),
        (logic_or_op, 2, opAssoc.LEFT, EvalOrOp),
    ]
)

expr <<= logic_expr

if_statement = Forward()
while_statement = Forward()
statement = Forward()

statement_list = statement[...]

var_decl_statement = Suppress(VarKW) + TYPE + variable + Suppress(ASSIGN) + expr + Suppress(EOS)

if_statement <<= Suppress(IfKW) \
                 + Suppress(LP) \
                 + expr \
                 + Suppress(RP) \
                 + Suppress(LB) \
                 + statement_list \
                 + Suppress(RB) + Optional(Suppress(ElseKW + LB) + statement_list + Suppress(RB))

while_statement <<= Suppress(WhileKW) \
                    + Suppress(LP) \
                    + expr \
                    + Suppress(RP) \
                    + Suppress(LB) \
                    + statement_list \
                    + Suppress(RB)

statement <<= while_statement | var_decl_statement | if_statement | return_statement | (expr + Suppress(Literal(";")))

function_declaration \
    = Suppress(FunctionKW) + \
      variable + Suppress(LP) + \
      Group(Optional(Group(TYPE + variable) + (Suppress(COMMA) + Group(TYPE + variable))[...])) \
      + Suppress(RP) + Suppress(ARROW) + TYPE + Suppress(LB) + statement_list + Suppress(RB)

program_entry = function_declaration[1, ...]

var_decl_statement.setParseAction(VariableDeclaration)
if_statement.setParseAction(IfStatement)
while_statement.setParseAction(WhileStatement)
return_statement.setParseAction(ReturnStatement)
program_entry.setParseAction(LProgram)
function_declaration.setParseAction(FunctionDeclaration)

with open('input.txt', 'r') as file:
    data = file.read().replace('\n', '')

ast = program_entry.parseString(data)[0]

print(ast)
