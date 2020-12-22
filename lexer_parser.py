import ply.lex as lex # штука, которая разбивает текст на базовые элементы языка. Ну или группирует текст в базовые элементы
import ply.yacc as yacc # штука (парсер), в которую мы передаем токены и описываем правила их соединения (грамматику). Походу работы этой программы мы можем создать абстрактное дерево или же сразу выполнять программу

tokens = ("ALPHABET", "OR", "DOT", "PLUS", "OPTPART", "REPEAT", "LEFTBRACKET", "RIGHTBRACKET", "ID")
#Регулярные выражения должны поддерживать следующие операции:
#Операция ‘или’: r1|r2 (метасимвол ‘|’)
#Операция ‘конкатенация’: r1r2 (опциональный метасимвол ‘.’, r1.r2)
#Операция ‘позитивное замыкание’: r+ (метасимвол ‘+’)
#Операция ‘опциональная часть’: r? (метасимвол ‘?’)
#Операция ‘любой символ’: . (метасимвол ‘.’)
#Операция ‘повтор выражения в диапазоне’: r{x,y} (метасимвол ‘{х, y}’, где x – нижняя граница, y – верхняя граница), границы могут отсутствовать.
#Операция ‘именованная группа захвата’: (<name>r) (метасимвол ‘(<name>)’, name – имя группы захвата)
#Операция ‘выражение из именованной группы захвата’: <name> (метасимвол ‘<name>’, name – имя группы захвата)

#t_OR = r"\|"  ИЛИ
#t_DOT = r"\."  КОНКАТЕНАЦИЯ
#t_PLUS = r"\+"  ПОЗИТИВНОЕ ЗАМЫКАНИЕ
#t_OPTPART = r"\?"  ОПЦИОНАЛЬНАЯ ЧАСТЬ
#t_LEFTBRACKET = r"\("  ЛЕВАЯ СКОБКА
#t_RIGHTBRACKET = r"\)"  ПРАВАЯ СКОБКА
#t_ID = r"\<[A-Za-z][A-Za-z0-9_]*\>"  ИДЕНТИФИКАТОР

def t_error(t): # вывод ошибок
    global ErrorsList
    ErrorsList.append("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1) # пропустить n=1 символов в строке

def t_ALPHABET(t): # Экранируются символом ‘&’
    r"(&[?+.|&{}<>()])|([^<>?+.|&)({}])"
    symb = str(t.value)
    if len(symb) > 1: #экранированный или нет
        symb = symb[1]
    t.value = symb
    return t

def t_OR(t):
    r"\|"
    t.value = str(t.value)
    return t

def t_CONCATENATION(t):
    r"\."
    t.value = str(t.value)
    return t

def t_PLUS(t):
    r"\+"
    t.value = str(t.value)
    return t

def t_OPTPART(t):
    r"\?"
    t.value = str(t.value)
    return t

def t_REPEAT(t):
    r"\{(([0-9]+)?,([0-9]+)?)?\}"
    bounds = str(t.value)[1:-1] # берем все со второго до предпоследнего символа
    if bounds:
        x, y = bounds.split(",") # x -  нижняя граница, y -  верхняя граница
    else:
        x, y = "", ""
    if not x:
        x = 0
    else:
        x = int(x)
    if not y:
        y = -1
    else:
        y = int(y)
    t.value = (x, y)
    return t

def t_LEFTBRACKET(t):
    r"\("
    t.value = str(t.value)
    return t

def t_RIGHTBRACKET(t):
    r"\)"
    t.value = str(t.value)
    return t

def t_ID(t):
    r"\<[A-Za-z][A-Za-z0-9_]*\>"
    t.value = str(t.value)[1:-1]
    return t

lexer = lex.lex()

def p_regular(p):
    """regular : r"""
    p[0] = RootNode(p[1])

def p_error(p):
    global ErrorsList
    ErrorsList.append("Unknown token: " + str(p))

def p_or(p):
    """or : r OR r"""
    p[0] = OrNode(p[1], p[3])

def p_concatenation(p):
    """concatenation : r r"""
    p[0] = ConcatenationNode(p[1], p[2])

def p_positiveclose(p):
    """positiveclose : r PLUS"""
    p[0] = PositiveCloseNode(p[1])

def p_optional(p):
    """optionalpart : r OPTPART"""
    p[0] = OptionalPartNode(p[1])

def p_anysymbol(p):
    """anysymbol : DOT"""
    p[0] = SymbolNode()  # char = -1 - special code for symbol

ErrorsList = []

def p_repeat(p):
    """repeat : r REPEAT"""
    global ErrorsList
    x = p[2][0] # токен репит на втором месте
    y = p[2][1]
    if x <= y and y or y < 0:
        if x == 0 and y == -1:
            p[0] = ClosureNode(p[1])
        else:
            p[0] = RepeatNode(p[1], x, y)
    elif not y:
        ErrorsList.append("Y bound must be greater then 0")
    else:
        ErrorsList.append("Y bound must not be less then X")

NamedCaptureGroups = {}

def p_namedcapturecgroup(p):
    """namedcapturecgroup : LEFTBRACKET ID r RIGHTBRACKET"""
    global namedCaptureGroups  # эту переменную можно использовать вне функции
    if not namedCaptureGroups.get(p[2]): #проверка в библиотеке
        p[0] = NamedCaptureGroupeNode(p[3], p[2])
        namedCaptureGroups[p[2]] = p[3]
    else:
        global ErrorsList
        ErrorsList.append("Redefinition of named capture group:" + p[2])

def p_expressionnamedcapturecgroup(p):
    """expressionnamedcapturecgroup : ID"""
    global namedCaptureGroups
    if namedCaptureGroups.get(p[1]):
        p[0] = NamedCaptureGroupeNode(namedCaptureGroups[p[1]].copy(), p[1])
    else:
        global ErrorsList
        ErrorsList.append("Undefined named capture group: " + p[1])

def p_alphabet(p):
    """alphabet : ALPHABET"""
    p[0] = SymbolNode(p[1])

parser = yacc.yacc()

class RootNode:
    def __init__(self, child):
        self.child = child

    def copy(self):
        return RootNode(self.child.copy())

    def e_closure(self):
        return self.child.e_closure()

    def move(self):
        return self.child.move()

class OrNode:
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def copy(self):
        return OrNode(self.left.copy(), self.right.copy())

    def e_closure(self):
        return self.left.e_closure()
        return self.right.e_closure()

    def move(self):
        return self.left.move()
        return self.right.move()

class ConcatenationNode:
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def copy(self):
        return OrNode(self.left.copy(), self.right.copy())

    def e_closure(self):
        return self.left.e_closure()
        return self.right.e_closure()

    def move(self):
        return self.left.move()
        return self.right.move()

class ClosureNode:
    def __init__(self, child):
        self.child = child

    def copy(self):
        return RootNode(self.child.copy())

    def e_closure(self):
        return self.child.e_closure()

    def move(self):
        return self.child.move()


class PositiveCloseNode:
    def __init__(self, child):
        self.child = ConcatenationNode(child.copy(), ClosureNode(child))

    def copy(self):
        return RootNode(self.child.copy())

    def e_closure(self):
        return self.child.e_closure()

    def move(self):
        return self.child.move()

class OptionalPartNode:
    def __init__(self, child):
        self.child = child

    def copy(self):
        return RootNode(self.child.copy())

    def e_closure(self):
        return self.child.e_closure()

    def move(self):
        return self.child.move()

class RepeatNode:
    def __init__(self, child, XBound=0, YBound=-1): #XBound - нижняя граница, YBound - верхняя граница
        if YBound < 0 and not XBound:
            self.child = ClosureNode(child)
        elif YBound < 0:
            last = ClosureNode(child)
            first = child.copy()
            for i in range(1, XBound):
                first = ConcatenationNode(first, child.copy())
            self.child = ConcatenationNode(first, last)
        else:
            if not XBound:
                first = NullNode()
                first = OrNode(first, child)
            else:
                first = child
            for i in range(1, YBound):
                last = ConcatenationNode(child.copy(), child.copy())
                for j in range(1, i):
                    last = ConcatenationNode(last, child.copy())
                first = OrNode(first, last)
            self.child = first

    def copy(self):
        return RootNode(self.child.copy())

    def e_closure(self):
        return self.child.e_closure()

    def move(self):
        return self.child.move()


class NamedCaptureGroupeNode:
    def __init__(self, child, name):
        self.child = child
        self.name = name

    def copy(self):
        return NamedCaptureGroupeNode(self.child.copy(), self.name)

    def e_closure(self):
        return self.child.e_closure()

    def move(self):
        return self.child.move()


class SymbolNode:
    def __init__(self, char=-1):
        self.char = char  # -1 - special code for any symbol node
        global listNodes
        global followposList
        self.number = len(listNodes)

    def copy(self):
        return SymbolNode(self.char)

    def e_closure(self):
        return self.child.e_closure()

    def move(self):
        return self.child.move()

class NullNode:
    def __init__(self):
        pass

    def copy(self):
        return NullNode()

    def e_closure(self):
        pass

    def move(self):
        pass
