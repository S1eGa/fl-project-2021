def printStart(f):
    f.write('Func func() -> Bool {')

def printEnd(f):
    f.write('}')

def printFuncApply(f, frst, cnt):
    for i in range(cnt):
        f.write('func(' + frst + ',')
    for i in range(cnt):
        f.write(')')

def printPow(cnt):
    strr = "5"
    for i in range(cnt):
        strr = strr + "^" + "5"
    return strr

def printOps(cnt):
    strr = "5"
    for i in range(cnt):
        strr = strr + "^" + "5" + "&&" + "6" + "*" + "7"
    return strr

def genTest4():
    f = open('test4', 'w')
    printStart(f)
    f.write('Var Int y := ')
    printFuncApply(f, printPow(600), 600)
    f.write(';')
    printEnd(f)


def genTest5():
    f = open('test5', 'w')
    printStart(f)
    f.write('Var Int y := ')
    printFuncApply(f, printOps(600), 600)
    f.write(';')
    printEnd(f)

def genTest6():
    f = open('test6', 'w')
    printStart(f)
    f.write('Var Int y := ')
    f.write(printOps(100000))
    f.write(';')
    printEnd(f)

genTest4() 
genTest5()
genTest6()
