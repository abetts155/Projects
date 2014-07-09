import re, shlex
import Debug

noUnsignedWrap = 'nuw'
noSignedWrap   = 'nsw'
addrSuffix     = '.addr'

# =========================== All LLVM operations ===========================

binaryOps      = ['add', 
                  'sub', 
                  'mul', 
                  'fadd', 
                  'fsub', 
                  'fmul', 
                  'udiv', 
                  'sdiv', 
                  'fdiv', 
                  'urem', 
                  'srem', 
                  'frem']
comparisonOps  = ['icmp', 
                  'fcmp'] 

phiOp          = 'phi'

# =========================== All LLVM condition codes ===========================

conditions = ['eq',
              'ne',
              'neq',
              'ugt',
              'uge',
              'ult',
              'ule',
              'sgt',
              'sge',
              'slt',
              'sle']

# =========================== Helper utilities ===========================

def isInteger (string):
    return re.match(r'i[0-9]+', string)

def isLLVMBaseType (string):
    return re.match(r'void|i[0-9]+|half|float|double', string)

def isLLVMPointerType (dataType):
    return re.match(r'.*\*', dataType)

def numberOfIndirections (dataType):
    return dataType.count('*')

def isOnStack (symbol):
    return symbol.endswith(addrSuffix)

def stripAddrSuffix (symbol):
    return symbol[:-len(addrSuffix)]

def isLLVMAddressSpace (string):
    return re.match(r'addrspace\([1-4]\)', string)

# =========================== To extract LLVM struct definition ===========================

class LLVMStructDefinition:
    typePrefix = '%struct.'
    
    @staticmethod
    def getTypeName (symbol):
        return re.sub('%s' % LLVMStructDefinition.typePrefix, '', symbol)
    
    def __init__ (self, line):
        self.name       = None
        self.fieldTypes = []
        self.__parse(line)
        Debug.debugMessage("Found struct '%s' with field types %s" % (self.name, self.fieldTypes), 10)
        
    def __parse (self, line):
        lexemes     = re.split('[,\s]+', line)
        fieldsFound = False
        for lex in lexemes:
            if re.match(r'%s' % LLVMStructDefinition.typePrefix, lex):
                self.name = LLVMStructDefinition.getTypeName(lex)
            elif lex == '{':
                fieldsFound = True
            elif lex == '}':
                break
            elif fieldsFound:
                self.fieldTypes.append(lex)

# =========================== To extract LLVM type ===========================

class LLVMType:
    def __init__ (self, typeString):
        self.baseType   = None
        self.dimensions = 0
        self.struct     = False
        self.pointer    = False
        fields = shlex.split(typeString)
        for field in fields:
            if field == 'x':
                self.dimensions += 1
            field = re.sub('\[', '', field)
            field = re.sub('\]', '', field)
            if re.match(r'%s' % LLVMStructDefinition.typePrefix, field):
                self.baseType = LLVMStructDefinition.getTypeName(field)
                self.struct   = True
            elif isLLVMBaseType(field):
                self.baseType = field
        self.pointer = isLLVMPointerType(self.baseType)
        assert self.baseType
        Debug.debugMessage("Found type '%s' with %d dimensions" % (self.baseType, self.dimensions), 100)
