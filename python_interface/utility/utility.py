class bcolors:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'

def INFO(*args, **kwargs):
    if 'imp' in kwargs and kwargs['imp']:
        kwargs.pop('imp')
        print(bcolors.OKGREEN, end="")
        print(bcolors.BOLD, end="")
        print(*args, **kwargs)
        print(bcolors.ENDC, end="")
    else:
        print(bcolors.OKGREEN, end="")
        print(*args, **kwargs)
        print(bcolors.ENDC, end="")

def FAIL(*args, **kwargs):
    print(bcolors.FAIL, end="")
    print(*args, **kwargs)
    print(bcolors.ENDC, end="")

def MSG(*args, **kwargs):
    print(bcolors.OKBLUE, end="")
    print(*args, **kwargs)
    print(bcolors.ENDC, end="")