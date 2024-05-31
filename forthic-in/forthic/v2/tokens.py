class Token:
    pass


class StringToken(Token):
    def __init__(self, string: str):
        self.string: str = string


class CommentToken(Token):
    def __init__(self, string: str):
        self.string: str = string


class StartArrayToken(Token):
    pass


class EndArrayToken(Token):
    pass


class StartModuleToken(Token):
    def __init__(self, name: str):
        self.name: str = name


class EndModuleToken(Token):
    pass


class StartDefinitionToken(Token):
    def __init__(self, name: str):
        self.name: str = name


class EndDefinitionToken(Token):
    pass


class StartMemoToken(Token):
    def __init__(self, name: str):
        self.name: str = name


class WordToken(Token):
    def __init__(self, name: str):
        self.name: str = name


class EOSToken(Token):
    pass
