class UnauthorizedError(RuntimeError):
    def __init__(self, field):
        super().__init__(f'Unauthorized creds for: {field}')
        self.field = field
