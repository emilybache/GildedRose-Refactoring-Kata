class StringWrapper:
    def __init__(self):
        self.string = ""

    def append(self, text):
        self.string += text

    def __str__(self):
        return self.string

    def __repr__(self):
        return self.string
