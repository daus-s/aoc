class Directory: 
    
    def __init__(self, name, parent, depth=0):
        self.name = name
        self.depth = depth
        self.parent = parent
        self.contents = []

    def ls(self):
        out = self.depth*'\t' + self.name + '\n'
        for entry in self.contents:
            out += (self.depth+1)*'\t' + entry.ls()
        return out

    def update(self, entry): 
        if entry not in self.contents:
            self.contents.append(entry)
    
    def get(self, dir):
        for c in self.contents:
            print(c.name)
            print(dir)
            if c.name == dir:
                return c
        return None
    

    def __str__(self):
        s = f'name: {self.name[:-1]} contents: [\n'
        for c in self.contents:
            if not c == self.contents[-1]:
                s += str(c) + ', '
            else:
                s += str(c)
        s += f'\n] parent: {self.parent}\n'
        return s