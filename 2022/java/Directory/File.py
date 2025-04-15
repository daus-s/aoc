class File: 
   def __init__(self, name, size,  depth=0):
      self.name = name
      self.size = int(size)
      self.depth = int(depth)

   def ls(self):
      tab = self.depth*'\t'
      return f'{tab}{self.name}-- size:{self.size}B\n'
   

   def __str__(self):
      return f'{self.name}-- size:{self.size}B'