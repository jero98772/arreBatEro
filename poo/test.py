class counter:
    _instances = {}
    def __new__(cls, maxvalue):
        if maxvalue not in cls._instances:
            cls._instances[maxvalue] = super().__new__(cls)
        return cls._instances[maxvalue]

    def __init__(self, maxvalue):
        # Initialize only if the instance is newly created
        if not hasattr(self, 'initialized'):
            self.start = maxvalue
            self.maxvalue = maxvalue
            self.initialized = True
    def decrement(self,value=1):
	    self.start-=value



c=counter(30)
print(c.start)
c.decrement()
print(c.start)
c=counter(30)
print(c.start)
