import Gotodeftest.Gotodef

@[gotodef "./hello.txt"]
def hello := "world"

#check hello -- <-- go-to-def here should do it
