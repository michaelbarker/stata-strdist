
clear all

mata
		
bestaname = ""
bestbname = ""

namedist("Billy Jack" , "James Habyarimana", bestaname, bestbname)
bestaname
bestbname


namedist("Jack" , "Habyarimana James", bestaname, bestbname)
bestaname
bestbname


namedist("Jack" , "H James", bestaname, bestbname)
bestaname
bestbname


namedist("" , "H James", bestaname, bestbname)
bestaname
bestbname


namedist("Jack" , "Habyarimana", bestaname, bestbname)
bestaname
bestbname


namedist("" , "Habyarimana", bestaname, bestbname)
bestaname
bestbname


worddist("Jack" , "Habyarimana")
worddist("" , "Habyarimana")


end
