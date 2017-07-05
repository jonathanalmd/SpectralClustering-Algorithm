with open('iris.data.txt') as f:
    data = f.readlines()
data = [x.strip() for x in data if x != '\n' and x != '\r' and x != '\r\n'] 

proc_output = open('proc-iris.data.txt','w')
for instance in data:
	proc_instance = instance.split(',')
	proc_output.write("%s\n" % ','.join(proc_instance[2:]))	

