#!/usr/bin/python

# define filename, output filename here
filename = "Pokegema.lss"
output_filename = "Pokegema_flipped.lss"

ofp = open(output_filename, "w")

for line in open(filename):
    # strip out whitespace to left and right of string, split by the space between
    parts = line.split()
    print parts[0]
    
    if(parts[0] == "<X>"):
       # when an X is found, "parts[1]" still has the stupid "</X>" appended to it
       # this ugly looking code removes the trailing "</X>" or "<Y>"
       my_x = parts[1].split("<")[0]
       
    if(parts[0] == "<Y>"):
        my_y = parts[1].split("<")[0]
        iCount = iCount + 1
        mydict = makeDictionary(my_x, my_y)
        mylist.append(mydict)

#    ofp.write("\t\t\t<Y> " + item["Y"] + "</Y>\n")    
#    ofp.write("\t\t</Vertex>\n")

ofp.flush()
ofp.close()


