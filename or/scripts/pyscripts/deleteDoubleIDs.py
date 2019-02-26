'''This script deletes all sentences from the database that have been extracted several times'''

'Path to the result database from after running tdtlite'
f = open("/home/neele/XPRAG/tdtlite/or/results/swbd.tab", "r")

idlist = list()
database = list()
'delete all doubled sentences that were extracted of tdtlite because they contained more than one or' \
'keep those sentences that are annotated with the correct information for "more than one or"'
for line in f.readlines():
    s = line.split("\t")[1]
    id = (line.split("\t")[0]).split(":")[0]
    moreThanOneor = line.split("\t")[6]
    if (moreThanOneor == "yes"):
        if (id not in idlist):
            idlist.append(id)
            database.append(line)
    else:
        if (id not in idlist):
            idlist.append(id)
            database.append(line)

'path tho the file that contains only unique sentences'
filed = open('/home/neele/XPRAG/databaseWithUniqueIDs', 'w')
for line in database:
    filed.write(line)

filed.close()
f.close()