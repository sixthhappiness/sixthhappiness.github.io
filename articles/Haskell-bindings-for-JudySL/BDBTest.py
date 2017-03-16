from bsddb3 import db

adb = db.DB()
adb.open("pywords.db", dbtype = db.DB_HASH, flags = db.DB_CREATE)

f = open("/usr/share/dict/words", 'r')

for w in f:
    adb.put(w.rstrip().encode(), "1")
    
adb.close()

adb = db.DB()
adb.open("pywords.db", dbtype = db.DB_HASH)

f = open("/usr/share/dict/words", 'r')

for w in f:
    if adb.get(w.rstrip().encode()) != "1".encode():
        print("Error!")
    
adb.close()
