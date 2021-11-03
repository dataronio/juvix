import json
import glob
import hashlib
import os


checksums = {}
for ju in sorted(glob.glob("stdlib/*.ju") + glob.glob("stdlib/*/*.ju")):
    basename = ju[7:]
    checksums[basename] = hashlib.sha256(open(ju, "rb").read()).hexdigest()

updated_jus = list(checksums.values())

json.dump(checksums, open("stdlib/checksums.json", "w+"), indent=4, sort_keys=True)