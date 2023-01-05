import packages_oop.package as p
import numpy as np
import packages_oop.test2 as test
np.random.seed(999)


pack = p.packages()
test1 = test.Test()
linkage_field = ["first_name", "middle_name", "last_name", "res_street_address", "birth_year"]

#plink = pack.splink(2000, linkage_field, "zip_code", "test.txt")
python_recordlinkage = pack.python_recordlinkage(2000, linkage_field, "zip_code", "test.txt", 0)

#splink2 = test1.splink()