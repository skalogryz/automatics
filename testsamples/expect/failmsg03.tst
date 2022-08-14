# simple condition, and it should fail to run
failmsg you won't see me
expect 1=1
failmsg Wrong Expression 2
expect 1>2