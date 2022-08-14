# we try to parse the  input01unix.txt file 
# and the output should result01.txt
run ../../samples/testsyntaxtest.exe input01sh.txt
expect TextMatch(stdoutfn, 'result01.txt','')
