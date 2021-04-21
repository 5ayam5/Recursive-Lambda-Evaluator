make
echo
for file in testcases/*
do
	echo $file
	./a3 $file
	echo
done
